module Octopart (queryMpns, Responses) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import Data.Scientific
import Control.Exception (throwIO)
import Control.Monad (mapM, mzero, when, MonadPlus)
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.Vector ((!?))
import qualified Network.HTTP.Req as R
import Network.HTTP.Req ((=:), (/:))
import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Text.Read (readMaybe)
import qualified Data.Vector as V

-- There are well-known issues with representing monetary values
-- using floating point numbers. For this application, it would not
-- be appropriate to model monetary values as integers either, since
-- manafacturers/distributors may in some cases have prices that cannot
-- be exactly represented in the smallest units of a given currency.
-- For this reason I am using Scientific values to represent monetary values.
-- (This is more convenient than using e.g. Rational because Data.Aeson uses
-- Scentific to represent JSON number literals.)

data Offer = Offer {
  seller :: T.Text,
  quantity :: Int,
  priceUSD :: Scientific
} deriving Show

mpnToJsonQuery :: T.Text -> Value
mpnToJsonQuery mpn =
  object [
    (T.pack "mpn") .= mpn
  ]

-- This is not nice. I couldn't find a better way to pass JSON-encoded query parameters
-- using the 'req' library.
mpnsToJsonQueryText :: [T.Text] -> T.Text
mpnsToJsonQueryText mpns = TL.toStrict $ encodeToLazyText $ Array $ V.fromList $ map mpnToJsonQuery mpns

instance R.MonadHttp IO where
  handleHttpException = throwIO

queryMpns :: T.Text -> [T.Text] -> IO (R.JsonResponse Responses)
queryMpns apiKey mpns =
  R.req
    R.GET
    (R.https (T.pack "octopart.com") /: (T.pack "api") /: (T.pack "v3") /: (T.pack "parts") /: (T.pack "match"))
    R.NoReqBody
    R.jsonResponse
    ((T.pack "queries") =: (mpnsToJsonQueryText mpns) <>
     (T.pack "apikey") =: apiKey)

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

offer :: T.Text -> Value -> Parser Offer
offer seller = withArray "offer" $ \o -> do
  quantity <- (liftMaybe $ o !? 0) >>= (withScientific "quantity" (liftMaybe . toBoundedInteger))
  price <- (liftMaybe $ o !? 1) >>= (withText "price" (liftMaybe . readMaybe . T.unpack))
  return $ Offer { quantity = quantity, priceUSD = price, seller = seller }

offerFieldToOffers :: Value -> Parser (V.Vector Offer)
offerFieldToOffers = withObject "offers" $ \o -> do
  klass <- o .: (T.pack "__class__")
  when (klass /= (T.pack "PartOffer")) (fail "'__class__' field should be 'PartOffer'")
  prices <- o .: (T.pack "prices")
  usd <- prices .:? (T.pack "USD")
  case usd of
    Nothing -> return V.empty
    Just usd -> do
      seller <- o .: (T.pack "seller")
      name <- seller .: (T.pack "name")
      mapM (offer name) usd

itemToOffers :: Value -> Parser (V.Vector Offer)
itemToOffers = withObject "item" $ \o -> do
  offers <- o .: (T.pack "offers")
  V.concat <$> V.toList <$> mapM offerFieldToOffers offers

resultToOffers :: Value -> Parser (V.Vector Offer)
resultToOffers = withObject "result" $ \o -> do
  items <- o .: (T.pack "items")
  V.concat <$> V.toList <$> mapM itemToOffers items

parseResponses :: Value -> Parser (V.Vector (V.Vector Offer))
parseResponses = withObject "response" $ \o -> do
  results <- o .: (T.pack "results")
  mapM resultToOffers results

newtype Responses = Responses (V.Vector (V.Vector Offer)) deriving Show

instance FromJSON Responses where
  parseJSON x = fmap Responses (parseResponses x)