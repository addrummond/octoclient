module Main where

import qualified CSV
import qualified Octopart as O
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Scientific
import Control.Exception (throw)
import Network.HTTP.Req (responseBody)
import Control.Applicative ((<|>))
import System.Environment (getArgs, getEnv)
import qualified Text.Read as TR
import qualified BOM
import Formatting (fprint, (%), fixed, string)

-- Octopart API docs state that at most 20 queries may be made in a single request.
-- This program uses the smallest number of requests possible given this constraint.
octopartMaxQueries = 20

usage = "Program must be given two arguments: filename and batch size (integer >= 1)"

main :: IO ()
main = do
  (filename, batchSize) <- (parseArgs <$> getArgs) >>= liftEither
  apiKey <- (T.pack <$> getEnv "OCTOPART_API_KEY")
            <|> fail "You must define the environment variable OCTOPART_API_KEY before running this program"
  csvContents <- B.readFile filename
  bomLines <- liftEither $ BOM.fromCsv csvContents
  partNumbers <- return $ map BOM.partNumber bomLines
  queries <- return $ groupN octopartMaxQueries partNumbers
  -- At this point we convert the vector of vectors of query responses
  -- to a list of query responses, since we're going to loop through
  -- without consing. The alternative would be to V.concatMap using
  -- the vector representation, which would definitely require allocating
  -- a new vector.
  responses <-
    concatMap (V.toList . O.unwrapResponses . responseBody)
    <$> mapM (O.queryMpns apiKey) queries
  responsesFor <- return $ length $ filter ((> 0) . length) responses
  coverage <- return $ 100.0 * ((fromIntegral responsesFor) :: Double) / ((fromIntegral (length responses)) :: Double)
  bestPrices <-
    return $ map (\(quantity, offers) -> (bestTotalPrice (quantity * batchSize) offers)
                                         * (fromIntegral quantity) * (fromIntegral batchSize))
                 (zip (map BOM.quantity bomLines) responses)
  totalCost <- return $ (sum bestPrices)
  fprint ((fixed 2) % string % (fixed 1) % string) totalCost " USD\nBOM coverage: " coverage "%\n"

bestTotalPrice :: Int -> V.Vector O.Offer -> Scientific
bestTotalPrice n offers =
  V.foldl' f (0 :: Scientific) offers
    where
      f best offer
        | O.quantity offer <= n && (best == 0 || O.priceUSD offer < best) = O.priceUSD offer
        | True = best

-- Given an integer i, splits a list l into the smallest list of sublists l'
-- such that (a) concat l' == l and (b) each member of l' is of length <= i.
groupN :: Int -> [a] -> [[a]]
groupN _ [ ] = [ ]
groupN i l = firstN : (groupN i rest) where (firstN, rest) = splitAt i l

-- Command line syntax is too simple to make it worthwhile to use a command
-- line parsing library.
parseArgs :: [String] -> Either String (String, Int)
parseArgs args
  | length args /= 2 = Left usage
  | True = case TR.reads (args !! 1) of
             [(n,"")] ->
               if n <= 0
                 then Left usage
                 else Right ((args !! 0), n)
             _ -> Left usage

liftEither :: Either String a -> IO a
liftEither = either fail return