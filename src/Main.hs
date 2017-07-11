module Main where

import qualified CSV
import qualified Octopart as O
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Aeson.Types as AT
import Control.Exception (throw)
import Network.HTTP.Req (responseBody)
import Control.Monad (mzero, MonadPlus)
import Control.Applicative ((<|>))
import System.Environment (getArgs, getEnv)

liftEither :: Either String a -> IO a
liftEither = either fail return

main :: IO ()
main = do
  apiKey <- (T.pack <$> getEnv "OCTOPART_API_KEY")
            <|> fail "You must define the environment variable OCTOPART_API_KEY before running this program"
  json <- responseBody <$> O.queryMpns apiKey [T.pack "SN74S74N"]
  print json