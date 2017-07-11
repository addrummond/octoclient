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

apiKey = T.pack "c058e4f2"

liftEither :: Either String a -> IO a
liftEither = either fail return

main :: IO ()
main = do
  json <- responseBody <$> O.queryMpns apiKey [T.pack "SN74S74N"]
  print json