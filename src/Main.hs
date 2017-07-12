module Main where

import qualified CSV
import qualified Octopart as O
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Exception (throw)
import Network.HTTP.Req (responseBody)
import Control.Applicative ((<|>))
import System.Environment (getArgs, getEnv)
import qualified Text.Read as TR
import qualified BOM

main :: IO ()
main = do
  (filename, batchSize) <- (parseArgs <$> getArgs) >>= liftEither
  apiKey <- (T.pack <$> getEnv "OCTOPART_API_KEY")
            <|> fail "You must define the environment variable OCTOPART_API_KEY before running this program"
  csvContents <- B.readFile filename
  bomLines <- liftEither $ BOM.fromCsv csvContents
  json <- responseBody <$> O.queryMpns apiKey [T.pack "SN74S74N"]
  print json

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

usage = "Program must be given two arguments: filename and batch size (integer >= 1)"

-- Given an integer i, splits a list l into the smallest list of sublists l'
-- such that (a) concat l' == l and (b) each member of l' is of length <= i.
groupN :: Int -> [a] -> [[a]]
groupN _ [ ] = [ ]
groupN i l = firstN : (groupN i rest) where (firstN, rest) = splitAt i l

liftEither :: Either String a -> IO a
liftEither = either fail return