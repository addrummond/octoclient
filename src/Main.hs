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
  -- Check that required cmd line args and env vars are present.
  (filename, batchSize) <- (parseArgs <$> getArgs) >>= liftEither
  apiKey <- (T.pack <$> getEnv "OCTOPART_API_KEY")
            <|> fail "You must define the environment variable OCTOPART_API_KEY before running this program"

  -- Read the CSV file and parse it to [BOM.BOMLine]
  csvContents <- B.readFile filename
  bomLines <- liftEither $ BOM.fromCsv csvContents
  partNumbers <- return $ map BOM.partNumber bomLines

  -- Group queries into chunks of <= 20 queries.
  queries <- return $ groupN octopartMaxQueries partNumbers

  -- Make one request per chunk.
  responses <-
    map (O.unwrapResponses . responseBody)
    <$> mapM (O.queryMpns apiKey) queries

  -- As we have multiple responses per query, we now have a list of vectors of
  -- vectors of offers. This could be flattened to a list of vectors of offers
  -- (with one member of the list for each line in the BOM). However, to avoid
  -- unnecessary allocation, we deal with the data as-is in the code below.

  -- Calculate BOM coverage by calculating for what fraction of lines in the BOM
  -- we found corresponding offers.
  responsesFor <-
    return $ sum $ map (V.foldl' (\count -> \v -> count + (if V.length v > 0 then 1 else 0)) 0) responses
  coverage <-
    return $ 100.0 * ((fromIntegral responsesFor) :: Double) / ((fromIntegral (length bomLines)) :: Double)

  -- Find the best price for the total order (taking into account batch size).
  bestPrice <-
    return $ sum $ map (fst . (V.foldl' (sumBestPrices batchSize) (0.0, bomLines))) responses

  -- Output the total and BOM coverage.
  fprint ((fixed 2) % string % (fixed 1) % string) bestPrice " USD\nBOM coverage: " coverage "%\n"

sumBestPrices :: Int -> (Scientific, [BOM.BOMLine]) -> V.Vector O.Offer -> (Scientific, [BOM.BOMLine])
sumBestPrices batchSize x@(_, []) _ = x
sumBestPrices batchSize (total, line:rest) offers =
  (total + total', rest)
  where 
    total' = (bestTotalPrice (quantity * batchSize) offers) *
             (fromIntegral quantity) * (fromIntegral batchSize)
    quantity = BOM.quantity line

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