module BOM (fromCsv) where

import qualified CSV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Text.Read as TR
import Control.Monad.Extra (concatMapM)
import Text.Printf (printf)

data BomLine = BomLine {
  manafacturer :: T.Text,
  partNumber :: T.Text,
  quantity :: Int
}

fromCsv :: B.ByteString -> Either String [BomLine]
fromCsv input = fromCsv' $ CSV.parse input

fromCsv' :: [[B.ByteString]] -> Either String [BomLine]
fromCsv' [] = Left "Empty BOM CSV file"
fromCsv' (header:lines) = do
  checkHeader header
  concatMapM lineToBom (zip [1..] lines)

manfacturerHeading = C8.pack "manafacturer"
partNumberHeading = C8.pack "partNumber"
quantityHeading = C8.pack "quantity"

checkHeader :: [B.ByteString] -> Either String ()
checkHeader [a,b,c] = if a == manfacturerHeading && b == partNumberHeading && c == quantityHeading
                        then Right ()
                        else Left "Unexpected header in BOM CSV file"

-- This returns a list of BomLine records so that we can deal
-- with the case where the quantity is 0 (in which case we
-- just ignore the relevant line in the BOM).
lineToBom :: (Int, [B.ByteString]) -> Either String [BomLine]
lineToBom (lineNumber, [manafacturer, partNumber, quantity]) =
  case reads (C8.unpack quantity) of -- C8.unpack is ok here, as any valid number will be ASCII only
    [(quantity, "")] ->
        if quantity < 0
            then Left (printf "Line %i: Quantity must be an integer >= 0" lineNumber)
            else if quantity == 0
                then Right [ ]
                else Right [BomLine (TE.decodeUtf8 manafacturer) (TE.decodeUtf8 partNumber) quantity]
    _ -> Left (printf "Line %i: Quantity (third column) must be an integer" lineNumber)
lineToBom (lineNumber, _) = Left (printf "Line %i: Each line in BOM must have three columns" lineNumber)
