module BOM where

import qualified CSV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Text.Read as TR
import Control.Monad.Extra (concatMapM)

data BomLine = BomLine {
  manafacturer :: T.Text,
  partNumber :: T.Text,
  quantity :: Int
}

getBOM :: B.ByteString -> Either String [BomLine]
getBOM input = getBOM' $ CSV.parse input

getBOM' :: [[B.ByteString]] -> Either String [BomLine]
getBom' [] = Left "Empty BOM CSV file"
getBOM' (header:lines) = do
  checkHeader header
  concatMapM lineToBom lines

manfacturerHeading = C8.pack "manafacturer"
partNumberHeading = C8.pack "partNumber"
quantityHeading = C8.pack "quantity"

checkHeader :: [B.ByteString] -> Either String ()
checkHeader [manfacturerHeading, partNumberHeading, quantityHeading] = Right ()
checkHeader _ = Left "Unexpected header in BOM CSV file"

-- This returns a list of BomLine records so that we can deal
-- with the case where the quantity is 0 (in which case we
-- just ignore the relevant line in the BOM).
lineToBom :: [B.ByteString] -> Either String [BomLine]
lineToBom [manafacturer, partNumber, quantity] =
  case reads (C8.unpack quantity) of -- C8.unpack is ok here, as any valid number will be ASCII only
    [(quantity, "")] ->
        if quantity < 0
            then Left "Quantity must be an integer >= 0"
            else if quantity == 0
                then Right [ ]
                else Right [BomLine (TE.decodeUtf8 manafacturer) (TE.decodeUtf8 partNumber) quantity]
    _ -> Left "Badly formatted BOM file"
