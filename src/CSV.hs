--
-- As a coding exercise, I have implemented the CSV parser myself.
-- For production code, I would just use an existing library
-- such as cassava: https://hackage.haskell.org/package/cassava
--

module CSV (parse) where

import qualified Data.ByteString as B
import qualified ByteString.TreeBuilder as BB
import Data.Monoid (mempty)
import Data.Word (Word8)
import Data.Char (ord)

-- The parser assumes that input is UTF-8 encoded. Since all of the special
-- chars relevant to CSV parsing are ASCII, it's not necessary to do
-- a full UTF-8 decoding step.
--
-- The parser is is implemented as a strict left fold over the sequence of bytes
-- in the input ByteString. It uses ByteString.TreeBuilder to build the byte
-- strings for individual fields.
--
-- The parser assumes that the input is well-formed, and is permissive
-- in what it accepts. It is possible in principle for a string not to be valid
-- CSV (e.g. the RFC does not allow unquoted whitespace within fields). However, in
-- practice, so many CSV files violate these sorts of constraints that there is
-- little to be gained by signaling errors.
--

parse :: B.ByteString -> [[B.ByteString]]
parse inp =
  reverse $ tidy $ currentLines $ addCurrentLine $
    B.foldl'
      parse'
      (State {
        currentField = mempty,
        currentLine = [ ],
        currentLines = [ ],
        afterQuote = False, -- We have just read '"', which may or may not be escaped by a subsequent '"'
        inQuoted = False,   -- The parser is currently within a quoted string
        afterComma = False  -- The parser has just read a comma and now needs to skip any whitespace.
      })
      inp

cr :: Word8
cr = fromIntegral (ord '\r')
nl :: Word8
nl = fromIntegral (ord '\n')
comma :: Word8
comma = fromIntegral (ord ',')
dquote :: Word8
dquote = fromIntegral (ord '"')
space :: Word8
space = fromIntegral (ord ' ')
tab :: Word8
tab = fromIntegral (ord '\t')

-- Avoid adding a trailing empty line or empty field to the parse.
tidy :: [[B.ByteString]] -> [[B.ByteString]]
tidy [ ] = [ ]
tidy ([]:rest) = rest
tidy ls@([bs]:rest)
  | B.length bs == 0 = rest
  | True = ls
tidy xs = xs

addCurrentField :: State -> State
addCurrentField s =
  s {
    currentField = mempty,
    currentLine = (BB.toByteString (currentField s)) : (currentLine s)
  }

addCurrentLine :: State -> State
addCurrentLine s =
  s' {
    currentLines = (reverse (currentLine s')) : (currentLines s'),
    currentLine = [ ]
  } where s' = addCurrentField s

addChar :: Word8 -> State -> State
addChar c s = s { currentField = mappend (currentField s) (BB.byte c) }

parse' :: State -> Word8 -> State
parse' s c
  | afterComma s =
      if c == space || c == tab
        then s
        else parse' (s { afterComma = False }) c
  | afterQuote s =
      if c == dquote
        then (addChar c s) { afterQuote = False }
        else parse' (s {
          inQuoted = not (inQuoted s),
          afterQuote = False
        }) c
  | inQuoted s =
      if c == dquote
        then s { afterQuote = True }
        else addChar c s
  | c == dquote =
      if afterQuote s
        then (addChar c s) { afterQuote = False }
        else s { afterQuote = True }
  | c == comma = (addCurrentField s) { afterComma = True }
  | c == cr = s -- We simply ignore '\r'. This is fine as long as the input is well-formed.
  | c == nl = addCurrentLine s
  | True = addChar c s

data State = State {
  currentField :: BB.Builder,
  currentLine :: [B.ByteString],
  currentLines :: [[B.ByteString]],
  afterQuote :: Bool,
  inQuoted :: Bool,
  afterComma :: Bool
}