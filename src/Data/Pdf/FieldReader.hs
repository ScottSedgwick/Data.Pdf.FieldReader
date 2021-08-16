{- |
Copyright: (c) 2021 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>
Stability: experimental
Portability: unknown

Simple function to extract PDF form field values from a PDF file.
-}

module Data.Pdf.FieldReader
  ( -- * File data parser
    --
    -- $fileDataParser
    readPdfFields
  ) where

import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt)
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), anySingle, chunk, many, parseMaybe, takeWhileP)

-- $fileDataParser
-- 
-- Extract a Map of name-value pairs from the data read from a PDF file.
-- For example:
--
-- >import qualified Data.ByteString as B
-- >import Data.Pdf.FieldReader (readPdfFields)
-- >
-- >main :: IO()
-- >main = do
-- >  xs <- Data.ByteString.readFile "filename"
-- >  let ys = readPdfFields xs
-- >  print ys

-- | Read fields from PDF file data

readPdfFields :: ByteString -> M.Map Text Text
readPdfFields = maybe M.empty collate . parseMaybe pdfFieldParser . decodeLatin1

type Parser = Parsec Void Text
data FieldPart 
  = FieldName Text
  | FieldValue Text
  | FieldNothing
  deriving stock (Show, Eq)

collate :: [FieldPart] -> M.Map Text Text
collate = snd . foldl' f (Nothing, M.empty)
  where
    f (Nothing, m) (FieldName n) = (Just n, m)
    f (Nothing, m) _             = (Nothing, m)
    f (Just n, m) (FieldValue v) = (Nothing, M.insert n v m) 
    f (Just n, m) _              = (Just n, m) 

pdfFieldParser :: Parser [FieldPart]
pdfFieldParser = many (pFieldName <|> pFieldValue <|> pFieldHexValue <|> anythingElse)

anythingElse :: Parser FieldPart
anythingElse = anySingle >> pure FieldNothing

pFieldName :: Parser FieldPart
pFieldName = getBetween "T(" ")" <&> FieldName

pFieldValue :: Parser FieldPart
pFieldValue = getBetween "V(" ")" <&> FieldValue

pFieldHexValue :: Parser FieldPart
pFieldHexValue = getBetween "V<" ">" <&> FieldValue . decodeHexField

getBetween :: Text -> Text -> Parser Text
getBetween x y = chunk x >> takeWhileP Nothing (/= T.head y) >>= \z -> chunk y >> pure z

decodeHexField :: Text -> Text
decodeHexField xs = if head ys /= "FEFF" then "" else T.pack (map (chr . decodeHex) (tail ys))
  where
    ys = splitBy 4 $ T.concat $ T.lines xs

splitBy :: Int -> Text -> [Text]
splitBy _ "" = []
splitBy x s  = T.take x s : splitBy x (T.drop x s)

decodeHex :: Text -> Int
decodeHex = foldl' (\b a -> b * 16 + digitToInt a) 0 . T.unpack
