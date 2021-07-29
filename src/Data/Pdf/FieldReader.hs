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

import Prelude hiding (drop, init, lines)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.List (foldl')
import Data.Map (Map, fromList)
import Data.Text (Text, drop, init, isPrefixOf, lines, pack, replace, strip)

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

-- | Read fields from file data
readPdfFields :: ByteString -> Map Text Text
readPdfFields = fromList . snd . foldl' f (Nothing, []) . lines . pack . unpack
  where
    f (Nothing,  b) x | isFldName x  = (Just (fmtFldName x), b) 
                      | otherwise    = (Nothing, b)
    f ((Just n), b) x | isFldName x  = (Just (fmtFldName x), b)
                      | isFldValue x = (Nothing, (n, (fmtFldValue x)) : b)
                      | otherwise    = ((Just n), b)
    isFldName     = isPrefixOf "/T("
    fmtFldName    = stripBrackets
    isFldValue    = isPrefixOf "/V("
    fmtFldValue   = unescape . stripBrackets
    stripBrackets = init . drop 3 . strip
    unescape xs   = foldr (\(a, b) c -> replace a b c) xs escPairs
    escPairs      = [("\\n", "\n"), ("\\(", "("), ("\\)", ")")]
       