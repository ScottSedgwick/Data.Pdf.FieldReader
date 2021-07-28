module Main (main) where

import Data.Pdf.FieldReader (readPdfFields)
import qualified Data.ByteString as B
import Options.Applicative

data Options = Options {
  datafile :: String
}

options :: Parser Options
options = Options
  <$> strOption ( short 'f' <> long "datafile" <> metavar "DATAFILE" <> help "PDF file to read" )
  
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Read a PDF file and print all the values in its form fields"
     <> header "pdfreader - a test for Data.Pdf.FieldReader" )
     
run :: Options -> IO()
run opts = do
  xs <- B.readFile (datafile opts)
  let ys = readPdfFields xs
  print ys
