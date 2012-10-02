module Main where

import BenDoerr.IO.Common
import BenDoerr.WordFrequency.Reader
import BenDoerr.WordFrequency.Count
import BenDoerr.WordFrequency.Printer

main :: IO ()
main = ioHandled $ getSpecimen >>= printHistogram 80 . countWords
