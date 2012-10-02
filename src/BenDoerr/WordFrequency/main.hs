module Main where

import BenDoerr.IO.Common
import BenDoerr.WordFrequency.Reader
import BenDoerr.WordFrequency.Count

main :: IO ()
main = ioHandled $ do
                   spec <- getSpecimen
                   let (WordCountDetails wordCounts mCount mLength) = countWords spec
                   putStrLn $ "Max Count: " ++ show mCount
                   putStrLn $ "Max Length: " ++ show mLength
