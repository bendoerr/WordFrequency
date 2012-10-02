{- |
     Module: BenDoerr.WordFrequency.Printer

     Prints a histogram of the word counts.

     TODO: Refactor stats into their own data type.
-}
module BenDoerr.WordFrequency.Printer (
    -- * Print a Histogram
      printHistogram
    -- * Helpers
    , printWord, findWidth, printHeader
    ) where

import Text.Printf (printf)

import BenDoerr.WordFrequency.Reader (Word, toString)
import BenDoerr.WordFrequency.Count
        (WordCounts, WordCountDetails (..), wordCountsToList)

{- | Main function to print a histogram of the 'WordCountDetails'.
     This will also print a header out for the histogram with the min and max
     counts as scale.
-}
printHistogram :: Integer           -- ^ Total width of the histogram.
               -> WordCountDetails  -- ^ 'WordCountDetails' to print.
               -> IO ()
printHistogram maxWidth (WordCountDetails wordCounts maxCount minCount maxLength) =
        printHeader maxWidth maxCount minCount maxLength >>
        mapM_ curriedPrintWord (wordCountsToList wordCounts)
    where curriedPrintWord = printWord maxWidth maxCount minCount maxLength

{- | Print the label and bar for a single word.

     The format is: @  label : \#\#\#\#\#\#\#\#\# @
-}
printWord :: Integer         -- ^ Total width of the histogram.
          -> Integer         -- ^ Highest count. Max Count.
          -> Integer         -- ^ Lowest count. Min Count.
          -> Integer         -- ^ Length of max word.
          -> (Word, Integer) -- ^ The word and it's count.
          -> IO ()
printWord maxWidth maxCount minCount maxLength (word, count) =
        printf format (toString word) bar
    where barWidth = findWidth 3 maxWidth maxCount maxLength count
          bar      = replicate barWidth '#'
          format   = "%" ++ show maxLength ++ "s : %s\n"


{- | Find the width to use for the bar given the max size of the histogram, the
     max size of the label and any margin between the label and bar.
-}
findWidth :: Integral b
          => Integer    -- ^ Margin used when calculating size that can be used.
          -> Integer    -- ^ Total width of the histogram.
          -> Integer    -- ^ Max Count.
          -> Integer    -- ^ Min Count.
          -> Integer    -- ^ The current count.
          -> b
findWidth margin maxWidth maxCount maxLength count =
        round $ fromIntegral count / fromIntegral maxCount * usableSize
    where usableSize = fromIntegral $ maxWidth - maxLength - margin

{- | Print a nice header with some bold borders and show the minimum count and
     maximum count for scale and perspective.
-}
printHeader :: Integer -- ^ Total width of the histogram.
            -> Integer -- ^ Max Count.
            -> Integer -- ^ Min Count.
            -> Integer -- ^ Length of max word.
            -> IO ()
printHeader maxWidth maxCount minCount maxLength = printLine >>
                                                   headers >>
                                                   printLine
    where printLine = putStrLn $ replicate (fromIntegral maxWidth) '='
          headers   = printf ("%" ++ show maxLength ++ "s : ") "Word" >>
                      printf ("%" ++ show minL ++ "s") (show minCount) >>
                      printf ("%" ++ show maxL ++ "s\n") (show maxCount)
          minL       = max (fromIntegral (length (show minCount)))
                        (findWidth 3 maxWidth maxCount maxLength minCount)
          maxL       = maxWidth - (maxLength + 3) - minL
