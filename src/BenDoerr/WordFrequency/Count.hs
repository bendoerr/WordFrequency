{- |
     Module BenDoerr.WordFrequency.Count

     Description
-}
module BenDoerr.WordFrequency.Count (
    -- * Types
      WordCounts, WordCountDetails (..)
    -- * Primary Interface
    , countWords
    -- * Helpers
    , addWordOccurrence, wordCountsToList
    ) where

import qualified Data.Map as M (lookup, insertWith, insertWith', empty, toList)

import Data.List (sortBy)
import Data.Map (Map)

import BenDoerr.WordFrequency.Reader (Word, wordLength)

-- | Simple type alais for our map of words and their counts.
type WordCounts = Map Word Integer

{- | The data type that will hold details about the counted words, including
     the maximum count and maximum length of a word. These details will be
     useful when displaying.

     The details have been made to evaulate strictly to avoid a buildup of
     thunks.
-}
data WordCountDetails = WordCountDetails {
                            -- | The current state of all counted words. Strict.
                              wordCounts :: ! WordCounts
                            -- | The current maxiumum count. Strict.
                            , maxCount   :: ! Integer
                            -- | The current miniumum count. Strict.
                            , minCount   :: ! Integer
                            -- | The current maximum word length. Strict.
                            , maxLength  :: ! Integer
                            } deriving (Show)

{- | Builds up 'WordCountDetails' for every word. I think there will be a lot
     for the GC to do here since we build a new WordCountDetails on every word.
-}
countWords ::  [Word] -> WordCountDetails
countWords = foldl addWordOccurrence (WordCountDetails M.empty 0 100 0)

{- | The beef. Gets a new 'WordCountDetails'.

     Using strict version of insertWith 'M.insertWith'' to avoid thunks
     (slightly better memory footprint).
-}
addWordOccurrence ::  WordCountDetails -> Word -> WordCountDetails
addWordOccurrence details word =
        WordCountDetails nwordCounts nMaxCount nMinCount nMaxLength
    where count = maybe 1 (+ 1) . M.lookup word $ wordCounts details
          length = wordLength word
          nMaxCount = max count (maxCount details)
          nMinCount = min count (minCount details)
          nMaxLength = max length (maxLength details)
          nwordCounts = M.insertWith' (+) word 1 (wordCounts details)

{- | Transform the 'WordCounts' into a List of ('Word', 'Integer')

     The list will be orded descending by the count.
-}
wordCountsToList :: WordCounts -> [(Word, Integer)]
wordCountsToList = sortBy descValue . M.toList
    where descValue (lk, lv) (rk, rv) = compare rv lv
