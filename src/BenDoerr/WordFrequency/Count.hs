{- |
     Module BenDoerr.WordFrequency.Count

     Description
-}
module BenDoerr.WordFrequency.Count (
    -- * Types
      WordCounts, WordCountDetails(..)
    -- * Primary Interface
    , countWords
    -- * Helpers
    , addWordOccurrence
    ) where

import qualified Data.Map as M (lookup, insertWith, insertWith', empty)

import Data.Map (Map)

import BenDoerr.WordFrequency.Reader (Word, wordLength)

-- | Simple type alais for our map of words and their counts.
type WordCounts = Map Word Integer

{- | The data type that will hold details about the counted words, including
     the maximum count and maximum length of a word. These details will be useful
     when displaying.

     The details have been made to evaulate strictly to avoid a buildup of thunks.
-}
data WordCountDetails = WordCountDetails {
                            -- | The current state of all counted words. Strict.
                              wordCounts :: ! WordCounts
                            -- | The current maxiumum count. Strict.
                            , maxCount   :: ! Integer
                            -- | The current maximum word length. Strict.
                            , maxLength  :: ! Integer
                            } deriving (Show)

{- | Builds up 'WordCountDetails' for every word. I think there will be a lot
     for the GC to do here since we build a new WordCountDetails on every word.
-}
countWords ::  [Word] -> WordCountDetails
countWords = foldl addWordOccurrence (WordCountDetails M.empty 0 0)

{- | The beef. Gets a new 'WordCountDetails' 

     Using strict version of insertWith 'M.insertWith'' to avoid thunks
     (slightly better memory footprint).
-}
addWordOccurrence ::  WordCountDetails -> Word -> WordCountDetails
addWordOccurrence details word = WordCountDetails nwordCounts nMaxCount nMaxLength
    where count = maybe 1 (+ 1) . M.lookup word $ wordCounts details
          length = wordLength word
          nMaxCount = max count (maxCount details)
          nMaxLength = max length (maxLength details)
          nwordCounts = M.insertWith' (+) word 1 (wordCounts details)
