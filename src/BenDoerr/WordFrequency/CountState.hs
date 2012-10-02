{- | Module: BenDoerr.WordFrequency.CountState

     A go at using the State monad but it was much to memory intensive as well
     as hitting stack overflows.
-}
module BenDoerr.WordFrequency.CountState (
    -- * ???
      WordList, WordCountDetails(..), WordCount
    -- * ???
    , countWords, countWord, getWordCounts
    ) where

import qualified Data.Map as M (lookup, insertWith', empty)

import Control.Monad.State.Strict
        (State, execState, get, put, gets)
import Data.Map (Map)

import BenDoerr.WordFrequency.Reader (Word, wordLength)

type WordList = Map Word Integer

data WordCountDetails = WordCountDetails { wordMap     ::  WordList
                                         , maxCount    ::  Integer
                                         , longestWord ::  Integer
                                         } deriving (Show)

type WordCount a = State WordCountDetails a

countWord :: Word -> WordCount ()
countWord w = do
              state <- get
              let wordCount      = maybe 1 (+ 1) . M.lookup w . wordMap $ state
                  newMaxCount    = max wordCount (maxCount state)
                  wordLen        = wordLength w
                  newLongestWord = max wordLen (longestWord state)
                  newWordMap     = M.insertWith' (+) w 1 (wordMap state)
              put $ WordCountDetails newWordMap newMaxCount newLongestWord

countWords :: [Word] -> WordCount ()
countWords = mapM_ countWord

getWordCounts :: [Word] -> WordCountDetails
getWordCounts words = execState (countWords words) (WordCountDetails M.empty 0 0)
