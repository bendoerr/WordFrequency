{- |
     Module BenDoerr.WordFrequency.Reader

     Description
-}
module BenDoerr.WordFrequency.Reader (
    -- * Primary Interface
      getSpecimen, Word
    -- * Building the Word List
    , cleanWords, normalizeWord
    -- * Helpers
    , toString, wordLength, printSpecimen, printSpecimen'
) where

import qualified Data.Text.Lazy as T
    (null, words, toLower, filter, unpack, dropAround, length)
import qualified Data.Text.Lazy.IO as TIO (putStrLn)

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)

import BenDoerr.IO.Common (readFromStdinOrArgsFiles)

-- | Type alias of Text for a type that represents a word from the specimen.
type Word = Text

{- | The primary function that takes input from a list of files or if no files
     are specified then from stdin. Breaks the text in those files into their
     word. Words are normalized in lower case and have surrounding punctuation
     removed.
-}
getSpecimen :: IO [Word]
getSpecimen = cleanWords `fmap` readFromStdinOrArgsFiles

{- | Read the 'ByteString' as UTF-8, separate it into words, lowercase the
     words, drop punctuation around the word, and return only non null words.
-}
cleanWords :: ByteString -> [Word]
cleanWords = filter (not . T.null) . map normalizeWord . T.words . decodeUtf8

{- | Converts the word to lowercase and removes surrounding punctuation.

     * Punctuation is considered @ *,.-!?\<>/\";:][}{)(\@\#$%^&\`~-_=+ @
-}
normalizeWord :: Word -> Word
normalizeWord = T.dropAround goodChars . T.toLower
    where goodChars c = c `elem` "*,.-!?<>/\";:][}{)(@#$%^&`~-_=+"

-- | Unpack a 'Word' to a 'String'.
toString :: Word -> String
toString = T.unpack

-- | The length of a particular word.
wordLength :: Word -> Integer
wordLength = fromIntegral . T.length

-- | Print a word list.
printSpecimen :: [Word] -> IO ()
printSpecimen = mapM_ TIO.putStrLn

-- | Print a word list that is the result of 'getSpecimen'
printSpecimen' :: IO ()
printSpecimen' = do
                 specimen <- getSpecimen
                 printSpecimen specimen
