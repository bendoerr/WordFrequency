{- |
   Module: BenDoerr.IO.Common

   Common things that I usualy do with IO, error handling and reading files and
   standared input. Things that I tend to write again and again.
-}
module BenDoerr.IO.Common (
    -- * Dealing with IO errors
      ioHandled, commonErrorHandler
    -- * Getting Text
    , readFromStdinOrArgsFiles, readFromStdinOrFiles
) where

import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy.Char8 as S
    (readFile, getContents, concat)

import Control.Monad (fmap, mapM)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (hPutStr)
import GHC.IO.Handle.FD (stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (IOError, isDoesNotExistError, ioeGetFileName)

{- | Catch 'IOError's using the 'commonErrorHandler'.

     Use on main functions for consistent handling of IOErrors at the top
     level.

     > main = ioHandled $ do
     >                    c <- getContents
     >                    putStrLn c
-}
ioHandled :: IO () -> IO ()
ioHandled f = f `E.catch` commonErrorHandler

{- | Handle top level 'IOError's consistently.

     * Currently only handles DoesNotExistErrors.

     * Will continue to add as I run into them.
-}
commonErrorHandler :: IOError -> IO ()
commonErrorHandler e
    | isDoesNotExistError e = exitFailureWithMessage $
        "Oops! File doesn't exist: " ++ fromMaybe "" (ioeGetFileName e)
    | otherwise = ioError e
    where exitFailureWithMessage msg = do
                                       hPutStr stderr (msg ++ "\n")
                                       exitFailure

{- | Wrapper around 'readFromStdinOrFiles' that passes arguments from
     'System.Environment.getArgs'.
-}
readFromStdinOrArgsFiles :: IO ByteString
readFromStdinOrArgsFiles = do
                           args <- getArgs
                           readFromStdinOrFiles args

{- | If an empty list is given then read from Stdin. Otherwise try reading from
     files specified by each of the argument list, then join them togeather.

     This should read multiple files using near constant memory, there will be
     a little bump for each addtional file due to an additional file handle.
-}
readFromStdinOrFiles :: [FilePath] -> IO ByteString
readFromStdinOrFiles paths
    | null paths = S.getContents
    | otherwise = ioStringFromFiles paths
    where ioStringFromFiles = fmap S.concat . mapM S.readFile
