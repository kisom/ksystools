{-|
Module      : System.Data.Directory
Copyright   : (c) K. Isom (2017)
License     : BSD-style

Maintainer  : K. Isom <kyle@imap.cc>
Stability   : stable
Portability : portable

Dir contains utilities for interacting with directories, such
as obtaining lists of files.
  -}
module System.Data.Directory
  ( Listing
  , listDirectory
  , listingFiles
  , listingDirectories
  , listingParent
  ) where

import Control.Monad (filterM)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified System.Directory as Dir
import System.IO (FilePath)

-- | A Listing stores directories and files.
data Listing = Listing
  { directories :: [FilePath]
  , files :: [FilePath]
  , parent :: FilePath
  } deriving (Show)

notM
  :: Monad m
  => (t -> m Bool) -> t -> m Bool
notM f x = do
  val <- f x
  return $ not val

ignoreFiles :: [String]
ignoreFiles = [".", "..", ".git", ".hg", ".stack-work"]

isNotIgnored
  :: Monad m
  => String -> m Bool
isNotIgnored path = return $ notElem path ignoreFiles

isFile :: FilePath -> IO Bool
isFile = notM Dir.doesDirectoryExist

listFiles :: FilePath -> IO [FilePath]
listFiles path =
  fmap (map (joinPath path)) (Dir.getDirectoryContents path)
  >>= filterM isFile

sanitiseDir :: FilePath -> FilePath
sanitiseDir "." = ""
sanitiseDir "./" = ""
sanitiseDir path
  | last path == '/' = path
  | otherwise = path ++ "/"

joinPath :: FilePath -> FilePath -> FilePath
joinPath parent path = sanitiseDir parent ++ path

listDirectories :: FilePath -> IO [FilePath]
listDirectories path =
  Dir.getDirectoryContents path
  >>= filterM isNotIgnored
  >>= return . map (joinPath path)
  >>= filterM Dir.doesDirectoryExist

listDirectory :: FilePath -> IO Listing
listDirectory path = do
  ds <- listDirectories path
  fs <- listFiles path
  return Listing {directories = ds, files = fs, parent = path}

-- | listingFiles returns the list of files in a Listing.
listingFiles :: Listing -> [FilePath]
listingFiles (Listing _ f _) = f

-- | listingDirectories returns the list of directories in a Listing.
listingDirectories :: Listing -> [FilePath]
listingDirectories (Listing d _ _) = d

-- | listingParent returns the parent in a Listing.
listingParent :: Listing -> FilePath
listingParent (Listing _ _ p) = p

stripPrefix :: FilePath -> FilePath -> FilePath
stripPrefix path prefix = fromMaybe path (L.stripPrefix pfx path)
  where
    pfx =
      if last prefix == '/'
         then prefix
         else prefix ++ "/"

mergeListings :: Listing -> Listing -> Listing
mergeListings (Listing d1 f1 p1) (Listing d2 f2 p2) =
  Listing {directories = d1 ++ d2, files = f1 ++ f2, parent = p1}

mergeListings' :: [Listing] -> Listing
mergeListings' [] = Listing {directories = [], files = [], parent = ""}
mergeListings' listings =
  Listing
    { directories = concatMap listingDirectories listings
    , files = concatMap listingFiles listings
    , parent = listingParent $ head listings
    }

ioMergeListings :: IO Listing -> IO Listing -> IO Listing
ioMergeListings listing1 listing2 = do
  l1 <- listing1
  l2 <- listing2
  return $ mergeListings l1 l2
