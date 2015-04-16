module Main where

import           Control.Applicative
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L
-- import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Builder          as B

import           Data.Maybe
import           Data.Monoid
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Data.Traversable as T
import qualified Data.Foldable    as F

import           Text.Printf (printf)

import           System.Process.Exts
import           System.Environment

-- import FileEntry
import FileInfo

------------------------------------------------------------------------

compareContainers :: [Entry] -> [Entry] -> IO () -- (Set.Set (ByteString, ByteString), Set.Set (ByteString, ByteString))
compareContainers c1 c2 =
  let helper :: Entry -> (ByteString, Entry)
      helper e = (fromJust . e_checkSum $ e, e)
      shelper e = (fromJust . e_checkSum $ e, last . BC.split '/' . e_path $ e)
      importer = Map.fromList . map helper . filter (not . needsChecksum)
      simporter = Set.fromList . map shelper . filter (not . needsChecksum)
      m1 = importer c1
      m2 = importer c2
      s1 = simporter c1
      s2 = simporter c2
      common = s1 `Set.intersection` s2
      sz1 = Set.size s1
      sz2 = Set.size s2
      szc = Set.size common
      sA' = Set.map snd $ s1 `Set.difference` common
      sB' = Set.map snd $ s2 `Set.difference` common
      sC' = sA' `Set.intersection` sB'
  in do
    printf "     common: %9d\n" szc
    printf "unique to A: %9d\n" (sz1 - szc)
    printf "unique to B: %9d\n" (sz2 - szc)
    printf "different content, same name in A, B: %d\n" (Set.size sC')
    mapM_ print . Set.toList $ sC'
    -- return (s1, s2)

compareDirectories :: FilePath -> FilePath -> IO ()
compareDirectories indexFile1 indexFile2 = do
  rs1 <- readState' indexFile1
  rs2 <- readState' indexFile2
  _ <- compareContainers rs1 rs2
  return ()
  
main :: IO ()
main = do
  [i1, i2] <- getArgs
  compareDirectories i1 i2

