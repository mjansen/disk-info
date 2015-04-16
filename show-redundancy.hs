module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L
-- import qualified Data.Attoparsec.ByteString.Char8 as P
-- import qualified Data.ByteString.Builder          as B

import           Data.Char
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import qualified Data.Map.Strict  as Map
-- import qualified Data.Set         as Set
import qualified Data.Traversable as T
import           Data.Monoid

import           Text.Printf (printf)

import           System.FilePath
import           System.Process.Exts
import           System.Directory
import           System.Environment

-- import FileEntry

import FileInfo

type ByteString = BC.ByteString

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let fullPath cwd path@('/':_) = path
      fullPath cwd path         = cwd </> path
  [path] <- map (fullPath cwd) <$> getArgs
  -- use the free monoid to accumulate entries with the same checksum:
  db <- Map.fromListWith mappend . map (\ e -> (e_checkSum e, [e])) <$> readState' path
  let duplication@(Duplication t u)  = Map.foldr Main.combine mempty db
  print duplication
  when (t > 0) $ do
    printf "total size = %d, wasted = %d (%d %%)\n" t (t - u) (100*(t - u) `quot` t)

-- like readState, but the supplied path does not have to contain an Info structure:
    
data Duplication = Duplication Int64 Int64
  deriving Show

instance Monoid Duplication where
  mempty = Duplication 0 0
  mappend (Duplication x1 y1) (Duplication x2 y2) = Duplication (x1 + x2) (y1 + y2)

combine :: [Entry] -> Duplication -> Duplication
combine = mappend . computeDuplication

computeDuplication :: [Entry] -> Duplication
computeDuplication [] = mempty
computeDuplication rs@(r:rest) = Duplication (sum . map sz $ rs) (sz r)
  where sz = toEnum . fromEnum . e_size

------------------------------------------------------------------------
