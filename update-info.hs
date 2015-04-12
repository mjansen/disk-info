module Main where

import           Control.Applicative
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L
-- import qualified Data.Attoparsec.ByteString.Char8 as P
-- import qualified Data.ByteString.Builder          as B

import           Data.Monoid
import qualified Data.Map.Strict  as Map
-- import qualified Data.Set         as Set
import qualified Data.Traversable as T

-- import           Text.Printf (printf)

import           System.Process.Exts

-- import FileEntry

import FileInfo

type ByteString = BC.ByteString

main :: IO ()
main = do
  Just db1 <- fmap (Map.fromList . map (\ e -> (e_path e, e)) . parseState)
           <$> runCommandCleanly "find" [ ".", "-type", "f", "-printf", "%Ts %s %i - %p\n" ] BC.empty
  db2 <- Map.fromList . map (\ e -> (e_path e, e)) <$> readState "./Info/index"
  let db3 = Map.unionWith combine db1 db2
  db4 <- T.mapM fixChecksum db3
  L.writeFile "./Info/index.new" . L.concat . map (unparseEntry . snd) . Map.toList $ db4

combine :: Entry -> Entry -> Entry
combine e1@(Entry ts1 sz1 nd1 cs1 fn1) e2@(Entry ts2 sz2 nd2 cs2 fn2) =
  if ts1 == ts2 && sz1 == sz2 && nd1 == nd2 && fn1 == fn2
  then case (cs1, cs2) of
    (Nothing, Just cs) -> Entry ts2 sz2 nd1 (Just cs) fn2
    (Nothing, Nothing) -> e1
    _                  -> error $ "unexpected combination" ++ show (e1, e2)
  else e1 -- error $ "unexpected combination: " ++ show (e1, e2)

------------------------------------------------------------------------
