module Main where

import           Control.Applicative
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Builder          as B

import           Data.Monoid
import qualified Data.Map.Strict  as Map
-- import qualified Data.Set         as Set
import qualified Data.Traversable as T

-- import           Text.Printf (printf)

import           System.Process.Exts

-- import FileEntry

type ByteString = BC.ByteString

data Entry = Entry
  { e_timeStamp :: {-# UNPACK #-} !Int
  , e_size      :: {-# UNPACK #-} !Int
  , e_inode     :: {-# UNPACK #-} !Int
  , e_checkSum  ::                !(Maybe ByteString)
  , e_path      :: {-# UNPACK #-} !ByteString
  } deriving (Eq, Ord, Show)

needsChecksum :: Entry -> Bool
needsChecksum (Entry _ _ _ Nothing _) = True
needsChecksum _                       = False

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

readState :: FilePath -> IO [Entry]
readState = (parseState <$>) . BC.readFile

parseState :: ByteString -> [Entry]
parseState content =
  case P.parseOnly parseFile content of
    Left msg -> error "could not parse file"
    Right rs -> rs
  
------------------------------------------------------------------------

parseEntry :: P.Parser Entry
parseEntry = do
  ts <- P.decimal
  P.char ' '
  sz <- P.decimal
  P.char ' '
  nd <- P.decimal
  P.char ' '
  cs <- P.choice [ const Nothing <$> P.char '-'
                 , Just <$> P.takeWhile (/= ' ')
                 ]
  P.char ' '
  fn <- P.takeWhile (/= '\n')
  P.char '\n'
  return $ Entry ts sz nd cs fn
  
parseFile :: P.Parser [Entry]
parseFile = do
  rs <- P.many' parseEntry
  P.endOfInput
  return rs

unparseEntry :: Entry -> L.ByteString
unparseEntry (Entry ts sz nd cs' fn) = B.toLazyByteString $
  foldr mappend mempty [ B.intDec ts
                       , B.char7 ' '
                       , B.intDec sz
                       , B.char7 ' '
                       , B.intDec nd
                       , B.char7 ' '
                       , case cs' of { Nothing -> B.char7 '-'; Just cs -> B.byteString cs }
                       , B.char7 ' '
                       , B.byteString fn
                       , B.char7 '\n'
                       ]

------------------------------------------------------------------------

-- find . -type f -printf "%Ts %s %i - %p\n" > Info/find

------------------------------------------------------------------------

findChecksum :: FilePath -> IO (Maybe ByteString)
findChecksum fName = do
  putStrLn $ "checking " ++ fName
  fmap (BC.take 32) <$> runCommandCleanly "md5sum" [ fName ] BC.empty

fixChecksum :: Entry -> IO Entry
fixChecksum e =
  case e_checkSum e of
    Nothing -> (\ cs -> e { e_checkSum = cs }) <$> findChecksum (BC.unpack . e_path $ e)
    Just _  -> return e
    
------------------------------------------------------------------------
