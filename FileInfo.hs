{-# LANGUAGE DeriveGeneric #-}

module FileInfo where

import           Control.Applicative
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Builder          as B

import           Data.Maybe
import           Data.Monoid
import qualified Data.Map.Strict  as Map
-- import qualified Data.Set         as Set
import qualified Data.Traversable as T
import           Data.Serialize

-- import           Text.Printf (printf)

import           System.Process.Exts
import           System.Directory
import           System.FilePath

import           GHC.Generics

-- import FileEntry

type ByteString = BC.ByteString

data Entry = Entry
  { e_timeStamp :: {-# UNPACK #-} !Int
  , e_size      :: {-# UNPACK #-} !Int
  , e_inode     :: {-# UNPACK #-} !Int
  , e_checkSum  ::                !(Maybe ByteString)
  , e_path      :: {-# UNPACK #-} !ByteString
  } deriving (Eq, Ord, Show, Generic)

instance Serialize Entry;

needsChecksum :: Entry -> Bool
needsChecksum (Entry _ _ _ Nothing _) = True
needsChecksum _                       = False

------------------------------------------------------------------------

readState :: FilePath -> IO [Entry]
readState fName = do
  ok <- doesFileExist (fName ++ "-bin")
  if ok
    then readStateB (fName ++ "-bin")
    else parseState <$> BC.readFile fName

parseState :: ByteString -> [Entry]
parseState content =
  case P.parseOnly parseFile content of
    Left msg -> error "could not parse file"
    Right rs -> rs
  
readStateFile :: FilePath -> IO (Maybe [Entry])
readStateFile path = do
  ok <- doesFileExist path
  if ok
    then Just <$> readState path
    else return Nothing

readState' :: FilePath -> IO [Entry]
readState' path = do
  rs <- take 1 . catMaybes <$> mapM readStateFile [ path, path </> "index", path </> "Info/index" ]
  case rs of
    []     -> return []
    (rs:_) -> return rs

readStateB :: FilePath -> IO [Entry]
readStateB fName = do
  result <- decode <$> BC.readFile fName
  case result of
   Left msg -> error msg
   Right rs -> return rs

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
