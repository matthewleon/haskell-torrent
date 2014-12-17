{-# OPTIONS -fglasgow-exts #-}
module Data.Torrent
    ( Torrent(..)
    , TorrentInfo(..)
    , TorrentFile(..)
    , readTorrent      -- :: ByteString -> Either String Torrent
    , serializeTorrent
    , torrentSize      -- :: Torrent -> Int
    ) where

-- import Network.URI

import Data.BEncode
import Data.BEncode.Parser
import Data.Torrent.SHA1
import Data.Maybe
import Data.Binary
import Data.Generics

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import qualified Data.Map as Map

data Torrent
    = Torrent
    { tAnnounce     :: ByteString
    , tAnnounceList :: [ByteString]
    , tComment      :: ByteString
    , tCreatedBy    :: Maybe ByteString
    , tInfo         :: TorrentInfo
    , tInfoHash     :: ByteString
    } deriving (Show, Read, Typeable, Data)

data TorrentInfo
    = SingleFile
    { tLength      :: Int
    , tName        :: ByteString
    , tPieceLength :: Int
    , tPieces      :: ByteString }
    | MultiFile
    { tFiles       :: [TorrentFile]
    , tName        :: ByteString
    , tPieceLength :: Int
    , tPieces      :: ByteString
    } deriving (Show, Read, Typeable, Data)

data TorrentFile
    = TorrentFile
    { fileLength :: Int
    , filePath   :: [ByteString]
    } deriving (Show, Read, Typeable, Data)

instance Binary Torrent where
    put = put . serializeTorrent
    get = do e <- get
             case readTorrent e of
               Left err -> fail $ "Failed to parse torrent: " ++ err
               Right t  -> return t


torrentSize :: Torrent -> Int
torrentSize torrent
    = case tInfo torrent of
        s@SingleFile{} -> tLength s
        MultiFile{tFiles=files} -> sum (map fileLength files)

{-
buri :: BParser BEncode -> BParser URI
buri p = do str <- bstring p
            case parseURI str of
              Nothing -> fail $ "Expected URI: " ++ str
              Just uri -> return uri
-}

readTorrent :: ByteString -> Either String Torrent
readTorrent inp
    = case bRead inp of
        Nothing -> Left "Not BEncoded"
        Just be -> runParser parseTorrent be

parseTorrent :: BParser Torrent
parseTorrent =
    do announce <- bbytestring $ dict "announce"
       creator  <- optional $ bbytestring $ dict "created by"
       info     <- dict "info"
       setInput info
       name     <- bbytestring $ dict "name"
       pLen     <- bint $ dict "piece length"
       pieces   <- bbytestring $ dict "pieces"
       torrentInfo      <- parseTorrentInfo name pLen pieces
       let infoHash = sha1 (BS.pack $ bShow info "")
       return $ Torrent announce [] BS.empty creator torrentInfo infoHash

parseTorrentInfo :: ByteString -> Int -> ByteString -> BParser TorrentInfo
parseTorrentInfo name pLen pieces
    = do len <- bint $ dict "length"
         return $ SingleFile len name pLen pieces
      <|>
      do files <- list "files" $ do len <- bint $ dict "length"
                                    filePaths <- list "path" $ bbytestring token
                                    return $ TorrentFile len filePaths
         return $ MultiFile files name pLen pieces

serializeTorrent :: Torrent -> BEncode
serializeTorrent torrent
    = BDict $ Map.fromList [("announce",BString $ tAnnounce torrent)
                           ,("comment", BString $ tComment torrent)
                           ,("info",info)
                           ]
      where info = BDict $ Map.fromList $ [("name", BString $ tName (tInfo torrent))
                                          ,("pieces", BString $ tPieces (tInfo torrent))
                                          ,("piece length", BInt $ tPieceLength (tInfo torrent))
                                          ] ++ case tInfo torrent of
                                                SingleFile len _ _ _ -> [("length", BInt len)]
                                                MultiFile files _ _ _ -> [("files", BList $ flip map files $ \file ->
                                                                               BDict $ Map.fromList [("length", BInt (fileLength file))
                                                                                                    ,("path", BList (map BString $ filePath file))]
                                                                          )]
