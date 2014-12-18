{-# LANGUAGE DeriveDataTypeable #-}

module Data.Torrent
	( Torrent(..)
	, TorrentInfo(..)
	, TorrentFile(..)
	, readTorrent
	, serializeTorrent
	, torrentSize
	) where

import Data.BEncode
import Data.BEncode.Parser
import Data.Binary
import Data.Generics

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map

data Torrent
	= Torrent
		{ tAnnounce     :: ByteString
		, tAnnounceList :: [ByteString]
		, tComment      :: ByteString
		, tCreatedBy    :: Maybe ByteString
		, tInfo         :: TorrentInfo
		}
	deriving (Show, Read, Typeable, Data)

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
		}
	deriving (Show, Read, Typeable, Data)

data TorrentFile
	= TorrentFile
		{ fileLength :: Int
		, filePath   :: [ByteString]
		}
	deriving (Show, Read, Typeable, Data)

instance Binary Torrent where
	put = put . serializeTorrent
	get = do
		e <- get
		case readTorrent e of
			Left err -> fail $ "Failed to parse torrent: " ++ err
			Right t  -> return t

-- | Size of the files in the torrent.
torrentSize :: Torrent -> Int
torrentSize torrent = case tInfo torrent of
	s@SingleFile{} -> tLength s
	MultiFile{tFiles=files} -> sum (map fileLength files)

readTorrent :: ByteString -> Either String Torrent
readTorrent inp = case bRead inp of
	Nothing -> Left "Not BEncoded"
	Just be -> runParser parseTorrent be

parseTorrent :: BParser Torrent
parseTorrent = do
	announce <- bbytestring $ dict "announce"
	creator <- optional $ bbytestring $ dict "created by"
	info <- dict "info"
	setInput info
	name <- bbytestring $ dict "name"
	pLen <- bint $ dict "piece length"
	pieces <- bbytestring $ dict "pieces"
	torrentInfo <- parseTorrentInfo name (fromIntegral pLen) pieces
	return $ Torrent announce [] BS.empty creator torrentInfo

parseTorrentInfo :: ByteString -> Int -> ByteString -> BParser TorrentInfo
parseTorrentInfo name pLen pieces = single <|> multi
  where
	single = do
		len <- bint $ dict "length"
		return $ SingleFile (fromIntegral len) name (fromIntegral pLen) pieces
	multi = do
		files <- list "files" $ do
			len <- bint $ dict "length"
			filePaths <- list "path" $ bbytestring token
			return $ TorrentFile (fromIntegral len) filePaths
		return $ MultiFile files name pLen pieces

serializeTorrent :: Torrent -> BEncode
serializeTorrent torrent = BDict $ Map.fromList
	[ ("announce", BString $ tAnnounce torrent)
	, ("comment", BString $ tComment torrent)
	, ("info", info)
	]
  where
	info = BDict $ Map.fromList $
		[ ("name", BString $ tName (tInfo torrent))
		, ("pieces", BString $ tPieces (tInfo torrent))
		, ("piece length", BInt $ fromIntegral $ tPieceLength (tInfo torrent))
		] ++ case tInfo torrent of
			SingleFile len _ _ _ ->
				[ ("length", BInt $ fromIntegral len) ]
			MultiFile files _ _ _ ->
				[ ("files", BList $ map serfile files) ]

	serfile file = BDict $ Map.fromList
		[ ("length", BInt (fromIntegral (fileLength file)))
		, ("path", BList (map BString $ filePath file))
		]
