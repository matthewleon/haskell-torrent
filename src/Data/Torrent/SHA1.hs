-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Torrent.SHA1
-- Copyright   :  (c) Lemmih 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.Torrent.SHA1 (sha1) where

import Foreign
import Foreign.C
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base as BS
import Data.ByteString (ByteString)

--void SHA1(sha1_byte *data, unsigned int len, sha1_byte digest[SHA1_DIGEST_LENGTH]);
foreign import ccall safe "SHA1" c_sha1 :: CString -> Int -> CString -> IO ()

sha1 :: ByteString -> ByteString
sha1 fs = unsafePerformIO $
          BS.unsafeUseAsCString fs $ \cstr ->
              do ret <- mallocBytes 20
                 c_sha1 cstr (BS.length fs) ret
                 fp <- newForeignPtr finalizerFree (castPtr ret)
                 return (BS.fromForeignPtr fp 20)

