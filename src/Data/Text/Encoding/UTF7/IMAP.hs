{-# LANGUAGE OverloadedStrings,
             ForeignFunctionInterface,
             MagicHash,
             UnliftedFFITypes #-}

-- #if __GLASGOW_HASKELL__ >= 702
-- {-# LANGUAGE Trustworthy #-}
-- #endif

module Data.Text.Encoding.UTF7.IMAP where

-- #if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
--  #else
-- import Control.Monad.ST (unsafeIOToST, unsafeSTToIO)
-- #endif

import Control.Exception        (evaluate, try)

import Data.ByteString          as B
import Data.ByteString.Internal as B hiding (c2w)
import Data.Text.Internal       (Text (..), safe)
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)

import Data.Text.Encoding       (Decoding)
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)

import Data.Text.Internal.Unsafe.Shift (shiftR)
import Data.Text.Unsafe         (unsafeDupablePerformIO)

import Data.Word                (Word8)
import Foreign.C.Types          (CSize(..))
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Marshal.Utils    (with)
import Foreign.Ptr              (Ptr, minusPtr, plusPtr)
import Foreign.Storable         (peek, poke)
import GHC.Base                 (ByteArray#, MutableByteArray#)
import qualified Data.Text.Array as A

-- | Decode a 'ByteString' containing UTF-7 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-7 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf7'' or
-- 'decodeUtf7With'.
decodeUtf7 :: ByteString -> Text
decodeUtf7 = decodeUtf7With strictDecode
{-# INLINE[0] decodeUtf7 #-}

-- | Decode a 'ByteString' containing UTF-7 encoded text.
--
-- If the input contains any invalid UTF-7 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf7' :: ByteString -> Either UnicodeException Text
decodeUtf7' = unsafeDupablePerformIO . try . evaluate . decodeUtf7With strictDecode
{-# INLINE decodeUtf7' #-}

decodeUtf7With :: OnDecodeError -> ByteString -> Text
decodeUtf7With onErr (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                curPtr' <- c_decode_utf7 (A.maBA dest) destOffPtr curPtr end
                if curPtr' == end
                  then do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  else do
                    x <- peek curPtr'
                    case onErr desc (Just x) of
                      Nothing -> loop $ curPtr' `plusPtr` 1
                      Just c -> do
                        destOff <- peek destOffPtr
                        w <- unsafeSTToIO $
                             unsafeWrite dest (fromIntegral destOff) (safe c)
                        poke destOffPtr (destOff + fromIntegral w)
                        loop $ curPtr' `plusPtr` 1
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Network.IMAP.UTF7.decodeUtf7With: Invalid UTF-7 stream"
{-# INLINE[0] decodeUtf7With #-}

encodeUtf7 :: Text -> ByteString
encodeUtf7 (Text arr off len)
  | len == 0  = B.empty
  | otherwise = unsafeDupablePerformIO $ do
    fp <- mallocByteString (len * 4) -- what's the longest a utf-7 could be?
    withForeignPtr fp $ \ptr ->
      with ptr $ \destPtr -> do
        c_encode_utf7 destPtr (A.aBA arr) (fromIntegral off) (fromIntegral len)
        newDest <- peek destPtr
        let utf7len = newDest `minusPtr` ptr
        if utf7len >= len `shiftR` 1
          then return (PS fp 0 utf7len)
          else do
            fp' <- mallocByteString utf7len
            withForeignPtr fp' $ \ptr' -> do
              memcpy ptr' ptr (fromIntegral utf7len)
              return (PS fp' 0 utf7len)

foreign import ccall unsafe "_hs_text_decode_utf7" c_decode_utf7
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "_hs_text_encode_utf7" c_encode_utf7
    :: Ptr (Ptr Word8) -> ByteArray# -> CSize -> CSize -> IO ()
