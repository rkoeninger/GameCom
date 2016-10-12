module Memory (
    Ram,
    byteToWord,
    wordToByte,
    malloc,
    storeByte,
    loadByte,
    storeWord,
    loadWord)
where

import           Data.Bits                   ((.|.), (.&.), shiftR, shiftL)
import           Data.Word                   (Word8, Word16)
import           Data.Functor                ((<$>))
import           Control.Monad.Primitive     (PrimState)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

type Ram = U.MVector (PrimState IO) Word8

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

malloc :: Int -> IO Ram
malloc n = M.replicate n 0

storeByte :: Ram -> Word16 -> Word8 -> IO ()
storeByte ram index val = M.write ram (fromIntegral index) val

loadByte :: Ram -> Word16 -> IO Word8
loadByte ram index = M.read ram (fromIntegral index)

storeWord :: Ram -> Word16 -> Word16 -> IO ()
storeWord ram index word =
    let b0 = wordToByte $ word .&. 0xff in
    let b1 = wordToByte $ word `shiftR` 8 in
    storeByte ram index b0 >> storeByte ram (index + 1) b1

loadWord :: Ram -> Word16 -> IO Word16
loadWord ram index = do
    b0 <- byteToWord <$> loadByte ram index
    b1 <- byteToWord <$> loadByte ram (index + 1)
    return $ b0 .|. (b1 `shiftL` 8)
