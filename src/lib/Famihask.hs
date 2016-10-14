module Famihask (demo) where

import Memory

import           Data.Word (Word8)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed  as U

demo :: IO ()
demo = do
    line <- getLine
    let lbs = L.pack (map (toEnum.fromEnum) line)               -- Get all of the contents from stdin
    mutable <- malloc 256
    addBytes mutable lbs             -- Add all of the bytes from stdin
    vector <- U.unsafeFreeze mutable -- Freeze to get an immutable version
    U.zipWithM_ printFreq (U.enumFromTo 0 255) vector -- Print the frequency of each byte, In newer vectors: we can use imapM_

addBytes :: Ram -> L.ByteString -> IO ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte :: Ram -> Word8 -> IO ()
addByte v w = do
    oldCount <- loadByte v index     -- Read out the old count value
    storeByte v index (oldCount + 1) -- Write back the updated count value
  where
    index = fromIntegral w -- Indices in vectors are always Ints. Our bytes come in as Word8, so we need to convert them.

printFreq :: Int -> Word8 -> IO ()
printFreq index count = putStrLn $ "Frequency of byte " ++ show index ++ ": " ++ show count
