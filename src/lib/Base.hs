module Base where

import Data.Bits ((.|.), shiftL, shiftR)
import Data.Default (Default(..))
import Data.Word (Word8, Word16)
import Data.Vector.Persistent (Vector, index)
import qualified Data.Vector.Persistent as P

at :: (Integral n, Show n) => n -> Vector a -> a
at i v = case index v (fromIntegral i) of
    Just result -> result
    _ -> error $ "invalid vector index: " ++ show i

vector :: Default d => Int -> Vector d
vector n = P.fromList (replicate n def)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

transfer :: (a -> b) -> (b -> a -> c) -> a -> c
transfer from to state = to (from state) state

ifs :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifs condition consequent alternative x =
    if condition x
        then consequent x
        else alternative x

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(->>) :: (a -> b) -> (b -> a -> c) -> a -> c
(->>) f g = transfer f g

(.>>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>>) f g = g . f

(*>>) :: (a -> (b, c)) -> (b -> c -> d) -> a -> d
(*>>) f g = uncurry g . f

($>>) :: (s -> (a, s)) -> (s -> (b, s)) -> s -> ((a, b), s)
($>>) f g s0 = do
    let (x, s1) = f s0
    let (y, s2) = g s1
    ((x, y), s2)

infixl 0 |>
infixl 1 ->>
infixl 1 .>>
infixl 1 *>>
infixl 1 $>>

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

bytesToWord :: Word8 -> Word8 -> Word16
bytesToWord b0 b1 = byteToWord b0 .|. byteToWord b1 `shiftL` 8

wordToBytes :: Word16 -> (Word8, Word8)
wordToBytes w = (wordToByte w, wordToByte (w `shiftR` 8))
