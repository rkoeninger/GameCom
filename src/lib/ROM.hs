module ROM (Mirroring(..), ROM(..), Region(..), parseROM) where

import Base
import Data.Attoparsec.ByteString (word8, anyWord8, string)
import Data.Attoparsec.ByteString as A
import Data.Bits ((.|.), (.&.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString, pack, unpack)
import Data.Default (Default(..))
import Data.Sequence (Seq, empty, fromList)
import Data.Word (Word8, Word16)

data Mirroring = Horizontal | Vertical | FourScreen deriving (Eq, Show)

data Region = NTSC | PAL deriving (Eq, Show)

data ROM = ROM {
    mirroring  :: Mirroring,
    trainer    :: Bool,
    persistent :: Bool,
    inesMapper :: Word8,
    mapper     :: Word8,
    playChoice :: Bool,
    unisystem  :: Bool,
    ramSize    :: Word16,
    region     :: Region,
    prg        :: Seq Word8,
    chr        :: Seq Word8
} deriving (Eq, Show)

instance Default ROM where
    def = ROM {
        mirroring  = Horizontal,
        trainer    = True,
        persistent = False,
        inesMapper = 0,
        mapper     = 0,
        playChoice = False,
        unisystem  = False,
        ramSize    = 0,
        region     = NTSC,
        prg        = empty,
        chr        = empty
    }

pROM = do
    -- Header is 16 bytes
    string $ pack [0x4e, 0x45, 0x53, 0x1a] -- "NES\x1a"
    prgSize16KB <- anyWord8
    chrSize8KB  <- anyWord8
    flags6      <- anyWord8
    flags7      <- anyWord8
    ramSize8KB  <- anyWord8
    flags9      <- anyWord8
    A.take 6 -- ignore flags10 and last 5 are just zeroes

    prgBytes <- A.take (fromIntegral prgSize16KB `shiftL` 14)
    chrBytes <- A.take (fromIntegral chrSize8KB `shiftL` 13)

    return ROM {
        mirroring = if testBit flags6 3 then FourScreen else
                    if testBit flags6 0 then Vertical else Horizontal,
        trainer    = testBit flags6 2,
        persistent = testBit flags6 1,
        inesMapper = flags6 `shiftR` 4,
        mapper     = (flags7 .&. 0xf0) .|. (flags6 `shiftR` 4),
        playChoice = testBit flags7 1,
        unisystem  = testBit flags7 0,
        ramSize    = byteToWord ramSize8KB `shiftL` 13,
        region     = if testBit flags9 0 then PAL else NTSC,
        prg        = fromList $ unpack $ prgBytes,
        chr        = fromList $ unpack $ chrBytes
    }

parseROM :: ByteString -> Either String ROM
parseROM = parseOnly pROM
