module ROM where

import Data.Word (Word8, Word16)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Text.Parsec.ByteString (Parser)
import qualified Memory as M

data Region = NTSC | PAL deriving (Eq, Show)

data ROM = ROM {
    -- header is 16 bytes
    -- First 4 bytes are 'N' 'E' 'S' '\x1a'
    --prgSize :: Word8, -- x16KB
    --chrSize :: Word8, -- x8KB
    ramSize :: Word8, -- x8KB
    mapper :: Word8,
    tranier :: Bool,
    persistent :: Bool,
    region :: Region,
    {- MMMMATPA
    /// * A: 0xx0: vertical arrangement/horizontal mirroring (CIRAM A10 = PPU A11)
    ///      0xx1: horizontal arrangement/vertical mirroring (CIRAM A10 = PPU A10)
    ///      1xxx: four-screen VRAM
    pub flags_6: u8,
    /// * V: If 0b10, all following flags are in NES 2.0 format
    /// * P: ROM is for the PlayChoice-10
    /// * U: ROM is for VS Unisystem
    pub flags_7: u8,
    pub flags_10: u8,-}
    -- Last 5 bytes are zero
    prg :: M.Ram,
    chr :: M.Ram
}

instance Eq ROM where
    _ == _ = False

instance Show ROM where
    show = const "ROM"

bite bytes def = maybe def Right (B.uncons bytes)

parseROM :: ByteString -> Either String ROM
parseROM bytes = do
    (b0, bytes) <- bite bytes (Left "file too short")
    (b1, bytes) <- bite bytes (Left "file too short")
    (b2, bytes) <- bite bytes (Left "file too short")
    (b3, bytes) <- bite bytes (Left "file too short")
    if [b0, b1, b2, b3] == [0x4e, 0x45, 0x53, 0x1a]
        then Left "magic matches"
        else Left "error"
