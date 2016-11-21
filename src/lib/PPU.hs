module PPU where

import Data.Bits (Bits, (.|.), (.&.), complement, testBit, shiftL, shiftR)
import Data.Word (Word8, Word16)
import Data.Vector.Persistent (Vector, index, update)
import Memory

screenWidth       = 256
screenHeight      = 240
cyclesPerScanline = 114
vBlankScanline    = 241
lastScanline      = 261

palette = [
    (0x7c, 0x7c, 0x7c), (0x00, 0x00, 0xfc), (0x00, 0x00, 0xbc), (0x44, 0x28, 0xbc),
    (0x94, 0x00, 0x84), (0xa8, 0x00, 0x20), (0xa8, 0x10, 0x00), (0x88, 0x14, 0x00),
    (0x50, 0x30, 0x00), (0x00, 0x78, 0x00), (0x00, 0x68, 0x00), (0x00, 0x58, 0x00),
    (0x00, 0x40, 0x58), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xbc, 0xbc, 0xbc), (0x00, 0x78, 0xf8), (0x00, 0x58, 0xf8), (0x68, 0x44, 0xfc),
    (0xd8, 0x00, 0xcc), (0xe4, 0x00, 0x58), (0xf8, 0x38, 0x00), (0xe4, 0x5c, 0x10),
    (0xac, 0x7c, 0x00), (0x00, 0xb8, 0x00), (0x00, 0xa8, 0x00), (0x00, 0xa8, 0x44),
    (0x00, 0x88, 0x88), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xf8, 0xf8, 0xf8), (0x3c, 0xbc, 0xfc), (0x68, 0x88, 0xfc), (0x98, 0x78, 0xf8),
    (0xf8, 0x78, 0xf8), (0xf8, 0x58, 0x98), (0xf8, 0x78, 0x58), (0xfc, 0xa0, 0x44),
    (0xf8, 0xb8, 0x00), (0xb8, 0xf8, 0x18), (0x58, 0xd8, 0x54), (0x58, 0xf8, 0x98),
    (0x00, 0xe8, 0xd8), (0x78, 0x78, 0x78), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xfc, 0xfc, 0xfc), (0xa4, 0xe4, 0xfc), (0xb8, 0xb8, 0xf8), (0xd8, 0xb8, 0xf8),
    (0xf8, 0xb8, 0xf8), (0xf8, 0xa4, 0xc0), (0xf0, 0xd0, 0xb0), (0xfc, 0xe0, 0xa8),
    (0xf8, 0xd8, 0x78), (0xd8, 0xf8, 0x78), (0xb8, 0xf8, 0xb8), (0xb8, 0xf8, 0xd8),
    (0x00, 0xfc, 0xfc), (0xf8, 0xd8, 0xf8), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00)]

getPixel :: (Int, Int) -> Screen -> Color
getPixel (x, y) screen = case index screen (x + y * 256) of
    Just color -> color
    _ -> error $ "Co-ordinates not on screen: " ++ show x ++ ", " ++ show y

putPixel :: (Int, Int) -> Color -> Screen -> Screen
putPixel (x, y) = update (x + y * 256)

spriteHeight :: MachineState -> Word8
spriteHeight state =
    case spriteSize state of
    Size8x8  -> 8
    Size8x16 -> 16

onScanline :: Sprite -> Word8 -> MachineState -> Bool
onScanline sprite y state = y >= spriteY && y < spriteY + spriteHeight state
    where spriteY = yPosition sprite

inBoundingBox :: Sprite -> Word8 -> Word8 -> MachineState -> Bool
inBoundingBox sprite x y state =
    x >= spriteX && x < spriteX && y >= spriteY && y < spriteY + spriteHeight state
    where spriteX = xPosition sprite
          spriteY = yPosition sprite

tiles :: Sprite -> MachineState -> SpriteTile
tiles sprite state =
    case spriteSize state of
    Size8x8  -> Tile8x8 $ byteToWord i .|. base
    Size8x16 -> Tile8x16 first $ first + 1
    where base = spritePatternTableAddr state
          i = tileIndex sprite
          addend = if testBit i 0 then 0x1000 else 0
          first = byteToWord (i .&. 0xf3) + addend

priority sprite    = if testBit sprite 5 then BelowBackground else AboveBackground
flipHorizontal sprite = testBit sprite 6
flipVertical sprite   = testBit sprite 7

renderScanline :: MachineState -> MachineState
renderScanline = id

nextScanline :: Int -> MachineState -> MachineState
nextScanline runToCycle state =
    if runToCycle >= cycleCount state
        then state
        else nextScanline (runToCycle - 1) (renderScanline state)

step :: MachineState -> MachineState
step state = nextScanline (cyclesPerScanline + cycleCount state) state

-- Returns the color (pre-palette lookup) of pixel (x,y) within the given tile.
getPatternPixel :: PixelLayer -> Word16 -> (Word8, Word8) -> MachineState -> Word8
getPatternPixel layer tile (x, y) state = do
    let offset = (tile `shiftL` 4) + byteToWord y + patternTableAddr layer state
    let plane0 = vramLoadByte offset state
    let plane1 = vramLoadByte (offset + 8) state
    let amount = 7 - (x `mod` 8)
    let bit0 = (plane0 `shiftR` fromIntegral amount) .&. 0x01
    let bit1 = (plane1 `shiftR` fromIntegral amount) .&. 0x01
    (bit1 `shiftL` 1) .|. bit0
