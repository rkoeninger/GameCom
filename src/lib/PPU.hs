module PPU where

import Data.Bits (Bits, (.|.), (.&.), complement, testBit, shiftL, shiftR)
import Data.Maybe (isJust)
import Data.Vector.Persistent (Vector, index, update)
import Data.Word (Word8, Word16)

import Base
import Memory

data NametableAddress = NametableAddress {
    xIndex :: Word8,
    yIndex :: Word8,
    base :: Word16
}

data StepResult = StepResult {
    vBlankResult :: Bool, -- We entered VBLANK and must generate an NMI.
    newFrameResult :: Bool, -- We wrapped around to the next scanline.
    scanlineIrqResult :: Bool -- The mapper wants to execute a scanline IRQ.
}

screenHeight      = 240
-- screenWidth       = 256 -- TOOD: use this constant again. compiler warning about overflow 256::Word8
cyclesPerScanline = 114
vBlankScanline    = 241
lastScanline      = 261

colors :: [Color]
colors = [
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
getPixel (x, y) screen = at (x + y * 256) screen

putPixel :: (Integral i) => (i, i) -> Color -> MachineState -> MachineState
putPixel (x, y) color state = state { screen = update (fromIntegral x + (fromIntegral y) * 256) color (screen state) }

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
    Size8x8  -> Tile8x8 (byteToWord i .|. base)
    Size8x16 -> Tile8x16 first (first + 1)
    where base = spritePatternTableAddr state
          i = tileIndex sprite
          addend = if testBit i 0 then 0x1000 else 0
          first = byteToWord (i .&. 0xf3) + addend

spritePalette  sprite = attribute sprite .&. 0x03 + 4
priority       sprite = if testBit (attribute sprite) 5 then BelowBackground else AboveBackground
flipHorizontal sprite =    testBit (attribute sprite) 6
flipVertical   sprite =    testBit (attribute sprite) 7

nametableAddr :: Word16 -> Word16 -> NametableAddress
nametableAddr x y = do
    let xIndex = x `mod` 64
    let yIndex = y `mod` 60
    let base = case (xIndex >= 32, yIndex >= 30) of
               (False, False) -> 0x2000
               (True,  False) -> 0x2400
               (False, True)  -> 0x2800
               (True,  True)  -> 0x2c00
    NametableAddress {
        base = base,
        xIndex = wordToByte (xIndex `mod` 32),
        yIndex = wordToByte (yIndex `mod` 30)
    }

getSpriteInfo :: Word16 -> MachineState -> Sprite
getSpriteInfo index state =
    Sprite {
        yPosition = at (index * 4 + 0) (oamData state) + 1,
        tileIndex = at (index * 4 + 1) (oamData state),
        attribute = at (index * 4 + 2) (oamData state),
        xPosition = at (index * 4 + 3) (oamData state)
    }

-- Returns the color (pre-palette lookup) of pixel (x,y) within the given tile.
-- TODO : have x, y not be a tuple
getPatternPixel :: PixelLayer -> Word16 -> (Word8, Word8) -> MachineState -> (Word8, MachineState)
getPatternPixel layer tile (x, y) state = do
    let offset = (tile `shiftL` 4) + byteToWord y + patternTableAddr layer state
    let (plane0, stat2) = loadVramByte offset state
    let (plane1, stat3) = loadVramByte (offset + 8) stat2
    let amount = 7 - (x `mod` 8)
    let bit0 = (plane0 `shiftR` fromIntegral amount) .&. 0x01
    let bit1 = (plane1 `shiftR` fromIntegral amount) .&. 0x01
    ((bit1 `shiftL` 1) .|. bit0, stat3)

getBackgroundPixel :: Word8 -> MachineState -> (Maybe Color, MachineState)
getBackgroundPixel x state = do
    let x2 = byteToWord x + byteToWord (ppuScrollX state)
    let y2 = byteToWord (scanline state) + byteToWord (ppuScrollY state)
    let nt = nametableAddr (x2 `shiftR` 3) (y2 `shiftR` 3)
    let ntBase = base nt
    let ntXIndex = byteToWord (xIndex nt)
    let ntYIndex = byteToWord (yIndex nt)
    let xSub = wordToByte (x2 .&. 0x07)
    let ySub = wordToByte (y2 .&. 0x07)
    let (tile, stat2) = loadVramByte (ntBase + (ntXIndex `shiftL` 5) + ntYIndex) state
    let (patternColor, stat3) = getPatternPixel BackgroundLayer (byteToWord tile) (xSub, ySub) stat2
    if patternColor == 0 then
        (Nothing, stat2)
    else do
        let group = ((ntYIndex .&. 0xfffc) `shiftL` 1) + (ntXIndex `shiftR` 2)
        let (attr, stat4) = loadVramByte (ntBase + 0x03c0 + group) stat3
        let attrTableColor = case ((ntXIndex .&. 0x03) < 2, (ntYIndex .&. 0x03) < 2) of
                             (True,  True)  -> attr .&. 0x03
                             (False, True)  -> (attr `shiftR` 2) .&. 0x03
                             (True,  False) -> (attr `shiftR` 4) .&. 0x03
                             (False, False) -> (attr `shiftR` 6) .&. 0x03
        let tileColor = (attrTableColor `shiftL` 2) .|. patternColor
        let (paletteIndex, stat5) = loadVramByte (0x3f00 + byteToWord tileColor) stat4
        (Just(colors !! fromIntegral (paletteIndex .&. 0x3f)), stat5)

getSpritePixel :: [Maybe Word8] -> Word8 -> Bool -> MachineState -> (Maybe SpriteColor, MachineState)
getSpritePixel [] _ _ state = (Nothing, state)
getSpritePixel (Nothing : _) _ _ state = (Nothing, state)
getSpritePixel (Just spriteIndex : rest) x opaqueBackground state = do
    let sprite = getSpriteInfo (byteToWord spriteIndex) state
    if inBoundingBox sprite x (scanline state) state then
        getSpritePixel rest x opaqueBackground state
    else do
        let tile = case tiles sprite state of -- TODO: 8x16 not implemented
                   Tile8x8 t -> t
                   Tile8x16 t _ -> t
        let sl = scanline state
        let x2 = if flipHorizontal sprite then 7 - x + xPosition sprite else x - xPosition sprite
        let y2 = if flipVertical sprite then 7 - sl + yPosition sprite else sl - yPosition sprite
        let (patternColor, stat2) = getPatternPixel SpriteLayer tile (x2, y2) state
        if patternColor == 0 then
            getSpritePixel rest x opaqueBackground state
        else do
            let stat3 = if (spriteIndex == 0) && opaqueBackground then setSpriteZeroHit True stat2 else stat2
            let tileColor = (spritePalette sprite `shiftL` 2) .|. patternColor
            let (paletteIndex, stat4) = loadVramByte (0x3f00 + byteToWord tileColor) stat3
            let finalColor = colors !! fromIntegral (paletteIndex .&. 0x3f)
            (Just (priority sprite, finalColor), stat4)

computeVisibleSprites :: MachineState -> ([Maybe Word8], MachineState)
computeVisibleSprites state0 = do
        let (results, state) = recur [0 .. 63] [] state0
        let l = length results
        (results |> reverse |> (++ replicate (8 - l) Nothing), state)
    where recur [] results state = (results, state)
          recur (i : rest) results state = do
            let sprite = getSpriteInfo (byteToWord i) state
            if onScanline sprite (scanline state) state then
                if length results < 8 then
                    recur rest (Just i : results) state
                else
                    (results, setSpriteOverflow True state)
            else
                recur rest results state

-- TODO: set sprite zero on this frame or the next one?
startVBlank :: StepResult -> MachineState -> (StepResult, MachineState)
startVBlank result state = (newResult, newState)
    where newResult = result { vBlankResult = vBlankNMI state }
          newState = state |> setInVBlank True |> setSpriteZeroHit False

choosePixel :: Color -> Maybe Color -> Maybe SpriteColor -> Color
choosePixel baseColor Nothing Nothing = baseColor
choosePixel _ (Just color) Nothing = color
choosePixel _ (Just color) (Just (BelowBackground, _)) = color
choosePixel _ Nothing (Just (BelowBackground, color)) = color
choosePixel _ _ (Just (_, color)) = color

renderPixel :: Color -> [Maybe Word8] -> Word8 -> MachineState -> MachineState
renderPixel baseColor spriteIndicies x state0 = do
    let ifFlag s c f = if c s then f s else (Nothing, s)
    let (backgroundColor, state1) = ifFlag state0 showBackground (getBackgroundPixel x)
    let (spriteColor, state2) = ifFlag state1 showSprites (getSpritePixel spriteIndicies x (isJust backgroundColor))
    let color = choosePixel baseColor backgroundColor spriteColor
    putPixel (x, scanline state2) color state2

-- TODO: scrolling, mirroring
renderScanline :: MachineState -> MachineState
renderScanline state0 = do
    let (spriteIndicies, state1) = computeVisibleSprites state0
    let (baseColorIndex, state2) = loadVramByte 0x3f00 state1 |> mapFst (.&. 0x3f)
    let baseColor = colors !! fromIntegral baseColorIndex
    -- TODO: don't recompute tile for every pixel
    foldr (renderPixel baseColor spriteIndicies) state2 [0 .. 255]

nextScanline :: Int -> MachineState -> MachineState
nextScanline runToCycle state =
    if runToCycle >= cycleCount state
        then state
        else nextScanline (runToCycle - 1) (renderScanline state)

step :: MachineState -> MachineState
step state = nextScanline (cyclesPerScanline + cycleCount state) state
