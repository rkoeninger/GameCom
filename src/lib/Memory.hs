{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Memory where

import Data.Bits (Bits, (.|.), (.&.), shiftR, shiftL, complement, bit, testBit)
import Data.Vector.Persistent (Vector, update, index, fromList)
import qualified Data.Vector.Persistent as P
import Data.Word (Word8, Word16)

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

class Storage m where
    loadByte :: Word16 -> m -> Word8

    loadWord :: Word16 -> m -> Word16
    loadWord addr storage = b0 .|. (b1 `shiftL` 8)
        where b0 = byteToWord $ loadByte addr storage
              b1 = byteToWord $ loadByte (addr + 1) storage

    storeByte :: Word16 -> Word8 -> m -> m

    storeWord :: Word16 -> Word16 -> m -> m
    storeWord addr word = storeByte addr b0 . storeByte (addr + 1) b1
        where b0 = wordToByte $ word .&. 0xff
              b1 = wordToByte $ word `shiftR` 8

type RAM = Vector Word8

malloc :: Int -> RAM
malloc n = fromList (replicate n 0)

instance Storage RAM where
    loadByte addr ram =
        case index ram (fromIntegral addr) of
        Just value -> value
        _ -> error $ "invalid RAM address: " ++ show addr
    storeByte addr value ram = update (fromIntegral addr) value ram

data Sprite = Sprite {
    xPosition :: Word8,
    yPosition :: Word8,
    tileIndex :: Word8,
    attribute :: Word8
}

data SpriteSize = Size8x8 | Size8x16
data SpritePriority = AboveBackground | BelowBackground
data ScrollDirection = XDirection | YDirection
data PixelLayer = BackgroundLayer | SpriteLayer

data NametableAddress = NametableAddress {
    xIndex :: Word8,
    yIndex :: Word8,
    base :: Word16
}

data MachineState = MachineState {
    ram        :: RAM,
    aReg       :: Word8,
    xReg       :: Word8,
    yReg       :: Word8,
    sReg       :: Word8,
    flagReg    :: Word8,
    pcReg      :: Word16,
    controlReg :: Word8,
    maskReg    :: Word8,
    statusReg  :: Word8,
    oamData    :: RAM
}

defaultState = MachineState {
    ram        = malloc 2048,
    aReg       = 0x00,
    xReg       = 0x00,
    yReg       = 0x00,
    sReg       = 0xfd,
    flagReg    = unusedMask .|. irqMask,
    pcReg      = 0xc000,
    controlReg = 0x00,
    maskReg    = 0x00,
    statusReg  = 0x00,
    oamData    = malloc 256
}

modifyRAM f state = state { ram = f (ram state) }

setAReg value state = (setZN value state) { aReg = value }
setXReg value state = (setZN value state) { xReg = value }
setYReg value state = (setZN value state) { yReg = value }
setSReg value state = state { sReg = value }
setPCReg value state = state { pcReg = value }
setFlagReg value state = state { flagReg = (value .|. unusedMask) .&. (complement breakMask) }
setControlReg value state = state { controlReg = value }
setMaskReg value state = state { maskReg = value }
setStatusReg value state = state { statusReg = value }

setZN :: Word8 -> MachineState -> MachineState
setZN value = setZeroFlag (value == 0) . setNegativeFlag (testBit value 7)

setFlag :: Word8 -> Bool -> MachineState -> MachineState
setFlag mask value state = 
    let flags = flagReg state in
    state {
        flagReg =
            if value
                then flags .|. mask
                else flags .&. (complement mask)
    }

getFlag :: Word8 -> MachineState -> Bool
getFlag mask state = flagReg state .&. mask /= 0

carryBit, zeroBit, irqBit, decimalBit, breakBit, unusedBit, overflowBit, negativeBit :: Int
carryBit    = 0
zeroBit     = 1
irqBit      = 2
decimalBit  = 3
breakBit    = 4
unusedBit   = 5
overflowBit = 6
negativeBit = 7

carryMask, zeroMask, irqMask, decimalMask, breakMask, unusedMask, overflowMask, negativeMask :: Word8
carryMask    = bit carryBit
zeroMask     = bit zeroBit
irqMask      = bit irqBit
decimalMask  = bit decimalBit -- TODO: unused since BCD is disabled in NES
breakMask    = bit breakBit
unusedMask   = bit unusedBit -- NOTE: should always be 1 for some reason
overflowMask = bit overflowBit
negativeMask = bit negativeBit

setCarryFlag    = setFlag carryMask
setZeroFlag     = setFlag zeroMask
setIRQFlag      = setFlag irqMask
setDecimalFlag  = setFlag decimalMask
setBreakFlag    = setFlag breakMask
setOverflowFlag = setFlag overflowMask
setNegativeFlag = setFlag negativeMask

carryFlag    = getFlag carryMask
zeroFlag     = getFlag zeroMask
irqFlag      = getFlag irqMask
decimalFlag  = getFlag decimalMask
breakFlag    = getFlag breakMask
overflowFlag = getFlag overflowMask
negativeFlag = getFlag negativeMask

modifyAReg f state = setAReg (f $ aReg state) state
modifyXReg f state = setXReg (f $ xReg state) state
modifyYReg f state = setYReg (f $ yReg state) state
modifySReg f state = setSReg (f $ sReg state) state
modifyStatusReg f state = setStatusReg (f $ statusReg state) state

modifyPCReg :: (Word16 -> Word16) -> MachineState -> MachineState
modifyPCReg f state = setPCReg (f $ pcReg state) state

screenWidth       = 256
screenHeight      = 240
cyclesPerScanline = 114
vBlankScanline    = 241
lastScanline      = 261

xScrollOffset, yScrollOffset, vramAddrIncrement, spritePatternTableAddr, backgroundPatternTableAddr :: MachineState -> Word16
xScrollOffset state              = if testBit (controlReg state) 0 then screenWidth else 0x0000
yScrollOffset state              = if testBit (controlReg state) 1 then screenHeight else 0x0000
vramAddrIncrement state          = if testBit (controlReg state) 2 then 0x0020 else 0x0001
spritePatternTableAddr state     = if testBit (controlReg state) 4 then 0x1000 else 0x0000
backgroundPatternTableAddr state = if testBit (controlReg state) 5 then 0x1000 else 0x0000

spriteSize :: MachineState -> SpriteSize
spriteSize state = if testBit (controlReg state) 6 then Size8x16 else Size8x8

vBlankNMI :: MachineState -> Bool
vBlankNMI state = testBit (controlReg state) 7

showBackground state = testBit (maskReg state) 3
showSprites state    = testBit (maskReg state) 4
setSpriteOverflow value = if value then modifyStatusReg (.|. bit 5) else id
setSpriteZeroHit value  = if value then modifyStatusReg (.|. bit 6) else id
setInVBlank value       = if value then modifyStatusReg (.|. bit 7) else id

{-
    scroll: PpuScroll,  // PPUSCROLL: 0x2005
    addr: PpuAddr,      // PPUADDR: 0x2006
-}

dmaTransfer :: Word8 -> MachineState -> MachineState
dmaTransfer value state = do
    let start = byteToWord value `shiftL` 8
    let lookup = flip loadByte state . (+ start)
    state { oamData = P.fromList $ map lookup [0..255] }

instance Storage MachineState where
    loadByte addr state
        | addr <  0x2000 = loadByte (addr .&. 0x07ff) (ram state)
        | addr == 0x2000 = controlReg state
        | addr == 0x2001 = maskReg state
        | addr == 0x2002 = statusReg state
        | addr == 0x2003 = error "attempt to read from OAM Addr 0x2003"
        | addr == 0x2004 = error "attempt to read from OAM Data 0x2004"
        | addr == 0x4014 = error "attempt to read from DMA Trigger 0x4014"
        | otherwise = error $ "Storage MachineState loadByte: Address out of range: " ++ show addr

    storeByte addr value state
        | addr <  0x2000 = modifyRAM (storeByte (addr .&. 0x07ff) value) state
        | addr == 0x2000 = setControlReg value state
        | addr == 0x2001 = setMaskReg value state
        | addr == 0x2002 = setStatusReg value state
        | addr == 0x2003 = error "attempt to write to OAM Addr 0x2003"
        | addr == 0x2004 = error "attempt to write to OAM Data 0x2004"
        | addr == 0x4014 = dmaTransfer value state
        | otherwise = error $ "Storage MachineState storeByte: Address out of range: " ++ show addr
