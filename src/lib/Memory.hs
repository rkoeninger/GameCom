module Memory where

import Data.Bits ((.|.), (.&.), shiftR, shiftL, complement)
import Data.Word (Word8, Word16)
import Data.Functor ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

byteToWord :: Word8 -> Word16
byteToWord = fromIntegral

wordToByte :: Word16 -> Word8
wordToByte = fromIntegral

{-
class Storage m where
    loadByte :: Word16 -> m -> Word8
    storeByte :: Word16 -> Word8 -> m -> m
-}

class Storage m where
    loadByte :: m -> Word16 -> IO Word8

    loadWord :: m -> Word16 -> IO Word16
    loadWord storage index = do
        b0 <- byteToWord <$> loadByte storage index
        b1 <- byteToWord <$> loadByte storage (index + 1)
        return $ b0 .|. (b1 `shiftL` 8)

    storeByte :: m -> Word16 -> Word8 -> IO ()

    storeWord :: m -> Word16 -> Word16 -> IO ()
    storeWord storage index word = do
        let b0 = wordToByte $ word .&. 0xff
        let b1 = wordToByte $ word `shiftR` 8
        storeByte storage index b0
        storeByte storage (index + 1) b1

data RAM = RAM (U.MVector (PrimState IO) Word8)

newRAM :: Int -> IO RAM
newRAM n = RAM <$> M.replicate n 0

instance Storage RAM where
    loadByte  (RAM ram) index = M.read  ram (fromIntegral index)
    storeByte (RAM ram) index = M.write ram (fromIntegral index)

data CPURegsData = CPURegsData {
    accReg  :: Word8,
    xReg    :: Word8,
    yReg    :: Word8,
    sReg    :: Word8,
    flagReg :: Word8,
    pcReg   :: Word16
}

carryMask    = 0x01 :: Word8
zeroMask     = 0x02 :: Word8
irqMask      = 0x04 :: Word8
decimalMask  = 0x08 :: Word8
breakMask    = 0x10 :: Word8
overflowMask = 0x40 :: Word8
negativeMask = 0x80 :: Word8

type CPURegs = IORef CPURegsData

newCPURegs :: IO CPURegs
newCPURegs = newIORef CPURegsData {
    accReg  = 0x00,
    xReg    = 0x00,
    yReg    = 0x00,
    sReg    = 0xfd,
    flagReg = 0x24,
    pcReg   = 0xc000
}

setFlag_ mask val regs =
    let flags = flagReg regs in
    regs {
        flagReg =
            if val
                then flags .|. mask
                else flags .&. (complement mask)
    }

setFlags_ val regs = regs { flagReg = (val .|. 0x30) - 0x10 }

data Mem = Mem {
    ram          :: RAM,
    aRegister    :: IORef Word8,
    xRegister    :: IORef Word8,
    yRegister    :: IORef Word8,
    sRegister    :: IORef Word8,
    flagRegister :: IORef Word8,
    pcRegister   :: IORef Word16,
    cpuRegs :: CPURegs
}

{-
data Mem = Mem {
    ram :: Vector Word8,
    aRg :: Word8,
    ...
    ppuControl :: Word8
    ...
}

type MemRef = IORef Mem

module CPU where
eval opCode mem :: Word8 -> Mem -> Mem

module Main where
run memRef :: IORef Mem -> IO ()

whileM_ (complement <$> isBreakSet) memRef

-}

getReg f mem = readIORef (f mem)
setReg f mem val = writeIORef (f mem) val

getAReg = getReg aRegister
setAReg = setReg aRegister
getXReg = getReg xRegister
setXReg = setReg xRegister
getYReg = getReg yRegister
setYReg = setReg yRegister

getCPUReg :: Mem -> (CPURegsData -> a) -> IO a
getCPUReg mem f = f <$> readIORef (cpuRegs mem)

setPCReg :: Mem -> Word16 -> IO ()
setPCReg mem val = modifyIORef' (cpuRegs mem) f
    where f regs = regs { pcReg = val }

getFlag :: Mem -> Word8 -> IO Bool
getFlag mem mask = do
    regs <- readIORef (cpuRegs mem)
    return $ (flagReg regs .&. mask) /= 0

setFlag :: Mem -> Word8 -> Bool -> IO ()
setFlag mem mask val = modifyIORef' (cpuRegs mem) (setFlag_ mask val)

setFlags :: Mem -> Word8 -> IO ()
setFlags mem val = modifyIORef' (cpuRegs mem) (setFlags_ val)

setZN :: Mem -> Word8 -> IO ()
setZN mem val = do
    setFlag mem zeroMask $ val == 0
    setFlag mem negativeMask $ (val .&. 0x80) /= 0

instance Storage Mem where
    loadByte mem addr =
        if addr < 0x2000 then loadByte (ram mem) addr else
        error "out of range"

    storeByte mem addr val =
        if addr < 0x2000 then storeByte (ram mem) addr val else
        if addr == 0x4014 then do -- Writing to 0x4014 triggers DMA transfer
            let start = (byteToWord val) `shiftL` 8
            let move ad = loadByte mem ad >>= storeByte mem 0x2004
            forM_ [start .. (start + 255)] move
        else
        error "out of range"
