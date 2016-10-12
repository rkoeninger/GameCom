module CPU where

import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, complement)
import Data.Word (Word8, Word16)
import Memory

data Registers = Registers {
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

nmiVector   = 0xfffa :: Word16
resetVector = 0xfffc :: Word16
breakVector = 0xfffe :: Word16

getFlag :: Word8 -> Registers -> Bool
getFlag mask regs = (flagReg regs .&. mask) == 0

setFlag :: Word8 -> Bool -> Registers -> Registers
setFlag mask val regs =
    let flags = flagReg regs in
    regs {
        flagReg =
            if val
                then flags .|. mask
                else flags .&. (complement mask)
    }

setFlags :: Word8 -> Registers -> Registers
setFlags val regs = regs { flagReg = (val .|. 0x30) - 0x10 }

setZN :: Word8 -> Registers -> Registers
setZN val = (setFlag zeroMask $ val == 0) . (setFlag negativeMask $ (val .&. 0x80) /= 0)

loadIncPc :: Ram -> Registers -> IO (Registers, Word8)
loadIncPc ram regs = do
    let pc = pcReg regs
    val <- load ram pc
    return (regs { pcReg = pc + 1 }, val)

loadWordIncPc :: Ram -> Registers -> IO (Registers, Word16)
loadWordIncPc ram regs = do
    let pc = pcReg regs
    val <- loadWord ram pc
    return (regs { pcReg = pc + 2 }, val)

branch :: Ram -> Registers -> Bool -> IO Registers
branch ram regs cond =
    if cond
        then return regs
        else do
            (regs, disp) <- loadIncPc ram regs
            let pc = pcReg regs
            return $ regs { pcReg = pc + (byteToWord disp) }

push :: Ram -> Registers -> Word8 -> IO Registers
push ram regs val = do
    let s = sReg regs
    store ram (0x0100 + (byteToWord s)) val
    return $ regs { sReg = s - 1 }

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Ram -> Registers -> Word16 -> IO Registers
pushWord ram regs val = do
    let s = sReg regs
    storeWord ram (0x0100 + (byteToWord s) - 1) val
    return $ regs { sReg = s - 2 }

pop :: Ram -> Registers -> IO (Registers, Word8)
pop ram regs = do
    let s = sReg regs
    val <- load ram (0x0100 + (byteToWord s) + 1)
    return (regs { sReg = s + 1 }, val)

-- FIXME: see two functions up
popWord :: Ram -> Registers -> IO (Registers, Word16)
popWord ram regs = do
    let s = sReg regs
    val <- loadWord ram (0x0100 + (byteToWord s) + 1)
    return (regs { sReg = s + 2 }, val)

type Loader = Ram -> Registers -> IO (Registers, Word8)
type Storer = Ram -> Registers -> Word8 -> IO Registers
type Addresser = (Loader, Storer)

-- TODO: think about word sizes for addresses here

accumulatorMode :: Addresser
accumulatorMode = (loader, storer)
    where loader _ regs = return (regs, accReg regs)
          storer _ regs val = return $ regs { accReg = val }

immediateMode :: Addresser
immediateMode = (loader, storer)
    where loader = loadIncPc
          storer _ _ _ = error "Can't store in immediate mode"

memoryMode :: Word16 -> Addresser
memoryMode addr = (loader, storer)
    where loader ram regs = do
              val <- load ram addr
              return (regs, val)
          storer ram regs val = do
              store ram addr val
              return regs

zeroPageMode :: Ram -> Registers -> IO (Registers, Addresser)
zeroPageMode ram regs = do
    (regs, addr) <- loadIncPc ram regs
    return (regs, memoryMode (byteToWord addr))

zeroPageXMode :: Ram -> Registers -> IO (Registers, Addresser)
zeroPageXMode ram regs = do
    (regs, addr) <- loadIncPc ram regs
    let addr = addr + (xReg regs)
    return (regs, memoryMode (byteToWord addr))

zeroPageYMode :: Ram -> Registers -> IO (Registers, Addresser)
zeroPageYMode ram regs = do
    (regs, addr) <- loadIncPc ram regs
    let addr = addr + (yReg regs)
    return (regs, memoryMode (byteToWord addr))

absoluteMode :: Ram -> Registers -> IO (Registers, Addresser)
absoluteMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    return (regs, memoryMode addr)

absoluteXMode :: Ram -> Registers -> IO (Registers, Addresser)
absoluteXMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let addr = addr + byteToWord (xReg regs)
    return (regs, memoryMode addr)

absoluteYMode :: Ram -> Registers -> IO (Registers, Addresser)
absoluteYMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let addr = addr + byteToWord (yReg regs)
    return (regs, memoryMode addr)

indexedIndirectXMode :: Ram -> Registers -> IO (Registers, Addresser)
indexedIndirectXMode ram regs = do
    (regs, addr) <- loadIncPc ram regs
    let x = xReg regs
    addr <- loadWord ram (byteToWord addr + byteToWord x) -- FIXME: this needs some alternate behavior for zero page mem.rs:35
    return (regs, memoryMode addr)

indirectIndexedYMode :: Ram -> Registers -> IO (Registers, Addresser)
indirectIndexedYMode ram regs = do
    (regs, addr) <- loadIncPc ram regs
    let y = yReg regs
    addr <- loadWord ram (byteToWord addr) -- FIXME: this needs some alternate behavior for zero page mem.rs:35
    let addr = addr + (byteToWord y)
    return (regs, memoryMode addr)

lda (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { accReg = val }

ldx (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { xReg = val }

ldy (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { yReg = val }

sta (loader, storer) ram regs = storer ram regs (accReg regs)
stx (loader, storer) ram regs = storer ram regs (xReg regs)
sty (loader, storer) ram regs = storer ram regs (yReg regs)

adc (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let result = (byteToWord (accReg regs)) + (byteToWord val) + (if getFlag carryMask regs then 1 else 0)
    let regs = setFlag carryMask ((result .&. 0x0100) /= 0) regs
    let result8 = wordToByte result
    let a = accReg regs
    let overflow = (((a `xor` val) .&. 0x80) == 0) && (((a `xor` result8) .&. 0x80) == 0x80)
    let regs = setFlag overflowMask overflow regs
    return $ (setZN result8 regs) { accReg = result8 }

sbc (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let a = accReg regs
    let result = (byteToWord a) - (byteToWord val) - (if getFlag carryMask regs then 0 else 1)
    let regs = setFlag carryMask ((result .&. 0x0100) == 0) regs
    let result8 = wordToByte result
    let overflow = (((a `xor` result8) .&. 0x80) /= 0) && (((a `xor` val) .&. 0x80) == 0x80)
    let regs = setFlag overflowMask overflow regs
    return $ (setZN result8 regs) { accReg = result8 }

compareHelper loader ram regs x = do
    (regs, y) <- loader ram regs
    let result = (byteToWord x) - (byteToWord y)
    let regs = setFlag carryMask ((result .&. 0x0100) == 0) regs
    return $ (setZN (wordToByte result) regs)

cmp (loader, _) ram regs = compareHelper loader ram regs (accReg regs)
cpx (loader, _) ram regs = compareHelper loader ram regs (xReg regs)
cpy (loader, _) ram regs = compareHelper loader ram regs (yReg regs)

bitwiseHelper loader ram regs f = do
    (regs, val) <- loader ram regs
    let val = val `f` accReg regs
    return $ (setZN val regs) { accReg = val }

add (loader, _) ram regs = bitwiseHelper loader ram regs (.&.)
ora (loader, _) ram regs = bitwiseHelper loader ram regs (.|.)
eor (loader, _) ram regs = bitwiseHelper loader ram regs xor

bit (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    let a = accReg regs
    let regs = setFlag overflowMask ((val .&. 0x40) /= 0) .
               setFlag negativeMask ((val .&. 0x80) /= 0) .
               setFlag zeroMask ((val .&. a) == 0) $ regs
    return regs

shiftLeftHelper (loader, storer) ram regs lsb = do
    (regs, val) <- loader ram regs
    let carry = (val .&. 0x80) /= 0
    let result = (val `shiftL` 1) .|. (if lsb then 0x01 else 0x00)
    let regs = setZN result $ setFlag carryMask carry regs
    storer ram regs result

shiftRightHelper (loader, storer) ram regs msb = do
    (regs, val) <- loader ram regs
    let carry = (val .&. 0x01) /= 0
    let result = (val `shiftR` 1) .|. (if msb then 0x80 else 0x00)
    let regs = setZN result $ setFlag carryMask carry regs
    storer ram regs result

rol addresser ram regs = shiftLeftHelper addresser ram regs (getFlag carryMask regs)
ror addresser ram regs = shiftRightHelper addresser ram regs (getFlag carryMask regs)
asl addresser ram regs = shiftLeftHelper addresser ram regs False
lsr addresser ram regs = shiftRightHelper addresser ram regs False

inc (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let val = val + 1
    storer ram (setZN val regs) val

dec (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let val = val - 1
    storer ram (setZN val regs) val

inx regs = let x = xReg regs + 1 in (setZN x regs) { xReg = x }
dex regs = let x = xReg regs - 1 in (setZN x regs) { xReg = x }
iny regs = let y = yReg regs + 1 in (setZN y regs) { yReg = y }
dey regs = let y = yReg regs - 1 in (setZN y regs) { yReg = y }
tax regs = let acc = accReg regs in (setZN acc regs) { xReg = acc }
tay regs = let acc = accReg regs in (setZN acc regs) { yReg = acc }
txa regs = let x = xReg regs in (setZN x regs) { accReg = x }
tya regs = let y = yReg regs in (setZN y regs) { accReg = y }
txs regs = let x = xReg regs in regs { sReg = x }
tsx regs = let s = sReg regs in (setZN s regs) { xReg = s }
clc = setFlag carryMask False
sec = setFlag carryMask True
cli = setFlag irqMask False
sei = setFlag irqMask True
clv = setFlag overflowMask False
cld = setFlag decimalMask False
sed = setFlag decimalMask True
bpl ram regs = branch ram regs $ not $ getFlag negativeMask regs
bmi ram regs = branch ram regs $ getFlag negativeMask regs
bvc ram regs = branch ram regs $ not $ getFlag overflowMask regs
bvs ram regs = branch ram regs $ getFlag overflowMask regs
bcc ram regs = branch ram regs $ not $ getFlag carryMask regs
bcs ram regs = branch ram regs $ getFlag carryMask regs
bne ram regs = branch ram regs $ not $ getFlag zeroMask regs
beq ram regs = branch ram regs $ getFlag zeroMask regs

jmp ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    return $ regs { pcReg = addr }

jmpi ram regs = do
    (regs, addr) <- loadWordIncPc ram regs

    -- NOTE: apparently made necessary by bug in 6502 chip ???
    lo <- load ram addr
    hi <- load ram ((addr .&. 0xff00) .|. ((addr + 1) .&. 0x00ff))
    let addr = ((byteToWord hi) `shiftL` 8) .|. (byteToWord lo)

    return $ regs { pcReg = addr }

jsr ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let pc = pcReg regs
    regs <- pushWord ram regs (pc - 1)
    return $ regs { pcReg = addr }

rts ram regs = do
    (regs, addr) <- popWord ram regs
    return $ regs { pcReg = addr + 1 }

brk ram regs = do
    let pc = pcReg regs
    regs <- pushWord ram regs (pc + 1)
    let flags = flagReg regs
    regs <- push ram regs flags -- FIXME: FCEU sets BREAK_FLAG and U_FLAG here, why?
    let regs = setFlag irqMask True regs
    addr <- loadWord ram breakVector
    return $ regs { pcReg = addr }

rti ram regs = do
    (regs, flags) <- pop ram regs
    let regs = setFlags flags regs
    (regs, addr) <- popWord ram regs
    return $ regs { pcReg = addr } -- NOTE: no +1

pha ram regs = push ram regs (accReg regs)

pla ram regs = do
    (regs, val) <- pop ram regs
    return $ (setZN val regs) { accReg = val }

php ram regs = push ram regs (flagReg regs .|. breakMask)

plp ram regs = do
    (regs, flags) <- pop ram regs
    return $ setFlags flags regs

{-The main fetch-and-decode routine
    pub fn step(&mut self) {
        self.trace();

        let op = self.loadb_bump_pc();
        decode_op!(op, self);

        self.cy += CYCLE_TABLE[op as usize] as Cycles;
    }
-}

reset ram regs = do
    addr <- loadWord ram resetVector
    return (regs { pcReg = addr })

nmi ram regs = do
    let pc = pcReg regs
    let flags = flagReg regs
    regs <- pushWord ram regs pc
    regs <- push ram regs flags
    addr <- loadWord ram nmiVector
    return $ regs { pcReg = addr }

irq ram regs =
    if getFlag irqMask regs
        then return regs
        else do
            let pc = pcReg regs
            let flags = flagReg regs
            regs <- pushWord ram regs pc
            regs <- push ram regs flags
            addr <- loadWord ram breakVector
            return $ regs { pcReg = addr }

app :: (Addresser -> Ram -> Registers -> IO Registers)
    -> Ram
    -> Registers
    -> (Ram -> Registers -> IO (Registers, Addresser))
    -> IO Registers
app op ram regs builder = do
    (regs, addresser) <- builder ram regs
    op addresser ram regs

eval :: Ram -> Registers -> Word8 -> IO Registers
eval ram regs opCode =
    case opCode of
        0xa1 -> app lda ram regs indexedIndirectXMode
        0xa5 -> app lda ram regs zeroPageMode
        0xa9 ->     lda immediateMode ram regs
        0xad -> app lda ram regs absoluteMode
        0xb1 -> app lda ram regs indirectIndexedYMode
        0xb5 -> app lda ram regs zeroPageXMode
        0xb9 -> app lda ram regs absoluteYMode
        0xbd -> app lda ram regs absoluteXMode
        0xa2 ->     ldx immediateMode ram regs
        0xa6 -> app ldx ram regs zeroPageMode
        0xb6 -> app ldx ram regs zeroPageYMode
        0xae -> app ldx ram regs absoluteMode
        0xbe -> app ldx ram regs absoluteYMode
        0xa0 ->     ldy immediateMode ram regs
        0xa4 -> app ldy ram regs zeroPageMode
        0xb4 -> app ldy ram regs zeroPageXMode
        0xac -> app ldy ram regs absoluteMode
        0xbc -> app ldy ram regs absoluteXMode
        0x85 -> app sta ram regs zeroPageMode
        0x95 -> app sta ram regs zeroPageXMode
        0x8d -> app sta ram regs absoluteMode
        0x9d -> app sta ram regs absoluteXMode
        0x99 -> app sta ram regs absoluteYMode
        0x81 -> app sta ram regs indexedIndirectXMode
        0x91 -> app sta ram regs indirectIndexedYMode
        0x86 -> app stx ram regs zeroPageMode
        0x96 -> app stx ram regs zeroPageYMode
        0x8e -> app stx ram regs absoluteMode
        0x84 -> app sty ram regs zeroPageMode
        0x94 -> app sty ram regs zeroPageXMode
        0x8c -> app sty ram regs absoluteMode
        0x69 ->     adc immediateMode ram regs
        0x65 -> app adc ram regs zeroPageMode
        0x75 -> app adc ram regs zeroPageXMode
        0x6d -> app adc ram regs absoluteMode
        0x7d -> app adc ram regs absoluteXMode
        0x79 -> app adc ram regs absoluteYMode
        0x61 -> app adc ram regs indexedIndirectXMode
        0x71 -> app adc ram regs indirectIndexedYMode
        0xe9 ->     sbc immediateMode ram regs
        0xe5 -> app sbc ram regs zeroPageMode
        0xf5 -> app sbc ram regs zeroPageXMode
        0xed -> app sbc ram regs absoluteMode
        0xfd -> app sbc ram regs absoluteXMode
        0xf9 -> app sbc ram regs absoluteYMode
        0xe1 -> app sbc ram regs indexedIndirectXMode
        0xf1 -> app sbc ram regs indirectIndexedYMode
        0xc9 ->     cmp immediateMode ram regs
        0xc5 -> app cmp ram regs zeroPageMode
        0xd5 -> app cmp ram regs zeroPageXMode
        0xcd -> app cmp ram regs absoluteMode
        0xdd -> app cmp ram regs absoluteXMode
        0xd9 -> app cmp ram regs absoluteYMode
        0xc1 -> app cmp ram regs indexedIndirectXMode
        0xd1 -> app cmp ram regs indirectIndexedYMode
        0xe0 ->     cpx immediateMode ram regs
        0xe4 -> app cpx ram regs zeroPageMode
        0xec -> app cpx ram regs absoluteMode
        0xc0 ->     cpy immediateMode ram regs
        0xc4 -> app cpy ram regs zeroPageMode
        0xcc -> app cpy ram regs absoluteMode
        0x29 ->     add immediateMode ram regs
        0x25 -> app add ram regs zeroPageMode
        0x35 -> app add ram regs zeroPageXMode
        0x2d -> app add ram regs absoluteMode
        0x3d -> app add ram regs absoluteXMode
        0x39 -> app add ram regs absoluteYMode
        0x21 -> app add ram regs indexedIndirectXMode
        0x31 -> app add ram regs indirectIndexedYMode
        0x09 ->     ora immediateMode ram regs
        0x05 -> app ora ram regs zeroPageMode
        0x15 -> app ora ram regs zeroPageXMode
        0x0d -> app ora ram regs absoluteMode
        0x1d -> app ora ram regs absoluteXMode
        0x19 -> app ora ram regs absoluteYMode
        0x01 -> app ora ram regs indexedIndirectXMode
        0x11 -> app ora ram regs indirectIndexedYMode
        0x49 ->     eor immediateMode ram regs
        0x45 -> app eor ram regs zeroPageMode
        0x55 -> app eor ram regs zeroPageXMode
        0x4d -> app eor ram regs absoluteMode
        0x5d -> app eor ram regs absoluteXMode
        0x59 -> app eor ram regs absoluteYMode
        0x41 -> app eor ram regs indexedIndirectXMode
        0x51 -> app eor ram regs indirectIndexedYMode
        0x24 -> app bit ram regs zeroPageMode
        0x2c -> app bit ram regs absoluteMode
        0x2a ->     rol accumulatorMode ram regs
        0x26 -> app rol ram regs zeroPageMode
        0x36 -> app rol ram regs zeroPageXMode
        0x2e -> app rol ram regs absoluteMode
        0x3e -> app rol ram regs absoluteXMode
        0x6a ->     ror accumulatorMode ram regs
        0x66 -> app ror ram regs zeroPageMode
        0x76 -> app ror ram regs zeroPageXMode
        0x6e -> app ror ram regs absoluteMode
        0x7e -> app ror ram regs absoluteXMode
        0x0a ->     asl accumulatorMode ram regs
        0x06 -> app asl ram regs zeroPageMode
        0x16 -> app asl ram regs zeroPageXMode
        0x0e -> app asl ram regs absoluteMode
        0x1e -> app asl ram regs absoluteXMode
        0x4a ->     lsr accumulatorMode ram regs
        0x46 -> app lsr ram regs zeroPageMode
        0x56 -> app lsr ram regs zeroPageXMode
        0x4e -> app lsr ram regs absoluteMode
        0x5e -> app lsr ram regs absoluteXMode
        0xe6 -> app inc ram regs zeroPageMode
        0xf6 -> app inc ram regs zeroPageXMode
        0xee -> app inc ram regs absoluteMode
        0xfe -> app inc ram regs absoluteXMode
        0xc6 -> app dec ram regs zeroPageMode
        0xd6 -> app dec ram regs zeroPageXMode
        0xce -> app dec ram regs absoluteMode
        0xde -> app dec ram regs absoluteXMode
        0xe8 -> return $ inx regs
        0xca -> return $ dex regs
        0xc8 -> return $ iny regs
        0x88 -> return $ dey regs
        0xaa -> return $ tax regs
        0xa8 -> return $ tay regs
        0x8a -> return $ txa regs
        0x98 -> return $ tya regs
        0x9a -> return $ txs regs
        0xba -> return $ tsx regs
        0x18 -> return $ clc regs
        0x38 -> return $ sec regs
        0x58 -> return $ cli regs
        0x78 -> return $ sei regs
        0xb8 -> return $ clv regs
        0xd8 -> return $ cld regs
        0xf8 -> return $ sed regs
        0x10 -> bpl ram regs
        0x30 -> bmi ram regs
        0x50 -> bvc ram regs
        0x70 -> bvs ram regs
        0x90 -> bcc ram regs
        0xb0 -> bcs ram regs
        0xd0 -> bne ram regs
        0xf0 -> beq ram regs
        0x4c -> jmp ram regs
        0x6c -> jmpi ram regs
        0x20 -> jsr ram regs
        0x60 -> rts ram regs
        0x00 -> brk ram regs
        0x40 -> rti ram regs
        0x48 -> pha ram regs
        0x68 -> pla ram regs
        0x08 -> php ram regs
        0x28 -> plp ram regs
        0xea -> return regs
        _ -> error $ "Invalid op code: " ++ show opCode
