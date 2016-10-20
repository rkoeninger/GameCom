module CPU where

import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, complement)
import Data.Word (Word8, Word16)
import Control.Monad (forM_)
import Memory

nmiVector   = 0xfffa :: Word16
resetVector = 0xfffc :: Word16
breakVector = 0xfffe :: Word16

loadByteIncPc :: MachineState -> (Word8, MachineState)
loadByteIncPc state = (loadByte (pcReg state) state, modifyPCReg (+ 1) state)

loadWordIncPc :: MachineState -> (Word16, MachineState)
loadWordIncPc state = (loadWord (pcReg state) state, modifyPCReg (+ 2) state)

pushByte :: Word8 -> MachineState -> MachineState
pushByte val state = modifySReg (subtract 1) $ storeByte (0x0100 + (byteToWord $ sReg state)) val state

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Word16 -> MachineState -> MachineState
pushWord val state = modifySReg (subtract 2) $ storeWord (0x0100 + (byteToWord (sReg state - 1))) val state

popByte :: MachineState -> (Word8, MachineState)
popByte state = (loadByte (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 1) state)

-- FIXME: see two functions up
popWord :: MachineState -> (Word16, MachineState)
popWord state = (loadWord (0x0100 + byteToWord (sReg state) + 1) state, modifySReg (+ 2) state)

type Loader = MachineState -> (Word8, MachineState)
type Storer = Word8 -> MachineState -> MachineState
type Addresser = (Loader, Storer)
type AddresserBuilder = MachineState -> (Addresser, MachineState)
type Instruction = Addresser -> MachineState -> MachineState

nullMode :: AddresserBuilder
nullMode state = ((loader, storer), state)
    where loader _  = error "this instruction should not access memory"
          storer _ _ = error "this instruction should not access memory"

accumulatorMode :: AddresserBuilder
accumulatorMode state = ((loader, storer), state)
    where loader state = (aReg state, state)
          storer = setAReg

immediateMode :: AddresserBuilder
immediateMode state = ((loader, storer), state)
    where loader = loadByteIncPc
          storer _ _ = error "Can't store in immediate mode"

memoryMode :: Word16 -> Addresser
memoryMode addr = (loader, storer)
    where loader state = (loadByte addr state, state)
          storer val state = storeByte addr val state

mapFst f (x, y) = (f x, y)

zeroPageMode :: AddresserBuilder
zeroPageMode state = mapFst (memoryMode . byteToWord) (loadByteIncPc state)

zeroPageXMode :: AddresserBuilder
zeroPageXMode state = mapFst (memoryMode . byteToWord . (+ (xReg state))) (loadByteIncPc state)

zeroPageYMode :: AddresserBuilder
zeroPageYMode state = mapFst (memoryMode . byteToWord . (+ (yReg state))) (loadByteIncPc state)

absoluteMode :: AddresserBuilder
absoluteMode state = mapFst memoryMode (loadWordIncPc state)

absoluteXMode :: AddresserBuilder
absoluteXMode state = mapFst (memoryMode . (+ (byteToWord $ xReg state))) (loadWordIncPc state)

absoluteYMode :: AddresserBuilder
absoluteYMode state = mapFst (memoryMode . (+ (byteToWord $ yReg state))) (loadWordIncPc state)

indexedIndirectXMode :: AddresserBuilder
indexedIndirectXMode state = mapFst (memoryMode . flip loadWord state . (+ (byteToWord $ xReg state)) . byteToWord) (loadByteIncPc state)

-- FIXME: this needs some alternate behavior for zero page mem.rs:35
indirectIndexedYMode :: AddresserBuilder
indirectIndexedYMode state = mapFst (memoryMode . (+ (byteToWord $ yReg state)) . flip loadWord state . byteToWord) (loadByteIncPc state)

lda (loader, _) state = (uncurry setAReg) (loader state)
ldx (loader, _) state = (uncurry setXReg) (loader state)
ldy (loader, _) state = (uncurry setYReg) (loader state)
sta (_, storer) state = storer (aReg state) state
stx (_, storer) state = storer (xReg state) state
sty (_, storer) state = storer (yReg state) state

adc (loader, _) state = do
    let (val, state) = loader state
    let a = aReg state
    let result = byteToWord a + byteToWord val + (if carryFlag state then 1 else 0)
    let state = setCarryFlag (result .&. 0x0100 /= 0) state
    let result8 = wordToByte result
    let a = aReg state
    let overflow = (((a `xor` val) .&. 0x80) == 0) && (((a `xor` result8) .&. 0x80) == 0x80)
    setAReg result8 $ setOverflowFlag overflow $ state

sbc (loader, _) state = do
    let (val, state) = loader state
    let a = aReg state
    let result = byteToWord a - byteToWord val - (if carryFlag state then 0 else 1)
    let state = setCarryFlag (result .&. 0x0100 == 0) state
    let result8 = wordToByte result
    let a = aReg state
    let overflow = ((a `xor` result8) .&. 0x80 /= 0) && ((a `xor` val) .&. 0x80 == 0x80)
    setAReg result8 $ setOverflowFlag overflow $ state

compareHelper :: Loader -> (MachineState -> Word8) -> MachineState -> MachineState
compareHelper loader reg state = do
    let x = reg state
    let (y, state) = loader state
    let result = (byteToWord x) - (byteToWord y)
    setZN (wordToByte result) $ setCarryFlag (result .&. 0x0100 == 0) state

cmp (loader, _) = compareHelper loader aReg
cpx (loader, _) = compareHelper loader xReg
cpy (loader, _) = compareHelper loader yReg

bitwiseHelper :: Loader -> (Word8 -> Word8 -> Word8) -> MachineState -> MachineState
bitwiseHelper loader f state = do
    let (val, state) = loader state
    setAReg (val `f` aReg state) state

add (loader, _) = bitwiseHelper loader (.&.)
ora (loader, _) = bitwiseHelper loader (.|.)
eor (loader, _) = bitwiseHelper loader xor

bit (loader, _) state = setOverflowFlag (val .&. 0x40 /= 0) $
                        setNegativeFlag (val .&. 0x80 /= 0) $
                        setZeroFlag     (val .&. aReg st2 == 0) st2
    where (val, st2) = loader state

shiftLHelper :: Addresser -> MachineState -> Bool -> MachineState
shiftLHelper (loader, storer) state lsb = do
    let (val, state) = loader state
    let carry = val .&. 0x80 /= 0
    let result = (val `shiftL` 1) .|. (if lsb then 0x01 else 0x00)
    let state = setZN result $ setCarryFlag carry state
    storer result state

shiftRHelper :: Addresser -> MachineState -> Bool -> MachineState
shiftRHelper (loader, storer) state msb = do
    let (val, state) = loader state
    let carry = val .&. 0x01 /= 0
    let result = (val `shiftR` 1) .|. (if msb then 0x80 else 0x00)
    let state = setZN result $ setCarryFlag carry state
    storer result state

rol addresser state = shiftLHelper addresser state (carryFlag state)
ror addresser state = shiftRHelper addresser state (carryFlag state)
asl addresser state = shiftLHelper addresser state False
lsr addresser state = shiftRHelper addresser state False

inc (loader, storer) state = do
    let (val, state) = mapFst (+ 1) (loader state)
    storer val $ setZN val state

dec (loader, storer) state = do
    let (val, state) = mapFst (subtract 1) (loader state)
    storer val $ setZN val state

inx _ = modifyXReg (+ 1)
dex _ = modifyXReg (subtract 1)
iny _ = modifyYReg (+ 1)
dey _ = modifyYReg (subtract 1)
tax _ state = setXReg (aReg state) state
tay _ state = setYReg (aReg state) state
txa _ state = setAReg (xReg state) state
tya _ state = setAReg (yReg state) state
txs _ state = setSReg (xReg state) state
tsx _ state = setXReg (sReg state) state
clc _ = setCarryFlag    False
sec _ = setCarryFlag    True
cli _ = setIRQFlag      False
sei _ = setIRQFlag      True
clv _ = setOverflowFlag False
cld _ = setDecimalFlag  False
sed _ = setDecimalFlag  True

branch :: MachineState -> Bool -> MachineState
branch state cond =
    if cond
        then state
        else uncurry modifyPCReg $ mapFst ((+) . byteToWord) $ loadByteIncPc state

bpl _ state = branch state $ not $ negativeFlag state
bmi _ state = branch state $       negativeFlag state
bvc _ state = branch state $ not $ overflowFlag state
bvs _ state = branch state $       overflowFlag state
bcc _ state = branch state $ not $ carryFlag    state
bcs _ state = branch state $       carryFlag    state
bne _ state = branch state $ not $ zeroFlag     state
beq _ state = branch state $       zeroFlag     state

jmp _ = uncurry setPCReg . loadWordIncPc

    -- NOTE: apparently shift is made necessary by bug in 6502 chip ???
jpi _ state = setPCReg ((hi `shiftL` 8) .|. lo) st2
    where (addr, st2) = loadWordIncPc state
          lo = byteToWord $ loadByte addr st2
          hi = byteToWord $ loadByte ((addr .&. 0xff00) .|. ((addr + 1) .&. 0x00ff)) st2

jsr _ state = setPCReg val st3
    where (val, st2) = loadWordIncPc state
          pc = pcReg st2
          st3 = pushWord (pc - 1) st2

rts _ = uncurry setPCReg . mapFst (+ 1) . popWord

brk _ state = do
    let pc = pcReg state
    let state = pushWord (pc + 1) state
    let flags = flagReg state
    let state = setIRQFlag True $ pushByte flags state -- FIXME: FCEU sets BREAK_FLAG and U_FLAG here, why?
    setPCReg (loadWord breakVector state) state

rti _ state = setPCReg addr st4 -- NOTE: no +1
    where (flags, st2) = popByte state
          (addr, st4) = popWord (setFlagReg flags st2)

pha _ state = pushByte (aReg state) state
pla _ state = uncurry setAReg $ popByte state
php _ state = pushByte (flagReg state .|. breakMask) state
plp _ state = uncurry setFlagReg $ popByte state
nop _ = id

{-The main fetch-and-decode routine
    pub fn step(&mut self) {
        self.trace();

        let op = self.loadb_bump_pc();
        decode_op!(op, self);

        self.cy += CYCLE_TABLE[op as usize] as Cycles;
    }

reset ram regs = do
    addr <- loadWord ram resetVector
    return (regs { pcReg = addr })

nmi ram regs = do
    let pc = pcReg regs
    let flags = flagReg regs
    regs <- pushWord ram regs pc
    regs <- pushByte ram regs flags
    addr <- loadWord ram nmiVector
    return $ regs { pcReg = addr }

irq ram regs =
    if getFlag irqMask regs
        then return regs
        else do
            let pc = pcReg regs
            let flags = flagReg regs
            regs <- pushWord ram regs pc
            regs <- pushByte ram regs flags
            addr <- loadWord ram breakVector
            return $ regs { pcReg = addr }
-}

eval :: Word8 -> MachineState -> MachineState
eval opCode state = let (addresser, state) = builder state in op addresser state
    where (op, builder) = decode opCode

decode :: Word8 -> (Instruction, AddresserBuilder)
decode 0xa1 = (lda, indexedIndirectXMode)
decode 0xa5 = (lda, zeroPageMode)
decode 0xa9 = (lda, immediateMode)
decode 0xad = (lda, absoluteMode)
decode 0xb1 = (lda, indirectIndexedYMode)
decode 0xb5 = (lda, zeroPageXMode)
decode 0xb9 = (lda, absoluteYMode)
decode 0xbd = (lda, absoluteXMode)
decode 0xa2 = (ldx, immediateMode)
decode 0xa6 = (ldx, zeroPageMode)
decode 0xb6 = (ldx, zeroPageYMode)
decode 0xae = (ldx, absoluteMode)
decode 0xbe = (ldx, absoluteYMode)
decode 0xa0 = (ldy, immediateMode)
decode 0xa4 = (ldy, zeroPageMode)
decode 0xb4 = (ldy, zeroPageXMode)
decode 0xac = (ldy, absoluteMode)
decode 0xbc = (ldy, absoluteXMode)
decode 0x85 = (sta, zeroPageMode)
decode 0x95 = (sta, zeroPageXMode)
decode 0x8d = (sta, absoluteMode)
decode 0x9d = (sta, absoluteXMode)
decode 0x99 = (sta, absoluteYMode)
decode 0x81 = (sta, indexedIndirectXMode)
decode 0x91 = (sta, indirectIndexedYMode)
decode 0x86 = (stx, zeroPageMode)
decode 0x96 = (stx, zeroPageYMode)
decode 0x8e = (stx, absoluteMode)
decode 0x84 = (sty, zeroPageMode)
decode 0x94 = (sty, zeroPageXMode)
decode 0x8c = (sty, absoluteMode)
decode 0x69 = (adc, immediateMode)
decode 0x65 = (adc, zeroPageMode)
decode 0x75 = (adc, zeroPageXMode)
decode 0x6d = (adc, absoluteMode)
decode 0x7d = (adc, absoluteXMode)
decode 0x79 = (adc, absoluteYMode)
decode 0x61 = (adc, indexedIndirectXMode)
decode 0x71 = (adc, indirectIndexedYMode)
decode 0xe9 = (sbc, immediateMode)
decode 0xe5 = (sbc, zeroPageMode)
decode 0xf5 = (sbc, zeroPageXMode)
decode 0xed = (sbc, absoluteMode)
decode 0xfd = (sbc, absoluteXMode)
decode 0xf9 = (sbc, absoluteYMode)
decode 0xe1 = (sbc, indexedIndirectXMode)
decode 0xf1 = (sbc, indirectIndexedYMode)
decode 0xc9 = (cmp, immediateMode)
decode 0xc5 = (cmp, zeroPageMode)
decode 0xd5 = (cmp, zeroPageXMode)
decode 0xcd = (cmp, absoluteMode)
decode 0xdd = (cmp, absoluteXMode)
decode 0xd9 = (cmp, absoluteYMode)
decode 0xc1 = (cmp, indexedIndirectXMode)
decode 0xd1 = (cmp, indirectIndexedYMode)
decode 0xe0 = (cpx, immediateMode)
decode 0xe4 = (cpx, zeroPageMode)
decode 0xec = (cpx, absoluteMode)
decode 0xc0 = (cpy, immediateMode)
decode 0xc4 = (cpy, zeroPageMode)
decode 0xcc = (cpy, absoluteMode)
decode 0x29 = (add, immediateMode)
decode 0x25 = (add, zeroPageMode)
decode 0x35 = (add, zeroPageXMode)
decode 0x2d = (add, absoluteMode)
decode 0x3d = (add, absoluteXMode)
decode 0x39 = (add, absoluteYMode)
decode 0x21 = (add, indexedIndirectXMode)
decode 0x31 = (add, indirectIndexedYMode)
decode 0x09 = (ora, immediateMode)
decode 0x05 = (ora, zeroPageMode)
decode 0x15 = (ora, zeroPageXMode)
decode 0x0d = (ora, absoluteMode)
decode 0x1d = (ora, absoluteXMode)
decode 0x19 = (ora, absoluteYMode)
decode 0x01 = (ora, indexedIndirectXMode)
decode 0x11 = (ora, indirectIndexedYMode)
decode 0x49 = (eor, immediateMode)
decode 0x45 = (eor, zeroPageMode)
decode 0x55 = (eor, zeroPageXMode)
decode 0x4d = (eor, absoluteMode)
decode 0x5d = (eor, absoluteXMode)
decode 0x59 = (eor, absoluteYMode)
decode 0x41 = (eor, indexedIndirectXMode)
decode 0x51 = (eor, indirectIndexedYMode)
decode 0x24 = (bit, zeroPageMode)
decode 0x2c = (bit, absoluteMode)
decode 0x2a = (rol, accumulatorMode)
decode 0x26 = (rol, zeroPageMode)
decode 0x36 = (rol, zeroPageXMode)
decode 0x2e = (rol, absoluteMode)
decode 0x3e = (rol, absoluteXMode)
decode 0x6a = (ror, accumulatorMode)
decode 0x66 = (ror, zeroPageMode)
decode 0x76 = (ror, zeroPageXMode)
decode 0x6e = (ror, absoluteMode)
decode 0x7e = (ror, absoluteXMode)
decode 0x0a = (asl, accumulatorMode)
decode 0x06 = (asl, zeroPageMode)
decode 0x16 = (asl, zeroPageXMode)
decode 0x0e = (asl, absoluteMode)
decode 0x1e = (asl, absoluteXMode)
decode 0x4a = (lsr, accumulatorMode)
decode 0x46 = (lsr, zeroPageMode)
decode 0x56 = (lsr, zeroPageXMode)
decode 0x4e = (lsr, absoluteMode)
decode 0x5e = (lsr, absoluteXMode)
decode 0xe6 = (inc, zeroPageMode)
decode 0xf6 = (inc, zeroPageXMode)
decode 0xee = (inc, absoluteMode)
decode 0xfe = (inc, absoluteXMode)
decode 0xc6 = (dec, zeroPageMode)
decode 0xd6 = (dec, zeroPageXMode)
decode 0xce = (dec, absoluteMode)
decode 0xde = (dec, absoluteXMode)
decode 0xe8 = (inx, nullMode)
decode 0xca = (dex, nullMode)
decode 0xc8 = (iny, nullMode)
decode 0x88 = (dey, nullMode)
decode 0xaa = (tax, nullMode)
decode 0xa8 = (tay, nullMode)
decode 0x8a = (txa, nullMode)
decode 0x98 = (tya, nullMode)
decode 0x9a = (txs, nullMode)
decode 0xba = (tsx, nullMode)
decode 0x18 = (clc, nullMode)
decode 0x38 = (sec, nullMode)
decode 0x58 = (cli, nullMode)
decode 0x78 = (sei, nullMode)
decode 0xb8 = (clv, nullMode)
decode 0xd8 = (cld, nullMode)
decode 0xf8 = (sed, nullMode)
decode 0x10 = (bpl, nullMode)
decode 0x30 = (bmi, nullMode)
decode 0x50 = (bvc, nullMode)
decode 0x70 = (bvs, nullMode)
decode 0x90 = (bcc, nullMode)
decode 0xb0 = (bcs, nullMode)
decode 0xd0 = (bne, nullMode)
decode 0xf0 = (beq, nullMode)
decode 0x4c = (jmp, nullMode)
decode 0x6c = (jpi, nullMode)
decode 0x20 = (jsr, nullMode)
decode 0x60 = (rts, nullMode)
decode 0x00 = (brk, nullMode)
decode 0x40 = (rti, nullMode)
decode 0x48 = (pha, nullMode)
decode 0x68 = (pla, nullMode)
decode 0x08 = (php, nullMode)
decode 0x28 = (plp, nullMode)
decode 0xea = (nop, nullMode)
decode opCode = error $ "Invalid op code: " ++ show opCode
