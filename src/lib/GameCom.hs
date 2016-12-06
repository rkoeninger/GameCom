module GameCom where

import Base
import Memory
import ROM
import qualified CPU
import qualified PPU
import qualified APU

step :: MachineState -> MachineState
step s0 = do
	let s1 = CPU.step s0
	let (r, s2) = PPU.step s1
	if PPU.vBlankResult r then
		CPU.nmi s2
	else if PPU.scanlineIrqResult r then
		CPU.irq s2
	else
		s2
