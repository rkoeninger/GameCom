module GameCom where

import Control.Arrow ((>>>))
import Memory
import ROM
import qualified CPU
import qualified PPU
import qualified APU

step :: MachineState -> MachineState
step = CPU.step >>> PPU.step >>> APU.step
