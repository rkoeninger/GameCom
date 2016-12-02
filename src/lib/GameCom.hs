module GameCom where

import Control.Arrow ((>>>))

import Base
import Memory
import ROM
import qualified CPU
import qualified PPU
import qualified APU

step :: MachineState -> MachineState
step = CPU.step >>> PPU.step >>> snd >>> APU.step
