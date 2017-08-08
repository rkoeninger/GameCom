{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Default (Default(..))
import Foreign.C.Types
import GameCom (step)
import Mapper
import Memory (MachineState(..), Color)
import PPU (getPixel)
import SDL.Vect
import qualified SDL

draw :: MachineState -> SDL.Window -> IO ()
draw state window =
    SDL.getWindowSurface window >>=
        (\ surface ->
            forM_ [0..255] (\ x ->
                forM_ [0..239] (\ y ->
                  do
                    let (r, g, b) = getPixel (x, y) state
                    let color = V4 r g b maxBound
                    let area = SDL.Rectangle (P (V2 x y)) (V2 1 1)
                    SDL.surfaceFillRect surface (Just area) color)))

main :: IO ()
main = do
    let state = def
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow "GameCom" SDL.defaultWindow { SDL.windowInitialSize = V2 256 240 }
    SDL.showWindow window

    draw state window
    SDL.updateWindowSurface window

    threadDelay 5000000

    SDL.destroyWindow window
    SDL.quit
