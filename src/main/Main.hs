{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Foreign.C.Types
import GameCom (step)
import Memory (MachineState(..), Color, defaultState)
import PPU (getPixel)
import SDL.Vect
import qualified SDL

draw :: MachineState -> SDL.Window -> IO ()
draw state window =
    SDL.getWindowSurface window >>=
        (\ surface ->
            forM_ [0..255] (\ row ->
                forM_ [0..239] (\ col ->
                  do
                    let (r, g, b) = getPixel (row, col) state
                    let color = V4 r g b maxBound
                    let area = SDL.Rectangle (P (V2 row col)) (V2 1 1)
                    SDL.surfaceFillRect surface (Just area) color)))

main :: IO ()
main = do
    let state = defaultState
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow "GameCom" SDL.defaultWindow { SDL.windowInitialSize = V2 256 240 }
    SDL.showWindow window

    draw state window
    SDL.updateWindowSurface window

    threadDelay 5000000

    SDL.destroyWindow window
    SDL.quit
