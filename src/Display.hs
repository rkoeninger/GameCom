module Display (display) where

import Graphics.UI.GLUT
import Control.Monad
import Cube
import Points

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  forM_ (points 7) $ \(x,y,z) ->
    preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      cube 0.1
  flush