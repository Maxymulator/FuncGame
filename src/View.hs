module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState _ _ _ _ Paused)   = Translate (-windowWidth*0.35) 0 (Text "Paused")
viewPure (GameState _ _ _ _ GameOver) = Translate (-windowWidth*0.35) 0 (Text "Game Over")
viewPure (GameState s t w _ Running)  = p <> q <> r
  where 
    p :: Picture
    p = getPicture s
    q :: Picture
    q = getPicture t
    r :: Picture
    r = getPicture w