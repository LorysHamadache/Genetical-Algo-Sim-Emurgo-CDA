module Render where

import Types
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate

-- TOOLS


getcolor:: Team -> Color
getcolor Red = red
getcolor Blue = blue
getcolor Green = green


-- MODEL
render_model:: Model -> Picture
render_model model@(Model env character_list _) =
    pictures $ (
        [render_env env]
        ++
        (map render_character character_list))

-- CHARACTER
render_character :: Character -> Picture
render_character c = if (state c == Dead) then Blank else
    translate (fst $ position c) (snd $ position c) 
    $ color (getcolor $ (team c)) 
    $ (circleSolid (size c))

-- ENVIRONMENT

render_box:: Environment -> Picture
render_box (Environment s _) = Color black box
    where 
        path = rectanglePath s s
        box = lineLoop path

render_env:: Environment -> Picture
render_env env = render_box env 