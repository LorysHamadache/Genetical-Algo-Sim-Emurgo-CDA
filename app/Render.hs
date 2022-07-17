module Render where

import Types
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate

-- TOOLS

color_dark = makeColorI 46 51 54 180
color_blue = makeColorI 64 129 167 255
color_green = makeColorI 123 155 79 255
color_orange = makeColorI 215 144 14 255
color_red = makeColorI 191 53 72 255

get_color:: Team -> Color
get_color T1 = color_red
get_color T2 = color_blue
get_color T3 = color_green


-- MODEL
render_model:: Model -> Picture
render_model model@(Model env character_list _) =
    pictures $ (
        [render_env env]
        ++
        (map render_character character_list))

-- CHARACTER


render_character :: Character -> Picture
render_character c = if (state c == Dead) then Blank else pictures [render_characterCircle c, render_characterName c]


render_characterCircle :: Character -> Picture
render_characterCircle c = 
    translate (fst $ position c) (snd $ position c) 
    $ color (get_color $ (team c)) 
    $ circleSolid (size c)

render_characterName::Character -> Picture
render_characterName c = 
    translate (fst $ position c) (snd $ position c)
    $ color (get_color $ (team c)) 
    $ scale 0.3 0.3
    $ text (name c)

-- ENVIRONMENT

render_envBox:: Environment -> Picture
render_envBox (Environment s _) =  pictures [box]
    where 
        path = rectanglePath (s+1) (s+1)
        box = Color white (lineLoop path)
        solidbox = Color green (rectangleSolid s s)

render_env:: Environment -> Picture
render_env env= pictures $ [render_envBox env] ++ (map render_food (objects env))

render_food:: Food -> Picture
render_food f = 
    translate (fst $ fposition f) (snd $ fposition f)
    $ color yellow 
    $ thickArc 20 160 (fsize f) ((fsize f)/3)