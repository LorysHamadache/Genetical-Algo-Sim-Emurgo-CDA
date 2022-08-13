{-|
Module      : Render
Description : Module containing all the functions to render all data types (Model, Character,Environment)
-}

module Render where

import Types
import Environment
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate

-- * Tool Functions

color_dark = makeColorI 46 51 54 180
color_blue = makeColorI 64 129 167 255
color_green = makeColorI 123 155 79 255
color_orange = makeColorI 215 144 14 255
color_red = makeColorI 191 53 72 255

-- | Get Color to render the Character based on its Team
get_color:: Team -> Color
get_color T1 = color_red
get_color T2 = color_blue
get_color T3 = color_green


-- | Round a specific number of digit
float_round:: Float -> Int -> Float
float_round f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- * Render Model
-- | Rendering the model by fetching the Picture of each element (Environment & [Character]) by calling their respective render function
render_model:: Model -> IO Picture
render_model model=
    return $ pictures $ (
        [render_env (environment model)]++
        (map render_character (character_list model))++
        [render_UI model] ++ [render_stats model]
        )
-- | Render the UI with the Picture rendered from the Tick number and the Generation Number
render_UI :: Model -> Picture
render_UI m =  translate  0 ((get_size $ environment m)/2 + 20)
    $ pictures [render_tick m, render_gen m]

-- | Render Tick Counter Text
render_tick:: Model -> Picture
render_tick m = translate (-500) (0)
    $ scale 0.3 0.3
    $ color white
    $ text ("Tick : " ++ show (current_tick m))

-- | Render Generation Counter Text
render_gen:: Model -> Picture
render_gen m = translate (100) (0)
    $ scale 0.3 0.3
    $ color white
    $ text ("Generation : " ++ show (gen m))

render_stats::Model -> Picture
render_stats m = translate (-500) ( - (get_size $ environment m)/2 - 50)
    $ scale 0.2 0.2
    $ color white
    $ text (t)
    where
        t = "Population: " ++ show (length $ character_list m) ++ 
            " Speed Average = " ++ show(rounded_avgspeed) ++
            " Max Speed" ++ show (rounded_maxspeed)
        speed_list =  map (\x -> speed x) (character_list m)
        avg_speed = (foldr (+) 0 speed_list) / (fromIntegral $ length speed_list)
        rounded_avgspeed = float_round avg_speed 2
        rounded_maxspeed = float_round (maximum speed_list) 2


-- * Render Character
-- | Rendering the Character by fetching the picture of its different component (Circle, Name)
render_character :: Character -> Picture
render_character c = if (state c == Dead) then Blank else pictures [render_characterCircle c] --,render_characterLabel c]

-- | Render the Character Circle based on its position & Color
render_characterCircle :: Character -> Picture
render_characterCircle c = 
    translate (fst $ position c) (snd $ position c) 
    $ color (get_color $ (team c)) 
    $ circleSolid (size c)

-- | Render the Character Name
render_characterLabel::Character -> Picture
render_characterLabel c = 
    translate (fst $ position c) (snd $ position c)
    $ color white 
    $ scale 0.2 0.2
    $ text (name c)


-- * Render Environment

-- | Render the environment by fetching the pictures of each of its element (Box, Objects)
render_env:: Environment -> Picture
render_env env= pictures $ [render_envBox env] ++ (map render_food (objects env))

-- | Render the box delimiting the play environment
render_envBox:: Environment -> Picture
render_envBox (Environment s _) =  pictures [box]
    where 
        path = rectanglePath (s+1) (s+1)
        box = Color white (lineLoop path)
        solidbox = Color green (rectangleSolid s s)


-- | Render the food objects
render_food:: Food -> Picture
render_food f = 
    translate (fst $ fposition f) (snd $ fposition f)
    $ color yellow 
    $ thickArc 20 160 (fsize f) ((fsize f)/3)
