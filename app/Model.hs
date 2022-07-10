module Model where

-- Imports
import Characters
import Environment
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- Type Definition
data Model = Model Environment [Character]

-- Functions

render_model:: Model -> Picture
render_model model@(Model env character_list) =
    pictures $ (
        [render_env env]
        ++
        (map render_character character_list))


update_model:: ViewPort -> Float -> Model -> Model
update_model _ _ model@(Model env character_list) = 
    Model env (map (\x -> update_character env x) character_list )



