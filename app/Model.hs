module Model where

-- Imports
import Types
import Characters
import Environment
import Graphics.Gloss.Data.ViewPort

-- Functions

update_model:: ViewPort -> Float -> Model -> Model
update_model _ _ model = model {
     character_list = map (\x -> update_character model x) (character_list model) }



