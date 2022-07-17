module Model where

-- Imports
import Types
import Characters
import Environment
import Graphics.Gloss.Data.ViewPort
import System.Random

-- Functions

update_model:: ViewPort -> Float -> Model -> Model
update_model _ _ model = model {
     character_list = map (\x -> update_character model x) (character_list model) }



-- Init Character


init_character:: Position -> StdGen -> Character
init_character p gen = basic_character {position = p, rdgen = gen}

init_characterGen:: Model -> (Model, [StdGen])
init_characterGen m = (new_model, list_gen)
     where
          base = mdseed m
          randoms =  randomRs (1000,99999999) mdseed
          list_gen = map mkStdGen (take 100 randoms)
          new_model = m {mdseed = mkStdGen (sum $ take 100 randoms)}

init_characterPos:: Model -> (Model,[Position])
init_characterPos m = (new_model, zipWith (tool_randomtoPos bound) list_side list_value)
     where
          bound = (envsize (environment m)/2) -1
          gen1 = mdseed m
          list_value =  take 100 (fst $ randomRs (-bound,bound) gen1)
          gen2 = mkStdGen (sum list_value)
          r2 = randomRs (1,4) gen2       
          list_side = take 100 (fst $  r2)
          new_model = m {mdseed = snd $ r2}


tool_randomtoPos:: Float -> Int -> Float -> Position
tool_randomtoPos bound side value
     | side == 1 = (-bound,value)
     | side == 2 = (value,bound)
     | side == 3 = (bound,value)
     | side == 4 = (value,-bound)




