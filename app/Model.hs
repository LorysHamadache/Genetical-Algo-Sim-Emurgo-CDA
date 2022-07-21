
{-|
Module      : Model
Description : Module Containing all the main functions applied to the Model Data Type and the update method passed to the Vizualization Library
-}


module Model where

-- Imports
import Types
import Characters
import Environment
import Graphics.Gloss.Data.ViewPort
import System.Random
import qualified Control.Monad.State.Lazy as SM



-- * Model Update
-- | Update the Model. This is run once every tick of the simulation
update_model:: ViewPort -> Float -> Model -> IO Model
update_model _ _ model = do
     let clist = character_list model -- [Character]
     (new_clist,new_env) <- SM.runStateT (mapM (\x -> update_character model x) clist) (environment model)
     let m_tick = if (current_tick model == tick_perGen model) then 0 else (current_tick model) + 1
     let m_gen = if (current_tick model == tick_perGen model) then (gen model+1) else  gen model
     return $ model {
          environment = new_env,
          character_list = new_clist, 
          current_tick = m_tick,
          gen = m_gen
          }  




-- | Initiate a Character Totally Randomely
init_character:: Float -> IO Character
init_character x = do

     let bound =  (x/2 )-1

     c_speed <- randomRIO (1.0,2.0)
     c_n <- randomRIO (1::Int,999999::Int)
     c_side <- randomRIO (1,4)
     c_value <- randomRIO (-bound,bound)
     c_mx <- randomRIO (0::Float, 1.0)
     c_my <- randomRIO (0::Float, 1.0)

     let c_name = "Default" ++ show(c_n)

     
     let c_pos = tool_randomtoPos bound c_side c_value
     let c_dir = (c_mx,c_my)
     return $ basic_character {name = c_name, speed = c_speed, position = c_pos, direction = c_dir}

init_food:: Float -> IO Food
init_food env_size = do
     x <- randomRIO ((10-env_size)/2, (env_size-10)/2)
     y <- randomRIO ((10-env_size)/2, (env_size-10)/2)
     return $ basic_food {fposition = (x,y)}

-- |  A small helper function helping to generate a position on the env box based on the boundaries, the side and a value
tool_randomtoPos:: Float -> Int -> Float -> Position
tool_randomtoPos bound side value
     | side == 1 = (-bound,value)
     | side == 2 = (value,bound)
     | side == 3 = (bound,value)
     | side == 4 = (value,-bound)




