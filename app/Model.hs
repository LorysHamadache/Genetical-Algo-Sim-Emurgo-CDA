
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
     let clist = character_list model
     let next_tick = if (current_tick model == tick_perGen model) then 0 else (current_tick model) + 1
     let next_gen = if (current_tick model == tick_perGen model) then (gen model+1) else  gen model
     

     (newtick_clist,newtick_env) <- SM.runStateT (mapM (\x -> update_characterAtTick model x) clist) (environment model)
     newgen_clist <- init_characterGen model clist
     newgen_env <-  init_envGen model
     
     if (current_tick model /= 0)
          then return $ model {
                    environment = newtick_env,
                    character_list = newtick_clist, 
                    current_tick = next_tick,
                    gen = next_gen
                    }  
          else 
               return $ model {
                    environment = newgen_env,
                    character_list = newgen_clist, 
                    current_tick = next_tick,
                    gen = next_gen
                    }  








