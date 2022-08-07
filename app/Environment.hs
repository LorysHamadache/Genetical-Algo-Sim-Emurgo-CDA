
{-|
Module      : Environment
Description : Module Containing all the main functions applied to the Environment Data Type and the update method passed to the Vizualization Library
-}

module Environment where

-- Imports
import Types
import System.Random


-- * Environment Field Toos

-- | Fetch the environment size
get_size:: Environment -> Size
get_size (Environment s _) = s


init_envGen:: Model -> IO Environment
init_envGen model =  do
    let s = get_size $ environment model
    food_list <- mapM initR_food (replicate 50 s)
    let env1 = Environment s food_list
    return env1

initR_food:: Float -> IO Food
initR_food env_size = do
     x <- randomRIO ((10-env_size)/2, (env_size-10)/2)
     y <- randomRIO ((10-env_size)/2, (env_size-10)/2)
     return $ basic_food {fposition = (x,y)}

  
