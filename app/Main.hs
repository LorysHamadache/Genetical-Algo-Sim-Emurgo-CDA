{-|
Module      : Main
Description : Entry Point of the program. Initialize Variables & start the simulation
-}


module Main where

-- Imports

import Types
import Render
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate
import System.Random
import System.Environment


import Model
import Environment
import Characters

-- LAUNCH WITH 
--cabal haddock --haddock-executables  


background = color_dark
(window_w, window_h) = (1200,1200)

-- | The Main function is the opening function of our program
-- It consists of defining the initial state of our model and starting the simulation function
main :: IO ()
main = do
  args <- getArgs
  let argsParserFunc = argsParser args
  
  if (fst  $ argsParserFunc "help")
    then printHelp
    else do
      let fps = fromIntegral $ if (fst  $ argsParserFunc "fps") then (snd  $ argsParserFunc "fps") else 200
      let env_size = fromIntegral $ if (fst  $ argsParserFunc "env_size") then (snd  $ argsParserFunc "env_size") else 1000
      let nb_character = if (fst  $ argsParserFunc "nb_char") then (snd  $ argsParserFunc "nb_char") else 50
      let nb_food = if (fst  $ argsParserFunc "nb_food") then (snd  $ argsParserFunc "nb_food") else 50
      
      c_list <- mapM initR_character (replicate nb_character env_size)
      food_list <- mapM initR_food (replicate nb_food env_size)
      let env1 = Environment env_size food_list
      let model1 = Model {
        environment = env1,
        character_list = c_list,
        current_tick = 1 ,
        gen = 0,
        tick_perGen = 1000}
      
      (screen_w,screen_h) <- getScreenSize -- ^ Get User Screen Size in order to center the window on start (Useful especially for big screen 5120x1440)
      let (center_w, center_h) = ((screen_w - window_w) `div` 2, (screen_h - window_h) `div` 2 )
      let window = InWindow "Lorys - Haskell Project " (window_w, window_h) (center_w, center_h) -- ^ Define the window of the simulation

      simulateIO window background fps model1 render_model update_model


-- | This function parses the arguments of the executabkes
argsParser :: [String] -> String -> (Bool,Int)
argsParser list arg
  | length filtered == 0 = (False,0)
  | otherwise = (True, read $ snd (head filtered))
  where
    removepunc = \x -> [ y | y <- x, not (y `elem` ",.?!-:;\"\'") ]
    pair_list = get_pairs (map removepunc list)
    filtered = filter (\x -> fst x == arg) pair_list

-- | This function print the Help in the terminal
printHelp:: IO ()
printHelp = do
      putStrLn ("LORYS'S PROJECT HELP")
      putStrLn("--help to display this help")
      putStrLn ("Enter --arg1 value1 for each argument, with value being an Integer")
      putStrLn ("args:")
      putStrLn ("fps          for the number of simulation per second, default 200")
      putStrLn ("env_size     for the size of the environment, default 1000")
      putStrLn ("nb_char      for the size of the population, default 50")
      putStrLn ("nb_food      for the number of food item per gen, default 50")