
{-|
Module      : Environment
Description : Module Containing all the main functions applied to the Environment Data Type and the update method passed to the Vizualization Library
-}

module Environment where

-- Imports
import Types


-- * Environment Field Toos

-- | Fetch the environment size
get_size:: Environment -> Size
get_size (Environment s _) = s


