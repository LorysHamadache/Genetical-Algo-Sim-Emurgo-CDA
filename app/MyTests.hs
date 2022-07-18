module MyTests where

import System.Random
import Control.Monad.State.Lazy

data Environment = Environment { name::String, nb_elem::Int, size::(Float,Float)} deriving (Show)


env_changeName:: String -> State Environment Environment
env_changeName new_name = do
  env <- get
  let new_env = env {name = new_name}
  put new_env
  return(new_env)


    