module MyTests where

import Control.Monad.State.Lazy
import System.Random

data Environment = Environment { name::String, nb_elem::Int, size::(Float,Float)} deriving (Show) 
data Character = Character {c_name::String, c_size:: Int} deriving Show

data Model = Model {env::Environment, clist::[Character] } deriving Show


update_character:: Character -> StateT Environment IO Character
update_character c = do
  env1 <- get
  rd_num <- liftIO $ randomRIO(1,10)
  let c2 = c {c_size = rd_num}
  put (env1 {nb_elem = nb_elem env1 +1})
  return c2


update_model:: Model -> IO Model
update_model m = do
  let start_env = env m
  (end_clist, end_env) <-  runStateT  (mapM (\x -> update_character x) (clist m)) start_env
  return $ m {env = end_env, clist = end_clist}

--update_model:: Model -> IO Model
--update_model m = do
--  let start_env = env m
--  let end_clist = map(\x -> evalState (update_character x) start_env) (clist m)
--  end_env <- get
--  return $ m {env = end_env, clist = end_clist}

main :: IO ()
main = do
  let env1 = Environment "E1" 10 (100,100)
  let c1 = Character "L1" 10
  let c2 = Character "L2" 20
  let m1 = Model env1 [c1,c2]
  m2 <- update_model m1
  print (m2)
  
