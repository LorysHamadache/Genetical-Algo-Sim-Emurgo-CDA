module Tests where

import Model
import Characters
import Environment

-- Test Set

model_size = 1000
c1 = Character "Lorys" Alive (-100,0) 0 10 100 Blue
character_list = [c1]
env1 = Environment model_size []
model1 = Model env1 character_list


