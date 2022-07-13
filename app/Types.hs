module Types where


import System.Random


-- Model
data Model = Model {environment::Environment, character_list::[Character], mdseed::StdGen}

-- Character

data Team = Red | Blue | Green deriving Show

type Position = (Float,Float)
type MovementVector = (Float,Float)
data State = Dead | Alive deriving (Show,Eq)


data Character = Character {
    name::String,
    team::Team,
    generation::Int,
    state::State,
    energy:: Float,
    size::Float,
    speed::Float,
    position::Position,
    direction::MovementVector,
    rdGen::StdGen
} deriving (Show)


-- Environment

type Size = Float
data Environment = Environment Size Objects
type Objects = [Float]

