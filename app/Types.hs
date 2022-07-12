module Types where


import System.Random


-- Model
data Model = Model {environment::Environment, character_list::[Character], mdseed::StdGen}

-- Character
type Position = (Float,Float)
data Team = Red | Blue | Green deriving Show
data State = Dead | Alive deriving Show

data Character = Character {
    name::String,
    state::State,
    position::Position,
    generation::Int,
    size::Float,
    speed::Int,
    energy:: Int,
    team::Team,
    rdGen::StdGen
}

-- Environment

type Size = Float
data Environment = Environment Size Objects
type Objects = [Float]

