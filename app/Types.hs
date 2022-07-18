module Types where


import System.Random


-- Model
data Model = Model {environment::Environment, character_list::[Character]}

-- Character

data Team = T1 | T2 | T3 deriving Show

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
    direction::MovementVector
} deriving (Show)

basic_character = Character {
    name = "Default",
    team = T1,
    generation = 0,
    state = Alive,
    energy = 100,
    size = 10,
    speed = 10,
    position = (0,0),
    direction = (1,0)
    }


-- Environment

type Size = Float
data Environment = Environment {envsize::Size, objects::Objects}
type Objects = [Food]

data Food = Food {
    fname::String,
    fsize::Float,
    fenergy::Float,
    fposition::Position
}

basic_food = Food {
    fname = "Banana",
    fsize = 10,
    fenergy = 10,
    fposition = (0,0)
}