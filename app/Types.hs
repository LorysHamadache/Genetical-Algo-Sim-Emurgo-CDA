
{-|
Module      : Types
Description : Module Containing all the main Data Types definition as well as basic values
-}


module Types where
import Data.List


-- * Low Level Types

data Team = T1 | T2 | T3 deriving Show
type Position = (Float,Float)
type MovementVector = (Float,Float)
data State = Dead | Alive deriving (Show,Eq)
type Size = Float

-- * Model
-- | Model contains an environment and a list of Character
data Model = Model {
    environment::Environment,
    character_list::[Character],
    current_tick::Int,
    gen::Int,
    tick_perGen:: Int
    }

-- * Character
-- | Character contains a number of characteristics about its state, position and identity
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
    visionfield::Float
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
    direction = (1,0),
    visionfield = 50.0
    }

sort_character_energy::Character -> Character -> Ordering
sort_character_energy c1 c2
    | ec1 > ec2 = GT
    | ec2 > ec2 = LT
    | otherwise = EQ
    where
        ec1 = energy c1
        ec2 = energy c2

-- * Environment
-- | Environment contains an environment size and a list of objects
data Environment = Environment {envsize::Size, objects::Objects}

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
-- | Objects contains all the necessary elements of the environment. For now only Food
type Objects = [Food]

