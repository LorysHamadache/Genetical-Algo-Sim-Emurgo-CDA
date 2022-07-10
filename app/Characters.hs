module Characters where


-- Imports
import Graphics.Gloss
import Environment
import System.Random

-- Type Definition

type Position = (Float,Float)
data Team = Red | Blue | Green deriving Show
data State = Dead | Alive deriving Show


data Character = Character {
    name::String,
    state::State,
    position::Position,
    generation::Int,
    size::Float,
    energy:: Int,
    team::Team
    } deriving (Show)

-- Tools

getRandomDirection:: Character -> IO Position
getRandomDirection c  = rd
    where 
        rd = randomRIO (0,7) >>= return . (!!) list_direction
        (x,y) = position c
        list_direction = [(x-1,y), (x+1,y),(x-1,y-1),(x+1,y-1),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y+1)]

-- Character change 

onestep_character::  Character -> Position -> Character
onestep_character c pos 
    | energy c >= 1 = c {state = Dead, energy = 0}
    | otherwise = Character {
        name = name c,
        state = Alive,
        position = pos,
        generation = generation c,
        size = size c,
        energy = energy c - 1,
        team = team c
        }

-- Movement Logic
update_character:: Environment -> Character -> Character
update_character env c
    | direction >>= move_inbounds env = direction >>= onestep_character c
    | otherwise = update_character env c
    where
        direction = getRandomDirection c

---- Movement Logic
--update_character:: Environment -> Character -> Character
--update_character env c
--    | move_inbounds pos_topright env = onestep_character pos_topright c
--    | otherwise = onestep_character pos_botleft c
--    where
--        direction = getRandomDirection c       


move_inbounds:: Environment ->Position -> Bool
move_inbounds env pos
    | xpos < bound && xpos > - bound && ypos < bound && ypos > - bound = True
    | otherwise = False
    where
        bound = (getsize env)/2
        xpos = getx pos
        ypos = gety pos

-- Render

getx::(Float,Float) -> Float
getx (x,y) = x

gety::(Float,Float) -> Float
gety (x,y) = y

getcolor:: Team -> Color
getcolor Red = red
getcolor Blue = blue
getcolor Green = green

render_character :: Character -> Picture
render_character c = 
    translate (getx $ position c) (gety $ position c) 
    $ color (getcolor $ (team c)) 
    $ (circleSolid (size c))


