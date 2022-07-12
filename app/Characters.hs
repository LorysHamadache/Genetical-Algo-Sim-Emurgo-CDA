module Characters where


-- Imports
import Types
import System.Random
import Environment


-- Tools

getx::(Float,Float) -> Float
getx (x,y) = x

gety::(Float,Float) -> Float
gety (x,y) = y

getRandomNextPosition:: Character -> Position
getRandomNextPosition c = (!!) list_direction random_value
    where 
        (x,y) = position c
        list_direction = [(x-1,y), (x+1,y),(x-1,y-1),(x+1,y-1),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y+1)]
        random_value = (fst $ randomR(0,7)(rdGen c))

ismove_inbounds:: Model -> Position -> Bool
ismove_inbounds mod pos
    | xpos < bound && xpos > - bound && ypos < bound && ypos > - bound = True
    | otherwise = False
    where
        bound = (getsize env)/2
        xpos = getx pos
        ypos = gety pos
        env = environment mod

-- Character Unitary Change 

update_rdGen::Character -> Character
update_rdGen c = c {rdGen = snd $ randomR(0,v)(rdGen c)}
    where
        v::Int
        v = (fst $ randomR(1111,9999999) gen) +  round (x*y*nrj *10000)
        (x,y) = position c
        nrj = fromIntegral $ (energy c)
        gen = rdGen c

update_position:: Character -> Position -> Character
update_position c pos = c {position = pos}

update_state::Character -> Character
update_state c 
    | energy c == 0 = c {state = Dead, energy = 0}
    | otherwise = c {energy = (energy c) - (speed c)}

---- Movement Logic
character_movement:: Model -> Character -> Character
character_movement model c
    | ismove_inbounds model next_pos = update_position c next_pos
    | otherwise = character_movement model c
    where
       env = environment model
       next_pos = getRandomNextPosition $ update_rdGen c        


update_character::Model -> Character -> Character
update_character model c = update_rdGen $ update_state $ character_movement model c




