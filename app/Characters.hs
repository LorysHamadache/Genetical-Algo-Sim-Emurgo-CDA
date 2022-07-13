module Characters where

--NOTES
-- Consensus in all the functions is that update_RdGen is called before any function using stdGen is called

-- Imports
import Types
import System.Random
import Environment


-- Tools

getx::(Float,Float) -> Float
getx (x,y) = x

gety::(Float,Float) -> Float
gety (x,y) = y

getRandomMovement:: Character -> Position
getRandomMovement c = (fst$movement, snd$movement)
    where
        spd = speed c
        rdgen = rdGen c
        movement = fst $ randomR((-spd,-spd),(spd,spd)) rdgen

getRandomNextPosition:: Character -> Character
getRandomNextPosition c = next_c {position = (x+dx,y+dy), direction = (dx,dy)}
    where 
        percentage_change_direction = 2
        percentage_change_direction::Int
        select_if_change =  (fst $ randomR(0,100)(rdGen c)) <= percentage_change_direction
        (x,y) = position c
        next_c = update_rdGen c
        (dx,dy) = if select_if_change then getRandomMovement next_c else direction c

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
        nrj = (energy c)
        gen = rdGen c

update_position:: Character -> Position -> Character
update_position c pos = c {position = pos}

update_state::Character -> Character
update_state c 
    | energy c <= 0 = c {state = Dead, energy = 0}
    | otherwise = c {energy = (energy c) - (speed c)/100}

---- Movement Logic
character_movement:: Model -> Character -> Character
character_movement model c
    | ismove_inbounds model next_pos && (direction new_c /= (0,0))= new_c
    | otherwise = character_movement model (c{direction = (-lx,-ly)})
    where
       new_c = getRandomNextPosition $ update_rdGen c 
       next_pos =  position new_c
       (lx,ly) = direction c

update_character::Model -> Character -> Character
update_character model c = update_rdGen $ update_state $ character_movement model c




