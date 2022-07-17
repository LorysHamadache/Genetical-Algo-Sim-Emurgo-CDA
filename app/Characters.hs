module Characters where

--NOTES
-- Consensus in all the functions is that update_RdGen is called before any function using stdGen is called (WITH _random in name)

-- Imports
import Types
import System.Random
import Environment



-- Tools

is_MoveInbound:: Model -> Position -> Bool
is_MoveInbound mod pos
    | xpos < bound && xpos > - bound && ypos < bound && ypos > - bound = True
    | otherwise = False
    where
        bound = (getsize env)/2
        (xpos,ypos) = pos
        env = environment mod

get_position:: Character -> MovementVector -> Position
get_position c mv = (x + mx*spd, y + my*spd)
    where
        (x,y) = position c
        (mx,my) = mv
        spd = speed c

-- Character Unitary Change 

update_rdgen::Character -> Character
update_rdgen c = c {rdgen = snd $ randomR(0,v)(rdgen c)}
    where
        v::Int
        v = (fst $ randomR(1111,9999999) gen) +  round (x*y*nrj *10000)
        (x,y) = position c
        nrj = (energy c)
        gen = rdgen c


update_state::Character -> Character
update_state c 
    | energy c <= 0 = c {state = Dead, energy = 0}
    | otherwise = c

update_energy::Character -> Float -> Character
update_energy c f
    | total > 100 = c {energy = 100}
    | total < 0 = c {energy = 0}
    where
        total = (energy c) + f 
---- Movement Logic


get_randomMovement:: Character -> MovementVector
get_randomMovement c = movement
    where
        gen = rdgen c
        (mx,my) = direction c
        (rmx,rmy,rperc) = fst $ randomR((-1.0,-1.0,0),(1.0,1.0,100)) (rdgen c)
        rperc::Int
        movement = if (rperc<= 5) then (rmx,rmy) else (mx,my)


move_characterRandomly:: Model -> Character -> Character
move_characterRandomly model c 
    | is_valid && (next_direction /= (0,0)) = c {position = next_position, direction = next_direction, energy = energy c - (speed c)/100}
    | otherwise = move_characterRandomly model (update_rdgen c)
    where
        next_direction = get_randomMovement $ update_rdgen c 
        next_position =  get_position c next_direction
        is_valid = is_MoveInbound model next_position

update_character::Model -> Character -> Character
update_character model c =  update_state $ move_characterRandomly model (update_rdgen c)




