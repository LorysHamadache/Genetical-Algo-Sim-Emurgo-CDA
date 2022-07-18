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


get_randomMovement:: Character -> IO MovementVector
get_randomMovement c = do
    let (mx,my) = direction c
    (new_mx, new_my) <- randomRIO ((-1.0,-1.0),(1.0,1.0))
    change_percentage <- randomRIO(0::Int,100) 
    if (change_percentage<5) 
        then return (new_mx,new_my) 
        else return (mx,my)


move_characterRandomly:: Model -> Character -> IO Character
move_characterRandomly model c = do
    next_direction <- get_randomMovement c
    let next_position =  get_position c next_direction
    let move_isvalid = is_MoveInbound model next_position

    new_c <- if (move_isvalid && (next_direction /= (0,0)))
        then return $ c {position = next_position, direction = next_direction, energy = energy c - (speed c)/100}
        else move_characterRandomly model c
    return new_c
        

update_character::Model -> Character -> IO Character
update_character model c =   (move_characterRandomly model c)
                            >>= return . update_state




