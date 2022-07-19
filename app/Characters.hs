
{-|
Module      : Character
Description : Module Containing all the main functions applied to the Character Data Type and its update methods in the model
Copyright   : (c) Lorys Hamadache
-}
module Characters where



-- Imports
import Types
import System.Random
import Environment



-- | Check if the position is inside the model/environment boundaries
is_MoveInbound:: Model -> Position -> Bool
is_MoveInbound mod pos
    | xpos < bound && xpos > - bound && ypos < bound && ypos > - bound = True
    | otherwise = False
    where
        bound = (getsize env)/2
        (xpos,ypos) = pos
        env = environment mod

-- | Get the next position of a Character based on its current position, its speed & the next movement (MovementVector)
get_position:: Character -> MovementVector -> Position
get_position c mv = (x + mx*spd, y + my*spd)
    where
        (x,y) = position c
        (mx,my) = mv
        spd = speed c

-- | Update the state of the Character based on its energy. If the Energy is 0 or less, the state goes from Alive to Dead
update_state::Character -> Character
update_state c 
    | energy c <= 0 = c {state = Dead, energy = 0}
    | otherwise = c

-- | A Helper function to update the energy of a Character by a given float and make sure its stays in the bound [0,100]
update_energy::Character -> Float -> Character
update_energy c f
    | total > 100 = c {energy = 100}
    | total < 0 = c {energy = 0}
    where
        total = (energy c) + f 

-- | A function taking a Character and returning a potential random movement. The Character has a high chance of continuating in the same direction.
get_randomMovement:: Character -> IO MovementVector
get_randomMovement c = do
    let (mx,my) = direction c
    (new_mx, new_my) <- randomRIO ((-1.0,-1.0),(1.0,1.0))
    change_percentage <- randomRIO(0::Int,100) 
    if (change_percentage<5) 
        then return (new_mx,new_my) 
        else return (mx,my)

-- | Move the Character Randomely in a valid position based on its speed
move_characterRandomly:: Model -> Character -> IO Character
move_characterRandomly model c = do
    next_direction <- get_randomMovement c
    let next_position =  get_position c next_direction
    let move_isvalid = is_MoveInbound model next_position

    new_c <- if (move_isvalid && (next_direction /= (0,0)))
        then return $ c {position = next_position, direction = next_direction, energy = energy c - (speed c)/100}
        else move_characterRandomly model c
    return new_c
        
-- | Update the Character. This is run once every tick of the simulation
update_character::Model -> Character -> IO Character
update_character model c =   (move_characterRandomly model c)
                            >>= return . update_state




