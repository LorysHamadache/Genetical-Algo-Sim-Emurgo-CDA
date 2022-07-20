
{-|
Module      : Character
Description : Module Containing all the main functions applied to the Character Data Type and its update methods in the model
-}

module Characters where



-- Imports
import Types
import System.Random
import Environment


-- * Movement Tool Functions

-- | Check if the position is inside the model/environment boundaries
is_MoveInbound:: Model -> Position -> Bool
is_MoveInbound mod pos
    | xpos < bound && xpos > - bound && ypos < bound && ypos > - bound = True
    | otherwise = False
    where
        bound = (get_size env)/2
        (xpos,ypos) = pos
        env = environment mod

-- | Get the next position of a Character based on its current position, its speed & the next movement (MovementVector)
get_position:: Character -> MovementVector -> Position
get_position c mv = (x + mx*spd, y + my*spd)
    where
        (x,y) = position c
        (mx,my) = mv
        spd = speed c
    
 -- | Normalize a vector Movement       
get_normalizedmovement::MovementVector  -> MovementVector
get_normalizedmovement (x,y)
    | (x,y) == (0,0) = (0,0)
    | otherwise = (x/denom, y/denom)
    where
        denom = max (abs x) (abs y)

-- | Provide the Euclidian Distance between 2 Position 
get_distance:: Position -> Position -> Float
get_distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)**2 + (y1-y2)**2

-- | Given a list of Food Item and a character, return the direction to the closest food item
get_directionFood:: [Food] -> Character -> MovementVector
get_directionFood food_list c = get_normalizedmovement (mx,my)
    where
        mappingfunc = \c fx fy -> if (get_distance (position c) (fposition fx) > get_distance (position c) (fposition fy))
                then fy else fx
        closest_f = foldr (mappingfunc c) (basic_food {fposition= (-9999999.9, -999999.9)}) food_list
        (cx,cy) = position c
        (mx,my) = ((fst $ fposition closest_f) - cx, (snd $ fposition closest_f) - cy)
        
-- * Update Fields Functions

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

-- * Movement Functions

-- | A function taking a Character and returning a potential random movement. The Character has a high chance of continuating in the same direction.
get_randomMovement:: Character -> IO MovementVector
get_randomMovement c = do
    let (mx,my) = direction c
    (new_mx, new_my) <- randomRIO ((-1.0,-1.0),(1.0,1.0))
    change_percentage <- randomRIO(0::Int,100) 
    if (change_percentage<5) 
        then return $ get_normalizedmovement (new_mx,new_my) 
        else return $ get_normalizedmovement (mx,my)

-- | Move the Character Randomely in a valid position based on its speed
move_characterRandomly:: Model -> Character -> IO Character
move_characterRandomly model c = do
    next_direction <- get_randomMovement c
    let next_position =  get_position c next_direction
    let move_isvalid = is_MoveInbound model next_position

    new_c <- if (move_isvalid && (next_direction /= (0,0)))
        then return $ c {position = next_position, direction = next_direction, energy = energy c - (speed c)/100}
        else move_characterRandomly model (c {direction = (- (fst $ next_position), - (snd $ next_position))})
            -- return $ c {direction = (- (fst $ next_position), - (snd $ next_position)), energy = energy c - (speed c)/100}
    return new_c

-- | Move the Character based on the direction input
move_inDirection:: MovementVector -> Character -> Character
move_inDirection mv c= c {position = next_position, direction = mv, energy = energy c - (speed c)/100}
    where
        next_position =  get_position c mv


move_character:: Model -> Character -> IO Character
move_character model c = do
    let food_list = objects $ environment model 
    let in_visionfood = filter (\f -> get_distance (fposition f) (position c) <= (visionfield c)) food_list
    if (length in_visionfood == 0)
        then move_characterRandomly model c
        else do
            let (cx,cy) = position c
            let food_objective = get_directionFood in_visionfood c
            return $ move_inDirection food_objective c
            
            


-- * Character Update
-- | Update the Character. This is run once every tick of the simulation
update_character::Model -> Character -> IO Character
update_character model c =   (move_character model c)
                            >>= return . update_state




