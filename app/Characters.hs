
{-|
Module      : Character
Description : Module Containing all the main functions applied to the Character Data Type and its update methods in the model
-}

module Characters where



-- Imports
import Types
import System.Random
import Environment
import Data.List
import qualified Control.Monad.State.Lazy as SM


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
    | total >= 100 = c {energy = 100}
    | total < 0 = c {energy = 0}
    | otherwise = c {energy = total}
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
        then return $ c {position = next_position, direction = next_direction}
        else move_characterRandomly model (c {direction = (- (fst $ next_position), - (snd $ next_position))})
    return new_c

-- | Move the Character based on the direction input
move_inDirection:: MovementVector -> Character -> Character
move_inDirection mv c= c {position = next_position, direction = mv}
    where
        next_position =  get_position c mv


move_character:: Model -> Character -> IO Character
move_character model c = do
    let food_list = objects $ environment model 
    let in_visionfood = filter (\f -> get_distance (fposition f) (position c) <= (visionfield c)) food_list
    let new_c = update_energy c (negate (speed c/50))
    if (length in_visionfood == 0)
        then move_characterRandomly model new_c
        else do
            let (cx,cy) = position new_c
            let food_objective = get_directionFood in_visionfood new_c
            return $ move_inDirection food_objective new_c
            
-- * Behavior Functions

-- | Function updating the energy of a character based on the list of food he is eating
eat_food:: [Food] -> Character -> Character
eat_food flist c =  update_energy c e
    where e = (foldr (\f -> (+) (fenergy f)) 0 flist)


-- * Character Update

-- | Update the Character during a normal tick of the simulation (not Tick 0 of each generation).
update_characterAtTick::Model -> Character -> SM.StateT Environment IO Character
update_characterAtTick model c =  do
    moved <- SM.liftIO $ move_character model c                                                 
    env1 <- SM.get                                                                                
    let f_list1 = objects env1                                                                      
    let remaining_food = filter (\x -> get_distance (fposition x) (position moved) > 10 ) f_list1
    let eaten_food = filter (\x -> get_distance (fposition x) (position moved) <= 10 ) f_list1
    let moved_fed = eat_food eaten_food moved
    SM.put (env1 {objects = remaining_food})
    return $ (update_state moved_fed)

-- | Create a new generation based on the precedent, thanks to Genetic Algorithms


-- | Initiate a Character Totally Randomely
initR_character:: Float -> IO Character
initR_character x = do

     let bound =  (x/2)-1

     c_speed <- randomRIO (1.0,2.0)
     c_n <- randomRIO (1::Int,999999::Int)
     c_side <- randomRIO (1,4)
     c_value <- randomRIO (-bound,bound)
     c_mx <- randomRIO (0::Float, 1.0)
     c_my <- randomRIO (0::Float, 1.0)

     let c_name = "Default" ++ show(c_n)
     let c_pos = tool_randomtoPos bound c_side c_value
     let c_dir = (c_mx,c_my)
     return $ basic_character {name = c_name, speed = c_speed, position = c_pos, direction = c_dir}

init_characterGen::Model -> [Character] -> IO [Character]
init_characterGen model clist =  do
    
    let env_size = get_size $ environment model

    -- Selection
    let alive_clist = filter (\x -> state x == Alive) clist

    -- Parenting
    parents_clist <- SM.filterM (\x -> is_selectedGA x) alive_clist
    let pairs_clist = get_pairs parents_clist
    childs <- mapM (\(x,y) -> get_randomMutation env_size x y) pairs_clist
    
    -- Complete the list to reach the right number 
    let nb_pop = length clist
    let nb_child = length childs
    let completion_list = take (nb_pop - nb_child) (sortBy sort_character_energy (map (\x -> update_energy x 100) alive_clist))
    
    return (childs ++ completion_list)

    
-- |  A small helper function helping to generate a position on the env box based on the boundaries, the side and a value
tool_randomtoPos:: Float -> Int -> Float -> Position
tool_randomtoPos bound side value
     | side == 1 = (-bound,value)
     | side == 2 = (value,bound)
     | side == 3 = (bound,value)
     | side == 4 = (value,-bound)

is_selectedGA::Character -> IO Bool
is_selectedGA c = do
    p <- randomRIO(0::Float,100.0)
    return $ energy c >= p


get_pairs:: [a] -> [(a,a)]
get_pairs (x:y:ys) = [(x,y)] ++ get_pairs(ys)
get_pairs (x:[]) = [(x,x)]
get_pairs [] = [] 

get_randomMutation:: Float -> Character -> Character -> IO Character
get_randomMutation env_size parent1 parent2 = do
    basic_child <- initR_character env_size
    speed_selector <- randomRIO(1::Int,3::Int) -- 1 = Mere, 2 Pere, 3 Aléatoire entre les 2
    mutation_luck <- randomRIO (0::Float,1.0)
    mutation <- randomRIO (0.8,1.2)
    let from_parents = case speed_selector of
                            1 -> speed parent1
                            2 -> speed parent2
                            3 -> ((speed parent1) + (speed parent2)) /2 

    let after_mutation = if (mutation_luck > 0.85) then from_parents * mutation else from_parents

    return basic_child {generation = (generation parent1) + 1, speed = after_mutation}







                            




