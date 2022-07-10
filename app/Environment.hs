module Environment where

-- Imports
import Graphics.Gloss


-- Type Definition

type Size = Float
data Environment = Environment Size Objects
type Objects = [Float]

-- Utils

getsize:: Environment -> Size
getsize (Environment s _) = s

-- Functions

render_box:: Environment -> Picture
render_box (Environment s _) = Color black box
    where 
        path = rectanglePath s s
        box = lineLoop path

render_env:: Environment -> Picture
render_env env = render_box env 