module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game

type Snake = Path
type GameState = (Snake, Point, Int, Direction)

-- type Direction :: (Enum a) => a
-- type Direction = NORTH | SOUTH | EAST | WEST

data Direction = NORTH | SOUTH | EAST | WEST deriving (Enum, Eq)

-- Instantiate Initial Variables

squareDim :: Float
squareDim = 30

window :: Display
window = InWindow "Snake" (500, 500) (100, 100)

background :: Color
background = black

-- drawing :: Picture
-- drawing = blank


-- convertWorld :: Path -> Picture
-- convertWorld x = line x

handleKeyEvent :: Game.Event -> GameState -> GameState
handleKeyEvent (EventKey k ks _ _) (snake, point, int, direction) = (snake, point, int, newDirection)
	where newDirection | SpecialKey KeyUp <- k, Down <- ks = NORTH | SpecialKey KeyDown <- k, Down <- ks = SOUTH | SpecialKey KeyRight <- k, Down <- ks = EAST | SpecialKey KeyLeft <- k, Down <- ks = WEST | otherwise = direction
handleKeyEvent _ game = game


updateGame :: Float -> GameState -> GameState
updateGame _ (snake, apple, score, direction) =
    let newSnake = updateSnake snake direction
    in (newSnake, apple, score, direction)
    -- let newSnake = map (\(x,y) -> (x+40, y)) snake

updateSnake :: Path -> Direction -> Path
updateSnake snake direction = (deltaX + fst (head snake), deltaY + snd (head snake)): init snake
    where (deltaX, deltaY) | direction == NORTH = (0, 40) | direction == SOUTH = (0, -40) | direction == WEST = (-40, 0) | direction == EAST = (40, 0)

-- Initial Functions

drawSquare :: Point -> Picture
drawSquare bottomLeft@(x, y) =
    let topLeft = (x + squareDim, y)
        topRight = (x + squareDim, y - squareDim)
        bottomRight = (x, y - squareDim)
    in color white (polygon [bottomLeft, topLeft, topRight, bottomRight])

drawSnake :: Path -> [Picture]
drawSnake snake | length snake == 1 = (drawSquare (head snake)) : []
                | otherwise = (drawSquare (head snake)) : (drawSnake (tail snake))

point :: Point
point = (50,50)

-- square :: Picture
-- square = drawSquare point

newSnake :: Path
newSnake = [(0, 0), (40, 0), (80, 0)]

drawPicture :: GameState -> Picture
drawPicture (snake, _, _, _) = Pictures (drawSnake snake)

main :: IO ()
main =
    let newGame = (newSnake, (200,200), 0, NORTH)
        keyFrame = 2
    in play window background keyFrame newGame drawPicture handleKeyEvent updateGame

-- display window background drawPicture

-- play window background 1 world convertWorld dummyUpdate updateWorld
