module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game

{-# LANGUAGE ScopedTypeVariables #-}


-- type/data declarations and constants --

type Snake = Path
type GameState = (Snake, Point, Int, Direction)
type Direction = Point

newSnake :: Snake
newSnake = [(0, 0), (1, 0), (2, 0)]

squareDim :: Float
squareDim = 30

windowDim :: Int
windowDim = 600


-- functions for setting the game window --

window :: Display
window = InWindow "Snake" (windowDim, windowDim) (100, 100)

background :: Color
background = black


-- functions for rendering the picture --

drawPicture :: GameState -> Picture
drawPicture (snake, apple, _, _) =  Pictures  (color red (drawSquare apple) : (drawSnake snake))

drawSquare :: Point -> Picture
drawSquare bottomLeft@(x, y) =
    let topLeft = (x, y + 1)
        topRight = (x + 1, y + 1)
        bottomRight = (x + 1, y)
    in polygon (map (\(x, y) -> (x * squareDim, y * squareDim)) [bottomLeft, topLeft, topRight, bottomRight])

drawSnake :: Snake -> [Picture]
drawSnake [] = []
drawSnake snake@(x:xs) = color white (drawSquare x) : drawSnake xs

-- functions for handling game logic --

-- generateApple :: Snake -> Point
-- generateApple snake
    -- | ((fromIntegral (fst randPoint)), (fromIntegral (snd randPoint))) `elem` snake = generateApple snake
    -- | otherwise = ((fromIntegral (fst randPoint)), (fromIntegral (snd randPoint)))
    -- where
        -- randPoint :: (Integer, Integer)
	    -- randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
		
generateApple :: Snake -> Point
generateApple snake
    | let 
	    randPoint :: (Integer, Integer)
	    randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
	  in (fromIntegral (fst randPoint), fromIntegral (snd randPoint)) `elem` snake = generateApple snake
    | let 
	    randPoint :: (Integer, Integer)
	    randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
	  in otherwise = (fromIntegral (fst randPoint), fromIntegral (snd randPoint))
	    

moveSnake :: Snake -> Direction -> Snake
moveSnake snake (a, b) = (a + x, b + y) : init snake
    where (x, y) = head snake
	
growSnake :: Snake -> Direction -> Snake
growSnake snake (a, b) = (a + x, b + y) : snake
	where (x, y) = head snake

handleKeyEvent :: Game.Event -> GameState -> GameState
handleKeyEvent (EventKey k ks _ _) (snake, point, int, direction)
    | SpecialKey KeyUp <- k, Down <- ks = (snake, point, int, (0, 1))
    | SpecialKey KeyDown <- k, Down <- ks = (snake, point, int, (0, -1))
    | SpecialKey KeyRight <- k, Down <- ks = (snake, point, int, (1, 0))
    | SpecialKey KeyLeft <- k, Down <- ks = (snake, point, int, (-1, 0))
    | otherwise = (snake, point, int, direction)
handleKeyEvent _ game = game

updateGame :: Float -> GameState -> GameState
updateGame _ (snake, apple, score, direction@(a, b))
	| (a + x, b + y) == apple = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
	| otherwise = (moveSnake snake direction, apple, score, direction)
	where (x, y) = head snake

-- run the game! --

main :: IO ()
main =
    let newGame = (newSnake, (-1,-1), 0, (0, 1))
        keyFrame = 4
    in play window background keyFrame newGame drawPicture handleKeyEvent updateGame
	-- let
	    -- randPoint :: (Int, Int)
	    -- randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
    -- in putStr (show randPoint)

    -- display window background drawPicture
