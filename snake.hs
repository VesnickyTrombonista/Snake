import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Data.Map
import System.Random

type Score = Int
type Food = (Int, Int)
type Snake = [Food]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
data GameState = GameState {getSnake :: Snake, getFood :: Food, getDirection :: Direction,
                            isGameOver :: Bool, getRandomStdGen :: StdGen, getScore :: Score}

directionVectors = fromList [(UP, (0, -1)), (DOWN, (0, 1)), (LEFT, (-1, 0)), (RIGHT, (1, 0))] 
-- make up like dictionary

window :: Display
window = InWindow "Snake Game" (800, 570) (250, 50) -- size and position from upper left corner

windowBackground :: Color
windowBackground = white

initialState gameOver score = GameState { getSnake = [snake], getFood = food, 
        getDirection = RIGHT, isGameOver = gameOver, getRandomStdGen = mkStdGen 100, getScore = score}
                                -- columns = 32, rows = 24
        where   snake = (snakeX, snakeY)
                snakeX = 8      -- 32 `div` 4
                snakeY = 6      -- 24 `div` 4
                food = (foodX, foodY)
                foodX = 16      -- 32 `div` 2
                foodY = 12      -- 24 `div` 2
                

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState snake food direction game random score) direction2 = 
        GameState snake food direction2 game random score

boostDirection :: GameState -> GameState -- used, when space is down for double speed
boostDirection gameState = updateState 2 gameState 

checkGameOver :: Snake -> Bool -- check, if snake is in a picture
checkGameOver snake = headX == 0 || headX == 32 || headY == 0 || headY == 24 || headSnake `elem` body
                        where   headSnake = head snake
                                (headX, headY) = headSnake
                                body = tail snake

renderAll :: GameState -> Picture
renderAll gameState = pictures $        [ fillRectangleBy black (16, 0) (660, 20),   -- top line
                                          fillRectangleBy black (16, 24) (660, 20),  -- bottom line
                                          fillRectangleBy black (0, 12) (20, 470),   -- left line
                                          fillRectangleBy black (32, 12) (20, 470)  -- right line
                                        ] ++
                                        fmap (convertToPicture chartreuse) snake ++    -- body
                                        fmap (convertToPicture orange) [snakeHead] ++  -- head
                                        fmap (convertToPicture blue) [food] ++         -- food
                                        gameOverPicture
        where   snake = getSnake gameState 
                food = getFood gameState
                score = getScore gameState
                snakeHead = head snake
                -- getting necessary values

                convertToPicture :: Color -> (Int, Int) -> Picture
                convertToPicture color' (x, y) = fillRectangleBy color' (x, y) (20, 20) 
                -- 20x20 size of food, snakeHead etc.
        
                fillRectangleBy color' (x, y) (width, height) =  color color' $ scale 1 (-1) $ 
                        translate (fromIntegral(x) * 20 - 320) (fromIntegral(y) * 20 - 240) $ 
                        rectangleSolid width height

                gameOverPicture = if (isGameOver gameState) 
                                    then [  
                                        color aquamarine $ 
                                        translate (-130) (180) $ 
                                        scale 0.25 0.25 $ 
                                        text $ "Best score : " ++ show score 
                                        ,
                                        color red $ 
                                        translate (-155) (0) $ 
                                        scale 0.4 0.4 $ 
                                        text "GAME OVER"
                                        ,
                                        color blue $ 
                                        translate (-140) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press SPACE to start."
                                        ] 
                                    else [
                                        color blue $ 
                                        translate (-40) (260) $ 
                                        scale 0.15 0.15 $ 
                                        text $ "Score: " ++ show (length(snake)) 
                                        ]

move :: Food -> Direction -> Snake -> (Bool, Snake)
move food direction snake = if foodEaten 
                            then (True, newHead : snake)
                            else (False, newHead : init snake) -- init, new snake every frame
                        where   foodEaten = newHead == food 
                                newHead =  (headX + shiftX, headY + shiftY)
                                (shiftX, shiftY) = directionVectors ! direction 
                                --(!) :: Ord k => Map k a -> k -> a, on this position, in Dict
                                (headX, headY) = head snake
                                                        
updateState :: Float -> GameState -> GameState
updateState seconds gameState =  if (gameOver) 
                                then gameState
                                else GameState newSnake newFood direction newGameOver newStdGen newScore
        where   snake = getSnake gameState 
                food = getFood gameState
                direction = getDirection gameState
                gameOver = isGameOver gameState
                stdGen = getRandomStdGen gameState
                score = getScore gameState
                -- getting actual arguments
                newScore =   if (length newSnake > score)
                               then score + 1
                               else score
                (foodEaten, newSnake) = move food direction snake
                (generatedFood, newStdGen) = generateNewFood newSnake stdGen
                newFood =   if foodEaten 
                              then generatedFood 
                              else food
                newGameOver = checkGameOver newSnake

generateNewFood :: Snake -> StdGen -> (Food, StdGen)
generateNewFood snake stdGen =  if newFood `elem` snake
                                  then generateNewFood snake stdGen3
                                  else ((foodX, foodY), stdGen3)
        where   (foodX, stdGen2) = randomR (1, 31) stdGen
                (foodY, stdGen3) = randomR (1, 23) stdGen2
                newFood = (foodX, foodY)

servicePressedKeys :: Event -> GameState -> GameState
-- arrows
servicePressedKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = changeDirection gameState LEFT
servicePressedKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT 
servicePressedKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = changeDirection gameState UP 
servicePressedKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = changeDirection gameState DOWN 
-- wasd
servicePressedKeys (EventKey (Char 'a') Down _ _) gameState = changeDirection gameState LEFT
servicePressedKeys (EventKey (Char 'd') Down _ _) gameState = changeDirection gameState RIGHT 
servicePressedKeys (EventKey (Char 'w') Down _ _) gameState = changeDirection gameState UP 
servicePressedKeys (EventKey (Char 's') Down _ _) gameState = changeDirection gameState DOWN 
-- start + boost
servicePressedKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState = 
        if (isGameOver gameState) 
          then initialState False $ getScore gameState
          else boostDirection gameState

servicePressedKeys _ gameState = gameState
-- necessary for program running, else throws exception

-- adding fps can change the difficulty because of speed

main :: IO ()
main = play window windowBackground 8 (initialState True 0) renderAll servicePressedKeys updateState