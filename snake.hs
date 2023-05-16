import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Data.Map
import System.Random

type Food = (Int, Int)
type Snake = [Food]
type Score = Int
data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
data GameState = GameState {getSnake :: Snake, getFood :: Food, getDirection :: Direction,
                            isGameOver :: Bool, getRandomStdGen :: StdGen, getScore :: Score}

directionVectorMap = fromList [(UP, (0, -1)), (DOWN, (0, 1)), (LEFT, (-1, 0)), (RIGHT, (1, 0))] 
-- make up like dictionary

window :: Display
window = InWindow "Snake Game" (800, 570) (250, 50) -- size and position from upper left corner

background :: Color
background = white

initialGameState gameOver score = GameState { getSnake = [(snakeX, snakeY)], getFood = (foodX, foodY), 
        getDirection = RIGHT, isGameOver = gameOver, getRandomStdGen = mkStdGen 100, getScore = score}
                                -- columns = 32, rows = 24
        where   snakeX = 8      -- 32 `div` 4
                snakeY = 6      -- 24 `div` 4
                foodX = 16      -- 32 `div` 2
                foodY = 12      -- 24 `div` 2

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState snake food direction game random score) direction2 = GameState snake food direction2 game random score

boostDirection :: GameState -> GameState -- used, when space is down for double speed
boostDirection gameState = updateState 2 gameState 

checkGameOver :: Snake -> Bool -- check, if snake is in a picture
checkGameOver snake = headX == 0 || headX == 32 || headY == 0 || headY == 24 || headSnake `elem` body
                        where   headSnake = head snake
                                (headX, headY) = headSnake
                                body = tail snake

render :: GameState -> Picture
render gameState = pictures $   [ fillRectangle black (16, 0) (660, 20)   -- top line
                                , fillRectangle black (16, 24) (660, 20)  -- bottom line
                                , fillRectangle black (0, 12) (20, 470)   -- left line
                                , fillRectangle black (32, 12) (20, 470)  -- right line
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
            convertToPicture color' (x, y) = fillRectangle color' (x,y) (20,20) -- 20x20 size of food etc.
        
            fillRectangle color' (x, y) (width, height) =  color color' $ scale 1 (-1) $ 
                translate (fromIntegral(x) * 20 - 320) (fromIntegral(y) * 20 - 240) $ 
                rectangleSolid width height

            gameOverPicture =   if (isGameOver gameState) 
                                then [  
                                        color aquamarine $ 
                                        translate (-110) (180) $ 
                                        scale 0.25 0.25 $ 
                                        text $ "Best score: " ++ show score 
                                        ,
                                        color red $ 
                                        translate (-200) (0) $ 
                                        scale 0.5 0.5 $ 
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
                                        text $ "Score: " ++ show (length(snake)) ]

move :: Food -> Direction -> Snake -> (Bool, Snake)
move food direction snake = if foodEaten 
                            then (True, newHead : snake)
                            else (False, newHead : init snake) -- init, new snake every frame
                        where   foodEaten = newHead == food 
                                newHead =  (headX + shiftX, headY + shiftY)
                                (shiftX, shiftY) = directionVectorMap ! direction 
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

handleKeys :: Event -> GameState -> GameState
-- arrows
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT 
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = changeDirection gameState UP 
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = changeDirection gameState DOWN 
-- wasd
handleKeys (EventKey (Char 'a') Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (Char 'd') Down _ _) gameState = changeDirection gameState RIGHT 
handleKeys (EventKey (Char 'w') Down _ _) gameState = changeDirection gameState UP 
handleKeys (EventKey (Char 's') Down _ _) gameState = changeDirection gameState DOWN 
-- start + boost
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =    if (isGameOver gameState)
                                                                    then initialGameState False $ getScore gameState
                                                                    else boostDirection gameState

handleKeys _ gameState = gameState
-- necessary for program running, else throws exception

-- adding fps can change the difficulty because of speed

main :: IO ()
main = play window background 8 (initialGameState True 0) render handleKeys updateState