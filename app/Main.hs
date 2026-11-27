module Main (main) where

--Gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Graphics.Gloss

data GameState = GameState {
    debug :: String,
    framecounter :: Int 
}

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 720

windowPosX :: Int
windowPosX = 10

windowPosY :: Int
windowPosY = 32

window :: Display
window = InWindow "BakingBad" (windowWidth, windowHeight) (windowPosX, windowPosY)

background :: Color
background = black

fps :: Int
fps = 60

initialGameState :: GameState
initialGameState = GameState {
    framecounter = 0
}

renderer :: GameState -> Picture
renderer state = Blank

inputHandler :: Event -> GameState -> GameState
inputHandler e as = as

iterator :: Float -> GameState -> IO GameState
iterator deltaTime state = do
    return GameState {
        framecounter = (framecounter state)+1
    }

gameloop :: IO ()
gameloop = Graphics.Gloss.playIO window background fps initialGameState (return . renderer) (\event gs -> return $ inputHandler event gs) iterator

main :: IO ()
main = do
        gameloop
