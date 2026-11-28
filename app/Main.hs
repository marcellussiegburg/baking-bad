module Main (main) where

--Gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Graphics.Gloss

--Internals
import Bakingbad (Bakery (..), Food (..), sell)

data GameState = GameState {
    debug :: String,
    framecounter :: Int,
    bakery :: Bakery
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
    debug = "",
    framecounter = 0,
    bakery = Bakery {
        inventory = [Food {pricetag = 10.00}],
        funds = 100.00
    }
}

renderer :: GameState -> Picture
renderer _ = Blank

-- Press "s" to sell the first item in the inventory.
inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (Char 's') _ _ _) state = case inventory (bakery state) of
    (food:_) -> state {
        bakery = sell (bakery state) food,
        debug = "sold for " ++ show (pricetag food)
    }
    [] -> state { debug = "nothing left to sell" }
inputHandler _ state = state

iterator :: Float -> GameState -> IO GameState
iterator _deltaTime state = do
    return GameState {
        debug = debug state,
        framecounter = (framecounter state)+1,
        bakery = bakery state
    }

gameloop :: IO ()
gameloop = Graphics.Gloss.playIO window background fps initialGameState (return . renderer) (\event gs -> return $ inputHandler event gs) iterator

main :: IO ()
main = gameloop
