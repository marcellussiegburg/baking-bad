module Main (main) where

--Gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Graphics.Gloss

--OpenAL
import Control.Monad (filterM, when, void)
import Data.Maybe (listToMaybe)
import Control.Concurrent
import Sound.ALUT
import Data.IORef
import System.IO.Unsafe

--External Files
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Paths_baking_bad (getDataFileName)

--State
data GameState = GameState {
    debug :: String,
    framecounter :: Int 
}

findAsset :: FilePath -> IO (Maybe FilePath)
findAsset rel = do
    installed <- getDataFileName rel
    let candidates = [installed, "assets" </> rel]
    hits <- filterM doesFileExist candidates
    pure (listToMaybe hits)
    
--Graphics
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

--Sound
soundBuffer :: IORef (Maybe Sound.ALUT.Buffer)
{-# NOINLINE soundBuffer #-}
soundBuffer = unsafePerformIO $ newIORef Nothing

waitUntilSoundPlaybackFinished :: Source -> IO ()
waitUntilSoundPlaybackFinished source = do 
    state <- get (sourceState source)
    case state of
        Playing -> do
            threadDelay 10000  -- 10ms
            waitUntilSoundPlaybackFinished source
        _ -> return ()

playFile :: FilePath -> IO ()
playFile fileName = do
    mBuf <- readIORef soundBuffer
    buf <- case mBuf of
        Just b -> return b
        Nothing -> do
            b <- createBuffer (File fileName)
            writeIORef soundBuffer (Just b)
            return b
    source <- genObjectName
    buffer source $= Just buf
    Sound.ALUT.play [source]
    void $ forkIO $ do
        --threadDelay 1000000 -- 1s
        waitUntilSoundPlaybackFinished source
        deleteObjectNames [source]

main :: IO ()
main = do
    withProgNameAndArgs runALUT $ \progName args -> do
        maybePath <- findAsset "assets/bounce.wav"
        case maybePath of
            Just path -> playFile path
            Nothing -> putStrLn "asset not found: assets/bounce.wav"
        gameloop
