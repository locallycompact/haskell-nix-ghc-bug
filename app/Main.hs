module Main where

import Control.Monad
import Data.Default.Class
import Data.IORef
import Data.Time.Clock
import FRP.Yampa
import qualified SDL
import qualified SDL.Mixer as Mix
import Techlab
import Techlab.Dhall

data X = X

twoSecondsPassed :: SF () Bool
twoSecondsPassed = time >>> arr (> 2)

initialize :: IO ()
initialize = putStrLn "Hello... wait for it..."

actuate :: Bool -> Bool -> IO Bool
actuate _ x = do
  when x $ do
    sound <- Mix.load "foo.wav"
    Mix.play sound
  return x

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, Just ())

{--
k :: SF () (Event b)
k = repeatedly 10000 X
--}
main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  Mix.openAudio def 256

  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) actuate twoSecondsPassed

  Mix.closeAudio

  Mix.quit
  SDL.quit
