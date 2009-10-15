module Main where

import Random
import Array
import Data.Ix
import Control.Monad
import System.IO.Unsafe

import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL as SDL

-- "Constants"
num_balls :: Int
num_balls = 3

h :: Int
h = 10

w :: Int
w  = 10

bw :: Int
bw = 28


quitHandler :: IO ()
quitHandler = do
  e <- waitEvent
  case e of
    Quit -> return ()
    otherwise -> quitHandler


load_image :: String -> IO Surface
load_image filename = load filename >>= displayFormat
--load_image = load >>= displayFormat


apply_surface :: Int -> Int -> Surface -> Surface -> IO Bool
apply_surface x y src dst =
    blitSurface src Nothing dst (Just offset)
    where offset = Rect x y 0 0


random_ball :: IO Int
random_ball = (getStdRandom . randomR) (1,3)

-- Lame.  TODO: Do away with this hack.
random_ball_xy :: Int -> Int -> Int
random_ball_xy x y = unsafePerformIO random_ball

-- generate_random_board :: IO (Array (Int, Int) Int)
-- generate_random_board =
--     do
--       array (w, h) [((x, y), n) | n <- random_ball,
--                                   x <- range (0, w - 1),
--                                   y <- range (0, h - 1)]

--new_board ::
new_board =
      array ((0, 0), (w, h)) [((x, y), rxy x y) | x <- range (0, w - 1),
                                                  y <- range (0, h - 1)]
      where rxy = random_ball_xy

load_ball :: Int -> IO Surface
load_ball n = load_image ("images/ball-" ++ (show n) ++ ".png")


main :: IO ()
main =
    do
      SDL.init [InitEverything]
      screen <- setVideoMode screen_width screen_height screen_bpp []
      setCaption "SameHask" []
      balls <- mapM load_ball [1..3]
      apply_surface 0 0 (head balls) screen
      apply_surface bw 0 (head (tail balls)) screen
      apply_surface 0 bw (head (tail (tail balls))) screen
      SDL.flip screen
      quitHandler
    where
      screen_width  = 640
      screen_height = 480
      screen_bpp    = 32
      std_gen       = mkStdGen 1
      board         = new_board



