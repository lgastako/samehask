module Main where

import Random
import Array
import Data.Ix
import Control.Monad
import System.IO.Unsafe

import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL as SDL

type Board = Array (Int, Int) Int
type Game  = (Board, [Surface])

-- "Constants"
num_balls :: Int
num_balls = 3

h :: Int
h = 20

w :: Int
w  = 20

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
random_ball = (getStdRandom . randomR) (0,(dec num_balls))

-- Lame.  TODO: Do away with this hack.
random_ball_xy :: Int -> Int -> Int
random_ball_xy x y = unsafePerformIO random_ball


new_board :: Board
new_board =
      array ((0, 0), (w, h)) [((x, y), rxy x y) | x <- range (0, w - 1),
                                                  y <- range (0, h - 1)]
      where rxy = random_ball_xy

load_ball :: Int -> IO Surface
load_ball n = load_image ("images/ball-" ++ (show n) ++ ".png")


-- TODO: Really?
dec :: Int -> Int
dec n = n - 1

draw_cell :: Game -> Surface -> Int -> Int -> IO Bool
draw_cell (b, balls) s y x =
      apply_surface (x * bw) (y * bw) (balls !! (b ! (x, y))) s


draw_down_cols :: Game -> Surface -> Int -> Int -> IO Bool
draw_down_cols g s y x
    | x > 0 = draw_cell g s y x >> draw_down_cols g s y (dec x)
    | otherwise = draw_cell g s y x


draw_down_rows :: Game -> Surface -> Int -> IO Bool
draw_down_rows g s y
    | y > 0 = draw_down_cols g s y (dec w) >> draw_down_rows g s (dec y)
    | otherwise = draw_down_cols g s y (dec w)


draw_board :: Game -> Surface -> IO Bool
draw_board g s =
    draw_down_rows g s (dec h)


main :: IO ()
main =
    do
      SDL.init [InitEverything]
      screen <- setVideoMode screen_width screen_height screen_bpp []
      setCaption "SameHask" []
      balls <- mapM load_ball [1..3]
      draw_board (board, balls) screen
      SDL.flip screen
      quitHandler
    where
      screen_width  = w * bw
      screen_height = h * bw
      screen_bpp    = 32
      std_gen       = mkStdGen 1
      board         = new_board



