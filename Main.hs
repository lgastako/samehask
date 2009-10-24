module Main where

import Random
import Control.Monad

import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL as SDL

type Board = [[Int]]
type Game  = (Board, [Surface])
type Point = (Int, Int)
type Ball  = Int

type Filename = String


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


load_image :: Filename -> IO Surface
load_image filename = load filename >>= displayFormat


apply_surface :: Int -> Int -> Surface -> Surface -> IO Bool
apply_surface x y src dst =
    blitSurface src Nothing dst (Just offset)
    where offset = Rect x y 0 0


random_ball :: IO Ball
random_ball = (getStdRandom . randomR) (1,num_balls)


new_row :: IO [Ball]
new_row = forM [0..w] (\_ -> random_ball)

new_board :: IO Board
new_board = forM [0..h] (\_ -> new_row)


load_ball :: Ball -> IO Surface
load_ball n = load_image ("images/ball-" ++ (show n) ++ ".png")


draw_cell :: Game -> Surface -> Int -> Int -> IO Bool
draw_cell (b, balls) s y x =
      apply_surface (x * bw) (y * bw) (balls !! (get_ball b (x, y))) s


draw_down_cols :: Game -> Surface -> Int -> Int -> IO Bool
draw_down_cols g s y x
    | x > 0 = draw_cell g s y x >> draw_down_cols g s y (pred x)
    | otherwise = draw_cell g s y x


draw_down_rows :: Game -> Surface -> Int -> IO Bool
draw_down_rows g s y
    | y > 0 = draw_down_cols g s y (pred w) >> draw_down_rows g s (pred y)
    | otherwise = draw_down_cols g s y (pred w)


draw_board :: Game -> Surface -> IO Bool
draw_board g s =
    draw_down_rows g s (pred h)


get_ball :: Board -> Point -> Ball
get_ball b (x, y) = (b !! y) !! x


point_u :: Point -> Point
point_u (x, y) = (x, y - 1)


point_d :: Point -> Point
point_d (x, y) = (x, y + 1)


point_l :: Point -> Point
point_l (x, y) = (x - 1, y)


point_r :: Point -> Point
point_r (x, y) = (x + 1, y)


eradicate :: Board -> Point -> Ball -> Board
eradicate board point ball
    | out_of_bounds point = board
    | wrong_color (get_ball board point) ball = board
    | otherwise =
        (eradicate
         (eradicate
          (eradicate
           (eradicate
            board
            (point_u point) ball)
           (point_d point) ball)
          (point_l point) ball)
         (point_r point) ball)
        where wrong_color = (/=)
              out_of_bounds (x,y)
                  | x < 0 = True
                  | y < 0 = True
                  | x > w = True
                  | y > h = True
                  | otherwise = False



mouse_point_to_board_point :: Point -> Point
mouse_point_to_board_point (x, y) = (x `div` w, y `div` h)


remove_from_mouse_click :: Board -> Point -> Board
remove_from_mouse_click b click =
    eradicate b location ball
    where
      ball  = get_ball b location
      location = mouse_point_to_board_point click


main :: IO ()
main =
    do
      SDL.init [InitEverything]
      screen <- setVideoMode screen_width screen_height screen_bpp []
      setCaption "SameHask" []
      balls <- mapM load_ball [0..3]
      board <- new_board
--      draw_board ((remove_from_mouse_click board (0, 0)), balls) screen
      draw_board (board, balls) screen
      SDL.flip screen
      quitHandler
    where
      screen_width  = w * bw
      screen_height = h * bw
      screen_bpp    = 32
