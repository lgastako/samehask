module Main where

import Random
import Control.Monad
import System.IO.Unsafe

import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL as SDL

type Board = [[Int]]
type Game  = (Board, [Surface])
type Point = (Int, Int)

-- Gratuitous?
type Filename = String


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


load_image :: Filename -> IO Surface
load_image filename = load filename >>= displayFormat


apply_surface :: Int -> Int -> Surface -> Surface -> IO Bool
apply_surface x y src dst =
    blitSurface src Nothing dst (Just offset)
    where offset = Rect x y 0 0


random_ball :: IO Int
random_ball = (getStdRandom . randomR) (0,(dec num_balls))


-- Lame.  TODO: Do away with this hack.
random_ball_xy :: Int -> Int -> Int
random_ball_xy _ _ = unsafePerformIO random_ball


new_board :: Board
new_board = [ [ random_ball_xy x y | x <- [0..w]] | y <- [0..h] ]


load_ball :: Int -> IO Surface
load_ball n = load_image ("images/ball-" ++ (show n) ++ ".png")


-- TODO: Really?
dec :: Int -> Int
dec n = n - 1


draw_cell :: Game -> Surface -> Int -> Int -> IO Bool
draw_cell (b, balls) s y x =
      apply_surface (x * bw) (y * bw) (balls !! (get_ball b (x, y))) s


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


build_eradication_list :: Board -> Point -> Int -> [Point] -> [Point]
build_eradication_list b (x, y) v accum
    | (x < 0) || (y < 0)  = accum
    | (x > w) || (y > h)  = accum
    | (x, y) `elem` accum = accum
    | otherwise           =
        build_eradication_list b ((pred x), y) v accum3
        where
          accum3 = build_eradication_list b ((succ x), y) v accum2
          accum2 = build_eradication_list b (x, (pred y)) v accum1
          accum1 = build_eradication_list b (x, (succ y)) v accum


-- From here:
-- http://hackage.haskell.org/packages/archive/luhn/0.1/doc/html/src/Luhn.html
-- Like Python's enumerate function - returns a tuple where the first
-- element is the index from 0 of the second element in the input list.
enumerate :: Integral n => [a] -> [(n, a)]
enumerate xs = enumerate' 0 xs
    where
        enumerate' _ [] = []
        enumerate' counter (a:as) =
            (counter, a) : enumerate' (counter + 1) as


eradicate :: Board -> [Point] -> Board
eradicate b []     = b
eradicate b points =
   [handle_row y row | (y, row) <- enumerate b]
   where
     handle_row y row =
         [handle_col y x v | (x, v) <- enumerate row]
     handle_col y x v
         | (x, y) `elem` points = 0
         | otherwise            = v


get_ball :: Board -> Point -> Int
get_ball b (x, y) = (b !! y) !! x


remove_adjacent :: Board -> Point -> Board
remove_adjacent b xy =
    eradicate b eradication_list
    where eradication_list = build_eradication_list b xy value []
          value = get_ball b xy

mouse_point_to_board_point :: Point -> Point
mouse_point_to_board_point (x, y) = (x `div` w, y `div` h)


remove_from_mouse_click :: Board -> Point -> Board
remove_from_mouse_click b click =
    remove_adjacent b $ mouse_point_to_board_point click


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



