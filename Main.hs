module Main where

import Random
import Control.Monad

import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL as SDL

type Ball  = Int
type BallViews = [Surface]

type Board = [[Int]]
type Game  = (Board, BallViews)
type Point = (Int, Int)

type Filename = String


num_balls :: Int
num_balls = 3


h :: Int
h = 20


w :: Int
w  = 20


bw :: Int
bw = 28


origin :: Point
origin = (0, 0)


load_image :: Filename -> IO Surface
load_image filename = load filename >>= displayFormat


apply_surface :: Point  -> Surface -> Surface -> IO Bool
apply_surface (x, y) src dst =
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
      apply_surface ((x * bw), (y * bw)) (balls !! (get_ball b (x, y))) s


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


set_ball :: Board -> Point -> Ball -> Board
set_ball board point ball = [[update x y | x <- [0..w]] | y <- [0..h]]
    where
      update x y
          | (x, y) == point = ball
          | otherwise       = get_ball board (x, y)


up :: Point -> Point
up (x, y) = (x, y - 1)


down :: Point -> Point
down (x, y) = (x, y + 1)


left :: Point -> Point
left (x, y) = (x - 1, y)


right :: Point -> Point
right (x, y) = (x + 1, y)


out_of_bounds :: Point -> Bool
out_of_bounds (x,y)
    | x < 0 = True
    | y < 0 = True
    | x > w = True
    | y > h = True
    | otherwise = False


empty :: Board -> Point -> Bool
empty board point = get_ball board point == 0


eradicate :: Board -> Point -> Ball -> Board
eradicate board _ 0 = board
eradicate board point ball
    | out_of_bounds point = board
    | this_ball /= ball   = board
    | otherwise =
        let new_board = set_ball board point 0
        in
          (eradicate
           (eradicate
            (eradicate
             (eradicate new_board
              (up point) ball)
             (down point) ball)
            (left point) ball)
           (right point) ball)
        where
          this_ball = get_ball board point


mouse_point_to_board_point :: Point -> Point
mouse_point_to_board_point (x, y) = (f x, f y)
    where f = (`div` bw)


drop_col :: Board -> Point -> Board
drop_col board point
    | out_of_bounds point     = board
    | out_of_bounds above     = board
    | empty board above       = board
    | not $ empty board point = board
    | otherwise =
         let above_ball = get_ball board above
             first_swap = set_ball board point above_ball
             next_swap  = set_ball first_swap above 0
         in
           drop_col next_swap above
         where
           above = down point


collapse_point :: Point -> Board -> Board
collapse_point point board
    | x > w = collapse_point (0, succ y) board
    | y > h = board
    | otherwise =
        if empty board point
        then collapse_point point $ drop_col board point
        else collapse_point (right point) board
            where (x, y) = point


--collapse :: Board -> Board
--collapse = collapse_point origin

collapse = id


remove_from_mouse_click :: Game -> Point -> Game
remove_from_mouse_click game click =
    (collapse (eradicate board location ball), balls)
    where
      (board, balls) = game
      ball           = get_ball board location
      location       = mouse_point_to_board_point click


game_loop :: Game -> Surface -> IO ()
game_loop game screen =
    do
      draw_board game screen
      SDL.flip screen
      event <- waitEvent
      case event of
        Quit -> quit
        KeyDown (Keysym SDLK_q _ _) -> quit
        KeyDown (Keysym SDLK_r _ _) -> do
            nb <- new_board
            game_loop (nb, balls) screen
            where
              (_, balls) = game
        MouseButtonDown x y ButtonLeft -> do
            putStrLn $ show mouse_point ++ show dbg_remove_me
            game_loop (remove_from_mouse_click game mouse_point) screen
            where
              mouse_point = (fromIntegral x, fromIntegral y)
              dbg_remove_me = mouse_point_to_board_point mouse_point
        otherwise ->
            game_loop game screen
      where
        quit = return ()


main :: IO ()
main =
    do
      SDL.init [InitEverything]
      screen <- setVideoMode screen_width screen_height screen_bpp []
      setCaption "SameHask" []
      balls <- mapM load_ball [0..3]
      board <- new_board
      game_loop (board, balls) screen
    where
      screen_width  = w * bw
      screen_height = h * bw
      screen_bpp    = 32
