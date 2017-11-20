{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Arrow ((&&&))
import Control.Comonad.Representable.Store
import Control.Comonad
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map(..))
import Data.MemoTrie
import Data.Traversable
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import System.Random

boardSize :: Int
boardSize = 50

screenSize :: Int
screenSize = 500

wrap :: Int -> ((Int, Int) -> a) -> (Int, Int) -> a
wrap n f p =
  f $ (join bimap) (\x -> if x < 0 then n - (x `mod` n) else x `mod` n) p

newtype Mem a = Mem { unMem :: (Int, Int) -> a }

mem = Mem . wrap boardSize

instance Functor Mem where
  fmap f (Mem g) = mem . memo $ f . g

instance Distributive Mem where
  distribute x = Mem . distribute $ fmap (\m -> unMem m) x

instance Representable Mem where
  type Rep Mem = (Int,Int)
  tabulate = mem . tabulate . memo
  index (Mem f) k = f k

type Board = Store Mem Bool

alive :: Board -> Bool
alive b = c && n > 1 && n < 4 || not c && n == 3
  where
    (c,n) = (extract &&& liveNeighbors) b

liveNeighbors :: Board -> Int
liveNeighbors b = length . filter id $ do
  x <- [-1..1]
  y <- [-1..1]
  if x == 0 && y == 0
    then []
    else
      return $ let (cx,cy) = pos b in peek (cx + x, cy + y) b

blankBoard :: Board
blankBoard = store (const False) (0,0)

randomBoard :: Int -> IO Board
randomBoard n = do
  rs <- replicateM n $ randoms <$> mkStdGen <$> randomIO
  return . mkBoard . M.fromList $ do
    (x, row) <- zip [1..n] rs
    (y, c)   <- zip [1..n] row
    return ((x,y), c)

mkBoard :: Map (Int, Int) Bool -> Board
mkBoard m = store (fromMaybe False . flip M.lookup m) (0,0)

lifeStep = extend alive

printBoard :: Int -> Int -> Board -> IO ()
printBoard x y b = do
  for_ [1..y] $ \y -> do
    for_ [1..x] $ \x ->
      putStr (if peek (x,y) b then "x" else ".")
    putStrLn ""
  putStrLn ""

renderBoard :: Board -> Picture
renderBoard b =
  Pictures $ do
    y <- [1..boardSize]
    x <- [1..boardSize]
    return $ if peek (x,y) b then cell x y else blank

cell x y = polygon [(f x, f y), (f (x + 1), f y), (f (x + 1), f (y + 1)), (f x, f (y + 1))]
  where f v = fromIntegral $ v * (screenSize `div` boardSize) - screenSize `div` 2 - 1

main :: IO ()
main =
  randomBoard boardSize >>= \b ->
    simulate (InWindow "Conway's Game of Life" (screenSize, screenSize) (10, 10))
             white
             10
             b
             renderBoard
             (\_ _ -> lifeStep)

