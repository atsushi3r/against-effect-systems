-- | Project Euler #9。Leave だけを import するので runEarly は呼べない。
module Euler.Problem9
  ( Triplet
  , product3
  , euler9
  , euler9Full
  , euler9Lazy
  , euler9LazyFor
  ) where

import            Data.Maybe          ( listToMaybe )
import            Control.Monad       (forM_, when)
import            Data.IORef
import            Effect.Early.Leave

type Triplet = (Int, Int, Int)

product3 :: Triplet -> Int
product3 (a, b, c) = a * b * c

-- | 早期リターンあり：見つけた瞬間に二重ループごと抜ける
euler9 :: IORef Int -> Early Triplet -> IO ()
euler9 counter early =
  forM_ [1 .. 333] $ \a ->
    forM_ [a + 1 .. 999] $ \b -> do
      modifyIORef' counter (+ 1)
      let c = 1000 - a - b
      when (c > b && a * a + b * b == c * c) $
        leave early (a, b, c)

-- | 早期リターンなし：全探索して最後に取り出す
euler9Full :: IORef Int -> IO [Triplet]
euler9Full counter = do
  found <- newIORef []
  forM_ [1 .. 333] $ \a ->
    forM_ [a + 1 .. 999] $ \b -> do
      modifyIORef' counter (+ 1)
      let c = 1000 - a - b
      when (c > b && a * a + b * b == c * c) $
        modifyIORef' found ((a, b, c) :)
  readIORef found

-- | 参考：純粋な探索なら遅延リストでも自然にかける
euler9Lazy :: Triplet
euler9Lazy = case candidates of
  t:_ -> t
  _   -> error "euler9Lazy: no triplet found"
  where
    candidates =
      [ (a, b, c)
      | a <- [1 .. 333], b <- [a + 1 .. 999]
      , let c = 1000 - a - b, c > b, a * a + b * b == c * c ]

euler9LazyFor :: Int -> Maybe Triplet
euler9LazyFor s = listToMaybe
  [ (a, b, c)
  | a <- [1 .. s `div` 3], b <- [a + 1 .. s - 1]
  , let c = s - a - b, c > b, a * a + b * b == c * c ]
