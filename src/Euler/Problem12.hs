module Euler.Problem12
  ( numDivisors
  , euler12
  , euler12Fast
  , triangleDivisors
  , euler12FastFull
  , euler12FastLazy
  ) where

import          Control.Monad      ( forM_, when )
import          Data.IORef
import          Data.List          ( find, group )
import          Effect.Early.Leave

euler12 :: Int -> Early Int -> IO ()
euler12 limit early = loop 1 1
  where
    loop :: Int -> Int -> IO ()
    loop i tri = do
      when (numDivisors tri > limit) $ leave early tri
      loop (i + 1) (tri + i + 1)

-- | 整数nの約数の個数を数える
-- 1 から√nまでの約数dを調べ、カウントをdがnの平方根であれば(+1)し、
-- そうでなければ(+2)する
numDivisors :: Int -> Int
numDivisors n = go 1 0
  where
    go d acc
      | d * d > n      = acc
      | d * d == n     = acc + 1
      | n `mod` d == 0 = go (d + 1) (acc + 2)
      | otherwise      = go (d + 1) acc

euler12Fast :: Int -> Early Int -> IO ()
euler12Fast limit early = loop 1
  where
    loop :: Int -> IO ()
    loop i = do
      let nTri = triangleDivisors i
          tri = i * (i + 1) `div` 2
      when (nTri > limit) $ leave early tri
      loop (i + 1)

-- | n番目の三角数の約数を数える
-- triangleDivisors n == numDivisors (n * (n + 1) `div` 2)
triangleDivisors :: Int -> Int
triangleDivisors n
  | even n = numDivisors' (n `div` 2) * numDivisors' (n + 1)
  | otherwise = numDivisors' n * numDivisors' ((n + 1) `div` 2)

-- | 素因数分解して、各因数の組み合わせの数で約数を数える
numDivisors' :: Int -> Int
numDivisors' = product . map ((+1) . length) . group . primeFactors

-- | 整数n'を素因数分解する
primeFactors :: Int -> [Int]
primeFactors n' = go n' 2
  where
    go m p
      | m == 1 = []
      | p * p > m = [m]
      | m `mod` p == 0 = p : go (m `div` p) p
      | otherwise = go m (p + 1)

triangle :: Int -> Int
triangle i = (i * (i + 1)) `div` 2

-- | 早期リターンなし：i を 1..bound まで全部調べ、見つかった三角数を集めて返す
-- （bound は答えより大きくなければならない。無限ループはかけない）
euler12FastFull :: Int -> Int -> IO [Int]
euler12FastFull limit bound = do
  found <- newIORef []
  forM_ [1 .. bound] $ \i ->
    when (triangleDivisors i > limit) $
      modifyIORef' found (triangle i :)
  reverse <$> readIORef found

-- | 参考：無限リストを遅延評価で探す版（Early を使わずに早く止まる）
euler12FastLazy :: Int -> Int
euler12FastLazy limit =
  case find ((> limit) . triangleDivisors) [1 ..] of
    Just i -> triangle i
    Nothing -> error "euler12: unreachable (infinite list)"
