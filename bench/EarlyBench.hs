module Main (main) where

import           Control.Exception (evaluate)
import           Control.Monad (forM_, replicateM, unless)
import           Data.IORef
import           Data.List (sort)
import           Data.Maybe (listToMaybe)
import           Effect.Early.Runner
import           Euler.Problem12
import           Euler.Problem9
import           GHC.Clock (getMonotonicTime)
import           System.Exit (exitFailure)
import           Text.Printf (printf)

-- | 計測する1ケース。action は答えを Int で返す（遅延評価で計算が後回しにならないように）
data Case = Case { name :: String, action :: IO Int }

data Stat = Stat { answer :: Int, best :: Double, median :: Double }

timeOnce :: IO Int -> IO (Int, Double)
timeOnce act = do
  t0 <- getMonotonicTime
  ans <- act >>= evaluate
  t1 <- getMonotonicTime
  pure (ans, t1 - t0)

-- | 1回ウォームアップしてから runs 回測る
measure :: Int -> IO Int -> IO Stat
measure runs act = do
  _ <- timeOnce act
  rs <- replicateM runs (timeOnce act)
  let ts = sort (map snd rs)
  case (rs, ts) of
    ((a, _) : _, t : _) ->
      pure Stat { answer = a, best = t, median = ts !! (runs `div` 2) }
    _ -> fail "measure: runs must be >= 1"

ms :: Double -> Double
ms = (* 1000)

-- ===== ケース定義 =====

euler9Cases :: [Case]
euler9Cases =
  [ Case "early return" $ do
      c <- newIORef 0
      r <- runEarly (euler9 c)
      pure (either product3 (const (-1)) r)
  , Case "full scan" $ do
      c <- newIORef 0
      rs <- euler9Full c
      pure (maybe (-1) product3 (listToMaybe rs))
  , Case "lazy list" $
      pure (maybe (-1) product3 (euler9LazyFor 1000))
  ]

-- limit 500 の答えは i = 12375 番目の三角数。bound は「答えを知っている」前提の上限
-- （出力は ASCII のみ。ロケールによっては日本語の出力でエンコードエラーになるため）
euler12Cases :: Int -> [Case]
euler12Cases bound =
  [ Case "early return" $ do
      r <- runEarly (euler12Fast 500)
      pure (either id (const (-1)) r)
  , Case ("full scan (bound=" ++ show bound ++ ")") $ do
      rs <- euler12FastFull 500 bound
      pure (maybe (-1) id (listToMaybe rs))
  , Case "lazy list" $
      pure (euler12FastLazy 500)
  ]

-- | 1グループ(同じ問題の複数実装)を測って表示し、答えの一致を返す
runGroup :: Int -> String -> [Case] -> IO Bool
runGroup runs title cases = do
  printf "%s\n" title
  printf "  %-28s | %10s | %10s | %8s\n"
    ("variant" :: String) ("min [ms]" :: String) ("median[ms]" :: String) ("vs first" :: String)
  stats <- mapM (\c -> (,) c <$> measure runs (action c)) cases
  let base = case stats of
        (_, s) : _ -> median s
        []         -> 1
  forM_ stats $ \(c, s) ->
    printf "  %-28s | %10.2f | %10.2f | %7.2fx\n"
      (name c) (ms (best s)) (ms (median s)) (median s / base)
  let answers = map (answer . snd) stats
      ok = case answers of
        a : rest -> all (== a) rest
        []       -> True
  unless ok $ printf "  MISMATCH: %s\n" (show answers)
  printf "\n"
  pure ok

main :: IO ()
main = do
  let runs = 7 :: Int
  printf "runs per case: %d (after 1 warm-up); \"vs first\" = median / early-return median\n\n" runs
  oks <- sequence
    [ runGroup runs "Euler #9" euler9Cases
    , runGroup runs "Euler #12 (bound = 12375: exactly the answer position)" (euler12Cases 12375)
    , runGroup runs "Euler #12 (bound = 25000: about 2x)" (euler12Cases 25000)
    , runGroup runs "Euler #12 (bound = 50000: about 4x)" (euler12Cases 50000)
    ]
  unless (and oks) exitFailure
