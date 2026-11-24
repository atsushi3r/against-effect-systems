{-# LANGUAGE RankNTypes #-}

module Main (main) where

import            Control.Exception   ( evaluate )
import            Control.Monad       ( forM, replicateM, unless )
import            Data.List           ( sort )
import            Effect.Early.Runner
import            Euler.Problem12
import            GHC.Clock           ( getMonotonicTime )
import            System.Exit         ( exitFailure )
import            Text.Printf         ( printf, PrintfType )

type Solver = Int -> Early Int -> IO ()

-- | 1回実行して、答えと経過秒を返す
timeOnce :: Solver -> Int -> IO (Int, Double)
timeOnce solver limit = do
  t0  <- getMonotonicTime
  r   <- runEarly (solver limit)
  ans <- evaluate (either id (const (-1)) r)
  t1  <- getMonotonicTime
  pure (ans, t1 - t0)

data Stat = Stat
  { answer :: Int
  , best   :: Double
  , median :: Double
  }

-- | 1回ウォームアップしてから runs 回測る
measure :: Int -> Solver -> Int -> IO Stat
measure runs solver limit = do
  _ <- timeOnce solver limit
  rs <- replicateM runs (timeOnce solver limit)
  let ts = sort (map snd rs)
  case (rs, ts) of
    ((a, _) : _, t : _) ->
      pure Stat { answer = a, best = t, median = ts !! (runs `div` 2) }
    _ -> fail "measure: runs must be >= 1"

ms :: Double -> Double
ms = (* 1000)

printfCyan :: forall r. PrintfType r => String -> r
printfCyan = printf . ("\ESC[36m" ++) . (++ "\ESC[0m")

main :: IO ()
main = do
  let runs = 7 :: Int
      limits = [100 :: Int, 200, 500]
  printf "runs per case: %d (after 1 warm-up)\n\n" runs
  printfCyan "%6s   %-12s   %10s   %10s   %8s\n"
    "limit" "solver" "min [ms]" "median[ms]" "speedup"
  oks <- forM limits $ \limit -> do
    old  <- measure runs euler12 limit
    fast <- measure runs euler12Fast limit
    printf "%6d   %-12s   %10.2f   %10.2f   %8s\n"
      limit "euler12" (ms $ best old) (ms $ median old) "-"
    printf "%6d   %-12s   %10.2f   %10.2f   %7.1fx\n"
      limit "euler12Fast" (ms $ best fast) (ms $ median fast) (median old / median fast)
    let ok = answer old == answer fast
    unless ok $
      printf "  MISMATCH at limit %d: %d vs %d\n" limit (answer old) (answer fast)
    pure ok
  unless (and oks) exitFailure


