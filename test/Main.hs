module Main (main) where

import Control.Monad       ( unless, when )
import Data.IORef
import Effect.Early.Leave
import Effect.Early.Runner
import Euler.Problem9
import System.Exit         ( exitFailure )

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "PASS: " else "FAIL: ") ++ name)
  pure ok

example :: Bool -> Early () -> IO Bool
example flag early = do
  putStrLn "Ran this action"
  when flag $ leave early ()
  putStrLn "Didn't run this action"
  pure True

testLeave :: IO Bool
testLeave = do
  r <- runEarly (example True)
  check "leave returns Left" (r == Left ())

testNoLeave :: IO Bool
testNoLeave = do
  r <- runEarly (example False)
  check "no leave returns Right" (r == Right True)

-- 外側のleaveを区別するための専用型
newtype Fatal = Fatal ()
  deriving (Eq)

-- 内側と外側が同じ型 (): 外側のleaveを内側が捕まえてしまう
nestedSame :: IO (Either () String)
nestedSame = runEarly $ \outer -> do
  inner <- runEarly $ \_ -> (leave outer () :: IO Int)
  case inner of
    Left () -> pure "caught by inner"
    Right _ -> pure "unreachable"

-- 外側だけ Fatal 型：内側は素通しして外側まで抜ける
nestedDistinct :: IO (Either Fatal String)
nestedDistinct = runEarly $ \outer -> do
  inner <- runEarly $ \_ -> (leave outer (Fatal ()) :: IO Int)
  case inner of
    Left () -> pure "caught by inner"
    Right _ -> pure "unreachable"

testNestedSameType :: IO Bool
testNestedSameType = do
  r <- nestedSame
  check "same type: inner swallows outer leave" (r == Right "caught by inner")

testNestedDistinct :: IO Bool
testNestedDistinct = do
  r <- nestedDistinct
  check "distinct types: outer leave escapes inner" (r == Left (Fatal ()))

-- ===== Project Euler =====

testEuler9 :: IO Bool
testEuler9 = do
  counter <- newIORef 0
  rs <- runEarly (euler9 counter)
  n <- readIORef counter
  putStrLn $ "  early-exit iterations: " ++ show n
  check "euler9 (early)"
    (fmap product3 (either Just (const Nothing) rs) == Just 31875000)

testEuler9Full :: IO Bool
testEuler9Full = do
  counter <- newIORef 0
  rs <- euler9Full counter
  n <- readIORef counter
  putStrLn (" full-scan iterations: " ++ show n)
  check "euler9 (full scan)" (map product3 rs == [31875000])

testEuler9Lazy :: IO Bool
testEuler9Lazy = do
  check "euler9 (lazy list)" (product3 euler9Lazy == 31875000)

main :: IO ()
main = do
  results <- sequence
    [ testLeave
    , testNoLeave
    , testNestedSameType
    , testNestedDistinct
    , testEuler9
    , testEuler9Full
    , testEuler9Lazy
    ]
  unless (and results) exitFailure
