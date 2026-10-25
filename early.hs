#!/usr/bin/env cabal
{- cabal:
build-depends: base
default-language: Haskell2010
ghc-options: -Wall
-}
{-# LANGUAGE RoleAnnotations #-}

module Main (main) where

import Control.Exception
import Control.Monad     ( unless, when )
import Data.Typeable
import System.Exit       ( exitFailure )

type role Early nominal
data Early a = Early

data ReturningEarly a = ReturningEarly a

instance Typeable e => Show (ReturningEarly e) where
  show = displayException

instance Typeable e => Exception (ReturningEarly e) where
  displayException (ReturningEarly _) = "Early return xception"

leave :: Typeable e => Early e -> e -> IO a
leave _early e = throwIO $ ReturningEarly e

runEarly :: Typeable e => (Early e -> IO a) -> IO (Either e a)
runEarly f = catch (Right <$> f Early) (\(ReturningEarly e) -> pure $ Left e)

-- ===== 使用例 =====

example :: Bool -> Early () -> IO Bool
example flag early = do
  putStrLn "Ran this action"
  when flag $ leave early ()
  putStrLn "Didn't run this action"
  pure True

-- ===== テスト =====

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "Pass: " else "FAIL: ") ++ name)
  pure ok

testLeave :: IO Bool
testLeave = do
  r <- runEarly (example True)
  check "leave returns Left" (r == Left ())

testNoLeave :: IO Bool
testNoLeave = do
  r <- runEarly (example False)
  check "no leave returns Right" (r == Right True)

main :: IO ()
main = do
  results <- sequence [ testLeave
                      , testNoLeave
                      ]
  unless (and results) exitFailure
