{-# LANGUAGE RoleAnnotations #-}

-- | 実装の詳細。コンストラクタや例外型をここに閉じ込める。
module Effect.Early.Internal
  ( Early (..)
  , ReturningEarly (..)
  , leave
  , runEarly
  ) where

import           Control.Exception
import           Data.Typeable


type role Early nominal
data Early a = Early


data ReturningEarly a = ReturningEarly a

instance Typeable e => Show (ReturningEarly e) where
  show = displayException

instance Typeable e => Exception (ReturningEarly e) where
  displayException (ReturningEarly _) = "Early return exception"


leave :: Typeable e => Early e -> e -> IO a
leave _early e = throwIO $ ReturningEarly e


runEarly :: Typeable e => (Early e -> IO a) -> IO (Either e a)
runEarly f = catch (Right <$> f Early) (\(ReturningEarly e) -> pure $ Left e)
