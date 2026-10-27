-- | 「抜ける側」のインターフェース。トークンを受け取って leave するだけ。
-- runEarly は見えない。
module Effect.Early.Leave
  ( Early
  , leave
  ) where

import           Effect.Early.Internal
