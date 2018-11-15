{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | I assume that MonadWriter cannot be lifted through ContT in general, since
-- mtl does not have such an instance. But I only need tell, which should be
-- fine, so I'll just write this orphan instance here.

module Orphans where

import Control.Monad.Cont
import Control.Monad.Writer

instance (Monoid w, MonadWriter w m) => MonadWriter w (ContT r m) where
    tell = lift . tell
    listen = error "Don't listen!"
    pass = error "Don't pass!"
