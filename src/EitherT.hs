-- | MonadErr is just like MonadError without the catchError ability. 
--
-- I need this because Liang and Hudak's monad transformer stack has
-- continuations above errors, so one needs to lift the error operations through
-- the continuation transformer. But MonadError cannot be lifted through ContT
-- because it is not clear how to implement catchError. I don't need catchError
-- anyway, so here I recreate MonadError without catchError.

module EitherT where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

class Monad m => MonadErr e m where
    -- This is just like MonadError's throwError
    err :: e -> m a

instance Monad m => MonadErr e (ExceptT e m) where
    err = throwError

instance MonadErr e m => MonadErr e (ReaderT r m) where
    err = lift . err

instance MonadErr e m => MonadErr e (ContT r m) where
    err = lift . err

instance MonadErr e m => MonadErr e (StateT r m) where
    err = lift . err

instance (Monoid w, MonadErr e m) => MonadErr e (WriterT w m) where
    err = lift . err
