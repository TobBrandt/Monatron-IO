-- | Instances of the 'MonadIO' class for 'StateT', 'WriterT', 'ReaderT', 'ExcT',
--   'ContT', and 'ListT'.
--
--   Re-exports "Control.Monad.IO.Class" for convenience.
module Control.Monatron.IO (module Control.Monad.IO.Class) where

import Control.Monatron.Monatron (MonadT(..), Monoid, StateT, WriterT, ReaderT, ExcT, ContT, ListT)
import Control.Monad.IO.Class

instance MonadIO m => MonadIO (StateT z m) where
  liftIO = lift . liftIO

instance (MonadIO m, Monoid z) => MonadIO (WriterT z m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT z m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExcT z m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ContT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO
