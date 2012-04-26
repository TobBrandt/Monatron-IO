{-# LANGUAGE TypeOperators, FlexibleContexts #-}

-- | An instance of the 'MonadIO' class for a zipper:
--
--   @
--     instance ('MonadIO' (t2 m), 'MonadT' t1, 'MonadT' t2, 'Monad' m) => 'MonadIO' ((t1 ':>' t2) m)
--   @
--
--   Re-exports "Control.Monad.IO.Class" for convenience.
module Control.Monatron.Zipper.IO (module Control.Monad.IO.Class) where

import Control.Monatron.Monatron (MonadT(..), Monad)
import Control.Monatron.Zipper ((:>), zipper)
import Control.Monad.IO.Class

instance (MonadIO (t2 m), MonadT t1, MonadT t2, Monad m) => MonadIO ((t1 :> t2) m) where
  liftIO = zipper . lift . liftIO
