module Common where

import Control.Monad.Error

raise :: (MonadError e m, Error e) => String -> m a
raise = throwError . strMsg
