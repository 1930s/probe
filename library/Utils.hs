module Utils ( embedEither
             , embedMaybe
             ) where

-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html
import Control.Monad.Except

-- Some handy embedding functions.
embedEither :: (MonadError e m) => (s -> e) -> Either s a -> m a
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- embedEither f esa = either (throwError . f) return esa
embedEither f = either (throwError . f) return

embedMaybe :: (MonadError e m) => e -> Maybe a -> m a
embedMaybe err = maybe (throwError err) return
