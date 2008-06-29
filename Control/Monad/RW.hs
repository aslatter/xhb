{-# LANGUAGE RecursiveDo
            ,FlexibleInstances
            ,MultiParamTypeClasses
  #-}

module Control.Monad.RW where

import Data.Monoid

import Control.Applicative

import Control.Monad.Reader
import Control.Monad.Writer

newtype RW r w a = RW {runRW :: r -> (a, w)}

instance Monoid w => Monad (RW r w) where
    return x = RW $ \_ -> (x, mempty)

    m >>= k = RW $ \r -> 
         let (a,w) = runRW m r
             (b,w') = runRW (k a) r
         in (b, w `mappend` w')

    m1 >> m2 = RW $ \r ->
         let (_,w)  = runRW m1 r
             (a,w') = runRW m2 r
         in (a, w `mappend` w')

instance Functor (RW r w) where
    fmap f m = RW $ \r -> 
         let (a,w) = runRW m r
         in  (f a, w)

instance Monoid w => Applicative (RW r w) where
    pure = unwrapMonad . pure
    f <*> a = unwrapMonad $ (WrapMonad f) <*> (WrapMonad a)

instance Monoid w => MonadReader r (RW r w) where
    ask = RW $ \r -> (r,mempty)
    local f m = RW $ \r -> runRW m (f r)

instance Monoid w => MonadWriter w (RW r w) where
    tell w = RW $ \_ -> ((),w)

    listen m = RW $ \r -> 
           let (a,w) = runRW m r
           in ((a,w),w)

    pass m = RW $ \r ->
            let ((a, f), w) = runRW m r
            in (a, f w)

-- The transformer

newtype RWT r w m a = RWT {runRWT :: r -> m (a, w)}

instance (Monoid w, Monad m) => Monad (RWT r w m) where
    return x = RWT $ \_ -> return (x, mempty)

    m >>= k = RWT $ \r -> do
                 ~(a,w)  <- runRWT m r
                 ~(b,w') <- runRWT (k a) r
                 return $ (b, w `mappend` w')

    m1 >> m2 = RWT $ \r -> do
                  ~(_,w)  <- runRWT m1 r
                  ~(a,w') <- runRWT m2 r
                  return $ (a, w `mappend` w')


instance Monad m => Functor (RWT r w m) where
    fmap f m = RWT $ \r -> do
                  ~(a,w) <- runRWT m r
                  return (f a, w)

instance (MonadPlus m, Monoid w) => MonadPlus (RWT r w m) where
    mzero = RWT $ \_ -> mzero
    m `mplus` n = RWT $ \r -> runRWT m r `mplus` runRWT n r

instance (Monad m, Monoid w) => Applicative (RWT r w m) where
    pure = unwrapMonad . pure
    f <*> a = unwrapMonad $ (WrapMonad f) <*> (WrapMonad a)

instance (MonadPlus m, Monoid w) => Alternative (RWT r w m) where
    empty = unwrapMonad empty
    m1 <|> m2 = unwrapMonad $ (WrapMonad m1) <|> (WrapMonad m2) 

--

instance Monoid w => MonadTrans (RWT r w) where
    lift m = RWT $ \_ -> do
               a <- m
               return $ (a, mempty)

instance (Monad m, Monoid w) => MonadWriter w (RWT r w m) where
    tell w = RWT $ \_ -> return ((),w)
    listen m = RWT $ \r -> do
                   ~(a,w) <- runRWT m r
                   return ((a, w),w)
    pass m = RWT $ \r -> do
                 ~((a, f), w) <- runRWT m r
                 return (a, f w)

instance (Monad m, Monoid w) => MonadReader r (RWT r w m) where
    ask = RWT $ \r -> return (r, mempty)
    local f m = RWT $ \r -> runRWT m (f r)

embed :: (Monoid w, Monad m) => ReaderT r m a -> RW r w (m a)
embed m = RW $ \r -> (runReaderT m r, mempty)


-- |always succeds.  failing elements of the input list are discarded.
filterAlt :: Alternative f => [f a] -> f [a]
filterAlt [] = pure []
filterAlt (x:xs) = pure (:) <*> x <*> filterAlt xs
               <|> filterAlt xs
