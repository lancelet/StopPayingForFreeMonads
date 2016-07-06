{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}

module ALaCarte where

infixr 5 :+:

-- |Coproduct of functors
data (f :+: g) a = Inl (f a) | Inr (g a) deriving (Show, Eq)


instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap g (Inl x) = Inl (fmap g x)
  fmap g (Inr x) = Inr (fmap g x)

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)


-- Injections
instance {-# OVERLAPS #-} (Functor f) => f :<: f where
  inj = id
  prj = Just . id

instance {-# OVERLAPS #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj f = Inl f
  prj (Inl f) = Just f
  prj (Inr _) = Nothing

instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj f = Inr (inj f)
  prj (Inl _) = Nothing
  prj (Inr h) = prj h

inject :: (f :<: g) => f (Free g a) -> Free g a
inject = Free . inj


-- |Free monad
data Free f a = Pure a | Free (f (Free f a)) deriving Functor

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
deriving instance (Eq a, Eq (f (Free f a))) => Eq (Free f a)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g <*> a = fmap g a
  Free g <*> a = Free (fmap (<*> a) g)

instance Functor f => Monad (Free f) where
  Pure x >>= g = g x
  Free x >>= g = Free (fmap (>>= g) x)

foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree g _ (Pure x) = g x
foldFree g e (Free x) = e (fmap (foldFree g e) x)

-- Interpreting, using a type class to compose interpretations

class (Functor f, Monad m) => Interpret f m where
  intp :: f a -> m a

instance (Interpret f m, Interpret g m) => Interpret (f :+: g) m where
  intp (Inl x) = intp x
  intp (Inr x) = intp x

interpret :: (Interpret f m) => Free f a -> m a
interpret = undefined

-- Interpreting, but composing interpretations manually

type f ~> g = forall a. f a -> g a

interpret' :: (Functor f, Monad m) => (f ~> m) -> Free f a -> m a
interpret' = undefined

