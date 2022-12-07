module Data.Functor.Sum.Both where

import Data.Functor.Classes

data Sum' f g a
  = InL' (f a)
  | InR' (g a)
  | InB' (f a) (g a)
  deriving (Eq, Ord)

instance (Eq1 f, Eq1 g) => Eq1 (Sum' f g) where
    liftEq f = \ cases
        (InL' a₀) (InL' a₁) -> liftEq f a₀ a₁
        (InR' b₀) (InR' b₁) -> liftEq f b₀ b₁
        (InB' a₀ b₀) (InB' a₁ b₁) -> liftEq f a₀ a₁ && liftEq f b₀ b₁
        _ _ -> False

instance (Ord1 f, Ord1 g) => Ord1 (Sum' f g) where
    liftCompare f x y = case (toMaybes x, toMaybes y) of
        ((am₁, bm₁), (am₂, bm₂)) -> liftCompare (liftCompare f) am₁ am₂ <> liftCompare (liftCompare f) bm₁ bm₂

fromMaybes :: Maybe (f a) -> Maybe (g a) -> Maybe (Sum' f g a)
fromMaybes Nothing Nothing = Nothing
fromMaybes (Just x) Nothing = Just (InL' x)
fromMaybes Nothing (Just y) = Just (InR' y)
fromMaybes (Just x) (Just y) = Just (InB' x y)

toMaybes :: Sum' f g a -> (Maybe (f a), Maybe (g a))
toMaybes = \ case
    InL' x -> (Just x, Nothing)
    InR' y -> (Nothing, Just y)
    InB' x y -> (Just x, Just y)
