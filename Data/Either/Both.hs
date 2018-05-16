module Data.Either.Both where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Classes

data Either' a b = JustLeft a | JustRight b | Both a b
  deriving (Read, Show)

instance Bifunctor Either' where
    bimap f g = \ case JustLeft a -> JustLeft (f a)
                       JustRight b -> JustRight (g b)
                       Both a b -> Both (f a) (g b)

instance Bifoldable Either' where
    bifoldMap f g = \ case JustLeft a -> f a
                           JustRight b -> g b
                           Both a b -> f a <> g b

instance Bitraversable Either' where
    bitraverse f g = \ case JustLeft a -> JustLeft <$> f a
                            JustRight b -> JustRight <$> g b
                            Both a b -> Both <$> f a <*> g b

instance Eq2 Either' where
    liftEq2 f g x y = case (toMaybes x, toMaybes y) of
        ((am₁, bm₁), (am₂, bm₂)) -> liftEq f am₁ am₂ && liftEq g bm₁ bm₂

instance Ord2 Either' where
    liftCompare2 f g x y = case (toMaybes x, toMaybes y) of
        ((am₁, bm₁), (am₂, bm₂)) -> liftCompare f am₁ am₂ <> liftCompare g bm₁ bm₂

instance (Eq a, Eq b) => Eq (Either' a b) where (==) = eq2
instance (Ord a, Ord b) => Ord (Either' a b) where compare = compare2

instance (Semigroup a, Semigroup b) => Semigroup (Either' a b) where
    JustLeft  a₁ <> JustLeft  a₂ = JustLeft  (a₁ <> a₂)
    JustRight b₁ <> JustRight b₂ = JustRight (b₁ <> b₂)
    JustLeft  a₁ <> JustRight b₁ = Both a₁ b₁
    JustRight b₁ <> JustLeft  a₁ = Both a₁ b₁
    JustLeft  a₁ <> Both a₂ b₂   = Both (a₁ <> a₂) b₂
    JustRight b₁ <> Both a₂ b₂   = Both a₂ (b₁ <> b₂)
    Both a₁ b₁   <> JustLeft  a₂ = Both (a₁ <> a₂) b₁
    Both a₁ b₁   <> JustRight b₂ = Both a₁ (b₁ <> b₂)
    Both a₁ b₁   <> Both a₂ b₂   = Both (a₁ <> a₂) (b₁ <> b₂)

instance (Monoid a, Monoid b) => Monoid (Either' a b) where
    mempty = Both mempty mempty

fromMaybes :: Maybe a -> Maybe b -> Maybe (Either' a b)
fromMaybes Nothing Nothing = Nothing
fromMaybes (Just a) Nothing = Just (JustLeft a)
fromMaybes Nothing (Just b) = Just (JustRight b)
fromMaybes (Just a) (Just b) = Just (Both a b)

toMaybes :: Either' a b -> (Maybe a, Maybe b)
toMaybes (JustLeft a) = (Just a, Nothing)
toMaybes (JustRight b) = (Nothing, Just b)
toMaybes (Both a b) = (Just a, Just b)
