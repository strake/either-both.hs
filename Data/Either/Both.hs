module Data.Either.Both where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Associative (Associative (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Filtrable
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Alt (..))

data Either' a b = JustLeft a | JustRight b | Both a b
  deriving (Read, Show, Foldable, Functor, Traversable)

either' :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Either' a b -> c
either' f g h = \ case
    JustLeft a -> f a
    JustRight b -> g b
    Both a b -> h a b

fromEither' :: a -> b -> Either' a b -> (a, b)
fromEither' a b = either' (flip (,) b) ((,) a) (,)

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

instance Associative Either' where
    assoc = \ case
        JustLeft (JustLeft a) -> JustLeft a
        JustLeft (JustRight b) -> JustRight (JustLeft b)
        JustLeft (Both a b) -> Both a (JustLeft b)
        JustRight c -> JustRight (JustRight c)
        Both (JustLeft a) c -> Both a (JustRight c)
        Both (JustRight b) c -> JustRight (Both b c)
        Both (Both a b) c -> Both a (Both b c)
    disassoc = \ case
        JustLeft a -> JustLeft (JustLeft a)
        JustRight (JustLeft b) -> JustLeft (JustRight b)
        Both a (JustLeft b) -> JustLeft (Both a b)
        JustRight (JustRight c) -> JustRight c
        Both a (JustRight c) -> Both (JustLeft a) c
        JustRight (Both b c) -> Both (JustRight b) c
        Both a (Both b c) -> Both (Both a b) c

instance Semigroup a => Applicative (Either' a) where
    pure = JustRight
    JustLeft a₁ <*> JustLeft a₂ = JustLeft (a₁ <> a₂)
    JustLeft a₁ <*> JustRight _ = JustLeft a₁
    JustLeft a₁ <*> Both a₂ _ = JustLeft (a₁ <> a₂)
    JustRight _ <*> JustLeft a = JustLeft a
    JustRight f <*> JustRight b = JustRight (f b)
    JustRight f <*> Both a b = Both a (f b)
    Both a₁ _ <*> JustLeft a₂ = JustLeft (a₁ <> a₂)
    Both a₁ f <*> JustRight b = Both a₁ (f b)
    Both a₁ f <*> Both a₂ b = Both (a₁ <> a₂) (f b)

instance Semigroup a => Monad (Either' a) where
    am >>= f = case f <$> am of
        JustLeft a -> JustLeft a
        JustRight (JustLeft a) -> JustLeft a
        JustRight (JustRight b) -> JustRight b
        JustRight (Both a b) -> Both a b
        Both a₁ (JustLeft a₂) -> JustLeft (a₁ <> a₂)
        Both a₁ (JustRight b) -> Both a₁ b
        Both a₁ (Both a₂ b) -> Both (a₁ <> a₂) b

instance Eq a => Eq1 (Either' a) where liftEq = liftEq2 (==)
instance Ord a => Ord1 (Either' a) where liftCompare = liftCompare2 compare

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

partitionEithers' :: Filtrable f => f (Either' a b) -> (f a, f b)
partitionEithers' = (,) <$> mapMaybe (getAlt . bifoldMap pure mempty) <*> mapMaybe (getAlt . bifoldMap mempty pure)

partitionEithers1 :: NonEmpty (Either a b) -> Either' (NonEmpty a) (NonEmpty b)
partitionEithers1 (x:|xs) = case (x, ls, rs) of
    (Left y, ys, []) -> JustLeft (y:|ys)
    (Left y, ys, z:zs) -> Both (y:|ys) (z:|zs)
    (Right z, [], zs) -> JustRight (z:|zs)
    (Right z, y:ys, zs) -> Both (y:|ys) (z:|zs)
  where
    (ls, rs) = partitionEithers xs

distrEitherPair' :: Either' (a, b) c -> (Either' a c, Either' b c)
distrEitherPair' (JustLeft (a, b))    = (JustLeft a, JustLeft b)
distrEitherPair' (JustRight c)         = (JustRight c, JustRight c)
distrEitherPair' (Both (a, b) c) = (Both a c, Both b c)

undistrEitherPair' :: (Either' a c, Either' b c) -> Either' (a, b) c
undistrEitherPair' (JustLeft a,    JustLeft b)    = JustLeft (a, b)
undistrEitherPair' (JustRight c,    JustRight _)    = JustRight c
undistrEitherPair' (Both a c, Both b _) = Both (a, b) c
undistrEitherPair' (JustLeft _,    JustRight c)    = JustRight c
undistrEitherPair' (JustLeft a,    Both b c) = Both (a, b) c
undistrEitherPair' (JustRight c,    JustLeft _)    = JustRight c
undistrEitherPair' (JustRight c,    Both _ _) = JustRight c
undistrEitherPair' (Both a c, JustLeft b)    = Both (a, b) c
undistrEitherPair' (Both _ c, JustRight _)    = JustRight c

distrPairEither' :: (Either' a b, c) -> Either' (a, c) (b, c)
distrPairEither' (JustLeft a,    c) = JustLeft (a, c)
distrPairEither' (JustRight b,    c) = JustRight (b, c)
distrPairEither' (Both a b, c) = Both (a, c) (b, c)

undistrPairEither' :: Either' (a, c) (b, c) -> (Either' a b, c)
undistrPairEither' (JustLeft (a, c))         = (JustLeft a, c)
undistrPairEither' (JustRight (b, c))         = (JustRight b, c)
undistrPairEither' (Both (a, c) (b, _)) = (Both a b, c)
