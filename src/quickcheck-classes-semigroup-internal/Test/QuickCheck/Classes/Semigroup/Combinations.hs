{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use uncurry" -}

module Test.QuickCheck.Classes.Semigroup.Combinations where

import Test.QuickCheck
    ( Arbitrary (..), Gen, oneof )

data SemigroupCombination2 s
    = A_A   s
    | AA_A  s
    | A_AA  s
    | AB_A  s s
    | A_AB  s s
    | BA_A  s s
    | A_BA  s s
    | AB_AC s s s
    | AC_AB s s s
    | BA_CA s s s
    | CA_BA s s s

semigroupCombination2 :: Semigroup s => SemigroupCombination2 s -> (s, s)
semigroupCombination2 = \case
    A_A   a     -> (a, a)
    AA_A  a     -> (a <> a, a)
    A_AA  a     -> (a, a <> a)
    AB_A  a b   -> (a <> b, a)
    A_AB  a b   -> (a, a <> b)
    BA_A  a b   -> (b <> a, a)
    A_BA  a b   -> (a, b <> a)
    AB_AC a b c -> (a <> b, a <> c)
    AC_AB a b c -> (a <> c, a <> b)
    BA_CA a b c -> (b <> a, c <> a)
    CA_BA a b c -> (c <> a, b <> a)

instance Arbitrary s => Arbitrary (SemigroupCombination2 s) where
    arbitrary = arbitrarySemigroupCombination2
    shrink = shrinkSemigroupCombination2

arbitrarySemigroupCombination2 :: Arbitrary s => Gen (SemigroupCombination2 s)
arbitrarySemigroupCombination2 = oneof
    [ A_A   <$> arbitrary
    , AA_A  <$> arbitrary
    , A_AA  <$> arbitrary
    , AB_A  <$> arbitrary <*> arbitrary
    , A_AB  <$> arbitrary <*> arbitrary
    , BA_A  <$> arbitrary <*> arbitrary
    , A_BA  <$> arbitrary <*> arbitrary
    , AB_AC <$> arbitrary <*> arbitrary <*> arbitrary
    , AC_AB <$> arbitrary <*> arbitrary <*> arbitrary
    , BA_CA <$> arbitrary <*> arbitrary <*> arbitrary
    , CA_BA <$> arbitrary <*> arbitrary <*> arbitrary
    ]

shrinkSemigroupCombination2
    :: Arbitrary s => SemigroupCombination2 s -> [SemigroupCombination2 s]
shrinkSemigroupCombination2 = \case
    A_A   a     ->            A_A   <$> shrink a
    AA_A  a     ->            AA_A  <$> shrink a
    A_AA  a     ->            A_AA  <$> shrink a
    AB_A  a b   -> fromTuple2 AB_A  <$> shrink (a, b)
    A_AB  a b   -> fromTuple2 A_AB  <$> shrink (a, b)
    BA_A  a b   -> fromTuple2 BA_A  <$> shrink (a, b)
    A_BA  a b   -> fromTuple2 A_BA  <$> shrink (a, b)
    AB_AC a b c -> fromTuple3 AB_AC <$> shrink (a, b, c)
    AC_AB a b c -> fromTuple3 AC_AB <$> shrink (a, b, c)
    BA_CA a b c -> fromTuple3 BA_CA <$> shrink (a, b, c)
    CA_BA a b c -> fromTuple3 CA_BA <$> shrink (a, b, c)
  where
    fromTuple2 :: (a -> b -> t) -> (a, b) -> t
    fromTuple2 f (a, b) = f a b
    fromTuple3 :: (a -> b -> c -> t) -> (a, b, c) -> t
    fromTuple3 f (a, b, c) = f a b c
