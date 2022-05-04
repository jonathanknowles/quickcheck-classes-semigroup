{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use uncurry" -}

module Test.QuickCheck.Classes.Semigroup.Combinations where

import Test.QuickCheck
    ( Arbitrary (..), Gen, arbitraryBoundedEnum )

data SemigroupCombinationPattern
    = A
    | AB
    | BA
    | ABC
    | ACB
    | BAC
    | BCA
    | CAB
    | CBA
    deriving (Bounded, Enum, Eq, Ord, Show)

arbitrarySemigroupCombinationPattern :: Gen SemigroupCombinationPattern
arbitrarySemigroupCombinationPattern = arbitraryBoundedEnum

semigroupCombinationPattern1
    :: Semigroup s
    => (s, s, s)
    -> SemigroupCombinationPattern
    -> s
semigroupCombinationPattern1 (a, b, c) = \case
    A   -> a
    AB  -> a <> b
    BA  -> b <> a
    ABC -> a <> b <> c
    ACB -> a <> c <> b
    BAC -> b <> a <> c
    BCA -> b <> c <> a
    CAB -> c <> a <> b
    CBA -> b <> b <> a

data SemigroupCombinationPattern2 = SemigroupCombinationPattern2
        SemigroupCombinationPattern
        SemigroupCombinationPattern
    deriving (Eq, Ord, Show)

arbitrarySemigroupCombinationPattern2 :: Gen SemigroupCombinationPattern2
arbitrarySemigroupCombinationPattern2 = SemigroupCombinationPattern2
    <$> arbitrarySemigroupCombinationPattern
    <*> arbitrarySemigroupCombinationPattern

semigroupCombinationPattern2
    :: Semigroup s
    => (s, s, s)
    -> SemigroupCombinationPattern2
    -> (s, s)
semigroupCombinationPattern2 t (SemigroupCombinationPattern2 x y) =
    (semigroupCombinationPattern1 t x, semigroupCombinationPattern1 t y)

data SemigroupCombination2 s =
    SemigroupCombination2 SemigroupCombinationPattern2 (s, s, s)
    deriving Eq

instance (Show s, Semigroup s) => Show (SemigroupCombination2 s) where
    show c = unlines
        [ "\na:"
        , show a
        , "\nb:"
        , show b
        ]
      where (a, b) = semigroupCombination2 c

semigroupCombination2 :: Semigroup s => SemigroupCombination2 s -> (s, s)
semigroupCombination2 (SemigroupCombination2 pattern t) =
    semigroupCombinationPattern2 t pattern

arbitrarySemigroupCombination2 :: Arbitrary s => Gen (SemigroupCombination2 s)
arbitrarySemigroupCombination2 = SemigroupCombination2
    <$> arbitrarySemigroupCombinationPattern2
    <*> arbitrary

shrinkSemigroupCombination2
    :: Arbitrary s
    => SemigroupCombination2 s
    -> [SemigroupCombination2 s]
shrinkSemigroupCombination2 (SemigroupCombination2 pattern tuple) =
    SemigroupCombination2 pattern <$> shrink tuple

instance Arbitrary s => Arbitrary (SemigroupCombination2 s) where
    arbitrary = arbitrarySemigroupCombination2
    shrink = shrinkSemigroupCombination2
