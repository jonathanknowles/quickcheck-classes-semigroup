{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use uncurry" -}

module Test.QuickCheck.Classes.Semigroup.Combinations where

import Test.QuickCheck
    ( Arbitrary (..), Gen, oneof )

data SemigroupCombination1 s
    = A  s
    | AA s
    | AB s s
    | BA s s

data SemigroupCombination2 s
    = A___A s
    | AA__A s
    | A__AA s
    | AB__A s s
    | A__AB s s
    | BA__A s s
    | A__BA s s
    | AB_AC s s s
    | AC_AB s s s
    | BA_CA s s s
    | CA_BA s s s
    deriving (Eq, Show)

semigroupCombination2 :: Semigroup s => SemigroupCombination2 s -> (s, s)
semigroupCombination2 = \case
    A___A a     -> (a     , a     )
    AA__A a     -> (a <> a, a     )
    A__AA a     -> (a     , a <> a)
    AB__A a b   -> (a <> b, a     )
    A__AB a b   -> (a     , a <> b)
    BA__A a b   -> (b <> a, a     )
    A__BA a b   -> (a     , b <> a)
    AB_AC a b c -> (a <> b, a <> c)
    AC_AB a b c -> (a <> c, a <> b)
    BA_CA a b c -> (b <> a, c <> a)
    CA_BA a b c -> (c <> a, b <> a)

instance Arbitrary s => Arbitrary (SemigroupCombination2 s) where
    arbitrary = arbitrarySemigroupCombination2
    shrink = shrinkSemigroupCombination2

arbitrarySemigroupCombination2 :: Arbitrary s => Gen (SemigroupCombination2 s)
arbitrarySemigroupCombination2 = oneof
    [ A___A <$> arbitrary
    , AA__A <$> arbitrary
    , A__AA <$> arbitrary
    , AB__A <$> arbitrary <*> arbitrary
    , A__AB <$> arbitrary <*> arbitrary
    , BA__A <$> arbitrary <*> arbitrary
    , A__BA <$> arbitrary <*> arbitrary
    , AB_AC <$> arbitrary <*> arbitrary <*> arbitrary
    , AC_AB <$> arbitrary <*> arbitrary <*> arbitrary
    , BA_CA <$> arbitrary <*> arbitrary <*> arbitrary
    , CA_BA <$> arbitrary <*> arbitrary <*> arbitrary
    ]

shrinkSemigroupCombination2
    :: Arbitrary s => SemigroupCombination2 s -> [SemigroupCombination2 s]
shrinkSemigroupCombination2 = \case
    A___A a     ->            A___A <$> shrink a
    AA__A a     ->            AA__A <$> shrink a
    A__AA a     ->            A__AA <$> shrink a
    AB__A a b   -> fromTuple2 AB__A <$> shrink (a, b)
    A__AB a b   -> fromTuple2 A__AB <$> shrink (a, b)
    BA__A a b   -> fromTuple2 BA__A <$> shrink (a, b)
    A__BA a b   -> fromTuple2 A__BA <$> shrink (a, b)
    AB_AC a b c -> fromTuple3 AB_AC <$> shrink (a, b, c)
    AC_AB a b c -> fromTuple3 AC_AB <$> shrink (a, b, c)
    BA_CA a b c -> fromTuple3 BA_CA <$> shrink (a, b, c)
    CA_BA a b c -> fromTuple3 CA_BA <$> shrink (a, b, c)
  where
    fromTuple2 :: (a -> b -> t) -> (a, b) -> t
    fromTuple2 f (a, b) = f a b
    fromTuple3 :: (a -> b -> c -> t) -> (a, b, c) -> t
    fromTuple3 f (a, b, c) = f a b c
