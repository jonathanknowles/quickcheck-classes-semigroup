{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use uncurry" -}

module Test.QuickCheck.Classes.Semigroup.Combinations where

import Test.QuickCheck
    ( Arbitrary (..), Gen, arbitraryBoundedEnum, oneof )
import Text.Show.Pretty
    ( ppShow )

import qualified Data.Foldable as F

data Add1 = A
    deriving (Bounded, Enum, Eq, Ord)

data Add2 = AB | BA
    deriving (Bounded, Enum, Eq, Ord)

data Add3 = ABC | ACB | BAC | BCA | CAB | CBA
    deriving (Bounded, Enum, Eq, Ord)

add1 :: Semigroup s => s -> Add1 -> s
add1 a = \case
    A -> a

add2 :: Semigroup s => (s, s) -> Add2 -> s
add2 (a, b) = \case
    AB -> a <> b
    BA -> a <> a

add3 :: Semigroup s => (s, s, s) -> Add3 -> s
add3 (a, b, c) = \case
    ABC -> a <> b <> c
    ACB -> a <> c <> b
    BAC -> b <> a <> c
    BCA -> b <> c <> a
    CAB -> c <> a <> b
    CBA -> c <> b <> a

data SemigroupTuple2 s
    = Add11 Add1 Add1 s
    | Add12 Add1 Add2 (s, s)
    | Add21 Add2 Add1 (s, s)
    | Add22 Add2 Add2 (s, s)
    | Add23 Add2 Add3 (s, s, s)
    | Add32 Add3 Add2 (s, s, s)
    | Add33 Add3 Add3 (s, s, s)
    deriving (Eq, Ord)

instance (Show s, Semigroup s) => Show (SemigroupTuple2 s) where
    show c = unlines
        [ mempty, "a:", showWrap a
        , mempty, "b:", showWrap b
        ]
      where
        (a, b) = semigroupTuple2 c

showWrap :: Show a => a -> String
showWrap x
    | singleLineMaxLengthExceeded =
        multiLine
    | otherwise =
        singleLine
  where
    multiLine = ppShow x
    singleLine = show x
    singleLineMaxLength = 80
    singleLineMaxLengthExceeded = F.length singleLine > singleLineMaxLength

semigroupTuple2 :: Semigroup s => SemigroupTuple2 s -> (s, s)
semigroupTuple2 = \case
    Add11 f g (a      ) -> (add1 (a      ) f, add1 (a      ) g)
    Add12 f g (a, b   ) -> (add1 (a      ) f, add2 (a, b   ) g)
    Add21 f g (a, b   ) -> (add2 (a, b   ) f, add1 (a      ) g)
    Add22 f g (a, b   ) -> (add2 (a, b   ) f, add2 (a, b   ) g)
    Add23 f g (a, b, c) -> (add2 (a, b   ) f, add3 (a, b, c) g)
    Add32 f g (a, b, c) -> (add3 (a, b, c) f, add2 (a, b   ) g)
    Add33 f g (a, b, c) -> (add3 (a, b, c) f, add3 (a, b, c) g)

arbitrarySemigroupTuple2 :: Arbitrary s => Gen (SemigroupTuple2 s)
arbitrarySemigroupTuple2 = oneof
    [r Add11, r Add12, r Add21, r Add22, r Add23, r Add32, r Add33]
  where
    r x = x <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum <*> arbitrary

shrinkSemigroupTuple2
    :: Arbitrary s
    => SemigroupTuple2 s
    -> [SemigroupTuple2 s]
shrinkSemigroupTuple2 = \case
    Add11 f g (a      ) -> Add11 f g <$> shrink (a      )
    Add12 f g (a, b   ) -> Add12 f g <$> shrink (a, b   )
    Add21 f g (a, b   ) -> Add21 f g <$> shrink (a, b   )
    Add22 f g (a, b   ) -> Add22 f g <$> shrink (a, b   )
    Add23 f g (a, b, c) -> Add23 f g <$> shrink (a, b, c)
    Add32 f g (a, b, c) -> Add32 f g <$> shrink (a, b, c)
    Add33 f g (a, b, c) -> Add33 f g <$> shrink (a, b, c)

instance Arbitrary s => Arbitrary (SemigroupTuple2 s) where
    arbitrary = arbitrarySemigroupTuple2
    shrink = shrinkSemigroupTuple2
