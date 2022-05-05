{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use uncurry" -}
{- HLINT ignore "Use newtype instead of data" -}

module Test.QuickCheck.Classes.Semigroup.Combinations where

import Data.Semigroup.Foldable
    ( Foldable1 (..) )
import Data.List
    ( intersperse )
import Test.QuickCheck
    ( Arbitrary (..), Gen, arbitraryBoundedEnum, oneof )
import Text.Show.Pretty
    ( ppShow )

import qualified Data.Foldable as F
import qualified Data.Semigroup.Foldable as F1

data Expr1 a
    = Expr1Const a
    deriving stock (Foldable, Functor, Traversable)
    deriving anyclass Foldable1

data Expr2 a
    = Expr2Const (Expr1 a)
    | Expr2AppendL a (Expr1 a)
    | Expr2AppendR (Expr1 a) a
    deriving stock (Foldable, Functor, Traversable)
    deriving anyclass Foldable1

data Expr3 a
    = Expr3Const (Expr2 a)
    | Expr3AppendL a (Expr2 a)
    | Expr3AppendR (Expr2 a) a
    deriving stock (Foldable, Functor, Traversable)
    deriving anyclass Foldable1

evalExpr1 :: Expr1 a -> a
evalExpr1 = \case
    Expr1Const a -> a

evalExpr2 :: Semigroup a => Expr2 a -> a
evalExpr2 = \case
    Expr2Const e -> evalExpr1 e
    Expr2AppendL a e -> a <> evalExpr1 e
    Expr2AppendR e a -> evalExpr1 e <> a

evalExpr3 :: Semigroup a => Expr3 a -> a
evalExpr3 = \case
    Expr3Const e -> evalExpr2 e
    Expr3AppendL a e -> a <> evalExpr2 e
    Expr3AppendR e a -> evalExpr2 e <> a

evalExpr :: (Foldable1 f, Show a, Semigroup a) => f a -> a
evalExpr = F1.fold1

showExpr :: (Foldable1 f, Show a) => f a -> String
showExpr = F1.intercalateMap1 " <> " show

newtype Flat a = Flat a

instance Show a => Show (Expr1 a) where
    show = showExpr

instance Show a => Show (Expr2 a) where
    show = showExpr

instance Show a => Show (Expr3 a) where
    show = showExpr

arbitraryExpr1 :: Arbitrary a => Gen (Expr1 a)
arbitraryExpr1 = Expr1Const <$> arbitrary

arbitraryExpr2 :: Arbitrary a => Gen (Expr2 a)
arbitraryExpr2 = oneof
    [ Expr2Const <$> arbitraryExpr1
    , Expr2AppendL <$> arbitrary <*> arbitraryExpr1
    , Expr2AppendR <$> arbitraryExpr1 <*> arbitrary
    ]

arbitraryExpr3 :: Arbitrary a => Gen (Expr3 a)
arbitraryExpr3 = oneof
    [ Expr3Const <$> arbitraryExpr2
    , Expr3AppendL <$> arbitrary <*> arbitraryExpr2
    , Expr3AppendR <$> arbitraryExpr2 <*> arbitrary
    ]

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

data SemigroupTuple3 s
    = Add123 Add1 Add2 Add3 (s, s, s)
    | Add132 Add1 Add3 Add2 (s, s, s)
    | Add213 Add2 Add1 Add3 (s, s, s)
    | Add231 Add2 Add3 Add1 (s, s, s)
    | Add312 Add3 Add1 Add2 (s, s, s)
    | Add321 Add3 Add2 Add1 (s, s, s)

instance (Show s, Semigroup s) => Show (SemigroupTuple2 s) where
    show (semigroupTuple2 -> (a, b)) = unlines
        [ mempty, "a:", showWrap a
        , mempty, "b:", showWrap b
        ]

instance (Show s, Semigroup s) => Show (SemigroupTuple3 s) where
    show (semigroupTuple3 -> (a, b, c)) = unlines
        [ mempty, "a:", showWrap a
        , mempty, "b:", showWrap b
        , mempty, "c:", showWrap c
        ]

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

semigroupTuple3 :: Semigroup s => SemigroupTuple3 s -> (s, s, s)
semigroupTuple3 = \case
    Add123 f g h (a, b, c) -> (add1 (a) f, add2 (a, b) g, add3 (a, b, c) h)
    Add132 f g h (a, b, c) -> (add1 (a) f, add3 (a, b, c) g, add2 (a, b) h)
    Add213 f g h (a, b, c) -> (add2 (a, b) f, add1 (a) g, add3 (a, b, c) h)
    Add231 f g h (a, b, c) -> (add2 (a, b) f, add3 (a, b, c) g, add1 (a) h)
    Add312 f g h (a, b, c) -> (add3 (a, b, c) f, add1 (a) g, add2 (a, b) h)
    Add321 f g h (a, b, c) -> (add3 (a, b, c) f, add2 (a, b) g, add1 (a) h)

arbitrarySemigroupTuple2 :: Arbitrary s => Gen (SemigroupTuple2 s)
arbitrarySemigroupTuple2 = oneof
    [r Add11, r Add12, r Add21, r Add22, r Add23, r Add32, r Add33]
  where
    r x = x <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum <*> arbitrary

arbitrarySemigroupTuple3 :: Arbitrary s => Gen (SemigroupTuple3 s)
arbitrarySemigroupTuple3 = oneof
    [r Add123, r Add132, r Add213, r Add231, r Add312, r Add321]
  where
    r x = x
        <$> arbitraryBoundedEnum
        <*> arbitraryBoundedEnum
        <*> arbitraryBoundedEnum
        <*> arbitrary

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

shrinkSemigroupTuple3
    :: Arbitrary s
    => SemigroupTuple3 s
    -> [SemigroupTuple3 s]
shrinkSemigroupTuple3 = \case
    Add123 f g h (a, b, c) -> Add123 f g h <$> shrink (a, b, c)
    Add132 f g h (a, b, c) -> Add132 f g h <$> shrink (a, b, c)
    Add213 f g h (a, b, c) -> Add213 f g h <$> shrink (a, b, c)
    Add231 f g h (a, b, c) -> Add231 f g h <$> shrink (a, b, c)
    Add312 f g h (a, b, c) -> Add312 f g h <$> shrink (a, b, c)
    Add321 f g h (a, b, c) -> Add321 f g h <$> shrink (a, b, c)

instance Arbitrary s => Arbitrary (SemigroupTuple2 s) where
    arbitrary = arbitrarySemigroupTuple2
    shrink = shrinkSemigroupTuple2

instance Arbitrary s => Arbitrary (SemigroupTuple3 s) where
    arbitrary = arbitrarySemigroupTuple3
    shrink = shrinkSemigroupTuple3
