module Type where

import Data.String
import Data.Monoid

newtype TVar = TV String deriving (Eq, Ord)

instance Show TVar where
  show (TV x) = x

instance IsString TVar where
  fromString = TV

newtype Poly = P (Either (TVar, Poly) Mono) deriving (Eq)
newtype Mono = M (Either (String, [Mono]) TVar) deriving (Eq)

(-->) :: Mono -> Mono -> Mono
a --> b = M (Left ("->", [a, b]))
infixr 5 -->

instance Show Mono where
  showsPrec d (M (Right a)) = showsPrec d a
  showsPrec d (M (Left (t, ts))) = showParen (d > 9) inner
    where
    ss x = showsPrec 11 x
    [a, b] = ts
    inner
      | t == "->" = ss a . (" -> " ++) . ss b
      | otherwise = mconcat $ (t ++) : map ss ts

instance Show Poly where
  showsPrec d (P (Left (a, p))) = showParen (d > 9) $
    showString "forall " .
    shows a .
    showString ". " .
    shows p
  showsPrec d (P (Right m)) = showsPrec d m

class Type a where
  toPoly :: a -> Poly

instance Type Poly where
  toPoly = id

instance Type Mono where
  toPoly = P . Right

instance IsString Mono where
  fromString = M . Right . fromString

instance IsString Poly where
  fromString = P . Right . fromString

class Free a where
  free :: a -> [TVar]

instance Free Mono where
  free (M (Right a)) = [a]
  free (M (Left (_, ts))) = ts >>= free

instance Free Poly where
  free (P (Left (a, p))) = filter (/= a) (free p)
  free (P (Right m)) = free m
