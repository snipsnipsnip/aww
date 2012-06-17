
module Context
( Context ()
, unContext
, makeContext
) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Type

newtype Context = C
  { unC :: M.Map TVar Poly
  } deriving (Eq)

instance Show Context where
  show c = show [I $ show v ++ " : " ++ show t | (v, t) <- unContext c]

newtype I = I String

instance Show I where
  show (I i) = i

unContext = M.toList . unC
makeContext = C . M.fromList

instance Free Context where
  free = F.foldMap free . unC
