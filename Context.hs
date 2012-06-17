
module Context
( Context ()
, makeContext
, unContext
, findContext
, addContext
) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Type

newtype Context = C
  { unC :: M.Map TVar Poly
  } deriving (Eq)

instance Show Context where
  show c = show [I $ show v ++ " : " ++ show t | (v, t) <- unContext c]

newtype I = I String

instance Show I where
  show (I i) = i

instance Free Context where
  free = F.foldMap free . unC

instance Monoid Context where
  mempty = C mempty
  mappend a b = C $ unC a `mappend` unC b
  mconcat xs = C $ mconcat $ map unC xs

unContext = M.toList . unC
makeContext = C . M.fromList
addContext k v = C . M.insert k v . unC
findContext k = M.lookup k . unC
