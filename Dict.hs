
module Dict
( Dict ()
, listDict
, makeDict
, findDict
, pairDict
) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Type
import Expr

type Context = Dict TVar
type Env = Dict Var

newtype Dict key = D
  { unD :: M.Map key Poly
  } deriving (Eq)

instance Ord k => Free (Dict k) where
  free = F.foldMap free . unD

instance Ord k => Monoid (Dict k) where
  mempty = D mempty
  mappend a b = D $ unD a `mappend` unD b
  mconcat xs = D $ mconcat $ map unD xs

listDict :: Dict a -> [(a, Poly)]
listDict = M.toList . unD

makeDict :: (Ord a) => [(a, Poly)] -> Dict a
makeDict = D . M.fromList

pairDict :: a -> Poly -> Dict a
pairDict k v = D $ M.singleton k v

findDict :: (Ord a) => a -> Dict a -> Maybe Poly
findDict k = M.lookup k . unD

