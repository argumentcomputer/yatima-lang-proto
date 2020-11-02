{-
Module      : Yatima.Core.UnionFind
Description : An lightweight implementation of Tarjan's Union-Find algorithm for
IPFS content-ids. This is a concretized port of the /equivalence/ package.
Copyright   : 2020 Yatima Inc
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental

The union-find algorithm works as follows:

Each equivalence class has one member, or root, that serves as its
representative element. Every element in the class is either the root
(distance 0), points directly to the root (distance 1),
or points to an element with a smaller distance to the root.

Therefore, whenever we want to test whether two elements are in the same
class, we follow their references until we hit their roots, and then compare
their roots for equality.

This algorithm performs lazy path compression. Whenever we traverse a path
containing nodes with a distance from root > 1, once we hit the root we
update all the nodes in that path to point to the root directly:

@
*           *
|         /   \
a   =>   a     b
|
b
@

Additionally, when we merge two classes via `union`, the root of the smaller
class will point to the root of the larger

@
*1      *2           *2
|   +   |    =>    /  |
a       b         *1  b
        |         |   |
        c         a   c
@
-}

module Yatima.Core.UnionFind where

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad
import           Data.STRef

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M

import           Data.IPLD.CID

-- A reference to a node
newtype NRef s = NRef { _ref :: STRef s (Node s) } deriving Eq

-- A Node is either root or a link
data Node s
  = Root {_value :: CID, _weight :: Int}
  | Node {_value :: CID, _parent :: NRef s}

-- An equivalence relation is a reference to a map of elements to node references
data Equiv s = Equiv {_elems :: STRef s (Map CID (NRef s))}

-- A new equivalence relation
newEquiv :: ST s (Equiv s)
newEquiv = Equiv <$> (newSTRef M.empty)

-- create a new class in a relation with a value as root
singleton :: Equiv s -> CID -> ST s (NRef s)
singleton eq val = do
  root <- NRef <$> newSTRef (Root {_value = val, _weight = 1})
  modifySTRef (_elems eq) (M.insert val root)
  return root

-- given a reference, find a reference to the root of its equivalence class.
-- This function performs path compression
findRoot :: NRef s -> ST s (NRef s)
findRoot ref = do
  node <- readSTRef (_ref ref)
  case node of
    Root {} -> return ref
    Node {_parent = refToParent} -> do
      refToRoot <- findRoot refToParent
      if refToRoot /= refToParent then
        writeSTRef (_ref ref) node{_parent = refToRoot} >> return refToRoot
      else return refToRoot

-- combine two equivalence classes, merging the smaller into the larger
union :: NRef s -> NRef s -> ST s ()
union refX refY = do
  refToRootX <- findRoot refX
  refToRootY <- findRoot refY
  when (refToRootX /= refToRootY) $ do
    (Root vx wx) <- readSTRef (_ref refToRootX)
    (Root vy wy) <- readSTRef (_ref refToRootY)
    if (wx >= wy) then do
      writeSTRef (_ref refToRootY) (Node vy refToRootX)
      writeSTRef (_ref refToRootX) (Root vx (wx + wy))
    else do
      writeSTRef (_ref refToRootX) (Node vx refToRootY)
      writeSTRef (_ref refToRootY) (Root vy (wx + wy))

-- Are these two references pointing to the same root?
getRef :: Equiv s -> CID -> ST s (Maybe (NRef s))
getRef eq x = do
  m <- readSTRef (_elems eq)
  return $ M.lookup x m

equivalent :: Equiv s -> CID -> CID -> ST s Bool
equivalent eq x y = do
  rx <- getRef eq x
  ry <- getRef eq y
  case (rx, ry) of
    (Just x, Just y) -> liftA2 (==) (findRoot x) (findRoot y)
    _                -> return $ x == y

equate :: Equiv s -> CID -> CID -> ST s ()
equate eq x y = do
  rx <- (maybe (singleton eq x) return) =<< (getRef eq x)
  ry <- (maybe (singleton eq y) return) =<< (getRef eq y)
  union rx ry
