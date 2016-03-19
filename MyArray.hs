module MyArray where

import MyIndex

data BST i e = Empty | Node {key :: i, value :: e, left, right :: (BST i e)}
  deriving (Eq, Show)

data Array i e = Arr (i, i) (BST i e)
