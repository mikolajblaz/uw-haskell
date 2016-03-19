module MyArray where

import MyIndex

data BST i e = Empty | Node {key :: i, val :: e, left, right :: (BST i e)}
  deriving (Eq, Show)

contains  :: (Ix i) => i -> BST i e -> Bool
contains _ Empty = False
contains k (Node kn _ l r) | k < kn = contains k l
                           | k > kn = contains k r
                           | otherwise = True

insert    :: (Ix i) => i -> e -> BST i e -> BST i e
insert k v Empty = Node k v Empty Empty
insert k v (Node kn vn l r) | k < kn = Node kn vn (insert k v l) r
                            | k > kn = Node kn vn l (insert k v r)
                            | otherwise = Node kn v l r

getElem   :: (Ix i) => i -> BST i e -> e
getElem _ Empty = error "No such index"
getElem k (Node kn vn l r) | k < kn = getElem k l
                           | k > kn = getElem k r
                           | otherwise = vn

fromList  :: (Ix i) => [(i, e)] -> BST i e
fromList kvs = foldr (\(k, v) tree -> insert k v tree) Empty kvs

toList    :: (Ix i) => BST i e -> [e]
toList Empty = []
toList (Node _ vn l r) = (toList l) ++ (vn : toList r)



data Array i e = Arr {rng :: (i, i), tree :: (BST i e)}
  deriving (Eq, Show)

-- | Buduje tablicę dla danego zakresu i listy elementów
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray r es = array r (zip (range r) es)

-- | Daje element tablicy o podanym indeksie
(!)       :: Ix i => Array i e -> i -> e
(!) (Arr r t) k | inRange r k = getElem k t
                | otherwise = error "Index out of range"

-- | Daje listę elementów tablicy (w kolejności indeksów)
elems     :: Ix i => Array i e -> [e]
elems (Arr _ t) = toList t

-- | Buduje tablicę z podanej listy par (indeks,wartość)
array     :: (Ix i) => (i, i) -> [(i,e)] -> Array i e
array r kvs = Arr r (fromList kvs)

-- | Daje tablicę będącą wariantem danej, zmienioną pod podanym indeksem
update    :: Ix i => i -> e -> Array i e -> Array i e
update k v (Arr r t) | inRange r k = Arr r (insert k v t)
                     | otherwise = error "Index out of range"

-- | Daje tablicę będącą wariantem danej, zmienioną pod podanymi indeksami
(//)      :: (Ix i ) => Array i e -> [(i,e)] -> Array i e
(//) arr kvs = foldr (\(k, v) a -> update k v a) arr kvs
