module MyArray(
  Array,
  listArray, (!), elems, array, update, (//), present,
  range, index, inRange, rangeSize
) where

import MyIndex

half :: (Int, Int) -> Int
half (a, b) = (a + b) `div` 2

-- Typ drzew przedziałowych wraz z podstawową implementacją
data IntervalTree e =
    Branch      (Int, Int) (IntervalTree e) (IntervalTree e) |
    EmptyBranch (Int, Int) (IntervalTree e) (IntervalTree e) |
    Leaf e |
    EmptyLeaf
  deriving (Eq, Show)


-- | Tworzy pusty węzeł z rekurencyjnie tworzonymi dziećmi
makeEmpty :: (Int, Int) -> IntervalTree e
makeEmpty (a, b) | a == b = EmptyLeaf
                 | otherwise = EmptyBranch (a, b) (makeEmpty (a, s)) (makeEmpty (s + 1, b))
  where s = half (a, b)

unsafeFind :: Int -> IntervalTree e -> IntervalTree e
unsafeFind k (Branch ran l r) | k <= half ran = unsafeFind k l
                              | otherwise     = unsafeFind k r
unsafeFind k n = n

contains  :: (Ix i) => i -> IntervalTree e -> Bool
contains _ (EmptyBranch _ _ _) = False
contains _ (EmptyLeaf) = False
contains _ (Leaf _) = True
contains k (Branch ran l r) | k <= half ran = contains k l
                            | otherwise     = contains k r

unsafeInsert :: (Ix i) => i -> e -> IntervalTree e -> IntervalTree e
unsafeInsert _ v EmptyLeaf = Leaf v
unsafeInsert _ v (Leaf vn) = Leaf v
unsafeInsert k v (EmptyBranch ran l r) | k <= half ran = Branch ran (unsafeInsert k v l) r
                                       | otherwise     = Branch ran l (unsafeInsert k v r)
unsafeInsert k v (Branch ran l r)      | k <= half ran = Branch ran (unsafeInsert k v l) r
                                       | otherwise     = Branch ran l (unsafeInsert k v r)

insert :: (Ix i) => i -> e -> IntervalTree e -> IntervalTree e
insert k v t = undefined

getElem   :: (Ix i) => i -> IntervalTree e -> e
getElem _ EmptyLeaf           = error "No such index"
getElem _ (EmptyBranch _ _ _) = error "No such index"
getElem _ (Leaf v) = v
getElem k (Branch ran l r) | k <= half ran = getElem k l
                           | otherwise     = getElem k r

fromList  :: (Ix i) => (i, i) -> [(i, e)] -> IntervalTree e
fromList ran kvs = foldr (\(k, v) tree -> insert k v tree) (makeEmpty ran) kvs

toList    :: (Ix i) => IntervalTree e -> [e]
toList EmptyLeaf           = []
toList (EmptyBranch _ _ _) = []
toList (Leaf v) = [v]
toList (Branch _ vn l r) = (toList l) ++ (vn : toList r)



-- Tablice
data Array i e = Arr {rng :: (i, i), tree :: (IntervalTree e)}
  deriving (Eq)

instance (Ix i, Show i, Show e) => Show (Array i e) where
  show a@(Arr r _) = "array " ++ (show r) ++ " " ++ (show $ elems a)

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

-- | Sprawdza czy w tablicy znajduje się coś pod danym indeksem
present   :: Ix i => i -> Array i e -> Bool
present k (Arr _ t) = contains k t
