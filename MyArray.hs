module MyArray(
  Array,
  listArray, (!), elems, array, update, (//), present, empty,
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
makeEmpty (a, b) | a < b = EmptyBranch (a, b) (makeEmpty (a, s)) (makeEmpty (s + 1, b))
                 | otherwise = EmptyLeaf
  where s = half (a, b)

unsafeFind :: Int -> IntervalTree e -> IntervalTree e
unsafeFind k (Branch rng l r) | k <= half rng = unsafeFind k l
                              | otherwise     = unsafeFind k r
unsafeFind _ n = n

unsafeContains  :: Int -> IntervalTree e -> Bool
unsafeContains k t = case unsafeFind k t of
  Leaf _    -> True
  otherwise -> False

unsafeGetElem   :: Int -> IntervalTree e -> e
unsafeGetElem k t = case unsafeFind k t of
  Leaf v    -> v
  otherwise -> error "No such index"

unsafeInsert :: Int -> e -> IntervalTree e -> IntervalTree e
unsafeInsert _ v EmptyLeaf = Leaf v
unsafeInsert _ v (Leaf _)  = Leaf v
unsafeInsert k v (EmptyBranch rng l r) | k <= half rng = Branch rng (unsafeInsert k v l) r
                                       | otherwise     = Branch rng l (unsafeInsert k v r)
unsafeInsert k v (Branch rng l r)      | k <= half rng = Branch rng (unsafeInsert k v l) r
                                       | otherwise     = Branch rng l (unsafeInsert k v r)

insert :: Int -> e -> IntervalTree e -> IntervalTree e
insert _ v EmptyLeaf = Leaf v
insert _ v (Leaf _)  = Leaf v
insert k v t@(EmptyBranch rng _ _) | inRange rng k = unsafeInsert k v t
                                   | otherwise     = error "Index out of range"
insert k v t@(Branch rng l r)      | inRange rng k = unsafeInsert k v t
                                   | otherwise     = error "Index out of range"

toList    :: IntervalTree e -> [e]
toList EmptyLeaf           = []
toList (EmptyBranch _ _ _) = []
toList (Leaf v) = [v]
toList (Branch _ l r) = (toList l) ++ (toList r)



-- | Do implementacji tablic używamy drzew indeksowanych typem Int. Przed każdą
-- operacją na tablicy zamieniamy typ (Ix i) na typ Int za pomocą funkcji 'index'.
data Array i e = Arr {rng :: (i, i), tree :: (IntervalTree e)}
  deriving (Eq)

instance (Ix i, Show i, Show e) => Show (Array i e) where
  show a@(Arr rng _) = "array " ++ (show rng) ++ " " ++ (show $ elems a)

-- | Buduje tablicę dla danego zakresu i listy elementów
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray rng vs = array rng (zip (range rng) vs)

-- | Buduje tablicę z podanej listy par (indeks,wartość)
array     :: (Ix i) => (i, i) -> [(i,e)] -> Array i e
array rng kvs = (empty rng) // kvs

-- | Daje tablicę będącą wariantem danej, zmienioną pod podanymi indeksami
(//)      :: (Ix i ) => Array i e -> [(i,e)] -> Array i e
(//) arr kvs = foldr (\(k, v) a -> update k v a) arr kvs

-- Powyższe trzy funkcje nie zależą od implementacji struktury tablicy, poniższe owszem.

-- | Daje tablicę będącą wariantem danej, zmienioną pod podanym indeksem
update    :: Ix i => i -> e -> Array i e -> Array i e
update k v (Arr rng t) = Arr rng (unsafeInsert (index rng k) v t)

-- | Daje element tablicy o podanym indeksie
(!)       :: Ix i => Array i e -> i -> e
(!) (Arr rng t) k = unsafeGetElem (index rng k) t

-- | Daje listę elementów tablicy (w kolejności indeksów)
elems     :: Ix i => Array i e -> [e]
elems (Arr _ t) = toList t

-- | Tworzy pustą tablicę o zadanym zakresie indeksów
-- Zakładamy, że dla każdego niepustego przedziału (a, b) zachodzi:
-- index (a, b) a == 0 oraz index (a, b) b == (rangeSize (a, b)) - 1
empty     :: Ix i => (i, i) -> Array i e
empty rng = Arr rng $ makeEmpty (0, (rangeSize rng) - 1)

-- | Sprawdza czy w tablicy znajduje się coś pod danym indeksem
present   :: Ix i => i -> Array i e -> Bool
present k (Arr rng t) | inRange rng k = unsafeContains (index rng k) t
                      | otherwise     = False
