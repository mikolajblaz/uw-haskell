module MyArray(
  Array,
  listArray, (!), elems, array, update, (//), present, empty,
  range, index, inRange, rangeSize, half
) where

import MyIndex

-- | Średnia dwóch liczb nieujemnych. Równoważnie:
-- half (a, b) = (a + b) `div` 2, ale wtedy może nastąpić przepełnienie
half :: (Int, Int) -> Int
-- half (a, b) = (a + b) `div` 2
half (a, b) = divA + divB + (modA + modB) `div` 2
  where
    (divA, modA) = divMod a 2
    (divB, modB) = divMod b 2

-- | Typ drzew przedziałowych wraz z podstawową implementacją.
-- Zakładamy, że każda funkcja (operująca na kluczach) wywoływana jest
-- z kluczami, które są wewnątrz przedziału reprezentowanego przez całe drzewo.
--
-- Drzewo składa się z węzłów wewnętrznych (Branch), które reprezentują
-- przedziały oraz z liści (Leaf), które reprezentują przedziały jednopunktowe
-- i przechowują wartości.
-- Żeby nie przeszukiwać pustych drzew, puste poddrzewa oznaczamy jako
-- EmptyBranch (dla węzłów) oraz EmptyLeaf (dla liści)
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

find :: Int -> IntervalTree e -> IntervalTree e
find k (Branch rng l r) | k <= half rng = find k l
                        | otherwise     = find k r
find _ n = n

contains  :: Int -> IntervalTree e -> Bool
contains k t = case find k t of
  Leaf _    -> True
  otherwise -> False

getElem   :: Int -> IntervalTree e -> e
getElem k t = case find k t of
  Leaf v    -> v
  otherwise -> error "No such index"

insert :: Int -> e -> IntervalTree e -> IntervalTree e
insert _ v EmptyLeaf = Leaf v
insert _ v (Leaf _)  = Leaf v
insert k v (EmptyBranch rng l r) | k <= half rng = Branch rng (insert k v l) r
                                 | otherwise     = Branch rng l (insert k v r)
insert k v (Branch rng l r)      | k <= half rng = Branch rng (insert k v l) r
                                 | otherwise     = Branch rng l (insert k v r)

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
update k v (Arr rng t) = Arr rng (insert (index rng k) v t)

-- | Daje element tablicy o podanym indeksie
(!)       :: Ix i => Array i e -> i -> e
(!) (Arr rng t) k = getElem (index rng k) t

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
present k (Arr rng t) = inRange rng k && contains (index rng k) t
