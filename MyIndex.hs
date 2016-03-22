module MyIndex where

class Ord a => Ix a where
  -- range (lo,hi) daje listę wszystkich indeksów
  -- pomiędzy lo a hi
  range :: (a, a) -> [a]
  -- index (lo,hi) i daje numer kolejny indeksu i w zakresie
  -- (od 0)
  -- np index (7,9) 8 = 1; index ('a','d') 'd' = 3
  -- komunikat o błędzie jesli indeks poza zakresem.
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int


-- Instancje dla Char, Int, Integer oraz Ix(a, b)

instance Ix Char where
  range (a, b) = [a..b]
  index (a, b) c | inRange (a, b) c = fromEnum c - fromEnum a
                 | otherwise = error ("Index " ++ show c ++ " out of range " ++ show (a, b))
  inRange (a, b) c = a <= c && c <= b
  rangeSize (a, b) | a <= b = fromEnum b - fromEnum a + 1
                   | otherwise = 0


instance Ix Int where
  range (a, b) = [a..b]
  index (a, b) c | inRange (a, b) c = c - a
                 | otherwise = error ("Index " ++ show c ++ " out of range " ++ show (a, b))
  inRange (a, b) c = a <= c && c <= b
  rangeSize (a, b) | a <= b = b - a + 1
                   | otherwise = 0

instance Ix Integer where
  range (a, b) = [a..b]
  index (a, b) c | inRange (a, b) c = fromInteger (c - a)
                 | otherwise = error "[Integer] Index out of range"
  inRange (a, b) c = a <= c && c <= b
  rangeSize (a, b) | a <= b = fromInteger (b - a + 1)
                   | otherwise = 0

instance (Ix a, Ix b) => Ix (a,b) where
  range ((a1, b1), (a2, b2)) = [(x, y) | x <- range (a1, a2), y <- range (b1, b2)]
  index ((a1, b1), (a2, b2)) (c, d)
    | inRange ((a1, b1), (a2, b2)) (c, d) = index (a1, a2) c * rangeSize (b1, b2) + index (b1, b2) d
    | otherwise = error "[A, B] Index out of range"
  inRange ((a1, b1), (a2, b2)) (c, d) = inRange (a1, a2) c && inRange (b1, b2) d
  rangeSize ((a1, b1), (a2, b2)) = rangeSize (a1, a2) * rangeSize (b1, b2)
