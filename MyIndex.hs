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

instance Ix Int where
  range (a, b) = [a..b]
  index (a, b) c | inRange (a, b) c = c - a
                 | otherwise = error "Index out of range"
  inRange (a, b) c = a <= c && c <= b
  rangeSize (a, b) | a <= b = b - a + 1
                   | otherwise = 0
