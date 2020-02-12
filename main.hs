-- Basic list data type
data ML = E | L (Int, ML) deriving Show

v = L (1, L (2, L (3, L (4, E))))

last2 (L (x, E)) = x
last2 (L (x, y)) = last2 y

-- Basic binary tree data type
data BT a = EMPTY | NODE (a, BT a, BT a) deriving Show

bt = NODE (3, NODE (4, EMPTY, EMPTY), NODE (1, NODE (6, EMPTY, EMPTY), EMPTY))

-- sumbt : Get sum of binary tree node values
sumbt EMPTY = 0
sumbt (NODE (x, y, z)) = x + (sumbt y) + (sumbt z)

-- Mutual recursive data type example
data TAKA = Taka TUKA | Tak Int
data TUKA = Tuka TAKA | Tuk Bool

taka = Taka (Tuka (Taka (Tuka (Taka (Tuk True)))))
tuka = Tuka (Taka (Tuka (Tak 2)))

main = do
  print $ last2 v
  print $ sumbt bt