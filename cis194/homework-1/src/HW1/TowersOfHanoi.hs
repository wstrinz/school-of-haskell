module HW1.TowersOfHanoi (hanoi) where

type Peg = Char
type Move = (Peg, Peg)

step1 :: Int -> [Move]
step1 n = replicate (n - 1) ('a', 'c')

step2 :: Int -> [Move]
step2 n
  | n > 1 = [('a','b')]
  | otherwise = []

step3 :: Int -> [Move]
step3 n = replicate (n - 1) ('c', 'b')

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- recursive function; takes last move and npegs,
-- return next move + recursive call to self
-- hanoiHelper n lastMove
--   | lastMove == ('a', 'b') = nMoves $ n - 1 ('a', 'c')
--   | lastMove == ('a', 'c') = ('a', 'b')
--   | lastMove == ('a', 'b') = nMoves $ n - 1 ('b', 'c')



-- hanoi 3 'a' 'b' 'c' == [(a, c), (a, b), (c, b), (a, b), (c, a), (c, b), (a, b)]
