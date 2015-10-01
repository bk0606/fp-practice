-- арифметические прогрессии в простых числах длины 3 с расстоянием k
-- например, take 2 (arith 2) = [[5,11,17], [7,13,19]]

addToList :: [Int] -> Int -> [Int]
addToList (a:as) k = a + k:addToList (as) k
addToList [] _ = []

arith :: Int -> [[Int]]
arith dist = a' [3, 9, 15] dist
    where a' start currDist = addToList start currDist:a' start (currDist+dist)

-- положение ферзей на шахматной доске nxn, при котором они не бьют друг друга
-- список номеров горизонталей, на которых находятся ферзи
-- например, Board [1,2,3,4,5,6,8,7] -- это такое расположение
--  +--------+
-- 8|      ♕ |
-- 7|       ♕|
-- 6|     ♕  |
-- 5|    ♕   |
-- 4|   ♕    |
-- 3|  ♕     |
-- 2| ♕      |
-- 1|♕       |
-- -+--------+
--  |abcdefgh|

newtype Board = Board { unBoard :: [Int] } deriving (Eq,Show)

queens :: Int -> [Board]
queens n = undefined

-- Белые начинают и дают мат в два хода
-- 
-- Белые: Фf8 g4 Крh2
-- Черные: g5 h5 a4 Крh4
-- 
-- (написать перебор всех вариантов полуходов длины три,
-- вернуть список последовательностей полуходов, ведущих
-- к решению; до этого определить необходимые типы)
-- См. https://en.wikipedia.org/wiki/Chess_notation

-- Определите так, как нужно
type Move = Int -- ???

solutions :: [[Move]]
solutions = undefined

