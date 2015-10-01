
-- Функция декодирования двоичной записи числа
-- (аналогично декодированию азбуки Морзе, но
-- учтите бесконечность дерева -- нужна ленивость)
decodeBinary :: String -> Int
decodeBinary str = decode' 0 str
   where decode' currNum ('1':lastStr) = decode' (currNum * 2 + 1) lastStr
         decode' currNum ('0':lastStr) = decode' (currNum * 2) lastStr
         decode' currNum [] = currNum


-- Функция декодирования записи числа в системе
-- Фибоначчи: разряды -- числа Фибоначчи, нет
-- двух единиц подряд:
--    0f = 0
--    1f = 1
--   10f = 2
--  100f = 3
--  101f = 4
-- 1000f = 5
-- 1001f = 6
-- 1010f = 7
--   .....
-- (аналогично декодированию азбуки Морзе, но
-- учтите бесконечность дерева -- нужна ленивость)
decodeFibo :: String -> Int
decodeFibo str = decode' 0 1 (reverse str)
   where decode' _ _ ('1':'1':_) = error "Invalid input: neighbour units"
         decode' prevNum currNum ('1':lastStr) = prevNum + currNum + decode' currNum (prevNum + currNum) lastStr
         decode' prevNum currNum ('0':lastStr) = decode' currNum (prevNum + currNum) lastStr
         decode' _ _ [] = 0
