
deleteInt :: Int -> [Int] -> [Int]
deleteInt _ [] = []
deleteInt arg (head:tail) | head == arg = deleteInt arg tail
                          | otherwise = head : (deleteInt arg tail)


findIndicesInt :: Int -> [Int] -> [Int]
findIndicesInt _ [] = []
findIndicesInt arg (head:tail) = findIndices' arg (head:tail) 0
    where findIndices' _ [] _ = []
          findIndices' arg (head:tail) ind | arg == head = ind : (findIndices' arg tail (ind+1))
                                           | otherwise = findIndices' arg tail (ind+1)


tribo :: Int -> Int
tribo 0 = 1
tribo 1 = 1
tribo 2 = 1
tribo 3 = 3
tribo n = tribo (n-3) + tribo (n-2) + tribo (n-1)


normChanel :: Int -> Int
normChanel chanel | chanel > 255 = 255
                  | otherwise = chanel

data Color = Rgb { r :: Int, g :: Int, b :: Int }

white = Rgb 255	255	255
black = Rgb 0	0 	0
red   = Rgb 255 0	0
green = Rgb 0	255	0
blue  = Rgb 0	0	255

addColors :: Color -> Color -> Color
addColors (Rgb r1 g1 b1) (Rgb r2 g2 b2) = Rgb (normChanel(r1 + r2))
                                              (normChanel(g2 + g1))
                                              (normChanel(b1 + b2))

getRedChanel :: Color -> Int
getRedChanel (Rgb r _ _) = r

getBlueChanel :: Color -> Int
getBlueChanel (Rgb _ g _) = g

getGreenChanel :: Color -> Int
getGreenChanel (Rgb _ _ b) = b