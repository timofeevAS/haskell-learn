fibonacci :: Integer-> Integer
fibonacci n = f1 n 0 0 1 where f1 n k a b | n == k = a
                                          | n > 0 = f1 n (k + 1) (a + b) a
                                          | n < 0 = (-1)^(-n+1) * f1 (-n) (k+1) (a+b) a

factorial :: Integer -> Integer
factorial n | n >= 0    = helper 1 n
            | otherwise = error "arg >= 1"

helper acc 0 = acc
helper acc n = helper (acc+n) (n-1)

--Task 1
{-
summary :: Integer -> Integer -> Integer
summary begin end | begin == end = end
                  | otherwise = begin + (summary (begin+1) end)
-}


sum1 :: Integer -> Integer
sum1 0 = 0
sum1 n = n + sum1 (n-1)


mult1 :: Integer -> Integer
mult1 1 = 1
mult1 n = sum1(n) * mult1(n-1)

sum2 :: Integer -> Integer
sum2 0 = 0
sum2 n = mult1(n) + sum2(n-1)

mult2 :: Integer -> Integer
mult2 1 = 1
mult2 n = sum2(n) * mult2(n-1)

summary1 :: Integer -> Integer -> Integer -> Integer
summary1 acc begin end | begin == end = acc+end
                       | otherwise = summary1 (acc+begin) (begin+1) (end)

multiply1 :: Integer -> Integer -> Integer -> Integer
multiply1 acc begin end | begin == end = acc*(summary1 (0) (1) (begin))
                        | otherwise = multiply1 (summary1 (0) (1) (begin) ) (begin+1) (end)
--Task 2

reverseTuple :: [a] -> [a]
reverseTuple [] = []
reverseTuple arr = (reverseTuple (tail arr)) ++ [(head arr)]

--Task 3

mergeArrays :: [t] -> [[t]] -> [t]
mergeArrays acc [] = [] ++ acc
mergeArrays acc arr = mergeArrays (acc ++ (head arr)) (tail arr)

-- mergeArrays [] [[1],[2],[3]]

mergeArrays2 :: [[t]] -> [t]
mergeArrays2 (x:[]) = x
mergeArrays2 arr = (head arr) ++ mergeArrays2(tail arr)

--Task 4
mergeTwo :: [a] -> [a] -> [a]
mergeTwo [] ys = ys
mergeTwo (x:xs) ys = x:mergeTwo ys xs

splitterArrays :: [String] -> [String] -> [String]
splitterArrays arr1 arr2 = mergeTwo (arr1) ("-.-" : arr2)


--Task 5
{-
  | 0   1   2   3   4   5   6
--+--------------------------
0 | 1
1 | 1   1
2 | 1   2   1
3 | 1   3   3   1
4 | 1   4   6   4   1
5 | 1   5  10  10   5   1
6 | 1   6  15  20  15   6   1
-}

pascalTriangle :: Integer -> Integer -> Integer 
pascalTriangle row 0 = 1
pascalTriangle 0 column = column
pascalTriangle row column = div (row * pascalTriangle (row-1) (column-1)) (column)
-- | i < j      = error "Invalid coordinates"
--Task 6

{-
--Task 6 Fractal Piphagor's Tree
drawTree :: [Double] -> Double -> Double -> Double -> Double -> [Double]
drawTree acc startX startY length angle | length <= 2    = acc ++ [startX,startY]
                                        | otherwise = acc ++ ((drawTree (acc) (startX + length * cos angle) (startY - length * sin angle) (length*0.7) (angle + pi/4)) ++ (drawTree (acc) (startX + length * cos angle) (startY - length * sin angle) (length*0.7) (angle - pi/6)))
-}

drawTree :: Double -> Double -> Double -> Double -> [Double]
drawTree startX startY length angle | length <= 20   = [] ++ ([startX,startY])
                                    | otherwise       = (drawTree (startX + length * cos angle) (startY - length * sin angle) (length * 0.7) (angle + pi/4) ) ++ (drawTree (startX + length * cos angle) (startY - length * sin angle) (length * 0.7) (angle - pi/6)  )



--Length function
lengthOfList :: [a] -> Integer
lengthOfList [] = 0
lengthOfList (x:xs) = 1 + lengthOfList xs