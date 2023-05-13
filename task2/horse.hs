import Data.Array
import Control.Monad.Writer 
import Control.Monad.Reader 
import Data.List

near :: (Eq a, Num a) => (a, a) -> (a, a) -> Bool
near (x1,y1) (x2,y2) = abs ((x2-x1)*(y2-y1)) == 2
-- near смотрит является ли клетка достижимой из x1y1 ->x2y2

neighbours :: (Eq a, Num a) => [(a, a)] -> (a, a) -> [(a, a)]
neighbours xs x = filter (near x) xs
 
connected _ [] = True
connected [] _ = False
connected (x:xs) ws = 
    let ns = neighbours ws x 
    in connected (xs++ns) (ws\\ns)

--список соседей для точки x и списка клеток xs

ksort :: (Ord a, Num a) => [(a, a)] -> [(a, a)] -> [(a, a)]
ksort xs ns = map snd $ sort $ zip 
    (map (length . neighbours xs) ns) ns
--сортировка массива по кл-ву возможных ходов

knFromTo :: (Num a, Ord a) => (a, a) -> (a, a) -> [(a, a)] -> [[(a, a)]]
knFromTo x _ [] = [[x]] --если список пуст возвращаем x
knFromTo x s xs = [x:ks |
    connected [x] (s:xs),
    k <- ksort xs $ neighbours xs x, 
    ks <- knFromTo k s $ delete k xs]


knightC :: (Num a, Ord a, Enum a) => a -> (a, a) -> [(a, a)]
knightC n startPos = 
    head . knFromTo startPos startPos $ tail
    [(x,y) | x <- [1..n], y <- [1..n]] 

chessBoard :: (Num t, Ix a) => Array a t -> [a] -> t -> Array a t
chessBoard newBoard [] iter = newBoard
chessBoard newBoard path iter =
    let newBoard' = newBoard // [(head path,iter)]
    in chessBoard newBoard' (tail path) (iter+1)




result :: IO ()
result = do
    let size = 8
    let startPos = (1,1)
    let myArray = array ((1,1),(size,size)) [((i,j), -1) | i <- [1..size], j <- [1..size]]
    let path = knightC size startPos
    let toa = chessBoard myArray path 0 
    if length (nub path) /= size*size then putStrLn "Horse didnt walk by every cell" else 
        putStrLn $ unlines [unwords [show (toa ! (i, j)) ++ "\t" | j <- [1..size]] | i <- [1..size]]
    
