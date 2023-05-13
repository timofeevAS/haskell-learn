import System.Random (randomRs, randomRIO, randomR)
import System.Random (newStdGen)
import System.Random 
import Data.Monoid (Sum(..))
import Control.Monad (replicateM, sequence)

 
--task 4
newtype MyWriter w a = MyWriter { runMyWriter:: (a, w) }

instance Functor (MyWriter w) where
  fmap f (MyWriter (a, w)) = MyWriter (f a, w)


instance Monoid w => Applicative (MyWriter w) where
  pure a = MyWriter (a, mempty)
  MyWriter (f, w1) <*> MyWriter (a, w2) = MyWriter (f a, w1 `mappend` w2)

instance Monoid w => Monad (MyWriter w) where
  return = pure
  m >>= k = let (a, w1) = runMyWriter m
                (b, w2) = runMyWriter (k a)
            in MyWriter (b, w1 `mappend` w2)


logSum :: Int -> MyWriter (Sum Int) Int
logSum x = MyWriter (x, Sum x)

calculation ::Int->Int-> MyWriter (Sum Int) Int
calculation a b = do
  x <- logSum a
  y <- logSum b
  return (x + y)



data MyType = Bar | Bang | Bark | Base | Bank | Bare | Barn | Basin | Barley | Banner | Barrel | Basket | Bargain | Barrier | Barbecue | Baseball | Basement
--             0     1      2       3      4      5     6       7       8       9           10      11      12      13          14          15          16
maxlen = 16
instance Random MyType where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

instance Bounded MyType where
  minBound = Bar
  maxBound = Basement

instance Enum MyType where
    toEnum n
        | n == fromEnum Bar = Bar
        | n == fromEnum Bang = Bang
        | n == fromEnum Bark = Bark
        | n == fromEnum Base = Base
        | n == fromEnum Bank = Bank
        | n == fromEnum Bare = Bare
        | n == fromEnum Barn = Barn
        | n == fromEnum Basin = Basin
        | n == fromEnum Barley = Barley
        | n == fromEnum Banner = Banner
        | n == fromEnum Barrel = Barrel
        | n == fromEnum Basket = Basket
        | n == fromEnum Bargain = Bargain
        | n == fromEnum Barrier = Barrier
        | n == fromEnum Barbecue = Barbecue
        | n == fromEnum Baseball = Baseball
        | n == fromEnum Basement = Basement
        | otherwise = error "Invalid argument to toEnum"

    fromEnum Bar = 0
    fromEnum Bang = 1
    fromEnum Bark = 2
    fromEnum Base = 3
    fromEnum Bank = 4
    fromEnum Bare = 5
    fromEnum Barn = 6
    fromEnum Basin = 7
    fromEnum Barley = 8
    fromEnum Banner = 9
    fromEnum Barrel = 10
    fromEnum Basket = 11
    fromEnum Bargain = 12
    fromEnum Barrier = 13
    fromEnum Barbecue = 14
    fromEnum Baseball = 15
    fromEnum Basement = 16
        
instance Num MyType where
    a + b = toEnum $ (fromEnum a + fromEnum b) `mod` maxlen
    a - b = toEnum $ (fromEnum a - fromEnum b) `mod` maxlen
    a * b = toEnum $ (fromEnum a * fromEnum b) `mod` maxlen
    negate a = toEnum $ (maxlen - (fromEnum a)) `mod` maxlen
    abs a = a
    signum a = toEnum $ signum $ fromEnum a
    fromInteger n = toEnum $ (fromIntegral n) `mod` maxlen


instance Show MyType where
    show Bar = "Bar"
    show Bang = "Bang"
    show Bark = "Bark"
    show Base = "Base"
    show Bank = "Bank"
    show Bare = "Bare"
    show Barn = "Barn"
    show Basin = "Basin"
    show Barley = "Barley"
    show Banner = "Banner"
    show Barrel = "Barrel"
    show Basket = "Basket"
    show Bargain = "Bargain"
    show Barrier = "Barrier"
    show Barbecue = "Barbecue"
    show Baseball = "Baseball"
    show Basement = "Basement"

instance Eq MyType where
    (==) a b = fromEnum a == fromEnum b 

instance Ord MyType where
    (<=) a b = fromEnum a <= fromEnum b

--task2
fibonacci :: Int -> [Int]
fibonacci n = take n fibs
  where fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

myTypeList :: [[MyType]]
myTypeList = map (\n -> take n $ cycle [Bar .. Basement]) (fibonacci 10)

randomList :: Int -> IO [MyType]
randomList n = replicateM n $ randomRIO (Bar, Basement)

prepareToShow :: [MyType] -> String
prepareToShow array = show array

-- Быстрая сортировка Хоара
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

randomBool :: IO Bool
randomBool = randomIO

randomMyType :: IO MyType
randomMyType = randomIO

main :: IO ()
main = do
    -- task 1 example
    putStrLn $ show (Bar + Barbecue)
    -- task 2
    print myTypeList
    -- task 3
    print ("My random list:")
    myList <- randomList 15
    let sortedList = quickSort myList
    print (myList :: [MyType])
    print (sortedList :: [MyType])
    let (result, log) = runMyWriter (calculation 22 222)
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Log: " ++ show log
    randomValue <- randomMyType
    print randomValue


