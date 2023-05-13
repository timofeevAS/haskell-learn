import Text.Regex.Posix 
import Data.Char (isLetter, isSpace,isDigit,isAscii)
import Data.Bool (Bool)
import Data.List
import Data.Map (Map)
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Bigram = (String, String)
type Dictionary = Map.Map String [String]

isLetterOrSpace :: Char -> Bool
isLetterOrSpace c = isLetter c || isSpace c

splitText :: String -> [[String]]
splitText text =
  let sentences = getAllTextMatches (text =~ "[^.!?;:()]*[.!?;:()]+" :: AllTextMatches [] String)
  in [ [ word | word <- words sentence, word =~ "\\b[[:alpha:]]+\\b" ] | sentence <- sentences]


cleanString :: String -> String
cleanString = filter (\c -> isLetter c && not (isDigit c) && isAscii c)

cleanSentence :: [String] -> [String]
cleanSentence wordList = filter (\s-> s /= "" ) (map cleanString wordList)

cleanText :: [[String]] -> [[String]]
cleanText text = filter (\el -> el /= []) (map cleanSentence text)

getWords :: String -> [String]
getWords text = getAllTextMatches (text =~ "\\w+")

ngrams :: [[String]] -> Int -> [[[String]]]
ngrams sentences n = map (generateNgrams n) sentences
  where
    generateNgrams :: Int -> [String] -> [[String]]
    generateNgrams n words
      | length words < n = []
      | otherwise = take n words : generateNgrams n (tail words)



-- Функция для получения всех биграмм из списка предложений
getBigrams :: [[String]] -> [Bigram]
getBigrams sentences = concatMap extractBigrams sentences
  where
    extractBigrams words = zip words (tail words)

alreadyExist :: String -> [String] -> Bool
alreadyExist el listOfEl = el `elem` listOfEl

createDict :: [Bigram] -> Dictionary -> Dictionary
createDict [] dict = dict
createDict ((word1, word2) : rest) dict =
  let updatedDict = Map.insertWith (\new old -> old ++ filter (`notElem` old) new) word1 [word2] dict
  in createDict rest updatedDict


-- Функция для записи словаря в файл
writeDictionaryToFile :: FilePath -> Dictionary -> IO ()
writeDictionaryToFile filePath dict = writeFile filePath (dictionaryToString dict)

-- Функция для чтения словаря из файла
readDictionaryFromFile :: FilePath -> IO Dictionary
readDictionaryFromFile filePath = do
  contents <- readFile filePath
  return (stringToDictionary contents)

-- Преобразование словаря в строку
dictionaryToString :: Dictionary -> String
dictionaryToString dict = unlines [key ++ ": " ++ valuesToString (dict Map.! key) | key <- Map.keys dict]

-- Преобразование списка значений в строку
valuesToString :: [String] -> String
valuesToString = unwords

-- Преобразование строки в словарь
stringToDictionary :: String -> Dictionary
stringToDictionary = Map.fromList . map parseKeyValuePair . lines
  where
    parseKeyValuePair :: String -> (String, [String])
    parseKeyValuePair line =
      let (key, valuesStr) = break (== ':') line
          values = words (drop 1 valuesStr)
      in (trim key, values)

-- Удаление лишних пробелов в начале и конце строки
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')


firstRead :: Bool
firstRead = False

-- Генерация текста

-- Функция для получения случайного слова по ключу из словаря

getRandomWord :: String -> Dictionary -> IO String
getRandomWord key dict =
  case Map.lookup key dict of
    Just words -> do
      randomIndex <- randomRIO (0, length words - 1)
      return (words !! randomIndex)
    Nothing -> return " "

generateText :: Dictionary -> String -> String -> Int -> IO String
generateText _ _ curText 0 = return curText
generateText dict curKey curText size = do
  newWord <- getRandomWord curKey dict
  if newWord == " "
    then return (curText)
    else generateText dict newWord (curText ++ newWord ++ " ") (size - 1)


loop :: Dictionary -> IO ()
loop dict = do
  putStrLn "Введите слово: (ctrl+c для завершения)"
  word <- getLine
  randomSize <- randomRIO (2 :: Int, 20 :: Int)
  text <- generateText dict word (word++" ") randomSize
  putStrLn $ "Text: " ++ show text ++ "\nSize: " ++ show randomSize
  loop dict

main :: IO ()
main = do
  let fileName = "bronte_jane"
  let filePath = "C:\\Users\\User\\Desktop\\task5\\" ++ "dictionary_" ++ fileName ++ ".txt"

  putStrLn $ "Your filename is: " ++ show fileName


  if firstRead == True then do
    text <- readFile ("C:\\Users\\User\\Desktop\\task5\\" ++ fileName ++ ".txt")
    let sentences = cleanText $ splitText text
    print "WORDS"
    let bigrams = getBigrams sentences
    let dictionary = createDict bigrams Map.empty
    
    
    print $ "Make Dictionary"
    -- Запись словаря в файл
    print $ "Writing into file: " ++ show filePath ++ "..."
    writeDictionaryToFile filePath dictionary
    
    print $ "Done."
  else do
    print $ "PASS"
  -- Чтение словаря из файла
  dict <- readDictionaryFromFile filePath
  print $ "Dict read into code"
  --print $ dict
  loop dict
  

   
    

