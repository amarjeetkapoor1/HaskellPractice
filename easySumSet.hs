import Data.List (intersect)

-- problem statement https://www.hackerearth.com/practice/algorithms/searching/linear-search/practice-problems/algorithm/easy-sum-set-problem-7e6841ca/

main = do 
    answer <- unwords . map show <$> (createSetB <$> (converToIntList <$> getLine) <*> (converToIntList <$> getLine))
    putStrLn answer

converToIntList b = read <$> words b :: [Int]

diff :: Num a => [a] -> [a] -> [[a]]
diff aList cList = let diffList = (-) <$> cList 
    in (diffList <*> ) . (:[]) <$> aList
    
createSetB listA listC = foldl1 intersect $ diff listA listC 
