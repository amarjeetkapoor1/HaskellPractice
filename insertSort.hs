

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (init:xs) = sort [init] xs

sort :: Ord a => [a] -> [a] -> [a]
sort init [x] = insertInto init x
sort init (x:xs) = sort (insertInto init x) xs


insertInto  :: (Ord a) => [a] -> a -> [a]
insertInto [] a = [a]
insertInto (first:list) a = if first < a then first : insertInto list a  else a:first:list