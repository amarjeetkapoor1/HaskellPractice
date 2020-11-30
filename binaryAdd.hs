

add :: (Ord a, Num a) => [a] -> [a] -> [a]
add x c = reverse $ addReverse (reverse x) (reverse c)

addReverse :: (Ord a, Num a) => [a] -> [a] -> [a]
addReverse x [] = x
addReverse [] x = x
addReverse [x] [c] = if x+c > 1 then [0,1] else [x+c] 
addReverse (x:xs) (c:cs) = if x+c > 1 then 0: addReverse (addReverse [1] xs) cs else (x+c) : addReverse xs cs 

