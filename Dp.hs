import Data.Function

-- problem statement https://www.hackerearth.com/practice/algorithms/dynamic-programming/introduction-to-dynamic-programming-1/tutorial/

solve:: [Int] -> Int -> Int -> Int
solve x a b
    | b<a  = 0
    | otherwise = 
        let year = length x - (b-a+1)+1 ;
            left = ((x !! a) * year) + solve x (a+1) b
            right = ((x !! b) * year)  + solve x a (b-1) 
                in if left > right then left else right


solveM::  [Int] -> Int -> Int -> Int
solveM lst a b = map (map solve [0..] !! a) [0..] !! b
    where 
        solve e d 
            | d == e  = (lst !! a) * (length lst - (d-e+1)+1)
            | otherwise = 
                let year = length lst - (d-e+1)+1
                    left = ((lst !! e) * year) + solveM lst (e+1) d
                    right = ((lst !! d) * year)  + solveM lst e (d-1)
                        in if left > right then left else right


solveWithoutRec:: [Int] -> (Int -> Int -> Int) -> Int -> Int -> Int
solveWithoutRec x f a b 
    | b == a  = (x !! a) * year
    | otherwise = if left > right then left else right
    where 
        year = length x - (b-a+1)+1
        left = ((x !! a) * year) + f (a+1) b
        right = ((x !! b) * year)  + f a (b-1) 


mem:: (Int -> a) -> (Int -> a)
mem f = (map f [0 ..] !! )


memSolveWithFix x  = fix $ mem . mem . solveWithoutRec x  -- Wrong solution as it seem it uses Memoization, but doesn't use Memoization

memSolveWithFix' x  = fix (\ y -> mem . mem . solveWithoutRec x y)

main = do 
    putStrLn $ "Dp problem without Memoization" ++ show (solve [1,4,2,3] 0 3)
    putStrLn $ "Dp problem with Memoization" ++ show (solveM [1,4,2,3] 0 3)
    putStrLn $ "Dp problem with seperate Mem and recursion (Wrong solution as it seem it uses Memoization, but doesn't use Memoization)" ++ show (memSolveWithFix [1,4,2,3] 0 3)
    putStrLn $ "Dp problem with seperate Mem and recursion" ++ show (memSolveWithFix' [1,4,2,3] 0 3)


