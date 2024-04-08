replicate' :: Int -> a -> [a]
replicate' x y = [y | _ <- [1..x]]

perfects :: Int -> [Int]
perfects x = [n | n <- [1..x], n == sum(divisors n)]

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x `div` 2], x `mod` d == 0]

find :: Eq a => a -> [(a, b)] -> [b]
find _ [] = []  -- If the list is empty, return an empty list
find x ((key, value):xs)
    | x == key  = value : find x xs  -- If the current tuple key matches x, add the corresponding value to the result
    | otherwise = find x xs           -- Otherwise, continue searching in the rest of the list


main :: IO ()
main = do 
    putStrLn "Replicate: "  
    print (replicate' 5 "test code")

    putStrLn "Perfects: "  
    print (perfects 9000)

    putStrLn "Find: "  
    print (find 'b' ['b',1], ['a',2], ['b',4])