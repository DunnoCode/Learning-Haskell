-- Question 1
solve :: Int -> Int -> [Int] 
solve n x  
    | return0 = []  
    | return1 = take x (repeat (n `div` x))
    | otherwise = (n `div` x):solve (n - (n `div` x)) (x-1)
    where
        return0 = n == 0
        return1 = ((n `mod` x) == 0)

-- Question 2
ismember :: (Eq a) => a -> [a] -> Bool
ismember a [] = False
ismember element list
    | isTrue = True
    | isFalse = ismember element (tail list)
    where
        isTrue = element == head(list)
        isFalse = element /= head(list)

-- Question 3
checkHappyDigits :: [Int] -> Int -> Int
checkHappyDigits list k
    | return0 = 0
    | isTrue  = 1 + checkHappyDigits (tail list) k
    | otherwise = 0 + checkHappyDigits (tail list) k
    where
        return0 = null list
        isTrue = length(show [ x | x <- show(head list), x == '4' || x == '7']) <= k + 2

-- Question 4
-- Long excution time when nodes > 20
-- numBST :: Int -> Int
-- numBST 0 = 1
-- numBST nodes = count (nodes - 1) 0 
--     where count :: Int -> Int -> Int
--           count (-1) _ = 0
--           count left right = ((numBST left * numBST(right)) + count (left - 1) (right + 1)) `mod` (10^9 + 7)

-- update
-- https://www.youtube.com/watch?v=YDf982Lb84o
-- catalin program
numBST :: Int -> Integer
numBST x = (xs !! x) `mod` (10 ^ 9 + 7)
    where xs = map fst (iterate f (1,0)) where f (x,y) = ((4 * y + 2) * x `div` (y + 2), y + 1)

-- Question 5
pointIn :: [Int] -> Int -> [Int]
pointIn list number
    | return0 = [-1]
    | otherwise = take 2 (repeat (quickSort(list) !! (number - 1)))
    where  
        return0 = length list < number
        quickSort :: (Ord a) => [a] -> [a]
        quickSort [] = []
        --making a reverse list back putting larger at front and smaller at back
        quickSort (x:xs)  = quickSort larger ++ [x] ++ quickSort smaller
            where smaller = filter (<=x) xs
                  larger  = filter (> x) xs

-- Question 6
isTwinPaired :: [Int] -> Bool
isTwinPaired list = check [x | x <- list, x `mod` 2 == 0] [x | x <- list, x `mod` 2 /= 0]
    where 
          check :: [Int] -> [Int] -> Bool
          check even odd 
            | isSorted even && isSorted (reverse odd) = True
            | otherwise = False
            where
                isSorted :: [Int] -> Bool
                -- if the sorted list is same as the orginal list return True
                isSorted list 
                    | ((quickSort list) == list) = True
                    |  otherwise = False
                    where
                        quickSort :: (Ord a) => [a] -> [a]
                        quickSort [] = []
                        quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
                            where smaller = filter (<=x) xs
                                  larger  = filter (> x) xs

-- Question 7
--Set up all the condition to check there any conflict
safe:: [(Int, Int)] -> (Int, Int) -> Bool
safe [] (x, y) = True
safe ((i,j):xs) (x,y)
    |   vertical = False
    |   horizontal = False
    |   diagonal = False
    |   lShape1 = False
    |   lShape2 = False
    |   otherwise = safe xs (x,y) 
    where
        vertical = (x == i)
        horizontal = (y == j)
        diagonal = (abs(x - i) == abs(y - j))
        lShape1 = (abs(x - i) == 2 && abs(y - j) == 1)
        lShape2 = (abs(x - i) == 1 && abs(y - j) == 2)
-- N Queens Problem using Backtracking
-- https://www.youtube.com/watch?v=xFv_Hl4B83A
chess:: Int -> Int
chess knights = chess_ (knights - 1) 0 0 []
    where
        chess_:: Int -> Int -> Int -> [(Int, Int)] -> Int
        chess_ size x y xs
            | y > size = 1
            | x > size = 0
            | (safe xs (x,y)) = (chess_ size (x + 1) y xs) + (chess_ size 0 (y + 1) ((x,y):xs))
            | otherwise = chess_ size (x + 1) y xs
