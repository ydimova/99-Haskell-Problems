import Data.List

-- problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
elementAt list i    = list !! (i-1)

-- Problem 4
myLenght :: [a] -> Int
myLenght [] = 0
myLenght (x: xs) = 1 + myLenght xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs ) ++ [x] 

-- Problem 6
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome xs = (head xs) == (last xs) && (palindrome $ init $ tail xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
 
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == (head xs) = compress xs
    |otherwise = [x] ++ compress xs
    
-- Problem 9 
pack l= group l
    
-- Problem 10 
encode :: (Eq a) => [a] -> [(Int,a)] 
encode [] = []
encode l = [(length(head (group l)),head(head (group l)))] ++ encode (concat.tail.group$l)
    

    
    