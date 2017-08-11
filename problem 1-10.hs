doubleMe x = x + x


doubleUs x y = x*2 + y*2

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

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
 
 flatten List [] = []
 flatten (Elem a) = [a]
 flatten List (x:xs) = [x] ++ flatten xs
 flatten (List [list]) = flatten list