import Data.List

encode :: (Eq a) => [a] -> [(Int,a)] 
encode [] = []
encode l = [(length(head (group l)),head(head (group l)))] ++ encode            (concat.tail.group$l)

--Problem 11
--encodeModified :: (Eq a) => [a] -> [[a]] 
--encodeModified [] = []
--encodeModified l 
--    | (length .head $group l) == 1  = ["Single" ++ head(group l)]  --++ encodeModified (tail.group$l)
--    | otherwise = ["Multiple" ++ length.head(group l) ++ head(group --l)] ++ encodeModified (tail.group$l)
    
--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x]++[x]++(dupli xs)

--Propblem 15
--relpi :: [a] -> Int -> [a]
--repli [] _ = []
--repli (x:xs) n = (take n (cycle [x]))++ (repli xs n)

--Problem 16 : drop every n-th element
dropEvery xs n = map fst $ filter ((/=n). snd) $ zip xs (cycle [1..n])

--Problem 17 : split
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split l@(x:xs) n
    | n > 0 = (x:zf,zt)
    | otherwise = ([],l)
    where (zf,zt) = split xs (n-1)