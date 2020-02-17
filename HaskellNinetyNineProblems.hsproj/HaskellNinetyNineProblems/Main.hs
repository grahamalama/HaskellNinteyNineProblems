-- 1. Find the last element of a list
myLast :: [a] -> a
myLast [] = error "Can't operate on an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2. Find the last-but-one element of a list
lastButOne :: [a] -> a
lastButOne [] = error "Can't operate on an empty list"
lastButOne [x,y] = x
lastButOne (_:xs) = lastButOne xs

-- 3. Find the K'th element of a list. 
--    The first element in the list is number 1. 
-- First attempt
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of range"
elementAt (x : xs) n
 | n == 0 = error "Index out of range"
 | n == 1 = x
 | otherwise = elementAt xs (n-1)

-- 4. Find the number of elements in a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = myReverse xs ++ [x]

-- 6. Find out whether a list is a palindrome.
palindrome :: (Integral a) => [a] -> Bool
palindrome xs = null [(x,y) | (x,y) <- zip xs (reverse xs), x/=y]

-- 7. Flatten a nested list structure. 
--    We have to define a new data type, because lists in Haskell are
--    homogeneous. 
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten(x) ++ flatten(List xs)

-- 8. Eliminate consecutive duplicates of list elements.
compress ::(Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == (head xs) = [x] ++ compress (tail xs)
  | otherwise = [x] ++ compress (xs)
  
-- 9. Pack consecutive duplicates of list elements into sublists. 
--    If a list contains repeated elements they should be placed in
--    separate sublists. 
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
  | x == head xs = (x: (head (pack xs))) : tail (pack xs)
  | otherwise = [x] : pack xs
  
-- 10. Run-length encoding of a list. Use the result of problem P09 to 
--     implement the so-called run-length encoding data compression 
--     method. Consecutive duplicates of elements are encoded as lists 
--     (N E) where N is the number of duplicates of the element E. 
encode :: (Eq a) => [a] -> [(a, Int)]
encode (xs) = [(head x, length x) | x <- pack xs]

