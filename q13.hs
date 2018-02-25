-- haskell implementation of question 13
-- write a function that returns the union of two
-- simple list parameters

-- create a member function to check whether
-- a value is in the list
member x [] = False
member x (y:ys)
    | x == y = True
    | otherwise =  member x ys

union xs [] = xs
union xs (y:ys) 
    | member y xs = union xs ys
    | otherwise = y : union xs ys

