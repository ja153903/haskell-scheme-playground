-- haskell implementation of q11
-- where we want to write a function that returns
-- the reverse of its simple list parameter

reverse_list [] = []
reverse_list (x:xs) = reverse_list xs ++ [x]
