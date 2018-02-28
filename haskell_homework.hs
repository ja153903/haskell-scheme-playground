-- write a haskell function that returns the reverse of a simple list parameter
reverse_list [] = []
reverse_list (x:xs) = [y | y <- reverse_list xs] ++ [x]

-- write a haskell function that returns the union of two simple list parameters
union xs [] = xs
union xs (y:ys) 
    | member y xs = union xs ys
    | otherwise = y : union xs ys
        where member x [] = False
              member x (y:ys)
                  | x == y = True
                  | otherwise = member x ys

-- write a haskell function that takes a simple list of numbers as its parameter
-- and returns a list identical to the parameter list except with the numbers in
-- ascending order
sort_asc [] = []
sort_asc (x:xs) = insert x (sort_asc xs)
    where insert x [] = [x]
          insert x (y:ys)
              | x <= y = x: (y:ys)
              | otherwise = y : insert x ys

-- write a haskell function that takes a simple list of numbers as its parameter
-- and returns the largest and smallest numbers in the list
min_max [] = []
min_max (x:xs) = min_max_helper xs x x 
    where min_max_helper [] max_num min_num = [max_num, min_num]
          min_max_helper (x:xs) max_num min_num
              | x < min_num = min_max_helper xs max_num x
              | x > max_num = min_max_helper xs x min_num
              | otherwise = min_max_helper xs max_num min_num

-- write a haskell function that takes a simple list as its parameter and returns
-- a list of all permutations of the given list
permute [] = [[]]
permute xs = [head : permuted_tail | (head, tail) <- get_head xs, permuted_tail <- permute tail]
    where get_head [] = []
          get_head (x:xs) = (x, xs) : [(head, x:tail) | (head, tail) <- get_head xs]
