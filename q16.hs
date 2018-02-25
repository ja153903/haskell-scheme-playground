-- haskell implementation of sort function

insert x [] = [x]
insert x (y:ys)
    | x <= y = x : (y:ys)
    | otherwise = y : insert x ys

sort_asc [] = []
sort_asc (x:xs) = insert x (sort_asc xs)
