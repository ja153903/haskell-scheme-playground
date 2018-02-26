-- haskell implementation of q17 (min-max)

min_max_closure [] max_num min_num = [min_num, max_num]
min_max_closure (x:xs) max_num min_num 
     | x < min_num = min_max_closure xs max_num x
     | x > max_num = min_max_closure xs x min_num
     | otherwise = min_max_closure xs max_num min_num

min_max [] = []
min_max (x:xs) = min_max_closure xs x x
