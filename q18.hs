-- haskell function that takes a simple list as its parameter
-- and returns a list of all permutations of the given list

perm [] = [[]]
perm xs = [y:zs | (y, ys) <- select xs, zs <- perm ys]
    where select [] = []
          select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]
