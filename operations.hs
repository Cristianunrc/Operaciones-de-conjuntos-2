-- Implementation of sets operations like union, intersection, complement, diference and simetric diference.

-- Union of sets
union :: Eq a => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
-- usar una funcion merge sort y eliminar repetidos.

-- mergeSort returns a ordered list of elements
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fstHalf xs)) (mergeSort (sndHalf xs))

-- merge is invocated in function mergeSort. Order two halves of one list.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys 