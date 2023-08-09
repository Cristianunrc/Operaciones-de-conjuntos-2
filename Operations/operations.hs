-- Implementation of sets operations like UNION, INTERSECTION, COMPLEMENT, DIFERENCE and SIMETRIC DIFERENCE.

-- ========== Union of sets ==========
-- first list is A set and second list is B set.
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) = deleteRepElem (mergeSort ((x:xs) ++ (y:ys)))

-- mergeSort returns a ordered list of elements.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fstHalf xs)) (mergeSort (sndHalf xs))

-- merge is invocated in mergeSort function. Order two halves of a list.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- take n first elements of a list, is invocated in mergeSort function.
fstHalf :: Ord a => [a] -> [a]
fstHalf xs = take (div (length xs) 2) xs

-- drop n first elements of a list, is invocated in mergeSort function.
sndHalf :: Ord a => [a] -> [a]
sndHalf xs = drop (div (length xs) 2) xs

-- delete repeated elements of a list, this function require a ordered list.
deleteRepElem :: Ord a => [a] -> [a]
deleteRepElem [] = []
deleteRepElem [x] = [x]
deleteRepElem (x:y:ys) | x == y = deleteRepElem (y:ys)
                       | otherwise = x : deleteRepElem (y:ys)

-- ========== Intersection of sets ==========
-- first list is A set and second list is B set.
intersection :: Ord a => [a] -> [a] -> [a]
intersection xs ys = intersectionAux (mergeSort xs) (mergeSort ys)

-- intersection operation is invocated in intersection function.
-- return the intersection of two sets, which is A and B. 
intersectionAux :: Ord a => [a] -> [a] -> [a]
intersectionAux _ [] = []
intersectionAux [] _ = []
intersectionAux (x:xs) (y:ys) | x == y = x : intersectionAux xs ys
                              | x < y = intersectionAux xs (y:ys)
                              | x > y = intersectionAux (x:xs) ys 

-- ========== Diference of sets ==========
-- first list is A set and second list is B set.
diference :: Ord a => [a] -> [a] -> [a]
diference xs ys = diferenceAux (mergeSort xs) (mergeSort ys)

-- diference operation is invocated in diference function.
-- return the diference of two sets, which is A and B.
diferenceAux :: Ord a => [a] -> [a] -> [a]
diferenceAux [] _ = []
diferenceAux xs [] = xs
diferenceAux (x:xs) (y:ys) | include x (y:ys) = diferenceAux xs (y:ys)
                           | otherwise = x : diferenceAux xs (y:ys)

-- return a boolean that said if an element is included in a list.
include :: Ord a => a -> [a] -> Bool
include x [] = False
include x (y:ys) = x == y || include x ys

-- ========== Complement of sets ==========
-- first list is Universal set and second list is A set.
complement :: Ord a => [a] -> [a] -> [a]
complement us xs = complementAux (mergeSort us) (mergeSort xs)

-- complement operation is invocated in complement function.
-- return the complement of a set, considering the universal set.
complementAux :: Ord a => [a] -> [a] -> [a]
complementAux [] _ = []
complementAux us [] = us
complementAux (u:us) (x:xs) | include u (x:xs) = complementAux us (x:xs)
                            | otherwise = u : complementAux us (x:xs)
