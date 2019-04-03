
module QuickSort where

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerElementsSorted = quicksort [a | a <- xs, a <= x]  
        biggerElementsSorted = quicksort [a | a <- xs, a > x]  
    in  smallerElementsSorted ++ [x] ++ biggerElementsSorted 