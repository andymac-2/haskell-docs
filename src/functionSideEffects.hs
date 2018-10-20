head' :: [a] -> a
head' (x: _) = x
head' [] = error "Tried to take the head of an empty list"

divide :: Int -> Int -> Int
divide _ 0 == error "Tried to divide by zero"
divide x y = x `div` y
