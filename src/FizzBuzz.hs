module FizzBuzz where


fizzBuzz35 :: [String]
fizzBuzz35 = fmap replaceNumbers [1..100]
 where
  replaceNumbers currentNumber
    | (rem currentNumber 15 == 0) = "FizzBuzz"
    | (rem currentNumber 3 == 0)  = "Fizz"
    | (rem currentNumber 5 == 0)  = "Buzz"
    | otherwise                   = show currentNumber   


fizzBuzzGeneric :: Int -> Int ->  [String]
fizzBuzzGeneric fizz buzz = fmap replaceNumbers [1..100]
 where
  replaceNumbers currentNumber 
    | (rem currentNumber (fizz * buzz) == 0) = "FizzBuzz"
    | (rem currentNumber fizz == 0)          = "Fizz"
    | (rem currentNumber buzz == 0)          = "Buzz"
    | otherwise                              = show currentNumber   

