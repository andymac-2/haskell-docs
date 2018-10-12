module FizzBuzz where


fizzBuzz35 :: [String]
fizzBuzz35 = do
  fmap replaceNumbers [1..100]
 where
  replaceNumbers currentNumber = 
    if (rem currentNumber 15 == 0)
    then "FizzBuzz"
    else if (rem currentNumber 3 == 0)
         then "Fizz"
         else if (rem currentNumber 5 == 0)
              then "Buzz"
              else show currentNumber   


fizzBuzzGeneric :: Int -> Int ->  [String]
fizzBuzzGeneric fizz buzz = do
  fmap replaceNumbers [1..100]
 where
  replaceNumbers currentNumber = 
    if (rem currentNumber (fizz * buzz) == 0)
    then "FizzBuzz"
    else if (rem currentNumber fizz == 0)
         then "Fizz"
         else if (rem currentNumber buzz == 0)
              then "Buzz"
              else show currentNumber   

