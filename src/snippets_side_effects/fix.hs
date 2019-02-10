import Data.Function
import Data.Function.Memoize

factorial' :: Integer -> Integer
factorial' = fix (\this n -> case n of
    0 -> 1
    n -> n * this (n - 1))

factorial :: Integer -> Integer
factorial n = case n of
    0 -> 1
    n -> n * factorial (n - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibFix :: Integer -> Integer
fibFix = fix (\this n -> case n of
    0 -> 0
    1 -> 1
    n -> this (n - 1) + this (n - 2))

fibMemo :: Integer -> Integer
fibMemo = memoFix (\this n -> case n of
    0 -> 0
    1 -> 1
    n -> this (n - 1) + this (n - 2))

main :: IO ()
main = fix $ \this -> do
    x <- getLine
    putStrLn x
    this
