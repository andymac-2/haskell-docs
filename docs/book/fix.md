# The `fix` function

According to the documentation for `base` the `fix` function finds the least fixed point of a function, whatever that means. In real terms, `fix` allows us to use recursion within a lambda function.

Consider the simple factorial function

```haskell
factorial :: Integer -> Integer
factorial n = case n of
    0 -> 1
    n -> n * factorial (n - 1)
```

This is easily made recursive by calling `factorial` by name. However, sometiumes we may wish to make a lambda function recursive. Lambda functions by definition have no name, so we cannot call them that way. However, with some trickery, maybe we can coerce haskell into passing the lanbda function itself, *as an argument to itself*. That way, we will have access to it.

Enter `fix`. `fix` enables us to pass a lambda function to itself, minus it's first argument:

```haskell
import Data.Function

factorial' :: Integer -> Integer
factorial' = fix (\this n -> case n of
    0 -> 1
    n -> n * this (n - 1))
```

When we use `fix`, we can pass the resulting function to itself as it's first argument. In our case we pass `this` to our lambda function, where `this` is our factorial function. That way we can call it recursively.

## Other examples:

Looping infinitely (terminate with either Ctrl - D or Ctrl - C):

```haskell
main :: IO ()
main = fix $ \this -> do
    x <- getLine
    putStrLn x
    this
```

## Memoization

Functions in Haskell can be memoized easily. Consider a naive implementation of the fibbonacci numbers:

```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

which can be rewritten using `fix`:

```haskell
fibFix :: Integer -> Integer
fibFix = fix (\this n -> case n of
    0 -> 0
    1 -> 1
    n -> this (n - 1) + this (n - 2))
```

Don't even try `fib 10000` with either of these implementations, it is simply too slow. Since our base cases are `0` and `1`, we end up either calculating our answer by adding `0` or `1` to our result, or we call `fib` recursively. Since we create our result by adding a maximum of `1` each step, the time it takes to calculate a fibbonacci number must be proportional to the number itself.

This function would be far quicker if we recorded the intermediate results. That means we would only need to calculate each fibbinacci number once, and then retrieve it if we need it again. We will use the `memoize` library for this.

If we have written our function recursively using `fix`, then all we have to do is change `fix` to `memoFix` and our function results are automatically memoized:

```haskell
fibMemo :: Integer -> Integer
fibMemo = memoFix (\this n -> case n of
    0 -> 0
    1 -> 1
    n -> this (n - 1) + this (n - 2))
```

And we can now get an almost instantaneous result:

```haskell
ghci> fibMemo 10000
3364476487643178326662161200510754331030214846...
```

`fix` allows us to use recursion inside lambda functions. Therefore if we replace `fix` with something else, we can customise how our functions behave when they call themselves recursively. If we had called our function by name, we would not have complete control over any recursive behaviour. In this case, we can use `memoFix` to memoise intermediate results. An explanation of how this works is beyond the scope of this article.

