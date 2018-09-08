# Applicative Functors

## Explaining the Applicative typeclass

We were just introduced to the `Functor` typeclass and the `<$>` function which allows us to apply a regular function to something with a side effect. Unfortunately, `<$>` only allows us to use one side effect value as an argument. We can see this from it's type signature:

```haskell
-- only takes a function which takes one value and returns another: (a -> b)
(<$>) :: (a -> b) -> f a -> f b

-- but we want something that takes an (a -> b -> c), or (a -> b -> c -> d) or longer.
something :: (a -> b -> c) -> f a -> f b -> f c
```

If we look at `something` above we can see that it could take a function that takes two arguments, and combines the side effects of `f a` and `f b` into one side effect value `f c`. This would allow us to use a regular pure function on multiple side effects. The name of the function that does this is called `liftA2`, but we won't be using it in this chapter. Instead let's try to calculate some simple arithmetic expressions:

*Task: create a function that takes two integer arguments, `x` and `y` and returns `(100 / y) + (150 / x) + 30`. It should not crash your program when either `x` or `y` is zero, but instead use an error side effect*

We'll first create a safe division function that doesn't crash our program when we try to divide by zero,

```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

Now we may be tempted to try something like:

```haskell
myFunc :: Int -> Int -> Int

-- Type mismatch! "a `safeDiv` b" has type "Maybe Int", but we can't add two
-- "Maybe Int" together.
myFunc x y = (100 `safeDiv` y) + (150 `safeDiv` y) + 30
```

However the type system tells us that we can't add two `Maybe Int` together. We can write this function differently:

```haskell
myFunc' :: Int -> Int -> Maybe Int
myFunc' x y = go <$> (100 `safeDiv` y) <*> (150 `safeDiv` y) where
    go a b = a + b + 30
```

And there you have it. We can apply multiple argument functions to side effect values. The general principle of function application in this manner is

```haskell
-- If we can write
regularFunction argument0 argument1 ... argumentN
-- fora pure function, then we can write
regularFunction <$> sideEffect argument0 <*> sideEffect argument1 <*> ... <*> sideEffect argumentN
-- if the arguments to that function have side effects.
```

where `argument0` to `argumentN` are side effect values of the same type. The `<*>` function is part of the Applicative typeclass. Side effects that can be combined

#### Exercises 1

1. Write a program that asks the user to input two integers on separate lines, and prints the result of adding those two numbers together.
1. Using what you have learned above. Modify your program to add the following feature: if the user inputs something that isn't a number, print "Not a number" and exit. (Hint: The `readMaybe` function from the `Text.Read` module will return `Nothing` when it fails rather than terminating the program)

### Example: Lists

We mentioned previously that `[a]` represents a non-deterministic value. When we think of a list as a side effect, we think of it having all the values in the list *at the same time*. For example if we have a list `[1, 2, 3]`, then, we think of it as a single integer, that could be either 1, or, 2, or 3. If our values exist in a "quantum" state where they are every value at the same time, then every time we apply a function to that value, we have to apply it to every value it could be, for example:

```haskell
ghci> (*3) <$> [1, 2, 3]
[3,6,9]
ghci> succ <$> [1, 2, 3]
[2,3,4]
```

But what about functions that take two arcuments? How do they behave? Let's try.

```haskell
ghci> (+) <$> [1, 2, 3] <*> [10, 20, 30]
[11,21,31,12,22,32,13,23,33]
```

What it's doing is taking one value from the list on the left, and adding it to one value from the array on the right. It does this for every possible combination of values. So if you want to apply a function to combination of values, you can use a list:

```haskell
ghci> animals = ["dog", "cat", "chicken"]
ghci> adjectives = ["loud", "hungry", "brown"]
ghci> (\a b -> a ++ " " ++ b) <$> adjectives <*> animals
["loud dog","loud cat","loud chicken","hungry dog","hungry cat","hungry chicken","brown dog","brown cat","brown chicken"]
```

#### Summary

* `<$>` can only be used to apply a function to a single side effect value.
* If we would write `regularFunction a b c d`, we can also write `regularFunction <$> s a <*> s b <*> s c <*> s d` where `s` is a side effect belonging to the `Applicative` typeclass.
* `Maybe` is a side effect for errors, and `[]` is a side effect for non-determinism.
