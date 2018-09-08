## Explaining Applicative

We were just introduced to the `Functor` typeclass and the `<$>` function which allows us to apply a regular function to something with a side effect. Unfortunately, `<$>` only allows us to use one side effect value. We can see this from it's type signature:

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
myFunction = regularFunction <$> argument0 <*> argument1 <*> argument2 <*> ... <*> argumentN
```

where `argument0` to `argumentN` are side effect values of the same type.

TODO : continue

