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

But what about functions that take two arguments? How do they behave? Let's try.

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





## Miscelaneous notes

## Basic usage

`<$>` and `<*>` are the bread and butter of `Applicative` usage. In the basic sense, if we have a function `g` which is applied to several arguments:

```haskell
arg1 :: a1
arg2 :: a2
arg3 :: a3
arg4 :: a4
...
g :: a1 -> a2 -> a3 -> a4 -> ... -> r
-- return value is of type: r
g arg1 arg2 arg3 arg4 ...
```

Then we can apply `g` to arguments wrapped in an `Applicative` side effect, as long as they are all wrapped in the same type of `Applicative`:

```haskell
f :: Applicative a => a
arg1 :: f a1
arg2 :: f a2
arg3 :: f a3
arg4 :: f a4
...
g :: a1 -> a2 -> a3 -> a4 -> ... -> r
-- return value is of type: f r
g <$> arg1 <*> arg2 <*> arg3 <*> arg4 ...
```

## (Optional) Rationale

`<$>` is an infix version of `fmap`, but otherwise identical. Any member of the `Applicative` typeclass, is a member of `Functor` by definition. Using our function `g` above, we can use `fmap` to apply `g` to anything that belongs to `Applicative`: 

``` haskell
fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b

-- Note: aything the belongs to Applicative, is also a Functor
arg1 :: Applicative f => f a1
g :: a1 -> a2 -> a3 -> a4 -> ... -> r
g <$> arg1 :: f (a2 -> a3 -> a4 -> ... -> r)
```

`g <$> a` has a type of `f (a2 -> a3 -> a4 -> ... -> r)`. To turn that into our desired return type (`f r`), we will need to use `<*>`. `<*>` has an interesting type, similar to `fmap`:

```haskell
(<$>) :: Functor     f =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b


arg1 :: (Applicative f) => f a1
arg2 :: (Applicative f) => f a2
arg3 :: (Applicative f) => f a3
arg4 :: (Applicative f) => f a4
...
g :: a1 -> a2 -> a3 -> a4 -> ... -> r
g <$> arg1                      :: f (a2 -> a3 -> a4 -> ... -> r)
g <$> arg1 <*> arg2             :: f (a3 -> a4 -> ... -> r)
g <$> arg1 <*> arg2 <*> arg3    :: f (a4 -> ... -> r)
```

Therefore we can use a regular function `g`, and apply it to arguments wrapped in an `Applicative` using `<$>` and `<*>`.

### Examples and usage

```haskell
Prelude> multiply = (*)
Prelude> multiply 4 5
20
Prelude> multiply <$> Just 4 <*> Just 5
Just 20
Prelude> multiply <$> Just 4 <*> Nothing
Nothing
Prelude> multiply <$> [4] <*> [5]
[20]
Prelude> multiply <$> [4, 3] <*> [5]
[20,15]
Prelude> multiply <$> [4, 3] <*> []
[]
```

## Applying side effects without using the return value

We can sequence `Applicative` side effects but ignore the return values using `<*` and `<$`. For example:

```haskell
-- returns Just 20
multiply <$ Just 10 <*> Just 4 <* Just 17 <*> Just 5

-- This can be written in a more easier to understand way as:
multiply 
    <$ Just 10 -- this is ignored
    <*> Just 4 
    <* Just 17 -- this is ignored
    <*> Just 5
```

If we write it out the second way, we know to ignore the return value for any line beginning weith  `<$` or `<*`. We ignore `Just 10` and `Just 17` and end up multiplying `Just 4` and `Just 5` together to get `Just 20`. This does not appear to be particularly useful, however, the side effects are not ignored. Since the return value is ignored, the return values of ignored arguments can be of any type:

```haskell
-- Side effects are not ignored, just the return value
-- returns Nothing
multiply <$ Nothing <*> Just 4 <* Just 17 <*> Just 5

-- return values are ignored, so they can be any type
-- returns Just 20
multiply <$ Just () <*> Just 4 <* Just "Hello there" <*> Just 5
```


