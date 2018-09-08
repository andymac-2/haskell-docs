## Basic operations:

`Functor` is a typeclass which is useful when you want to do something to a value after some side effect has occurred. for example, consider a `head` function that instead of giving an error on an empty list, returns `Nothing`:

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x: _) = Just x
```

We call this function `safeHead` beacuse it can't crash our program if we give it an empty list. If we examine the type signature of `safeHead`, we have a list of `a` and return a `Maybe a`. The `Maybe a` type means that this function will return something of type `a`, but it might generate an error and fail. The side effect in this case is therefore failure. This function works exactly as expected:

```haskell
*> safeHead []
Nothing
*> safeHead [23, 35, 46]
Just 23
```

Now `Nothing` means that we've had an error, and `Just x` means that we have sucessfully called `safeHead` and got a valid result. If you know that you should never have an empty list, then you can safely terminate your program if you encounter one (since you should never encounter one anyway), however, if we assume that an empty list might be valid, we must handle the error gracefully.

If we wanted to write a function that squared the first element of a list using the regular `head` function we could write something like:

```haskell
square :: Int -> Int
square x = x * x

squareFirstElem :: [Int] -> Int
squareFirstElem list = let
    x = head list
    in square x
```

And this function will fail for an empty list because `head` fails on an empty list. However, writing our program this way won't work with our new "safe" head function.

```haskell
squareFirstElem' :: [Int] -> Int
squareFirstElem' list = let
    x = safeHead list 
    -- type mismatch!! safeHead returns a Maybe Int, but sqaure x expect an Int!
    in square x
```

We can't mix a type `a` with a type `s a` where `s` is some side effect. In our case, we tried to mix `Maybe Int` with `Int`, and the compiler won't let us. So what is the solution to this problem? Imagine we give an empty list to `squareFirstElem'`, we would want it to return `Nothing` since we cannot square something that doesn't exist, and if it succeeds, we would want to return `Just something` where `something` is the first element squared. The obvious way to do this would be to take the `safeHead` result, and use a `case` expression to inspect the return result:

```haskell
squareFirstElem' :: [Int] -> Maybe Int
squareFirstElem' list = let
    x = safeHead list 
    in case x of 
        Nothing -> Nothing
        Just x -> Just (square x)
```

Which works, but has two disadvantages: It's verbose, and it's not generic. Instead of writing the whole `case` expression, we can write our function like so:

```haskell
squareFirstElem'' :: [Int] -> Maybe Int
squareFirstElem'' list = let
    x = safeHead list 
    in square <$> x
```
 which reads almost exactly like our unsafe example. This allows us to use a regular function on a side effect wrapped value. In summary, if we can write `someFunction value`, then we can also use `someFunction <$> SideEffect value`. This will not perform any additional side effects, but it will allow us to use the return value of a function that has side effects as if it were just a regular value. The resulting expression will have the same side effect as the original. In our case, our side effect was `Maybe` so our expression `square <$> x` has `Maybe` in it's type as well. `<$>` has the type signature `(a -> b) -> f a -> f b`, where `f` is a side effect belonging to the `Functor` typeclass. It takes a regular function, a side effect return value, and gives us a new side effect return value.
 
If we don't care what the return value is, but just want to perform the side effect, we can use `<$` to do so. `<$` will take a side effect value on the right, and replace the return value with the value on the left. For example,
 
 ```haskell
 -- Reads a line from stdin and ignores it.
 skipLine :: IO ()
 skipline = () <$ getLine
 ```
 
So in the above case, `getLine` returns a `String`. If we just want to skip over a line and we don't care about it's value we can use `<$` to ignore the `String` and instead return a `()`. To remember this function, just think that whatever the angled bracket doesn't point to is ignored. For `<$>`, the angled bracked points in both directions, so we care about the values on the left and the right. For `<$` the angled brackets only point to the left, so we can ignore thi right side.

Both `<$` and `<$>` can be used for any side effect belonging to the `Functor` typeclass.

#### Exercises 1

1. Write a function similar to `safeHead` which performs safe integer division, and can handle division by zero.
2. Write a safe function using `<$>` to calculate `(100 / x) + 5` which doesn't crash when `x` is zero.
3. Write a safe function using `<$` to calculate `(x / x)` which doesn't crash when `x` is zero. (Note `(x / x)` is equal to `1` everywhere, except where `x` is zero. Wehn `x` is zero, `(x / x)` is undefined.

## Further use of `Functor`

We have already discussed the function `fmap` which maps a function over every element in a collection. It turns out that `fmap` and `<$>` are the same function, just with different names. That means we can think of `fmap` as two different things: we can think of it as mapping values over a collection, or we can think of it as applying a regular function to the return value of a side effect.

TODO: expand
