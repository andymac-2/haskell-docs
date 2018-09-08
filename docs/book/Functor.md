## Basics of side effects

Haskell has a way around this. All we have to do is declare what side effects we want to use in the type signature. The important typeclasses to learn about are `Functor`, `Applicative` and `Monad`. Within reason, we should try to minimise the use of side effects. This is similar to the principle of least privelage in software design. If a function does not need a particular side effect, then it's one less way in which we can make mistakes.

Let's introduce some data structures which represent side effects. If a function has side effects it will have a type that represents both the kind of side effect it uses, and the return value it will give after executing the said effects. The return value may be bsed on the effect, so in order to get the value, we must execute the effect first.

* `Maybe a`: The `Maybe` type represents errors. On success, a function that has a return type of `Maybe a` will return `Just something` whereas if it fails, it will return `Nothing`. (Not all functions take `Nothing` to mean an error, but that's how the side effect is treated.)
* `Either a b`: This type also represents errors, except this time, we can also carry some error information. A function that succeeds will return `Right successValue` and on failure return `Left failureValue`
* `IO a`: `IO` represents input/output. a function with a type of `IO a` will do some input/output, and return a value of type `a`.
* `[a]`: Lists can represent nondeterministic computation. When we use lists, we can take one value from each list and run our functions. We can repeat this process for every possible combination of values.
* `ST s a`: A way in which we can "cheat" Haskell's function purity. Normally haskell values are constants. Using `ST`, we can have values which can be truly modified.

Some other useful side effects that can be used in Haskell

* `Parser a`: Found in `Text.Parsec.String` in the package "Parsec". Given a `String`, a `Parser a` will parse some or all of the stream, and return a value of type `a`.
* `State s a`: Found in `Control.Monad.State` in the package "mtl". The `State` side effect allows us to use a internal state which we can modify.
* `Writer w a`: Found in `Control.Monad.Writer` in the package "mtl". The `Writer` side effect allows us to use logging.
* `Reader r a`: Found in `Control.Monad.Reader` in the package "mtl". The `Reader` side effect allows us to read data from shared variables inside functions.
* `RWS r w s a`: A coimbination of `Reader`, `Writer` and `State`.

... and many more.

## Basic operations: `Functor`

`Functor` is a typeclass which is useful when you want to do something to a value after some side effect has occurred. for example, consider a `head` function that instead of giving an error on an empty list, returns `Nothing`:

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x: _) = Just x
```

We call this function `safeHead` beacuse it can't crash our program. This function works exactly as expected:

```haskell
*> safeHead []
Nothing
*> safeHead [23, 35, 46]
Just 23
```

Now `Nothing` means that we've had an error, and `Just x` means that we have sucessfully called `head'` and got a valid result. If you know that you should never have an empty list, then you can safely terminate your program if you encounter one (since you should never encounter one anyway), however, if an empty list might be valid, we must handle the error gracefully.

If we wanted to write a function that squared the first element of a list using the regular `head` function we could write something like:

```haskell
square :: Int -> Int
square x = x * x

squareFirstElem :: [Int] -> Int
squareFirstElem list = let
    x = head list
    in square x
```

But that won't work with our new "safe" head function.

```haskell
squareFirstElem' :: [Int] -> Int
squareFirstElem' list = let
    x = safeHead list 
    -- type mismatch!! safeHead returns a Maybe Int, but sqaure x expect an Int!
    in square x
```

We can't mix a type `a` with a type `s a` where `s` is some side effect. So what is the solution to this problem? First of all remember that only functions that 
