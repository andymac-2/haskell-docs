# `Reader`: the side effect of immutable state

The reader side effect allows us to apply the same argument to many functions at once. The `ReaderT` transformer allows us to use an embedded DSL with the commands `ask`, `local` and `reader`.

```haskell
greet :: String -> String
greet name = "Hello, " ++ name ++ ".\n"

farewell :: String -> String
farewell name = "Goodbye, " ++ name ++ ".\n"

smalltalk :: String -> String
smalltalk name = "It's a lovely day today, isn't it " ++ name ++ "?\n"

-- Traditional style
conversation :: String -> String
conversation name = greet name ++ smalltalk name ++ farewell name

-- Applicative style
conversationA :: String -> String
conversationA = go <$> greet <*> smalltalk <*> farewell 
    where go a b c = a ++ b ++ c

-- Sequential
conversationS :: String -> String
conversationS = concat . sequenceA [greet, smalltalk, farewell]

-- Monadic style
conversationM :: String -> String
conversationM = do
    a <- greet
    b <- smalltalk
    c <- farewell
    return (a ++ b ++ c)

-- Using MonadReader
conversationR :: String -> String
conversationR = runReader $ do
    a <- reader greet
    b <- reader smalltalk
    c <- reader farewell
    return (a ++ b ++ c)

```

## Point free style



## `r -> a`: the naked reader

Functions of type `r -> a` are part of the `Monad` typeclass. This is generally written as `(->) r`. When using the naked reader, the argument is passed as the fiurst argument to every line in the block. For example consider a function that returns `True` if a number is between `10` and `20` inclusive, we can write that using the naked reader syntax.

```haskell
-- original implementation
between10and20 :: Int -> Bool
between10and20 x = x >= 10 && x <= 20

-- monadic style
between10and20' :: Int -> Bool
between10and20' = do
    -- our argument is passed first to (>= 10), the result stored in above10
    above10 <- (>= 10)
    -- our argument is then passed to (<= 20), the result stored in below20
    below20 <- (<= 20)
    -- The final result is true iff above10 and below20 are both true
    return (above10 && below20)
    
-- Applicative style
between10and20'' :: Int -> Bool
-- we apply our argument to (>= 10) and (<= 20), then we apply (&&) to the result.
between10and20'' = (&&) <$> (>= 10) <*> (<= 20)
```

Thus functions of type `r -> a` can be used to simulate an immutable state, where `r` is the type of the state.

## `(->) r` is a monad

A function of type `r -> a` is a `Functor`. In order to implement `fmap :: (a -> b) -> (r -> a) -> (r -> b)` we need to be able to convert an `r -> a` to an `r -> b` using a function of type `a -> b`. This is easy, we can use `.` to compose the `a -> b` with the `r -> a` to get an `r -> b`. Therefore `fmap` will be `.`.

`r -> a` is also `Applicative`. In order to implement `pure :: a -> (r -> a)`, we must be able to turn an `a` into a function that takes an `r` and returns an `a`. The only way to do this is to ignore whatever the `r` is and just return the `a` unchanged. The function which ignores it's first argument is `const`, so naturally, `pure` will be `const`.

To implement `(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)`, we first realise that it is the same as writing `(<*>) :: (r -> a -> b) -> (r -> a) -> r -> b`. We must somehow use an `r -> a -> b`, an `r -> a`, and an `r` to produce something of type `b`. There is only one way to do this. Take the `r` and apply it to the `r -> a -> b` and the `r -> a` to get an `a -> b` and an `a` respectively. Take the resulting `a` and apply it to the `a -> b` to get something of type `b`. Therefore `(<*>) f g r = f r (g r)`

The final function to implement is `>>=`. `>>=` for the naked reader will have a type `(r -> a) -> (a -> r -> b) -> (r -> b)`, which can also be written as `(r -> a) -> (a -> r -> b) -> r -> b`. We need to be able to produce something of type `b` using and `r -> a`, an `a -> r -> b` and an `r`. There is only one way to do this. We take the `r` and apply it to the `r -> a` to get something of type `a`. Then we take the `a` and the `r` and apply it to the `a -> r -> b` to get a `b`. Therefore the implementation of `>>=` will be `(>>=) f k r = k (f r) r`. 
