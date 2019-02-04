# Side effects

Haskell is considered a purely functional language. This means that any normal function can not have side effects. So what are side effects?

## What are side effects?

Consider a function in Haskell which takes an `String` and returns unit.

```haskell
someFunc :: String -> ()
someFunc = -- something in here
```

In an imperative language, a function like this that takes a String and returns a useless value, may do something like print to the screen, send a message, open a file, modify some memory, or fire missiles. It may not be safe to run it twice instead of once, or it may be essential to run before something else. Functions like this may interact in complex ways and have the potential to cause a lot of confusion ([1])[#notes].

In Haskell however, functions are transformations, so it can only do one thing([2])[#notes], and that is take any `String` and return `()`.

```haskell
someFunc :: String -> ()
someFunc anything = ()
```

*There is absolutely nothing else it can do*. It can't feed the cat, open a drawer, switch on the lights, email someone, or open a file on your computer. There is only one possible way to define `someFunc`, and any other way you define it will be equivalent to the way we have defined it above. This means that the type of the function has dictated the behaviour of the function. In Haskell, looking at type signatures is an important way to tell what it actually does.

In addition consider when we have a function which takes unit and returns an `Int`:

```haskell
someOtherFunc :: () -> `Int`
someOtherFunc () = -- something in here
```

In an imperative language, this function could also do something. It may return a random number, in C and C++, this is the type signature of `main` (`int main (void);`), which can be absolutely any program whatsoever. However in haskell, the return value can only be a constant. Functions are transformations from the values on the left to the values on the right, so it will always return the same value no matter what.

```haskell
someOtherFunc :: () -> `Int`
someOtherFunc () = 5
```

`someOtherFunc x` cannot be equal to anything other than `5`, because there isn't anything that `x` could be that would make it equal to anything other than `5`. Functions in Haskell are therefore said to be *pure*, and functions that have side effects are said to be *impure*.

Now this is presents a problem when writing programs. In order for a program to do anything useful, it must be able to take some input, and produce some output. Useful programs may need to display things on your screen, read and write files, send information over the network, or many thousands of other things. Pure functions will not be able to perform any input or output, since the *only* thing they can do is transform values.

#### Exercises 1

1. Consider a function that has the type `Bool -> Bool`. How many such functions are there?
2. Consider a function that has the type `a -> a`. Is there any way to create such a function? How many different functions can you make?

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

... and many more. Because side effects are declared as part of their type signature. This means that we can tell ahead of time what functions are going to do what. A function with a return type of `State s a` will have an internal state, but will not be able to throw errors for example. A function with return type `Maybe a` will be able to fail with `Nothing` but it won't have any internal state.

Declaring a side effect in our type signature allows us to use functions with that side effect internally in our functions. For example, consider a function we call `externalFunc` which has a type of `IO ()`. We have declared that `externalFunc` can perform some input/output, so that means `externalFunc` is allowed to call other functions internally which have input/output side effects. Another example would be that `externalFunc` has a type of `Maybe a`. That means that `externalFunc` may fail, so `externalFunc` can internally use other functions which can fail as well, and we won't have to handle the errors before returning anything. Note that `externalFunc` with a type of `Maybe a` will not be able to perform input/output. It can only throw errors. This just means that what we see in the type signature is what we get, so no surprises! The type system of Haskell will check to make sure you aren't using side effects you haven't declared, and we will see some exmaples of how this happens.





# Rewrite

## Actions

So far we have only experienced pure functions. Pure functions take arguments, return a result based on those arguments and nothing else. Pure functions therefore have certain properties:

- Deterministic: They will return the same result every time they are called. This means that you only need to calculate each result once. You could store the value for later, and know that it will be the same. In an imperative language, there is no guarantee that a function will return the smae value every time.
- No side effects: Functions do not *do* anything. They take a value and return a result. but do not perform any action, like print to the screen, change your volume, or feed your cat. This means that if we don't use the result of a function, then we don't need to actually calculate it. In an imperative language, we still need to run each function even if we don't use the return value in case it does something behind the scenes.

Due to these properties, Haskell evaluates functions *lazily*, that is, it only calculates the bare minimum required to get the result. Anything else that isn't explicitly required to get the result is left behind.

However, if our program can only use pure functions, then it is not particularly useful. Real world programs need to actually *do* things such as display text, throw errors, make sounds, or send information. Here we introduce actions. Actions are a combination of two things: a *side effect* and a *return value*. Actions belong to either the `Applicative` typeclass, or the `Monad` typeclass. Below are some examples of actions:

```haskell
-- IO (input/output) is the side effect, () is the return value
ioAction :: IO ()                
ioAction = putStrLn "Hello World"

-- The side effect is Maybe, an error action. Int is the return value
-- in this case if we try to divide by zero, we return Nothing instead
maybeAction :: Int -> Int -> Maybe Int
maybeAction _ 0 = Nothing
maybeAction x y = x `div` y

-- Lists are also considered an action
-- lists represent non determinism. A list is like schroedinger's cat.
listAction :: [Int]
-- In this example we can think of listAction as an Int that is all of
-- 1, 2, 3, and 4 all at the same time.
listAction = [1, 2, 3, 4]
```

We are not as free to use actions as if we would pure functions. Actions often need to be performed in a particular sequence, and sometimes we might want to perform an action and ignore it's return value. With pure functions, if we don't use the return value, we don't need to calculate it. Actions may actually do something besides return a value, so it is important to sequence actions corretly whether or not you use the return value. To do this, we introduce `do` notation.

## `do` notation

`do` notation allows us to correctly sequence a series of actions. You can only use actions that belong to the `Monad` class with `do` notation. Below is an example using `do` notation with the `IO` action:

```haskell
getNumber :: IO Int
getNumber = do
    string <- getLine
    pure (read string)

main :: IO ()
main = do
    putStrLn "Please enter a number"
    num1 <- getNumber
    putStrLn "Please enter another number"
    num2 <- getNumber
    putStrLn (show num1 ++ " + " ++ show num2 ++ " = " ++ show (num1 + num2))
```

Output:

```
Please enter a number
5
Please enter another number
7
5 + 7 = 12
```

After writing the keyword `do`, we indent the next line. All lines below this are part of the `do` block. There are a few simple rules to follow when using `do` notation.

1. Each line in the `do` block must have the same side effect. For example, `main` has the side effect type `IO`, so all of the lines in the `do` block must have the `IO` side effect. All of the lines in `main` have the type `IO something`, so this rule is satisfied.
1. If you want to use the return value of a given line, use `<-` to bind it to a variable. In our example, we use `string <- getLine` to read a line from the console.
1. The last line of a `do` block will be the return value of the entire block. For example, the last line of `main` has the type of `IO ()`, so `main` must have a type of `IO ()`. Our function `getNumber` has a type of `IO Int` so the line `pure (read string)` must also have the type `IO Int`
1. If you want to do nothing, but return a value, use the function `pure`. In our example, `getNumber` is of type `IO Int`, but `getLine` has a type of `IO String`. We can get the string from the `IO` action using `<-` and we store it in `string`. next we want to return an `Int`. We can convert a `String` into an `Int` using `read`. We can use `pure` to return the `Int` without doing anything else. Note that in Haskell, you can also use the `return` function to do the same thing as `pure`, however, `return` is used in outher languages to do something different, so `pure` is advised to reduce confusion.



## Notes

1. Although functions may have side effects in other languages, the vast majority of software is written this way. So when I say it has the potential to cause confusion, I don't mean that it is necessarily a bad feature as such, it just means that it's one less thing we have to worry about in Haskell, that you do have to worry about in other languages.
2. Functions in haskell actually *can* do one other thing apart from transform their values: throw an error. Any value can be equal to bottom, so any value can be an error.

#### Answers

* Exercises 1
    1. There are 4 functions: `const True`, `const False`, `id` and `not`.
    2. Yes. There is only one possible function with that type signature: `id`.
