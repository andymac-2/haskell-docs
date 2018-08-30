# Side effects

Haskell is considered a purely functional language. This means that any normal function can not have side effects. So what are side effects?

## Basics of side effects

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

Haskell has a way around this. All we have to do is declare what side effects we want to use in the type signature. The important typeclasses to learn about are `Functor`, `Applicative` and `Monad`.


## Notes

1. Although functions may have side effects in other languages, the vast majority of software is written this way. So when I say it has the potential to cause confusion, I don't mean that it is necessarily a bad feature as such, it just means that it's one less thing we have to worry about in Haskell.
2. Functions in haskell actually *can* do one other thing apart from transform their values: throw an error. Any value can be equal to bottom, so any value can be an error.

#### Answers

* Exercises 1
    1. There are 4 functions: `const True`, `const False`, `id` and `not`.
    2. Yes. There is only one possible function with that type signature: `id`.
