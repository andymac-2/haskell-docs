# Functions

# What are functions?

Haskell is what we call a *functional language*. This means that we build programs using *functions*. So what is a function? A function is something that takes some data as an input, does something with that data, and produces an output:

image

The inputs to a function are calles it's *arguments*, and the output is called the *return value*. The return value of a function is always going to be the same, as long as you give it the same arguments. In addition to this, functions will not do anything "behind the scenes". They cannot fire missiles, feed your cat, or drive a car. Functions are therfore said not to have *side effects* which means the *only* thing they do is transform an input to an output. We call functions that do not have side effects *pure*. In Haskell, all functions are pure, and this differentiates it from many other languages. In plain english, here are some examples of functions:

* Given two numbers, add them together to get a new number.
* Given a list of objects, return how many objects there are.
* Given the width and height of a rectangle, return it's area.
* Given a list of objects, return the first object in the list.

Functions are therefore, quite a simple concept. We can consider them a "black box", which means, we shoudn't need to know what they do on the inside, or how they work, as long as they produce the correct answer. Haskell is also a *lazy"* language. This means that Haskell will only do the absolute minimum amount of work required in order to get you the return value of your function. We will learn a bit more about laziness later.

## Errors and exceptions

In programming, we will always encounter bugs: areas in the code which produce unexpected results, and cause our program to crash, break, or otherwise malfunction. In Haskell it is generally useful to distinguish between two kinds of bugs: errors and exceptions. For the purposes of this book, an *error* is  something wrong with the program itself, that should be fixed, and an exception is an unexpected program state. For example, an error could be when your types mismatch, when you make a typo in your program and it doesn't compile, or when you try to divide by zero. An exception could be where a user selects the wrong options on the command line, or we try to open a file that no longer exists. The fault of exceptions could lie with the user of the program, the environment in which it is run, or some external factor. They are not the fault of the programmer, however, we can minimise the consequences.

We will consider two broad categories of errors: compile time errors, and runtime errors. GHC is an extremely smart compiler, that can catch a large number of errors before the program is even run the first time. Any errors that do not stop the program from being compiled may occur when the program is being run. In these cases we have to test our program to make sure that it does the right thing. Unfortunately we cannot always test every possibility. Quite often the program will crash after all the testing has been done in front of a large audience when it is least expected. Alternatively your program may work for yourself, but when somebody else uses it in a way you didn't expect, it may break. For this reason it is generally preferable to write your program in such a way that most errors are caught at compile time, that way, you don't have to rely on testing every possibility.

Exceptions are handled in many different ways internally. We will cover these ways in later chapters.

## Functions in Haskell are not completely pure

We have talked about functions not having any side effects, but in real life, in order to make Haskell a useful language, this is not entirely true. There are some things that real world functions can do that "pure" functions cannot. Haskell functions can:

* Run forever
* Consume resources
* Sneakily perform `IO` or other side effects
* Be undefined, or partially defined
* Throw errors/exceptions

We will explore these in more detail in the following sections.

### Running Forever

There are some Haskell functions that will never return a value. They will get stuck in an infinite loop and never stop:

```haskell
silly :: a
silly = silly
```

Haskell will not complain when you try to compile this. If we try to evaluate `silly`, we end up replacing the `silly` on the left hand side of the equals sign with the `silly` on the right. We end up replacing the value with itself, so it never actually gets evaluated, and we are stuck in an infinite loop. `silly` has a type signature of `a`, which means it is a value that can be any type at all. Haskell allows this because the only way that it will ever actually produce a concrete value is if terminates. Since we know that it will run forever and never terminate, we can rest easy knowing that it is impossible for `silly` to actually be used for anything.

```haskell
*ghci> x = silly :: Int
*ghci> x + 4

```

Press `Ctrl` + `C` at the `ghci` prompt to stop the execution. Allthough this is a very contrived example, it is inevitable that you as a programmer will encounter an infinite loop by accident.

```haskell
badFactorial :: Int -> Int
badFactorial n = badFactorial (n - 1) * n
-- We forgot to provide a base case. Our function is stuck!
```

Running infinitely is almost always an error. However, in real world programs, sometimes we want to keep a program going forever, or at least until we decide to exit it. It would be annoying to say the least if your web browser, or operating system had to be restarted at regular intervals just because your programming language only allowed programs that would run for a finite period of time. Therefore, Haskell allows us to create infinite loops if we want.

### Consuming resources

Some implementations of functions are going to be a lot faster to run than others, and may consume more or less memory. When we talk about "pure" functions, the function does not take any real time to produce it's result, and it doesn't have to do any "work", it just "knows" the answer and spits it out. In the real world, this is not the case, and we have to consider that a function will require memory and time to produce an answer.

Algorithmic complexity is beyond the scope of this book, and will not be addressed, except to say that some algorithms produce a result faster and with less memory than others. Performance of programs written in Haskell will be discussed later in more detail.

### Sneakily perform `IO` or other side effects

Some values or functions in Haskell will perform `IO` and other side effects behind the scenes. Generally speaking, it is not a good idea to write a function which does this without careful consideration first. In order to do this you must use "backdoor" functions such as `unsafePerformIO`. These functions are crafted in such a way that it is essentially impossible to accidentally perform `IO` or other side effects unless that is what you explicitly want.

An example of a function which does this this is `getContents`, which allows us to read lazily from a file, it returns a `String`. When we access the elements of the `String` inside pure code, we are actually reading directly from the file.

### Be undefined, or partially defined

Consider the function `head`:

```haskell
*ghci> :t head
head :: [a] -> a
*ghci> head [1, 2, 3]
1
*ghci> head []
*** Exception: Prelude.head: empty list
```

It doesn't make sense to try to call `head` on an empty list. If you try to do so, either `ghci` will raise an exception, or if you are running a compiled program, the program will terminate with an error message. `head` is therefore considered a *partial function*, because it is not defined for every input. In our case, `head` is not defined for the empty list `[]`. Similarly `div` and `quot` are undefined when you try to divide a number by zero.

Haskell actually has a value called `undefined` that we can use when a function is completely undefined. This can be useful if we want to declare a function without writing the code for it. `undefined`, like `silly` above has type `a`, but unlike `silly` which will run forever, `undefined` will terminate with an error. We can use it when we want to check if some of our code compiles, but we haven't finished writing it yet:

```haskell
longFunction :: [Int] -> String
longFunction = undefined -- TODO: implement this function later.
```

### Throw errors

We have already discussed that functions are not completely pure. If a function runs forever, uses up too much memory, or is undefined for the input we want, these are all considered some sort of error. When a program is in one of these states we say that it's value is *bottom* or ⊥ (written as an upside-down T). Bottom's type is `a`, that is it has any type whatsoever. We allow this because if we try to read from a value which is bottom, we will encounter an error *before* we can actually get a concrete value from it. Like above, Haskell allowed us to try to add `4` to `silly`, but that is only because Haskell knew that we would encounter an error before we actually had to do the addition.

If we want to, we can throw our own errors to indicate that the program is in an invalid state, and must terminate early. For example let's say we wanted to implement our own version of `head`:

```haskell
head' :: [a] -> a
head' (x: _) = x
head' [] = error "Tried to take the head of an empty list"
```

The `error` function has type `String -> a`. It's return value is totally polymorphic, and thus can be used anywhere. If we try `head' []` we get an error. It is the responsibility of the function which calls `head'` to ensure that we never give it an empty list. In practice, we try to avoid partially defined functions like `head` whenever possible. This is because they tend to be more difficult to reason about. As a rule of thumb, partially defined functions are considered "evil", that is, only use them if the only way to avoid them is to use something even more evil.

## Total functions.

Total functions are the opposite of partially defined functions. They are defined for every input, and they *terminate* for every input, that is, they will not run forever. The Haskell compiler cannot reliably determine that a function will terminate for every input. In order to determine this, we would need to solve the Halting problem, which is famously impossible for any computer or human to solve. Therefore Haskell will not even attempt to figure out if a loop runs forever. The responsibility for this lies squarely on the programmer.

Whenever possible, use total functions. If every function that you use in your program is a total function, then your program should not encounter any runtime errors. This is because all errors will be caught at compile time.

## Design by contract

## total functions, partial functions/errors, compile time/runtime errors and evil.


## A note about validity

In the basic sense, types should ideally only be able to have valid values. Let's say we wanted to create our own type that represents the days of the week. There are obviously only seven valid values. We could use an integer to represent any weekday, wher `0` represents Monday, `1` represents Tuesday and so forth. However, there are more integers than there are weekdays, so either some numbers will be invalid weekdays, or they will be duplicates. For example `-1` could be Sunday, or it could just be a  nonsense value. Either way, it means we have more chances to write a program wrong. Any time we wanted to use an integer as a weekday, we would have to be sure that it is between `0` and `6`. Humans make mistakes, and it would be hard to guarantee that this is always going to be the case, allowing for potential bugs. The `Int` type is too "big" for weeekdays.

In the opposite direction, We could use a type which is too "small" to hold all of the valid weekdays. For example if we tried to store the weekdays in a `Bool`, we would have trouble. `True` could mean Saturday, and `False` could mean Sunday, but then the other days of the week have no real representation. Ideally we would have a representation which is "just right"; that can store *all* of the valid values of a type, cannot store any *invalid* value, and has no duplicate values.

We can use the `data` keyword to declare a datatype that can *only* hold valid weekdays, no more, no less, no duplicates:

```haskell
data Weekday 
= Monday 
| Tuesday 
| Wednesday 
| Thursday 
| Friday 
| Saturday 
| Sunday
```

Considering that our datatype can only hold valid weekdays, it would be impossible to create something of type `Weekday` that is not a day of the week. We use this property to our advantage all the time in Haskell. Functions in Haskell also have types. Consider a function that takes a `Weekday` and returns a `Bool`:

```haskell
isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False
```

We know that `isWeekend` cannot do anything apart from take a valid `Weekday` and return a valid `Bool`.  There are seven weekdays, and for each weekday, we can return either `True` or `False`, therefore, there are only 2 ^ 7 or 128 possible functions `isWeekend` can be. We have restricted an essentially infinite range of posiblilities that our program could be to only 128. In a similar fashion, there are only four valid functions which take a `Bool` and return a `Bool` for example. This makes it incredibly difficult to write the wrong program. In some extreme cases, there is only a single valid function that has a given type. In such a situation, as long as your code compiles, you know that what you have written is correct.

## Generic functions

The more generic we make our functions, the less likely it is we write them incorrectly. Consider a function with the following type signature:

```haskell
someFunction :: a -> a
```

`someFunction` is a function that can take anything, and return anything

