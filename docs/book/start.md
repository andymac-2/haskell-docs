# The Beginning

## Introduction

*Disclaimer: I made this to help me learn, and document the ways in which I would solve problems with the language. I don't guarantee the wuality of what I write. I'm always learning, so if there is a different or better way of doing things let me know!*

Learning Haskell is hard. Coming from a background of almost any other programming language, you have to forget almost everything you learn. Haskell should hopefully, by the end of this document, make you think in different ways about solving problems in programming. 

My particular style of learning is much less about teaching theoretical knowledge, but more emphasis is given towards how to write useful programs, the kinds of programs that you would write in other programming languages. Previous tutorials have often referred students to look at source code, or have taught specific implementations in order to gleam useful lessons. I believe that this apporach is limited, as source code changes over time as new requirements need to be met. In addition, source code can get some complexity creep which makes it difficult for beginners to read. Specifically in functional programming, due to the lack of side effects, once a function has been implemented, it's implementation details are almost completely irrelevant. I understand that this approach may not be for everyone, but this document should describe my learning journey well.

In terms of previous knowledge, I will assume a high school level of mathematics, basic command line usage, and knowledge of at least one other programming language.

The first thing any programmer does with any language is write a "Hello World" program. This ensures that your build environment is set up correctly. I reccomend [the haskell tool stack](https://github.com/commercialhaskell/stack/) for first time users. If you already have a haskell environment, you can sklip the next section

## Setting up your environment

The best way to learn programming is to start writing programs. I'm not going to try to get bogged down with details about what the best way to set your environment is, as this can be corrected later. Ideally we can get quickly to the point in which you can start writing useful programs in Haskell, covering some of the main features of the language but may skip over some of the more techincal details. Experienced programmers may find this chapter misleading, as some of the content may not be precise, or best practice.

To get a useful environment oprational, I suggest installing the Haskell tool stack. A tutorial can be found [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/). If you already have a haskell environment installed, you can skip this section and go to "Your First Program". Once stack is installed navigate into a working directory and run the following:

```bash
$ stack new hello-world
```

Once this is complete you should see a directory structure:

```bash
.
└── hello-world
    ├── ChangeLog.md
    ├── LICENSE
    ├── README.md
    ├── Setup.hs
    ├── app
    │   └── Main.hs
    ├── hello-world.cabal
    ├── package.yaml
    ├── src
    │   └── Lib.hs
    ├── stack.yaml
    └── test
        └── Spec.hs
```

navigate into `./hello-world/app` and open `Main.hs` in you favourite text editor and type the following:

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

then run the following to build your program:

```bash
$ stack build
```

Which may take some time as it installs the Gascow Haskell Compiler (GHC) for the first time. After this step is completed, run your program using

```bash
$ stack exec hello-world-exe
Hello World!
```

If you see the words "Hello World!" On the screen, then your environment is now set up. You can use the command `stack build && stack exec hello-world-exe` in order to compile and run your program. 

## Your First Program

To begin, let's start by writing a simple "Hello World!" program [Note 1](#note-1).

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

```
Hello World!
```

Once you have seen this, you know that your build enviroment is set up correctly. To explain the program itself, it will help to break it into parts. Haskell programs are made up of *functions*. Functions take information in the form of *arguments* and return a result. Functions are defined as equal to *expressions*, and running a Haskell program is equivalent to evaluating these expressions. The first line of the program,

``` haskell
main :: IO ()
```

decalares that the function called `main` has a *type* of `IO ()`. The two colons indicate can be read as "has type of", and since everything in Haskell is a function, we don't need any special syntax to represent this.

Functions in Haskell may be similar to those found in other programming languages, but unlike functions in other languages, the only thing that a function in Haskell can do is return a value, and that value must only be determined by the information given to it by it's arguments. A function that does something else apart from returning a value based purely on it's arguments is said to have *side effects*. We will talk about side effects in more detail later.

In Haskell if a function has any side effects, they must be declared as part of the function's type. In order for any program to be useful, it must perform some input, and some output. Since these are considered side effects by Haskell, they must be declared beforehand. The type `IO ()` has two parts. `IO` stands for the input/output side effect, and `()` is the return value. `()` is the same as `void`, `null`, `undefined` or `nil` in other languages. It essentially means that the function returns no value. That means, the only thing that `main` will do, if it does not return a value, is to perform some input/output.

The second line of the program defines what `main` actually is:

``` haskell
main = putStrLn "Hello World!"
```

`main` is the entry point for a Haskell program, and evaluation (execution) will start there. This is where Haskell, and regular imperative programming languages start to really differ. Instead of telling the computer what steps to perform, we simply tell the computer *what we want the result to be*. `main` is *equal* to the expression `putStrLn "Hello World!"`. Because of this property, we say that Haskell is a *declarative* language, instead of an *imperative* one.

The function definition comes after the equals sign. `putStrLn` (**put** a **str**ing and a **l**i**n**e) will take a `String` as an argument and print it to the screen. When we give one or many arguments to a function, we are said to have *applied* those arguments. Function application in Haskell is denoted by writing the function name, and then all of the arguments separated by some whitespace:

```haskell
functionName argment1 argument2 ... argumentN
```
In other languages, function application is usually denoted by parenthesis (e.g. `myFunction()`). Since in Haskell we are going to be using function applicatin a lot, parenthesi are not used.

A series of characters surrounded by double quotes like "Hello World!" is called a *String* in Haskell (note the capital S in "String"). a `String` is a `List` (collection) of `Char` (characters). As expected, it stores some text. When expressing a string like this, it can be called a string literal.

#### Exercises

1. Try replace `putStrLn` with `putStr`. What happens?

## A simple calculator

The next exercise will be to create a simple calculator so we can appreciate the way that Haskell runs it's programs.

```haskell
main :: IO ()
main = let
    y = x * 2
    x = 10 + 5

    in putStrLn (show y)
```

The result of running this program should be `30`. Let's examine the part after `main =`. We have `let <expressions> in <expression>`. Writing `let x in y` in Haskell is called a *let expression*. And it simply means that given what is written in `x`, solve for `y`. Consider for a moment that you are in a Math class and you are asked the following question, which is a duplicate of the above code:

```haskell
Given that
y = 2x
x = 10 + 5

Solve for y
```

your working for this question may be

```haskell
y   = 2x      
    = 2 * (10 + 5)      substitute x
    = 2 * 15            simplify   
    = 30                simplify
```

And this is exactly what Haskell does behind the scenes. Note that because Haskell works the same way as a math student does, it does not require the expressions to be in order. Defining `y` before `x` is perfectly valid Haskell. In addition, the values for `x` and `y` are constants, and we cannot reassign them later. Haskell does not have variables in the traditional sense like other programming languages.

Haskell can only do two things to evaluate an expression. It can either *simplify* and expression if it has enough information, or it can *substitute* one expression into another. If you give it a problem that cannot be solved using these two operations, then it will fail. For example let's consider another question you might see in a Mathematics class.

```haskell
Given that
y = 2x
x = y - 5

Solve for y
```

Now a human can solve `x = 5` and `y = 10`, but Haskell is going to try to do the following:

```haskell
y   = 2 * x
    = 2 * (y - 5)                                   substitute x = y - 5
    = 2 * (2 * (y - 5) - 5)                         substitute y = 2 * (y - 5) using the above line
    = 2 * (2 * (2 * (2 * (y - 5) - 5) - 5) - 5)     substitution y using the above line
    ...
```

And it will continue substituting forever without reaching an answer. It is important that you only give expressions that Haskell can always evaluate using only simplification and substitution, otherwise your program will either run out of memory, or run indefinitely. In this case, because Haskell cannot evaluate `y`, we say that `y` is equal to *bottom*, or ⊥ (which is written as `_|_` in plain ASCII). If something is equal to bottom, then there has been sime kind of error when trying to evaluate it. If your program hangs, press `Ctrl + C` on the command prompt to stop it.

The only other new thing we see here is the `show` function. `show` is a function which will convert something into a `String`. In this case, we use it to convert an integer into a string so we can print it to the screen. So in total, Haskell does the following:

```haskell
main = let
    y = x * 2
    x = 10 + 5

    in putStrLn (show y)
    
main    = putStrLn (show y)
        = putStrLn (show (x * 2))
        = putStrLn (show ((10 + 5) * 2))
        = putStrLn (show (15 * 2))
        = putStrLn (show 30)
        = putStrLn "30"
```

and after "30" has been printed to the screen, we are complete.

#### Summary

1. `putStrLn` will print a string and go to the next line
1. `let x in y` means given what's written in `x`, solve for `y`
1. Haskell only uses substitution and simplification when evaluating expressions
1. Expressions which cannot be solved, or have some kind of error are said to be equal to "bottom" or ⊥
1. `show` will convert something to a `String`. In this case, an integer

#### Exercises

1. Replace `y = x * 2` with `y = x * 2.0`. Does anything change?
1. Experiment with different mathematical expressions for `x` and `y`. Try adding something for `z` as well. 
1. For some of your modifications try to evaluate the expressions by hand like we did above.

## Functions

When we define top level functions, we can call them from anywhere in the file. We can modify our program from above to define some new custom functions. Valid function names include lowercase letters, uppercase letters, digits, underscores, and apostrophes. Function names must start with a lowercase letter. Valid function names therefore include `x`, `cat`, `orangeFruit94` and `charlie's_function` but do not include `Apples`, `2real` or `bad-function-name`.

Functions can also be operators. Operators contain one or more of the following symbols:

```
!, #, $, %, &, ⋆, +, ., /, <, =, >, ?, @, \, ^, |, -, ~, :
```

In addition to this, they must not begin with a colon. Some operators we are already familiar with are `+` and `*` from the above examples. Now that we have that out of the way, let's start making some of our own functions. we will create a function which takes a temperature in Celsius and returns a temperature in Fahrenheit using the formula `f = (9/5)C + 32`, and another function which takes the mean of two numbers. We will make this one an operator and create a funny symbol for it (for example: `>.<`)

```haskell
toFahrenheit :: Int -> Int
toFahrenheit c = (9 * c) `quot` 5 + 32

(>.<) :: Int -> Int -> Int
x >.< y = (x + y) `quot` 2      -- "quot" is integer division, so x `quot` y is x divided by y.
```

We are introduced to some new syntax. First of all, out type signatures looks a bit different. `toFahrenheit :: Int -> Int` means that `toFahrenheit` is a function which takes an `Int` (the temperature in celsius) as an argument and returns an `Int` (the temperature in Fahrenheit). We also have `(>.<) :: Int -> Int -> Int`. That means that the function `>.<` takes an `Int`, then it takes another `Int`, then it returns the average of the two as an `Int`. When writing the type signature, we need to put `>.<` in parenthesis, which is why we see `(>.<)`.

When we define functions that are operators (i.e. using symbols like `>.<` or `+` or `*`), they are automatically applied using *infix notation*. Infix is where we use functions like `+` and `*` which we put in between arguments. For example, `+` is a function which takes two arguments, and returns the result of adding them together (e.g. 2 + 5), `*` is a similar function which takes two numbers as arguments but instead returns the result of the two numbers multiplied together.

If instead we use letters, numbers, underscores and apostrophes to define a function (e.g `toFahrenheit`). By default, these functions are applied using *prefix notation*. Prefix notation is where the function name is placed before the arguments (e.g: `show y`)

If we want to use a prefix function as an infix operator, we can surround the function name in backticks. If we want to use an infix operator as a prefix function, we can surround it in parenthesis:

```haskell
x >.< y     =   (>.<) x y
quot x y    =   x `quot` y
```

Once we have defined a function, Haskell will use it to substitute values. After we defined `toFahrenheit`, anywhere `toFahrenheit x` appears, we can replace it with ``(9 * x) `quot` 5 + 32``. Similarly, anwhere `x >.< y` appears, we can replace it with ``(x + y) `quot` 2``.

Without some kind of control flow, we are very limited in what we can achieve. There must be some way in which we can make branching calculations. In order to introduce this, let's introduce something called *guards*. Guards will allow us to write functions that behave differently based on their arguments. For this example, we can implement a sign function, which returns 1 for positive numbers, -1 for negative numbers, 0 for 0.

```haskell
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0
 ```
 
 Guards are given by a vertical bar followed by an expression. If the expression is true, the function will evaluate to the value on the right of the equals sign. The guards are checked from top to bottom. For example `sign -3` checks the first guard. Since `x > 0` is false, then we move down to the next guard. since `x < 0` is true, `sign -3` equals the value on the right, so we can substitute `sign -3` with `-1`. Our last guard has a special value called `otherwise` which always succeeds. This construct is similar to `if`, `else if` and `else` in other languages.
 
#### Summary

1. Function names start with a lowercase letter, and then include lowercase letters, uppercase letters, digits, underscores and apstrophes. They are called using prefix notation.
1. Operators consist of one or more of the following `!#$%&⋆+./<=>?@\^|-~:`, and must not begin with a colon. Operators are called using infix notation.
1. To call a named function in infix notation, surround it in backticks, to call an operator using prefix notation, surround it in parenthesis.
1. Guards are the equivalent of `if`, `else if`, and `else` in other programming languages. The special value `otherwise` always succeeds.

#### Exercises

1. Write a recursive function which returns the factorial of n.
1. Using what you have learned, write a recursive function for the greatest common divisor of two integers.
1. Using substitution and simplification, and some examples of your choosing, see how haskell would evaluate your function. Does the expression become larger and larger, or does it remain the same size?

## Lists and arithmetic

For the next program we'll print a Celsius to Fahrenheit converter.

```
0	32
10	50
20	68
30	86
40	104
50	122
60	140
70	158
80	176
90	194
100	212
```

The program consists of multiple function definitions, and we are introduced to some new concepts: comments, lists, new syntax, and some new functions.

```haskell
main :: IO ()
main = putStr table

-- A list is denoted by square brackets. and each element of the list is
-- separated by a comma
celsius :: [Int]
celsius = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

fahrenheit :: [Int]
fahrenheit = toFahrenheit <$> celsius

toFahrenheit :: Int -> Int
toFahrenheit c = (9 * c) `quot` 5 + 32

combineWithTab :: String -> String -> String
combineWithTab str1 str2 = str1 ++ "\t" ++ str2

table :: String
table = let
    -- <$> allows us to do something to everything in a collection of things
    celsiusStrings = show <$> celsius
    fahrenheitStrings = show <$> fahrenheit
    
    tableList = zipWith combineWithTab celsiusStrings fahrenheitStrings

    -- unlines takes a list of Strings and joins them together, with a line 
    -- between them
    in unlines tableList
```

Any line beginning with `--` is a comment. Comments begin from the hyphen and extend to the end of the line. They are ignored by the compiler, and are for people reading your code only. 

`main` in this program is similar to what we saw previously. It consists of a type signature, followed by a definition. The type signature (`main :: IO ()`) and most other type signatures is optional. Haskell is proficient at determining the type of a function, so this can generally be omitted. In some cases however, when Haskell cannot figure out the type of something, you must specify a type. Sometimes if you have not specified a type, Haskell will choose a type this it thinks is suitable.

Generally speaking however, for any top level declarations, (where something is defined with no indentation) writing a type signature is a good idea. Type signatures elsewhere are written at the discretion of the programmer: too many, and the code becomes cluttered, too few, and you will recieve error messages which are not informative.

In the definition of main, we are introduced to a new function: `putStr`. `putStr` is similar to `putStrLn` but will not print a newline at the end.

Next we are introduced to lists and numbers. Haskell has a few different types for numbers which we can use. The type `Int` is a data type which is guaranteed to be able to express any single number between -2^29 and 2^29-1 (-536870912 to 536870911). It may be able to store numbers larger or smaller than this range, but this is machine and compiler dependent. Haskell also has other numeric types which we can use:

* `Integer` an integral type which can store *any* integer. It has no bounds.
* `Float` a single precision floating point number
* `Double` a double prescision floating point number
* `(Integral a) => Ratio a` e.g. `Ratio Int` or `Ratio Integer`. A rational number. `Ratio Int` can represent any fraction that can be expressed by one `Int` divided by another. `Ratio Integer` can represent any rational number.
* `(RealFloat a) => Complex a` e.g. `Complex Float` or `Complex Double`. A complex number. `Complex Float` will be a complex number where the real and imaginary parts are single precision floating point numbers. `Complex Double` will be the same, but with double precision numbers.

For most ordinary calculations, `Int` is preferred. Although `Integer` can express larger or smaller numbers, it is also considerably slower. For those using stack or GHC, the complete list of numeric types can be found in the documentation for `Prelude` in the `base` package.

Lists are a type of collection of something. A list of `Int` is denoted as `[Int]`, so when we write `numbers :: [Int]` we say that `numbers` is a list of `Int`. When we write a list, we write it as several values seperated by commas, and surrounded by square brackets. in our example our list is `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`

`makeEven` is our first user defined function we have encountered so far. 

TODO

A more complicated example would be to print to the screen a conversion table between Celsius and Fahrenheit. I will use the formula `f = c * (9/5) + 32` to do this. Since I'm from Australia, I want to convert to Fahrenheit, so I want my table to look like this:

```
0	32
10	50
20	68
30	86
40	104
50	122
60	140
70	158
80	176
90	194
100	212
```



## Notes

#### Note 1

I'm probably going to be criticised by experianced haskellers for using "Hello World" as a starting program, as it will not adequately explain how `IO` works in Haskell. Unfortunately the only other way is to let users learn a large volume of material before learning how to write useful programs. Since this book is designed primarily to write useful programs, and not to explain mathematical theory, I don;t think it's necessary this early to explain what monads are, but primarily just how to use them.
