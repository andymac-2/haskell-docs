# The Beginning

## Introduction

*Disclaimer: I made this to help me learn, and document the ways in which I would solve problems with the language. I don't guarantee the wuality of what I write. I'm always learning, so if there is a different or better way of doing things let me know!*

Learning Haskell is hard. Coming from a background of almost any other programming language, you have to forget almost everything you learn. My experiences with Haskell have made me a better programmer for that reason. Haskell should hopefully, by the end of this document, make you think in different ways about solving problems in programming. I will try my hardest to cater for different learning styles, and try to distill the essencve of what programming in Haskell is all about. I will assume a high school level of mathematics, basic command line usage, and knowledge of at least one other programming language. Examples not written in haskell will be written in JavaScript, but in depth knwledge of JavaScript will not be required.

The first thing any programmer does wiht any language is write a "Hello World" program. This ensures that your build environment is set up correctly. I reccomend [the haskell tool stack](https://github.com/commercialhaskell/stack/) for first time users. If you already have a haskell environment, you can sklip the next section

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

A series of characters surrounded by double quotes like "Hello World!" is called a *String* in Haskell (note the capital S in "String"). a `String` is a `List` (collection) of `Char` (characters). As expected, it stores some text.

## Lists and arithmetic

For the next program we'll try to print all the even numbers below 20:

```
0
2
4
6
8
10
12
14
16
18
```

The program consists of multiple function definitions, and we are introduced to some new concepts: comments, lists, integers, some new syntax, and some new functions.

```haskell
-- Print some even numbers
main :: IO ()
main = putStr evenNumbers

-- A list is denoted by square brackets. and each element of the list is
-- separated by a comma
numbers :: [Int]
numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-- "makeEven" is a function that takes an Int and returns an Int, so we write 
-- that as Int -> Int.
makeEven :: Int -> Int
makeEven x = 2 * x 

evenNumbers :: String
evenNumbers = let
    -- "fmap" allows us to do something to everything in a collection of things
    evenNumbers = fmap makeEven numbers

    -- "show" converts something to a String
    evenNumberStrings = fmap show evenNumbers

    -- unlines takes a list of Strings and joins them together, with a line 
    -- between them
    in unlines evenNumberStrings
```

Any line beginning with `--` is a comment. Comments begin from the hyphen and extend to the end of the line. They are ignored by the compiler, and are for people reading your code only. 

`main` in this program is similar to what we saw previously. It consists of a type signature, followed by a definition. The type signature (`main :: IO ()`) and most other type signatures is optional. Haskell is proficient at determining the type of a function, so this can generally be omitted. In some cases however, when Haskell cannot figure out the type of something, you must specify a type. Sometimes if you have not specified a type, Haskell will choose a type this it thinks is suitable.

Generally speaking however, for any top level declarations, (where something is defined with no indentation) writing a type signature is a good idea. Type signatures elsewhere are written at the discretion of the programmer: too many, and the code becomes cluttered, too few, and you will recieve error messages which are not informative.

in the definition of main, we are introduced to a new function: `putStr`. `putStr` is similar to `putStrLn` but will not print a newline at the end.

Next we are introduced to lists and numbers. Haskell has a few different types for numbers which we can use. The type `Int` is a data type which is guaranteed to be able to express any single number between -2^29 and 2^29-1 (-536870912 to 536870911).

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
