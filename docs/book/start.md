# The Beginning

The only way to learn programming is to start writing programs. In this section, I will give a very brief overview of some of the main language features in an attempt to get the reader to a point in which they can begin writing useful programs for themselves. An advanced reader may notice that some of the information here is not precise, but it should bring the user to a point in which they can start writing programs of their own as quickly as possible.

## Your First Program

To begin, let's start by writing a simple "Hello World!" program<sup>[1](#notes)</sup>.

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

And run using `stack build && stack exec <program name>-exe`.

```
Hello World!
```

Once you have seen this, you know that your build enviroment is set up correctly. To explain the program itself, it will help to break it into parts. Haskell programs are made up of *functions*. Functions take information in the form of *arguments* and return a result. Functions are defined as equal to *expressions*, and running a Haskell program is equivalent to evaluating these expressions. The first line of the program,

``` haskell
main :: IO ()
```

decalares that the function called `main` has a *type* of `IO ()`. The two colons indicate can be read as "has type of", and since everything in Haskell is a function, we don't need any special syntax to represent this.

Functions in Haskell may be similar to those found in other programming languages, but unlike functions in other languages, the only thing that a function in Haskell can do is return a value, and that value must only be determined by the information given to it by it's arguments. A function that does something else apart from returning a value based purely on it's arguments is said to have *side effects*. We will talk about side effects in more detail later.

In Haskell if a function has any side effects, they must be declared as part of the function's type. In order for any program to be useful, it must perform some input, and some output. Since these are considered side effects by Haskell, they must be declared beforehand. The type `IO ()` has two parts. `IO` stands for the input/output side effect, and `()` is the return value. `()` (pronounced "unit") is the same as `void`, `null`, `undefined` or `nil` in other languages. It essentially means that the function returns no value. That means, the only thing that `main` will do, if it does not return a value, is to perform some input/output.

The second line of the program defines what `main` actually is:

``` haskell
main = putStrLn "Hello World!"
```

`main` is the entry point for a Haskell program, and evaluation (execution) will start there. This is where Haskell, and regular imperative programming languages start to really differ. Instead of telling the computer what steps to perform, we simply tell the computer *what we want the result to be*. `main` is *equal* to the expression `putStrLn "Hello World!"`. Because of this property, we say that Haskell is a *declarative* language, instead of an *imperative* one.

The function definition comes after the equals sign. `putStrLn` (**put** a **str**ing and a **l**i**n**e) will take a `String` as an argument and print it to the screen. When we give one or many arguments to a function, we are said to have *applied* those arguments. Function application in Haskell is denoted by writing the function name, and then all of the arguments separated by some whitespace:

```haskell
functionName argment1 argument2 ... argumentN
```
Using functions this way is called *prefix notation*. In other languages, function application is usually denoted by parenthesis (e.g. `myFunction()`). Since in Haskell we are going to be using function application a lot, parenthesis are not used.

A series of characters surrounded by double quotes like "Hello World!" is called a *String* in Haskell (note the capital S in "String"). a `String` is a `List` (collection) of `Char` (characters). As expected, it stores some text. When expressing a string like this, it is called called a string literal.

#### Exercises

1. Try replace `putStrLn` with `putStr`. What happens?
2. Replace "Hello World!" with a message of your choosing.

## A simple calculator

The next exercise will be to create a simple calculator so we can appreciate the way that Haskell runs it's programs.

```haskell
main :: IO ()
main = let
    y = x * 2
    x = 10 + 5
    in putStrLn ("x equals " ++ show x ++ ", y equals " ++ show y ++ ".")
```

Wa use `+` to add two numbers together, and `*` to multiply them together. In addition, for all every type of number, we can subtract them using `-`.

The result of running this program should be `x equals 15, y equals 30.` Let's examine the part after `main =`. We have `let <expressions> in <expression>`. Writing `let x in y` in Haskell is called a *let expression*. We can write a `let` expression anywhere we would can write a normal expression. It simply means that given what is written in `x`, solve for `y`. Consider for a moment that you are in a mathematics class and you are asked the following question, which is a duplicate of the above code:

```haskell
Given that
y = 2x
x = 10 + 5

Solve for x and y
```

your working for this question may be

```haskell
x   = 10 + 5
    = 15                simplify
    
y   = 2x      
    = 2 * 15            substitute x 
    = 30                simplify
```

And this is exactly what Haskell does behind the scenes. Note that because Haskell works the same way as a math student does, it does not require the expressions to be in order. Defining `y` above `x` is perfectly valid Haskell. In addition, the values for `x` and `y` are constants, and we cannot reassign or change them later. Haskell does not have variables in the traditional sense like other programming languages.

Haskell can only do two things to evaluate an expression. It can either *simplify* an expression if it has enough information, or it can *substitute* one expression into another. Note that in Haskell, we only substitute from left to right: that is, in te above example, we can replace `y` with `x * 2`, but we cannot replace `x * 2` with `y`. If you give it a problem that cannot be solved using these two operations, then it will fail. For example let's consider another question you might see in a mathematics class.

```haskell
Given that
y = 2x
x = y - 5

Solve for y
```

Now a human can solve `x = 5` and `y = 10` through various methods, but Haskell is going to get stuck doing the following:

```haskell
y   = 2 * x
    = 2 * (y - 5)                                   substitute x = y - 5
    = 2 * (2 * (y - 5) - 5)                         substitute y = 2 * (y - 5) using the above line
    = 2 * (2 * (2 * (2 * (y - 5) - 5) - 5) - 5)     substitute y using the above line
    ...
```

And it will continue substituting forever without reaching an answer. It is important that you only give expressions that Haskell can always evaluate using only simplification and substitution, otherwise your program will either run out of memory, or run indefinitely. In this case, because Haskell cannot evaluate `y`, we say that `y` is equal to *bottom*, or ⊥ (which is written as `_|_` in plain ASCII). If something is equal to bottom, then there has been some kind of error when trying to evaluate it. If your program hangs, press `Ctrl + C` on the command prompt to stop it.

We also experince the `show` function. `show` is a function which will convert something into a `String`. In Haskell, the text of a number is distinct from the number itself. Consider that a phone number such as `0470 101 234` is not actually a number in a sense, becuase it is distinctly different from `470,101,234`. It also doesn't make sense to add or subtract two phone numbers either. In this case, we use the `show` function to convert an integer into a `String` so we can print it to the screen. 

The last part of the program that hasn;t been explained yet is the `++` operator. `++` will concatenate two `List`s together. As mentioned above, `String` is a `List` of `Char`, so we can use `++` to join two `String`s together. For example:

```haskell
*ghci> "Hello " ++ "World!"
"Hello World!"
*ghci> "Pi is equal to " ++ show pi ++ "."
"Pi is equal to 3.141592653589793."
```

So for our calculator program, Haskell does the following:

```haskell
main = let
    y = x * 2
    x = 10 + 5

    in putStrLn ("x equals " ++ show x ++ ", y equals " ++ show y ++ ".")
    
main    = putStrLn ("x equals " ++ show (10 + 5) ++ ", y equals " ++ show y ++ ".")
        = putStrLn ("x equals " ++ show 15 ++ ", y equals " ++ show (x * 2) ++ ".")
        = putStrLn ("x equals " ++ show 15 ++ ", y equals " ++ show (15 * 2) ++ ".")
        = putStrLn ("x equals " ++ show 15 ++ ", y equals " ++ show 30 ++ ".")
        = putStrLn ("x equals " ++ "15" ++ ", y equals " ++ "30" ++ ".")
        = putStrLn "x equals 15, y equals 30."
```

and after `equals 15, y equals 30.` has been printed to the screen, we are complete.

#### Summary

1. `putStrLn` will print a string and go to the next line.
1. `let x in y` means given what's written in `x`, solve for `y`.
1. Haskell only uses substitution and simplification when evaluating expressions.
1. Expressions which cannot be solved using only simplification and substitution, or have some kind of error are said to be equal to "bottom" or ⊥.
1. `show` will convert something to a `String`. In this case, an integer.
1. Use `++` to concatenate two `String`s together.

#### Exercises

1. Replace `y = x * 2` with `y = x * 2.0`. Does anything change?
1. Experiment with different mathematical expressions for `x` and `y`. Try adding something for `z` as well.
1. For some of your modifications try to evaluate the expressions by hand like we did above.

## A more complicated example:

For the next program we'll print a Celsius to Fahrenheit converter. I will use the formula `f = c * (9/5) + 32` to do this. Since I'm from Australia, I want to convert to Fahrenheit, so I want my table to look like this:

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

Our program looks a bit more complicated, and we are introduced to some new syntax:

```haskell
startTemp = 0
stopTemp = 100
tempStep = 10

main :: IO ()
main = putStr (temperatureTable startTemp)

-- Create a temperature conversion table starting from startTemp, finishing at stopTemp
temperatureTable :: Int -> String
temperatureTable c
    | c <= stopTemp = let

        -- quot is integer division, so quot x y is x divided by y
        f = quot (9 * c) 5 + 32
        restOfTable = temperatureTable (c + tempStep)
        
        in show c ++ "\t" ++ show f ++ "\n" ++ restOfTable

    | otherwise =  ""
```

So let's break this down into parts. Firstly at the top of our file we have

```haskell
startTemp = 0
stopTemp = 100
tempStep = 10
```

This is similar to what we saw when we wrote `y = x * 2` before. We define some numbers which are constants throughout our program. It is good practice to assign constants to a name, for a number of reasons. Firstly, if you want to change the number, you only have to change it one place instead of everywhere the number appears. Secondly, you have given the number a description, so it gives you some idea of what the number is.

The second new part of syntax is the type signature for `temperatureTable`:

```haskell
temperatureTable :: Int -> String
```

This line tells us that `temperatureTable` is a function that takes an integer (Which we call `Int`) and returns a `String`. This is a quite similar type to `show` which we have been introduced to before.

```haskell
temperatureTable c
    | c <= stopTemp = let ... in ...
    | otherwise =  ""
```

The syntax above is called a *guard*. What we say here is that if `c` is less than or equal to `stopTemp` (where we write `<=` for "less than or equal to") then `temperatureTable c` can be substituted for the `let ... in ...` expression. Otherwise, (if `c` is greater than `stopTemp`,) `temperatureTable c` is equal to the empty string `""`. We can also have guards with more than two conditions.

```haskell
-- quot is integer division, so quot x y is x divided by y
``` 

The line above is a comment. Comments begin with a double hyphen (`--`) and extend to the end of a line. Comments are ignored by the haskell compiler, so you can use this to tell other people in plain english (or whatever language you prefer) why the code is written the way it is. Everything apart from that we have seen before.

```haskell
f = quot (9 * c) 5 + 32
restOfTable = temperatureTable (c + tempStep)

in show c ++ "\t" ++ show f ++ "\n" ++ restOfTable
```

The first line above just tells us that the variable `f`, meaning fahrenheit in this example, is equal to `quot (9 * c) 5 + 32`, which is just some math to convert celsius to fahrenheit. The result is then the temperature in celsius, plus a tab (`\t`) plus the temperature in fahrenheit, plus a new line (`\n`) plus whatever the rest of the table is. In Haskell, we simply tell the compiler what the result is, an not how to get there. Intuitively, a conversion table starting at `0` to `100` in steps of `10` is actually just a single line for the temperature at `0`, added to a table starting at `10` and going to `100` in increments of `10`.

![Diagram illustrating the parts of the temperature table](images/beginning/tempTable2.svg)

## Functions

When we define top level functions, we can call them from anywhere in the file. We can modify our program from above to define some new custom functions. Valid function names include lowercase letters, uppercase letters, digits, underscores, and apostrophes. Function names must start with a lowercase letter. Valid function names therefore include `x`, `cat`, `orangeFruit94` and `charlie's_function` but do not include `Apples`, `2real` or `bad-function-name`.

Functions can also be operators. Operators contain one or more of the following symbols:

```
! # $ % & ⋆ + . / < = > ? @ \ ^ | - ~ :
```

In addition to this, they must not begin with a colon. Some operators we are already familiar with are `+` and `*` from the above examples. Now that we have that out of the way, let's start making some of our own functions. We will create a function which takes a temperature in Celsius and returns a temperature in Fahrenheit using the formula `f = (9/5)C + 32`, and another function which takes the mean of two numbers. We will make this one an operator and create a funny symbol for it (for example: `>.<`). Below is how to write this in Haskell

```haskell
toFahrenheit :: Int -> Int
toFahrenheit c = (9 * c) `quot` 5 + 32

(>.<) :: Int -> Int -> Int
x >.< y = (x + y) `quot` 2      -- "quot" is integer division, so x `quot` y is x divided by y.
```

We are introduced to some new syntax. First of all, our type signatures looks a bit different. `toFahrenheit :: Int -> Int` means that `toFahrenheit` is a function which takes an `Int` (the temperature in celsius) as an argument and returns an `Int` (the temperature in Fahrenheit). An `Int` is the basic integer type in Haskell. We also have `(>.<) :: Int -> Int -> Int`. That means that the function `>.<` takes an `Int`, then it takes another `Int`, then it returns the average of the two as an `Int`. You'll notice that we don't use any special notation for the type of the return value. The return value type is just the last `Int`, and the arguments are the first two `Int`s, which are separated with `->`. We don't write something like `Int Int -> Int` to differentiate the return value from it's arguments. This will be important later. When writing the type signature, we need to put operators in parenthesis, which is why we see `(>.<)` instead of `>.<`.

When we define functions that are operators (i.e. using symbols like `>.<` or `+` or `*`), they are automatically applied using *infix notation*. Infix notation is where we use functions like `+` and `*` which we put in between our arguments. For example, `+` is a function which takes two arguments, and returns the result of adding them together (e.g. 2 + 5), `*` is a similar function which takes two numbers as arguments but instead returns the result of the two numbers multiplied together.

If instead we use letters, numbers, underscores and apostrophes to define a function (e.g `toFahrenheit`), then our function is applied using *prefix notation*. Prefix notation is where the function name is placed before the arguments (e.g: `show y`)

If we want to use a prefix function as an infix operator, we can surround the function name in backticks. If we want to use an infix operator as a prefix function, we can surround it in parenthesis:

```haskell
x >.< y     is the same as   (>.<) x y
quot x y    is the same as   x `quot` y
```

Once we have defined a function, Haskell will use it to substitute values. Once we hve defined `toFahrenheit`, anywhere `toFahrenheit x` appears, we can replace it with ``(9 * x) `quot` 5 + 32``. Similarly, anwhere `x >.< y` appears, we can replace it with ``(x + y) `quot` 2``.

#### Exercises

1. Create a prefix function called `square` which takes a number and squares it.
1. Create an infix function called `/\` which determines the area of a triangle of base length `b`, and height `h`
1. Write type signatures for the above functions, and test them in GHCi

## Making decisions in code

Without some kind of control flow, we are very limited in what we can achieve. There must be some way in which we can make branching calculations, or "decisions" in our code. We might want two different things based on the result of something else. To achieve this we will introduce the boolean type (called `Bool`).

A `Bool` can only have two values, either `True` or `False`. Because there are only two values that it can be, there are not going to be too many functions which do anything useful. In Haskell we have `&&` (pronounced "And") which will only be true if both arguments are `True`, we have `||`, which will be `True` if at least one argument is `True`, and we have `not` which will give the opposite of a boolean value.

```haskell
*ghci> not True
False
*ghci> not False
True
*ghci> True && False
False
*ghci> True && True
True
*ghci> True || False
True
*ghci> False || False
False
```

We'll take the time to create our own function that works the same as `not`, and call it `not'`. The `not` function is almost the simplest function that makes a "decision". `not` will decide to return `True` if we give it `False`, and vice versa. This is written in Haskell about as simply as you could imagine:

```haskell
not' :: Bool -> Bool
not' True = False
not' False = True
```

We simply tell the comiuler what we want the result to be for each value. As an aside, the apostrophe after `not` is called *prime*, and is used to indicate that a change or modification, or a new version of something. `foo'` is therfore pronounced "foo prime". A more compilcated example of a function is `&&` which only returns `True` if both of it's arguments are `True`. We'll call our copy of this function `&&&`:

```haskell
(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False
```

Again, we simply tell Haskell what we actually want the result to be. In the second line of the definition, we use underscores as arguments. If we put an underscore when definining a function where an argument should be, it tells the compiler that we don't care what that value is. As long as both arguments are not `True`, then we don't care what the arguments are, we already know that the answer is going to be `False`.

The part to the left of the equals sign is called a *pattern*. When we see that particular pattern in our code, we can *substitute* it with the part to the right of the equals sign. Haskell will try the patterns from the top of the file first, and then progress down the file to the bottom. We can see this in action using the following function:

```haskell
matchingTest :: Bool -> Bool -> Bool
matchingTest True _ = True
matchingTest _ True = False
```

```haskell
*ghci> matchingTest True True
True
```

Even though both patterns match `matchingTest True True`, the first line captures the match first, so the result is `True`. For completeness, try evaluating `matchingTest False False` which doesn't match *any* pattern. Since we haven't told the compiler what `matchingTest False False` is, we get an error.

Let's comapre our `not'` function to our `toFahrenheit` example. With our `toFahrenheit` function, to the left of the equals sign, we have `toFahrenheit c`. Wherever a pattern includes a name beginning with a *lowercase* letter, the pattern will *always* match where the name appears, and the value can be accessed using the name. For example when we write `toFahrenheit 52`, wherever `c` appears in the definition of `toFahrenheit`, we can replace it with `52` to get the answer. If, however, a part of a pattern includes a name beginning with an *uppercase* letter, the pattern will match if and only if the value is the same as the pattern. This is a distinction between *data constructors* which begin with an uppecse letter, and *variables* which begin with a lowercase letter, which we will talk about more detail later. To demonstrate this, consider the following definition for a copy of the `||` function we'll call `|||`:

```haskell
(|||) :: Bool -> Bool -> Bool
True ||| _ = True
False ||| x = x
```

The boolean "or" function returns `True` as long as one of it's arguments is `True`. In the above function, `True ||| anything` will match with the first line, and return `True` As discussed before, an underscore means we don't care what the value is, so an underscore will match with anything. In the second line `False ||| anything` will match with the second line. Since `x` is a variable, it will always match, and we can replace any instance of `x` to the right of the equals sign with the value of `x`. To show that our `|||` function is correct, we can use GHCi:

```haskell
*ghci> True ||| False
True
*ghci> False ||| True
True
*ghci> False ||| False
False
```

We can also pattern match numbers and letters (and any other data type). Try out using these functions:

```haskell
notZero :: Int -> Bool
notZero 0 = False
notZero _ = True

isWhitespace :: Char -> Bool
isWhitepace ' ' = True
isWhitespace '\t' = True
isWhitespace _ = False
```

```haskell
*ghci> notZero 2
True
*ghci> notZero 0
False
*ghci> isWhitespace ' '
True
*ghci> isWhitespace 'c'
False
```

For completeness, we can actually write `|||` or `notZero` a bit more simply:

```haskell
(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

notZero :: Int -> Bool
notZero = (/= 0)
```

#### Summary

* `Bool` has only two value: `True` and `False`
* Available to use, we have `||`, `&&` and `not` functions which operate on `Bool`
* We can pattern match fucntions to make "decisions".
* Patterns with an underscore always match, and the result is ignored.
* Patterns with a name starting with a lowercase letter always match, and the value is available whenever the name appears in the function definition. These are called *variables*.
* Patterns with a name starting with an uppercase letter match ony if the value is that same as the pattern. names starting with uppercase letters are called *data constructors*.

#### Exercises

1. Write a function called `xor` which takes two `Bool` arguments and returns `True` only if both arguments are different.

## Comparisons and more decisions

We can use pattern matching to create an `if'` function, which will evaluate to one thing if something is `True`, and another thing if `False`

```haskell
if' :: Bool -> a -> a -> a
if' True trueValue _ = trueValue
if' False _ falseValue = falseValue
```

Notice our type signature again. We are already familiar with what a `Bool` is, but what's `a`?. In the same way that we have data constructors mentioned above like `True` and `False`, we begin with uppercate letters, we also have *type constructors*, which also begin with a capital letter (Like `Bool`). In the same way we have variables which begin with a lowercase letter, we have *type variables* which also begin with a lowercase letter. Type constructors, like data constructors, only match when the value is the same. Type variables, like regular variables, will match with anything, and the value will be available whenever you see the same name. In essence, we say that we don't care what the type of `a` is, as long as all of the `a`'s have the same type. This means we can use our `if'` function for *any* type we can think of.

Haskell actually has a built in syntax for `if`, which has the form `if <booleanExpression> then <trueValue> else <falseValue>`. As an example wei'll write a sign function which gives `-1` if the a number is negative, `1` if the number is positive, and `0` otherwise:

```haskell
sign x :: Int -> Int
sign x = if x < 0 
    then -1 
    else if x > 0
        then 1
        else 0
```
 
We are introduced to comparison operators with the above code. Comparison operators take two values and return `True` or `False`. Below is a list of comparison operators available by default and some examples:

```haskell
(==)                    "Equal to"
3 == 5                  False
2 == 2                  True

(/=)                    "Not Equal to"
3 /= 5                  True
2 /= 2                  False

(>)                     "Greater than"
10 > 3                  True
5 > 5                   False
4 > 7                   False

(<)                     "Less than"
10 < 3                  False
5 < 5                   False
4 < 7                   True

(>=)                    "Greater than or equal to"
10 >= 3                 True
5 >= 5                  True
4 >= 7                  False

(<=)                    "Less than or equal to"
10 <= 3                 False
5 <= 5                  True
4 <= 7                  True
```

In addition, we are introduced to guards. Guards are given by a vertical bar followed by an expression. If the expression results in `True`, the function will evaluate to the value on the right of the equals sign. The guards are checked from top to bottom. For example `sign -3` checks the first guard. Since `-3 > 0` is `False`, then we move down to the next guard. since `-3 < 0` is true, `sign -3` equals the value on the right, so we can substitute `sign -3` with `-1`. Our last guard has a special value called `otherwise` which always succeeds. This construct is similar to `if`, `else if` and `else` in other languages.

Let's consider writing a function that takes two numbers `a` and `b`, and returns `a` to the power of `b`. 

```haskell
power :: Int -> Int
power _ 0 = 1
power a b = a * power a (b - 1)
```

In the above definition, we say that anything to the power of zero is equal to 1, otherwise x<sup>y</sup> is equal to x * x<sup>y - 1</sup>. For example 3<sup>4</sup> equals 3 * 3<sup>3</sup>, or 10<sup>3</sup> equals 10 * 10<sup>2</sup>.  We have now been introduced to *recursion*. Recursion is where we use a function inside of itself to get a result. To show that what we have written above is correct, let's consider an example. We will check it using our "mathematics class" method:

``` haskell
power 2 4   = 2 * power 2 3                     -- We can substitute power a b with a * power a (b - 1)
            = 2 * 2 * power 2 2                 -- b is still not zero, we substitute again
            = 2 * 2 * 2 * power 2 1             -- substitute again
            = 2 * 2 * 2 * 2 * power 2 0         -- and again
            = 2 * 2 * 2 * 2 * 1                 -- b is equal to zero, so we substitute in 1 instead
            = 2 * 2 * 2 * 2                     -- simplify
```

So `power 2 4` is indeed 2 to the power of 4. 
 
#### Summary

1. Function names start with a lowercase letter, and then include lowercase letters, uppercase letters, digits, underscores and apstrophes. They are called using prefix notation.
1. Operators consist of one or more of the following `!#$%&⋆+./<=>?@\^|-~:`, and must not begin with a colon. Operators are called using infix notation.
1. To call a named function in infix notation, surround it in backticks, to call an operator using prefix notation, surround it in parenthesis.
1. We have available some eoperators which will perform comparisons, they are `==`, `/=`, `>`, `<`, `<=` and `>=`. 
1. Guards are the equivalent of `if`, `else if`, and `else` in other programming languages. The special value `otherwise` always succeeds.

#### Exercises

1. Write a recursive function that multiplies two numbers together by adding them repeatedly. You are not allowed to use the built in `*` function.
1. Write a recursive function which returns the factorial of `n`. The factorial of `n` is zero if n is zero, and equal to `n` multiplied by the factorial of `n - 1` otherwise.
1. Using substitution and simplification, and some examples of your choosing, see how haskell would evaluate your function.

## Lists and arithmetic


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



## Notes

1. I will porbably be criticised by some Haskell purists who strongly believe that "Hello World" is not a suitable starting program to learn Haskell, because it skips a lot of information about monads, and side effects. However, I think it's important to teach how to write useful programs early on, and `IO` is a big part of that.
