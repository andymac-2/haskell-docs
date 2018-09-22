# Preface

*Disclaimer: I made this to help me learn, and document the ways in which I would solve problems with the language. I don't guarantee the quality of what I write. I'm always learning, so if there is a different or better way of doing things let me know!*

Learning Haskell is hard. Coming from a background of almost any other programming language, you have to forget almost everything you learn. Haskell should hopefully, by the end of this document, make you think in different ways about solving problems in programming.

One of the difficulties of learning Haskell, and a personal difficulty of mine, is that often, the primary focus is given towards very abstract concepts, and inner workings. Those who are less mathematically inclined can be put off by the particularly mathematical and convoluted explanations. This has been epitomised by the quote "monads are just monoids in the category of endofunctors" which has been used to joke about the obscurity of some of the concepts of Haskell. To someone not familiar with advanced category theory, the quote above, and other descriptions like it are meaningless word salad, and do not help anyone to learn anything.

One of the common paradigms of computing is to separate a large program into small parts, and ensure that each small part works well on it's own. Once we have created a part that works, we can then forget about *how* it works, as long as we know *how to use it*. The idea behind having a library of code written and tested by somebody else has existed for as long as practical computer programs have been around. Functional programming is excellent at creating small, isolated, reusable parts, unfortunately, I think most authors seem to discard this way of thinking when actually trying to explain how to write programs in functional lanaguages.

Even books like "Learn You a Haskell For Great Good" (which is a fantastic book by the way) in my opinion, focus too much on inner workings, when the focus should be given to where these constructs are useful and what kinds of problems that they can solve. If a programmer is then curious to the inner workings of a function, then they can dive into source code and read explanations, but understanding the inner workings and rationale behind library functions too early can be counterintuitive and often confusing. Using source code as an explanation is especially bad, since it can change behind the scenes without warning. Almost no documentation in other programming languages attempts to explain how something works before they explain how to use it.

Therefore, I put emphasis towards how to write useful programs, the kinds of programs that you would write in other programming languages. *This comes at the expense of some accuracy*. This is most likely going to be controversial to a number of Haskell programmers, but I do not claim to be an authority on what constitutes a good way to learn for everybody. If you as the reader, find that this book does not suit your particular style of learing, I encourage you to find a resource that suits your needs, and there are some excellent resources out there. This book is therefore intended to help people learn in the way that I would have liked to learn Haskell.

In terms of previous knowledge, I will assume a high school level of mathematics, basic command line usage, and knowledge of at least one other programming language.

## Setting up your environment

The first thing any programmer does with any language is write a "Hello World" program. This ensures that your build environment is set up correctly. I recommend [the haskell tool stack](https://github.com/commercialhaskell/stack/) for first time users. A tutorial to install stack can be found [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/) If you already have a haskell environment, you can skip the next section. Many parts of this book will assume that you have `stack` installed. Once stack is installed navigate into a new directory and run the following on the command line:

```bash
$ stack new hello-world
```

Once this is complete you should see a directory structure similar to the following:

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

### GHCi

GHCi or *GHC interactive* is an interactive terminal that can be used to evaluate haskell expressions. GHCi can be used for debugging, or just as a playground for trying new things. To start GHCi, navigate somewhere into the `hello-world` directory you just created, and use `stack ghci` to run GHCi. You will be greeted with a prompt which may look something like this:

```haskell
*Main Lib>
```

Which means that the `Main` module and the `Lib` module have been loaded. `Main` and `Lib` correspond to `app/Main.hs` and `src/Lib.hs` respectively. You can write any valid haskell expression and it will evaluate when you press return:

```haskell
*ghci> 2 + 2
4
*ghci> head [1, 2, 3]
1
*ghci> True && False
False
```

Open up `src/Lib.hs` and replace it with the following:

```haskell
testString :: String
testString = "It Works!"
```

After this moment, whenever we use GHCi, we'll specify it by writing `*ghci>`. Go back into your GHCi terminal and type the following:

```haskell
*ghci> :r
```

This will reload all of the files in your project. To test that it has in fact reloaded your files:

```haskell
*ghci> testString
"It works!"
```

You write functions in your `src/Lib.hs` file, and use `:r` to load them into an interactive session. We will do this a lot in this tutorial.

For additional help, use the `:h` command, or go to the [user guide](https://downloads.haskell.org/~ghc/master/users-guide/ghci.html)
