# The Beginning

## Introduction

*Disclaimer: I made this to help me learn, and document the ways in which I would solve problems with the language. I don't guarantee the wuality of what I write. I'm always learning, so if there is a different or better way of doing things let me know!*

Learning Haskell is hard. Coming from a background of almost any other programming language, you have to forget almost everything you learn. My experiences with Haskell have made me a better programmer for that reason. Haskell should hopefully, by the end of this document, make you think in different ways about solving problems in programming. I will try my hardest to cater for different learning styles, and try to distill the essencve of what programming in Haskell is all about. I will assume a high school level of mathematics, basic command line usage, and knowledge of at least one other programming language. Examples not written in haskell will be written in JavaScript, but in depth knwledge of JavaScript will not be required.

The first thing any programmer does wiht any language is write a "Hello World" program. This ensures that your build environment is set up correctly. I reccomend [the haskell tool stack](https://github.com/commercialhaskell/stack/) for first time users. If you already have a haskell environment, you can sklip the next section

## An aside: Installing the Haskell tool stack.

Once stack is installed navigate into an empty directory and run

```bash
$ stack new hello-world
```

Once this is installed you should see a directory structure:

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
module Main where

main :: IO ()
main = putStrLn "Hello World!"
```

then run

```bash
$ stack build
```

Which may take some time as it installs the Gascow Haskell Compiler (GHC). After this step is completed, run your program using

```bash
$ stack exec hello-world-exe
Hello World!
```

If you see the words "Hello World!" On the screen, then your program has compiled, and you have completed the hardest part of this book.

## Your first Haskell program

The best way to learn programming is to start writing programs. I'm not going to try to get bogged down with details about what the best way to set your environment is, as this can be corrected later. For now, we have an environment where we can write to `Main.hs` and compile and run with `stack build && stack exec hello-world-exe`. So if you haven't already, write the following to `Main.hs` and run it. You should see the following:

```
Hello World!
```

Once you have seen this, you know that your build enviroment is set up correctly.

