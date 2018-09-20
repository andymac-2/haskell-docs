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

