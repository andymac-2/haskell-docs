# Monads

Monads are the most general kind of side effect. They allow you to 






## do notation

Rules:

* Each action is written on a separate line
* Every line inside a `do` block must have the same kind of action as each other: e.g, you can't mix `IO a` with `Reader a`
* To use the value returned by a function, use `<-`
* If you want to write some pure function that don't need side effects, you can write a `let` block. Note that inside of a `do` block, `let` does not require `in`
* You can also use `fmap` or `<$>` to use pure functions inside a `do` block.
