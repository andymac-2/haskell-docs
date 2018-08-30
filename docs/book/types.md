# Haskell types and typeclasses

In haskell, types play a fundamental role to write any useful program. Haskell is a *strongly typed* language, which means that types cannot be converted to one another automatically. Therefore to convert between types you must use a function to do so. Haskell supports a wide range of useful types, and supports advanced typing features, so without further ado, let's introduce some types.

* `Bool`: `Bool` can only have two values `True` or `False`
* `()`: (pronounced "unit"). Unit is like `Bool` but instead of two values, `True` and `False`, unit only has one value which is written by an empty set of parenthesis: `()`.
* `Ordering`: When you compare two things together, you will get an ordering. Ordering can be one of three values, `LT` for less than, `GT` for greater than, and `EQ` for equal.
* `Char`: a single character. Characters are written using single quotes for example: `'c'` or `'\n'`. For the complete list of escape characters see [the Haskell 2010 report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)
* `Int`: an integer within the range -2^29 and 2^29-1 (-536870912 to 536870911).
* `Integer`: an integral type which can store *any* integer. It has no bounds.
* `Float`: a single precision floating point number
* `Double`: a double prescision floating point number

In addition to this, we can have type variables. These allow us to create types based on other types. For example, a list of integers is not the same as a list of booleans. In fact, we can have a list of anything, including a list of lists. Here are some examples of some type constructors.

* `[a]`: a list of `a`. `a` could be any other type, like `Bool`, `()`, `Double`, `Int` or even itself (a list of lists)
* `Maybe a`: an optional value. It might be a value, it might be nothing. Again, `a` could be anything. If the value exists


TODO


* `(Integral a) => Ratio a` e.g. `Ratio Int` or `Ratio Integer`. A rational number. `Ratio Int` can represent any fraction that can be expressed by one `Int` divided by another. `Ratio Integer` can represent any rational number.
* `(RealFloat a) => Complex a` e.g. `Complex Float` or `Complex Double`. A complex number. `Complex Float` will be a complex number where the real and imaginary parts are single precision floating point numbers. `Complex Double` will be the same, but with double precision numbers.
