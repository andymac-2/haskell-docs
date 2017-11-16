The <strong>`Bool`</strong> datatype represents boolean `True` or `False` values.

## Packages
* (base) Prelude
* (base) Data.Bool

## Syntax

```haskell
data Bool :: *
```

### Contrustors

```haskell
True
```

Takes no arguments and returns `True`

```haskell
False
```

Takes no arguments and returns `False`

## Description

The `Bool` datatype represents truth values in boolean algebra. `Bool` is used to represent values which can have two results either `True` or `False`. In Haskell, `True` and `False` are both constructors and therefore capitalised. `True` and `False` take no arguments, and return themselves.

## Functions


### `(&&)` Pronounced "AND"
[Main article : Data.Bool.(&&)]((&&).md)
```haskell
(&&) :: Bool -> Bool -> Bool
```
Returns `True` if both arguments are `True`; otherwise `False`.

### `(||)` Pronounced "OR" 
[Main article : Data.Bool.(||)]((||).md)
```haskell
(||) :: Bool -> Bool -> Bool
```
Returns `True` if any one of the arguments are `True`; otherwise `False`.

### `not`
[Main article : Data.Bool.not](not.md)
```haskell
not :: Bool -> Bool
```
Returns `True` if the argument is `False`; otherwise `False`

### `otherwise`
[Main article : Data.Bool.otherwise](otherwise.md)
```haskell
otherwise :: Bool
```
Defined as `True`

### `bool`
[Main article : Data.Bool.bool](otherwise.md)
```haskell
bool :: a -> a -> Bool -> a
```
`bool falseValue trueValue test` returns `falseValue` when `test` is `False` and `truthValue` when `test` is `True`

## Instances

### Bounded
The <strong>`Bool`</strong> datatype represents boolean `True` or `False` values.

## Packages
* (base) Prelude
* (base) Data.Bool

## Syntax

```haskell
data Bool :: *
```

### Contrustors

```haskell
True
```

Takes no arguments and returns `True`

```haskell
False
```

Takes no arguments and returns `False`

## Description

The `Bool` datatype represents truth values in boolean algebra. `Bool` is used to represent values which can have two results either `True` or `False`. In Haskell, `True` and `False` are both constructors and therefore capitalised. `True` and `False` take no arguments, and return themselves.

## Functions


### `(&&)` Pronounced "AND"
[Main article : Data.Bool.(&&)]((&&).md)
```haskell
(&&) :: Bool -> Bool -> Bool
```
Returns `True` if both arguments are `True`; otherwise `False`.

### `(||)` Pronounced "OR" 
[Main article : Data.Bool.(||)]((||).md)
```haskell
(||) :: Bool -> Bool -> Bool
```
Returns `True` if any one of the arguments are `True`; otherwise `False`.

### `not`
[Main article : Data.Bool.not](not.md)
```haskell
not :: Bool -> Bool
```
Returns `True` if the argument is `False`; otherwise `False`

### `otherwise`
[Main article : Data.Bool.otherwise](otherwise.md)
```haskell
otherwise :: Bool
```
Defined as `True`

### `bool`
[Main article : Data.Bool.bool](otherwise.md)
```haskell
bool :: a -> a -> Bool -> a
```
`bool falseValue trueValue test` returns `falseValue` when `test` is `False` and `truthValue` when `test` is `True`
