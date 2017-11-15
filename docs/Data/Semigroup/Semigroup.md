The <strong>`Semigroup`</strong> represents a set with an associative binary operation. This makes a semigroup a superset of [`Data.Monoid.Monoid`](../Monoid/Monoid.md). Semigoups have no other restrictions, and are a very general typeclass.

See also [Data.Monoid.Monoid](../Monoid/Monoid.md): a `Semigroup` with an identity value.

## Packages
* (base) Data.Semigroup

## Syntax

```haskell
class Semigroup a where
    (<>) :: a -> a -> a
    sconcat :: Nonempty a -> a
    stimes :: Integral b => b -> a -> a
```

### Minimal Complete Definition

```haskell
(<>)
```

## Description

Any datatype `a` which has an associative binary operation will be able to become a member of the `Semigroup` typeclass. An instance of `Monoid a` automatically satisfies the requirements of a `Semigroup` making `Semigroup` a strict superset of `Monoid`. The `Monoid` typeclass however does not enforce it's instances to already be instances of `Semigroup`

The `Semigroup` is a particularly forgiving typeclass in it's requirements, and datatypes may have many instances of `Semigroup` as long as they have functions which satisfy the requirements.
        
### Semigroup Laws

In addition to the class requirements above, potential instances of `Semigroup` must obey a single law in order to become instances:

*   *The binary operation `<>` must be associative*
    ```haskell
    (a <> b) <> c == a <> (b <> c)
    ```
    As long as you do not change the order of the arguments, you can insert parenthesis anywhere, and the result will be the same.

For example, addition (`a + (b + c) == (a + b) + c`), and multiplication (`a * (b * c) == (a * b) * c`) satisfy this requirement. Therefore `<>` could be defined as `+` or `*` for instances of class `Num a`. Division (`div`) however, would not be a candidate as it is not associative: ``8 `div` (4 `div` 2) == 4`` is not equal to ``(8 `div` 4) `div` 2 == 2``.
    
In essence, the `<>` function could do anything, as long as it doesn't matter where you put parenthesis.

### Rules for Monoids

Instances of `Monoid` have to obey an additional rule:
```haskell(<>) == mappend```
This is to ensure that the instance of `Monoid` is equivalent to a more strict instance of `Semigroup`.
    
## Methods

```haskell(<>) :: a -> a -> a```
:An associative binary operation.
```haskellsconcat :: [[Data.List.Nonempty|Nonempty]] a -> a```
:Take a nonempty list of type `a` and apply the `<>` operation to all of them to get a single result.
```haskellstimes :: Integral b => b -> a -> a```
:Given a number `x` and a value of type `a`, apply `<>` to the value `x` times.

## Examples

### Sum numbers using `<>`:

```haskell
Sum 3 <> Sum 4       -- with the type "Sum a", "<>" becomes "+"
-- returns: Sum {getSum = 7} because (3 <> 4) == (3 + 4) == 7
```

### Exponents using `stimes`:

```haskell
stimes 4 (Product 3) -- with the type "Product a" "<>" becomes "*"
-- returns: Product {getProduct = 81} 
-- This is because (3 <> 3 <> 3 <> 3) == (3 * 3 * 3 * 3) == 81
-- i.e. 3 multiplied by itself 4 times.
```

### Test for any elements which are True in a non-empty list using `sconcat`:

```haskell
sconcat (Any True :| [Any False, Any True, Any False])
-- sconcat will apply "<>" to all of the members in a list
-- returns: Any {getAny = True}
-- If any elements in the list are True than the whole expression is True
-- The type "Any" converts "<>" to "||"
-- (True <> False <> True <> False) == (True || False || True || False) == True
```

### Test if all elements are True in a non-empty list using `sconcat`:

```haskell
sconcat (All True :| [All False, All True, All False])
-- returns: All {getAll = False}
-- If all elements in the list are True than the whole expression is True
-- The type "All" converts "<>" to "&&"
-- (True <> False <> True <> False) == (True && False && True && False) == True
```

## See Also
* [Data.Monoid.Monoid](../Monoid/Monoid.md): a special case of `Semigroup` with an identity element `mempty`
