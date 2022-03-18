# Haskell classes and my love-hate relationship with them 

## An overview

### What are Haskell classes

Haskell classes are 

1.  ### A way to group types with certain properties
    This is the "original" use case Haskell classes have been made 
    for, a classic example for that is the class `Semigroup`:

    ```haskell
    type Semigroup :: Type -> Constraint 
    class Semigroup a where 
      (<>) :: a -> a -> a
      -- other class methods ommited because
      -- they are unimportant
    ```

    The `Semigroup` class makes it possible to group all Types 
    together with an associative operation, namely `<>`. In the 
    case of `String`, this aliases to `++`, in case of the `newtype` 
    `Sum` it aliases to `+`. 

2.  ### A way to define functions from the type to the value level
    of course here I'm obligated to show the infamous `reifyNat` example.

    ```haskell
    {-# LANGUAGE KindSignatures, DataKinds, 
        AllowAmbiguousTypes, TypeApplications, 
        ScopedTypeVariables #-}

    import Numeric.Natural (Natural)

    data Nat = Z 
             | S Nat 

    class ReifyNat (n::Nat) where
      reifyNat :: Natural

    instance ReifyNat n => ReifyNat (S n) where 
        reifyNat = 1 + reifyNat @n

    instance ReifyNat Z where 
      reifyNat = 0

    sampleNat :: Natural 
    sampleNat = reifyNat @(S (S Z))
    ```
    (this code snippet makes you wonder whether there is actually anything
    useful you can do with Haskell without extensions, lol)

    Of course this doesn't only apply to converting type level peano naturals 
    to value level naturals but also to more useful things, e.g. proof witnesses 
    for things like heterogeneous records. 

3.  ### A way to overload functions 
    This generalizes and at the same time - at least in my opinion - abuses the 
    class mechanism. I think if you use classes to overload function names, the 
    grouped types should at least in some sense allow for the definition of laws 
    for the class, which makes this a subset of 1.

4.  ### A way to define Relations between types
    A nice example for that is that you might always want a singleton in a length
    indexed vector to be returned as the element it actually contains instead of 
    the singleton to save unwrapping. This might sound a bit contrived but I have 
    encountered this particular example two times in a short time recently so I don't 
    want to leave it out: 

    First of all let us - after enabling some more extensions, I will not continue 
    writing them down anymre as GHC offers you to activate them anyway - define a 
    vector GADT: 
    ```haskell
    data Vec (n :: Nat) (a::Type) where
      Nil :: Vec Z a
      (:::) :: a -> Vec n a -> Vec (S n) a

    infixr 5 :::
    ```
    Let's look if it type checks: 
    ```haskell 
    v :: Vec (S (S Z)) Int
    v = 3 ::: 4 ::: Nil
    ``` 
    Thankfully, all is fine. 

    Now let's go on and define our class: 
    ```haskell
    class UnwrapSingleton a b | a -> b where 
      maybeUnwrap :: a -> b

    -- every singleton gets unwrapped to the element
    instance UnwrapSingleton (Vec (S Z) a) a where 
      maybeUnwrap (a ::: Nil) = a

    -- every list that is the successor of anything 
    -- that isn't Z just gets id'd
    instance 
      UnwrapSingleton (Vec (S (S b)) a) (Vec (S (S b)) a) 
      where 
      maybeUnwrap = id 

    -- same for empty vectors
    instance UnwrapSingleton (Vec Z a) (Vec Z a) where 
      maybeUnwrap = id
    ``` 
    As a result we can now use it like this: 
    ```haskell
    sing = maybeUnwrap (3 ::: Nil)

    notsing = maybeUnwrap (4 ::: 3 ::: Nil)
    ``` 
    if we ask ghc what type `sing` has it will happily tell us that it is 
    an `Integer` (it defaults the literal 3 to be of that type)




