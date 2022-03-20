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

    The `Semigroup` class makes it possible to group Types 
    together with an associative operation, namely `<>`. In the 
    case of `String`, this aliases to `++`, in case of the `newtype` 
    `Sum` it aliases to `+`. 

2.  ### A way to define functions from the type to the value level
    Of course here I'm obligated to show the infamous `reifyNat` example.

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
    writing them down, as GHC offers you to activate them anyway - define a 
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

### So what's the issue? 

In my humble opinion, it is a big issue that instance resolution happens strictly *before* unification, making 
it impossible to go back to resolution if unification has finishes, that means in particular that there cannot 
be two instances that have the same instance head but different contexts (This is because GHC only considers 
the head of the instance when doing instance declaration). 

A good example for this would be the following scenario: 

```haskell
D a b where -- fallible Coercion
  g :: a -> b 

C a b where -- infallible Coercion
  f :: a -> b 

instance D b a => C a b where 
  f = unsafeCoerce -- not really, but you get the idea

instance 
  ( E a 
  , c ~ Fam a b -- Fam is an associated type family to E
  ) => 
  C a c where -- here I get a more specific type, 
              -- i.e. the type resulting from application
              -- of Fam but I can't write that obvious 
              -- reasons, it would be practical, tho
  f = classMethodOfE
```

There really is no possibility to make this work - for obvious reasons, we have to commit to an instance *before*
the type family reduces, so we cannot know whether the type family application is more specific. 

I have two basic ideas to losen this constraint: 

1. ### Go back to instance resolution after failing to unify

Like this we could consider the context, in the example, first choose any of the instances, try to unify, if 
that fails, choose any of the remaining equally specific instances, only fail if there are no instances 
left that fit the type and unify. 

A justified concern with that is, that there is no real way to choose which instance to pick when there are two
matching instances, because potentially, there can be two instances that both match the type *and* unify. 

There are two solutions to account for that:

- disallow two instances that are not disjoint
- make it the concern of the programmer (of course, this is the worse option)

2. ### instance families

I propose a syntax like: 

```haskell
instance family C a b where
  E a, c ~ Fam a b => f = ...
  D b a => f = ...
```

Like this, we can match on the contexts in order and resolve the instances locally so that the impact can be 
controlled. GHC then choose the first context that matches an lets instances resolution happen after this. 

This has two main advantages: 
- There is exactly one scenario where we have unification before instance resolution
- It is harder to accidentally have two constraints overlap 

Resolving the instance resolution problem could not only make some situations easier to resolve for the 
programmer but would also offer new programs, consider the following types: 


