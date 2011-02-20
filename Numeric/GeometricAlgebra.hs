{-# LANGUAGE GADTs #-}
module Numeric.GeometricAlgebra
  (
   -- * Fundamental types
   -- | These are exported opaquely because their representation is not what
   --   you probably expect if you are used to linear algebra.  See the
   --   "shortcuts to construction", below, for some ways to construct them.
   Object,
   Scalar,
   Vector,
   Bivector,
   Trivector,
   Blade,
   Multivector,
   
   -- * Shortcuts to construction
   scalar,
   vector,
   bivector,
   trivector,
   object,
   blade,
   multivector,
   
   -- * Shortcuts to examination
   scalarValue,
   vectorValues,
   bivectorValues,
   trivectorValues,
   objectValues,
   bladeValues,
   multivectorValues,
   
   -- * Multiplication-like binary operations
   -- | We provide two names for each of these, a verbose name for those who
   --   wish to be explicit about what they are doing and an operator for
   --   those who wish brevity.
   scalarProduct,
   outerProduct,
   innerProduct,
   geometricProduct,
   geometricQuotient,
   `[*]`,
   `[^]`,
   `[.]`,
   `[@]`,
   `[/]`,
   
   -- * Addition-like binary operations
   -- | We provide two names for each of these, a verbose name for those who
   --   wish to be explicit about what they are doing and an operator for
   --   those who wish brevity.
   sum,
   difference,
   `[+]`,
   `[-]`,
   
   -- * Unary operations
   normal,
   grade
  )
  where


-- -----------------------------------------------------------------------------
-- Fundamental types

-- | The basic object in geometric algebra; scalars, vectors, bivectors, and
--   trivectors are (non-exhaustive) subtypes of it.
data Object float where
  Object :: float -> float -> float -> float
         -> float -> float -> float -> float
         -> Object float
{-
  This MUST have a better name, as it is not the most general type - it is not,
in particular, a supertype of Blades or Multivectors.
  This is a GADT of some sort, probably?  I don't understand quite how to
make Haskell do what we want here.  Perhaps you do.
  The basis of all geometric-algebra objects is { 1, e1, e2, e3, e1^e2,
e2^e3, e3^e1, e1^e2^e3 }.  It is therefore possible to represent them with
eight scalars.  Right?
  Notice that I parametrized this on the type of the float.  We probably want
to add the appropriate context requiring it to be a type that behaves like
a float.  Although consider:

    http://hackage.haskell.org/trac/haskell-prime/wiki/NoDatatypeContexts
-}


-- | A type synonym for all Objects which behave like scalars; that is, which
--   have zero for all values except the unit component.
type Scalar = Object ...

-- | A type synonym for all Objects which behave like vectors; that is, which
--   have zero for all values except the three basis-vector components.
type Vector = Object ...

-- | A type synonym for all Objects which behave like bivectors; that is, which
--   have zero for all values except the three  basis vector double outer
--   product components.
type Bivector = Object ...

-- | A type synonym for all Objects which behave like trivectors; that is,
--   which have zero for all values except the single basis vector triple outer
--   product component.
type Trivector = Object ...


data Blade float = Blade ...


data Multivector float = Multivector ...


-- -----------------------------------------------------------------------------
-- Shorcuts to construction

scalar :: float -> Object float


vector :: (float, float, float) -> Object float


bivector :: (float, float, float) -> Object float


trivector :: float -> Object float


blade :: ... -> Blade float


multivector :: ... -> Multivector float


-- -----------------------------------------------------------------------------
-- Shorcuts to examination

scalarValue :: Scalar float -> float


vectorValues :: Vector float -> (float, float, float)


bivectorValues :: Bivector float -> (float, float, float)


trivectorValues :: Trivector float -> float


bladeValues :: Blade float -> ...


multivectorValues


normal :: Object ... -> Scalar


-- -----------------------------------------------------------------------------
-- Multiplication-like binary operations

-- -----------------------------------------------------------------------------
-- Addition-like binary operations

-- -----------------------------------------------------------------------------
-- Unary operations
