{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- | Builders of algebraic structures, they build the structure from given operations and significant elements.
module ReifiedAlgebra (
  -- * Builders
  {-| They create 'Def'. Defined constructors:

    * 'Def.ReifiedAbelianGroup'
    Takes:

           1. 'Def.ReifiedAbelianGroup.zero_ag' the neutral element of addition.
           2. 'Def.ReifiedAbelianGroup.plus_ag' the addition operation.
           3. 'Def.ReifiedAbelianGroup.negate_ag' the addition inverse operation.


    * 'Def.ReifiedRing'
    Takes:

           1. 'Def.ReifiedRing.zero_r' the neutral element of addition.
           2. 'Def.ReifiedRing.plus_r' the addition operation.
           3. 'Def.ReifiedRing.negate_r' the addition inverse operation.
           4. 'Def.ReifiedRing.multiply_r' the multiplicative operation.
           5. 'Def.ReifiedRing.one_r' the neutral element of multiplication.


    * 'Def.ReifiedField'
    Takes:

           1. 'Def.ReifiedField.zero_f' the neutral element of addition.
           2. 'Def.ReifiedField.plus_f' the addition operation.
           3. 'Def.ReifiedField.negate_f' the addition inverse operation.
           4. 'Def.ReifiedField.multiply_f' the multiplicative operation.
           5. 'Def.ReifiedField.one_f' the neutral element of multiplication.
           6. 'Def.ReifiedField.inverse_f' the multiplicative inverse operation.

  -}
  Def(ReifiedAbelianGroup,
      ReifiedRing,
      ReifiedField),
  -- * Using
  -- | Takes a builded algebraic structure and applies it in a computation of the second argument.
  using
) where

import Data.Reflection
import Data.Constraint

import qualified Algebra.Additive as AG
import qualified Algebra.Ring as R
import qualified Algebra.Field as F

import Reificator

-- AbelianGroup
instance ReifiableConstraint AG.C where
  data Def AG.C a = ReifiedAbelianGroup {
    zero_ag :: a,
    plus_ag :: a -> a -> a,
    negate_ag :: a -> a }
  reifiedIns = Sub Dict

instance Reifies s (Def AG.C a) => AG.C (Lift AG.C a s) where
  zero = a where a = Lift $ zero_ag (reflect a)
  a + b = Lift $ plus_ag (reflect a) (lower a) (lower b)
  negate a = Lift $ negate_ag (reflect a) (lower a)

-- Ring
instance ReifiableConstraint R.C where
  data Def R.C a = ReifiedRing {
    zero_r :: a,
    plus_r :: a -> a -> a,
    negate_r :: a -> a,
    multiply_r :: a -> a -> a,
    one_r :: a }
  reifiedIns = Sub Dict
 
instance Reifies s (Def R.C a) => R.C (Lift R.C a s) where
  a * b = Lift $ multiply_r (reflect a) (lower a) (lower b)
  one = a where a = Lift $ one_r (reflect a)

instance Reifies s (Def R.C a) => AG.C (Lift R.C a s) where
  zero = a where a = Lift $ zero_r (reflect a)
  a + b = Lift $ plus_r (reflect a) (lower a) (lower b)
  negate a = Lift $ negate_r (reflect a) (lower a)

-- Field
instance ReifiableConstraint F.C where
  data Def F.C a = ReifiedField {
    zero_f :: a,
    plus_f :: a -> a -> a,
    negate_f :: a -> a,
    multiply_f :: a -> a -> a,
    one_f :: a,
    inverse_f :: a -> a }
  reifiedIns = Sub Dict

instance Reifies s (Def F.C a) => F.C (Lift F.C a s) where
  recip a = Lift $ inverse_f (reflect a) (lower a)
 
instance Reifies s (Def F.C a) => R.C (Lift F.C a s) where
  a * b = Lift $ multiply_f (reflect a) (lower a) (lower b)
  one = a where a = Lift $ one_f (reflect a)

instance Reifies s (Def F.C a) => AG.C (Lift F.C a s) where
  zero = a where a = Lift $ zero_f (reflect a)
  a + b = Lift $ plus_f (reflect a) (lower a) (lower b)
  negate a = Lift $ negate_f (reflect a) (lower a)
