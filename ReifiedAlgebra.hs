{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module ReifiedAlgebra (
  Def(ReifiedMonoid,
      ReifiedAbelianGroup,
      ReifiedRing,
      ReifiedField),
  using
) where

import Data.Reflection
import Data.Constraint

import qualified Algebra.Monoid as M
import qualified Algebra.Additive as AG
import qualified Algebra.Ring as R
import qualified Algebra.Field as F

import Reificator

-- Monoid
instance ReifiableConstraint M.C where
  data Def M.C a = ReifiedMonoid {
    zero_m :: a,
    plus_m :: a -> a -> a }
  reifiedIns = Sub Dict

instance Reifies s (Def M.C a) => M.C (Lift M.C a s) where
  idt = a where a = Lift $ zero_m (reflect a)
  a <*> b = Lift $ plus_m (reflect a) (lower a) (lower b)

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

instance Reifies s (Def AG.C a) => M.C (Lift AG.C a s) where
  idt = a where a = Lift $ zero_ag (reflect a)
  a <*> b = Lift $ plus_ag (reflect a) (lower a) (lower b)
OA
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

instance Reifies s (Def R.C a) => M.C (Lift R.C a s) where
  idt = a where a = Lift $ zero_r (reflect a)
  a <*> b = Lift $ plus_r (reflect a) (lower a) (lower b)

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

instance Reifies s (Def F.C a) => M.C (Lift F.C a s) where
  idt = a where a = Lift $ zero_f (reflect a)
  a <*> b = Lift $ plus_f (reflect a) (lower a) (lower b)
