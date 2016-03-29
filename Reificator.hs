{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, PolyKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Taken from an <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection article> about reflection in Haskell
module Reificator where

import Data.Proxy
import Data.Reflection
import Data.Constraint
import Data.Constraint.Unsafe

--------------------------------------------------------------------------------
-- Lift/ReifiableConstraint machinery.

-- | Prepares an value to be used with reified constraint
newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

-- | Kind of a first-class constraint
class ReifiableConstraint p where
  -- | A constraint builder
  data Def (p :: * -> Constraint) (a :: *) :: *
  -- | Uses a constraint builder
  reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

-- | Evaluates an annotated expression in a reifiable constraint
with :: Def p a                                       -- ^ a context constraint builder
     -> (forall s. Reifies s (Def p a) => Lift p a s) -- ^ an annotated expression
     -> a                                             -- ^ an evaluated value
with d v = reify d (lower . asProxyOf v)
  where
    asProxyOf :: f s -> Proxy s -> f s
    asProxyOf x _ = x

--------------------------------------------------------------------------------
-- Kicking it up to over 9000

-- | Evaluates an expression in a reifiable constriant
using :: forall p a. ReifiableConstraint p
      => Def p a                           -- ^ a context constraint builder
      -> (p a => a)                        -- ^ an expression in a given context
      -> a                                 -- ^ an evaluated value
using d m = reify d $ \(_ :: Proxy s) ->
  let replaceProof :: Reifies s (Def p a) :- p a
      replaceProof = trans proof reifiedIns
        where proof = unsafeCoerceConstraint :: p (Lift p a s) :- p a
  in m \\ replaceProof
