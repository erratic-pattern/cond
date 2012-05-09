{-# LANGUAGE FlexibleInstances #-}
module Data.Algebra.Boolean 
       ( Boolean(..), fromBool
       ) where
import Data.Monoid (Any(..), All(..), Dual(..), Endo(..))
import Prelude hiding ((&&), (||), not)
import qualified Prelude as P
infixr  0 <-->
infixr  1 -->
infixr  2 ||
infixr  3 &&

-- |A class for boolean algebras. Instances of this class should obey
-- all the axioms of boolean algebra.
--
-- Minimal complete definition: 'true', 'false', 'not' or '<-->', '||' or '&&'. 
class Boolean b where
  -- |Truth value.
  true    :: b
  -- |False value.
  false   :: b
  -- |Logical negation.
  not     :: b -> b
  -- |Logical conjunction.
  (&&)    :: b -> b -> b
  -- |Logical inclusive disjunction.
  (||)    :: b -> b -> b
  -- |Logical exclusive disjunction.
  xor   :: b -> b -> b
  -- |Logical implication
  (-->) :: b -> b -> b
  -- |Logical biconditional
  (<-->) :: b -> b -> b
  
  -- Default implementations
  not       = (<--> false)
  x && y    = not (x || y) 
  x || y    = not (x && y)
  x `xor` y = (x || y) && (not (x && y))
  x --> y   = not x || y
  x <--> y  = (x && y) || not (x || y)

-- |Injection from 'Bool' into a boolean algebra.
fromBool :: Boolean b => Bool -> b
fromBool b = if b then true else false

instance Boolean Bool where
  true = True
  false = False
  (&&) = (P.&&)
  (||) = (P.||)
  not = P.not
  xor = (/=)
  True  --> True  = True
  True  --> False = False
  False --> _     = True
  (<-->) = (==)
  

instance Boolean Any where
  true                  = Any True
  false                 = Any False
  not (Any p)           = Any (not p)
  (Any p) &&    (Any q) = Any (p && q)
  (Any p) ||    (Any q) = Any (p || q)
  (Any p) `xor` (Any q) = Any (p `xor` q)
  (Any p) --> (Any q)   = Any (p --> q)
  (Any p) <--> (Any q)  = Any (p <--> q)
  
instance Boolean All where
  true                  = All True
  false                 = All False
  not (All p)           = All (not p)
  (All p) && (All q)    = All (p && q)
  (All p) || (All q)    = All (p || q)
  (All p) `xor` (All q) = All (p `xor` q)
  (All p) --> (All q)   = All (p --> q)
  (All p) <--> (All q)  = All (p <--> q)
  
instance Boolean (Dual Bool) where
  true                    = Dual True
  false                   = Dual False
  not (Dual p)            = Dual (not p)
  (Dual p) && (Dual q)    = Dual (p && q)
  (Dual p) || (Dual q)    = Dual (p || q)
  (Dual p) `xor` (Dual q) = Dual (p `xor` q)
  (Dual p) --> (Dual q)   = Dual (p --> q)
  (Dual p) <--> (Dual q)  = Dual (p <--> q)
  
instance Boolean (Endo Bool) where
  true                    = Endo (const True)
  false                   = Endo (const False)
  not (Endo p)            = Endo (not . p)
  (Endo p) && (Endo q)    = Endo (\a -> p a && q a)
  (Endo p) || (Endo q)    = Endo (\a -> p a || q a)
  (Endo p) `xor` (Endo q) = Endo (\a -> p a `xor` q a)
  (Endo p) --> (Endo q)   = Endo (\a -> p a --> q a)
  (Endo p) <--> (Endo q)  = Endo (\a -> p a <--> q a)