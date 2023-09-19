{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             DeriveDataTypeable
  #-}
module Data.Algebra.Boolean
       ( Boolean(..), fromBool, Bitwise(..)
       ) where
import Data.Monoid (Any(..), All(..), Dual(..), Endo(..))
import Data.Bits (Bits, complement, (.|.), (.&.))
import qualified Data.Bits as Bits
import Data.Function (on)
import Data.Typeable
import Data.Data
import Data.Ix
import qualified Data.Foldable as F
import Foreign.Storable
import Text.Printf
import Prelude hiding ((&&), (||), not, and, or, any, all)
import qualified Prelude as P

infixr  1 <-->, `xor`, -->
infixr  2 ||
infixr  3 &&

-- |A class for boolean algebras. Instances of this class are expected to obey
-- all the laws of boolean algebra.
--
-- Minimal complete definition: 'true' or 'false', 'not' or '<-->', '||' or '&&'.
class Boolean b where
  -- |Truth value, defined as the top of the bounded lattice
  true    :: b
  -- |False value, defined as the bottom of the bounded lattice.
  false   :: b
  -- |Logical negation.
  not     :: b -> b
  -- |Logical conjunction. (infixr 3)
  (&&)    :: b -> b -> b
  -- |Logical inclusive disjunction. (infixr 2)
  (||)    :: b -> b -> b
  -- |Logical exclusive disjunction. (infixr 1)
  xor   :: b -> b -> b
  -- |Logical implication. (infixr 1)
  (-->) :: b -> b -> b
  -- |Logical biconditional. (infixr 1)
  (<-->) :: b -> b -> b

  -- | The logical conjunction of several values.
  and :: Foldable t => t b -> b

  -- | The logical disjunction of several values.
  or :: Foldable t => t b -> b

  -- | The negated logical conjunction of several values.
  --
  -- @'nand' = 'not' . 'and'@
  nand :: Foldable t => t b -> b
  nand = not . and

  -- | The logical conjunction of the mapping of a function over several values.
  all :: Foldable t => (a -> b) -> t a -> b

  -- | The logical disjunction of the mapping of a function over several values.
  any :: Foldable t => (a -> b) -> t a -> b

  -- | The negated logical disjunction of several values.
  --
  -- @'nor' = 'not' . 'or'@
  nor :: Foldable t => t b -> b
  nor = not . or

  -- Default implementations
  true      = not false
  false     = not true
  not       = (<--> false)
  x && y = not (not x || not y)
  x || y = not (not x && not y)
  x `xor` y = (x || y) && (not (x && y))
  x --> y   = not x || y
  x <--> y  = (x && y) || not (x || y)
  and       = F.foldl' (&&) true
  or        = F.foldl' (||) false
  all p     = F.foldl' f true
    where f a b = a && p b
  any p     = F.foldl' f false
    where f a b = a || p b


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
  True  --> a = a
  False --> _ = True
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

-- | Pointwise boolean algebra.
--
instance Boolean b => Boolean (a -> b) where
  true      = const true
  false     = const false
  not p     = not . p
  p && q    = \a -> p a && q a
  p || q    = \a -> p a || q a
  p `xor` q = \a -> p a `xor` q a
  p --> q   = \a -> p a --> q a
  p <--> q  = \a -> p a <--> q a

instance (Boolean x, Boolean y) => Boolean (x, y) where
  true                = (true, true)
  false               = (false, false)
  not (a, b)          = (not a, not b)
  (a, b) && (c, d)    = (a && c, b && d)
  (a, b) || (c, d)    = (a || c, b || d)
  (a, b) `xor` (c, d) = (a `xor` c, b `xor` d)
  (a, b) --> (c, d)   = (a --> c, b --> d)
  (a, b) <--> (c, d)  = (a <--> c, b <--> d)

-- |A newtype wrapper that derives a 'Boolean' instance from any type that is both
-- a 'Bits' instance and a 'Num' instance,
-- such that boolean logic operations on the 'Bitwise' wrapper correspond to
-- bitwise logic operations on the inner type. It should be noted that 'false' is
-- defined as 'Bitwise' 0 and 'true' is defined as 'not' 'false'.
--
-- In addition, a number of other classes are automatically derived from the inner
-- type. These classes were chosen on the basis that many other 'Bits'
-- instances defined in base are also instances of these classes.
newtype Bitwise a = Bitwise {getBits :: a}
                  deriving (Num, Bits, Eq, Ord, Bounded, Enum, Show, Read, Real,
                            Integral, Typeable, Data, Ix, Storable, PrintfArg)

instance (Num a, Bits a) => Boolean (Bitwise a) where
  true   = not false
  false  = Bitwise 0
  not    = Bitwise . complement . getBits
  (&&)   = (Bitwise .) . (.&.) `on` getBits
  (||)   = (Bitwise .) . (.|.) `on` getBits
  xor    = (Bitwise .) . (Bits.xor `on` getBits)
  (<-->) = (not .) . xor
