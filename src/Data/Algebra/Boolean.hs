{-# LANGUAGE
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      DeriveDataTypeable,
      StandaloneDeriving,
      DerivingStrategies
  #-}
module Data.Algebra.Boolean(
  Boolean(..),
  fromBool,
  Bitwise(..),
  and,
  or,
  nand,
  nor,
  any,
  all,
  AnyB(..),
  AllB(..),
  XorB(..),
  EquivB(..),
  ) where
import Data.Monoid (Any(..), All(..), Dual(..), Endo(..))
import Data.Bits (Bits, complement, (.|.), (.&.))
import qualified Data.Bits as Bits
import Data.Function (on)
import Data.Semigroup (stimes, stimesIdempotentMonoid)
import Data.Ord (Down(..))
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
-- all the laws of [boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra_(structure)).
--
-- Minimal complete definition: 'true' or 'false', 'not' or ('<-->', 'false'), '||' or '&&'.
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

  {-# MINIMAL (false | true), (not | ((<-->), false)), ((||) | (&&)) #-}

  -- Default implementations
  true      = not false
  false     = not true
  not       = (<--> false)
  x && y    = not (not x || not y)
  x || y    = not (not x && not y)
  x `xor` y = (x || y) && (not (x && y))
  x --> y   = not x || y
  x <--> y  = (x && y) || not (x || y)


-- | The logical conjunction of several values.
and :: (Boolean b, Foldable t) => t b -> b
and = F.foldl' (&&) true

-- | The logical disjunction of several values.
or :: (Boolean b, Foldable t) => t b -> b
or = F.foldl' (||) false

-- | The negated logical conjunction of several values.
--
-- @'nand' = 'not' . 'and'@
nand :: (Boolean b, Foldable t) => t b -> b
nand = not . and

-- | The negated logical disjunction of several values.
--
-- @'nor' = 'not' . 'or'@
nor :: (Boolean b, Foldable t) => t b -> b
nor = not . or

-- | The logical conjunction of the mapping of a function over several values.
all :: (Boolean b, Foldable t) => (a -> b) -> t a -> b
all p = F.foldl' f true
  where f a b = a && p b

-- | The logical disjunction of the mapping of a function over several values.
any :: (Boolean b, Foldable t) => (a -> b) -> t a -> b
any p     = F.foldl' f false
  where f a b = a || p b


-- | A boolean algebra regarded as a monoid under disjunction
newtype AnyB b = AnyB {
  getAnyB :: b
} deriving (Eq, Ord, Show)

instance Boolean b => Semigroup (AnyB b) where
  AnyB x <> AnyB y = AnyB (x || y)
  stimes = stimesIdempotentMonoid

instance Boolean b => Monoid (AnyB b) where
  mempty = AnyB false


-- | A boolean algebra regarded as a monoid under conjunction
newtype AllB b = AllB {
  getAllB :: b
} deriving (Eq, Ord, Show)

instance Boolean b => Semigroup (AllB b) where
  AllB x <> AllB y = AllB (x && y)
  stimes = stimesIdempotentMonoid

instance Boolean b => Monoid (AllB b) where
  mempty = AllB true


stimesPeriod2 :: (Monoid a, Integral n) => n -> a -> a
stimesPeriod2 n x = case n `mod` 2 of
  1 -> x
  _ -> mempty

-- | A boolean algebra regarded as a monoid under exclusive or
newtype XorB b = XorB {
  getXorB :: b
} deriving (Eq, Ord, Show)

instance Boolean b => Semigroup (XorB b) where
  XorB x <> XorB y = XorB (x `xor` y)
  stimes = stimesPeriod2

instance Boolean b => Monoid (XorB b) where
  mempty = XorB false


-- | A boolean algebra regarded as a monoid under equivalence
newtype EquivB b = EquivB {
  getEquivB :: b
}  deriving (Eq, Ord, Show)

instance Boolean b => Semigroup (EquivB b) where
  EquivB x <> EquivB y = EquivB (x <--> y)
  stimes = stimesPeriod2

instance Boolean b => Monoid (EquivB b) where
  mempty = EquivB true


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

deriving newtype instance Boolean Any

deriving newtype instance Boolean All

deriving newtype instance Boolean a => Boolean (Dual a)

-- | Opposite boolean algebra: exchanges true and false, and `and` and
-- `or`, etc
instance Boolean a => Boolean (Down a) where
  true = Down false
  false = Down true
  not = Down . not . getDown
  (&&) = (Down .) . (||) `on` getDown
  (||) = (Down .) . (&&) `on` getDown
  xor = (Down .) . (<-->) `on` getDown
  (<-->) = (Down .) . xor `on` getDown

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

deriving newtype instance Boolean a => Boolean (Endo a)

-- |The trivial boolean algebra
instance Boolean () where
  true = ()
  false = ()
  not _ = ()
  _ && _ = ()
  _ || _ = ()
  _ --> _ = ()
  _ <--> _ = ()

instance (Boolean x, Boolean y) => Boolean (x, y) where
  true                = (true, true)
  false               = (false, false)
  not (a, b)          = (not a, not b)
  (a, b) && (c, d)    = (a && c, b && d)
  (a, b) || (c, d)    = (a || c, b || d)
  (a, b) `xor` (c, d) = (a `xor` c, b `xor` d)
  (a, b) --> (c, d)   = (a --> c, b --> d)
  (a, b) <--> (c, d)  = (a <--> c, b <--> d)

instance (Boolean x, Boolean y, Boolean z) => Boolean (x, y, z) where
  true                      = (true, true, true)
  false                     = (false, false, false)
  not (a, b, c)             = (not a, not b, not c)
  (a, b, c) && (d, e, f)    = (a && d, b && e, c && f)
  (a, b, c) || (d, e, f)    = (a || d, b || e, c || f)
  (a, b, c) `xor` (d, e, f) = (a `xor` d, b `xor` e, c `xor` f)
  (a, b, c) --> (d, e, f)   = (a --> d, b --> e, c --> f)
  (a, b, c) <--> (d, e, f)  = (a <--> d, b <--> e, c <--> f)


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
