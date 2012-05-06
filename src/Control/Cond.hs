-- |A convenient set of useful conditional operators.
module Control.Cond 
       ( -- * Simple conditional operators
         if', (??), bool
         -- * Lisp-style conditional operators 
       , cond, condPlus
         -- * Conditional operator on categories
       , (?.)
         -- * Conditional operator on monoids
       , (?<>)
         -- * Conditional operator on functions
       , select
         -- * C-style ternary conditional
       , (?)
         -- *Hoare's conditional choice operator
         -- |The following operators form a ternary conditional of the form
         -- 
         -- > t <| p |> f
         --
         -- These operators chain with right associative fixity. This allows 
         -- chaining of conditions, where the result of the expression is 
         -- the value to the left of the first predicate that succeeds.
         -- 
         -- For more information see 
         -- <http://zenzike.com/posts/2011-08-01-the-conditional-choice-operator>
       , (|>), (<|)
         -- **Unicode variants
         -- |Intended to resemble the notation used in Tony Hoare's 
         -- Unified Theories of Programming.
       , (⊳), (⊲)
         -- * Lifted conditional and boolean operators
       , ifM, (<||>), (<&&>), notM, condM, condPlusM
       , guardM, whenM, unlessM 
       ) where

import Control.Monad
import Control.Category 
import Data.Monoid
import Prelude hiding ((.), id)

infixr  0 <|, |>, ⊳, ⊲, ?
infixr  1 ??
infixr  2 <||>
infixr  3 <&&>
infixr  7 ?<>
infixr  9 ?. 

-- |A simple conditional function.
if' :: Bool -> a -> a -> a
if' p t f = if p then t else f
{-# INLINE if' #-}

-- |'if'' with the 'Bool' argument at the end (infixr 1).
(??) :: a -> a -> Bool -> a
(??) t f p = if' p t f 
{-# INLINE (??) #-}

-- |A catamorphism (aka fold) for the Bool type. This is analogous to 
-- 'foldr', 'Data.Maybe.maybe', and 'Data.Either.either'. The first argument is 
-- the false case, the second argument is the true case, and the last argument 
-- is the predicate value.
bool :: a -> a -> Bool -> a
bool f t p = if' p t f
{-# INLINE bool #-}

-- |Lisp-style conditionals. If no conditions match, then a runtime exception
-- is thrown. Here's a trivial example:
--
-- @
--   signum x = cond [(x > 0     , 1 )
--                   ,(x < 0     , -1)
--                   ,(otherwise , 0 )]
-- @
cond :: [(Bool, a)] -> a
cond [] = error "cond: no matching conditions"
cond ((p,v):ls) = if' p v (cond ls)

-- |Lisp-style conditionals generalized over 'MonadPlus'. If no conditions
-- match, then the result is 'mzero'. This is a safer variant of 'cond'.
--
-- Here's a highly contrived example using 'Data.Maybe.fromMaybe': 
--
-- @
--   signum x = fromMaybe 0 . condPlus $ [(x > 0, 1 ) 
--                                       ,(x < 0, -1)]
-- @
--
-- Alternatively, you could use the '<|' operator from Hoare's ternary
-- conditional choice operator, like so:
--
-- @
--   signum x = 0 \<| condPlus [(x > 0, 1 ) 
--                            ,(x < 0, -1)]
-- @
condPlus :: MonadPlus m => [(Bool, a)] -> m a
condPlus [] = mzero
condPlus ((p,v):ls) = if' p (return v) (condPlus ls)

-- |Conditional composition. If the predicate is False, 'id' is returned
-- instead of the second argument. This function, for example, can be used to 
-- conditionally add functions to a composition chain.
(?.) :: Category cat => Bool -> cat a a -> cat a a
p ?. c = if' p c id
{-# INLINE (?.) #-}

-- |Composes a predicate function and 2 functions into a single
-- function. The first function is called when the predicate yields True, the
-- second when the predicate yields False.
--
-- Note that after importing "Control.Monad.Instances", 'select' becomes a  
-- special case of 'ifM'.
select :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
select p t f x = if' (p x) (t x) (f x)
{-# INLINE select #-}

-- |'if'' lifted to 'Monad'. Unlike 'liftM3' 'if'', this is  
-- short-circuiting in the monad, such that only the predicate action and one of
-- the remaining argument actions are executed.
ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM p t f = p >>= bool f t
{-# INLINE ifM #-}

-- |Lifted boolean or. Unlike 'liftM2' ('||'), This function is short-circuiting
-- in the monad. Fixity is the same as '||' (infixr 2).
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) t f = ifM t (return True) f
{-# INLINE (<||>) #-}

-- |Lifted boolean and. Unlike 'liftM2' ('&&'), this function is 
-- short-circuiting in the monad. Fixity is the same as '&&' (infxr 3).
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) t f = ifM t f (return False)
{-# INLINE (<&&>) #-}

-- |Lifted boolean negation.
notM :: Monad m => m Bool -> m Bool
notM = liftM not
{-# INLINE notM #-}

-- |'cond' lifted to 'Monad'. If no conditions match, a runtime exception
-- is thrown.
condM :: Monad m => [(m Bool, m a)] -> m a 
condM [] = error "condM: no matching conditions"
condM ((p, v):ls) = ifM p v (condM ls)

-- |'condPlus' lifted to 'Monad'. If no conditions match, then 'mzero'
-- is returned.
condPlusM :: MonadPlus m => [(m Bool, m a)] -> m a
condPlusM [] = mzero
condPlusM ((p, v):ls) = ifM p v (condPlusM ls)

-- |a variant of 'Control.Monad.when' with a monadic predicate.
whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = ifM p m (return ())
{-# INLINE whenM #-}

-- |a variant of 'Control.Monad.unless' with a monadic predicate.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = ifM (notM p) m (return ())
{-# INLINE unlessM #-}

-- |a variant of 'Control.Monad.guard' with a monadic predicate.
guardM :: MonadPlus m => m Bool -> m ()
guardM = (guard =<<)
{-# INLINE guardM #-}

-- |Conditional monoid operator. If the predicate is 'False', the second
-- argument is replaced with 'mempty'. The fixity of this operator is one
-- level higher than 'Control.Monoid.<>'. 
--
-- It can also be used to chain multiple predicates together, like this: 
--
-- > even (length ls) ?<> not (null ls) ?<> ls
(?<>) :: Monoid a => Bool -> a -> a
p ?<> m = if' p m mempty
{-# INLINE (?<>) #-}
 

-- |An operator that allows you to write C-style ternary conditionals of
-- the form:
--
-- > p ? t ?? f
--
-- Note that parentheses are required in order to chain sequences of
-- conditionals together. This is probably a good thing.
(?) :: Bool -> (Bool -> a) -> a
p ? f = f p

-- |right bracket of the conditional choice operator. If the predicate
-- is 'False', returns 'Nothing', otherwise it returns 'Just' the right-hand
-- argument.
(|>) :: Bool -> a -> Maybe a
True  |> _ = Nothing
False |> f = Just f

-- |left bracket of the conditional choice operator. This is equivalent to
-- 'Data.Maybe.fromMaybe'
(<|) :: a -> Maybe a -> a
t <| Nothing = t
_ <| Just f  = f

-- |Unicode rebinding of '|>'. 
(⊲) :: a -> Maybe a -> a
(⊲) = (<|)

-- |Unicode rebinding of '<|'.
(⊳) :: Bool -> a -> Maybe a
(⊳) = (|>)