-- |A convenient set of useful conditional operators.
module Control.Cond 
       ( -- * Simple conditional operators
         if', (??), bool
         -- * Higher-order conditional
       , select
         -- * Lisp-style conditional operators 
       , cond, condPlus
         -- * Lifted conditional and boolean operators
       , ifM, (<||>), (<&&>), notM, condM, condPlusM
         -- * Lifted guard and when
       , guardM, whenM, unlessM 
       ) where
import Control.Monad ( MonadPlus (mzero), guard, liftM )

infixr 1 ??
infixr 2 <||>
infixr 3 <&&>

-- |A simple conditional function.
if' :: Bool -> a -> a -> a
if' p a b = if p then a else b
{-# INLINE if' #-}

-- |'if'' with the 'Bool' argument at the end (infixr 1).
(??) :: a -> a -> Bool -> a
(??) a b p = if' p a b 
{-# INLINE (??) #-}

-- |A catamorphism for the Bool type. This is analogous to foldr, maybe, and 
-- either. The first argument is the false case, the second argument is the 
-- true case, and the last argument is the predicate value.
bool :: a -> a -> Bool -> a
bool b a p = if' p a b
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
condPlus :: MonadPlus m => [(Bool, a)] -> m a
condPlus [] = mzero
condPlus ((p,v):ls) = if' p (return v) (condPlus ls)

-- |Composes a predicate function and 2 functions into a single
-- function. The first function is called when the predicate yields True, the
-- second when the predicate yields False.
select :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
select p a b x = if' (p x) (a x) (b x)
{-# INLINE select #-}

-- |'if'' lifted to 'Monad'. Unlike 'liftM3' 'if'', this is  
-- short-circuiting in the monad, such that only the predicate action and one of
-- the remaining argument actions are executed.
ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM p a b = p >>= bool b a
{-# INLINE ifM #-}

-- |Lifted boolean or. Unlike 'liftM2' ('||'), This function is short-circuiting
-- in the monad. Fixity is the same as '||' (infixr 2).
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) a b = ifM a (return True) b
{-# INLINE (<||>) #-}

-- |Lifted boolean and. Unlike 'liftM2' ('&&'), this function is 
-- short-circuiting in the monad. Fixity is the same as '&&' (infxr 3).
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) a b = ifM a b (return False)
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