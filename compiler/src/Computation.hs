-- |A monad that describes computations that can fail and return a list of
-- warnings.
module Computation(CompT,Result(..),warn,stringWarn,runComputation) where

import Control.Monad
import Control.Monad.Trans

newtype CompT w m a = CompT ([Warning w] -> m (Result w a))

type Comp w a = CompT w ((->)[Warning w]) a


data Result w a
  = Success [Warning w] a
  | Failure [Warning w] (Warning w)

data  Warning w
  = StringWarning String
  | CustomWarning w

instance (Show w) => Show (Warning w) where
  show (StringWarning s) = s
  show (CustomWarning w) = show w

instance (Show w,Show a) => Show (Result w a) where
  show (Success warnings val) = "Success " ++ (show warnings) ++ " " ++ show val
  show (Failure warnings err) = "Failure " ++ (show warnings) ++ " " ++ show err

instance (Monad n) => Monad (CompT w n) where
  m >>= f' =
    CompT (\ws -> let (CompT n) = m
                   in do f <- n ws
                         case f of
                           (Failure ws' w) -> return (Failure ws' w)
                           (Success ws' r) -> let (CompT n') = f' r
                                                in n' ws')
  return v = CompT (\ws -> return $ Success ws v)
  fail s   = CompT (\ws -> return $ Failure ws (StringWarning s))

instance (Monad n) => MonadPlus (CompT w n) where
  mzero = CompT (\ws -> return $ Failure ws (StringWarning "unknown error"))
  m `mplus` m2 = 
    CompT (\ws -> let (CompT n) = m
                   in do f <- n ws
                         case f of
                           (Failure ws' e) -> return (Failure ws' e)
                           (Success ws' v) -> 
                             let (CompT g) = m2 in g ws')

instance MonadTrans (CompT w) where
  lift n = CompT (\ws -> do r <- n
                            return (Success ws r))

warn:: Monad m => w -> CompT w m ()
warn w = CompT (\ws -> return $ Success (CustomWarning w:ws) ())

stringWarn:: Monad m => String -> CompT w m ()
stringWarn s = CompT (\ws -> return $ Success (StringWarning s:ws) ())

runComputation:: Monad m => CompT w m a -> m (Result w a)
runComputation (CompT f) = f []
