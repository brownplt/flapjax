{-# OPTIONS_GHC -fglasgow-exts #-}
module Misc(Symmetric,sym,
            noPos,everywhereTopDownM,topDownSingleM) where

import Control.Monad
import Text.PrettyPrint.HughesPJ(Doc)
import qualified Text.PrettyPrint.HughesPJ as Pp
import Data.Generics
import Data.Typeable
import Text.ParserCombinators.Parsec.Pos(SourcePos,initialPos)
import Data.Maybe(fromJust)


noPos:: SourcePos
noPos = initialPos ""

everywhereTopDownM:: (Data a, Monad m)
                  => (forall b . Data b => b -> m b)
                  -> a -> m a
everywhereTopDownM f a = do
  a' <- f a
  gmapM (everywhereTopDownM f) a'

-- Applies the transformer once.  Returns the transformed data structure if it
-- was applied, or Nothing if the transformer was not applied
{-onceM:: (Data a, Data b, Monad m)
     => (b -> m (Maybe b))
     -> a -> m (Maybe a)
onceM f a =
  case cast a of
    (Just b) -> do b' <- f b
                   (case b' of
                      (Just b'') -> return (cast b'') -- never be nothing
                      Nothing    -> gmapM (onceM f) a)
    Nothing -> gmapM (onceM f) a-}
                      

-- Traverse the data-structure a, top-down applying f to each applicable element
-- without recursively-descending into the elements.
--
-- For example, if f:: [a] -> m [a] and the data-structure has a node
-- [[1,2],[3,4],[5,6]], f will be applied to this list, but not to its sublists.
topDownSingleM:: (Data a, Show b, Typeable b) --, Monad m)
              => (b -> IO b)
              -> a -> IO a
topDownSingleM f a =   
  case cast a of
   Nothing  -> gmapM (topDownSingleM f) a
   (Just b) -> do b' <- f b
                  (case cast b' of
                     (Just a') -> return a'
                     -- We should be able to _prove_ that this cannot happen.
                     Nothing   -> fail "topDownSingleM: unsound!")
--}}}

--------------------------------------------------------------------------------
--{{{ Symmetries

class Symmetric a where
   -- sym x y is true if x and y are symmetric.  This is primarily used for
  -- debugging and testing.
  sym:: a -> a ->  Bool

instance Symmetric a => Symmetric [a] where
  sym []     []     = True
  sym (x:xs) (y:ys) = (sym x y) && (sym xs ys)
  sym _      _      = False

-- In general, we don't care about source locations while testing.
instance Symmetric SourcePos where
  sym _ _ = True

--}}}
