{-# LANGUAGE MagicHash, PatternSynonyms, ViewPatterns,
   RoleAnnotations, UnboxedTuples, BangPatterns, KindSignatures,
   DeriveDataTypeable, ScopedTypeVariables #-}

module Data.Maybe.Flat (
    Maybe
  , pattern Nothing
  , pattern Just
  , maybe
  ) where
    
import Prelude hiding (Maybe(..), maybe, null)

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import GHC.Prim
import GHC.Read
import GHC.Show
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L
import Data.Data

type role Maybe nominal
newtype Maybe (a :: *) = Maybe Any deriving (Typeable)

data Null = Null

null :: Null
null = Null
{-# NOINLINE null #-}

isNothing# :: Maybe a -> Int#
isNothing# (Maybe !any) = reallyUnsafePtrEquality# (unsafeCoerce# any) null
{-# INLINE isNothing# #-}

isJust# :: Maybe a -> (# Int#, a #)
isJust# (Maybe !any) =
  (# reallyUnsafePtrEquality# (unsafeCoerce# any) null, unsafeCoerce# any #)
{-# INLINE isJust# #-}  

pattern Nothing <- (isNothing# -> 1#) where
  Nothing = (unsafeCoerce# null :: Maybe a)

pattern Just a <- (isJust# -> (# 0#, a #)) where
  Just (a :: a) = (unsafeCoerce# a :: Maybe a)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f (Just a) = f a
maybe b f _        = b

instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap f x        = unsafeCoerce# x

instance Foldable Maybe where
  foldr f z (Just a) = f a z
  foldr f z _        = z
  
  foldl f = foldr (flip f)
  
  foldMap f (Just a) = f a
  foldMap f _        = mempty

instance Traversable Maybe where
  traverse f (Just a) = Just <$> f a
  traverse f x        = pure (unsafeCoerce# x)

instance Eq a => Eq (Maybe a) where
  Just a == Just b = a == b
  _      == _      = False

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  

instance Ord a => Ord (Maybe a) where
  compare (Just a) (Just b) = compare a b
  compare Nothing  (Just _) = LT
  compare (Just _) Nothing  = GT
  compare _        _        = EQ

instance Show a => Show (Maybe a) where
    showsPrec p (Just a) s =
      (showParen (p > appPrec) $
        showString "Just " .
        showsPrec appPrec1 a) s  
    showsPrec _ _ s = showString "Nothing" s


instance Read a => Read (Maybe a) where
  readPrec =
    parens
    (do expectP (L.Ident "Nothing")
        return Nothing
     +++
     prec appPrec (
        do expectP (L.Ident "Just")
           x <- step readPrec
           return (Just x))
    )                             

instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    x       <*> _m      = unsafeCoerce# x

    Just _m1 *> m2      = m2
    x        *> _m2     = unsafeCoerce# x

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    x        >>= _      = unsafeCoerce# x

    (>>) = (*>)

    return              = Just
    fail _              = Nothing

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

instance MonadPlus Maybe

instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust _        = error "mfix Maybe: Nothing"

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr :: Constr
justConstr    = mkConstr maybeDataType "Just"    [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl f z (Just x) = z Just `f` x
  gfoldl _ z x        = z x

  toConstr (Just _) = justConstr  
  toConstr _        = nothingConstr

  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "Data.Data.gunfold(Maybe)"
  dataTypeOf _ = maybeDataType
  dataCast1 f  = gcast1 f
  
