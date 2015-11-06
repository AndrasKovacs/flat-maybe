{-# LANGUAGE MagicHash, PatternSynonyms, ViewPatterns,
   RoleAnnotations, UnboxedTuples, BangPatterns, KindSignatures,
   DeriveDataTypeable, ScopedTypeVariables #-}

module Data.Maybe.Flat (
    Maybe
  , pattern Nothing
  , pattern Just
  , maybe
  , isJust
  , isNothing ) where
    
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
maybe b f Nothing  = b

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap f Nothing  = Nothing

instance Foldable Maybe where
  foldr f z (Just a) = f a z
  foldr f z Nothing  = z
  
  foldl f = foldr (flip f)
  
  foldMap f (Just a) = f a
  foldMap f Nothing  = mempty

instance Traversable Maybe where
  traverse f (Just a) = Just <$> f a
  traverse f Nothing  = pure Nothing

instance Eq a => Eq (Maybe a) where
  Just a == Just b = a == b
  _      == _      = False

instance Ord a => Ord (Maybe a) where
  compare (Just a) (Just b) = compare a b
  compare Nothing  (Just _) = LT
  compare (Just _) Nothing  = GT
  compare _        _        = EQ

instance Show a => Show (Maybe a) where
    showsPrec _ Nothing  s = showString "Nothing" s
    showsPrec p (Just a) s = (showParen (p > appPrec) $
                             showString "Just " .
                             showsPrec appPrec1 a) s

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
    Nothing <*> _m      = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

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
                   unJust Nothing  = error "mfix Maybe: Nothing"

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr :: Constr
justConstr    = mkConstr maybeDataType "Just"    [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl _ z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "Data.Data.gunfold(Maybe)"
  dataTypeOf _ = maybeDataType
  dataCast1 f  = gcast1 f
  
