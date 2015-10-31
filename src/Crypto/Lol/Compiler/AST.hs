{-# LANGUAGE NoImplicitPrelude, GADTs, TypeOperators, AllowAmbiguousTypes, KindSignatures,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
             DataKinds, TypeFamilies, RankNTypes, ConstraintKinds,
             ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}

-- | This module contains several application-independent AST operations 

module Crypto.Lol.Compiler.AST 
(eval
,Literal(..)
,ADDITIVE(..)
,RING(..)
,Let(..)
,constant
,share
,bottomUpMapM
) where

import Algebra.Additive as Additive (C)
import Algebra.Ring as Ring (C)

import Control.DeepSeq
import Control.Monad.State

import Crypto.Lol.LatticePrelude hiding ((!!), lookup, lift)

import Data.Syntactic
import Data.Syntactic.Functional
import Data.Tree
import Data.Typeable

-- Some generic AST functions
-- useful default instances
instance (Render a) => Equality a where
  equal = equalDefault
  hash = hashDefault

-- | evaluates an AST to its internal representation
eval :: (Syntactic a, EvalEnv (Domain a) RunEnv) => a -> Internal a
eval = evalClosed . desugar

-- | deep embedding for 
data Literal a where
  IntLit :: (ToInteger i, Show i, Ring a, NFData i) => i -> Literal (Full a)
  Const :: (Show a, NFData a) => a -> Literal (Full a)
  deriving (Typeable)

instance StringTree Literal -- uses default implementation
instance EvalEnv Literal env

instance Symbol Literal where
  rnfSym (IntLit i) = rnf i
  rnfSym (Const a) = rnf a
  symSig (IntLit _) = signature
  symSig (Const _) = signature

instance Render Literal where
  renderSym (IntLit i) = show i
  renderSym (Const c) = show c

instance Eval Literal where
  evalSym (IntLit i) = fromIntegral i
  evalSym (Const a) = a

-- | shallow embedding for Const
constant :: (Literal :<: Domain a, Syntactic a, 
             Show (Internal a), NFData (Internal a)) 
         => Internal a -> a
constant = sugarSym . Const

-- | deep embedding for Additive operations
data ADDITIVE a where
  Add :: (Additive a) => ADDITIVE (a :-> a :-> Full a)
  Sub :: (Additive a) => ADDITIVE (a :-> a :-> Full a)
  deriving (Typeable)

instance StringTree ADDITIVE
instance EvalEnv ADDITIVE env

instance Symbol ADDITIVE where
  -- rnf is default
  symSig Add = signature
  symSig Sub = signature

instance Render ADDITIVE where
  renderSym Add = "(+)"
  renderSym Sub = "(-)"

instance Eval ADDITIVE where
  evalSym Add = (+)
  evalSym Sub = (-)

instance (Literal :<: dom, Ring a, ADDITIVE :<: dom, Show  a) 
  => Additive.C (AST dom (Full a)) where
  zero = sugarSym $ IntLit (zero :: Integer)
  (+) = sugarSym Add
  (-)= sugarSym Sub

-- | deep embedding for ring multiplication
data RING :: (* -> *) where
  Mul :: (Ring a) => RING (a :-> a :-> Full a)
  deriving (Typeable)

instance StringTree RING
instance EvalEnv RING env

instance Symbol RING where
  symSig Mul = signature

instance Render RING where
  renderSym Mul = "(*)"

instance Eval RING where
  evalSym Mul = (*)

instance (Literal :<: dom, RING :<: dom,
          ADDITIVE :<: dom, Ring a, Show a) 
  => Ring.C (AST dom (Full a)) where
    one = sugarSym $ IntLit (one :: Integer)
    (*) = sugarSym Mul
    fromInteger = sugarSym . IntLit

-- | deep embedding for sharing computation in an AST
data Let :: (* -> *) where
  Let :: (Typeable a) => Let (a :-> (a -> b) :-> Full b)
  deriving (Typeable)

instance Render Let where
  renderSym Let = "letBind"

instance StringTree Let where
  stringTreeSym [a, Node lam [body]] Let
    | ("Lam",v) <- splitAt 3 lam = Node ("Let" ++ v) [a,body]
  stringTreeSym [a,f] Let = Node "Let" [a,f]

instance Eval Let where
  evalSym Let = flip ($)

instance EvalEnv Let env

instance Symbol Let where
  symSig Let = signature

-- | shallow embedding for sharing computation in an AST
share :: (Let :<: sup, sup ~ Domain (a -> b), 
          sup ~ Domain b, sup ~ Domain a, 
          Internal (a -> b) ~ (Internal a -> Internal b),
          Syntactic a, Syntactic b,
          Syntactic (a -> b), Typeable (Internal a),
          SyntacticN (a -> (a -> b) -> b) fi) -- requires AllowAmbiguousTypes
      => a -> (a -> b) -> b
share = sugarSym Let

-- | useful traversals and helper functions
bottomUpMapM :: forall sym sym' a m .
  (Monad m) => ( forall sig . 
                  sym sig -> Args (AST sym') sig -> m (sym' sig)
               )
               -> ASTF sym a
               -> m (ASTF sym' a)
bottomUpMapM f b = go b Nil
  where
    go :: (a ~ DenResult sig) => AST sym sig -> Args (AST sym) sig -> m (ASTF sym' a)
    go (Sym a)  as = do
      args' <- mapArgsM (bottomUpMapM f) as
      sym' <- f a args'
      return $ appArgs (Sym sym') args'
    go (s :$ a) as = go s (a :* as)
