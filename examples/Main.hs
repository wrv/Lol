{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, DataKinds, TypeOperators, 
             KindSignatures, TypeFamilies, UndecidableInstances, 
             FlexibleInstances, MultiParamTypeClasses, PackageImports,
             FlexibleContexts, ScopedTypeVariables, RankNTypes, PolyKinds,
             StandaloneDeriving, DeriveDataTypeable, ConstraintKinds, TypeSynonymInstances #-}

import Crypto.Lol.Compiler.AST hiding (Sub)
import Crypto.Lol.Compiler.CT
import Crypto.Lol.Compiler.CTDummy
import Crypto.Lol.Compiler.CTCompiler
import Language.Syntactic.Sugar.BindingTyped () -- need for instance of Syntactic (a->b) when using `share` below
import Language.Syntactic hiding (size)
import Language.Syntactic.Functional (lamTyped, BindingT)

import Crypto.Lol as Lol hiding ((**))
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Applications.SymmSHE hiding (CT)
import qualified Crypto.Lol.Applications.SymmSHE as SHE

import DRBG
import Types

import Data.Type.List
import Data.Typeable

import Control.Monad.Random
import "crypto-api" Crypto.Random
import Crypto.Random.DRBG

import Data.Foldable
import Data.Map as M hiding (replicate, size, map, toList, (\\))

-- don't turn on -fbreak-on-exception unless you want to trace because it makes the error messages much worse!

type CTDOM = Typed (ADDITIVE :+: RING :+: CTOps :+: CTDummyOps :+: Literal :+: Let :+: BindingT)
type CTExpr a = ASTF CTDOM a

main :: IO ()
main = do
  print "CT:"

  tunnelTest (Proxy::Proxy CT)
  prfTest (Proxy::Proxy CT)
  
  print "RT:"
  tunnelTest (Proxy::Proxy RT)
  prfTest (Proxy::Proxy RT)

  print "Done"

-- This function serves the dual purpose of specifying the random generator
-- and keeping all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function, but it would be hard
-- to get timing any other way.
evalCryptoRandIO :: Rand (CryptoRand HashDRBG) a -> IO a
evalCryptoRandIO x = do
  gen <- newGenIO -- uses system entropy
  return $ evalRand x gen

tunnelTest pc@(_::Proxy t) = do -- in IO
    let v = 0.1 :: Double
    x'<- getRandom

    ast0 <- time "Computing AST: " $ lamTyped $ tunnelAST pc

    (ast1, idMap) <- time "Generating keys: " =<< 
      evalCryptoRandIO (genKeys v ast0)

    let keyMap = M.fromList $ elems idMap

    ast2 <- time "Generating hints: " =<< 
      evalCryptoRandIO (genHints keyMap ast1)

    (x1,encsk) <- time "Encrypting input: " =<< 
      evalCryptoRandIO (encryptInput idMap x')

    putStrLn $ "Input noise level:\t" ++ (show $ errRatio x1 encsk)

    let decsk = getDecryptKey keyMap ast2

    ans <- time "Evaluating AST: " $ eval ast2 x1

    putStrLn $ "Output noise level:\t" ++ (show $ errRatio ans decsk)

    putStrLn "Done!"

errRatio :: forall m zp t m' zq z . 
  (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, Mod zq, Absolute (LiftOf zq),
   Ord (LiftOf zq), ToInteger (LiftOf zq))
  => SHE.CT m zp (Cyc t m' zq) -> SK (Cyc t m' z) -> Double
errRatio ct sk = 
  (fromIntegral $ maximum $ fmap abs $ unsafeUnCyc $ errorTermUnrestricted sk ct) / 
  (fromIntegral $ proxy modulus (Proxy::Proxy zq))

tunnelAST (_::Proxy t) (x :: CTExpr (SHE.CT H0 ZP2 (Cyc t H0' ZQ5))) =
  roundCTDown $
  --tunnHelper (Proxy::Proxy '(H5,H5',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H4,H4',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H3,H3',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H2,H2',ZQ6)) $
  tunnHelper (Proxy::Proxy '(H1,H1')) $ 
  roundCTUp x

prfTest pc@(_::Proxy t) = do
  -- public multiplier
  c :: Cyc t H0 ZP8 <- getRandom
  let v = 0.1 :: Double
  x' <- getRandom

  ast0 <- time "Computing AST: " $ lamTyped $ homomPRF c

  (ast1, idMap) <- time "Generating keys: " =<< 
    evalCryptoRandIO (genKeys v ast0)

  ast2 <- time "Generating hints: " =<< 
    evalCryptoRandIO (genHints (M.fromList $ elems idMap) ast1)

  (x,_) <- time "Encrypting input: " =<< 
    evalCryptoRandIO (encryptInput idMap x')

  _ <- time "Evaluating AST: " $ eval ast2 x
  
  putStrLn "Done!"

homomPRF c (x :: CTExpr (SHE.CT H0 ZP8 (Cyc t H0' ZQ4))) = 
  let scaledX = mulPublicCT c x
      hopX = roundCTDown $  -- after hopping, round down to ZQ3
             roundCTDown $  -- return to input CT modulus
             tunnHelper (Proxy::Proxy '(H5,H5')) $
             tunnHelper (Proxy::Proxy '(H4,H4')) $
             tunnHelper (Proxy::Proxy '(H3,H3')) $
             tunnHelper (Proxy::Proxy '(H2,H2')) $
             tunnHelper (Proxy::Proxy '(H1,H1')) $ 
             roundCTUp scaledX -- scale up modulus for applying hints
  in share hopX $ \y ->
    let z = y ** (addPublicCT one y)          -- x*(x+1)
        w = roundPTDown z                     -- round PT to ZP4
    in share w $ \w' ->                       -- w/w' has PT ZP4 and CT ZQ2
      let u = w' ** (addPublicCT (-one) w')   -- u = w*(w-1)
      in roundPTDown u                        -- round the PT to ZP2 and return

-- type restricted helpers

a ** b =
  let c = a*b
      c' = ksqHelper c
  in roundCTDown c'

ksqHelper :: forall m zp c m' zq z . (KSDummyCtx z TrivGad (ZQUp zq) m zp c m' zq)
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' zq))
ksqHelper u = proxy (ksqDummy u) (Proxy::Proxy '(TrivGad, ZQUp zq))

tunnHelper :: forall dom dom' gad c r r' s s' e e' z zp zq .
  (ASTTunnelCtx dom dom' gad c r r' s s' e e' z zp zq,
   gad ~ TrivGad, dom ~ CTDOM)
  => Proxy '(s,s') -> CTExpr (SHE.CT r zp (Cyc c r' zq)) -> CTExpr (SHE.CT s zp (Cyc c s' zq))
tunnHelper _ x = proxy (tunnDummy x) (Proxy::Proxy gad)

roundCTUp :: (ASTRoundCTCtx CTDOM dom' c m m' zp zq (ZQUp zq)) 
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' (ZQUp zq)))
roundCTUp x = roundCT x

roundCTDown :: (ASTRoundCTCtx CTDOM dom' c m m' zp zq (ZQDown zq))
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' (ZQDown zq)))
roundCTDown x = roundCT x

roundPTDown :: (ASTRoundPTCtx CTDOM dom' c m m' zp (ZPDiv2 zp) zq) 
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m (ZPDiv2 zp) (Cyc c m' zq))
roundPTDown x = roundPT x

type ZPDiv2 zp = NextListElt zp '[ZP8, ZP4, ZP2]
type ZQSeq = '[ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]
type ZQUp zq = PrevListElt zq ZQSeq
type ZQDown zq = NextListElt zq ZQSeq

type family ZQRoundDown zq where
  ZQRoundDown ZQ4 = ZQ3
  ZQRoundDown ZQ3 = ZQ2
  ZQRoundDown ZQ2 = ZQ1

type family NextListElt x xs where
  NextListElt x (x ': y ': ys) = y
  NextListElt x (y ': ys) = NextListElt x ys

type PrevListElt x xs = NextListElt x (Reverse xs)
