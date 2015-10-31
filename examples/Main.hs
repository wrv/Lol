{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, DataKinds, TypeOperators, 
             KindSignatures, TypeFamilies, UndecidableInstances, 
             FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, ScopedTypeVariables, RankNTypes, PolyKinds,
             StandaloneDeriving, DeriveDataTypeable, ConstraintKinds #-}

import Crypto.Lol.Compiler.AST hiding (Sub)
import Crypto.Lol.Compiler.CT
import Crypto.Lol.Compiler.CTDummy
import Crypto.Lol.Compiler.CTCompiler
import Data.Syntactic.Sugar.BindingT () -- need for instance of Syntactic (a->b) when using `share` below
import Data.Syntactic hiding (size)
import Data.Syntactic.Functional (lamT, BindingT)

import Crypto.Lol as Lol
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Applications.SymmSHE hiding (CT)
import qualified Crypto.Lol.Applications.SymmSHE as SHE

import DRBG
import Types

import Control.Monad.Random
import Crypto.Random
import Crypto.Random.DRBG

import Data.Foldable
import Data.Map as M hiding (replicate, size, map, toList, (\\))

-- don't turn on -fbreak-on-exception unless you want to trace because it makes the error messages much worse!

type CTDOM = (ADDITIVE :+: RING :+: CTOps :+: CTDummyOps :+: Literal :+: Let :+: BindingT)
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

    ast0 <- time "Computing AST: " $ lamT $ tunnelAST pc

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
  --tunnHelper (Proxy::Proxy '(H5,H5',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H4,H4',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H3,H3',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H2,H2',ZQ6)) $
  tunnHelper (Proxy::Proxy '(H1,H1',ZQ6)) x

prfTest pc@(_::Proxy t) = do
  -- public multiplier
  c :: Cyc t H0 ZP8 <- getRandom
  let v = 0.1 :: Double
  x' <- getRandom

  ast0 <- time "Computing AST: " $ lamT $ homomPRF pc c

  (ast1, idMap) <- time "Generating keys: " =<< 
    evalCryptoRandIO (genKeys v ast0)

  ast2 <- time "Generating hints: " =<< 
    evalCryptoRandIO (genHints (M.fromList $ elems idMap) ast1)

  (x,_) <- time "Encrypting input: " =<< 
    evalCryptoRandIO (encryptInput idMap x')

  _ <- time "Evaluating AST: " $ eval ast2 x
  
  putStrLn "Done!"

homomPRF (_::Proxy t) c (x :: CTExpr (SHE.CT H0 ZP8 (Cyc t H0' ZQ4))) = 
  let scaledX = mulPublicCT c x
      hopX = tunnHelper (Proxy::Proxy '(H5,H5',ZQ5)) $
             tunnHelper (Proxy::Proxy '(H4,H4',ZQ5)) $
             tunnHelper (Proxy::Proxy '(H3,H3',ZQ5)) $
             tunnHelper (Proxy::Proxy '(H2,H2',ZQ5)) $
             tunnHelper (Proxy::Proxy '(H1,H1',ZQ5)) $ scaledX
  in share hopX $ \y -> 
    let z0 = roundCTHelper (Proxy::Proxy ZQ3) y                    -- after hopping, round down to ZQ3
        z1 = z0 * (addPublicCT one z0)                             -- x*(x+1)
        z2 = proxy (ksqDummy z1) (Proxy::Proxy '(TrivGad, ZQ4))    -- key switch z1 after product using bonus modulus ZQ4
        z3 = roundCTHelper (Proxy::Proxy ZQ2) z2                   -- mod switch z2 after product to ZQ2
        w = roundPTHelper (Proxy::Proxy ZP4) z3                    -- round PT to ZP4
    in share w $ \w' ->                                            -- w/w' has PT ZP4 and CT ZQ2
      let u = (w' * (addPublicCT (-one) w'))                       -- u = w*(w-1)
          u' = roundCTHelper (Proxy::Proxy ZQ1) $ 
                 proxy (ksqDummy u) (Proxy::Proxy '(TrivGad, ZQ3)) -- key switch with bonus modulus ZQ3, the mod switch to ZQ1
      in roundPTHelper (Proxy::Proxy ZP2) u'                       -- round the PT to ZP2 and return


-- type restricted helpers

tunnHelper :: forall dom gad c r r' s s' e e' z zp zq zq' .
  (ASTTunnelCtx dom gad c r r' s s' e e' z zp zq zq',
   gad ~ TrivGad, dom ~ CTDOM)
  => Proxy '(s,s',zq') -> CTExpr (SHE.CT r zp (Cyc c r' zq)) -> CTExpr (SHE.CT s zp (Cyc c s' zq))
tunnHelper _ x = proxy (tunnDummy x) (Proxy::Proxy '(gad, zq'))

roundCTHelper :: (ASTRoundCTCtx CTDOM c m m' zp zq zq') 
  => Proxy zq' -> CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' zq'))
roundCTHelper _ x = roundCT x

roundPTHelper :: (ASTRoundPTCtx CTDOM c m m' zp zp' zq) 
  => Proxy zp' -> CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp' (Cyc c m' zq))
roundPTHelper _ x = roundPT x
