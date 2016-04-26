{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, PartialTypeSignatures,
             RebindableSyntax, RecordWildCards, ScopedTypeVariables,
             TypeFamilies #-}

module Generate (generateMain, ChallengeParams(..)) where

import           Beacon
import           Common
import           Crypto.Lol.RLWE.Continuous               as C
import           Crypto.Lol.RLWE.Discrete                 as D
import           Crypto.Lol.RLWE.RLWR                     as R

import           Crypto.Challenges.RLWE.Proto.RLWE.Challenge
import qualified Crypto.Challenges.RLWE.Proto.RLWE.ChallengeType as T
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR
import           Crypto.Challenges.RLWE.Proto.RLWE.SampleCont
import           Crypto.Challenges.RLWE.Proto.RLWE.SampleDisc
import           Crypto.Challenges.RLWE.Proto.RLWE.SampleRLWR
import           Crypto.Challenges.RLWE.Proto.RLWE.Secret        as S

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random

import Crypto.Lol                    hiding (RRq, lift)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Lol.Kq
import Crypto.Lol.Types.Proto.Lol.Rq
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.ByteString.Lazy as BS (writeFile)
import Data.Reflection      hiding (D)

import System.Console.ANSI
import System.Directory (createDirectoryIfMissing)

import Text.ProtocolBuffers        (messagePut)
import Text.ProtocolBuffers.Header

-- | Information to generate a challenge.
data ChallengeParams =
    Cont {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64,
          svar::Double, eps::Double}
  | Disc {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64,
          svar::Double, eps::Double}
  | RLWR {numSamples::Int, numInsts::InstanceID, m::Int32, q::Int64, p::Int64}
  deriving (Show)

-- Tensor type used to generate instances
type T = CT

-- | Generate and serialize challenges given the path to the root of the tree
-- and an initial beacon address.
generateMain :: FilePath -> BeaconAddr -> [ChallengeParams] -> IO ()
generateMain path beaconStart cps = do
  let len = length cps
      challIDs = take len [0..]
      beaconAddrs = take len $ iterate nextBeaconAddr beaconStart
  evalCryptoRandIO (sequence_ $
    zipWith3 (genAndWriteChallenge path) cps challIDs beaconAddrs
    :: RandT (CryptoRand HashDRBG) IO ())

genAndWriteChallenge :: (MonadRandom m, MonadIO m)
  => FilePath -> ChallengeParams -> ChallengeID -> BeaconAddr -> m ()
genAndWriteChallenge path cp challID ba@(BA t _) = do
  let name = challengeName cp
  liftIO $ putStrLn $ "Generating challenge " ++ name
  isAvail <- isBeaconAvailable t
  when isAvail $ do
    liftIO $ setSGR [SetColor Foreground Vivid Red]
    liftIO $ putStrLn $ "Beacon is already available!"
    liftIO $ setSGR [SetColor Foreground Vivid Black]
  chall <- genChallengeU cp challID ba
  liftIO $ writeChallengeU path name chall

-- | The name for each challenge directory.
challengeName :: ChallengeParams -> FilePath
challengeName Cont{..} =
  "chall-rlwec-m" ++ show m ++ "-q" ++ show q ++ "-v" ++ show svar
challengeName Disc{..} =
  "chall-rlwed-m" ++ show m ++ "-q" ++ show q ++ "-v" ++ show svar
challengeName RLWR{..} =
  "chall-rlwr-m" ++ show m ++ "-q" ++ show q ++ "-p" ++ show p

-- | Generate a challenge with the given parameters.
genChallengeU :: (MonadRandom rnd)
  => ChallengeParams -> ChallengeID -> BeaconAddr -> rnd ChallengeU
genChallengeU cp challID ba = do
  let chall = toProtoChallenge cp challID ba
      instIDs = take (fromIntegral $ numInsts cp) [0..]
  insts <- mapM (genInstanceU cp challID) instIDs
  return $ CU chall insts

-- | Generate an RLWE instance with the given parameters.
genInstanceU :: (MonadRandom rnd)
  => ChallengeParams -> ChallengeID -> InstanceID -> rnd InstanceU

genInstanceU cp@Cont{..} challID instID = reify q (\(_::Proxy q) ->
  reifyFactI (fromIntegral m) (\(_::proxy m) -> do
    (s, samples :: [C.Sample T m (Zq q) (RRq q)]) <- instanceCont svar numSamples
    let s' = toProtoSecret challID instID m q s
        inst = toProtoInstanceCont challID instID cp samples
    return $ IC s' inst))

genInstanceU cp@Disc{..} challID instID = reify q (\(_::Proxy q) ->
  reifyFactI (fromIntegral m) (\(_::proxy m) -> do
    (s, samples :: [D.Sample T m (Zq q)]) <- instanceDisc svar numSamples
    let s' = toProtoSecret challID instID m q s
        inst = toProtoInstanceDisc challID instID cp samples
    return $ ID s' inst))

genInstanceU cp@RLWR{..} challID instID = reify q (\(_::Proxy q) ->
  reify p (\(_::Proxy p) -> reifyFactI (fromIntegral m) (\(_::proxy m) -> do
    (s, samples :: [R.Sample T m (Zq q) (Zq p)]) <- instanceRLWR numSamples
    let s' = toProtoSecret challID instID m q s
        inst = toProtoInstanceRLWR challID instID cp samples
    return $ IR s' inst)))

-- | Constructs a 'Challenge' suitable for serialization.
toProtoChallenge :: ChallengeParams -> ChallengeID -> BeaconAddr -> Challenge
toProtoChallenge cp challengeID (BA beaconEpoch beaconOffset) =
  let numInstances = numInsts cp
  in case cp of
    Cont{..} -> Challenge{challType = T.Cont,..}
    Disc{..} -> Challenge{challType = T.Disc,..}
    RLWR{..} -> Challenge{challType = T.RLWR,..}

-- CJP: it would be nice to get rid of the incomplete pattern matches
-- for these next few functions, though I see why it's tricky.

-- | Constructs an 'InstanceCont' suitable for serialization.
toProtoInstanceCont :: forall t m zq rrq .
  (Fact m,
   Protoable (Cyc t m zq), ProtoType (Cyc t m zq) ~ Rq,
   Protoable (UCyc t m D rrq), ProtoType (UCyc t m D rrq) ~ Kq)
  => ChallengeID -> InstanceID -> ChallengeParams -> [C.Sample t m zq rrq] -> InstanceCont
toProtoInstanceCont challengeID instanceID Cont{..} samples' =
  let bound = proxy (C.errorBound svar eps) (Proxy::Proxy m)
      samples = (uncurry SampleCont) <$> (toProto samples')
  in InstanceCont{..}

-- | Constructs an 'InstanceDisc' suitable for serialization.
toProtoInstanceDisc :: forall t m zq .
  (Fact m, Protoable (Cyc t m zq), ProtoType (Cyc t m zq) ~ Rq)
  => ChallengeID -> InstanceID -> ChallengeParams -> [D.Sample t m zq] -> InstanceDisc
toProtoInstanceDisc challengeID instanceID Disc{..} samples' =
  let bound = proxy (D.errorBound svar eps) (Proxy::Proxy m)
      samples = uncurry SampleDisc <$> toProto samples'
  in InstanceDisc{..}

-- | Constructs an 'InstanceRLWR' suitable for serialization.
toProtoInstanceRLWR ::
  (Fact m,
   Protoable (Cyc t m zq), ProtoType (Cyc t m zq) ~ Rq,
   Protoable (Cyc t m zq'), ProtoType (Cyc t m zq') ~ Rq)
  => ChallengeID -> InstanceID -> ChallengeParams
     -> [R.Sample t m zq zq'] -> InstanceRLWR
toProtoInstanceRLWR challengeID instanceID RLWR{..} samples' =
  let samples = (uncurry SampleRLWR) <$> (toProto samples')
  in InstanceRLWR{..}

-- | Constructs an 'Secret' suitable for serialization.
toProtoSecret :: (Protoable (Cyc t m zq), ProtoType (Cyc t m zq) ~ Rq)
  => ChallengeID -> InstanceID -> Int32 -> Int64 -> Cyc t m zq -> Secret
toProtoSecret challengeID instanceID m q s' = Secret{s = toProto s', ..}

-- | Writes a 'ChallengeU' to a file given a path to the root of the tree
-- and the name of the challenge.
writeChallengeU :: FilePath -> String -> ChallengeU -> IO ()
writeChallengeU path challName (CU c insts) = do
  let challDir = challengeFilesDir path
      challFN = challFilePath path challName
  createDirectoryIfMissing True challDir
  writeProtoType challFN c
  mapM_ (writeInstanceU path challName) insts

-- | Writes an 'InstanceU' to a file given a path to the root of the tree
-- and the name of the challenge.
writeInstanceU :: FilePath -> String -> InstanceU -> IO ()
writeInstanceU path challName iu = do
  let s = secret iu
      idx = S.instanceID s
      instFN = instFilePath path challName idx
      secretFN = secretFilePath path challName idx
  case iu of
    (IC _ inst) -> writeProtoType instFN inst
    (ID _ inst) -> writeProtoType instFN inst
    (IR _ inst) -> writeProtoType instFN inst
  writeProtoType secretFN s

-- | Writes any auto-gen'd proto object to path/filename.
writeProtoType :: (ReflectDescriptor a, Wire a) => FilePath -> a -> IO ()
writeProtoType fileName obj = BS.writeFile fileName $ messagePut obj



-- | Generate a continuous RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceCont :: (C.RLWECtx t m zq rrq, Random zq, Random (LiftOf rrq),
              OrdFloat (LiftOf rrq), MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [C.Sample t m zq rrq])
instanceCont svar num = do
  s <- getRandom
  samples <- replicateM num $ C.sample svar s
  return (s, samples)

-- | Generate a discrete RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceDisc :: (D.RLWECtx t m zq, Random zq, MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [D.Sample t m zq])
instanceDisc svar num = do
  s <- getRandom
  samples <- replicateM num $ D.sample svar s
  return (s, samples)

-- | Generate a discrete RLWR instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceRLWR :: (R.RLWRCtx t m zq zp, Random zq, MonadRandom rnd)
  => Int -> rnd (Cyc t m zq, [R.Sample t m zq zp])
instanceRLWR num = do
  s <- getRandom
  samples <- replicateM num $ R.sample s
  return (s, samples)
