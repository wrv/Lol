{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.ZqProd (ZqProd(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ZqProd = ZqProd{xs :: !(P'.Seq P'.Word64)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable ZqProd where
  mergeAppend (ZqProd x'1) (ZqProd y'1) = ZqProd (P'.mergeAppend x'1 y'1)

instance P'.Default ZqProd where
  defaultValue = ZqProd P'.defaultValue

instance P'.Wire ZqProd where
  wireSize ft' self'@(ZqProd x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 4 x'1)
  wirePut ft' self'@(ZqProd x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 8 4 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.append (xs old'Self) new'Field}) (P'.wireGet 4)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.mergeAppend (xs old'Self) new'Field}) (P'.wireGetPacked 4)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ZqProd) ZqProd where
  getVal m' f' = f' m'

instance P'.GPB ZqProd

instance P'.ReflectDescriptor ZqProd where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RLWE.ZqProd\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"ZqProd\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"ZqProd.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.ZqProd.xs\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"ZqProd\"], baseName' = FName \"xs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType ZqProd where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ZqProd where
  textPut msg
   = do
       P'.tellT "xs" (xs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'xs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'xs
         = P'.try
            (do
               v <- P'.getT "xs"
               Prelude'.return (\ o -> o{xs = P'.append (xs o) v}))