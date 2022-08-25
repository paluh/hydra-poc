{-# LANGUAGE UndecidableInstances #-}

module Hydra.ClientInput where

import Hydra.Prelude

import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Ledger (IsTx, UTxOType)
import Codec.CBOR.Decoding (Decoder)
import Codec.Serialise.Decoding (decodeListLen)
import Cardano.Binary (DecoderError(DecoderErrorUnknownTag), matchSize, decodeWord8, encodeListLen)
import Cardano.Prelude (cborError)

data ClientInput tx
  = Init {contestationPeriod :: ContestationPeriod}
  | Abort
  | Commit {utxo :: UTxOType tx}
  | NewTx {transaction :: tx}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic)


instance (FromCBOR tx, FromCBOR ContestationPeriod, FromCBOR (UTxOType tx)) => FromCBOR (ClientInput tx) where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size' = matchSize "Interface.Error" size' len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> Init . UnsafeContestationPeriod <$> fromCBOR
      1 -> checkSize 1 >> pure Abort
      2 -> checkSize 2 >> Commit <$> fromCBOR
      3 -> checkSize 2 >> NewTx <$> fromCBOR
      4 -> checkSize 1 >> pure GetUTxO
      5 -> checkSize 1 >> pure Close
      6 -> checkSize 1 >> pure Contest
      7 -> checkSize 1 >> pure Fanout
      _ -> cborError $ DecoderErrorUnknownTag "Interface.Error" tag


instance (ToCBOR (UTxOType tx), ToCBOR  tx) => ToCBOR (ClientInput tx) where
  toCBOR err = case err of
    Init (UnsafeContestationPeriod cp) ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR cp
    Abort ->
      encodeListLen 1 <> toCBOR (1 :: Word8)
    Commit utxotype ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR utxotype
    NewTx tx ->
      encodeListLen 2
        <> toCBOR (3 :: Word8)
        <> toCBOR tx
    GetUTxO ->
      encodeListLen 1 <> toCBOR (4 :: Word8)
    Close ->
      encodeListLen 1 <> toCBOR (5 :: Word8)
    Contest ->
      encodeListLen 1 <> toCBOR (6 :: Word8)
    Fanout ->
      encodeListLen 1 <> toCBOR (7 :: Word8)

deriving instance IsTx tx => Eq (ClientInput tx)
deriving instance IsTx tx => Show (ClientInput tx)
deriving instance IsTx tx => ToJSON (ClientInput tx)
deriving instance IsTx tx => FromJSON (ClientInput tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init{} -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Close -> []
    Contest -> []
    Fanout -> []
