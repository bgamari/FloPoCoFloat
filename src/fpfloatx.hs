{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module FPFloatX  where

import qualified FPFloat as FP 
import Clash.Prelude
import qualified Prelude as P 
import Data.Typeable (cast, Typeable)
import Data.Ratio
import Data.Proxy
import qualified Data.Number.MPFR as M
type role FoFloatX nominal nominal

newtype FoFloatX (rnd :: M.RoundMode) (wE :: Nat) (wF :: Nat) =
  FoFloatX {unFoFloatX :: BitVector (2 + 1 + wE + wF) }
  deriving (Typeable, Generic, Eq)

class KnownRnd (rnd :: M.RoundMode) where
    rndVal :: Proxy rnd -> M.RoundMode 

instance KnownRnd M.Up where
    rndVal _ = M.Up

instance KnownRnd M.Down where
    rndVal _ = M.Down

instance KnownRnd M.Near where
    rndVal _ = M.Near

instacne KnownRnd M.Zero where
    rndVal _ = M.Zero

toFoFloat ::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloatX rnd wE wF ->
    FoFloat
toFoFloat (FoFloatX bv) = 
    let
        extV :: BitVector 2
        signV :: Bit
        expV :: BitVector wE
        fracV :: BitVector wF
        (extV,signV,expV,fracV) = unpack bv
    in
        FoFloat extV signV 
                (fromInteger (natVal (Proxy @wE))) 
                (fromInteger (natVal (Proxy @wF)))
                (rndVal (Proxy @rnd)) 
                (toInteger expV) 
                (toInteger fracV)  

fromFoFloat ::
    (KnownRnd rnd, KnownNat we, KnownNat wf)
    FoFloat ->
    FoFloatX rnd we wf
fromFoFloat (FoFloat {..}) =
    let 
        rndXX = rndVal (Proxy @rnd)
        expXX = fromInteger (natVal (Proxy @we))
        fracXX = fromInteger (natVal (Proxy @wf))

        valid = rndXX == rndMode &&
                expXX == exponentVal &&
                fracXX == fractionalVal

        expVal = pack
     in if valid then
            FoFloatX ()

instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Show (FoFloatX rnd wE wF) where
  show = show . toFoFloat

instance (KnownRnd rnd, KnownNat we, KnownNat wf) => Ord (FoFloatX rnd we wf) where
    compare a b = compare (toFoFloat a) (toFoFloat b)