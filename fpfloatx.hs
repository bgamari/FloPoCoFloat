{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FPFloatX  where

import FPFloat 
import Clash.Prelude
import qualified Prelude as P 
import Data.Typeable (cast, Typeable)
import Data.Ratio
import Data.Proxy
import qualified Data.Number.MPFR as M
import Data.Char (digitToInt)
import qualified FPFloat as FP




type role FoFloatX nominal nominal nominal

newtype FoFloatX (rnd :: M.RoundMode) (wE :: Nat) (wF :: Nat) = FoFloatX{unFoFloatX::BitVector (2 + 1 + wE + wF)}
  deriving (Typeable, Generic, Eq, NFDataX, ShowX)

instance BitPack (Proxy a) where
    type BitSize (Proxy a) = 0
    pack _ = 0
    unpack _ = Proxy


class KnownRnd (rnd :: M.RoundMode) where
    rndVal :: Proxy rnd -> M.RoundMode 

instance KnownRnd M.Up where
    rndVal _ = M.Up

instance KnownRnd M.Down where
    rndVal _ = M.Down

instance KnownRnd M.Near where
    rndVal _ = M.Near

instance KnownRnd M.Zero where
    rndVal _ = M.Zero


data FoFloat2 exponentBits mantissaBits (rndMode :: M.RoundMode) =
      FoFloat2 { ext' :: (BitVector 2)
              , sign' :: Bit
              , wE' :: (BitVector exponentBits)
              , wF' :: (BitVector mantissaBits)
              , rndMode' :: (Proxy rndMode)
              }
    deriving (Generic, Typeable, Show, BitPack)

a :: FoFloat2 5 10 M.Near
a = FoFloat2 1 0 5 3 Proxy :: FoFloat2 5 10 (M.Near)

getMantissaBits :: forall a exponentBits mantissaBits rndMode . (Num a, KnownNat mantissaBits) => FoFloat2 exponentBits mantissaBits rndMode -> a 
getMantissaBits _ = natToNum @mantissaBits

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

        reexpV::BitVector 64
        reexpV = resize expV
        expval = bitCoerce (reexpV) :: Unsigned 64

        refracV::BitVector 64
        refracV = resize fracV
        fracval = bitCoerce (refracV) :: Unsigned 64
    in
        FoFloat extV signV 
                (fromInteger (natVal (Proxy @wE))) 
                (fromInteger (natVal (Proxy @wF)))
                (rndVal (Proxy @rnd)) 
                (expval) 
                (fracval)  

{-
binStr2FoFloatX ::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    P.String ->
    Int ->
    Int ->
    M.RoundMode ->
    FoFloatX rnd wE wF
binStr2FoFloatX binstr wEVal wFVal rnd = do
    if P.length binstr /= (2 + 1 wEVal + wFVal) then
        error "The binary string's length is not equivalent to required binary length"
    else do
        let
         -- Parse bits from the string
            parsedBits :: [Bit]
            parsedBits = P.map (fromIntegral . digitToInt) binstr

            -- Convert the list of bits directly into a list of Bool
            boolList :: [Bool]
            boolList = P.map bitToBool parsedBits

            -- Convert the list of Bools to a list of BitVector
            bvList :: [BitVector(1 + 2 + wE + wF)]
            bvList = P.map boolToBV boolList

            
        FoFloatX rnd wEVal wFVal 
-}



fromFoFloat ::
    forall we wf rnd .
    (KnownRnd rnd, KnownNat we, KnownNat wf) =>
    FoFloat ->
    FoFloatX rnd we wf
fromFoFloat  fofloat = 
    let 
        -- Convert fields to BitVector with appropriate sizes
        
        

        newfofloat = FP.convertFoFloattoFoFloat fofloat (fromInteger (natVal (Proxy @we))) (fromInteger (natVal (Proxy @wf)))
        extXX = bitCoerce (ext newfofloat) :: BitVector 2
        signXX = bitCoerce (sign newfofloat) :: BitVector 1
        expXX = resize ((fromIntegral (exponentVal newfofloat)) :: BitVector 64) :: BitVector we
        fracXX = resize ((fromIntegral (fractionalVal newfofloat)) :: BitVector 64) :: BitVector wf
        
        -- Combine fields into a single BitVector
        vecres = extXX ++# signXX ++# expXX ++# fracXX
    in
        FoFloatX vecres
        
             

decomposeFoFloatX::
    forall we wf rnd .
    (KnownRnd rnd, KnownNat we, KnownNat wf)=>
    FoFloatX rnd we wf-> 
    (Integer, Integer)
decomposeFoFloatX  = decomposeFoFloat . toFoFloat 
instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Num (FoFloatX rnd wE wF) where
    f1 + f2        = do
        let m1 = toFoFloat f1
        let m2 = toFoFloat f2
        let temp = m1 + m2
        fromFoFloat temp
    f1 - f2        = do
        let m1 = toFoFloat f1
        let m2 = toFoFloat f2
        let temp = m1 - m2
        fromFoFloat temp
        
    f1 * f2        = do 
        let m1 = toFoFloat f1
        let m2 = toFoFloat f2
        let temp = m1 * m2
        fromFoFloat temp
    negate f1      = do
        let m1 = toFoFloat f1
        let temp = negate m1
        fromFoFloat temp
    abs f1         = do
        let m1 = toFoFloat f1
        let temp = abs m1
        fromFoFloat temp
    signum f1     = do
        let m1 = toFoFloat f1
        let temp = signum m1
        fromFoFloat temp
            

    fromInteger i = fromFoFloat (fromInteger i)  


instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => ShowMPFR (FoFloatX rnd wE wF) where
  showMPFR = showMPFR . toFoFloat

instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Show (FoFloatX rnd wE wF) where
  show = P.show . toFoFloat

instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Real (FoFloatX rnd wE wF)where
    toRational d = n % (2  P.^ e)
        where (n', e') = decomposeFoFloatX d
              (n, e) = if e' >= 0 then ((n' * (2 P.^ e')), 0)
                         else (n', - e') 
instance (KnownRnd rnd, KnownNat we, KnownNat wf) => Ord (FoFloatX rnd we wf) where
    compare a b = compare (toFoFloat a) (toFoFloat b)
    
    (<) a  b    = lessFoFloat (toFoFloat a) (toFoFloat b)
    (<=) a  b  = lesseqFoFloat (toFoFloat a) (toFoFloat b)
    (>)  a b    = greaterFoFloat (toFoFloat a) (toFoFloat b)
    (>=)  a  b   = greatereqFoFloat (toFoFloat a) (toFoFloat b)

instance (KnownRnd rnd, KnownNat we, KnownNat wf) => Fractional (FoFloatX rnd we wf) where
    f1 / f2        = do
        let m1 = toFoFloat f1
        let m2 = toFoFloat f2
        let temp = m1 / m2
        fromFoFloat temp 
    
    fromRational r = do 
    --    let temp::FoFloat = fromRational r 
    --    fromFoFloat temp 
        let precfo = (fromInteger (natVal (Proxy @wf))) + 1
        let n = M.fromIntegerA M.Near precfo (P.fromInteger (numerator r))
        let d = M.fromIntegerA M.Near precfo (P.fromInteger (denominator r))
        let temp = M.div M.Near precfo n d
        fromFoFloat (convertMPFRtoFoFloat temp (fromInteger (natVal (Proxy @we))) (fromInteger (natVal (Proxy @wf))))
    recip d        = do
        let temp::FoFloat = toFoFloat d 
        let res = recip temp 
        fromFoFloat res


instance (KnownRnd rnd, KnownNat we, KnownNat wf) => Floating (FoFloatX rnd we wf) where
    pi           = fromFoFloat (pi::FoFloat)
    exp d        = do 
        let temp = toFoFloat d 
        let tempres = P.exp temp
        fromFoFloat tempres
    log d        = do 
        let temp = toFoFloat d
        let tempres = P.log temp
        fromFoFloat tempres

    sqrt d       = do 
        let temp = toFoFloat d
        let tempres = P.sqrt temp
        fromFoFloat tempres
    
    (**) f1 f2    = do 
        let m1 = toFoFloat f1 
        let m2 = toFoFloat f2
        let tempres = (**) m1 m2
        fromFoFloat tempres
    logBase d d' = error "logBase is not defined for FoFloatX"
    sin d        = error "sin is not defined for FoFloatX"
    cos d        = error "cos is not defined for FoFloatX"
    tan d        = error "tan is not defined for FoFloatX"
    asin d       = error "asin is not defined for FoFloatX"
    acos d       = error "acos is not defined for FoFloatX"
    atan d       = error "atan is not defined for FoFloatX"
    sinh d       = error "sinh is not defined for FoFloatX"
    cosh d       = error "cosh is not defined for FoFloatX"
    tanh d       = error "tanh is not defined for FoFloatX"
    asinh d      = error "asinh is not defined for FoFloatX"
    acosh d      = error "acosh is not defined for FoFloatX"
    atanh d      = error "atanh is not defined for FoFloatX"

instance (KnownRnd rnd, KnownNat we, KnownNat wf) => RealFrac (FoFloatX rnd we wf) where
    properFraction d = error "properFraction is not defined for FoFloatX"

instance (KnownRnd rnd, KnownNat we, KnownNat wf) => RealFloat (FoFloatX rnd we wf) where
    floatRadix _ = 2
    
    floatDigits _ = (fromInteger (natVal (Proxy @we))) + (fromInteger (natVal (Proxy @wf))) + 1 + 2
    floatRange _ = error "floatRange is not defined for FoFloatX numbers"
    decodeFloat x = (d,e)
      where
      (d,eE) = decomposeFoFloatX x
      e = fromInteger  eE
    encodeFloat d e = error "encodeFloat function is not defined for FoFloatX"
      
    isNaN = isNaN . toFoFloat
    isInfinite = isInfinite . toFoFloat
    isDenormalized _ = False
    isNegativeZero = isNegativeZero . toFoFloat
    isIEEE _ = True
    atan2 d1 d2 = error "atan2 is not defined for FoFloatX" 