{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}
module FPFloat  where
import qualified Data.Number.MPFR as M --import functions
-- import instances
import qualified Data.Number.MPFR.Mutable as MM
import GHC.Generics(Generic)

import Clash.Prelude
import Control.Monad.ST(runST, ST)
import Data.Typeable (cast, Typeable)
import Clash.XException
import qualified Prelude as P
import qualified Data.Bits as DBits

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)

import System.IO (hFlush, stdout)
import Debug.Trace (trace)
import Data.Ratio
import Data.Proxy
import Data.Data  (Data)





{-
testRandom =
    do
    let rsP = M.newRandomStatePointer
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000

main = do print $ s1 1000 100000
          print $ s6 1000 100000
          print $ e1 1000 100000
          print $ e2 1000 100000
          testRandom
          let c1 = one
          putStrLn $ "exp 1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 c1)
          putStrLn $ "exp -1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 (M.neg M.Up 100 c1))
    
one = 1 :: M.MPFR
-}

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

-- type role FoFloat nominal nominal nominal 
data FoFloat (wE::Nat ) (wF::Nat) (rndMode:: M.RoundMode) =
      FoFloat { ext :: (BitVector 2)
              , sign :: Bit
              , exponentVal:: (BitVector wE)
              , fractionalVal:: (BitVector wF)
              , rndModeVal :: (Proxy rndMode)
              }
    deriving (Generic, Typeable,Show, BitPack, Eq, NFDataX, ShowX, Lift)

deriving instance (Lift (Proxy a))
deriving instance (NFDataX (Proxy a))
deriving instance (ShowX (Proxy a))

data IEEEFloat = IEEEFloat { signI :: Bit
                , wEI :: Int
                , wFI:: Int
                , rndModeI :: M.RoundMode
                , expI:: Integer
                , fracI:: Integer
                }
    deriving (Typeable, Generic, Show)

class ShowMPFR a where
    showMPFR::a -> P.String

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => ShowMPFR (FoFloat wE wF rnd) where
    showMPFR = P.show .toMPFR



{-

  (IEEEFloat sign1 wE1 wF1 rndMode1 exp1 frac1) ==
    (IEEEFloat sign2 wE2 wF2 rndMode2 exp2 frac2) =
      sign1 == sign2 &&
      wE1 == wE2 &&
      wF1 == wF2 &&
      eqRoundMode rndMode1 rndMode2 && -- Use custom equality
      exp1 == exp2 &&
      frac1 == frac2
-}
{-
eqRoundMode :: M.RoundMode -> M.RoundMode -> Bool
eqRoundMode M.Near       M.Near       = True
eqRoundMode M.Zero       M.Zero       = True
eqRoundMode M.Up         M.Up         = True
eqRoundMode M.Down       M.Down       = True
eqRoundMode _            _            = False

instance Eq FoFloat where
  (FoFloat ext1 sign1 wE1 wF1 rndMode1 exp1 frac1) ==
    (FoFloat ext2 sign2 wE2 wF2 rndMode2 exp2 frac2) =
      ext1 == ext2 &&
      sign1 == sign2 &&
      wE1 == wE2 &&
      wF1 == wF2 &&
      eqRoundMode rndMode1 rndMode2 && -- Use custom equality
      exp1 == exp2 &&
      frac1 == frac2

  _ == _ = False
-}

--newtype FoFloatX wE wF rndMode = FoFloatX {getBV :: BitVector (2 + wE + wF)}

--convertMPFRToBitVector :: MPFR -> BitVector n
{-
toMPFR::FoFloat -> M.MPFR
toMPFR fofloat = do
    let extVal = ext fofloat
    let signVal = sign fofloat
    let wEVal = wE fofloat
    let wFVal = wF fofloat
    let rndModeVal = rndMode fofloat
    let expVal = exponentVal fofloat 
    let fracVal= fractionalVal fofloat 

    case extVal of 
        -- Zero case
        00 -> do
            if signVal == 0 then
                M.fromDouble rndModeVal (1 + fromIntegral wFVal) 0.0 
            else
                M.fromDouble rndModeVal (1 + fromIntegral wFVal) (-0.0) 
        -- Infinity case
        10 -> do
            if signVal == 0 then
                M.setInf (1 + fromIntegral wFVal) 1
            else
                M.setInf (1 + fromIntegral wFVal) (-1)
        -- NaN case
        11 -> do
            let val = M.setNaN (1 + fromIntegral wFVal)
            val
        -- Nomral number case
        01 -> do
            -- mpfr_val = (-1)^(sign) * (1 + frac/2^(wF))*2^(unbiased_exp)
            -- unbiased_exp = exp - (shiftL 1 (wE - 1) ) + 1
            let precVal =2 + fromIntegral wFVal
            let frac_mpfr = M.fromIntegerA rndModeVal precVal fracVal
            let temp = M.div2i rndModeVal precVal frac_mpfr wFVal -- (frac/2^(wF))
            let temp1 = M.addw rndModeVal precVal temp 1 -- (1 + frac/2^(wF))
            let unbiased_exp = expVal - (DBits.shiftL 1 (fromIntegral wEVal -1)) + 1
            let mpfr_val = M.mul2i rndModeVal precVal temp1 (P.fromInteger unbiased_exp)
            if signVal == 1 then
                M.neg rndModeVal precVal mpfr_val
            else
                mpfr_val
-}

getRoundMode::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    M.RoundMode
getRoundMode fofloat = rndVal (rndModeVal fofloat)
toMPFR ::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    M.MPFR
toMPFR fofloat = do
    let 
        extVal :: BitVector 2 = ext fofloat
        signVal :: Bit = sign fofloat
        expBV :: BitVector wE = exponentVal fofloat
        fracBV :: BitVector wF = fractionalVal fofloat
    let wEVal::Int =  fromInteger (natVal (Proxy @wE))
    let wFVal =  fromInteger (natVal (Proxy @wF))
    let rndModeVal = getRoundMode fofloat
    let expVal = bitCoerce (expBV) ::Unsigned wE
    let fracVal = bitCoerce (fracBV) ::Unsigned wF

    case extVal of
        -- Zero case
        00 -> do
            if signVal == 0 then
                M.fromDouble rndModeVal (1 + wFVal) 0.0
            else
                M.fromDouble rndModeVal (1 + wFVal) (-0.0)
        -- Infinity case
        10 -> do
            if signVal == 0 then
                M.setInf (1 + wFVal) 1
            else
                M.setInf (1 + wFVal) (-1)
        -- NaN case
        11 -> do
            let val = M.setNaN (1 + wFVal)
            val
        -- Normal number case
        01 -> do
            -- mpfr_val = (-1)^(sign) * (1 + frac/2^(wF))*2^(unbiased_exp)
            -- unbiased_exp = exp - (shiftL 1 (wE - 1) ) + 1
            let precVal = 2 + wFVal
            --print $ "Precision Value: "
            --print $ show precVal
            let frac_mpfr = M.fromIntegerA rndModeVal precVal (toInteger fracVal)
            --print $ "Fraction as MPFR: " 
            --print $ show frac_mpfr
            let temp = M.div2i rndModeVal precVal frac_mpfr (fromIntegral wFVal) -- (frac/2^(wF))
            --print $ "Fraction after division: " 
            --print $ show temp
            let temp1 = M.addw rndModeVal precVal temp 1 -- (1 + frac/2^(wF))

            --
            -- let _ = rndVal $ Proxy @rndMode 


            --print $ "Fraction after addition: " 
            --print $ show temp1
            let unbiased_exp = expVal - (DBits.shiftL 1 (fromIntegral wEVal - 1)) + 1
            --print $ "Unbiased Exponent: " 
            --print $ show unbiased_exp
            let mpfr_val = M.mul2i rndModeVal precVal temp1 (P.fromInteger (toInteger unbiased_exp))
            --print $ "MPFR Value: " 
            --print $ show mpfr_val
            if signVal == 1 then do
                let neg_val = M.neg rndModeVal precVal mpfr_val
                --print $ "Negative MPFR Value: " 
                --print $ show neg_val
                --return $ neg_val
                neg_val
            else do
                mpfr_val
                --return $ mpfr_val



toFoFloat :: 
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    M.MPFR ->  
    FoFloat wE wF rnd
toFoFloat num = do
    let rndValue = M.Near
        -- Print debug information to stdout
    
    if M.isNaN num then
        FoFloat {ext = 11, sign = 1, rndModeVal = Proxy, exponentVal = 0, fractionalVal = 0}:: FoFloat wE wF rndVal
    else if M.isInfinite num then
        FoFloat {ext = 10, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, rndModeVal = Proxy, exponentVal = 0, fractionalVal = 0}:: FoFloat wE wF rndVal
    else if M.isZero num then
        FoFloat {ext = 00, sign = if M.signbit num == P.False then 0 else 1, rndModeVal = Proxy, exponentVal = 0, fractionalVal = 0}:: FoFloat wE wF rndVal
    else do
        let expVal :: Int = fromIntegral (M.getExp num - 1)
        let wFVal ::Int = fromInteger (natVal (Proxy @wF))
        let wEVal ::Int = fromInteger (natVal (Proxy @wE))
        --print ("Exponent value: ")
        --print (expVal)
        let precVal = M.getPrec num
        --print ("Precision value: ")
        --print (precVal)
        let absVal = M.absD rndValue precVal num
        --print ("Absolute value: ")
        --print (M.toStringExp 32 absVal)
        let temp = M.div2i rndValue precVal absVal expVal
        --print ("Temporary value after div2i: " )
        --print (M.toStringExp 32 temp)
        let temp1 = M.subw rndValue precVal temp 1
        --print ("Temporary value after subw: ")
        --print ( M.toStringExp 32 temp1)
        let temp2 = M.mul2i rndValue precVal temp1 wFVal
        --print ("Temporary value after mul2i: ")
        --print ( M.toStringExp 32 temp2)
        let tempfracVal = M.toInt rndValue temp2
        --print ("Fractional value: ")
        --print ( show tempfracVal)

        -- Due to rounding, the fraction might overflow (i.e. become bigger
        -- than we expect).
        let valcomp::Int = DBits.shiftL 1 wFVal
        --print ("Comparing fraction to: ")
        --print ( show valcomp)
        if tempfracVal == valcomp then do 
            let biasedExp = expVal + 1 + ((DBits.shiftL 1 (wEVal - 1)) - 1) 
            FoFloat {ext = 01, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, rndModeVal = Proxy, exponentVal = P.fromIntegral biasedExp, fractionalVal = 0}:: FoFloat wE wF rndVal
        else if tempfracVal > valcomp then
            error "Fraction is too big after conversion"
        else if tempfracVal < 0 then
            error "Fraction is negative after conversion"
        else do
            let biasedExp = expVal + ((DBits.shiftL 1 (wEVal - 1)) - 1)
            --print ("Biased exponent: " )
            --print (P.show biasedExp)
            -- Handle underflow
            if biasedExp < 0 then
                FoFloat {ext = 00, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, rndModeVal = Proxy, exponentVal = 0, fractionalVal = 0}:: FoFloat wE wF rndVal
            -- Handle overflow
            else if biasedExp >= (DBits.shiftL 1 wEVal) then
                FoFloat {ext = 10, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, rndModeVal = Proxy, exponentVal = 0, fractionalVal = 0}:: FoFloat wE wF rndVal
            else
                FoFloat {ext = 01, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, rndModeVal = Proxy, exponentVal = P.fromIntegral biasedExp, fractionalVal = P.fromIntegral tempfracVal}:: FoFloat wE wF rndVal
        


--showMPFR::FoFloat -> String
--showMPFR fofloat = P.show (toMPFR fofloat)
{-       
convertFoFloattoFoFloat::FoFloat -> Int -> Int ->FoFloat
convertFoFloattoFoFloat fofloat wEVal wFVal = do
    let num::M.MPFR = toMPFR fofloat
    toFoFloat num wEVal wFVal
-}

getPrecision::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd-> 
    M.Precision
getPrecision _ = fromInteger (natVal (Proxy @wE)) + fromInteger (natVal (Proxy @wF))

maxPrecision::
    forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    M.Precision
maxPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.max a b

minPrecision::
    forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    M.Precision
minPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.min a b

--type role FoFloat representational nominal nominal
--newtype FoFloat (rep:: Nat -> Type) (wE :: Nat) (wF :: Nat) = 

getwEwFfromMaxPrec:: forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    (Int, Int)
getwEwFfromMaxPrec fo1 fo2 = do
    if(getPrecision fo1 == maxPrecision fo1 fo2) then
        (fromInteger (natVal (Proxy @wE)), fromInteger (natVal  (Proxy @wF)))
    else
        (fromInteger (natVal (Proxy @wE1)), fromInteger (natVal  (Proxy @wF1)))

getwEwFfromMinPrec:: forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 ->  
    (Int, Int) 
getwEwFfromMinPrec fo1 fo2 = do
    if(getPrecision fo1 == minPrecision fo1 fo2) then
        (fromInteger (natVal (Proxy @wE)), fromInteger (natVal  (Proxy @wF)))
    else
        (fromInteger (natVal (Proxy @wE1)), fromInteger (natVal  (Proxy @wF1)))


instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Num (FoFloat wE wF rnd) where
    f1 + f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.add (rndVal (Proxy @rnd)) (getPrecision f1) m1 m2
        toFoFloat temp 
    f1 - f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.sub (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        toFoFloat temp 
        
    f1 * f2        = do 
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.mul (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        toFoFloat temp 
    negate f1      = do
        let m1 = toMPFR f1
        let temp = M.neg (rndVal (Proxy @rnd)) (getPrecision f1) m1
        toFoFloat temp 
    abs f1         = do
        let m1 = toMPFR f1
        let temp = M.absD (rndVal (Proxy @rnd)) (getPrecision f1) m1
        toFoFloat temp 
    signum f1     = 
        if (ext f1) == 00 then 0
        else if (ext f1) == 11 then f1
        -- Infinity case and Normal case
        else 
            if(sign f1) == 0 then 1 else 0
            

    fromInteger i = toFoFloat (M.fromIntegerA (rndVal (Proxy @rnd)) (fromInteger (natVal (Proxy @wE)) + fromInteger (natVal (Proxy @wF))) i) 
        
-- #ifdef INTEGER_SIMPLE
--     fromInteger i =
--         fromIntegerA Zero (max minPrec $ 1 + bitsInInteger i) i
-- #endif
-- #ifdef INTEGER_GMP
--     fromInteger (S# i) = fromInt Zero minPrec (E.I# i)
--     fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i
-- #endif
decomposeFoFloat:: 
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd-> 
    (Integer, Integer)
decomposeFoFloat fofloat = (toInteger (fractionalVal fofloat),toInteger (exponentVal fofloat) - (DBits.shiftL 1 (fromInteger (natVal (Proxy @wE)) - 1)) + 1)

cmpFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd -> 
    P.Ordering
cmpFoFloat fo1 fo2 = do
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    P.compare m1 m2 

lessFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
lessFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.less m1 m2
lesseqFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
lesseqFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.lesseq m1 m2
greaterFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
greaterFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.greater m1 m2
greatereqFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
greatereqFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.greatereq m1 m2



instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => Ord (FoFloat wE wF rnd) where
    compare  = cmpFoFloat 
    (<)       = lessFoFloat
    (<=)      = lesseqFoFloat
    (>)       = greaterFoFloat
    (>=)      = greatereqFoFloat
instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Real (FoFloat wE wF rnd) where
    toRational d = n % (2  P.^ e)
        where (n', e') = decomposeFoFloat d
              (n, e) = if e' >= 0 then ((n' * (2 P.^ e')), 0)
                         else (n', - e')

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => Fractional (FoFloat wE wF rnd) where
    f1 / f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.div (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        
        toFoFloat temp 
    fromRational r = do
        let n = M.fromIntegerA M.Near (fromInteger (natVal (Proxy @wF))) (P.fromInteger (numerator r))
        let d = M.fromIntegerA M.Near (fromInteger (natVal (Proxy @wF))) (P.fromInteger (denominator r))
        let temp = M.div M.Near (fromInteger (natVal (Proxy @wF))) n d
        toFoFloat temp 
    recip d        = do
        let one = M.fromWord M.Near (fromInteger (natVal (Proxy @wF))) 1
        let denom = toMPFR d 
        let temp = M.div (rndVal (Proxy @rnd)) (getPrecision d) one denom 
        toFoFloat temp 



instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Floating (FoFloat wE wF rnd) where
    pi           = toFoFloat (M.pi M.Near (fromInteger (natVal (Proxy @wF)))) 
    exp d        = do 
        let temp = toMPFR d 
        let tempres = M.exp (rndVal (Proxy @rnd)) (getPrecision d) temp 
        toFoFloat tempres 
    log d        = do 
        let temp = toMPFR d 
        let tempres = M.log M.Down (getPrecision d) temp 
        let res = toFoFloat tempres 
        --let tempres1 = M.log M.Up (getPrecision d) temp 
        --let res1  = toFoFloat tempres1 
        res 
    sqrt d       = do 
        let temp = toMPFR d 
        let tempres = M.sqrt M.Near (getPrecision d) temp 
        toFoFloat tempres 
    (**) f1 f2    = do 
        let temp = toMPFR f1 
        let temp1 = toMPFR f2
        let tempres = M.pow (rndVal (Proxy @rnd)) (getPrecision f1) temp temp1
        toFoFloat tempres 
    logBase d d' = error "logBase is not defined for FoFloat"
    sin d        = error "sin is not defined for FoFloat"
    cos d        = error "cos is not defined for FoFloat"
    tan d        = error "tan is not defined for FoFloat"
    asin d       = error "asin is not defined for FoFloat"
    acos d       = error "acos is not defined for FoFloat"
    atan d       = error "atan is not defined for FoFloat"
    sinh d       = error "sinh is not defined for FoFloat"
    cosh d       = error "cosh is not defined for FoFloat"
    tanh d       = error "tanh is not defined for FoFloat"
    asinh d      = error "asinh is not defined for FoFloat"
    acosh d      = error "acosh is not defined for FoFloat"
    atanh d      = error "atanh is not defined for FoFloat"

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => RealFrac (FoFloat wE wF rnd) where
    properFraction d = error "properFraction is not defined for FoFloat"

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => RealFloat (FoFloat wE wF rnd) where
    floatRadix _ = 2
    floatDigits = fromInteger . toInteger . getPrecision
    floatRange _ = error "floatRange is not defined for FoFloat numbers"
    decodeFloat x = (d,e)
      where
      (d,eE) = decomposeFoFloat x
      e = fromInteger (toInteger eE)
    encodeFloat d e = error "encodeFloat function is not defined for FoFloat"
      
    isNaN fo = if (ext fo) == 11 then P.True else P.False
    isInfinite fo = if (ext fo) == 10 then P.True else P.False
    isDenormalized _ = False
    isNegativeZero fo = if (ext fo) == 00 && (sign fo) == 1 then P.True else P.False
    isIEEE _ = True
    atan2 d1 d2 = error "atan2 is not defined for FoFloat" 