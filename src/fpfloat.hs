{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}
module FPFloat  where
import qualified Data.Number.MPFR as M --import functions
import Data.Number.MPFR.Instances.Up -- import instances

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
data FoFloat =
      FoFloat { ext :: (BitVector 2)
              , sign :: Bit
              , wE :: Int
              , wF :: Int
              , rndMode :: M.RoundMode
              , exponentVal:: Integer
              , fractionalVal::Integer
              }
    | IEEEFloat { sign :: Bit
                , wE :: Int
                , wF :: Int
                , rndMode :: M.RoundMode
                , exp:: Integer
                , frac:: Integer
                }
    deriving (Typeable, Generic, Show)

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

  (IEEEFloat sign1 wE1 wF1 rndMode1 exp1 frac1) ==
    (IEEEFloat sign2 wE2 wF2 rndMode2 exp2 frac2) =
      sign1 == sign2 &&
      wE1 == wE2 &&
      wF1 == wF2 &&
      eqRoundMode rndMode1 rndMode2 && -- Use custom equality
      exp1 == exp2 &&
      frac1 == frac2

  _ == _ = False


--newtype FoFloatX wE wF rndMode = FoFloatX {getBV :: BitVector (2 + wE + wF)}

--convertMPFRToBitVector :: MPFR -> BitVector n
{-
convertFoFloattoMPFR::FoFloat -> M.MPFR
convertFoFloattoMPFR fofloat = do
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


convertFoFloattoMPFR :: FoFloat -> M.MPFR
convertFoFloattoMPFR fofloat = do
    let extVal = ext fofloat
    let signVal = sign fofloat
    let wEVal = wE fofloat
    let wFVal = wF fofloat
    let rndModeVal = rndMode fofloat
    let expVal = exponentVal fofloat
    let fracVal = fractionalVal fofloat

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
        -- Normal number case
        01 -> do
            -- mpfr_val = (-1)^(sign) * (1 + frac/2^(wF))*2^(unbiased_exp)
            -- unbiased_exp = exp - (shiftL 1 (wE - 1) ) + 1
            let precVal = 2 + fromIntegral wFVal
            --print $ "Precision Value: "
            --print $ show precVal
            let frac_mpfr = M.fromIntegerA rndModeVal precVal fracVal
            --print $ "Fraction as MPFR: " 
            --print $ show frac_mpfr
            let temp = M.div2i rndModeVal precVal frac_mpfr wFVal -- (frac/2^(wF))
            --print $ "Fraction after division: " 
            --print $ show temp
            let temp1 = M.addw rndModeVal precVal temp 1 -- (1 + frac/2^(wF))
            --print $ "Fraction after addition: " 
            --print $ show temp1
            let unbiased_exp = expVal - (DBits.shiftL 1 (fromIntegral wEVal - 1)) + 1
            --print $ "Unbiased Exponent: " 
            --print $ show unbiased_exp
            let mpfr_val = M.mul2i rndModeVal precVal temp1 (P.fromInteger unbiased_exp)
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

{-
convertMPFRtoFoFloat::M.MPFR -> Int -> Int  -> FoFloat
convertMPFRtoFoFloat num wEVal wFVal = do
    let rndVal = M.Near
    if (M.isNaN num) then
        FoFloat {ext = 11, sign = 1, wE = wEVal, wF = wFVal,rndMode = rndVal ,exponentVal = 0, fractionalVal = 0}
    else if (M.isInfinite num) then
        FoFloat {ext = 10, sign = if ((fromMaybe 0 (M.sgn num)) > 0) then 0 else 1,  wE = wEVal, wF = wFVal, rndMode = rndVal ,exponentVal = 0, fractionalVal = 0}
    else if (M.isZero num)  then
        FoFloat {ext = 00, sign = if M.signbit num == P.False then 0 else 1, wE = wEVal, wF = wFVal,rndMode = rndVal ,exponentVal = 0, fractionalVal = 0}
    else do
        -- getExp return exponent for significant in [1/2,1)
		-- but we require [1,2). Hence the -1.
        let expVal::Int = unsafeCoerce (M.getExp num  - 1)
        let precVal = M.getPrec num
        let absVal = M.absD rndVal precVal num
        let temp = M.div2i rndVal precVal absVal expVal
        let temp1 = M.subw rndVal precVal temp 1
        let temp2 = M.mul2i rndVal precVal temp1 wFVal 
        let tempfracVal = M.toInt rndVal temp2 
        -- Due to rounding, the fraction might overflow (i.e. become bigger
		-- then we expect).
        
        let valcomp = DBits.shiftL 1 wFVal
        if (tempfracVal == valcomp) then
            FoFloat {ext = 01, sign = if ((fromMaybe 0 ( M.sgn num)) > 0) then 0 else 1, wE = wEVal, wF = wFVal,rndMode = rndVal ,exponentVal = P.fromIntegral (expVal + 1), fractionalVal = 0}
        else if (tempfracVal > valcomp) then
            unsafePerformIO (error "Fraction is too big after conversion")
        else if (tempfracVal < valcomp) then
            unsafePerformIO (error "Fraction is negative after conversion")
        else do
            let biasedExp = expVal + ((DBits.shiftL 1 (wEVal - 1)) - 1)
            -- Handle underflow
            if (biasedExp < 0) then 
                FoFloat 
                {ext = 00, 
                sign = if ((fromMaybe 0 (M.sgn num)) > 0) then 0 else 1, 
                wE = wEVal, 
                wF = wFVal,
                rndMode = rndVal ,
                exponentVal = 0, 
                fractionalVal = 0}
            -- Handle overflow
            else if(biasedExp >= (DBits.shiftL 1 wEVal)) then 
                FoFloat 
                {ext = 10, 
                sign = if ((fromMaybe 0(M.sgn num)) > 0) then 0 else 1, 
                wE = wEVal, 
                wF = wFVal,
                rndMode = rndVal ,
                exponentVal = 0, 
                fractionalVal = 0}
            else 
                FoFloat 
                {ext = 01, 
                sign = if ((fromMaybe 0 (M.sgn num)) > 0) then 0 else 1, 
                wE = wEVal, 
                wF = wFVal,
                rndMode = rndVal ,
                exponentVal = P.fromIntegral  expVal, 
                fractionalVal = P.fromIntegral tempfracVal}

-}


convertMPFRtoFoFloat :: M.MPFR -> Int -> Int ->  FoFloat
convertMPFRtoFoFloat num wEVal wFVal = do
    let rndVal = M.Near
        -- Print debug information to stdout
    
    if M.isNaN num then
        FoFloat {ext = 11, sign = 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = 0, fractionalVal = 0}
    else if M.isInfinite num then
        FoFloat {ext = 10, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = 0, fractionalVal = 0}
    else if M.isZero num then
        FoFloat {ext = 00, sign = if M.signbit num == P.False then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = 0, fractionalVal = 0}
    else do
        let expVal :: Int = fromIntegral (M.getExp num - 1)
        --print ("Exponent value: ")
        --print (expVal)
        let precVal = M.getPrec num
        --print ("Precision value: ")
        --print (precVal)
        let absVal = M.absD rndVal precVal num
        --print ("Absolute value: ")
        --print (M.toStringExp 32 absVal)
        let temp = M.div2i rndVal precVal absVal expVal
        --print ("Temporary value after div2i: " )
        --print (M.toStringExp 32 temp)
        let temp1 = M.subw rndVal precVal temp 1
        --print ("Temporary value after subw: ")
        --print ( M.toStringExp 32 temp1)
        let temp2 = M.mul2i rndVal precVal temp1 wFVal
        --print ("Temporary value after mul2i: ")
        --print ( M.toStringExp 32 temp2)
        let tempfracVal = M.toInt rndVal temp2
        --print ("Fractional value: ")
        --print ( show tempfracVal)

        -- Due to rounding, the fraction might overflow (i.e. become bigger
        -- than we expect).
        let valcomp::Int = DBits.shiftL 1 wFVal
        --print ("Comparing fraction to: ")
        --print ( show valcomp)
        if tempfracVal == valcomp then do 
            let biasedExp = expVal + 1 + ((DBits.shiftL 1 (wEVal - 1)) - 1) 
            FoFloat {ext = 01, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = P.fromIntegral biasedExp, fractionalVal = 0}
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
                FoFloat {ext = 00, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = 0, fractionalVal = 0}
            -- Handle overflow
            else if biasedExp >= (DBits.shiftL 1 wEVal) then
                FoFloat {ext = 10, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = 0, fractionalVal = 0}
            else
                FoFloat {ext = 01, sign = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1, wE = wEVal, wF = wFVal, rndMode = rndVal, exponentVal = P.fromIntegral biasedExp, fractionalVal = P.fromIntegral tempfracVal}
        
        
        

getPrecision::FoFloat -> Int
getPrecision fofloat = (wE fofloat) + (wF fofloat)

maxPrecision::FoFloat -> FoFloat -> Int
maxPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.max a b

minPrecision::FoFloat -> FoFloat -> Int
minPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.min a b

--type role FoFloat representational nominal nominal
--newtype FoFloat (rep:: Nat -> Type) (wE :: Nat) (wF :: Nat) = 

getwEwFfromMaxPrec:: FoFloat -> FoFloat -> (Int, Int)
getwEwFfromMaxPrec fo1 fo2 = do
    if(fo1 == maxPrecision fo1 fo2) then
        (wE fo1, wF fo1)
    else
        (wE fo2, wF fo2)

getwEwFfromMinPrec:: FoFloat -> FoFloat -> (Int, Int)
getwEwFfromMinPrec fo1 fo2 = do
    if(fo1 == minPrecision fo1 fo2) then
        (wE fo1, wF fo1)
    else
        (wE fo2, wF fo2)


instance Num FoFloat where
    f1 + f2        = do
        let m1 = convertFoFloattoMPFR f1
        let m2 = convertFoFloattoMPFR f2
        let temp = M.add (rndMode f1) (maxPrecision f1 f2) m1 m2
        let (wEVal, wFVal) = getwEwFfromMaxPrec f1 f2 
        convertMPFRtoFoFloat temp wEVal wFVal 
    f1 - f2        = do
        let m1 = convertFoFloattoMPFR f1
        let m2 = convertFoFloattoMPFR f2
        let temp = M.sub (rndMode f1) (maxPrecision f1 f2) m1 m2
        let (wEVal, wFVal) = getwEwFfromMaxPrec f1 f2 
        convertMPFRtoFoFloat temp wEVal wFVal 
        
    f1 * f2        = do 
        let m1 = convertFoFloattoMPFR f1
        let m2 = convertFoFloattoMPFR f2
        let temp = M.mul (rndMode f1) (maxPrecision f1 f2) m1 m2
        let (wEVal, wFVal) = getwEwFfromMaxPrec f1 f2 
        convertMPFRtoFoFloat temp wEVal wFVal
    negate f1      = do
        let m1 = convertFoFloattoMPFR f1
        let temp = M.neg (rndMode f1) (getPrecision m1) m1
        let (wEVal, wFVal) =(wE f1, wF f1) 
        convertMPFRtoFoFloat temp wEVal wFVal
    abs f1         = do
        let m1 = convertFoFloattoMPFR f1
        let temp = M.absD (rndMode f1) (getPrecision m1) m1
        let (wEVal, wFVal) =(wE f1, wF f1) 
        convertMPFRtoFoFloat temp wEVal wFVal
    signum f1     = 
        if (ext f1) == 00 then 0
        else if (ext f1) == 11 then f1
        -- Infinity case and Normal case
        else 
            if(sign f1) == 0 then 1 else 0
            

    fromInteger i = convertMPFRtoFoFloat (M.fromIntegerA M.Near 23 i) 8 23
        
-- #ifdef INTEGER_SIMPLE
--     fromInteger i =
--         fromIntegerA Zero (max minPrec $ 1 + bitsInInteger i) i
-- #endif
-- #ifdef INTEGER_GMP
--     fromInteger (S# i) = fromInt Zero minPrec (E.I# i)
--     fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i
-- #endif
decomposeFoFloat::FoFloat -> (Integer, Integer)
decomposeFoFloat fofloat = (fractionalVal fofloat, (exponentVal fofloat) - (DBits.shiftL 1 (fromIntegral (wE fofloat) - 1)) + 1)

instance Real FoFloat where
    toRational d = n % 2 ^ e
        where (n', e') = decomposeFoFloat d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional FoFloat where
    f1 / f2        = do
        let m1 = convertFoFloattoMPFR f1
        let m2 = convertFoFloattoMPFR f2
        let temp = M.div (rndMode f1) (maxPrecision f1 f2) m1 m2
        let (wEVal, wFVal) = getwEwFfromMaxPrec f1 f2 
        convertMPFRtoFoFloat temp wEVal wFVal 
    fromRational r = do
        let n = M.fromIntegerA M.Near 23 (P.fromInteger numerator r)
        let d = M.fromIntegerA M.Near 23 (P.fromInteger denominator r)
        let temp = M.div M.Near 23 n d
        convertMPFRtoFoFloat temp 8 23
    recip d        = do
        let oneFoFloat = M.fromInt M.Near 23 1
        let temp = M.div M.Near 

instance Floating FoFloat where
    pi           = S.pi Zero 53
    exp d        = S.exp Zero (getPrec d) d
    log d        = S.log Zero (getPrec d) d
    sqrt d       = A.sqrt Zero (getPrec d) d
    (**) d d'    = A.pow Zero (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = S.sin Zero (getPrec d) d
    cos d        = S.cos Zero (getPrec d) d
    tan d        = S.tan Zero (getPrec d) d
    asin d       = S.asin Zero (getPrec d) d
    acos d       = S.acos Zero (getPrec d) d
    atan d       = S.atan Zero (getPrec d) d
    sinh d       = S.sinh Zero (getPrec d) d
    cosh d       = S.cosh Zero (getPrec d) d
    tanh d       = S.tanh Zero (getPrec d) d
    asinh d      = S.asinh Zero (getPrec d) d
    acosh d      = S.acosh Zero (getPrec d) d
    atanh d      = S.atanh Zero (getPrec d) d

instance RealFrac FoFloat where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Zero (getPrec d) d

instance RealFloat FoFloat where
    floatRadix _ = 2
    floatDigits = fromInteger . toInteger . getPrec
    floatRange _ = error "floatRange is not defined for MPFR numbers"
    decodeFloat x = (d,e)
      where
      (d,eE) = decompose x
      e = fromInteger (toInteger eE)
    encodeFloat d e = error "encodeFloat function is not defined for FoFloat"
      
    isNaN (MP _ _ e _) = (e == expNaN)
    isInfinite (MP _ _ e _) = (e == expInf)
    isDenormalized _ = False
    isNegativeZero d@(MP _ _ e _) = (e == expZero && signbit d)
    isIEEE _ = False
    atan2 d1 d2 = S.atan2 Near (maxPrec d1 d2) d1 d2