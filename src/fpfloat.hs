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
import Data.Bits as DBits

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
            if sign == 0 then
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
            if sign == 1 then
                M.neg rndModeVal precVal mpfr_val
            else
                mpfr_val




--type role FoFloat representational nominal nominal
--newtype FoFloat (rep:: Nat -> Type) (wE :: Nat) (wF :: Nat) = 