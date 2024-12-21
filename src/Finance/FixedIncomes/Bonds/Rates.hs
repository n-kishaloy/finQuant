-- | Implement Fixed Incomes Rates modules for the FinQuant library
--
-- Module      : FinQuant.FixedIncomes.Bonds.Rates <br>
-- Copyright   : (c) 2024 Kishaloy Neogi <br>
-- License     : MIT <br>
-- Maintainer  : Kishaloy Neogi <br>
-- Email       : <nkishaloy@yahoo.com>
--
-- The module describes the base modules of Fixed Incomes Rates.
--
-- You may see the github repository at <https://github.com/n-kishaloy/finQuant>
module Finance.FixedIncomes.Bonds.Rates
  ( RateCurve (..),
    Rates (..),
  )
where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
-- import Debug.Trace (trace)
import Finance qualified as F
import GHC.Generics (Generic)
import GHC.Records (HasField (..))

-- debug :: c -> String -> c
-- debug = flip trace

-- | RateCurve defines Enum for different type of Rates (Nominal, Effective)
-- given as curve.
--
-- RateCurve tracked rates which varies over a period and is given as a periodic in terms
-- of Rates given in regular intervals. It can be of 3 types:
-- - NominalRateCurve
-- - EffectiveRateCurve
--
-- Each type has 2 fields
-- - rate  = Vector of rates
-- - freq  = freq at which the rates are being given per period.
data RateCurve
  = NominalRateCurve {rate :: Vector Double, freq :: Double}
  | EffectiveRateCurve {rate :: Vector Double, freq :: Double}
  deriving (Show, Generic)

-- | Estimate the rate at a particular time by interpolating between the rate curves points
--
-- - y = the time given as period whose rate is being sought.
instance HasField "rateEstimate" RateCurve (Double -> Double) where
  getField :: RateCurve -> Double -> Double
  getField rt z =
    let estima :: Vector Double -> Double -> Double -> Double
        estima rx fq y =
          let (fl, pf) = properFraction (y * fq)
              r0 = rx ! (fl - 1)
           in if pf < 1e-9 then r0 else r0 * (1.0 - pf) + (rx ! fl) * pf
     in case rt of
          NominalRateCurve {rate = r, freq = f} -> estima r f z
          EffectiveRateCurve {rate, freq} -> estima rate freq z

-- | The Present Value of a cash flow at a particular time.
--
-- - c     = cash flow
-- - tim   = time in period at which the cash flow occurs.
instance HasField "pv" RateCurve (Double -> Double -> Double) where
  getField :: RateCurve -> Double -> Double -> Double
  getField rt c tim =
    let rate = rt.rateEstimate tim
     in case rt of
          NominalRateCurve {freq = f} -> F.pv (rate / f) (tim * f) c
          EffectiveRateCurve {} -> F.pv rate tim c

-- | Convert the RateCurve to a curve with Nominal rates
instance HasField "toNominal" RateCurve RateCurve where
  getField :: RateCurve -> RateCurve
  getField NominalRateCurve {rate, freq} = NominalRateCurve {rate, freq}
  getField EffectiveRateCurve {rate = r, freq} =
    NominalRateCurve {rate = U.map (`F.effNomRate` freq) r, freq}

-- | Convert the RateCurve to a curve with Effective rates
instance HasField "toEffective" RateCurve RateCurve where
  getField :: RateCurve -> RateCurve
  getField EffectiveRateCurve {rate, freq} = EffectiveRateCurve {rate, freq}
  getField NominalRateCurve {rate = r, freq} =
    EffectiveRateCurve {rate = U.map (`F.nomEffRate` freq) r, freq}

-- | The Rates enum defines different types of Rates represemted in RateCurves
--
-- - SpotRates
-- - ParRates
-- - ForwardRates
data Rates
  = SpotRates {rate :: RateCurve}
  | ParRates {rate :: RateCurve}
  | ForwardRates {rate :: RateCurve}
  deriving (Show, Generic)

instance HasField "toSpot" Rates Rates where
  getField :: Rates -> Rates
  getField ParRates {rate} =
    let (rt, fq) =
          case rate of
            NominalRateCurve {rate = r, freq = f} -> (r, f)
            _ -> error "Not Implemented"

        n = U.length rt
        y = runST $ do
          (mv :: M.MVector (M.PrimState (ST s)) Double) <- M.unsafeNew n
          M.unsafeWrite mv 0 (rt ! 0)

          qr <- M.generate (n - 1) (+ 1)
          M.forM_ qr $ \i -> do
            let xm = (rt ! i) / fq
            sm <- U.forM [0 .. i - 1] $ \k -> do
              yk <- M.unsafeRead mv k
              return $ xm / (1.0 + yk / fq) ** fromIntegral (k + 1)

            M.unsafeWrite mv i ((((1.0 + xm) / (1.0 - U.sum sm)) ** (1.0 / fromIntegral (i + 1)) - 1.0) * fq)

          U.unsafeFreeze mv
     in SpotRates {rate = NominalRateCurve {rate = y, freq = fq}}
  getField _ = error "Not implemented"

-- | Change ParRates to SpotRates
instance HasField "toSpote" Rates Rates where
  getField :: Rates -> Rates
  getField ParRates {rate} =
    let (rt, fq) =
          case rate of
            NominalRateCurve {rate = r, freq = f} -> (r, f)
            _ -> error "Not Implemented"

        crt i = ((((1.0 + xm) / (1.0 - sm)) ** (1.0 / fromIntegral (i + 1)) - 1.0) * fq)
          where
            xm = (rt ! i) / fq
            sm = U.sum $ U.map (\k -> xm / (1.0 + (y !! k) / fq) ** fromIntegral (k + 1)) ([0 .. i - 1] :: U.Vector Int)

        y = (rt ! 0) : [crt j | j <- [1 ..]]
     in SpotRates {rate = NominalRateCurve {rate = U.fromList $ take (U.length rt) y, freq = fq}}
  getField _ = error "Not implemented"

-- | Change SpotRates to ParRates
instance HasField "toPar" Rates Rates where
  getField :: Rates -> Rates
  getField SpotRates {rate} =
    let (rt, fq) =
          case rate of
            NominalRateCurve {rate = r, freq = f} -> (r, f)
            _ -> error "Not Implemented"
     in ParRates
          { rate =
              NominalRateCurve
                { rate =
                    U.map
                      ( \i -> fq * (1.0 - 1.0 / (1.0 + (rt ! i) / fq) ** fromIntegral (i + 1)) / U.sum (U.map (\k -> 1.0 / (1.0 + (rt ! k) / fq) ** fromIntegral (k + 1)) [0 .. i])
                      )
                      [0 .. U.length rt - 1],
                  freq = fq
                }
          }
  getField _ = error "Not implemented"

-- | Estimate Rate at a time period
--
-- - y = time period at which rate is to be estimated
instance HasField "rateEstimate" Rates (Double -> Double) where
  getField :: Rates -> Double -> Double
  getField rt = rt.rate.rateEstimate

-- | Estimate the forward rate for a given forward period of a given tenor
--
-- - forward_period    = forward period start point
-- - tenor             = tenor of the forward period
instance HasField "forwardRate" Rates (Double -> Double -> Double) where
  getField :: Rates -> Double -> Double -> Double
  getField (SpotRates {rate}) forwardPeriod tenor =
    let ft = forwardPeriod + tenor
        f = case rate of
          NominalRateCurve {rate = _, freq} -> freq
          _ -> error "Not implemented"
        rEstim = rate.rateEstimate
     in ( ( (1.0 + rEstim ft / f) ** (ft * f)
              / (1.0 + rEstim forwardPeriod / f) ** (forwardPeriod * f)
          )
            ** (1.0 / (tenor * f))
            - 1.0
        )
          * f
  getField _ _ _ = error "Not implemented"