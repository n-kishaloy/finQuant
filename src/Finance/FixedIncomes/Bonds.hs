-- | Implement Fixed Incomes modules for the financelib library
--
-- Module      : financelib::fixedIncomes::bonds <br>
-- Copyright   : (c) 2022 Kishaloy Neogi <br>
-- License     : MIT <br>
-- Maintainer  : Kishaloy Neogi <br>
-- Email       : <nkishaloy@yahoo.com>
--
-- The module describes the base modules of Bonds.
--
-- You may see the github repository at <https://github.com/n-kishaloy/finQuant>
module Finance.FixedIncomes.Bonds
  ( CouponBond (..),
    FloatingRateNotes (..),
  )
where

import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as U
import Finance qualified as F
import Finance.FixedIncomes.Bonds.Rates qualified as R
import GHC.Generics (Generic)
import GHC.Records (HasField (..))

-- | CouponBond : struct defining a Coupon bond..
--
-- - par   = Par value
-- - c     = Coupon rate per period
-- - freq  = Frequency of coupon payment per period
-- - T     = Life of the Bond
data CouponBond = CouponBond
  { par :: Double,
    c :: Double,
    freq :: Double,
    t_life :: Double
  }
  deriving (Show, Generic)

-- | Price of Coupon bond given a discount rate
--
--    - rate  = Discount rate given as Nominal rate
instance HasField "price" CouponBond (Double -> Double) where
  getField :: CouponBond -> Double -> Double
  getField cp rate =
    F.pvAnnuity
      (rate / cp.freq)
      (cp.t_life * cp.freq)
      (-cp.c / cp.freq * cp.par)
      0.0
      + F.pv (rate / cp.freq) (cp.t_life * cp.freq) cp.par

-- | YTM of a Coupon bond given its price
--
--    - price = Price of Coupon bond
instance HasField "ytm" CouponBond (Double -> Double) where
  getField :: CouponBond -> Double -> Double
  getField cp price = fromJust $ F.newtRaph (\r -> cp.price r - price) 0.05 1e-6

-- | Price of Coupon bond given a discount RateCurve
--
--    - rc = Discount rate given as Nominal RateCurve
instance HasField "priceRateCurve" CouponBond (R.RateCurve -> Double) where
  getField :: CouponBond -> R.RateCurve -> Double
  getField cp rc =
    U.sum $
      U.imap
        (\i c -> rc.pv c (fromIntegral (i + 1) / cp.freq))
        cp.generateCashFlow

-- | Generate cash flow of the CouponBond
instance HasField "generateCashFlow" CouponBond (U.Vector Double) where
  getField :: CouponBond -> U.Vector Double
  getField cp =
    let c = cp.par * cp.c / cp.freq
        sz = round $ cp.freq * cp.t_life
        cb = U.replicate sz c
     in U.unsafeUpdate cb [(sz - 1, c + cp.par)]

-- | Calculates the accrued interest when the purchase is t periods into the next cycle.
--
-- Note that t is in period, so for 26 days, in a full period of 1 year, t = 26/360
instance HasField "accruedInterest" CouponBond (Double -> Double) where
  getField :: CouponBond -> Double -> Double
  getField cp t = t * cp.c * cp.par

-- | PV Full when purchase is t periods into next cycle.
instance HasField "pvFull" CouponBond (Double -> Double -> Double) where
  getField :: CouponBond -> Double -> Double -> Double
  getField cp rate t = cp.price rate * (1.0 + rate / cp.freq) ** (t * cp.freq)

-- | PV Flat when purchase is t periods into next cycle.
instance HasField "pvFlat" CouponBond (Double -> Double -> Double) where
  getField :: CouponBond -> Double -> Double -> Double
  getField cp rate t = cp.pvFull rate t - cp.accruedInterest t

-- | CouponBond : struct defining a Coupon bond..
--
-- - par               = Par value
-- - quoted_margin     = Quoted margin payment over the Index rate per period
-- - freq              = Frequency of payment per period
-- - discount_margin
-- - T                  = Life of the Bond
data FloatingRateNotes = FloatingRateNotes
  { par :: Double,
    quotedMargin :: Double,
    freq :: Double,
    t_life :: Double
  }
  deriving (Show, Generic)

-- | Price of Floating Rate Note given a discount rate
--
--    - rate  = Discount rate given as Nominal rate
instance HasField "price" FloatingRateNotes (Double -> Double -> Double) where
  getField :: FloatingRateNotes -> Double -> Double -> Double
  getField fp idxRate discountMargin =
    F.pvAnnuity
      ((idxRate + discountMargin) / fp.freq)
      (fp.t_life * fp.freq)
      (-((fp.quotedMargin + idxRate) * fp.par / fp.freq))
      0.0
      + F.pv
        ((idxRate + discountMargin) / fp.freq)
        (fp.t_life * fp.freq)
        fp.par

-- | Discount margin of a Floating Rate Note given the price and index rate
--
--    - price         = Price of the Floating Rate Note
--    - index_rate    = Index rate
instance HasField "discountMargin" FloatingRateNotes (Double -> Double -> Double) where
  getField :: FloatingRateNotes -> Double -> Double -> Double
  getField fp price idxRate = fromJust $ F.newtRaph (\x -> fp.price idxRate x - price) 0.005 1e-5

-- | Price of Floating Rate Note given a Index RateCurve
--
--    - rc  = Discount rate given as Nominal RateCurve
instance HasField "priceRateCurve" FloatingRateNotes (R.RateCurve -> Double -> Double) where
  getField :: FloatingRateNotes -> R.RateCurve -> Double -> Double
  getField _fp _idxRC _discountMargin = error "Not implemented"

-- | Discount margin of a Floating Rate Note given a Index RateCurve and a Price
instance HasField "discountMarginRateCurve" FloatingRateNotes (Double -> R.RateCurve -> Double) where
  getField :: FloatingRateNotes -> Double -> R.RateCurve -> Double
  getField fp price idxRC = fromJust $ F.newtRaph (\x -> fp.priceRateCurve idxRC x - price) 0.005 1e-6