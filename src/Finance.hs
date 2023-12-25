-- |
-- Implement Base modules for the FinQuant library
--
-- Module      : Finance <br>
-- Copyright   : (c) 2024 Kishaloy Neogi <br>
-- License     : MIT <br>
-- Maintainer  : Kishaloy Neogi <br>
-- Email       : <nkishaloy@yahoo.com>
--
-- The module describes the base modules of Finance like npv,xnpv,irr,xirr,time value of money etc.
--
-- PV is mentioned as PV, Future value as FV and Terminal value as TV
--
-- You may see the github repository at <https://github.com/n-kishaloy/finQuant>
module Finance
  ( yearFrac,
    DayCountConvention (..),
    dayToFloat,
    dayFromFloat,
    Period,
    yrFrac,
    Currency (..),
    disFact,
    xdisFact,
    forwardRate,
    pv,
    fv,
    invPeriod,
    xpv,
    newtRaph,
    xfv,
    pmt,
    pvAnnuity,
    npvT,
    npv,
    xnpv,
    irr,
    xirr,
    yearFracVec,
  )
where

import Data.Time (Day, diffDays, toGregorian)
import Data.Time.Calendar.OrdinalDate (Year, fromOrdinalDate, isLeapYear, toOrdinalDate)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import GHC.Generics (Generic)

type Period = (Day, Day)

invPeriod :: Period -> Period
invPeriod (x0, x1) = (x1, x0)

newtRaph :: (Double -> Double) -> Double -> Double -> Maybe Double
newtRaph f x xtol = nRx x (100 :: Int)
  where
    dx = xtol / 10.0
    nRx _ 0 = Nothing
    nRx y n = if abs dy < xtol then Just (y - dy) else nRx (y - dy) (n - 1)
      where
        z = f y
        dy = dx * z / (f (y + dx) - z)

daysInYear :: Year -> Double
daysInYear y = if isLeapYear y then 366.0 else 365.0

dayToFloat :: Day -> Double
dayToFloat dt =
  let (y, d) = toOrdinalDate dt
   in fromIntegral y + fromIntegral d / daysInYear y

dayFromFloat :: Double -> Day
dayFromFloat yr =
  let y = floor (yr - 0.00274)
   in fromOrdinalDate y (round ((yr - fromIntegral y) * daysInYear y))

-- | Enum defining different Days convention
--
-- - US30360 => US 30/360 or NASD 30/360
-- - EU30360 => EURO 30/360
-- - ACTACT => Actual days in Leap year / 366 + Actual days in Normal year / 365
-- - ACT360 => Actual nos of days / 360
-- - ACT365 => Actual nos of days / 365
data DayCountConvention
  = US30360
  | EU30360
  | ACTACT
  | ACT360
  | ACT365
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

-- | @yearFrac dayCon d0 d1 = Day difference (d1 - d0) in fraction of a year@
--
-- - dayConv = Days Convention
-- - d0 = reference date
-- - d1 = target date
--
-- Following methods are supported
-- - US30360 => US 30/360 or NASD 30/360
-- - EU30360 => EURO 30/360
-- - ACTACT => Days in Leap year / 366 + Days in Normal year / 365
-- - ACT360 => Actual nos of days / 360
-- - ACT365 => Actual nos of days / 365
--
-- Note that the ACTACT formula is different from MS Excel and follows the Actual/Actual ISDA rule. For more details refer <https://en.wikipedia.org/wiki/Day_count_convention>.
--
-- The yearfrac function is also signed with the result coming as negative in case dt0 > dt1. This is different from MS Excel, where the yearfrac number return absolute difference between the dates.
yearFrac :: DayCountConvention -> Day -> Day -> Double
yearFrac ACT360 d0 d1 = fromIntegral (d1 `diffDays` d0) / 360.0
yearFrac ACT365 d0 d1 = fromIntegral (d1 `diffDays` d0) / 365.0
yearFrac ACTACT d0 d1 = dayToFloat d1 - dayToFloat d0
yearFrac dayConv d0 d1 =
  let dayCountFactor yr0 mt0 dx0 yr1 mt1 dx1 = fromIntegral ((yr1 - yr0) * 360 + (fromIntegral mt1 - fromIntegral mt0) * 30 + (fromIntegral dx1 - fromIntegral dx0)) / 360.0
      (y0, m0, dt0) = toGregorian d0
      (y1, m1, dt1) = toGregorian d1
   in case dayConv of
        EU30360 ->
          let lastDay d = if d == 31 then 30 else d
           in dayCountFactor y0 m0 (lastDay dt0) y1 m1 (lastDay dt1)
        US30360 ->
          let lsFeb y m d = m == 2 && if isLeapYear y then d == 29 else d == 28
              (dx0, dx1) =
                if lsFeb y0 m0 dt0
                  then (30, if lsFeb y1 m1 dt1 then 30 else dt1)
                  else (dt0, dt1)
              dy1 = if dx1 == 31 && dx0 >= 30 then 30 else dx1
              dy0 = if dx0 == 31 then 30 else dx0
           in dayCountFactor y0 m0 dy0 y1 m1 dy1

yearFracVec :: DayCountConvention -> V.Vector Day -> Day -> U.Vector Double
yearFracVec dayConv di d0 = U.convert $ V.map (yearFrac dayConv d0) di

yrFrac :: Period -> Double
yrFrac (d0, d1) = yearFrac US30360 d0 d1

data Currency
  = INR
  | USD
  | NGN
  | EUR
  | GBP
  | CNY
  | MZN
  | ZAR
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

-- | Discount factor = 1/(1+r)^n
-- - r = rate per period
-- - n = nos of periods
disFact :: Double -> Double -> Double
disFact r n = 1.0 / (1.0 + r) ** n

-- | Discount Factor between period = 1 / (1+r) ** (yrFrac d0 d1)
-- - r  = rate for 1 year during period (d0, d1)
-- - (d0, d1) = (begin date, end date)
xdisFact :: Double -> Period -> Double
xdisFact r p = disFact r (yrFrac p)

-- | Forward rate between t0 and t1 given as Double
-- - (r0, t0) = Tuple of Rate and Time in (0, t0)
-- - (r1, t1) = Tuple of Rate and Time in (0, t1)
forwardRate :: (Double, Double) -> (Double, Double) -> Double
forwardRate (r0, t0) (r1, t1) = disFact r1 t1 / disFact r0 t0

-- | PV of a Future cash flow
-- - r  = Effective rate of return
-- - n  = number of periods
-- - f  = Future cash flow
pv :: Double -> Double -> Double -> Double
pv r n f = f / (1.0 + r) ** n

-- | FV of a Present cash flow
-- - r  = Effective rate of return
-- - n  = number of periods
-- - p  = Present cash flow
fv :: Double -> Double -> Double -> Double
fv r n p = p * (1.0 + r) ** n

-- | PV of a Future cash flow
-- - r  = Effective rate of return
-- - pr = Period of discounting
-- - f = Future cash flow
xpv :: Double -> Period -> Double -> Double
xpv r pr = pv r (yrFrac pr)

-- | FV of a Present cash flow
-- - r  = Effective rate of return
-- - pr = Period of discounting
-- - p = Present cash flow
xfv :: Double -> Period -> Double -> Double
xfv r pr = fv r (yrFrac pr)

-- | Payment to cover the PV of an Annuity
-- - r  = rate of return per period
-- - n  = number of periods (say, years)
-- - p  = PV of Annuity
-- - f  = FV of Residual value that may be received
pmt :: Double -> Double -> Double -> Double -> Double
pmt r n p f = -(p + f / rn) * r / (1.0 - 1.0 / rn)
  where
    rn = (1.0 + r) ** n

-- | PV of an annuity
-- - r   = rate of return per period
-- - n   = number of periods (say, years)
-- - pm  = payment made in each transaction
-- - f  = FV of Residual value that may be received
pvAnnuity :: Double -> Double -> Double -> Double -> Double
pvAnnuity r n pm f = -pm / r * (1.0 - 1.0 / rn) - f / rn
  where
    rn = (1.0 + r) ** n

-- | NPV of cash flows against time given in periods @ time = 0.0
-- - r   = rate of return across the periods
-- - ti  = vector of time of cash flows given as Double
-- - cf  = vector of corresponding cash flows
npvT :: Double -> U.Vector Double -> U.Vector Double -> Double
npvT r ti cf = U.sum $ U.zipWith (\t c -> c / r1 ** t) ti cf where r1 = 1.0 + r

-- | NPV of cash flows against time given in periods @ time = t0
-- - r   = rate of return across the periods
-- - ti  = vector of time of cash flows given as Double
-- - t0  = time period at which the NPV is sought. Essentially, NPV(ti - t0)
-- - cf  = vector of corresponding cash flows
npv :: Double -> U.Vector Double -> Double -> U.Vector Double -> Double
npv r ti t0 cf = npvT r ti cf * (1.0 + r) ** t0

-- | NPV of cash flows against time given by Date
-- - r   = rate of return across the years
-- - di  = vector of time of cash flows given as Date
-- - d0  = Date at which the NPV is sought.
-- - cf  = vector of corresponding cash flows
xnpv :: Double -> V.Vector Day -> Day -> U.Vector Double -> Double
xnpv r di d0 = npvT r (yearFracVec US30360 di d0)

-- | IRR of cash flow against time given in periods
-- - ti  = vector of time of cash flows given as Double
-- - cf  = vector of corresponding cash flows
irr :: U.Vector Double -> U.Vector Double -> Maybe Double
irr ti cf = newtRaph (\r -> npvT r ti cf) 0.1 1e-6

-- | IRR of cash flow against time given as Date
-- - di  = vector of time of cash flows given as Date
-- - cf  = vector of corresponding cash flows
xirr :: V.Vector Day -> U.Vector Double -> Maybe Double
xirr di = irr (yearFracVec US30360 di (di V.! 0))
