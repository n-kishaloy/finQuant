module Finance
  ( yearFrac,
    DayCountConvention,
    dayToFloat,
    dayFromFloat,
    Period,
    yrFrac,
  )
where

import Data.Time (Day, diffDays, toGregorian)
import Data.Time.Calendar.OrdinalDate (Year, fromOrdinalDate, isLeapYear, toOrdinalDate)
import GHC.Generics (Generic)

type Period = (Day, Day)

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

{-Enum defining different Days convention

- US30360 => US 30/360 or NASD 30/360
- EU30360 => EURO 30/360
- ACTACT => (Actual days in Leap year) / 366 + (Actual days in Normal year) / 365
- ACT360 => Actual nos of days / 360
- ACT365 => Actual nos of days / 365
-}
data DayCountConvention
  = US30360
  | EU30360
  | ACTACT
  | ACT360
  | ACT365
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

{- @yearFrac dayCon d0 d1 = Day difference (d1 - d0) in fraction of a year@

- dayConv = Days Convention
- d0 = reference date
- d1 = target date

Following methods are supported
- US30360 => US 30/360 or NASD 30/360
- EU30360 => EURO 30/360
- ACTACT => (Days in Leap year) / 366 + (Days in Normal year) / 365
- ACT360 => Actual nos of days / 360
- ACT365 => Actual nos of days / 365

Note that the ACTACT formula is different from MS Excel and follows the Actual/Actual ISDA rule. For more details refer <https://en.wikipedia.org/wiki/Day_count_convention>.

The yearfrac function is also signed with the result coming as negative in case dt0 > dt1. This is different from MS Excel, where the yearfrac number return absolute difference between the dates.
-}
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
          let lastDay d = if d == 0 then 30 else d
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

yrFrac :: Period -> Double
yrFrac (d0, d1) = yearFrac US30360 d0 d1