module Main (main) where

import Data.Approx (Approx ((=~)))
import Data.HashMap.Strict qualified as Hm
import Data.List (foldl')
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Finance (DayCountConvention (..), yearFrac)
import Finance qualified as F
import Finance.FixedIncomes.Bonds as B
import Finance.FixedIncomes.Bonds.Rates qualified as Rt
import Finance.Statements qualified as St

qTest :: (Show a) => a -> [Bool] -> IO ()
qTest nam x = putStrLn $ lJus 30 '.' nam ++ " => Tests :" ++ rJus 3 ' ' (p + f) ++ fl f
  where
    (p, f) = foldl' (\(u, v) b -> if b then (u + 1, v) else (u, v + 1)) (0, 0) x :: (Int, Int)
    fl 0 = " => Ok"
    fl z = " => +++++ << FAILED : " ++ show z ++ " >> +++++"
    lJus n c xr = st ++ replicate (n - length st) c where st = show xr
    rJus n c xr = replicate (n - length st) c ++ st where st = show xr

infix 3 `qCheck`

qCheck :: (Show a) => a -> Bool -> IO ()
qCheck nam False = putStrLn $ "*** Error *** : " ++ show nam ++ "\n"
qCheck _ _ = putStr ""

main :: IO ()
main = do
  qCheck ("Test" :: Text) True

  print ("Finance" :: Text)
  qTest
    ("YearFrac" :: Text)
    [ ( ( \(dt0, dt1) ->
            ( yearFrac US30360 dt0 dt1,
              yearFrac ACTACT dt0 dt1,
              yearFrac ACT360 dt0 dt1,
              yearFrac ACT365 dt0 dt1,
              yearFrac EU30360 dt0 dt1
            )
        )
          <$> ( [ (fromGregorian 2018 2 5, fromGregorian 2023 5 14),
                  (fromGregorian 2020 2 29, fromGregorian 2024 2 28),
                  (fromGregorian 2015 8 30, fromGregorian 2010 3 31),
                  (fromGregorian 2016 2 28, fromGregorian 2016 10 30),
                  (fromGregorian 2014 1 31, fromGregorian 2014 8 31),
                  (fromGregorian 2014 2 28, fromGregorian 2014 9 30),
                  (fromGregorian 2016 2 29, fromGregorian 2016 6 15)
                ] ::
                  [(Day, Day)]
              )
      )
        =~ [ ( 5.27500000000000,
               5.26849315068489,
               5.34444444444444444,
               5.271232876712329,
               5.27500000000000
             ),
             ( 3.9944444444444444444444,
               3.9972677595626465,
               4.0555555555555555555555,
               4.00000000000000,
               3.99722222222222222222222
             ),
             ( -5.4166666666666666666,
               -5.4164383561642350000,
               -5.4944444444444444444,
               -5.4191780821917810000,
               -5.4166666666666666666
             ),
             ( 0.6722222222222222222,
               0.6693989071038686000,
               0.6805555555555555555,
               0.6712328767123288000,
               0.6722222222222222222
             ),
             ( 0.5833333333333333333,
               0.5808219178084073000,
               0.5888888888888888888,
               0.5808219178082191000,
               0.5833333333333333333
             ),
             ( 0.5833333333333333333,
               0.5863013698631221000,
               0.5944444444444444444,
               0.5863013698630137000,
               0.5888888888888888888
             ),
             ( 0.2916666666666666666,
               0.2923497267759103000,
               0.2972222222222222222,
               0.2931506849315068700,
               0.2944444444444444444
             )
           ]
    ]
  qTest
    ("Discount factor" :: Text)
    [ F.disFact 0.09 3.0 =~ 0.7721834800610642,
      F.xdisFact 0.09 (fromGregorian 2015 3 15, fromGregorian 2018 10 8) =~ 0.7355566392384189,
      F.forwardRate (0.07, 1.0) (0.09, 3.0) =~ 0.8262363236653387,
      F.nomEffRate (F.effNomRate 0.08 4.0) 4.0 =~ 0.08,
      F.effNomRate (F.nomEffRate 0.08 2.0) 2.0 =~ 0.08
    ]

  qTest
    ("Newton Raphson" :: Text)
    [ F.newtRaph (\x -> (x - 3.0) * (x - 4.0)) 2.0 1e-6 =~ Just 3.0,
      F.newtRaph (\x -> (x - 4.0) ** 2.0) 2.0 1e-6 =~ Just 3.9999990972409805,
      F.newtRaph (\x -> (x - 4.0) ** 2.0 + 5.0) 2.0 1e-6 =~ Nothing
    ]

  qTest
    ("NPV" :: Text)
    [ F.pmt (0.08 / 12.0) (30.0 * 12.0) (-1000.0) 50.0 =~ 7.304096785187425,
      F.pvAnnuity (0.08 / 12.0) (30.0 * 12.0) 7.304096785187425 50.0 =~ -1000.0,
      F.xpv 0.08 (fromGregorian 2020 2 29, fromGregorian 2024 2 28) 5.638
        =~ 4.1458705451340800,
      F.xfv 0.08 (fromGregorian 2020 2 29, fromGregorian 2024 2 28) 5.638
        =~ 7.6671578752761100,
      F.npv 0.08 [0.25, 6.25, 3.5, 4.5, 1.25] (-0.45) [-6.25, 1.2, 1.25, 3.6, 2.5] =~ 0.36962283798505946,
      F.xnpv
        0.08
        [fromGregorian 2012 2 25, fromGregorian 2012 6 28, fromGregorian 2013 2 15, fromGregorian 2014 9 18, fromGregorian 2015 2 20]
        (fromGregorian 2012 1 10)
        [-15.0, 5.0, 25.0, -10.0, 50.0]
        =~ 44.165773653310936,
      F.irr [0.125, 0.29760274, 0.49760274, 0.55239726, 0.812671233] [-10.25, -2.5, 3.5, 9.5, 1.25] =~ Just 0.31813386475187844,
      F.irr [0.125, 0.29760274, 0.49760274, 0.55239726, 0.812671233] [10.25, 2.5, 3.5, 9.5, 1.25] =~ Nothing,
      F.xirr
        [ fromGregorian 2012 2 25,
          fromGregorian 2012 6 28,
          fromGregorian 2013 2 15,
          fromGregorian 2014 9 18,
          fromGregorian 2015 2 20
        ]
        [-115.0, 5.0, 25.0, -10.0, 200.0]
        =~ Just 0.27845538159261773
    ]

  let ep =
        Rt.NominalRateCurve
          { rate = [0.05, 0.06, 0.07, 0.08],
            freq = 2.0
          }

  let et =
        Rt.NominalRateCurve
          { rate = [0.0016, 0.0021, 0.0027, 0.0033, 0.0037, 0.0041],
            freq = 2.0
          }

  let er = et.toEffective
  let en = er.toNominal

  qTest
    ("Rate curves" :: Text)
    [ ep.rateEstimate 1.5 =~ 0.07,
      ep.rateEstimate 1.2 == 0.064,
      en.rate =~ et.rate
    ]

  let rx = [0.020000, 0.024000, 0.027600, 0.030840, 0.033756, 0.036380]
  let ew =
        Rt.ParRates
          { rate =
              Rt.NominalRateCurve
                { rate = rx,
                  freq = 2.0
                }
          }

  let ey = ew.toSpot
  -- let fy = ew.toSpote

  -- print ey
  -- print fy

  let eq = ey.toPar

  -- let Rt.SpotRates {rate = Rt.NominalRateCurve {rate = rt}} = ey
  rt <- case ey of
    Rt.SpotRates {rate = Rt.NominalRateCurve {rate = ry}} -> return ry
    _ -> error "Not implemented"

  rp <- case eq of
    Rt.ParRates {rate = Rt.NominalRateCurve {rate = ry}} -> return ry
    _ -> error "Not implemented"

  -- print rt

  let rq =
        Rt.SpotRates
          { rate =
              Rt.NominalRateCurve
                { rate = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0365, 0.0, 0.0418],
                  freq = 2.0
                }
          }

  qTest
    ("Rates" :: Text)
    [ rt =~ [2.0e-2, 2.4024047953330907e-2, 2.7668566429104757e-2, 3.0973763781325214e-2, 3.397441792873934e-2, 3.6700426487687565e-2],
      rp =~ rx,
      rq.forwardRate 3.0 1.0 =~ 0.057782903318259304
    ]

  let cb = B.CouponBond {t_life = 3.0, par = 100.0, freq = 2.0, c = 0.05}
  let cd = B.CouponBond {t_life = 9.0, par = 100.0, freq = 2.0, c = 0.05}

  qTest
    ("Bonds" :: Text)
    [ cb.price 0.03 =~ 105.6971871654752,
      cb.ytm 113.69147941993403 =~ 0.004038639185260602,
      cb.priceRateCurve
        ( Rt.NominalRateCurve
            { rate = [0.0016, 0.0021, 0.0027, 0.0033, 0.0037, 0.0041],
              freq = 2.0
            }
        )
        =~ 113.69147941993403,
      cb.accruedInterest (88.0 / 362.0) =~ 1.2154696132596685,
      cd.pvFull 0.048 (88.0 / 362.0) =~ 102.62432259347733,
      cd.pvFlat 0.048 (88.0 / 362.0) =~ 101.40885298021766,
      B.FloatingRateNotes {quotedMargin = 0.005, t_life = 2.0, par = 100.0, freq = 2.0}.price 0.0125 0.004 =~ 100.19594209266003,
      B.FloatingRateNotes {quotedMargin = 0.0075, t_life = 5.0, par = 100.0, freq = 4.0}.discountMargin 95.50 0.011 =~ 0.017180561798632237
    ]

  qTest
    ("Statements" :: Text)
    [ St.isCalc St.Inventories,
      St.isCalc St.EAT,
      St.debitType Hm.! St.InterestPayable == St.LiabilityEntry,
      St.debitType Hm.! St.OtherTangibleAssets == St.AssetEntry,
      St.debitType Hm.! St.AccumulatedAmortization == St.AssetContra,
      St.debitType Hm.! St.CommonStock == St.EquityEntry,
      St.debitType Hm.! St.DeferredCompensation == St.LiabilityEntry,
      St.debitType Hm.! St.AccumulatedDepreciation == St.AssetContra,
      isNothing (Hm.lookup St.LongTermLiabilities St.debitType)
    ]

  let ac1 =
        St.FinancialReport
          { profitLoss = Nothing,
            others = Nothing,
            cashFlow = Nothing,
            balanceSheetBegin =
              Just $
                Hm.fromList
                  [(St.Cash, 23.5), (St.Equity, 12.5)],
            balanceSheetEnd = Nothing,
            period = (fromGregorian 2009 5 22, fromGregorian 2010 9 27)
          }

  print ("Bye" :: Text)