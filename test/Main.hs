module Main (main) where

import Data.Approx
import Data.List (foldl')
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Finance (DayCountConvention (..), yearFrac)
import Finance qualified as F

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
      F.forwardRate (0.07, 1.0) (0.09, 3.0) =~ 0.8262363236653387
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
