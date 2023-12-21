module Main (main) where

import Data.List (foldl')

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
main = putStrLn "Test suite not yet implemented."
