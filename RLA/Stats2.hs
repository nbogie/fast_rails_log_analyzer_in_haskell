module RLA.Stats2 (makeBinnedText) where

import qualified Data.Map as M
import Data.List (sortBy)

cdf ::  [Int] -> [Double]
cdf xs = decorate $ accum xs
  where
    decorate :: [Int] -> [Double]
    decorate ys = map (\x -> fromIntegral x / fromIntegral tot) ys
    tot = sum xs
    accum ::  (Num a) => [a] -> [a]
    accum xs = accum' xs 0
      where
        accum' [] n = []
        accum' (x:xs) n = (x+n) : accum' xs (x+n)


testdata = [ 99,40,99,201,0,249,99 ]
expected = [ (0,2), (50,3), (200,2) ]

oldmain = print $ zip vals $ cdf vals
  where vals = [1,1,3,5]

main = putStrLn $ makeBinnedText 50 testdata

makeBinnedText binWidth xs = presentBins binWidth $ bin binWidth xs

bin :: Int -> [Int] -> M.Map Int Int
bin binWidth xs = bin' xs M.empty
  where bin' []     bins = bins
        bin' (x:xs) bins = bin' xs $ M.insertWith' (+) k 1 bins
          where k = x `div` binWidth 

type BinMap = M.Map Int Int

presentBins :: Int -> BinMap -> String
presentBins binWidth bmap = unlines $ map f $ cdfBins $ M.assocs bmap
  where f ((k,v), c) = show ((binWidth `div` 2) + k * binWidth) ++ " " ++ show v ++ " " ++ show c

cdfBins ::  [(Int, Int)] -> [((Int, Int),Double)]
cdfBins xs = zip xs $ cdf $ map snd xs
