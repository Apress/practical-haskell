module Main where

import Control.DeepSeq
import Control.Monad.Par

main :: IO ()
main = print (findTwoFactors 1024 3047)

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                 in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)

findTwoFactorsPre :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactorsPre x y = (findFactors x, findFactors y)

findTwoFactors :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)
