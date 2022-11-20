
{-# LANGUAGE MagicHash #-}

module CeilingLog where

import GHC.Base (Int (..), isTrue#, (+#), (==#))
import GHC.Integer.Logarithms (integerLogBase#)


-- | \x y -> ceiling (logBase x y), x > 1 && y > 0
--
clogBase :: Integer -> Integer -> Maybe Int
clogBase x y | x > 1 && y > 0 =
  case y of
    1 -> Just 0
    _ -> let z1 = integerLogBase# x y
             z2 = integerLogBase# x (y-1)
         in  if isTrue# (z1 ==# z2)
                then Just (I# (z1 +# 1#))
                else Just (I# z1)
clogBase _ _ = Nothing
