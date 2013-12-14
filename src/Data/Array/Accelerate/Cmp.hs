{-# LANGUAGE CPP #-}
module Data.Array.Accelerate.Cmp where

import Data.Array.Accelerate hiding (zip,(++),tail)
import qualified Data.Array.Accelerate.Interpreter as I

#ifdef USE_CUDA
import qualified Data.Array.Accelerate.CUDA as CUDA
#endif

runs :: Arrays a => [Acc a -> a]
#ifdef USE_CUDA
runs = [I.run,CUDA.run]
#else
runs = [I.run,I.run]
#endif

class ArrCmp a where
  arrEq :: a -> a -> Bool
  arrShow :: a -> String

instance (Eq a, Show a) => ArrCmp (Array sh a) where
  arrEq a b = toList a == toList b
  arrShow = show . toList

run :: (ArrCmp a, Arrays a) => Acc a -> a
run acc = report rss
  where
        ress = [ f acc | f <- runs ]
        rss = ress `zip` tail ress

        report [] = head ress
        report ((x,y):xs)
                | x `arrEq` y = report xs
                | otherwise = error $ unlines
                                [ "found different results in run"
                                , arrShow x
                                , arrShow y
                                ]
