{-# LANGUAGE CPP #-}
module Data.Array.Accelerate.Cmp (run, run1) where

import Data.Array.Accelerate hiding (zip,(++),tail,fst,snd,map,null)
import qualified Data.Array.Accelerate.Interpreter as I

import System.IO.Unsafe
import System.Environment

#ifdef USE_CUDA
import qualified Data.Array.Accelerate.CUDA as CUDA
#endif

runs :: Arrays a => [(String,Acc a -> a)]
#ifdef USE_CUDA
runs = [("CUDA",CUDA.run),("INTERP",I.run)]
#else
runs = [("INTERP",I.run)]
#endif

cmd :: String
cmd = unsafePerformIO $ do
        env <- getEnvironment
        case lookup "ACCEL_BACKEND" env of
          Nothing -> return "CMP"      -- default to cmp
          Just s | s `elem` cmds
                 -> return s           -- CUDA | INTERP | CMP
          Just s -> error $ "accelerate-cmp failed (" ++ show s ++ ")  expecting : " ++ show cmds

cmds :: [String]
cmds = ["CUDA","INTERP","CMP"]

class ArrCmp a where
  arrEq :: a -> a -> Bool
  arrShow :: a -> String

instance (Eq a, Show a) => ArrCmp (Array sh a) where
  arrEq a b = toList a == toList b
  arrShow = show . toList

run1 :: (ArrCmp b, Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b

run1
#ifdef USE_CUDA
     | cmd == "CUDA" = CUDA.run1                -- only use run1 for CUDA
#endif
     | otherwise     = \ f -> run . f . use

run :: (ArrCmp a, Arrays a) => Acc a -> a
run acc = report rss
  where
        r = runs        -- to share the type with error message
        ress = [ (ty,f acc) | (ty,f) <- r, cmd == "CMP" || cmd == ty ]
        rss = ress `zip` tail ress

        report [] | null ress = error $ "can not find run for " ++ cmd ++ ", looked at " ++ show (map fst r)
                  | otherwise = snd $ head ress
        report (((xn,x),(yn,y)):xs)
                | x `arrEq` y = report xs
                | otherwise = error $ unlines
                                [ "found different results in run"
                                , xn ++ ":"
                                , arrShow x
                                , yn ++ ":"
                                , arrShow y
                                ]
