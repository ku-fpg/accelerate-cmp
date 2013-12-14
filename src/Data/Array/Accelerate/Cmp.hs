module Data.Array.Accelerate.Cmp where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Interpreter as I

-- First, get it working for the model
run :: Arrays a => Acc a -> a
run = I.run

