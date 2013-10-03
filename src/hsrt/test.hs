module HSRT.Test where

import HSRT
import HSRT.Types

import Test.QuickCheck

---------------------------------------------------
-- QuickCheck testing
---------------------------------------------------

-- Generate random Rays
instance Arbitrary Ray where
    arbitrary = do
        p1 <- vectorOf 3 (arbitrary::Gen Double)
        p2 <- vectorOf 3 (arbitrary::Gen Double)
        return $ Ray p1 p2

-- The idempotency fails due to floating point precision issues...
--prop_normalIdempotency ray = (direction ray) /= [0,0,0] ==> (normalize . normalize) ray == (normalize . normalize . normalize) ray
prop_lengthOfNormalizedRay ray = (direction ray) /= [0,0,0] ==> 1-fudgefactor <= _len && _len <= 1+fudgefactor
    where 
        _len = ((distance [0,0,0]) . direction . normalize) ray 
        fudgefactor = 0.0000000000001
        
prop_normalizeDot p1 = (direction p1) /= [0,0,0] ==> 1-fudgefactor <= _dot && _dot <= 1+fudgefactor
    where 
        np1 = normalize p1
        _dot = np1 `dot` np1
        fudgefactor = 0.0000000000001
