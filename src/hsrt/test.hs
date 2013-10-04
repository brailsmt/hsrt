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
        p1 <- vectorOf 3 (arbitrary::Gen Double)  -- A list of 3 random doubles
        p2 <- vectorOf 3 (arbitrary::Gen Double)  -- A different list of 3 random doubles
        return $ Ray p1 p2

-- The idempotency fails due to floating point precision issues...
--prop_normalIdempotency ray = (direction ray) /= [0,0,0] ==> (normalize . normalize) ray == (normalize . normalize . normalize) ray

-- Test the length property of normalized rays.  That is the length of a normalized ray is always 1.
prop_lengthOfNormalizedRay ray = (direction ray) /= [0,0,0] ==> 1-fudgefactor <= _len && _len <= 1+fudgefactor
    where 
        _len = ((distance [0,0,0]) . direction . normalize) ray 
        fudgefactor = 0.0000000000001
        
-- Test the dot product of normalized rays, which is always 1
prop_normalizeDot p1 = (direction p1) /= [0,0,0] ==> 1-fudgefactor <= _dot && _dot <= 1+fudgefactor
    where 
        np1 = normalize p1
        _dot = np1 `dot` np1
        fudgefactor = 0.0000000000001
