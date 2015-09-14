{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Streams

data Bit = Zero | One  deriving (Show)

-- | Basic boolean functions on Bit
xor :: Bit -> Bit -> Bit 
xor Zero Zero = Zero
xor One One   = Zero
xor _ _       = One

nott :: Bit -> Bit
nott Zero = One
nott One = Zero

nxor :: Bit -> Bit -> Bit
nxor b1 b2 = nott  (xor b1 b2)

-- | An FSR is just list of Bit with a list of taps that have corresponding Bit values
type FSR     = [Bit] 
type TapList = [Int]
type BitList = [Bit]

taps :: FSR -> TapList -> BitList
taps ( []) _  = [Zero]
taps fsr taps = [ fsr !! i | i <- taps]

mapFSR :: (Bit -> Bit) -> FSR -> FSR
mapFSR _ ( []) =  []
mapFSR f ( bits) =  (map f bits)

-- | Shift all to the right and put a Bit into the first position
shiftR :: FSR -> Bit  -> FSR
shiftR [] b =  [b]
shiftR bits b =   b : init bits


-- | Helper functions
asString :: FSR -> String
asString fsr  = foldl (\acc x -> acc ++ bitAsStr x) "" fsr where
	bitAsStr Zero = " 0 "
	bitAsStr One  = " 1 "

fromStr :: String -> FSR
fromStr [] = []
fromStr str = foldl (\acc x -> acc ++ f x) [] str where
	f '1' = [One]
	f '0' = [Zero]
	f  _  = [One] 

asInteger :: FSR -> Integer
asInteger bits = sum [ (f (bits !! n) ) * 2^n | n <- [0.. (length bits) -1]] where
	f Zero = 0
	f One  = 1
	

latch :: FSR -> TapList -> FSR
latch fsr tapList = shiftR fsr nb where 
	nb =  foldl (\acc x -> xor x acc )  first rest where
		(first:rest) = taps fsr tapList


start :: FSR 
start = [One, Zero, One, One, One, Zero, One, One, One, Zero, One, Zero, One, One, One, One, One]

fsrStream :: FSR -> Stream Integer
fsrStream fsr = Cons (asInteger fsr') (fsrStream fsr') where fsr' = latch fsr [13,16]
