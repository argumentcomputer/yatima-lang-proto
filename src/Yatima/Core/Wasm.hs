{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Yatima.Core.WASM
-- Description : Defines utilities for Yatima's WASM numeric primitives
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This module modifies work by [Ilya Rezvov](https://github.com/SPY/haskell-wasm),
-- which is released under MIT terms included with this package in the
-- @licenses/2018_Ilya_Rezvov@ file
module Yatima.Core.Wasm where

import Data.Array.ST (MArray, STUArray, newArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Bits (complement, shiftR, xor, (.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import Data.Int
import Data.Serialize
import Data.Serialize.IEEE754
import Data.Word
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)
import GHC.ST (ST, runST)
import Numeric.Extras hiding (floor)
import Numeric.IEEE
import Numeric.IEEE (IEEE, copySign, identicalIEEE, maxNum, minNum)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast ::
  ( MArray (STUArray s) a (ST s),
    MArray (STUArray s) b (ST s)
  ) =>
  a ->
  ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

makeNaN :: Word64 -> Double
makeNaN w = wordToDouble $ 0x7FF0000000000000 .|. (0x000FFFFFFFFFFFFF .&. w)

doubleToFloat :: Double -> Float
doubleToFloat d =
  let w = doubleToWord d
   in if 0x7FF0000000000000 == (w .&. 0x7FF0000000000000) && (w .&. 0x0007FFFFFFFFFFFF) /= 0
        then wordToFloat $ fromIntegral $ ((0x8000000000000000 .&. w) `shiftR` 32) .|. 0x7F800000 .|. (0x7FFFFF .&. w)
        else realToFrac d

bytesToWord64 :: [Word8] -> Word64
bytesToWord64 (b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : _) =
  sh (f b0) 0 + sh (f b1) 8 + sh (f b2) 16 + sh (f b3) 24
    + sh (f b4) 32
    + sh (f b5) 40
    + sh (f b6) 48
    + sh (f b7) 56
  where
    f = fromIntegral
    sh = Bits.shift

word64ToBytes :: Word64 -> [Word8]
word64ToBytes n =
  f
    <$> [ n .&. 0xFF,
          (sh n (-8)) .&. 0xFF,
          (sh n (-16)) .&. 0xFF,
          (sh n (-24)) .&. 0xFF,
          (sh n (-32)) .&. 0xFF,
          (sh n (-40)) .&. 0xFF,
          (sh n (-48)) .&. 0xFF,
          (sh n (-56)) .&. 0xFF
        ]
  where
    f = fromIntegral
    sh = Bits.shift

ftou :: Double -> Word64
ftou n = bytesToWord64 $ B.unpack $ runPut $ putFloat64le n

utof :: Word64 -> Double
utof = wordToDouble

asInt32 :: Word32 -> Int32
asInt32 w =
  if w < 0x80000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFF - w + 1)

asInt64 :: Word64 -> Int64
asInt64 w =
  if w < 0x8000000000000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFFFFFFFFFF - w + 1)

asWord32 :: Int32 -> Word32
asWord32 i
  | i >= 0 = fromIntegral i
  | otherwise = 0xFFFFFFFF - (fromIntegral (abs i)) + 1

asWord64 :: Int64 -> Word64
asWord64 i
  | i >= 0 = fromIntegral i
  | otherwise = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1

nearest :: (IEEE a) => a -> a
nearest f
  | isNaN f = f
  | f >= 0 && f <= 0.5 = copySign 0 f
  | f < 0 && f >= -0.5 = -0
  | otherwise =
    let i = floor f :: Integer
     in let fi = fromIntegral i
         in let r = abs f - abs fi
             in flip copySign f $
                  ( if r == 0.5
                      then
                        ( case (even i, f < 0) of
                            (True, _) -> fi
                            (_, True) -> fi - 1.0
                            (_, False) -> fi + 1.0
                        )
                      else fromIntegral (round f :: Integer)
                  )

zeroAwareMin :: IEEE a => a -> a -> a
zeroAwareMin a b
  | identicalIEEE a 0 && identicalIEEE b (-0) = b
  | isNaN a = a
  | isNaN b = b
  | otherwise = minNum a b

zeroAwareMax :: IEEE a => a -> a -> a
zeroAwareMax a b
  | identicalIEEE a (-0) && identicalIEEE b 0 = b
  | isNaN a = a
  | isNaN b = b
  | otherwise = maxNum a b

floatFloor :: Float -> Float
floatFloor a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (floor a :: Integer)) a

doubleFloor :: Double -> Double
doubleFloor a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (floor a :: Integer)) a

floatCeil :: Float -> Float
floatCeil a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

doubleCeil :: Double -> Double
doubleCeil a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

floatTrunc :: Float -> Float
floatTrunc a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (truncate a :: Integer)) a

doubleTrunc :: Double -> Double
doubleTrunc a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (truncate a :: Integer)) a
