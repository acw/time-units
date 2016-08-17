{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |This module defines types for many useful time periods, as well as
-- mechanisms for converting between them.
module Data.Time.Units(
         TimeUnit(..)
       , Attosecond
       , Femtosecond
       , Picosecond
       , Nanosecond
       , Microsecond
       , Millisecond
       , Second
       , Minute
       , Hour
       , Day
       , Week
       , Fortnight
       , addTime
       , subTime
       , convertUnit
       , getCPUTimeWithUnit
       )
 where

import Data.Aeson.TH (Options (omitNothingFields), defaultOptions,
                      deriveJSON)
import Data.Ix(Ix)
import Data.Data(Data)
import Data.List(isPrefixOf)
import Data.Typeable(Typeable)
import System.CPUTime

-- |A generic class that describes all the units of time. We use microseconds
-- here because that tends to be what GHC (at least) tends to use as its
-- system-level minimum tick size.
class TimeUnit a where
  -- |Converts the given unit of time into microseconds, flooring the value
  -- if it comes to a fractional number of microseconds. (In other words:
  -- be careful, you may lose precision!)
  toMicroseconds   :: a -> Integer
  -- |Converts the given number of microseconds into the unit of time, flooring
  -- the value if it comes to a fraction number of the given unit. (In other
  -- words: be careful, you may lose precision!)
  fromMicroseconds :: Integer -> a

-- |Add two times together to get a useful third time unit. As per usual,
-- you'll want to make sure that you are careful regarding precision. This
-- function goes through microseconds as an intermediary form.
addTime :: (TimeUnit a, TimeUnit b, TimeUnit c) => a -> b -> c
addTime x y = fromMicroseconds (toMicroseconds x + toMicroseconds y)

-- |Subtract the second time from the first, to get a useful third time unit.
-- As per usual, you'll want to make sure that you are careful regarding
-- precision. This function goes through microseconds as an intermediary form.
subTime :: (TimeUnit a, TimeUnit b, TimeUnit c) => a -> b -> c
subTime x y = fromMicroseconds (toMicroseconds x - toMicroseconds y)

-- |Convert one time unit to another. Note that if you move from a smaller
-- time unit to a larger one, or between two time units smaller than a
-- microsecond, you will lose precision.
convertUnit :: (TimeUnit a, TimeUnit b) => a -> b
convertUnit = fromMicroseconds . toMicroseconds

-- |Get the current CPU time in your favorite units. This is probably not
-- very useful in itself, but is likely useful for comparison purposes ...
getCPUTimeWithUnit :: TimeUnit a => IO a
getCPUTimeWithUnit =
  (fromMicroseconds . toMicroseconds . Picosecond) `fmap` getCPUTime

--

newtype Attosecond  = Attosecond  Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Attosecond where
  toMicroseconds (Attosecond x) = x `div` (10 ^ 12)
  fromMicroseconds x            = Attosecond (x * (10 ^ 12))
instance Show Attosecond where
  show (Attosecond x) = show x ++ "as"
instance Read Attosecond where
  readsPrec = readUnit Attosecond "as"

readUnit :: (Integer -> a) -> String ->
            Int -> String ->
            [(a, String)]
readUnit builder unitstr prec str = processItems builder (readsPrec prec str)
 where
  processItems :: (Integer -> a) -> [(Integer,String)] -> [(a,String)]
  processItems builder  [] = []
  processItems builder ((a,s):rest)
    | unitstr `isPrefixOf` s =
           (builder a, drop (length unitstr) s) : (processItems builder rest)
        | otherwise              =
           processItems builder rest

--

newtype Femtosecond = Femtosecond Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Femtosecond where
  toMicroseconds (Femtosecond x) = x `div` (10 ^ 9)
  fromMicroseconds x             = Femtosecond (x * (10 ^ 9))
instance Show Femtosecond where
  show (Femtosecond x) = show x ++ "fs"
instance Read Femtosecond where
  readsPrec = readUnit Femtosecond "fs"

--

newtype Picosecond  = Picosecond  Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Picosecond where
  toMicroseconds (Picosecond x) = x `div` (10 ^ 6)
  fromMicroseconds x            = Picosecond (x * (10 ^ 6))
instance Show Picosecond where
  show (Picosecond x) = show x ++ "ps"
instance Read Picosecond where
  readsPrec = readUnit Picosecond "ps"

--

newtype Nanosecond  = Nanosecond  Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Nanosecond where
  toMicroseconds (Nanosecond x) = x `div` (10 ^ 3)
  fromMicroseconds x            = Nanosecond (x * (10 ^ 3))
instance Show Nanosecond where
  show (Nanosecond x) = show x ++ "ns"
instance Read Nanosecond where
  readsPrec = readUnit Nanosecond "ns"

--

newtype Microsecond = Microsecond Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Microsecond where
  toMicroseconds (Microsecond x) = x
  fromMicroseconds x             = Microsecond x
instance Show Microsecond where
  show (Microsecond x) = show x ++ "µs"
instance Read Microsecond where
  readsPrec = readUnit Microsecond "µs"

$(deriveJSON defaultOptions {omitNothingFields = True}
             ''Microsecond)

--

newtype Millisecond  = Millisecond  Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Millisecond where
  toMicroseconds (Millisecond x) = x * (10 ^ 3)
  fromMicroseconds x             = Millisecond (x `div` (10 ^ 3))
instance Show Millisecond where
  show (Millisecond x) = show x ++ "ms"
instance Read Millisecond where
  readsPrec = readUnit Millisecond "ms"

--

newtype Second      = Second      Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Second where
  toMicroseconds (Second x) = x * (10 ^ 6)
  fromMicroseconds x        = Second (x `div` (10 ^ 6))
instance Show Second where
  show (Second x) = show x ++ "s"
instance Read Second where
  readsPrec = readUnit Second "s"

--

newtype Minute      = Minute      Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Minute where
  toMicroseconds (Minute x) = x * (toMicroseconds $ Second 60)
  fromMicroseconds x        = Minute (x `div` (toMicroseconds $ Second 60))
instance Show Minute where
  show (Minute x) = show x ++ "m"
instance Read Minute where
  readsPrec = readUnit Minute "m"

--

newtype Hour        = Hour        Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Hour where
  toMicroseconds (Hour x) = x * (toMicroseconds $ Minute 60)
  fromMicroseconds x      = Hour (x `div` (toMicroseconds $ Minute 60))
instance Show Hour where
  show (Hour x) = show x ++ "h"
instance Read Hour where
  readsPrec = readUnit Hour "h"

--

newtype Day         = Day         Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Day where
  toMicroseconds (Day x) = x * (toMicroseconds $ Hour 24)
  fromMicroseconds x     = Day (x `div` (toMicroseconds $ Hour 24))
instance Show Day where
  show (Day x) = show x ++ "d"
instance Read Day where
  readsPrec = readUnit Day "d"

--

newtype Week        = Week        Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Week where
  toMicroseconds (Week x) = x * (toMicroseconds $ Day 7)
  fromMicroseconds x      = Week (x `div` (toMicroseconds $ Day 7))
instance Show Week where
  show (Week x) = show x ++ "w"
instance Read Week where
  readsPrec = readUnit Week "w"

--

newtype Fortnight   = Fortnight   Integer
 deriving (Enum,Eq,Integral,Data,Num,Ord,Real,Ix,Typeable)

instance TimeUnit Fortnight where
  toMicroseconds (Fortnight x) = x * (toMicroseconds $ Week 2)
  fromMicroseconds x           = Fortnight (x `div` (toMicroseconds $ Week 2))
instance Show Fortnight where
  show (Fortnight x) = show x ++ "fn"
instance Read Fortnight where
  readsPrec = readUnit Fortnight "fn"

