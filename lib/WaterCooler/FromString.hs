-- FromString.hs

module WaterCooler.FromString
( FromString
, fromString
) where

-- | A type class for reading data into a string.
-- This is an alternative to read intended to be use in cases
-- where a conversion from string is needed that is more flexible then
-- what is auto-derived with Read.
class FromString a where
  fromString :: String -> Maybe a
