module Data.Char.Properties.BidiCategoryTypes where

data BidiCategory
  = BidiL
  | BidiLRE
  | BidiLRO
  | BidiR
  | BidiAL
  | BidiRLE
  | BidiRLO
  | BidiPDF
  | BidiEN
  | BidiES
  | BidiET
  | BidiAN
  | BidiCS
  | BidiNSM
  | BidiBN
  | BidiB
  | BidiS
  | BidiWS
  | BidiON
  deriving (Eq)
