-- Generated from checked SessionBoundary Read transitions; do not hand-edit.
module SessionModelCases (SessionCase(..), sessionCases) where
import Data.Int (Int64)
-- Active token 1/6, live actor 1/2, token id, acting id, observed grant.
data SessionCase = SessionCase Bool Bool Bool Bool Int64 Int64 Bool deriving Show
sessionCases :: [SessionCase]
sessionCases =
  [ SessionCase False False False False 1 1 False
  , SessionCase False False False False 1 2 False
  , SessionCase False False False False 6 1 False
  , SessionCase False False False False 6 2 False
  , SessionCase False False False True 1 1 False
  , SessionCase False False False True 1 2 False
  , SessionCase False False False True 6 1 False
  , SessionCase False False False True 6 2 False
  , SessionCase False False True False 1 1 False
  , SessionCase False False True False 1 2 False
  , SessionCase False False True False 6 1 False
  , SessionCase False False True False 6 2 False
  , SessionCase False False True True 1 1 False
  , SessionCase False False True True 1 2 False
  , SessionCase False False True True 6 1 False
  , SessionCase False False True True 6 2 False
  , SessionCase False True False False 1 1 False
  , SessionCase False True False False 1 2 False
  , SessionCase False True False False 6 1 False
  , SessionCase False True False False 6 2 False
  , SessionCase False True False True 1 1 False
  , SessionCase False True False True 1 2 False
  , SessionCase False True False True 6 1 False
  , SessionCase False True False True 6 2 False
  , SessionCase False True True False 1 1 False
  , SessionCase False True True False 1 2 False
  , SessionCase False True True False 6 1 True
  , SessionCase False True True False 6 2 False
  , SessionCase False True True True 1 1 False
  , SessionCase False True True True 1 2 False
  , SessionCase False True True True 6 1 True
  , SessionCase False True True True 6 2 False
  , SessionCase True False False False 1 1 False
  , SessionCase True False False False 1 2 False
  , SessionCase True False False False 6 1 False
  , SessionCase True False False False 6 2 False
  , SessionCase True False False True 1 1 False
  , SessionCase True False False True 1 2 False
  , SessionCase True False False True 6 1 False
  , SessionCase True False False True 6 2 False
  , SessionCase True False True False 1 1 True
  , SessionCase True False True False 1 2 False
  , SessionCase True False True False 6 1 False
  , SessionCase True False True False 6 2 False
  , SessionCase True False True True 1 1 True
  , SessionCase True False True True 1 2 False
  , SessionCase True False True True 6 1 False
  , SessionCase True False True True 6 2 False
  , SessionCase True True False False 1 1 False
  , SessionCase True True False False 1 2 False
  , SessionCase True True False False 6 1 False
  , SessionCase True True False False 6 2 False
  , SessionCase True True False True 1 1 False
  , SessionCase True True False True 1 2 False
  , SessionCase True True False True 6 1 False
  , SessionCase True True False True 6 2 False
  , SessionCase True True True False 1 1 True
  , SessionCase True True True False 1 2 False
  , SessionCase True True True False 6 1 True
  , SessionCase True True True False 6 2 False
  , SessionCase True True True True 1 1 True
  , SessionCase True True True True 1 2 False
  , SessionCase True True True True 6 1 True
  , SessionCase True True True True 6 2 False
  ]
