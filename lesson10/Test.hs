module Test where

type Name = String

data Employee = Employee { name :: Name,
                          ,phone :: String }
                deriving Show


