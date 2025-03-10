module Test where

newtype Mon = Mon String
    deriving (Show, Eq)

instance Semigroup Mon where
    (<>) (Mon a) (Mon b) = Mon (a ++ b)

instance Monoid Mon where
    mempty = Mon ""
