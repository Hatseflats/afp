{-# LANGUAGE TypeSynonymInstances #-}

import GHC.Generics

class GRead a where
	greadsPrec :: Int -> ReadS a

instance GRead Char   where greadsPrec = showsPrec
instance GRead Int    where greadsPrec = showsPrec
instance GRead Float  where greadsPrec = showsPrec
instance GRead String where greadsPrec = showsPrec
instance GRead Bool   where greadsPrec = showsPrec