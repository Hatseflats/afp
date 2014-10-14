class A a
class (A a) => B a
instance A Bool
instance B Bool
instance A a => A (Maybe a)
instance (A a, B a) => A [a]

{-
Prove:

1. B Int ||- A (Maybe (Maybe Int))
2. ∅ ||- A (Maybe [Bool])
-}

{-
1. 
class (A a) => B a                           
------------------- (super)              
		B a ||- A a      instance A a => A (Maybe a)                    
        --------------------------------------------- (inst)       
				B a ||- A (Maybe a)      instance A a => A (Maybe a)    
            	----------------------------------------------------- (inst)
              			B a ||- A (Maybe (Maybe a))
              			--------------------------- (closure)
              					B Int ||- A (Maybe (Maybe Int))
-}