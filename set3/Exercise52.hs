class A a
class (A a) => B a
instance A Bool
instance B Bool
instance A a => A (Maybe a)
instance (A a, B a) => A [a]

{-
1. B Int ||- A (Maybe (Maybe Int))

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

{-
2. ∅ ||- A (Maybe [Bool])

                        instance (A a, B a) => A [a]            instance A a => A (Maybe a)                          
                        ---------------------------(inst)       -----------------------------(inst)
                        (A a, B a) ||- A [a]                    A a ||- A (Maybe a)
                        ------------------------(closure)       --------------------------- (closure)
instance A Bool         (A bool, B bool) ||- A [Bool]           A [Bool] ||- A (Maybe [Bool])
---------------         ------------------------------------------------------------------- (trans)
 ∅ ||- A Bool                           (A bool, B bool) ||- A (Maybe [Bool])
------------------------------------------------------------------------------- (trans)
                         ∅ ||- A (Maybe [Bool])       
-}