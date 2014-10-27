module Exercise52 where

class A a
class (A a) => B a
instance A Bool
instance B Bool
instance A a => A (Maybe a)
instance (A a, B a) => A [a]

{-
1. B Int ||- A (Maybe (Maybe Int))

                                                                         
                                                                        instance A a => A (Maybe a) 
                                                                        ---------------------------- (inst)  
                                   instance A a => A (Maybe a)          A a ||- A (Maybe a)
                                   ---------------------------- (inst)  -------------------- (closure)
class (A a) => B a                 A a ||- A (Maybe a)                  A (Maybe a) ||- A (Maybe (Maybe a))
------------------- (super)        ------------------------------------------------------------------------- (trans)
        B a ||- A a                A a ||- A (Maybe (Maybe a))
        ------------------------------------------------------- (trans)
                        B a ||- A (Maybe (Maybe a))
                        --------------------------- (closure)
                        B Int ||- A (Maybe (Maybe Int))
-}

{-
2. ∅ ||- A (Maybe [Bool])


                                                          instance (A a, B a) => A [a]            instance A a => A (Maybe a)                          
                                                          ---------------------------(inst)       -----------------------------(inst)
instance A Bool         instance B Bool                   (A a, B a) ||- A [a]                    A a ||- A (Maybe a)
--------------- (inst)  ---------------- (inst)           ------------------------(closure)       --------------------------- (closure)
 ∅ ||- A Bool            ∅ ||- B Bool                     (A Bool, B Bool) ||- A [Bool]           A [Bool] ||- A (Maybe [Bool])
-------------------------------------- (dist)             ---------------------------------------------------------------------- (trans)
      ∅ ||- (A Bool, B Bool)                                          (A bool, B bool) ||- A (Maybe [Bool])
------------------------------------------------------------------------------------------------------------ (trans)
                         ∅ ||- A (Maybe [Bool])
-}