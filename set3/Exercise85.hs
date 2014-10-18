f x y = (x, [y])

x = f . f . f

{-
x has the following type: 

a0 -> t0 -> (t1 -> (t2 -> (a0, t2), t1), t0)

which has an amount of type variables equal to 2^N, where N is the number of composed f's.
Because theres a new type variable required for every level it can not be internally represented using sharing.

Found at:

http://stackoverflow.com/questions/22060592/growth-of-type-definition-in-sml-using-hindley-milner-type-inference


-}



 