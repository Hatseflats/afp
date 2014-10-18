x0 = undefined
x1 = (x0, x0)
x2 = (x1, x1)
x3 = (x2, x2)
x4 = (x3, x3) -- ...and so on

{-
x0 has type a, which means you could, for example, apply the first element of (x0, x0) to a boolean value and the second to an int.
Therefore, x1 must have type (a, b) (both a and b can be choosen freely when applying the functions).
For the same reason, the type of the first polymorphic tuple x1 in x2, doesn't match the type of the second one (they use different type variables),
and so on..

Not that x0 can be any polymorphic function.

Every line of code doubles the number of type variables in the resulting type, so the type grows exponentially.

Based on:

http://stackoverflow.com/questions/22060592/growth-of-type-definition-in-sml-using-hindley-milner-type-inference

-}



 