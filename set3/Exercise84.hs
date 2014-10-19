module Exercise84 where

forceBoolList :: [Bool] -> [Bool]
forceBoolList []     = []
forceBoolList (x:xs) = case x of
                          True  -> True:(forceBoolList xs)
                          False -> False:(forceBoolList xs)

forceBoolList' :: [Bool] -> a -> a 
forceBoolList' []     r = r
forceBoolList' (x:xs) r = case x of
                             True  -> forceBoolList' xs r
                             False -> forceBoolList' xs r


force :: a -> a
force a = seq a a
{-
force is useless because it's equivalent with id.
Consider 'y = force x'; because of laziness 'force x' isn't evaluated untill 'y' is evaluated to head normal form, 
therefore 'seq' doesn't evaluate 'x' to head normal form untill 'y' is evaluated to head normal form, and the expression is equivalent to 'y = x'.

However, the case of forceBoolList is slightly different, because it completely evaluates the list. forceBoolList' is therefore equivalent to 
deepseq on lists of booleans, and forceBoolList is not entirely trivial:
length [undefined] == 1
length $ forceBoolList [undefined] == undefined

However, forceBoolList has the downside that it rebuilds the whole list, even though you just want to evaluate it, therefore forceBoolList' is better.
-}
