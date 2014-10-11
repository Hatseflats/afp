forceBoolList :: [Bool] -> [Bool]
forceBoolList b = b

forceBoolList' :: [Bool] -> r -> r 
forceBoolList' b n = undefined

main = do
	print $ forceBoolList [True,False,True,False]

{-
force :: a -> a
force a = seq a a

seq will evaluate a to weak head normal form.
So if it is a list of boolean expressions only 
the list will be evaluated, not the expressions inside.

deepseq would work for evaluating all expressions in the list.
-}