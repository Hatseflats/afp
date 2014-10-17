module Exercise84 where

forceBoolList :: [Bool] -> b -> b 
forceBoolList [] r = r
forceBoolList (x:xs) r = case x of 
	True -> forceBoolList xs r
	False -> forceBoolList xs r

{-
The forceBoolList fuction does not have the type signature forceBoolList :: [Bool] -> [Bool]
because there is no point in copying the values to a new list when all you want to do is evaluate the list.

force :: a -> a
force a = seq a a

seq evaluates it's first argument when the result of seq gets evaluated, so when the first and second argument are the same
it will evaluate 'a' when the result of seq gets evaluated, which also is 'a' so it's an entirely redundant expression.

http://www.haskell.org/haskellwiki/Seq
-}


main = print $ length (forceBoolList [True,2<3,undefined] "Test") 
