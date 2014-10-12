Sebastiaan Jong - sebastiaan.jong@gmail.com - 5546303
Koen Wermer     - koenwermer@gmail.com      - 3705951

We implemented all excercises in seperate files.

Additional comments:
2.9
We defined ListWithLength to keep track of the size of the stack on type level, to make the type checker reject invalid expressions like the example p3.

4.1
As a test, we used y to define factorial.
This exercise is not included in the cabal file, because cabal can't handle the definition of y.

4.2
As a bonus we made squares instance of Show. 
Unfortunately, our solution requires the elements of the square to be made instance of Show' (and things like lists to be made instance of ShowContainer, which is even more of a pain).
Please let us know if you have a better solution!