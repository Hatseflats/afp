Sebastiaan Jong - sebastiaan.jong@gmail.com - 5546303
Koen Wermer     - koenwermer@gmail.com      - 3705951

11.1
Because constrFields seems to always return an empty list, we can't use this information to omit parentheses, so we have to always show parentheses for algebraic types.
For example, we can read "T 1 1" and "T 1 (T 1 2)", but not "T 1 T 1 2" (which is good), but because of this, we can also not read "T 1 True". We can however read "T 1 (True)".