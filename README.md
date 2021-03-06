# Split Numbers into Odd and Even Lists - Haskell

Given a list of integers, split it into two lists: one containing odd numbers and the other the evens. Retain the order of the numbers and return both lists in a tuple, with the odd list followed by the even one. Examples:

```
Input:  [0, 1, -3, 4, 5, 9, 12]
Output: ([1,-3,5,9],[0,4,12])

Input:  []
Output: ([], [])

Input:  [2, 4, 6]
Output: ([], [2, 4, 6])
```
## Solutions

The simplest solution is just a single line of code using the *filter* function, as shown below.
```
splitOddEven :: [Int] -> ([Int], [Int])
splitOddEven xs = (filter odd xs, filter even xs)
```

I have also provided several other solutions using list comprehensions, list monads, structural recursion, and state monads. These solutions are by no means neccessary for such a simple problem; in fact it's overkill. It's like calling a fire engine to snuff out a cigarette butt.

But the other solutions might come in handy if you have been asked to solve the problem without using library functions or list comprehensions and if you're new to structural recursion and monads, as is sometimes the case for Haskell newbies, including me.

## List of Solutions

1. Filter function
2. Using list comprehensions
3. List monads
4. Desugaring list monads
5. Structural recursion
6. Structural recursion variation -- appending new element to end of list
7. State monad, passing both odd & even lists as part of the state
8. State monad, passing one list as the state and the other as the result
9. Return type of function is *Data.Map* instead of tuple

All these solutions appear in the file [SplitOddEven.hs](SplitOddEven.hs)

## Other Solutions (TBD)

1. Use difference lists to efficiently append an element to the end of a list
2. Desugar the state monads from solutions #6 and #7
3. Use semicolons instead of indenting
4. Create a *Data.Map* from an association list
