{-
Updated:  August 16, 2020
Author:   Ninad Jog
-}

module SplitOddEven where
  
import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

{-
Problem:
Split the given list of integers into two lists: one containing odd
numbers and the other the evens. Return both lists in a tuple,
with the odd one followed by the even one. The order of the
numbers should be maintained in both lists. Examples:

Input:  [0, 1, -3, 4, 5, 9, 12]
Output: ([1,-3,5,9],[0,4,12])

Input:  []
Output: ([], [])

Input:  [2, 4, 6]
Output: ([], [2, 4, 6])

Solutions

The simplest solution is just a single line of code using the filter 
function, as shown in Solution #1. It is the cleanest and the best.

I have also several other solutions using list comprehension, 
list monads, structural recursion, state monads, and desugaring the list
monads.

The other solutions might come in handy if you are required to solve
the problem without using library functions or list comprehensions
but with structural recursion and/or state or list monads, as is often 
the case for newcomers to Haskell, including me.
-}

-- ===============================================================
{-
Solution #1

Use the filter function. This solution is the simplest.
-}
splitOddEven1 :: [Int] -> ([Int], [Int])
splitOddEven1 xs = (filter odd xs, filter even xs)

{- 
Same solution, with the odd and even lists split into two lines
for documentation / ease of understanding
-}
splitOddEven1' :: [Int] -> ([Int], [Int])
splitOddEven1' xs = (oddL, evenL)
  where
    oddL  = filter odd xs
    evenL = filter even xs

-- ===============================================================
{-
Solution #2

Using list comprehensions
-}
splitOddEven2 :: [Int] -> ([Int], [Int])
splitOddEven2 xs = (oddL, evenL)
  where
    oddL  = [ x | x <- xs, odd x]
    evenL = [ x | x <- xs, even x]

-- ===============================================================
{-
Solution #3

Using list monads
-}

splitOddEven3 :: [Int] -> ([Int], [Int])
splitOddEven3 xs = (oddL, evenL)
  where
    oddL = do
      x <- xs
      guard (odd x)
      return x
      
    evenL = do
      x <- xs
      guard (even x)
      return x      

-- ===============================================================
{-
Solution #4

Desugaring the list monad.
(The >>= bind operator is reminscent of Elixir's pipe operator)
-}

splitOddEven4 :: [Int] -> ([Int], [Int])
splitOddEven4 xs = (oddL, evenL)
  where
    oddL  = xs >>= (\x -> guard (odd x)  >> return x)
    evenL = xs >>= (\x -> guard (even x) >> return x)
    
-- ===============================================================
{-
Solution #5

Using structural recursion

Insert a new element at the beginning of the list because it's
more efficient than appending it at the end of the list -
it takes O(1) time.

Reverse both the odd and even lists at the end to maintain the
order.
-}

splitOddEven5 :: [Int] -> ([Int], [Int])
splitOddEven5 xs = splitH xs ([], [])
  where
    splitH :: [Int] -> ([Int], [Int]) -> ([Int], [Int])
    splitH [] (oddL, evenL) = (reverse oddL, reverse evenL)
    splitH (x:xs) (oddL, evenL)
      | odd x     = splitH xs (x : oddL, evenL)
      | otherwise = splitH xs (oddL, x : evenL)
      
{-
Note about the implementation:

In the helper function splitH,
I have chosen to place the two accumulator variables, oddL and evenL,
in a tuple. I could just as well have placed them separately, so that
splitH's signature would have been

splitH :: [Int] -> Int] -> [Int] -> ([Int], [Int])

I like to collect all accumulator varibales in a single tuple though,
so that we can tell at a glance that all the variables appearing in
the tuple are accumulator variables. Accumulator varibales are those that
aid in the computation of the final result and whose value changes between
successive invocations of recursive function calls.

(If there were a variable whose
value didn't change between recursive calls, I would place it outside
the tuple because it's not an accumulator variable.)
-}

-- ===============================================================
{-
Solution #6

Structural recursion but appending new elements to the end of the 
list instead of the beginning.

This solution is NOT
efficient, particularly for long lists because each append operation
takes O(n) time, where n is the number of elements in the list.

Note that we do NOT have to reverse the lists in the end because they
are built in the correct order from the beginning.
-}

splitOddEven6 :: [Int] -> ([Int], [Int])
splitOddEven6 xs = splitH xs ([], [])
  where
    splitH [] (oddL, evenL) = (oddL, evenL)
    splitH (x:xs) (oddL, evenL)
      | odd x     = splitH xs (oddL ++ [x], evenL)
      | otherwise = splitH xs (oddL, evenL ++ [x])
      
{-
Another variation (not shown here) would be to use difference lists
for an efficient implementation of the list append operation.
-}

-- ===============================================================
{-
Solution #7

Using a State Monad

Stores both the odd and even lists as the state, which is why there
is no return value. (The return type is () and so is the return value)

This function is called by the runState or execState functions to
achieve the desired result.
-}

splitOddEvenM :: [Int] -> State ([Int], [Int]) ()
splitOddEvenM [] = do
  return ()
  
splitOddEvenM (x:xs)
  | odd x = do
      (oddL, evenL) <- get  -- pop the state off the state stack
      put (x : oddL, evenL) -- push the modified list back on the state stack
      splitOddEvenM xs
      
  | otherwise = do
      (oddL, evenL) <- get
      put (oddL, x : evenL)
      splitOddEvenM xs

{-
Implementation notes

1) This implementation might be a non-standard one because it stores both
accumulators (the odd and even lists) in the State. Another variation,
shown in Solution #7 below, is to store one accumulator (either the odd list
or the even one) in the State and the remaining one in the result.

2) Another variation would be to append the new element at the end of 
the list, as in oddL ++ [x], as in Solution #5.
-} 

----
{-
From the (answer, finalState) tuple of the runState function,
extract and return the final state because it contains both the
odd and even lists.
-}
splitOddEven7 :: [Int] -> ([Int], [Int])
splitOddEven7 xs = case runState (splitOddEvenM xs) ([], []) of
  (_, (oddL, evenL)) -> (reverse oddL, reverse evenL)
  
----
{-
Since we are abandoning the result (because the result is encapsulated
in the state), we can use the execState function instead of runState
as follows.
-}
splitOddEven7' :: [Int] -> ([Int], [Int])
splitOddEven7' xs = case execState (splitOddEvenM xs) ([], []) of
  (oddL, evenL) -> (reverse oddL, reverse evenL)
  
-- ===============================================================
{-
Solution #8

Using structural recursion where only the odd list is
captured as part of the State monad. Return type is the even list.

We could have done it the other way around: save the even list as
the state and return the odd list as the return value.
-}

splitOddEvenM' :: [Int] -> State [Int] [Int]
splitOddEvenM' [] = do
  return []

splitOddEvenM' (x:xs)
  | odd x = do
      oddL <- get         -- Get list of odd numbers from the State
      put (x : oddL)      -- Insert x into the list
      splitOddEvenM' xs   -- Save the updated list (state)
      
  | otherwise = do
      evenL <- splitOddEvenM' xs  -- Move on to the next number in list
      return (x : evenL)          -- Insert x into the returned list

----
{-
Use the runState function to extract the final answer from the
result and the state of the splitOddEvenM' function.
-}

splitOddEven8 :: [Int] -> ([Int], [Int])
splitOddEven8 xs = case runState (splitOddEvenM' xs)[] of
                    (evenL, oddL) -> (reverse oddL, evenL)
                    
-- ===============================================================
{-
Solution #9

Return type is a Data.Map rather than tuple. The keys of the lists are
True (for odds) and False (for evens)
-}

splitOddEven9 :: [Int] -> Map Bool [Int]
splitOddEven9 xs =
  let
    oddL  = filter odd xs
    evenL = filter even xs
  in
    Map.insert True  oddL .
    Map.insert False evenL $ Map.empty
 
-------------------------
{-
More variations (TBD)

* Difference lists to append one list to another efficiently

* In the State monad code we could have appended to the end of
the odd and even lists instead of inserting new elements at the
front of the list

* De-sugar the state monads in Solutions #6 and #7.

* Use semicolons instead of approprite indenting

* In the structural recursion solutions, the accummulator need not be
a tuple; the odd and even lists can appear separately.

* Create a Data.Map from an association list

-}
