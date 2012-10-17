---
author: Ben Doerr
tags: real-world-haskell, functional
category: Haskell
title: Chapter 3 Boss
---

Chapter 3 of [Real World Haskell][RWH] ends with an epic boss. Exercise question 12 states:

> Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of
> 2D points. You can find good description of what a convex hull is, and how the Graham scan algorithm should work, on
> Wikipedia.

Yea I just said WTF out loud too.

<!--MORE-->

It took me a day or so but I accomplished the task and here is a walk though. But first we will need the previous two
questions and their answers that I came up with.

Exercise 10
-----------------------------------------------------------------------------------------------------------------------

> Write a function that calculates the turn made by three two-dimensional points and returns a Direction.

~~~~~~~~~~{.haskell}
data Direction = LEFT | RIGHT | STRAIGHT
                 deriving (Show, Eq)

data CartesianPoint = Point {x :: Double, y :: Double}
                      deriving (Show)

directionOf :: CartesianPoint -- ^ Point A
            -> CartesianPoint -- ^ Vertex
            -> CartesianPoint -- ^ Point B
            -> Direction
directionOf a v b | sign > 0  = RIGHT
                  | sign < 0  = LEFT
                  | otherwise = STRAIGHT
            where sign = (x v - x a) * (y b - y a) - (y v - y a) * (x b - x a)
~~~~~~~~~~~~~~~~~~~~

I original struggled with this one, I had a partial solution that kinda worked halfway it wasn't till I read the
Wikipedia page for [Graham Scan][] that I saw this specific equation.

Exercise 11
-----------------------------------------------------------------------------------------------------------------------

> Define a function that takes a list of two-dimensional points and computes the direction of each successive triple.
> Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by
> [b,c,d], then [c,d,e]. Your function should return a list of Direction.

~~~~~~~~~~{.haskell}
directionsOf (a : v : b : ps) = directionOf a v b : directionsOf(v:b:ps)
directionsOf _                = []
~~~~~~~~~~~~~~~~~~~~

This one was pretty simple given that `directionOf` works correctly.

The Boss - Exercise 12
-----------------------------------------------------------------------------------------------------------------------

Ugh WTF again. Anyway lets walk though the algorithm that Wikipedia lists.

1. Find the point with the lowest y-coordinate. Call this point P.
1. Order the set of points in increasing order of the angle they and the point P make with the x-axis.
1. Consider each of the sorted points in sequence. Check if it is a Left or a Right turn. If it is a right turn, then
   the second-to-last point is not part of the convex hull and should be removed from consideration.
1. Continue until the point P is returned.

So how did I do this. First step. I sorted all of the points by their y value. The head of this list will be P.

~~~~~~~~~~{.haskell}
sortByY xs = sortBy lowestY xs
             where lowestY a b = compare (y a, x a) (y b, x b)
~~~~~~~~~~~~~~~~~~~~

Step two, the first item in the sorted set is P, and now I need to sort the set by their angle.

~~~~~~~~~~{.haskell}
sortByAngle ps = bottomLeft : sortBy (compareAngles bottomLeft) (tail (sortedPs))
    where sortedPs       = sortByY ps
           bottomLeft     = head (sortedPs)
           pointAngle a b = (x b - x a) / (y b - y a)
           compareAngles  = comparing . pointAngle
~~~~~~~~~~~~~~~~~~~~

Finally, given that I have my set of sorted points now, I can check if it is a left or right turn and drop the middle
point.

~~~~~~~~~~{.haskell}
grahamScan :: [CartesianPoint] -> [CartesianPoint]
grahamScan ps = scan (sortByAngle ps)
    where scan (a:v:b:xs) = if directionOf a v b == RIGHT
                             then a : scan(b:xs)
                             else a : scan(v:b:xs)
           scan [a, b]     = [a, b]
           scan _          = []
~~~~~~~~~~~~~~~~~~~~

The decomposition and recomposition of the list here is pretty cool.

And this gets me past the boss. I had to do a bit of grinding before I could beat him, but I am really super happy with
my result.



<!-- == Links ====================================================================================================== -->

[Graham Scan]:  http://en.wikipedia.org/wiki/Graham_scan "Graham Scan on Wikipedia"
[RWH]:          http://book.realworldhaskell.org/        "Read Real World Haskell Online"
