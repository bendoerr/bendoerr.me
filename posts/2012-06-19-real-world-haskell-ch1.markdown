---
author: Ben Doerr
tags: real-world-haskell, functional, groovy
category: Haskell
title: Starting the Journey
---

One of the greatest things that have come out of learning and working everyday in a [Java like language][Groovy]
that not just supports lambdas but makes them a way of life, is that before you know it you are getting really curious
about this [functional programming][GroovyFunctions] thing. At first my curiosity lead me to [Scala][], and
why not, it's been totally hyped, compiles to JVM bytecode, and has some awesome [things][Akka] going on. But, boy
was I unprepared for all the concepts that Scala required me to understand just to read it's standard libraries. I gave
Scala the [old college try][Lift] but didn't come away with much other than this burning question of "What the f\*
are Monads?", and I decided I needed to learn functional concepts in a more clean room environment rather than one that
merges them with the OO world.

<!--MORE-->

So why Haskell?
=======================================================================================================================

The answer is pretty simple, but I'll get to that in a minute. First, I'm sure any of the following would have been
fine: [Erlang][], [Lisp][], [Scheme][], [Standard ML][], [F#][], [Clojure][], or the many other programming
languages that fall into the functional paradigm.  However, I had to pick one, so I did what any other 21st century
biped would do, I looked them up on Wikipedia. Right away I ruled out Lisp and Closure on purely aesthetic reasons,
namely the <del>excessive parenthesis</del> <ins>[elegant weapons][Xkcd297]</ins> that were required. It all really
boiled down to cover judgements. I really liked the way Haskell looked, and the words *lazy* and *pure* also fit well
with my world view. So there it is, I choose to learn Haskell because I am *lazy* and was looking for a *pure*ly
functional language to learn that didn't feel [intimidating][ScalaCrazyness].

Once all this was decided I went [out][OReillyRWH] and picked up copy of [Real World Haskell][RWH] since it looked good
and O'Reilly had a sale going on.

The First few Chapters
=======================================================================================================================

Preface
-----------------------------------------------------------------------------------------------------------------------

Got me excited, ready to rock and learn this thing.

Chapter 1
-----------------------------------------------------------------------------------------------------------------------

Haskell is one of those languages that have more than one compiler for it, but the de facto standard is the Glasgow
Haskell Compiler (GHC). Even better there was something called the [Haskell Platform][] that bootstrap all the cool
Haskell things that I would need.

**GHCi** is introduced right away, it is a REPL which EVERY language needs! Shame on you Java. The basics of GHCi and
some simple Haskell syntax seem intuitive enough. The list 'cons' makes sense since I picked that up from Scala. The
type system really started to get me excited, the fact that the compiler is able to infer types makes life awesome. And
is a feature that I really enjoy in other languages. Shame on you Java. Some quick exercises at the end of the chapter
get you a little more familiar with the basic function call structure.

Even more excited now, another chapter must be consumed.

Chapter 2
-----------------------------------------------------------------------------------------------------------------------

The first part of this chapter is an introduction to Haskell's type system. There are three properties of Haskell's type
system that make it unique: types are *static*, types are *strong* and they can be *infered*. Having strong types means
gone are the days of `ClassCastException`s and even `NullPointerException`s. Haskell also will never automatically cast
one type to another so no `Int -> Double` coercion. Static typing means that the compiler knows not the runtime, the
compiler knows the type of every value. This is super cool since a lot of code that I write in Groovy or even Java that
compiles and fails at runtime would never pass the compiler in Haskell.

This type system is super refreshing to see since I have been working with a language that not only has fairly weak
types but allows duck typing as well.

The second part of the chapter introduces some of the basic concepts in Haskell. Function application which is
parentheses optional and without commas. Some common complex data types such as lists and tuples. Passing functions as
arguments is the entire reason I am here. And more.

Chapter 2 presents just enough to quench the thirst for the night.

<!-- == Links ====================================================================================================== -->

[Akka]:             http://akka.io/                                                 "Akka is pretty cool."
[Clojure]:          http://en.wikipedia.org/wiki/Clojure                            "Clojure"
[Erlang]:           http://en.wikipedia.org/wiki/Erlang_(programming_language)      "Erlang (programming language)"
[F#]:               http://en.wikipedia.org/wiki/F_Sharp_(programming_language)     "F# (programming language)"
[GroovyFunctions]:  http://groovy.codehaus.org/Functional+Programming+with+Groovy   "Functional Programming with Groovy"
[Groovy]:           http://groovy.codehaus.org/                                     "Groovy - A dynamic language for the Java platform"
[Haskell Platform]: http://www.haskell.org/platform/                                "Haskell Platform"
[Lift]:             http://liftweb.net/                                             "Lift Web Framework"
[Lisp]:             http://en.wikipedia.org/wiki/Common_Lisp                        "Common Lisp"
[OReillyRWH]:       http://shop.oreilly.com/product/9780596514983.do                "Real World Haskell"
[RWH]:              http://book.realworldhaskell.org/                               "Read Real World Haskell Online"
[ScalaCrazyness]:   http://yz.mit.edu/wp/true-scala-complexity/                     "Scala Intimidates Me"
[Scala]:            http://www.scala-lang.org/                                      "Scala Progamming Language"
[Scheme]:           http://en.wikipedia.org/wiki/Scheme_(programming_language)      "Scheme (programming language)"
[Standard ML]:      http://en.wikipedia.org/wiki/Standard_ML                        "Standard ML"
[Xkcd297]:          http://xkcd.com/297/                                            "Lisp Cycles"
