---
layout: post
title: Groovy's Fold
teaser: (1..100).inject(""){s,i->s+="$i-${i%3?'':'fizz'}${i%5?'':'buzz'}\n"} 
published: false
---
I began making use of `collect` and `findAll` very quickly when I started using Groovy, both had obvious value and replaced tedious Java boilerplate. It wasn't until I read **Beginning Scala** by *David Pollack* that I began to associate these with functional programming as well as discovering a third awesome function, `fold`. Of the common higher order functions that I have come across and use `fold` isn't one that is nearly as intuitive as `map` (groovy `collect`) or `filter` (groovy `findAll`). 

{% highlight java linenos %}
println (0..100).findAll {
  it % 3 == 0 && it % 5 == 0
}.collect {
  "$it-fizzbuzz"
}
{% endhighlight %}

`fold` is usually shown as an easy way to implement an add function.

{% highlight java linenos %}
def add = {a,b-> a+b}
def addCollection = { it.inject 0, add }

addCollection(1.100)
{% endhighlight %}
