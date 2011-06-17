---
layout: post
title: Groovy's Fold
published: false
---
When I started getting into functional programming I relaized that Groovy has some features which could be considered functional or higher-order functions. I commonly use methods like `findAll` (`filter`) and `collect` (`map`). As I was reading **Beginning Scala** by *David Pollack* I came across `fold` which I had never used before. Groovy calls this `inject`. `inject` allows us to walk though a data structure calling a function on each node, the result of that function is then passed to the function being called on the next node. 

{% highlight java linenos %}
assert 55 == (1..10).inject(0) { a,b-> a + b }

// which is the same as
assert 55 == 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
{% endhighlight %}   

###An example adding 1 though 10 together
<br />
{% highlight java linenos %}
// Say we wanted to map lower case characters to their upper case brothers
List alphabetLower = ['a', 'b', 'c', 'd', 'e']

Map alphabetMap = [:]
alphabetLower.each {letter->
    alphabetMap.put(letter, letter.toUpperCase())
}

// We can rerwite that as 
Map alphabetMap = alphabetLower.inject([:]) {map, letter->
    map.put(letter, letter.toUpperCase())
    return map
}
{% endhighlight %}
###I find my self making use of `inject` any time I write any sort of aggregator placed just before the start of some loop. 
<br />

{% highlight java linenos %}
print (1..100).inject(""){s,i->s+="$i-${i%3?'':'fizz'}${i%5?'':'buzz'}\n"} 
{% endhighlight %}
###Next time someone asks you to write a fizzbuzz answer with this