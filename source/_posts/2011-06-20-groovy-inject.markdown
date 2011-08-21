---
layout: post
title: Groovy's Fold
published: true
date: 2011-06-20 12:00
coments: false
categories: [Groovy, Functional Programming]
author: Ben Doerri
sharing: true
footer: true
---
When I started getting into functional programming I relaized that Groovy has some features which could be considered functional or higher-order functions. I commonly use methods like `findAll` (`filter`) and `collect` (`map`). As I was reading **Beginning Scala** by *David Pollack* I came across `fold` which I had never used before. Groovy calls this `inject`. `inject` allows us to walk though a data structure calling a function on each node, the result of that function is then passed to the same function being called on the next node. 

##Here are a few examples
{% codeblock sum.java %}
assert 55 == (1..10).inject(0) { a,b-> a + b }

// which is the same as
assert 55 == 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
{% endcodeblock %}
<br />
{% codeblock injectExample.java %}
// Say we wanted to map lower case characters to their upper case brothers
List alphabetLower = ['a', 'b', 'c', 'd', 'e']

Map alphabetMap = [:]
alphabetLower.each {letter->
    alphabetMap.put(letter, letter.toUpperCase())
}

// We can rerwite that as 
Map alphabetMap = alphabetLower.inject([:]) {map, letter->
    map.put(letter, letter.toUpperCase())
}   
{% endcodeblock %}
<br />
Answering a fizzbuzz like this will not get you the job.
{% codeblock fizzbuzz.java %}
print (1..100).inject(""){s,i->s+="$i-${i%3?'':'fizz'}${i%5?'':'buzz'}\n"} 
{% endcodeblock %}
<br />
## Pimp Groovy's List

Groovy's `inject` is considered a fold left since we work our way left to right. Groovy doesn't provide a fold right, however we can pimp java the same way Groovy does.

{% codeblock pimp.java %}
class FoldRight {
    static Object foldRight(List self, Object initalValue, Closure closure) {
        Object value = initalValue
        for (ListIterator iterator = self.listIterator(self.size()); iterator.hasPrevious();) {
            Object node = iterator.previous()
            value = closure.call(value, node)
        }
        return value
    } 
     
    static Object injectRight(List self, Object initalValue, Closure closure) {
        foldRight(self, initalValue, closure)
    }
}
 
// Test it
use(FoldRight) {
    assert "dcba" == ['a','b','c','d'].foldRight("") {a,b-> a+b }
    assert "dcba" == ['a','b','c','d'].injectRight("") {a,b-> a+b }
}
{% endcodeblock %}
