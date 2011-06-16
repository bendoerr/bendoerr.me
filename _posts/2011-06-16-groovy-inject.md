---
layout: post
title: Groovy's Fold
teaser: 
published: true
---
I began making use of `collect` and `findAll` very quickly when I started using Groovy, both had obvious value and replaced tedious Java boilerplate. It wasn't until I read **Beginning Scala** by *David Pollack* that I began to associate these with functional programming as well as discovering a third awesome function, `fold`. Of the common higher order functions that I have come across and use `fold` isn't one that is nearly as intuitive as `map` (groovy `collect`) or filter (groovy `findAll`). 
<script src="https://gist.github.com/1030015.js?file=foobar.groovy"></script>

{% highlight java linenos %}
println (0..100).findAll {
  it % 3 == 0 && it % 5 == 0
}.collect {
  "$it Foobar"
}
{% endhighlight %}
