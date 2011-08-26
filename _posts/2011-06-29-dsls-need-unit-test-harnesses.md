---
layout: post
title: How do I test that!
teaser: For the love of expressive languages give me a unit test harness for your DSL
published: true
---
`Groovy` as a language provides an awesome toolset for creating builder/DSL's to remove lots of boilerplate around libaries and improve readablility. However they come with a price, they are nearly impossible to mock out when you want to unit test that one bit of code burried down in it's bowels.

I first ran into this problem while using `Grails` **webflow**. Being an __enterprisy__ (read lots of individual steps, with full sorting, filter, and more) project our webflows quickly got very complex. Giving the [Graeme][graeme] and the `Grails` guys some credit they did provide a test case class to help with testing. What it allowed you to do was spin up a nearly fully functional instance (`integration` testing in `Grails` terminology) of your application and then force `events` that would then allow you to **walk** though the webflow. This was really overkill since we only wanted unit test like coverage on the webflows. The way that I go about testing, I would only need to do this sort of `integation` testing once, as a regression test to catch broken functionality when updating the webflow plugin. What I really want to test is all of that logic that I wrote inside the webflow.

I end with a plea to developers everywhere. When you write a DSL or a `Groovy` builder, write a test harness as well. In the next post I'll give an example of what a `Grails` webflow unit test harness could look like.

[graeme]: 		http://www.springsource.com/people/grocher "Graeme Rocher Rocks"