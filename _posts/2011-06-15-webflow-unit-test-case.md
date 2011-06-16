---
layout: post
title: Grails Webflow Unit Test Case
---
I recently got fed up (only took a year) with the lack of ability to unit test the individual parts of a web flow so I wrote a test harness to allow me to do just that. The motivation behind this test case is to be able to test logic within web flow actions without the need to work your way all the way though the web flow. 

<script src="https://gist.github.com/881935.js"> </script>

There are two classes. The test case (WebFlowUnitTestCase) and the webflow dsl -> map builder (WebFlowUnitTestSupport). The test case relies on ControllerUnitTestCase and tries to behave the same way as ControllerUnitTestCase such that FooControllerBarWebFlowTests mocks the FooController and mocks fooController.barFlow. 

In your tests you can use all the familiar ControllerUnitTestCase convinices such as mockParams, and I have added two new scopes for the WebFlowUnitTestCase. mockFlowScope and mockConversationScope as well as a property called stateTransition which will be set with the name of the return transition from the event action. 

<script src="https://gist.github.com/884076.js"> </script>
