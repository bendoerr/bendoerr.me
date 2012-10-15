---
author: Ben Doerr
tags: camel, jmx
category: Java
title: Stop and Start All Camel Routes
---
Here is a quick example of how you might stop all Camel routes regardless of the `CamelContext`. I assume that you have
prior knowledge of how to connect via JMX, or you can see my last [post][postPurgeQueue] on Purging ActiveMQ Queues
via JMX.

<!--MORE-->

~~~~~~~~~~{.java}
ObjectName jmxRoutesName = new ObjectName("org.apache.camel:type=routes,*");
for (ObjectName jmxRouteName : conn.queryNames(jmxRoutesName, null)) {
    ManagedRouteMBean routeMBean =
            MBeanServerInvocationHandler.newProxyInstance(conn, jmxRouteName, ManagedRouteMBean.class, true);
    routeMBean.stop();
}
~~~~~~~~~~

The [ManagedRouteMBean][docManagedRouteMBean] has quite a bit of other usefulness in it.

[postPurgeQueue]:       http://bendoerr.me/posts/2012-10-14-activemq-jmx-purge.html "Purge ActiveMQ Queues"
[docManagedRouteMBean]: http://camel.apache.org/maven/current/camel-core/apidocs/org/apache/camel/api/management/mbean/ManagedRouteMBean.html "Interface ManagedRouteMBean"
