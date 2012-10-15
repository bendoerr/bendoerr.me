---
author: Ben Doerr
tags: activemq, jxm, karaf
category: Java
title: Purge ActiveMQ Queues
---

Whether for testing or some edge case sometimes you just need to clear out messages that have been building up. Here is
an example of how you can clean all of a brokers queues using facilities exposed by [JMX][jmxWikipedia] in ActiveMQ.


<!--MORE-->

The JMX url that you use to connect will depend on how you have [configured][amqJMXConfig] your ActiveMQ broker. I was
running an embedded broker in a [Karaf][karaf] OSGi container that was configured to use JMX but not create a new JMX
connection. It took me a minute or two to figure out the correct JMX url of
`service:jmx:rmi://0.0.0.0:44444/jndi/rmi://0.0.0.0:1099/jmxrmi` so I share it here for prosperity.

~~~~~~~~~~{.xml}
<broker xmlns="http://activemq.org/config/1.0" brokerName="mqbroker" useJmx="true">
  ...
  <managementContext>
     <managementContext createConnector="false"/>
  </managementContext>
  ...
</broker>
~~~~~~~~~~

The first thing is to create a [MBeanServerConnection][docMBeanServerConnection] which we can then use to look up our
[BrokerViewMBean][docBrokerViewMBean] and [QueueViewMBean][docQueueViewMBean] by creating a connection from the
[JMXConnectorFactory][docJMXConnectorFactory].

~~~~~~~~~~{.java}
JMXServiceURL url = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://localhost:1099/jmxrmi");
JMXConnector jmxc = null;
try {
    jmxc = JMXConnectorFactory.connect(url))
    MBeanServerConnection conn = jmxc.getMBeanServerConnection();
~~~~~~~~~~

Now that you have the connection you can query it for the `BrokerViewMBean`.

~~~~~~~~~~{.java}
    ObjectName jmxBrokerName = new ObjectName("org.apache.activemq:BrokerName=mqbroker,Type=Broker");
    BrokerViewMBean brokerView =
            MBeanServerInvocationHandler.newProxyInstance(conn, jmxBrokerName, BrokerViewMBean.class, true);
~~~~~~~~~~

From the broker view you can get all of the queue names and then query those and finally purge them.

~~~~~~~~~~{.java}
    for (ObjectName jmxQueueName : mbean.getQueues()) {
        QueueViewMBean queueView =
            MBeanServerInvocationHandler.newProxyInstance(conn, jmxQueueName, QueueViewMBean.class, true);
        queueView.purge();
    }
~~~~~~~~~~

The don't forget to clean up.

~~~~~~~~~~{.java}
} finally {
    if (jmxc != null)
        jmxc.close();
}
~~~~~~~~~~

There is a bunch more that is exposed via these MBeans that is useful in monitoring your broker as well. I suggest
taking a look at the JavaDocs if that interests you.


[jmxWikipedia]:             http://en.wikipedia.org/wiki/Java_Management_Extensions "Wikipedia: Java Management Extensions"
[amqJMXConfig]:             http://activemq.apache.org/jmx.html "ActiveMQ JMX Feature"
[karaf]:                    http://karaf.apache.org/ "Karaf OSGi Runtime"
[docMBeanServerConnection]: http://docs.oracle.com/javase/7/docs/api/javax/management/MBeanServerConnection.html "Interface MBeanServerConnection"
[docJMXConnectorFactory]:   http://docs.oracle.com/javase/7/docs/api/javax/management/remote/JMXConnectorFactory.html "Class JMXConnectorFactory"
[docBrokerViewMBean]:       http://activemq.apache.org/maven/5.7.0/activemq-core/apidocs/org/apache/activemq/broker/jmx/BrokerViewMBean.html "Interface BrokerViewMBean"
[docQueueViewMBean]:        http://activemq.apache.org/maven/5.7.0/activemq-core/apidocs/org/apache/activemq/broker/jmx/QueueViewMBean.html "Interface QueueViewMBean"
