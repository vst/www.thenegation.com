---
title: Red5 and RESTEasy in 3 Steps
date: 2014-02-02 16:10:00
tags: computing
layout: post
---

Red5 is a media streaming server which is implemented using Java and bundled in a Tomcat application container. Decoupling it from Tomcat is a bit troublesome. If you want to add a bit of HTTP REST flavour to your application without the hassle of decoupling it from Tomcat, you can use RESTEasy. Here is how I did it.

[Red5](http://www.red5.org/) runs on Tomcat and is not really actively developed. I needed to add some lightweight HTTP API functionality to our Red5 application and decided to use [RESTEasy](http://www.jboss.org/resteasy) which is a fully certified and portable implementation of the JAX-RS specification. It is also included in the latest [Wildfly](http://wildfly.org/).

It was not that difficult:

1. [Download latest RESTEasy](http://www.jboss.org/resteasy) and unpack the `lib/` folder contents into the `lib/` folder of Red5 (or in other words, Tomcat).
2. Add a JAX-RS annotated class and method, like::
        package com.example.red5.application.api

        import java.util.HashMap;
        import javax.ws.rs.GET;
        import javax.ws.rs.Path;
        import javax.ws.rs.Produces;

        @Path("/")
        public class Version {
            @GET
            @Path("/version")
            @Produces("application/json")
            public HashMap<String, String> responseMsg() {
                HashMap<String, String> result = new HashMap<String, String>();
                result.put("version", "0.0.1");
                return result;
            }
        }
3. Add the following into your `WebContent/WEB-INF/web.xml` contents
(place within the `web-app` tag):
        <servlet>
          <servlet-name>resteasy-servlet</servlet-name>
          <servlet-class>
            org.jboss.resteasy.plugins.server.servlet.HttpServletDispatcher
          </servlet-class>
        </servlet>

        <servlet-mapping>
          <servlet-name>resteasy-servlet</servlet-name>
          <url-pattern>/api/*</url-pattern>
        </servlet-mapping>

        <context-param>
          <param-name>resteasy.scan</param-name>
          <param-value>true</param-value>
        </context-param>

        <context-param>
          <param-name>resteasy.servlet.mapping.prefix</param-name>
          <param-value>/api</param-value>
        </context-param>

        <listener>
          <listener-class>
            org.jboss.resteasy.plugins.server.servlet.ResteasyBootstrap
          </listener-class>
        </listener>

Now, you can deploy your application and restart the Red5 server. Hit
[http://localhost:5080/yourapp/api/version](http://localhost:5080/yourapp/api/version),
and voila!...
