---
title: Introduction to MongoDB, CouchDB and Redis Java APIs
date: 2015-01-03
tags: programming
layout: post
---

You want to see the MongoDB, CouchDB and Redis Java APIs in action? In
the same editor window? Take a glance at this...

<!-- more -->

Although NoSQL Databases are not very new both in concept and
implementation, they have been adopted by many project last few years,
especially by many startups and data science companies. There is still
an ongoing debate if it is worth to leave the SQL realm and move onto
a NoSQL solution. To me, it is just a matter of choosing the right
tool for the right problem.

In our daily research and commercial projects, we are virtually
swimming in a pool of data, sometimes semi-structured. SQL can be too
strict to model some data and SQL database management systems can be
poor in performance for some certain data definitions. This problem is
prevalent in time-series data management and analysis.

Here, I am not going to go into the argument of which NoSQL database
management system to use. But for the curious, I have a suggestion:

1. Check out the
   [comparison of the most widely used NoSQL database management solutions](http://kkovacs.eu/cassandra-vs-mongodb-vs-couchdb-vs-redis)
   and search
   [Stackoverflow](http://stackoverflow.com/search?q=sql+vs+nosql) for
   some tips.
2. Be sceptical about what you read and hear. Just try it yourself:
   Pick and install a DBMS, play with the API developed for your
   favourite programming language and do a little bit of benchmarking.

I have been asked recently by one of our clients to choose the best
DBMS solution for the problem in hand. My personal conclusion was
either of MongoDB or CouchDB, although I still maintain the opinion
that MongoDB would be a better choice in case things get a little bit
more complicated on the data definition front.

I still wanted to see the two APIs in the same editor window. So, I
scaffolded a Maven project with MongoDB and CouchDB library
dependencies, setup both servers quickly on my computer and tried both
APIs. I added Redis as a bonus.

Below, you will see both the Maven POM file and the Java source
file. Note that, CouchDB API is based on pure HTTP and I used the
[Ektorp](http://www.ektorp.org) Java API for CouchDB as
convenience. Nevertheless, conversations with MongoDB looks a lot more
convenient and familar than it is with CouchDB (ignoring the nice
object mapper of Ektorp).


**pom.xml:**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.vsthost.rnd</groupId>
    <artifactId>nosql</artifactId>
    <version>0.1-SNAPSHOT</version>

    <dependencies>
        <dependency>
            <groupId>org.mongodb</groupId>
            <artifactId>mongo-java-driver</artifactId>
            <version>2.12.4</version>
        </dependency>

        <dependency>
            <groupId>org.ektorp</groupId>
            <artifactId>org.ektorp</artifactId>
            <version>1.3.0</version>
        </dependency>

        <dependency>
            <groupId>redis.clients</groupId>
            <artifactId>jedis</artifactId>
            <version>2.6.0</version>
            <type>jar</type>
            <scope>compile</scope>
        </dependency>

        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-simple</artifactId>
            <version>1.7.9</version>
        </dependency>
    </dependencies>
</project>
```

**App.java:**

```java
package com.vsthost.rnd;

import com.mongodb.*;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.node.ObjectNode;
import org.ektorp.CouchDbConnector;
import org.ektorp.CouchDbInstance;
import org.ektorp.DocumentNotFoundException;
import org.ektorp.ViewQuery;
import org.ektorp.http.HttpClient;
import org.ektorp.http.StdHttpClient;
import org.ektorp.impl.StdCouchDbInstance;
import redis.clients.jedis.Jedis;

import java.net.UnknownHostException;
import java.util.HashMap;

/**
 * Provides a sandbox functionality as an introduction to MongoDB,
 * CouchDB and Redis APIs.
 */
public class App  {

    public static void main( String[] args ) throws UnknownHostException {
        // Let's try out MongoDB:
        System.out.println(">> MongoDB Section");
        tryMongoDB();

        // Now try the CouchDB:
        System.out.println(">> CouchDB Section");
        tryCouchDB();

        // Finally, try the Redis:
        System.out.println(">> Redis Section");
        tryRedis();
    }

    static void tryMongoDB () throws UnknownHostException {
        // Get the mongo client:
        MongoClient mongoClient = new MongoClient();

        // Drop database if exists:
        mongoClient.dropDatabase("nosqltest");

        // Get the database:
        DB db = mongoClient.getDB("nosqltest");

        // Create a collection:
        db.createCollection("collection", new BasicDBObject("capped", false));

        // List collections:
        db.getCollectionNames().forEach(x -> System.out.println("Collection: " + x));

        // Get the collection:
        DBCollection collection = db.getCollection("collection");

        // Insert a record:
        collection.insert(
                new BasicDBObject("key", "rec1")
                        .append("type", "integer")
                        .append("value", 1));

        // Insert another record:
        collection.insert(
                new BasicDBObject("key", "rec2")
                        .append("type", "string")
                        .append("value", "42"));

        // List all entries:
        collection.find()
                .forEach(x -> System.out.println("Record: " + x));
    }

    static void tryCouchDB () {
        // Get the HTTP Client:
        HttpClient httpClient = new StdHttpClient.Builder().build();

        // Get the CouchDB instance:
        CouchDbInstance dbInstance = new StdCouchDbInstance(httpClient);

        // Delete the database if any:
        try {
            dbInstance.deleteDatabase("nosqltest");
        }
        catch (DocumentNotFoundException ex) {
            // Pass...
        }

        // Get or create the database:
        CouchDbConnector db = dbInstance.createConnector("nosqltest", true);

        // Insert a record:
        ObjectNode rec1 = new ObjectMapper().createObjectNode();
        rec1.put("key", "rec1");
        rec1.put("type", "integer");
        rec1.put("value", 1);
        db.create(rec1);

        // Insert a record:
        ObjectNode rec2 = new ObjectMapper().createObjectNode();
        rec2.put("key", "rec2");
        rec2.put("type", "string");
        rec2.put("value", "42");
        db.create(rec2);

        // List all entries:
        db.queryView(new ViewQuery().allDocs().includeDocs(true))
                .forEach(x -> System.out.println("Record: " + x.getDocAsNode().toString()));
    }

    static void tryRedis () {
        // Get the jedis instance:
        Jedis jedis = new Jedis("localhost");

        // Insert a record:
        jedis.hmset("rec1", new HashMap<String, String>() {
            {
                put("type", "integer");
                put("value", "1"); // <<- Note that we can't put any type other than string
            }
        });

        // Insert another record:
        jedis.hmset("rec2", new HashMap<String, String>() {
            {
                put("type", "string");
                put("value", "1");
            }
        });

        // List all entries:
        jedis.keys("*")
                .forEach(x -> System.out.println("Record: " + x + " = " + jedis.hmget(x, "type", "value")));
    }
}
```
