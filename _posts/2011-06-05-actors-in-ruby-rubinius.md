---
layout: default
title: "Actors in Ruby: Rubinius (WIP)"
---

{{ page.title }}
--------------

### Prerequisites

To follow along with the examples, you'll need to install Rubinius. I used RVM:

    $rvm install rbx-head

Or you can get an install at the [Rubinius](http://rubini.us/) home page.

### What are actors?
An actor is a unit of concurrency. It is a process (thread, coroutine, etc.) with a mailbox for receiving asynchronous messages. Each message received by the actor is processed in order. Different types of messages can be processed by applying filters to messages when they arrive. Because actors only operate on messages explicitly sent them, it can be easier to reason out what a concurrent system is doing.

There are languages that employ the actor model. Erlang is probably the best known, but Scala and Io (one of my favorites) are two others you may have heard of. For actors in Ruby, you need a library. Revactor is a one library that relies on 1.9's fibers to implement the pattern. Rubinius also includes an actor implementation as part of the standard library. That's what I will be demonstrating in this article, since I was looking for an excuse to play with Rubinius anyway.

For starters, lets write a simple actor in Rubinius. Fire up irb and enter this code:

<script src="https://gist.github.com/1011573.js?file=silly_actor.rb">&nbsp;</script>

This is, of course, an unimpressive example, but it does demonstrate creating a simple actor, and then sending it a message asynchronously. Let's step through this code. 

We need to require 'actor'. Since Actor is part of the standard library in Rubinius, this is all we need to do to get access to Actors. Then we create our actor using Actor#spawn (Actor#new is also aliased to this this method) and assign the spawned actor to 'silly_actor'. An actor's behavior is defined in a block. Within the block we call Actor#receive, which blocks until a the actor actually receives a message. Then we assign the message to a variable and put it to stdout as part of our unimpressive message. Finally, we send our message to the actor using #<< (which is an alias of #send). 

There are a couple of gotchas to be aware of here.

First, #send (#<<) calls are asynchronous. They return immediately (and return an instance of the actor, by the way), even if the work isn't done. Second, be careful about what messages you send. It's a good idea to send either immutable objects or to duplicate or clone an object before using it as a message. Since actors execute concurrently, this will protect your object from changing out from under you and producing unpredictable results. 

### Filtering Messages

(forthcoming)
