---
layout: default
title: "Basics of Actors (using Rubinius)"
---

{{ page.title }}
--------------

### Prerequisites

To follow along with the examples, you'll need to install Rubinius. I used RVM:

    $rvm install rbx-head

Or you can get an install at the [Rubinius](http://rubini.us/) home page.

### What are actors?
An actor is a unit of concurrency. It is a process (thread, co-routine, etc.) with a mailbox for receiving asynchronous messages. Each message received by the actor is processed in order. Different types of messages can be processed by applying filters to messages when they arrive. Because actors only operate on messages explicitly sent them, it can be easier to reason out what a concurrent system is doing.

There are languages that employ the actor model. Erlang is probably the best known, but Scala and Io (one of my favorites) are two others you may have heard of. For actors in Ruby, you need a library. Revactor is a one library that relies on 1.9's fibers to implement the pattern. Rubinius also includes an actor implementation as part of the standard library. That's what I will be demonstrating in this article, since I was looking for an excuse to play with Rubinius anyway.

For starters, lets write a simple actor in Rubinius. Fire up irb and enter this code:

<script src="https://gist.github.com/1011573.js?file=silly_actor.rb">&nbsp;</script>

This is, of course, an unimpressive example, but it does demonstrate creating a simple actor, and then sending it a message asynchronously. Let's step through this code. 

We need to require 'actor'. Since Actor is part of the standard library in Rubinius, this is all we need to do to get access to Actors. Then we create our actor using Actor#spawn (Actor#new is also aliased to this this method) and assign the spawned actor to 'silly\_actor'. An actor's behavior is defined in a block. Within the block we call Actor#receive, which blocks until a the actor actually receives a message. Then we assign the message to a variable and put it to stdout as part of our unimpressive message. Finally, we send our message to the actor using #&lt;&lt; (which is an alias of #send). 

There are a couple of gotchas to be aware of here.

First, #send (#&lt;&lt;) calls are asynchronous. They return immediately (and return an instance of the actor, by the way), even if the work isn't finished. Second, be careful about what messages you send. It's a good idea to send either immutable objects or to duplicate or clone an object before using it as a message. Since actors execute concurrently, this will protect your object from changing out from under you and producing unpredictable results. 

### Filtering Messages

If you tried to send another message to our silly\_actor, you may have noticed something peculiar. Go ahead, try it.

    rbx-head :011> silly_actor << "is anybody out there?"

Nothing happened, right? That's because this actor is dead. Passed on. Ceased to be. Bereft of life. It rests in peace. Pushing up the daisies!

Actors aren't very practical if you have to instantiate a new one every time you want to process a message. Too much over head in creating the new object and a new process. It would be much better if we could keep sending messages to the same actor. And we can.

<script src="https://gist.github.com/1011573.js?file=taunting_actor.rb">&npsp;</script>

Let's look at what's changed in this code. First of all, we're now running in a loop, so our thread won't die. Within the loop, we still call #receive, but this time we pass it a block. In the block form, we now have access to the Filter object, which allows us to process different types of messages. When the message is a String, the actor taunts you. When the message is the symbol :stop, then the loop is exited and the actor process dies.

We've only been using strings and symbols as messages, but we could use any ruby object.

You'll notice that we've replaced Strings and symbols with our own types and the filtering logic still works fine.

<script src="https://gist.github.com/1011573.js?file=livestock_actor.rb">&nbsp;</script>

Our actors are getting out of hand. What they could really use now is some supervision. We'll cover supervisors in the next section.

### Supervisors

Per the [Erlang](http://www.erlang.org/doc/design_principles/sup_princ.html#) documentation:

    A supervisor is responsible for starting, stopping and monitoring its child processes.
    The basic idea of a supervisor is that it should keep its child processes alive by restarting them when necessary.

In other words, a supervisor is like a little project manager, handling all the dirty work so the workers can focus on that task at hand. If something unexpected happens in the worker actor, it can simply crash, and the supervisor will clean everything up. You're embracing failure; sometimes you may even find yourself spooning failure. It can be very liberating.

To get a better idea how this works, let's try writing our own supervisor.

First let's put our messages into their own file.

<script src="https://gist.github.com/1011573.js?file=messages.rb">&nbsp;</script>

We just did that to separate the messages from the actor logic. Now for the actors.

<script src="https://gist.github.com/1011573.js?file=reliable_actor.rb">&nbsp;</script>

This a naive implementation of a supervisor, but it demonstrates the concept. Let's take a look at what's here, then we can talk about how it can be improved.

We've stored our work as a proc this time, so we can re-use it if we have to spawn a new actor when ours goes down.

We've also set #trap\_exit to true, so that our supervisor will be alerted when the worker exits for any reason. Otherwise we'd never know when a child stopped running.

When we spawn our worker, we use #spawn\_link instead of spawn. #spawn\_link is responsible for setting up the relationship between the supervisor and the workers.

In our supervisor, we only handle the DeadActorError messages. When we receive one of those, we log the error and then spawn a new worker. All other messages are passed to the worker to handle. Since we're embracing failure, we're not worried about one of the messages causing our worker to crash, since our supervisor can just restore it.

There's a lot more to say about actors and supervision hierarchies. This implementation is quite incomplete. For example, we are only supervising a single worker, but we could be managing several workers. Also, our service could be made up of more then one type of actor. Maybe our supervisor could filter messages and route Livestock to a launcher and taunts to French soldier. It's also very likely that, in a complex system, there would be a logging service. Rather then putting actor failure messages to the console, we could send the error to the logger as a message and let the logger figure out who needs to be alerted about the failure.

Supervisors can be fairly complex, but fortunately, the logic is seldom domain specific. Many actor libraries/systems already come with hardened supervisor implementations available for you to use. For example, [Erlang/OTP](http://www.erlang.org/doc/design_principles/sup_princ.html#5) ships with several, including a one-for-one implementation (which is sorta what we wrote here).

There are still other powerful concepts to be explored with actors. So far our actors have all been local, but actors can be a powerful tool in a distributed system. I haven't dug that deep into the Rubinius code base yet, so I'm curious myself to see how that might be implemented.
