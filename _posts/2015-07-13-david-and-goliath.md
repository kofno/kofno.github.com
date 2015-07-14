---
layout: default
title: "The Monolith and the Piper"
---

{{ page.title }}
----------------

I've worked on several successful Rails monoliths, so what I'll say next may seem contradictory: I definitely prefer
smaller code bases. Here’s the rub: a complex network of small web applications is still a large app, only now the 
complexity is moved into the spaces between the apps. In routers and over wires. I can’t deal with that in code. And
the other complexity is that I suddenly have to deal with how to join data from disparate systems, which is something 
I _can_ deal with in code, but don’t want to. I think there may be a way to proxy in front of, say, 20 node processes 
and make that feel like a one application and do it with minimal pain; that’s kinda how node is designed, and how its 
adherents operate. This is not so with Rails…

