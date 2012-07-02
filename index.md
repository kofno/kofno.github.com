---
title: Kofno
layout: default
---

Articles
--------
I think I'm going to try blogging using the Jekyll and Github pages. This is that blog.

I've decided to start pushing the articles up, even when they aren't finished yet (iteratively, you might say). It's just how I work. I'll mark incomplete articles as Works in Progress (WIP).

{% for post in site.posts %}
  [{{ post.title }}]({{ post.url }}) 

{% endfor %}
