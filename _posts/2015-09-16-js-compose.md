---
layout: default
title: "Functional Composition"
---

Functional Composition
-----------------------

Functional composition is the nesting two or more functions to form a new function.

In Object Oriented development, we talk about Object composition. This refers to implementing code reuse through other classes. It's generally described as a modular alternative to inheritance. Beyond that, it has no strict definition.

Functional composition comes from mathematics and has a more strict definition. But we will forgo mathematical notation here, in favor of Javascript and pseudocode.

Given functions `f` and `g`, assuming the output of `g` is valid input for `f`:


    compose(f, g)(x)  == f(g(x));


A naive implementation of compose might look like this:


    function compose(f, g) {
      return function(x) {
        return f(g(x));
      }
    }


Composition is associative. We can compose as many functions as we like.


    compose(h, compose(f, g)) == h(f(g(x)));


And we can group the functions any way that we want.


    compose(compose(h, f), g) == compose(h, compose(f, g))


So how might we use this?

Given the following JavaScript function, we will rewrite it using compose.


    var absFloor = function(n) {
      return Math.abs(Math.floor(n));
    }


The shape of this function is almost exactly our definition of composition. Now we can replace the explicit applications with compose:


    var absFloor = function(n) {
      return compose(Math.abs, Math.floor)(n);
    }


Since `compose` returns a new function, we can further reduce this function declaration:

    var absFloor = compose(Math.abs, Math.floor);


What have we done here?

We've just written our function "pointfree". We replaced our explicit function with a composition. Our code is more compact. We've suppressed the variable declaration. This definition emphasizes the functions, over the data.

With composition, we combine smaller bits of functionality into larger, more complex features. We can focus on how our functions fit together, rather then juggling data. The higher order construction leads to more declarative code.

### Exercises

Our compose function is incomplete. It may fail in some scenarios. Rewrite this compose function to be more robust. Consider `this` being re-bound. Also, allow the last function to accept more then one argument.

For extra credit, rewrite the compose function to accept any number of functions.
