---
layout: default
title: "Purescript FFI - Using Node Modules"
---

{{ page.title }}
----------------

There are many creative and talented developers working in JavaScript. The
sheer volume of quality libraries produced is astounding. Purescript draws some
of its power from tapping into these existing libraries.

In an earlier article, I wrote a simple and mostly useless example of using
Purescript on AWS Lambda. Writing a useful Lambda example requires more then
what puescript core offers. We could build these features in Purescript, but
that is time consuming. Instead, we will use existing Javascript libraries for
the functionality we need.

In this example, we will write Purescript FFI bindings for the node `gm` module
([https://www.npmjs.com/package/gm](https://www.npmjs.com/package/gm)). This is
also my first time using Aff for asynchronous effects. This library is a work
in progress. You can find it on
[github](https://github.com/kofno/purescript-gm).

#### gm as types

gm is an image manipulation module for node. It uses command line tools in
child processes to read and modify images. To use gm, you need to install
either GraphicsMagick or ImageMagick.

To use gm in Javascript, you require the gm module or its ImageMagic subclass.
We will need a way to represent these modules in Purescript. Since the gm
module and its subclass are interchangeable, we only need on representation.

If you've used gm, you know that the gm module is actaully a function. Because
the gm function has variable argument types and arity, we can't represent it as
a single type signature in Purescript. Fortunately, we don't have to.

We will only construct and apply gm in the Javascript side of the ffi. For
Purescript, we only represent gm as a type.

    foreign import data GM :: *

We use GM to get access to another, stateful object. This object is contructed
in Javascript. It records all the transformations we will perform and then
executes them. For Purescript, we only need to hold a reference to the ojbect.
This requires us to declare a simple data type.

    foreign import data GMObject :: *

The gm module performs IO, and relies on state mutation. We could try to break
down exactly which side effects are performed on each operation. Instead, I'm
just going to create one effect type to document all gm related side effects.

    foreign import data GRAPHICS_MAGIC :: !

These two types and one side effect represent the foreign data structures. I
will define a few other type aliases as we go along. For now, we have enough to
begin importing functions.

#### gm functions

Now we are going to write Javascript functions that we can call from
Purescript. Purescript expects functions to be curried. We need to write a
layer of Javascript to be a bridge from Purescript to gm.

So far, we've only worked in Purescript and Puresript files. We write our type
signatures for Javascript functions in Purescript. The implementations are
written in a Javascript file.

The Javascript file should have the same name as the Purescript file. The file
extension will be .js instead of .purs. The Javascript file also needs a
comment in it identifying the Purescript module it is associated with. That
comment looks like this:

    // module Node.GM

Earlier we defined a type for GM. Now we need to get a GM instance into
Purescript. Remember that GM could be using the GraphicsMagick binary or the
ImageMagick binary. We need to be able to pick one of those from Purescript.
The types for these look like this:

    -- | A GraphicsMagick GM
    foreign import gm :: GM

    -- | An ImageMagick GM
    foreign import im :: GM

These are the only pure (side effect free) functions in the library.

The Javascript implementation of these is very short.

    exports.gm = require('gm');
    exports.gm = require('gm').subClass({ imageMagick: true });

Now we have our GM. We can begin building up a GMObject. One way we can get a
GMObject is to start with an image file. We can use the GM and a file path to
start our transformation.

    -- | Creates a GMOject from a file
    foreign import gmFile :: forall eff.
                             GM
                          -> FilePath
                          -> Eff (gm :: GRAPHICS_MAGIC | eff) GMObject

We declared the gmFile function to accept a GM and FilePath. It returns a
GMObject, but there are also side effects.

We haven't defined `FilePath` yet. It looks like this:

    type FilePath = String

This is a type alias. The compiler checks type aliases, but there is no
additional runtime overhead for FilePath. It is treated like any other String.

The Javascript implementation looks like this:

    exports.gmFile = function(gm) {
      return function(file) {
        return function() {
          return gm(file);
        };
      };
    };

Remember that Purescript requires our functions to be curried. The outer
function takes the GM as an argument. The next function takes the filepath. The
Filepath was just a type alias, so this filepath is just String. The last
function takes no arguments. It is the effect.

The actual implementation just applies the GM as a function. However, we can't
expose the gm function directly to Purescript, because it has many arities and
types.

If we want to apply the GM version that takes a Buffer, we write a different
function. The signature for that function looks like this:

    -- | Creates a GMObject from a Buffer
    foreign import gmBuffer :: forall eff.
                               GM
                            -> Buffer
                            -> FileName
                            -> Eff (gm :: GRAPHICS_MAGIC | eff) GMObject

This signature is similar to gmFile. It takes a Buffer and a FileName, but it
is still side effectual. Here's the implementation.

    exports.gmBuffer = function(gm) {
      return function(buffer) {
        return function(name) {
          return function() {
            return gm(buffer, name);
          };
        };
      };
    };

Now we have a couple of ways to get a GMObject. We can apply as many
transformations to the GMObject as we wish. We'll define a zero argument
transformation function first. The flip transformation reverses an image
vertically. It is a zero argument function in gm, but in purescript, we need to
pass the GMObject reference.

    -- | Flips the image vertically
    foreign import flipImage :: forall eff.
                                GMObject
                             -> Eff (gm :: GRAPHICS_MAGIC | eff) GMObject

Flip is already a prelude function in Purescript. I named the gm flip function
flipImage instead. It takes a GMObject and returns a GMObject. Again, all of
that happens in a side effect.

We implement flipImage in Javascript.

    exports.flipImage = function(gobj) {
      return function() {
        return gobj.flip();
      };
    };

We can write a function to resize images. We'll define a dimensions type to
pass as an argument to resize.

    type Dimensions = { height :: Int
                      , width  :: Int
                      }

    foreign import resize :: forall eff.
                             Dimensions
                          -> GMObject
                          -> Eff (gm :: GRAPHICS_MAGIC | eff) GMObject

Just like before, most of the implementation is taken up with currying.

    exports.resize = function(dims) {
      return function(gobj) {
        return function() {
          return gobj.resize(dims.width, dims.height);
        };
      };
    };

We aren't concerned with width or height being undefined. The Purescript type
system won't let us pass an object without a width and height.

Once we've transformed our image, we will save it to a file. We create a write
method in the Eff type.

    -- | Write the new image to disk
    foreign import write :: forall eff a b.
                            FilePath
                         -> GMObject
                         -> (a -> Eff eff Unit)
                         -> (Error -> Eff eff Unit)
                         -> Eff eff Unit

The first two arguments are the output file path and a GMObject. The next two
arguments are our callbacks. The first callback is valled is the write succeeds
and the second callback is called on an error.

You could use this function to write out your image. Instead, we're going to
use it to introduce a new type; Aff.

### the aff: asynchronous side effects

Aff is another type for handling side effects. Aff specifically handles
asynchronous side effects. It handles node style callback asynchrony, without
all the nasty callbacks.

The Aff library has a function that will convert out Eff write function in an
Aff write function.

    -- | Runs the write side effect in Aff
    write' :: forall eff. FilePath
                       -> GMObject
                       -> Aff eff Unit
    write' gmobj path = makeAff (\err success -> write gmobj path success err)

Notice how the `makeAff` function takes a function which accepts an error
callbak and a success callback. If you've programmed Javascript promises, this
might look more familiar to you written this way.

    makeAff (\reject resolve -> write gmobj path resolve reject)

Aff is very similar to a promise. With Aff, the `then`s and `catch`es are built in.

### a full example

Here is a full example of using this library, with Aff for writing to disk.

    module Main where

    import Prelude
    import Control.Monad.Eff.Class
    import Control.Monad.Eff
    import Control.Monad.Aff
    import Control.Monad.Aff.Console (print)

    import Node.GM

    main = launchAff $ do
      obj     <- liftEff $ gmFile (im) "examples/anonymous.jpg"
      orient  <- liftEff $ autoOrient obj
      flipped <- liftEff $ flipImage orient
      flopped <- liftEff $ flopImage flipped
      resized <- liftEff $ resize { height: 300, width: 600 } flopped
      e <- attempt $ write' "examples/anonymousFlipped.jpg" resized
      print $ show e

There are more examples in purescript-gm repository.

### more to do

This is a small portion of the gm feature set. I plan to implement most of the
library over the next week or so.

This library doesn't drift far from the Javascript approach. A future
enhancement would be to leverage more of Purescript's strengths. For example, I
think I'd like to build the transformations using pure functions and types. In
that scenario, only executing the transofrmation for have to side effectual

