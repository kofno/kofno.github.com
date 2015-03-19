---
layout: default
title: "Haskell Do-Its: Wrangling JS Assets"
---

{{ page.title }}
----------------

One convenient feature of Rails is the asset pipeline. It is a built-in
mechanism for packaging Javascripts and other assets in ways that are optimal
for delivery over the web. For example, packaging javascripts and serving them
as a single file to reduce server requests. Here we will look at the start of
such a feature in Happstack, uning JMacro.

Before we begin, we'll need some pragmas. OverloadedStrings is because we want
our string literals to be handled like Text where appropriate. QuasiQuotes is
needed because JMacro relies on them.

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE QuasiQuotes #-}

First we make our module declaration and imports.

> module Main where
>
> import Data.Monoid (mconcat)
> import Happstack.Server (Conf(..), ServerPart, Response, toResponse, simpleHTTP, nullConf, dir, ok)
> import Happstack.Server.JMacro
>
> import Control.Monad (msum)
> import Text.Blaze ((!))
> import Language.Javascript.JMacro
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A

A couple of the highlights from our imports:

Monoids: A monoid is an associative operation with an identity. String
concatenation, for example, can be grouped in any order, and uses an empty
string for identity (concatenating an empty string with any other string always
returns the other string)

Happstack: Happstack is the web library. I won't explain much about how
Happstack works, but for our purposes here, we are simply mapping paths to
functions (handlers).

Blaze: Blaze is how we will generate HTML. It's not essential to the examples
here, so I won't go into much detail about it here.

JMacro: JMacro is a library for programmatically generating javascript. It is a
superset of javascript and provides syntax checking, hygienic names, as well as
antiquotation and marshalling and unmarshalling of Haskell values. This is how
we will write and construct our javascript example.

The first snippet is from our someScript function. We write our javascript
inside the quasiquotes (that's the [jmacro||]). You can see that plain old
javascript will work quite nicely. This function returns a JStat instance. JStat
is a monoid, which is important for our stategy.

> someScript :: JStat
> someScript =
>   [jmacro|
>     function greet() {
>       console.log("thing")
>     }
>     window.logAThing = logAThing
>   |]

Here is another function returning a JStat. Since JMacro makes things hygienic,
we don't have to worry about the function names "greet", colliding.

> anotherScript :: JStat
> anotherScript =
>   [jmacro|
>    function greet() {
>      alert('a thing')
>    }
>    window.greet = greet
>   |]

This is where we combine our Javascript to create a single javascript resource
that we can serve to clients. Remember when I said that JStat was a monoid?
That's important because it makes combining JStats very easy; the mconcat
function.

> externalJS :: JStat
> externalJS = mconcat [ someScript
>                      , anotherScript
>                      ]

The rest of these functions are mostly about serving content using Happstack.

externalJsHandler serves our javascript from /js/script.js

appTemplate and helloBlaze render our html using Blaze.

jsConf configures our server to run on port 3000 (and a few other default
settings).

main lanches the server


> externalJsHandler :: ServerPart Response
> externalJsHandler =
>   dir "js" $ msum
>   [ dir "script.js" $ ok (toResponse externalJS)
>   ]
>
> appTemplate :: String -> [H.Html] -> H.Html -> H.Html
> appTemplate title headers body =
>   H.html $ do
>     H.head $ do
>       H.title (H.toHtml title)
>       H.meta ! A.httpEquiv "Content-Type"
>              ! A.content "text/html;charset=utf8"
>       sequence_ headers
>     H.body $ body
>
> helloBlaze :: ServerPart Response
> helloBlaze =
>   ok $ toResponse $
>     appTemplate "Hello, Blaze!"
>                 [ H.meta ! A.name "keywords"
>                          ! A.content "happstack, blaze, html"
>                 , H.script ! A.type_ "text/javascript" ! A.src "/js/script.js" $ ""
>                 ]
>                 (H.p $ do
>                   "Hello, "
>                   H.b "blaze-html!")
>
> jsConf :: Conf
> jsConf = nullConf { port = 3000 }
>
> main :: IO ()
> main = simpleHTTP jsConf $ msum
>        [ dir "hello" helloBlaze
>        , externalJsHandler
>        ]

This exercise demonstrated that it is fairly easy to emulate part of the asset
pipeline in haskell. We could further enhance this by also minimizing our
javascript using hjsmin, and by adding a hash to path, so that we can cache the
served content more aggressively. Perhaps we'll tackle those in future blog
posts.
