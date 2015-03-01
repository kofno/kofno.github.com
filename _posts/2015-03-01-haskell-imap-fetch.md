---
layout: default
title: "Haskell DoIts: Fetch email from IMAP"
---

{{ page.title }}
----------------

This is, what I hope, will be the first in a line of posts about
building things with Haskell. The plan is to try and write a small
program that does something using Hakell and Haskell libraries. This
post will specifically be about fetching messages from IMAP and
parsing them. Since I'm just a neophyte Haskell hobbiest myself, I'll
be learning as I go. Keep that in mind if you plan of using any of
this in your own code.

This post is written in Literate Haskell, which means the source file
is actually an executable Haskell program. I'm simply annotating it
with all of blog post fluff.

Let's start with a module declaration. Since this is going to be
executable, This module will be named Main. I could leave the module
declaration off.

``` sourceCode
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
```

``` sourceCode
module Main where
```

Next we have our imports. I'm going to be very specific about what I
import, if only to make it easier for me to learn where functions
live.

``` sourceCode
import System.Environment (getEnv)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.List (find)
import Data.Maybe
import Network.HaskellNet.IMAP.Types (UID)
import Network.HaskellNet.IMAP ( SearchQuery (ALLs)
                               , login
                               , select
                               , search
                               , logout
                               , fetch
                               )
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.SSL (Settings (..))
import Network.HaskellNet.IMAP.SSL ( connectIMAPSSLWithSettings
                                   , defaultSettingsIMAPSSL
                                   )
import Codec.MIME.Type  (MIMEValue (..), MIMEParam (..))
import Codec.MIME.Parse (parseMIMEMessage)

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
```

This is the signature of the main function (which drives our
program). Basically, all Haskell programs boil down to a function
doing IO.

``` sourceCode
main :: IO ()
```

Since our program will poll an IMAP server for email, we want it to
run forever. Conveniently, Haskell has a function for this.

``` sourceCode
main = forever $ do
```

Next we establish our connection to the server. This example uses
Gmail, so we need to connect over SSL.

``` sourceCode
  conn <- connectIMAPSSLWithSettings imapServer imapCfg
```

Then we authenticate to the server. The `username` and `password`
functions are pulling our credentials from the environment, which is
in IO, so we need to unwrap those values before we can pass them to
`login`, which is only expection `String`s.

``` sourceCode
  user <- username
  pass <- password
  login conn user pass
```

Now that we've established a connection, we are going to grab a list
of messages from the INBOX. We select the INBOX, and then we use an
IMAP query to fetch a list of UIDs. These are ids that uniquely
identify messages *for our current imap session*. We'll use the UIDs
shortly to fetch the actual message content.

``` sourceCode
  select conn "INBOX"
  uids <- search conn [ALLs]
```

This next line of code is just mapping over the uids. It's structure
is simply `map _some_function_ uids`. The *some\_function* in this case
is a *composition* of three functions. The function composition does
quite a few things, but from the names, its pretty easy to see what's
going on; reading the compostion "backwards", it is fetching the
message over the imap connections, grabbing the message id out of the
message, and then puting the message to standard out.

``` sourceCode
  mapM_ (putMessageID . getMessageID . fetchMessage conn) uids
```

When we are done with our work, we logoff this connection.

``` sourceCode
  logout conn
```

And print out a message so we know when things are happening (maybe we
don't have a high volume INBOX).

``` sourceCode
  putStrLn "Fetch complete"
```

This last line puts our program thread to sleep for a minute. When it wakes up,
it will poll the IMAP server again.

``` sourceCode
  threadDelay (10^6 * 60)
```

`fetchMessage` grabs the entire message from the server and converts
it to Text, assuming the bytestring is UTF-8 encoded. The imap library
actually has functions for pulling down just headers, or subsets or
headers, or whatever, but let's just assume that we have some grand
plan that requires the entire message.

``` sourceCode
fetchMessage :: IMAPConnection -> UID -> IO Text
fetchMessage conn uid = do
  content <- fetch conn uid
  return $ decodeUtf8 content
```

Every email message *should* have a Message-ID header that uniquely
identifies that message in the universe of all email. `getMessageID`
parses the message content and then hands it to messageID, which
extracts the Message-ID content from the headers.

I use a compostion trick later with `>>=`, but I couldn't figure out
how to make that work with the types here. That's why I'm just using
the 'do' syntax to unwrap the content from IO. ¯\_(ツ)\_/¯

``` sourceCode
getMessageID :: IO Text -> IO Text
getMessageID raw = do
  content <- raw
  mapM_ TIO.putStrLn $ headers (parseMIMEMessage content)
  return $ pluckMessageID (parseMIMEMessage content)
 where
   headers MIMEValue {..} = map headerName mime_val_headers
   headerName (MIMEParam header _) = header
```

`pluckMessageID` looks through the parsed email message for the Message-ID
header. When it finds it, it returns it. If there isn't a Message-ID
header, it returns a dummy value message. This is what the Maybe type
provides; a data type for representing a computation that may not
return a value. It is actually implemented in terms of the more
general `pluckHeaderValue`.

```haskell
pluckMessageID :: MIMEValue -> Text
pluckMessageID = pluckHeaderValue messageIDHeader

messageIDHeader :: Text
messageIDHeader = "message-id"

pluckHeaderValue :: Text -> MIMEValue -> Text
pluckHeaderValue headerName MIMEValue{..} =
  valueOrDefault $ find headerMatch mime_val_headers
  where
    headerMatch :: MIMEParam -> Bool
    headerMatch (MIMEParam headerName' _) = headerName' == headerName

    valueOrDefault :: Maybe MIMEParam -> Text
    valueOrDefault Nothing = T.concat ["No ", headerName]
    valueOrDefault (Just (MIMEParam _ value)) = value
```

`putMessageID` just spits the message out to standard out. It has to be called in such a way that it can handle the fact that our Text was the result of an IO operation. That's what the `>>=` is all about; it basically unwraps the text from the IO (someone will HATE that description)

```haskell
putMessageID :: IO Text -> IO ()
putMessageID msgID = msgID >>= TIO.putStrLn
```

Most of the hard work is done. The rest of these functions are simply
for binding our configuration to names we can use in the program.

Here we are hard coding our imap server, but you could certainly pull this from
the environment or from the command line arguments if you prefer.

```haskell
imapServer :: String
imapServer = "imap.gmail.com"
```

I was having trouble connecting to Gmail until I copied this configuration from the
example code. We're using this for our configuration, rather then just the default
settings.

```haskell
imapCfg :: Settings
imapCfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }
```

As noted earlier, we are pulling username and password from the
environment, if these environment varaibles are missing the program
will fail with an exception. We're notedealing with exceptions in this
post, so we're ok with that for now.

Note the method signature; since these values come from the
environment, they are `IO String` values. That's why we need to
process them as part of IO monad before we can pass the `String`
values on. There are probably more Haskell-y ways to do this, but this
was most clear to me.

```haskell
username :: IO String
username = getEnv "IMAP_USER"

password :: IO String
password = getEnv "IMAP_PASS"
```

There we have it! A small Haskell program that fetches email over IMAP
and then parses the content.

There are plenty of ways that we can make this program better. For one
thing, I'm sure my Haskell code could be much better. Also, this
program isn't very reliable. If our IMAP connection goes away, for
example, the program will crash with a broken pipe exception. Writing
a more reliable version of this program would be a good topic for
another post, so I'll leave this code where it is for now.
