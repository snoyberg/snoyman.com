---
title: Your Second Haskell Web App
---

## Your Second Haskell Web App

### A Yesod Workshop

* Michael Snoyman
* VP Engineering, FP Complete<br><img alt="FP Complete logo" src="https://tech.fpcomplete.com/images/fp-complete-logo-small.png" style="border:0">
* Berlin Functional Programming Group
    * c/o COVID-19
* Thursday, April 23, 2020

---

## Last time, on Functional Conf

* Previous talk: first web app with WAI and Warp
* We'll do a quick recap of that...
* Then explain what Yesod is
* This is an **interactive** workshop
* Get your keyboards ready!
* https://tech.fpcomplete.com/haskell/get-started

```
stack new mysecondapp yesodweb/simple
cd mysecondapp
stack build . yesod-bin
```

---

## Who are the players?

* Haskell: strongly typed functional programming language
* GHC: the compiler
* Stack: the build tool, wraps around GHC
* WAI: interface between apps and servers
* Warp: a Haskell web server
* Yesod: a web framework, built on WAI

---

## What is "built on WAI?"

* Yesod applications are WAI applications
* Can apply all of the WAI middleware on a Yesod app
* Can embed another WAI app inside a Yesod app
* Can run Yesod app on any WAI handler

---

## What is Yesod?

* It's boring!
* Standard MVC framework approach
* Original goal: what if a web framework had awesomeness of Haskell's type system?
* Want to prevent common bugs at compile time
* Want to enforce boundaries

---

## Type system can stop bugs?

* Yes it can!
* Cross-site scripting (XSS) by having an `HTML` data type
    * Generally: great way to avoid string escaping bugs
* Well-formed URLs are guaranteed by route data type
    * Can still get 404s due to, e.g., missing database ID
    * In practice, catches many common bugs

---

## Is boilerplate evil?

* Yes and no
* Yes: when it can lead to bugs
* Use codegen to avoid the problem
* High level DSL for routes
* Generates:
    * Route datatype
    * Parser
    * Renderer
    * Dispatcher

---

## Isn't this a workshop?

* Oh, right, it is
* Hold onto your butts
* __LIVE CODING__

Fair warning: first time I've ever done live coding in a talk.

---

## Scaffolding

* Run `stack new mysecondapp yesodweb/simple`
* Multiple scaffoldings, different DB backends
* Skipping database entirely here
* Run `cd mysecondapp`
* Run `stack build . yesod-bin`
* Run `stack exec yesod devel`
* Open http://localhost:3000

---

## Homepage

* Open up `templates/homepage.hamlet`
* Modify some of the text on the page
* Watch the recompile happen
* Reload the page, voila!

---

## Routing

* Open up `config/routes`
* Subsites: embed functionality
    * We're using static file serving
    * Create `static/foo.txt`
    * `curl -i http://localhost:3000/static/foo.txt`
    * Another common one: authentication
* `/` is called `HomeR`, supports `GET` and `POST`
* `/comments` is called `CommentR`, supports `POST`

---

## Our first route

* Say hi!
* Add `/greet GreetR GET`
* Recompile fails
* Add `import Handler.Greet` in `Application`
* Create `Handler.Greet`, copy stuff from `Handler.Home`
* Force recompile with enter

```haskell
getGreet :: Handler Html
getGreet = undefined
```

---

## Add some code

```haskell
{-# LANGUAGE QuasiQuotes #-}

getGreetR :: Handler Html
getGreetR = do
  defaultLayout $ do
    setTitle "Hello there!"
    [whamlet|<p>Hello there!</p>|]
```

---

## Add a link

* Open `templates/homepage.hamlet`
* Go to `Starting`
* Add `<a href="/greet">Greetings!`
* That's bad, mkay?
* `href=@{GreetR}`

---

## URL parameters

* What's your name?
* Change route: `/greet/#{Text}`
* `homepage.hamlet` breaks
* Error messages are difficult, sorry
* Change `getGreetR`
* Warning about unused variable
* Use the variable `#{name}`
* What about `/greet/<script>`

---

## Footgun

* `preEscapedToHtml`
* Add `import Text.Blaze.Html`
* Add `blaze-html` dependency
* Go to http://localhost:3000/greet/%3Cscript%3Ealert(%22danger%20will%20robinson%22)%3C%2fscript%3E

---

## Serving JSON

* `pure $ object ["name" .= name]`
* `Value` is a funny type
* Can provide multiple representations too

```haskell
getGreetR :: Text -> Handler TypedContent
getGreetR name = selectRep $ do
  provideRep $ pure $ object ["name" .= name]
  provideRep $ defaultLayout $ do
    setTitle "Hello there!"
    [whamlet|<p>Hello there #{preEscapedToHtml name}!</p>|]
```

curl -i http://localhost:3000/greet/Michael

---

## More info

* Check out the rest of the scaffolding
* Check out the book https://www.yesodweb.com/book
* What else do you want to see?
