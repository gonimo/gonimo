---
title: 'The Gonimo Architecture - Modular MVC architecture for reflex'
description: 'Nice, flexible, modular and usable architecture for reflex based programs'
tags: 'haskell reflex reflex-dom'
---

# Introduction

When people are talking about reflex, you will often hear a phrase like:

```
Reflex is not really complex or hard, but it can be hard to figure out how to properly structure your application. Patterns and best practices, still need to be developed and documented.
```

Well that's certainly true. I started
with [reflex](https://github.com/reflex-frp/reflex)
and [reflex-dom](https://github.com/reflex-frp/reflex-dom) in December 2017,
with a rewrite of [gonimo](https://gonimo.com), which was formerly written
in [PureScript](http://www.purescript.org/)
with [purescript-pux](https://github.com/alexmingoia/purescript-pux). I was
under deadline pressure, and new to reflex so I coded it down in a rush in about
4 - 5 months. The resulting code has basically no architecture at all and is
neither really modular, nor maintainable nor easily extensible and also rather
error-prone to work with.

Lately, I took some time, inspired by some discussions I had on [Haskell Exchange 2017](https://skillsmatter.com/conferences/8522-haskell-exchange-2017) with awesome people like [Christian Takle](https://github.com/christiantakle), to design a proper architecture for [Gonimo](https://github.com/gonimo/gonimo) and [reflex](https://github.com/reflex-frp/reflex) based applications in general. Most of Gonimo is still not ported to this new architecture, but the parts I ported look already pretty promising, so I am going to write about it.

# Intended Audience

People who are familar with FRP
and [reflex](https://github.com/reflex-frp/reflex) in particular and are
wondering how they can structure their applications better.

Spoiler alert, this is how a top level reflex wiring can look like:

```haskell
app :: forall t m. AppConstraint t m => Config -> m ()
app conf' = build $ \ ~(modelConf, model) -> do
  liftIO $ putStrLn "Loaded - yeah!"

  conf                     <- toModelConfig conf'

  __environment            <- Environment.make

  __server                 <- Server.make model modelConf

  (authConf, __auth)       <- Auth.make model

  (accountConf, __account) <- Account.make model modelConf

  subscriberConf           <- Subscriber.make model modelConf

  __settings               <- Settings.make modelConf

  uiConf                   <- makeUI model

  pure ( mconcat [ conf
                 , authConf
                 , accountConf
                 , subscriberConf
                 , uiConf
                 ]
       , Model {..}
       )
  where
    build :: ((ModelConfig t, Model t) -> m (ModelConfig t, Model t)) -> m ()
build = void . mfix
```

Note that the above code has the UI properly separated and is fully modular. The
Wiring is partly automated, and the compiler will warn you if you missed a
wiring. (Unused reference warning)

# Goals of this Architecture

The architecture should:

1. make an application modular
2. enable proper separation of logic from UI
3. make an application robust against coding errors
4. make an application maintainable and easily extensible
5. be fun, actually the most important part and of course related to points 1 to 4.

Let's dive in.

# Basic concepts

The basic concepts of the architecture are listed below, the currently pretty simple [Account](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs) component of Gonimo, will do as the running example.

`Account` currently only has a single purpose, it offers a `Dynamic` containing invitations currently claimed by the client.

```haskell
data Account t
  = Account { -- | Invitations currently claimed by the account. (At the moment,
              --   just the ones claimed in this session.)
              _claimedInvitations :: Dynamic t ClaimedInvitations
            }

type ClaimedInvitations = Map InvitationSecret InvitationInfo
```

Gonimo is a baby monitor, which needs devices to be coupled for operation.
Coupling is implemented by sending out invitations, which are then claimed by
the receiving device before responding to it. Once an invitation is claimed, no
other device will be able to claim the invitation. Just to provide a bit of
context.

## The Model

As suggested by the title we build a classical "Model - View - Controller"
architecture, with the model playing the essential role. What the model is, in
the Gonimo architecture, is actually dependent on whom you ask. Every component
has it's own view on the model. The component's model is basically the
components the component depends on. This is important and is the key component
to understand, it makes the architecture modular.

The model gets passed in, into a component, providing it with it's dependencies.


## The Config

In addition to the model, a component might define some component specific configuration data type. E.g. some events triggering the component to do some work.
## The ModelConfig


## Modularity

Simple self-contained components, with a well defined interface and hidden
implementation are easily replaced, with a different component offering the
same interface, can be re-used and can be tested independently.

It should be clear from the exposed definitions, how the component is going to be used. It should not be required

I will explain each of them individually, but first let me note that especially (1) and (2) seem to be mutually exclusive.
Solution: Typeclasses and lazy lenses.
