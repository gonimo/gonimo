---
title: 'The Gonimo Architecture - Modular MVC architecture for reflex'
description: 'Nice, flexible, modular and usable architecture for reflex based programs'
tags: 'haskell reflex reflex-dom'
---

# Introduction

When people are talking about reflex, you will often hear a phrase like:

```
Reflex is not really complex or hard, but it can be hard to figure out how to
properly structure your application. Patterns and best practices, still need to
be developed and documented.
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
other device will be able to claim the invitation.  - Just to provide a bit of
context.

## The Model

As suggested by the title, we build a classical "Model - View - Controller"
architecture, with the model playing the essential role. What the model is, in
the Gonimo architecture, is actually dependent on whom you ask. Every component
has it's own view on the model. The component's model is basically the
components the component depends on. This is important and is the key component
to understand, it makes the architecture modular.

The model gets passed in, into a component, providing it with it's dependencies.

For `Account`
the
[model](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs#L45) is
simply
the
[Server](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Server.hs#L33),
providing it with the means to react to messages coming from the server:

```haskell
type Model t = Server t
```

The above given model, actually just serves as an example satisfying our [HasModel](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs#L48) constraint. It is useful for testing the component in isolation, but other than that we resort to a polymorphic type satisfying `HasModel`:

```haskell
-- | Our dependencies
type HasModel model = Server.HasServer model
```

for the point of illustrating the model of a component with more requirements, here you can see the model definitions for [Subscriber](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Subscriber.hs#L41):

```haskell
data Model t
  = Model { __server :: Server t
          , __auth   :: Auth t
          }


-- | Constraint on needed dependencies.
type HasModel model = (Server.HasServer model, Auth.HasAuth model)
```

It does not only depend on the server but also
on
[Auth](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Auth/API.hs#L15),
which provides an event that signals successful authentication.

## The Config

In addition to the model, a component might define some component specific
configuration data type. E.g. some events triggering the component to do some
work. In our example the [Account.Config](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account/API.hs#L19) looks like this:

```haskell
data Config t
  = Config { -- | Claim an invitation by providing it's secret.
             _onClaimInvitation :: Event t [InvitationSecret]
             -- | Answer an invitation. (Decline/accept it.)
           , _onAnswerInvitation :: Event t [(InvitationSecret, InvitationReply)]
} deriving (Generic)
```

So Account can be told to claim an invitation and to respond to an invitation. This is the public facing API of `Account`, depending components can ask `Account` to claim an invitation by providing a config type that fullfills [Account.HasConfig](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account/API.hs#L60). At this point it should be noted, that in this architecture modules are intended to be imported qualifed.

## The ModelConfig

So a component takes optionally a `Model` and a `Config`, the [ModelConfig](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs#L51) or rather some type fulfilling [HasModelConfig](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs#L63) is one of two optional return values of the components [make](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account.hs#L72) function:

```haskell
-- | Example datatype fulfilling 'HasModelConfig'.
data ModelConfig t
  = ModelConfig { -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                  _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig     :: Server.Config t
                }

-- | Configurations we provide for the model as inputs.

type HasModelConfig c t = (IsConfig c t, Subscriber.HasConfig c, Server.HasConfig c)
```

The `ModelConfig` consists of `Config` values for components `Account` depends on. In our case it depends on some [Subscriber.Config](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Subscriber/API.hs#L18) and [Server.Config](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Server.hs#L28).

Which means that our `Account` will be able to send messages to the server and
will be able to tell `Subscriber` to subscribe to changes on the server.

For other components, that in turn depend on `Account`, they will require `Account.Config` on their `ModelConfig`, in our case this is currently [UI code](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/AcceptInvitation/UI.hs#L30).

So to recap, 'Config' is your components configuration, `ModelConfig` is the configuration you provide for your dependencies. You take a config, you provide a `ModelConfig`.

## The Component Type itself

In addition to a `ModelConfig` triggering actions in other components, a
component will usually provide some information/functionality which then in turn
will serve as part of the model for other components. This data is usually named
like the component, in our case
[Account](https://github.com/gonimo/gonimo/blob/5afd58dfd6e21525c0688508d978429b51bc85f7/front/src/Gonimo/Client/Account/API.hs#L29):

```haskell
-- | Account data.
--   All data belonging to the current active account should go here. Like
--   claimed invitations or user name, ...
data Account t
  = Account { -- | Invitations currently claimed by the account. (At the moment,
              --   just the ones claimed in this session.)
              _claimedInvitations :: Dynamic t ClaimedInvitations
}

type ClaimedInvitations = Map InvitationSecret InvitationInfo

```

So our account provides a dynamic Map consisting of claimed invitations, that
need to be responded to. Components needing this information, will simply have
a [HasModel](https://github.com/gonimo/gonimo/blob/2073d6e10e2862a016bf46770ad3c457d685fc0f/front/src/Gonimo/Client/AcceptInvitation/UI.hs#L32) definition that includes `Account.HasAccount`.

# How it works

Now that you know all the ingredients, let's have a look how this all plays out together:

- Classy lazy lenses
- Using HasModel & HasModelConfig of child components, to be robust to changes in dependencies.

# Trash:
## Modularity

Simple self-contained components, with a well defined interface and hidden
implementation are easily replaced, with a different component offering the
same interface, can be re-used and can be tested independently.

It should be clear from the exposed definitions, how the component is going to be used. It should not be required

I will explain each of them individually, but first let me note that especially (1) and (2) seem to be mutually exclusive.
Solution: Typeclasses and lazy lenses.
