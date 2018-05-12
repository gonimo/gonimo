{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Host
Description : User facing Host API
Copyright   : (c) Robert Klotzner, 2018
Platform independent interface to native host, e.g. Android functionality.
-}
module Gonimo.Client.Host where


import           Gonimo.Client.Prelude





-- | Configuration for creating an host.
--
--   Currently this is Android only. Once we have iOS support we will modify
--   this interface to suit both platforms.
data Config t
  = Config { -- | Keep the app alive.
             --
             --   As long as this behavior is `False`, the app should not be killed.
             --   This should be set to `False`, when connections are open
             --   (parent) or the baby station is active.
             --
             --   If this flag is `False` we try hard to not get killed, if this
             --   is `True` and `onStop` got triggered we will kill ourself
             --   after sometime to save battery.
             _appKillMask :: Behavior t Bool

             -- | Request the app to be killed.
             --
             --   This honors `_appKillMask`, so if `_appKillMask` is
             --   `False` this event is ignored.
           , _onKillApp :: Event t ()
           } deriving (Generic)

-- | Host functionality.
data Host t
  = Host { -- | Android onStop like event.
           --
           --   App is in the background and might get killed.
           _onStop :: Event t ()

           -- | Android onStart like event.
           --
           --   App is in foreground again.
         , _onStart :: Event t ()

           -- | A new intent was delivered and should be handled.
         , _onNewIntent :: Event t Intent
           -- | Do we need our native History implementation
           --
           --   (`Gonimo.Client.Router.Impl.Native`) on this host?
         , _needsNativeHistory :: Bool
         }

-- | Type alias for intent data.
type Intent = Text

instance Reflex t => Default (Config t) where
  def = Config (pure True) never

instance Reflex t => Semigroup (Config t) where
  Config kill1 killApp1 <> Config kill2 killApp2
    = Config
      (liftM2 (&&) kill1 kill2)
      (killApp1 <> killApp2)

instance Reflex t => Monoid (Config t) where
  mempty = def
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev = do
      _appKillMask <- switcher (pure True) $ _appKillMask <$> ev
      _onKillApp <- doSwitch never (_onKillApp <$> ev)
      pure $ Config {..}

-- Auto generated lenses ...

class HasConfig a where
  config :: Lens' (a t) (Config t)

  appKillMask :: Lens' (a t) (Behavior t Bool)
  appKillMask = config . go
    where
      go :: Lens' (Config t) (Behavior t Bool)
      go f config' = (\appKillMask' -> config' { _appKillMask = appKillMask' }) <$> f (_appKillMask config')


  onKillApp :: Lens' (a t) (Event t ())
  onKillApp = config . go
    where
      go :: Lens' (Config t) (Event t ())
      go f config' = (\onKillApp' -> config' { _onKillApp = onKillApp' }) <$> f (_onKillApp config')


instance HasConfig Config where
  config = id

class HasHost a where
  host :: Lens' (a t) (Host t)

  onStop :: Lens' (a t) (Event t ())
  onStop = host . go
    where
      go :: Lens' (Host t) (Event t ())
      go f host' = (\onStop' -> host' { _onStop = onStop' }) <$> f (_onStop host')


  onStart :: Lens' (a t) (Event t ())
  onStart = host . go
    where
      go :: Lens' (Host t) (Event t ())
      go f host' = (\onStart' -> host' { _onStart = onStart' }) <$> f (_onStart host')


  onNewIntent :: Lens' (a t) (Event t Text)
  onNewIntent = host . go
    where
      go :: Lens' (Host t) (Event t Text)
      go f host' = (\onNewIntent' -> host' { _onNewIntent = onNewIntent' }) <$> f (_onNewIntent host')


  needsNativeHistory :: Lens' (a t) Bool
  needsNativeHistory = host . go
    where
      go :: Lens' (Host t) Bool
      go f host' = (\needsNativeHistory' -> host' { _needsNativeHistory = needsNativeHistory' }) <$> f (_needsNativeHistory host')


instance HasHost Host where
  host = id

