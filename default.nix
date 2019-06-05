{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, nixpkgs ? obelisk.reflex-platform.nixpkgs
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; config.android_sdk.accept_license = true; };
let
  backendServer = if androidIsRelease
                  then "b00.gonimo.com"
                  else "b00.alpha.gonimo.com";

  frontendServer = if androidIsRelease
                   then "app.gonimo.com"
                   else "app.alpha.gonimo.com";

  androidVersionCode = "10";
  androidVersionName = "1.0.2.1";

  # androidIsRelease = builtins.pathExists ./release-key.nix;
  androidIsRelease = true;
in
project ./. ({pkgs, hackGet, ... }:
  let
    dontHaddock = pkgs.haskell.lib.dontHaddock;
  in {
    packages = {
      gonimo-common = ./gonimo-common;
      gonimo-back = ./back;
      gonimo-front = ./front;

      # gonimo-front-android = ./front-android;
      # gonimo-front-warp = ./front-warp;
      # gonimo-front-ghcjs = ./front-ghcjs;
    };

    android =
      {
        # executableName = "gonimo-front-android";
        applicationId = "com.gonimo.baby";
        version = { code = androidVersionCode;
                    name = androidVersionName;
                  };
        displayName = "Gonimo";
        resources = ./static/res;
        activityAttributes = ''
          android:launchMode="singleInstance"
          android:alwaysRetainTaskState="true"
        '';
        intentFilters = ''
          <intent-filter>
              <action android:name="android.intent.action.VIEW"/>
              <category android:name="android.intent.category.DEFAULT"/>
              <category android:name="android.intent.category.BROWSABLE" />
              <data android:scheme="https" />
              <data android:host="${frontendServer}" />
              <data android:pathPattern="/.*" />
          </intent-filter>
        '';
        permissions = ''
          <uses-permission android:name="android.permission.CAMERA" />
          <uses-permission android:name="android.permission.WAKE_LOCK" />
          <uses-permission android:name="android.permission.RECORD_AUDIO" />
          <uses-permission android:name="android.permission.MODIFY_AUDIO_SETTINGS" />
          <uses-permission android:name="android.permission.REQUEST_IGNORE_BATTERY_OPTIMIZATIONS" />

          <uses-feature
              android:name="android.hardware.camera"
              android:required="false" />
          <uses-feature
              android:name="android.hardware.camera.autofocus"
              android:required="false" />

          <meta-data android:name="android.webkit.WebView.MetricsOptOut"
                android:value="true" />

        '';
        services = ''
          <service
            android:name=".GonimoRunning"
            android:label="@string/app_name"
            android:icon="@drawable/ic_launcher"
            android:exported="false"
            android:description="@string/service_description"
          >
          </service>
        '';

        releaseKey = if androidIsRelease
                     then import /home/robert/projects/gonimo/release-key.nix
                     else null;
      };

    overrides = self: super:
      let
        reflexDomPkg = hackGet ./dep/reflex-dom;
        reflexDom = import reflexDomPkg self nixpkgs;
      in
        {
          gonimo-front = dontHaddock super.gonimo-front;

          android-activity = self.callPackage (hackGet ./dep/android-activity) {
            inherit (nixpkgs.buildPackages) jdk;
          };

          # WARNING: We don't conditionally enable library profiling, we also ignore the reflex
          # optimizer (as it is disabled in reflex-platform by default):
          reflex-dom = reflexDom.reflex-dom;
          reflex-dom-core = reflexDom.reflex-dom-core;
        };

    withHoogle = false;

    tools = ghc : with ghc; [
      # ghc-mod
      # gonimo-deploy has the very heavy conduit dependency - makes nix-shell build a lot of stuff.
      # We get rid of it for now:
      # (pkgs.haskellPackages.callPackage ./gonimo-deploy.nix {})
    ];

})
