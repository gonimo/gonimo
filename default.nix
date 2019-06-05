{}:
let
  backendServer = if androidIsRelease
                  then "b00.gonimo.com"
                  else "b00.alpha.gonimo.com";

  frontendServer = if androidIsRelease
                   then "app.gonimo.com"
                   else "app.alpha.gonimo.com";

  androidVersionCode = "11";
  androidVersionName = "1.0.2.2";

  androidIsRelease = builtins.pathExists ./release-key.nix;
in
(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    gonimo-common = ./common;
    gonimo-back = ./back;
    gonimo-front = ./front;
    gonimo-front-android = ./front-android;
    gonimo-front-warp = ./front-warp;
    gonimo-front-ghcjs = ./front-ghcjs;
  };

  android.gonimo-front-android =
  let
    gonimoAssets = pkgs.stdenv.mkDerivation {
      name = "gonimoAssets";
      src = ./front/static;
      installPhase = ''
        mkdir -p $out
        cp -R $src/* $out/
        cd $out/js
        chmod u+w .
        mv env.js env-orig.js
        cat env-orig.js | sed  's/gonimoBackServer.*/gonimoBackServer : "${backendServer}",/1' | sed 's/secure.*/secure : true,/1' > env.js
        cat env.js
      '';
    };
  in
  {
    executableName = "gonimo-front-android";
    applicationId = "com.gonimo.baby";
    version = { code = androidVersionCode;
                name = androidVersionName;
              };
    displayName = "Gonimo";
    assets = gonimoAssets.out;
    resources = ./front/static/res;
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
                 then import ./release-key.nix
                 else null;
  };

  overrides = self: super: {};
    # import ../reflex-dom/default.nix self pkgs;
    # in
    #   {
    #     inherit (reflex-dom-pkg) reflex-dom-core reflex-dom;
    #   # reflex-host = self.callPackage ./reflex-host.nix {};
    #   #       lens = self.callHackage "lens" "4.15.4" {};
    #   #       free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
    #   #         owner = "ekmett";
    #   #         repo = "free";
    #   #         rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
    #   #         sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
    #   #       }) {};
    #   };

  withHoogle = false;

  tools = ghc : with ghc; [
    # ghc-mod
    # gonimo-deploy has the very heavy conduit dependency - makes nix-shell build a lot of stuff.
    # We get rid of it for now:
    # (pkgs.haskellPackages.callPackage ./gonimo-deploy.nix {})
  ];

  shells = {
    ghc = ["gonimo-common" "gonimo-back" "gonimo-front" "gonimo-front-warp" ];
    ghcjs = ["gonimo-common" "gonimo-front" "gonimo-front-ghcjs" ];
  };
})
