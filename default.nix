{}:

(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    gonimo-common = ./common;
    gonimo-back = ./back;
    gonimo-front = ./front;
    gonimo-front-android = ./front-android;
    gonimo-front-warp = ./front-warp;
  };

  android.gonimo-front-android = {
    executableName = "gonimo-front-android";
    applicationId = "org.gonimo.gonimo";
    displayName = "Gonimo";
    assets = ./front/static;
    intentFilters = ''
      <intent-filter>
          <action android:name="android.intent.action.VIEW"/>
          <category android:name="android.intent.category.DEFAULT"/>
          <data android:scheme="https" />
          <data android:host="app.alpha.gonimo.com" />
      </intent-filter>
    '';
    permissions = ''
      <uses-permission android:name="android.permission.CAMERA" />
      <uses-permission android:name="android.permission.WAKE_LOCK" />
      <uses-permission android:name="android.permission.RECORD_AUDIO" />
      <uses-permission android:name="android.permission.MODIFY_AUDIO_SETTINGS" />
      <uses-feature android:name="android.hardware.camera.autofocus" />
      <uses-feature android:name="android.hardware.camera" />
    '';
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
    ghcjs = ["gonimo-common" "gonimo-front"];
  };
})
