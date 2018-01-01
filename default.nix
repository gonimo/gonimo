{}:

(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    gonimo-common = ./common;
    gonimo-back = ./back;
    gonimo-front = ./front;
  };

  android.front = {
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
      <uses-permission android:name="android.permission.RECORD_AUDIO" />
      <uses-permission android:name="android.permission.MODIFY_AUDIO_SETTINGS" />
      <uses-feature android:name="android.hardware.camera.autofocus" />
    '';
  };

  shells = {
    ghc = ["gonimo-common" "gonimo-back" "gonimo-front"];
    ghcjs = ["gonimo-common" "gonimo-front"];
  };
})
