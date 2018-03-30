[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/gonimo/Lobby)

# Gonimo - good night monitor

Welcome to the sourcecode of [Gonimo](https://gonimo.com), the web based baby
monitor. You can give it a try [here](https://app.gonimo.com)!

Please join on on [Gitter](https://gitter.im/gonimo/Lobby) if you have any questions.

It is currently a bit complicated to get this to build, you need the jsaddle
branch of reflex-platform, but this is the easy part - you also need to update
some dependencies manually. I hope I can make this easier soon, in the meantime
if you'd liked to contribute, but can not, please file an issue (or comment on
an already existing issue) - this way I know there would be interest and this
will become a higher priority! Thank you!

Also currently the reflex build process is based on nix, which is great for me -
our servers are running nix anyway (nixops for the win!), but a problem for
quite a few. Which I fully understand, nix is pretty powerful and therefor much
harder to grasp than stack, if you get gonimo/reflex to build with a stack based
setup please contact me - this would certainly help others. In the meantime, I
will make the nix based setup easier, as soon as I have some time!
