# Nottyme

![demo](https://tyruiop.org/~tyruiop/demo3.gif)

## A simple OCaml clock

Just a little exercice to discover [notty](http://pqwy.github.io/notty/). It *does* display the time. And can be used for countdowns.

## How to use

Run to get time, press Enter to stop the clock. It does support resizing \o/. Press `c` to trigger the countdown, then enter digits and press `c` again to start it. A third `c` will reset the countdown. The background will blink during the last 5 seconds of the countdown.

## Build

Run `make` and then `./nottyme.native`. You need `notty` and `lwt`.
