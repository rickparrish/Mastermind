Mastermind
==========

A BBS version of the <a href="https://en.wikipedia.org/wiki/Mastermind_%28board_game%29">Mastermind board game</a>.

I really enjoyed this game when I was younger, so it's been my go-to for implementing to test out new door kits.  This one uses my <a href="https://github.com/rickparrish/RMDoor">RMDoor</a>,
so you'll need to fork/download that as well if you want to compile this.

- Windows Server 2012 (64bit)<br />
 - Lazarus 1.0.14 (32bit) (FreePascal 2.6.2)<br />
 - Just open Mastermind.lpi and build<br />
 - Tested with GameSrv<br />

<!--
- Windows 7 (32bit)
 - FreePascal 2.6.4 (GO32V2)
 - build-go32v2.cmd
 - Tested with GameSrv

- Ubuntu Server 13.10 (32bit):
 - FreePascal 2.6.2 (32bit) (also needs fp-units-fcl)
 - build-linux.sh
 - Tested with Synchronet

- FreeBSD 9.1 (32bit):
 - FreePascal 2.6.0 (32bit)
 - cd /usr/ports/lang/fpc && make install && make clean
 - fpc -B -Furmdoor -FEbin/i386-freebsd LORD2.lpr
 - Tested with Synchronet
 - NOTE: These instructions are now out of date.  You should probably just create a new script based off the build-linux.sh script and use that to compile.
-->