#!/bin/bash

#requires fp-units-fcl

cd /mnt/Z/Programming/Mastermind/source
# fpc -MObjFPC -Scghi -O1 -g -gl -vewnhi -Fi../obj/i386-linux -Fu../../RMDoor -Fu. -FU../obj/i386-linux/ -l -FE../bin/i386-linux/ Mastermind.lpr
fpc -B Mastermind.lpr -MObjFPC -Scghi -O1 -g -gl -vewnhi -Fi../obj/i386-linux -Fu../../RMDoor -Fu. -FU../obj/i386-linux/ -l -FE../bin/i386-linux/