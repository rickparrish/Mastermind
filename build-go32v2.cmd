@echo off
z:
cd \programming\Mastermind\source
rem fpc -MObjFPC -Scghi -O1 -g -gl -vewnhi -Fi..\obj\i386-go32v2 -Fu..\..\RMDoor -Fu. -FU..\obj\i386-go32v2\ -l -FE..\bin\i386-go32v2\ Mastermind.lpr
fpc -Scghi -O1 -g -Fi..\obj\i386-go32v2 -Fu..\..\RMDoor -Fu. -FU..\obj\i386-go32v2\ -FE..\bin\i386-go32v2\ Mastermind.lpr
pause