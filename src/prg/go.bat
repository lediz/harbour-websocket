@echo off
set oldpath=%path%
set path=c:\bcc582\bin;c:\harbour\bin
hbmk2 -mt -trace -lhbwin %1
set path=%oldpath%
