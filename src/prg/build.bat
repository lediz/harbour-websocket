cls
@echo off
SET PRG_COMP_PATH=c:\harbour
SET PRG_COMP_BIN_PATH=%PRG_COMP_PATH%\bin
SET PRG_COMP_LIB_PATH=%PRG_COMP_PATH%\lib\win\mingw
SET PRG_COMP_INC_PATH=%PRG_COMP_PATH%\include
SET CC=c:\mingw\bin
 
echo compiling...
%PRG_COMP_BIN_PATH%\harbour %1 -n -I%PRG_COMP_INC_PATH%
echo harbour %1 -n -I%PRG_COMP_INC_PATH%

 
echo compiling C module...
%CC%\gcc %1.c -c -I%PRG_COMP_INC_PATH%
echo %CC%\gcc %1.c -c -I%PRG_COMP_INC_PATH%

echo linking...
gcc %1.o -Wl,--start-group -lgtwin -lhbwin -lhbcommon -lhbvmmt -lhbrtl -lhbrdd -lhbmacro -lhblang -lhbcpage -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbusrrdd -lhbct -lhbdebug -lws2_32 -Wl,--allow-multiple-definition --end-group -o%1 -L%PRG_COMP_LIB_PATH% 
echo gcc %1.o -Wl,--start-group -lhbwin -lhbcommon -lhbvmmt -lhbrtl -lhbrdd -lhbmacro -lhblang -lhbcpage -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbusrrdd -lhbct -lgttrm -lhbdebug -lrt -lm -lncurses -lpcre -Wl,--allow-multiple-definition --end-group -o%1 -L%PRG_COMP_LIB_PATH% 
del %1.c
del %1.o
echo done!
