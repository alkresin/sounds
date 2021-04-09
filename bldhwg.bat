@echo off
set HRB_DIR=c:\harbour
set HRB_LIBS=hbdebug.lib hbvm.lib hbrtl.lib gtgui.lib gtwin.lib hbcpage.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbct.lib hbcplr.lib hbwin.lib hbpcre.lib hbzlib.lib minizip.lib hbmzip.lib
set HWGUI_DIR=c:\papps\hwgui_uni
set HWGUI_LIBS=hwgui.lib hbxml.lib procmisc.lib hwgdebug.lib
set PORTAUDIO_DIR=..\Portaudio
set SNDFILE_DIR=..\libsndfile
set SRC_PATH=c:\papps\gitapps\sounds\source

%HRB_DIR%\bin\harbour %HWGUI_DIR%/utils/editor/hcediext.prg -n -q -i%HRB_DIR%\include;%HWGUI_DIR%\include
%HRB_DIR%\bin\harbour %SRC_PATH%\sounds.prg %SRC_PATH%\common.prg %SRC_PATH%\htrack.prg %SRC_PATH%\hmessage.prg %SRC_PATH%\mscz.prg %SRC_PATH%\mxl.prg %SRC_PATH%\midi.prg -n -q -w -i%HRB_DIR%\include;%HWGUI_DIR%\include 2>hberr.log

echo 1 24 "c:\papps\hwgui_218\image\WindowsXP.Manifest" > hwgui_xp.rc
brc32 -r hwgui_xp -fohwgui_xp

bcc32 -c -O2 -tW -M -I%HRB_DIR%\include;%HWGUI_DIR%\include;%PORTAUDIO_DIR%;%SNDFILE_DIR%\include sounds.c common.c htrack.c hmessage.c midi.c mscz.c mxl.c hcediext.c %SRC_PATH%\hb_pa.c
ilink32 -Gn -aa -Tpe -L%HRB_DIR%\lib\win\bcc;%HWGUI_DIR%\lib c0w32.obj sounds.obj common.obj htrack.obj hmessage.obj midi.obj mscz.obj mxl.obj hcediext.obj hb_pa.obj, sounds.exe, sounds.map, %HWGUI_LIBS% %HRB_LIBS% ws2_32.lib cw32.lib import32.lib libportaudio.lib libsndfile.lib,, hwgui_xp.res

del *.obj
del sounds.c
del common.c
del htrack.c
del hmessage.c
del midi.c
del mxl.c
del mscz.c
del hcediext.c
del *.map
del hwgui_xp.rc
del *.res
del *.tds