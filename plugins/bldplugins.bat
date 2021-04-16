@echo off
set HRB_DIR=c:\harbour
set HWGUI_DIR=c:\papps\hwgui_uni
set SRC_PATH=c:\papps\gitapps\sounds\plugins

%HRB_DIR%\bin\harbour %SRC_PATH%/plug_accords.prg -n -q -w2 -gh -i%HRB_DIR%\include;%HWGUI_DIR%\include
%HRB_DIR%\bin\harbour %SRC_PATH%/plug_recorder.prg -n -q -w2 -gh -i%HRB_DIR%\include;%HWGUI_DIR%\include
%HRB_DIR%\bin\harbour %SRC_PATH%/plug_guitar.prg -n -q -w2 -gh -i%HRB_DIR%\include;%HWGUI_DIR%\include