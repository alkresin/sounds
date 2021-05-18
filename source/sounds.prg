/*
 * Sounds - musical program for beginners
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hwgui.ch"
#ifdef __PLATFORM__UNIX
   #include "gtk.ch"
#endif
#include "inkey.ch"
#include "hbclass.ch"

#define APP_VERSION  "1.3"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_RED      0x0000ff
#define CLR_LIGHTGRAY_1 0xdddddd
#define CLR_LIGHTGRAY_2 0xaaaaaa
#define CLR_DARKGRAY_1  0x333333
#define CLR_DARKGRAY_2  0x666666

#define CLR_BROWN_1  0x154780
#define CLR_BROWN_2  0x396eaa
#define CLR_BROWN_3  0x6a9cd4
#define CLR_BROWN_4  0x9dc7f6
#define CLR_BROWN_5  0xaad2ff
#define CLR_TOPDARK 0x7b7680
#define CLR_TOPMID  0x5b5760
#define CLR_DLGBACK 0x154780
#define CLR_DLGHEA  0x2F343F

#define TOPPANE_HEIGHT  28

#define MENU_VP      1001
#define MENU_TEST    1002

#define MAINWND_H1    440
#define MAINWND_H2    680

#define SCORE_Y1       60
#define SCORE_Y_DELTA 100
#define SCORE_Y_LIM   150

#define KOL_RECENT      5

REQUEST HB_CODEPAGE_UTF8
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_STRTOUTF8, HB_TRANSLATE

STATIC aOggPaths := {}, nCurrInstr := 1
STATIC cFileHis, cTestHis, aRecent := {}, lChgRecent := .F.
STATIC cLanguage := "русский", aLangs, aSuff, cSuffix := "ru", cLangNew
STATIC nZoom := 1, nZoomNew, nWndWidth, aFontsSiz := { -15, -17, -19 }, aKeySiz := { {28,20}, {32,24,}, {36,28} }
STATIC nDelayArp := 120
STATIC aSounds, nOctave := 4
STATIC cMnmFile, mnmSound, lMnm := .F., nMnmVol := 1
STATIC hFileMap

STATIC oScore, lScoreLong := .F., oPaneScore

STATIC oMainWindow, oDlgEdi := Nil, oDlgPlay := Nil
STATIC oPaneHea, oPaneTop, oPaneNote, oPaneVP, oPaneBtn, oPaneTst1, oPaneTst2, oPaneTst3, oPaneTst5, oPaneTstRes
STATIC oFontWnd, oFontHea, oFontMenu, oFontBold, oFontMetre
STATIC aStyleW, aStyleB, aStyleDisabled, aStyleBtn, aStyleLenta
STATIC oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver
STATIC oStyleWKNormal, oStyleWKPressed, oStyleWKOver, oStyleDisabled
STATIC oStyleBKNormal, oStyleBKPressed, oStyleBKOver

STATIC oPenGrid, oPenRed, oPen2, oBrushCursor, oBrushRange
STATIC oclef1, oclef2, oBemol, oDiez, oBekar
STATIC oNote1, oNote2, oNote4, oNote8, oNote16, oNote32, oNote2d, oNote4d, oNote8d, oNote16d, oNote32d
STATIC op1, op2, op4, op8, op16, op32, oDot, oTie, oTied, oArpeggio
STATIC oBmpFs1, oBmpFs2
STATIC oIns, oBmpStop, oBmpQue, oBmpInfo, oBmpAudio
STATIC handCursor

STATIC nCurrMode := 0, nTestMode, nTestLevel, nTest1, nTest2
STATIC aLevels := { 3, 13, 10, 6, 3 }, aTxtHea
STATIC aTest2Data := { 0, 0 }
STATIC aTest3Data := { {2,8}, {6,12}, {4,9}, {8,12}, {3,8}, {1,5}, {5,8}, {8,10}, {2,3}, {1,2} }
STATIC aTest4Data := { {2,8,12}, {6,8,12}, {4,6,9}, {3,5,7}, {2,4,6}, {1,2,3} }
STATIC aTests := {}, nAns1

STATIC aNotesEn := { 'C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb', 'G', 'Ab', 'A', 'Bb', 'B' }
STATIC aNotesRu := { 'До', 'До#', 'Ре', 'Ре#', 'Ми', 'Фа', 'Фа#', 'Соль', 'Соль#', 'Ля', 'Ля#', 'Си' }
STATIC aOctaves[7], aIntervals[13], aIntDir[2]
STATIC aIntShort := { "P1", "m2", "M2", "m3", "M3", "P4", "A4", "P5", "m6", "M6", "m7", "M7", "P8" }
STATIC aKeySign := { "0", "-1", "-2", "-3", "-4", "-5", "-6", "-7", "1", "2", "3", "4", "5", "6", "7" }
STATIC aMetres := { "1/2", "1/4", "2/4", "3/4", "4/4", "2/8", "3/8", "4/8", "6/8", "8/8" }
STATIC aDur := { 2048, 1024, 512, 256, 128, 64, 32 }
STATIC lStopBtn

MEMVAR oMsg, aMsgs, pClr, aPlugMenu, bPlugNote, nCurrVol, nDelayAcc

FUNCTION Main

   LOCAL oTimer
   LOCAL bPaintHea := {|o,hDC|
      HB_SYMBOL_UNUSED( o )
      hwg_Drawtransparentbitmap( hDC, oBmpAudio:handle, 8, Int( (oPaneHea:nHeight-oBmpAudio:nHeight)/2 ), CLR_WHITE )
      RETURN .T.
   }
   PUBLIC oMsg, pClr, aPlugMenu := {}, bPlugNote, nCurrVol := 1, nDelayAcc := 80
   PUBLIC aMsgs := { "Звуки музыки", "Пиано", "Тест", "Опции", "Помощь", "Выход", ;
   "Октава от", "до", "Интервал не более", "Язык", "Закрыть", "Масштаб:", ;
   "Ok", "Отмена", "Громкость", "Инструмент", "Предыдущая октвва", "Следующая октава", ;
   "Сбросить панель нот", "Проиграть тест", "Повторить", "Результат", "Ваш ответ", ;
   "Контроктава", "Большая", "Малая", "Первая", "Вторая", "Третья", "Четвертая", ;
   "Прима", "Малая секунда", "Большая секунда", "Малая терция", "Большая терция", "Кварта", "Тритон", "Квинта", "Малая секста", "Большая секста", "Малая септима", "Большая септима", "Октава", ;
   "Восходящий", "Нисходящий", "Анализ", "Показать", "Отчет", "Файл", "Новый", "Открыть", "Сохранить", ;
   "Файл уже существует. Перезаписать?", "Редактор", "Длительность", "Назад", "Вперед", ;
   "Вставить паузу", "Режим", "Интервал: восходящий или нисходящий", "Определить интервал", ;
   "Уровень", "Первая -", "назовите вторую", "Определить вторую ноту", "Вид", "Играть мелодию", ;
   "Включить метроном", "Только метроном", "Громкость мелодии", "Громкость метронома", "Темп (bpm)", ;
   "Воспроизведение", "Играть выделенное", "Импорт", "Аккорды", "Экспорт", "Транспонирование", ;
   "Вверх", "Вниз", "Выполнить", "Отменить", "Лига", "Отменить выделение", "Заменить", ;
   "Создать аккорд","О программе", "Ноты были изменены. Сохранить?", "Да", "Нет", ;
   "Изменения вступят в силу после перезагрузки программы", "Арпеджио" }

   IF hwg__isUnicode()
      hb_cdpSelect( "UTF8" )
   ENDIF

   pClr := hb_Hash( "topdark", CLR_TOPDARK, "topmid", CLR_TOPMID, ;
      "dlgback", CLR_DLGBACK, "dlghea", CLR_DLGHEA, ;
      "clr1", CLR_BROWN_1, "clr2", CLR_BROWN_2, "clr3", CLR_BROWN_3, "clr4", CLR_BROWN_4, "clr5", CLR_BROWN_5 )
   oScore := Score():New()
   oMsg := HMessage():New()
   aLangs := { cLanguage }
   aSuff := { cSuffix }
   cFileHis := hb_DirBase() + "sounds.his"
   cTestHis := hb_DirBase() + "sounds_test.his"
   IniRead()
   hwg_SetResContainer( hb_DirBase() + "sounds.bin" )
   IniPlugRead()

   SetStyles()
   aSounds := Array( 84, 4 )
   aTxtHea := { aMsgs[60], aMsgs[65], aMsgs[61], aMsgs[61], aMsgs[61] }

   pa_initialize()

   nWndWidth := 120 + (aKeySiz[nZoom,1]+2)*14
#ifdef __PLATFORM__UNIX
   INIT WINDOW oMainWindow MAIN TITLE "Sounds"  ;
      AT 200, 40 SIZE nWndWidth, MAINWND_H1 FONT oFontWnd BACKCOLOR pClr["clr5"] STYLE WND_NOTITLE
#else
   INIT WINDOW oMainWindow MAIN TITLE "Sounds"  ;
      AT 200, 40 SIZE nWndWidth, MAINWND_H1 FONT oFontWnd BACKCOLOR pClr["clr5"] STYLE WND_NOTITLE + WND_NOSIZEBOX
#endif
   ADD HEADER PANEL oPaneHea HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR 0x2F343F ;
      FONT oFontHea TEXT aMsgs[1] COORS 36 BTN_CLOSE BTN_MINIMIZE
   oPaneHea:SetPaintCB( PAINT_ITEM, bPaintHea )
   oPaneHea:SetSysbtnColor( CLR_WHITE, 0x7b7680 )

   @ 0, TOPPANE_HEIGHT PANEL oPaneTop SIZE nWndWidth, TOPPANE_HEIGHT HSTYLE oStyleDarkNormal ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   // Select mode
   @ 0, 0 OWNERBUTTON OF oPaneTop SIZE (64+nZoom*8), TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[59] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| SeleMode() }

   // File / Level
   @ 64+nZoom*8, 0 OWNERBUTTON OF oPaneTop SIZE 64+nZoom*8, TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[49] COLOR CLR_WHITE ;
      FONT oFontMenu

   // Editor / Analyse
   @ (64+nZoom*8)*2, 0 OWNERBUTTON OF oPaneTop SIZE 64+nZoom*8, TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[54] COLOR CLR_WHITE ;
      FONT oFontMenu

   @ nWndWidth-(64+nZoom*8)*3, 0 OWNERBUTTON OF oPaneTop SIZE (64+nZoom*8), TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[4] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| Options() }

   @ nWndWidth-(64+nZoom*8)*2, 0 OWNERBUTTON OF oPaneTop SIZE (64+nZoom*8), TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[5] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| FileMenu( nWndWidth-(64+nZoom*8)*2,TOPPANE_HEIGHT*2,140,58,,,,{aMsgs[5],aMsgs[87]},{{||Help()},{||About()}}) }

   @ nWndWidth-(64+nZoom*8), 0 OWNERBUTTON OF oPaneTop SIZE (64+nZoom*8), TOPPANE_HEIGHT ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[6] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| hwg_EndWindow() }

   SetMode( 1 )
   SetKeyboard()

   SET TIMER oTimer OF oMainWindow VALUE 100 ACTION {|| TimerFunc() }

   ACTIVATE WINDOW oMainWindow

   ReleaseSounds()
   pa_terminate()
   IF (!Empty( cLangNew ) .AND. !(cLangNew == cLanguage) ) .OR. ;
      (!Empty( nZoomNew ) .AND. nZoomNew != nZoom)
      IniWrite()
   ENDIF
   SaveHis()

   RETURN NIL

STATIC FUNCTION SetStyles()

   oStyleDarkNormal := HStyle():New( { pClr["topdark"], pClr["topmid"] }, 1 )
   oStyleDarkPressed := HStyle():New( { pClr["topdark"] }, 1,, 2, CLR_WHITE )
   oStyleDarkOver := HStyle():New( { pClr["topdark"] }, 1 )

   oStyleWKNormal := HStyle():New( { CLR_WHITE, CLR_LIGHTGRAY_1 }, 1,, 1 )
   oStyleWKPressed := HStyle():New( { CLR_LIGHTGRAY_1, CLR_LIGHTGRAY_2 }, 2,, 1 )
   oStyleWKOver := HStyle():New( { CLR_WHITE, CLR_LIGHTGRAY_1 }, 1,, 2, 8421440 )
   aStyleW := { oStyleWKNormal, oStyleWKPressed, oStyleWKOver }

   oStyleDisabled := HStyle():New( { CLR_LIGHTGRAY_2 }, 1,, 1 )
   aStyleDisabled := { oStyleDisabled }

   oStyleBKNormal := HStyle():New( { CLR_BLACK, CLR_DARKGRAY_1 }, 1,, 1 )
   oStyleBKPressed := HStyle():New( { CLR_DARKGRAY_1, CLR_DARKGRAY_2 }, 2,, 1 )
   oStyleBKOver := HStyle():New( { CLR_BLACK, CLR_DARKGRAY_1 }, 1,, 2, 8421440 )
   aStyleB := { oStyleBKNormal, oStyleBKPressed, oStyleBKOver }

   aStyleBtn := { HStyle():New( { pClr["clr5"], pClr["clr3"] }, 1 ), ;
      HStyle():New( { pClr["clr5"], pClr["clr3"] }, 2 ) }
   aStyleLenta := { HStyle():New( { pClr["clr3"], pClr["clr4"] }, 1 ), ;
      HStyle():New( { pClr["clr2"] }, 1,, 1, CLR_LIGHTGRAY_2 ) }

   oPenGrid := HPen():Add( PS_SOLID, 1, CLR_BLACK )
   oPenRed := HPen():Add( PS_SOLID, 2, CLR_RED )
   oPen2 := HPen():Add( PS_SOLID, 2, CLR_BLACK )
   oBrushCursor := HBrush():Add( pClr["clr5"] )
   oBrushRange := HBrush():Add( pClr["clr4"] )

   oClef1 := HBitmap():AddResource( "skrip2_80" )
   oClef2 := HBitmap():AddResource( "bas2_36" )
   oBemol := HBitmap():AddResource( "bemol_24" )
   oDiez := HBitmap():AddResource( "diez_28" )
   oBekar := HBitmap():AddResource( "bekar_28" )
   oNote1 := HBitmap():AddResource( "note1_11" )
   oNote2 := HBitmap():AddResource( "note2_11" )
   oNote4 := HBitmap():AddResource( "note4_11" )
   oNote8 := HBitmap():AddResource( "note8_11" )
   oNote16 := HBitmap():AddResource( "note16_11" )
   oNote2d := HBitmap():AddResource( "note2_d" )
   oNote4d := HBitmap():AddResource( "note4_d" )
   oNote8d := HBitmap():AddResource( "note8_d" )
   oNote16d := HBitmap():AddResource( "note16_d" )
   op1 := HBitmap():AddResource( "p1" )
   op2 := HBitmap():AddResource( "p2" )
   op4 := HBitmap():AddResource( "p3" )
   op8 := HBitmap():AddResource( "p4" )
   op16 := HBitmap():AddResource( "p5" )
   op32 := HBitmap():AddResource( "p6" )
   oDot := HBitmap():AddResource( "dot" )
   oIns := HBitmap():AddResource( "ins_16" )
   oTie := HBitmap():AddResource( "tie_32" )
   oTied := HBitmap():AddResource( "tie_32d" )
   oArpeggio := HBitmap():AddResource( "arpeggio" )
   oBmpFs1 := HBitmap():AddResource( "fs1" )
   oBmpFs2 := HBitmap():AddResource( "fs2" )
   oBmpStop := HBitmap():AddResource( "dialog-error" )
   oBmpQue := HBitmap():AddResource( "que" )
   oBmpInfo := HBitmap():AddResource( "info" )
   oBmpAudio := HBitmap():AddResource( "audio" )
#ifdef __PLATFORM__UNIX
   handCursor := hwg_Loadcursor( GDK_HAND1 )
#else
   handCursor := hwg_Loadcursor( IDC_HAND )
#endif
   oIns:cargo := .F.

   PREPARE FONT oFontWnd NAME "Times New Roman" WIDTH 0 HEIGHT aFontsSiz[nZoom] CHARSET 4
   PREPARE FONT oFontHea NAME "Georgia" WIDTH 0 HEIGHT aFontsSiz[nZoom]+2 ITALIC CHARSET 4
   PREPARE FONT oFontMenu NAME "Georgia" WIDTH 0 HEIGHT aFontsSiz[nZoom]+2 CHARSET 4
   PREPARE FONT oFontBold NAME "Georgia" WIDTH 0 HEIGHT aFontsSiz[nZoom]+2 WEIGHT 700 CHARSET 4
   PREPARE FONT oFontMetre NAME "Times New Roman" WIDTH 0 HEIGHT -22 WEIGHT 700

   oMsg:Set( oFontWnd, pClr["clr5"], pClr["dlgback"], .T., oFontHea, CLR_WHITE, pClr["dlghea"], aStyleBtn )
   oMsg:cOk := aMsgs[11]
   oMsg:cYes := aMsgs[89]
   oMsg:cNo := aMsgs[90]
   oMsg:cCancel := aMsgs[14]
   oMsg:oImgStop := oBmpStop
   oMsg:oImgQue := oBmpQue
   oMsg:oImgInfo := oBmpInfo

   RETURN NIL

STATIC FUNCTION SetKeyBoard()

#ifdef __PLATFORM__UNIX
   SET KEY 0, Asc( "q" ) TO KeyPress( 1 )
   SET KEY 0, Asc( "w" ) TO KeyPress( 2 )
   SET KEY 0, Asc( "e" ) TO KeyPress( 3 )
   SET KEY 0, Asc( "r" ) TO KeyPress( 4 )
   SET KEY 0, Asc( "t" ) TO KeyPress( 5 )
   SET KEY 0, Asc( "y" ) TO KeyPress( 6 )
   SET KEY 0, Asc( "u" ) TO KeyPress( 7 )
   SET KEY 0, Asc( "i" ) TO KeyPress( 8 )
   SET KEY 0, Asc( "o" ) TO KeyPress( 9 )
   SET KEY 0, Asc( "p" ) TO KeyPress( 10 )

   SET KEY 0, Asc( "[" ) TO KeyPress( 11 )
   SET KEY 0, Asc( "]" ) TO KeyPress( 12 )
#else
   SET KEY 0, Asc( "Q" ) TO KeyPress( 1 )
   SET KEY 0, Asc( "W" ) TO KeyPress( 2 )
   SET KEY 0, Asc( "E" ) TO KeyPress( 3 )
   SET KEY 0, Asc( "R" ) TO KeyPress( 4 )
   SET KEY 0, Asc( "T" ) TO KeyPress( 5 )
   SET KEY 0, Asc( "Y" ) TO KeyPress( 6 )
   SET KEY 0, Asc( "U" ) TO KeyPress( 7 )
   SET KEY 0, Asc( "I" ) TO KeyPress( 8 )
   SET KEY 0, Asc( "O" ) TO KeyPress( 9 )
   SET KEY 0, Asc( "P" ) TO KeyPress( 10 )

   SET KEY 0, 0xdB TO KeyPress( 11 )
   SET KEY 0, 0xdd TO KeyPress( 12 )
#endif

#ifdef __PLATFORM__UNIX
   SET KEY 0, Asc( "a" ) TO KeyPress( 13 )
   SET KEY 0, Asc( "s" ) TO KeyPress( 14 )
   SET KEY 0, Asc( "d" ) TO KeyPress( 15 )
   SET KEY 0, Asc( "f" ) TO KeyPress( 16 )
   SET KEY 0, Asc( "g" ) TO KeyPress( 17 )
   SET KEY 0, Asc( "h" ) TO KeyPress( 18 )
   SET KEY 0, Asc( "j" ) TO KeyPress( 19 )
   SET KEY 0, Asc( "k" ) TO KeyPress( 20 )
   SET KEY 0, Asc( "l" ) TO KeyPress( 21 )

   SET KEY 0, Asc( ";" ) TO KeyPress( 22 )
   SET KEY 0, Asc( "'" ) TO KeyPress( 23 )
#else
   SET KEY 0, Asc( "A" ) TO KeyPress( 13 )
   SET KEY 0, Asc( "S" ) TO KeyPress( 14 )
   SET KEY 0, Asc( "D" ) TO KeyPress( 15 )
   SET KEY 0, Asc( "F" ) TO KeyPress( 16 )
   SET KEY 0, Asc( "G" ) TO KeyPress( 17 )
   SET KEY 0, Asc( "H" ) TO KeyPress( 18 )
   SET KEY 0, Asc( "J" ) TO KeyPress( 19 )
   SET KEY 0, Asc( "K" ) TO KeyPress( 20 )
   SET KEY 0, Asc( "L" ) TO KeyPress( 21 )

   SET KEY 0, 0xba TO KeyPress( 22 )
   SET KEY 0, 0xde TO KeyPress( 23 )
#endif
   SET KEY 0, VK_RETURN TO KeyPress( 24 )

   SET KEY FSHIFT, VK_LEFT TO SeleRange( -1 )
   SET KEY FSHIFT, VK_RIGHT TO SeleRange( 1 )

   SET KEY 0, VK_BACK TO SetNotesCursor( VK_BACK )
   SET KEY 0, VK_LEFT TO SetNotesCursor( VK_LEFT )
   SET KEY 0, VK_RIGHT TO SetNotesCursor( VK_RIGHT )
   SET KEY 0, VK_UP TO SetNotesCursor( VK_UP )
   SET KEY 0, VK_DOWN TO SetNotesCursor( VK_DOWN )
   SET KEY 0, VK_HOME TO SetNotesCursor( VK_HOME )
   SET KEY 0, VK_END TO SetNotesCursor( VK_END )
   SET KEY 0, VK_INSERT TO SetNotesCursor( VK_INSERT )

   SET KEY FCONTROL, Asc("N") TO NewNotes()
   SET KEY FCONTROL, Asc("O") TO LoadNotes()
   SET KEY FCONTROL, Asc("S") TO SaveNotes()

   SET KEY FCONTROL, Asc("E") TO NoteEditor()
   SET KEY FCONTROL, Asc("P") TO Player()
   SET KEY FCONTROL, Asc("Q") TO SwitchScore()

   RETURN NIL

STATIC FUNCTION IniRead()

   LOCAL cFile := hb_DirBase() + "sounds.ini"
   LOCAL hIni := _iniRead( cFile ), aIni, nSect, aSect, cTemp, i, arr, nPos
   LOCAL cLang, crlf := Chr(13)+Chr(10), n

   IF !Empty( hIni )
      aIni := hb_HKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[ nSect ] ) == "PATHS"
            IF !Empty( aSect := hIni[ aIni[ nSect ] ] )
               hb_HCaseMatch( aSect, .F. )
               FOR i := 1 TO 9
                  IF hb_HHasKey( aSect, cTemp := "instrument_"+Ltrim(Str(i)) ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     IF Len( arr := hb_ATokens( cTemp,';' ) ) == 3
                        Aadd( aOggPaths, arr )
                        IF !(Left( arr[3],1 ) == '/') .AND. !(':' $ arr[3])
                           arr[3] := hb_DirBase() + arr[3]
                        ENDIF
                     ELSE
                        oMsg:MsgStop( cTemp, "Error in ini file" )
                     ENDIF
                  ENDIF
               NEXT
               IF hb_HHasKey( aSect, cTemp := "metronom" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cMnmFile := cTemp
                  IF !(Left( cTemp,1 ) == '/') .AND. !(':' $ cTemp)
                     cMnmFile := hb_DirBase() + cTemp
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF Upper( aIni[ nSect ] ) == "OPTIONS"
            IF !Empty( aSect := hIni[ aIni[ nSect ] ] )
               hb_HCaseMatch( aSect, .F. )
               IF hb_HHasKey( aSect, cTemp := "langs" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aLangs := hb_ATokens( cTemp, ',' )
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "suff" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aSuff := hb_ATokens( cTemp, ',' )
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "lang" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cLang := cTemp
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "zoom" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nZoom := Val( cTemp )
                  IF nZoom != 1 .AND. nZoom != 2 .AND. nZoom != 3
                     nZoom := 1
                  ENDIF
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "delayacc" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nDelayAcc := Val( cTemp )
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "delayarp" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nDelayArp := Val( cTemp )
               ENDIF
               IF hb_HHasKey( aSect, cTemp := "filemap" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  arr:= hb_aTokens( cTemp, ",", .T. )
                  hFileMap := hb_Hash()
                  FOR i := 1 TO Len(arr)
                     IF ( nPos := At( "=>", arr[i] ) ) > 0
                        hFileMap[AllTrim(Left(arr[i],nPos-1))] := AllTrim(Substr(arr[i],nPos+2))
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   IF Empty( cLang ) .OR. Ascan( aLangs, cLang ) == 0 .OR. Len( aLangs ) != Len( aSuff ) .OR. ;
      ( aSuff[n := Ascan(aLangs,cLang)] != cSuffix .AND. !File( ( hb_DirBase() + "lang" + hb_ps() + aSuff[n] + ".msg" ) ) )
      aLangs := { cLanguage }
      aSuff := { cSuffix }
   ELSE
      cLanguage := cLang
      n := Ascan( aLangs, cLang )
      cSuffix := aSuff[n]
      cTemp := hb_DirBase() + "lang" + hb_ps() + aSuff[n] + ".msg"

      cTemp := Memoread( cTemp )
      IF !(crlf $ cTemp)
         crlf := Chr( 10 )
      ENDIF
      arr := hb_aTokens( cTemp, crlf )
      FOR i := 1 TO Len( arr )
         n := Val( arr[i] )
         IF n > 0 .AND. n <= Len( aMsgs )
            aMsgs[n] := Substr( arr[i],5 )
         ENDIF
      NEXT
   ENDIF
   FOR i := 1 TO Len( aOctaves )
      aOctaves[i] := Ltrim(Str(i)) + ": " + aMsgs[23+i]
   NEXT
   FOR i := 1 TO Len( aIntervals )
      aIntervals[i] := aIntShort[i] + " " + aMsgs[30+i]
   NEXT
   aIntDir[1] := aMsgs[44]
   aIntDir[2] := aMsgs[45]

   IF !Empty( cTemp := MemoRead( cFileHis ) )
      aRecent := hb_aTokens( cTemp, Chr(10) )
      FOR i := Len( aRecent ) TO 1 STEP -1
         IF Empty( aRecent[i] )
            hb_ADel( aRecent, i, .T. )
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

STATIC FUNCTION IniWrite()

   LOCAL cFile := hb_DirBase() + "sounds.ini"
   LOCAL s := "[PATHS]" + Chr(10), i, cLangs := "", cSuff := "", arr

   FOR i := 1 TO Len( aOggPaths )
      s += "instrument_" + Ltrim(Str(i)) + '=' + aOggPaths[i,1] + ';' + ;
         aOggPaths[i,2] + ';' + aOggPaths[i,3] + Chr(10)
   NEXT
   IF !Empty( cMnmFile )
      s += "metronom=" + cMnmFile + Chr(10)
   ENDIF

   IF !Empty( cLangNew ) .AND. !(cLangNew == cLanguage)
      cLanguage := cLangNew
   ENDIF
   IF !Empty( nZoomNew ) .AND. nZoomNew != nZoom
      nZoom := nZoomNew
   ENDIF
   FOR i := 1 TO Len( aLangs )
      cLangs += Iif( i==1,"","," ) + aLangs[i]
   NEXT
   FOR i := 1 TO Len( aSuff )
      cSuff += Iif( i==1,"","," ) + aSuff[i]
   NEXT
   s += Chr(10) + "[OPTIONS]" + Chr(10) + "langs=" + cLangs + Chr(10) + ;
      "suff=" + cSuff + Chr(10) + "lang=" + cLanguage + Chr(10)
   s += "zoom=" + Ltrim(Str(nZoom)) + Chr(10)
   s += "delayacc=" + Ltrim(Str(nDelayAcc)) + Chr(10) + ;
      "delayarp=" + Ltrim(Str(nDelayArp)) + Chr(10)
   IF !Empty( hFileMap )
      arr := hb_HKeys( hFileMap )
      s += "filemap="
      FOR i := 1 TO Len( arr )
         s += Iif( i==1,"","," ) + arr[i] + "=>" + hFileMap[arr[i]]
      NEXT
      s += + Chr(10)
   ENDIF

   hb_MemoWrit( cFile, s )

   RETURN NIL

STATIC FUNCTION IniPlugRead()

   LOCAL cFile := hb_DirBase() + "plugins" + hb_ps() + "plugins.ini"
   LOCAL hIni := _iniRead( cFile ), cTemp, hPlugs, aPlugs, i, cPlug, hPlug
   STATIC aPluginHandles := {}

   IF Empty( hIni ) .OR. !hb_HHasKey( hIni, cTemp := "PLUGINS" )
      RETURN NIL
   ENDIF

   aPlugs := hb_HKeys( hPlugs := hIni[ cTemp ] )

   FOR i := 1 TO Len( aPlugs )
      IF Lower(Left( aPlugs[i],6 )) == "plugin"
         cPlug := hPlugs[aPlugs[i]]
         IF File( cFile := hb_DirBase() + "plugins" + hb_ps() + cPlug + ".hrb" )
            IF !Empty( hPlug := hb_hrbLoad( cFile ) )
               AAdd( aPluginHandles, hPlug )
               hb_hrbDo( hPlug )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN NIL

STATIC FUNCTION SaveHis()

   LOCAL i, s := Dtos(Date()) + " " + Time() + " { "

   IF Len( aTests ) > 2
      FOR i := 1 TO Len( aTests )
         s += "/"+aTests[i,1] + " " + Ltrim(Str(aTests[i,2])) + " " + ;
            Ltrim(Str(aTests[i,3])) + " " + Ltrim(Str(aTests[i,4])) + " " + ;
            Iif( Len(aTests[i])>4, Ltrim(Str(aTests[i,5]))+" ", "" )
      NEXT
      s += "}"
      hb_MemoWrit( cTestHis, MemoRead( cTestHis ) + Chr(10) + s )
   ENDIF
   IF lChgRecent
      s := ""
      FOR i := 1 TO Len( aRecent )
         s += aRecent[i] + Chr(10)
      NEXT
      hb_MemoWrit( cFileHis, s )
   ENDIF

   RETURN NIL

STATIC FUNCTION SeleMode()

   LOCAL aMenu := { {.F.,aMsgs[2]}, {.F.,aMsgs[3]+" 1"}, {.F.,aMsgs[3]+" 2"}, {.F.,aMsgs[3]+" 3"}, {.F.,aMsgs[3]+" 4"}, {.F.,aMsgs[3]+" 5"}}
   LOCAL aFuncs := { {||SetMode(1)}, {||SetMode(2)}, {||SetMode(3)}, {||SetMode(4)}, {||SetMode(5)}, {||SetMode(6)} }

   IF nCurrMode == 1
      aMenu[1,1] := .T.
   ELSE
      aMenu[nTestMode+1,1] := .T.
   ENDIF

   FileMenu( 4,TOPPANE_HEIGHT*2,140,176,,,, aMenu, aFuncs )

   RETURN NIL

STATIC FUNCTION SetMode( nMode )

   LOCAL aChoices, aFuncs, i

   nCurrMode := Iif( nMode > 1, 2, 1 )
   IF nCurrMode == 1
      aChoices := { aMsgs[50]+",Ctrl-N", aMsgs[51]+",Ctrl-O", aMsgs[52]+",Ctrl-S", aMsgs[75], aMsgs[77] }
      aFuncs := { {||NewNotes()}, {||LoadNotes()}, {||SaveNotes()}, {||ImportNotes()}, {||ExportNotes()} }
      IF !Empty( aRecent )
         FOR i := 1 TO Len( aRecent )
            AAdd( aChoices, " -- " + hb_fnameName( aRecent[i] ) )
            AAdd( aFuncs, &( "{||LoadRecent(" + Ltrim(Str(i)) + ")}" ) )
         NEXT
      ENDIF
      SetTest( .F. )
      SetVP( .T. )
      SetPaneBtn()
      oPaneTop:aControls[2]:title := aMsgs[49]
      oPaneTop:aControls[2]:bClick := {|| FileMenu( 64+nZoom*8,TOPPANE_HEIGHT*2,170,Len(aChoices)*30-6,,,,aChoices, aFuncs) }
      oPaneTop:aControls[3]:title := aMsgs[66]
      oPaneTop:aControls[3]:bClick := {|| menu_View() }
   ELSE
      nTestMode := nMode - 1
      nTestLevel := 1
      IF lScoreLong
         SwitchScore()
      ENDIF
      SetVP( .F. )
      SetTest( .T. )
      SetPaneBtn()
      oPaneTop:aControls[2]:title := aMsgs[62]   // Level
      oPaneTop:aControls[2]:bClick := {|| SeleLevel() }
      oPaneTop:aControls[3]:title := aMsgs[46]   // Analyse
      oPaneTop:aControls[3]:bClick := {|| TestAnalyse() }
   ENDIF
   hwg_Redrawwindow( oPaneTop:aControls[2]:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   hwg_Redrawwindow( oPaneTop:aControls[3]:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   RETURN NIL

STATIC FUNCTION menu_View()

   LOCAL aMenu := { {(!(oDlgEdi==Nil)),aMsgs[54]+",Ctrl-E"}, {(!(oDlgPlay==Nil)),"Player,Ctrl-P"} }
   LOCAL i

   FOR i := 1 TO Len( aPlugMenu )
      AAdd( aMenu, { .F., aPlugMenu[i,1] } )
   NEXT

   i := FileMenu( (64+nZoom*8)*2, TOPPANE_HEIGHT*2, 170, 58+Len(aPlugMenu)*28,,,, aMenu )
   IF i == 1
      NoteEditor()
   ELSEIF i == 2
      Player()
   ELSEIF i > 2
      Eval( aPlugMenu[i-2,2] )
   ENDIF

   RETURN NIL

STATIC FUNCTION SeleLevel()

   LOCAL aMenu, aFuncs, i

   IF aLevels[nTestMode] > 1
      aMenu := {}
      aFuncs := {}
      FOR i := 1 TO aLevels[nTestMode]
         AAdd( aMenu, { i==nTestLevel, aMsgs[62]+" "+Ltrim(Str(i)) } )
         AAdd( aFuncs, &( "{|| SetLevel(" + Ltrim(Str(i)) + ")}" ) )
      NEXT
      FileMenu( 64+nZoom*8,TOPPANE_HEIGHT*2,140,29*aLevels[nTestMode],,,, aMenu, aFuncs )
   ENDIF

   RETURN NIL

FUNCTION SetLevel( n )

   nTestLevel := n
   oPaneTst1:oSay1:SetText( Ltrim(Str(nTestMode)) + ". " + aTxtHea[nTestMode] + ;
      Iif( aLevels[nTestMode]>1, ", "+aMsgs[62]+" "+Ltrim(Str(n)), "" ) )

   RETURN NIL

STATIC FUNCTION SetVP( lShow )

   LOCAL oBtn1, oBtn2, oBtnI, oSayNote1, oSayOct1, oSayInstr, oTrack
   LOCAL bVolChange := {|o,n|
      IF n > 0.48 .AND. n < 0.52
         nCurrVol := 1
      ELSEIF n >= 0.52
         nCurrVol := n * 3
      ELSE
         nCurrVol := n * 2
      ENDIF
      IF o:cargo == 1 .AND. oDlgPlay != Nil
         oDlgPlay:oTrack:Value := o:Value
      ELSEIF o:cargo == 2
         oPaneBtn:oTrack:Value := o:Value
      ENDIF
      RETURN .T.
   }
   LOCAL bTrPaint := {|o,hDC|
      LOCAL y1, nHalf
      hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )
      hwg_Selectobject( hDC, o:oPen1:handle )

      nHalf := Int(o:nSize/2)
      y1 := Int(o:nHeight/2)
      IF o:nCurr - nHalf > o:nFrom
         hwg_Drawline( hDC, o:nFrom, y1, o:nCurr-nHalf, y1 )
      ENDIF
      hwg_DrawGradient( hDC, o:nCurr-nHalf+2, y1-nHalf, o:nCurr+nHalf-2, y1+nHalf, 1, {pClr["clr3"]},, { 6,6 } )
      IF o:nCurr + nHalf < o:nTo
         hwg_Selectobject( hDC, o:oPen2:handle )
         hwg_Drawline( hDC, o:nCurr+nHalf+1, y1, o:nTo, y1 )
      ENDIF
      RETURN .T.
   }
   LOCAL bInstr := {||
      LOCAL h := Len( aOggPaths ) * 30, aMenu := Array( Len( aOggPaths ) ), i
      FOR i := 1 TO Len( aOggPaths )
         aMenu[i] := aOggPaths[i,1]
      NEXT
      IF ( i := FileMenu( oPaneBtn:nLeft+oPaneBtn:oSayInstr:nLeft,oPaneBtn:nTop-h,120,h,,,, aMenu ) ) > 0 ;
         .AND. nCurrInstr != i
         nCurrInstr := i
         oPaneBtn:oSayInstr:SetText( aOggPaths[i,1] )
         ReleaseSounds()
         CheckPianoKeys()
         SetPianoKeys()
      ENDIF
      RETURN .T.
   }

   IF lShow
      IF Empty( oPaneVP )

         @ 10, TOPPANE_HEIGHT*2 + 8 PANEL oPaneNote OF oMainWindow SIZE nWndWidth - 20, 164 ;
            HSTYLE HStyle():New( { pClr["clr1"] }, 1 ) ON SIZE {|| .T. }
         oPaneNote:oFont := oFontWnd

         @ 4, 4 PANEL oPaneScore OF oPaneNote SIZE oPaneNote:nWidth-8, 156 STYLE SS_OWNERDRAW BACKCOLOR CLR_WHITE ON SIZE {||.t.}
         oPaneScore:bPaint := {|| oScore_Paint()}
         oPaneScore:bOther := {|o,msg,wp,lp| oScore_Other(o,msg,wp,lp)}
         NewNotes()

         @ (oMainWindow:nWidth-140)/2 , oPaneNote:nTop+oPaneNote:nHeight SAY oSayNote1 CAPTION "" ;
            OF oMainWindow SIZE 140, 22 FONT oFontMenu STYLE SS_CENTER BACKCOLOR pClr["clr5"]

         @ 20, oPaneNote:nTop+oPaneNote:nHeight+22 PANEL oPaneVP SIZE nWndWidth - 40, 152 OF oMainWindow ;
            HSTYLE HStyle():New( { pClr["clr1"] }, 1 ) ON SIZE {|| .T. }
         oPaneVP:oFont := oFontWnd
         AddPianoKeys( 40, 22, 50, 0 )
         AddPianoKeys( 40 + (aKeySiz[nZoom,1]+2)*7, 22, 40, 12 )
         CheckPianoKeys()
         SetPianoKeys()

         @ 10, oMainWindow:nHeight-36 PANEL oPaneBtn SIZE nWndWidth - 20, 28 OF oMainWindow ;
            HSTYLE HStyle():New( { pClr["clr1"] }, 1 ) ON SIZE {|| .T. }
         oPaneBtn:oFont := oFontWnd

         @ oPaneBtn:nWidth/2-24, 3 SAY oSayOct1 CAPTION "" OF oPaneBtn SIZE 48, 22 STYLE SS_CENTER BACKCOLOR pClr["clr1"] COLOR pClr["clr5"]
         @ oPaneBtn:nWidth/2-54, 2 OWNERBUTTON oBtn1 OF oPaneBtn SIZE 30, 24 TEXT "" ON CLICK {|| ChangeOctave( - 1 ) } ;
            TOOLTIP aMsgs[17]
         oBtn1:aStyle := aStyleBtn

         @ oPaneBtn:nWidth/2+24, 2 OWNERBUTTON oBtn2 OF oPaneBtn SIZE 30, 24 TEXT "" ON CLICK {|| ChangeOctave( 1 ) } ;
            TOOLTIP aMsgs[18]
         oBtn2:aStyle := aStyleBtn

         @ oPaneBtn:nWidth/2-60, 2 SAY "" OF oPaneBtn SIZE 2, oPaneBtn:nHeight-4
         @ oPaneBtn:nWidth/2+60, 2 SAY "" OF oPaneBtn SIZE 2, oPaneBtn:nHeight-4

         @ 30, 2 BITMAP "volume" FROM RESOURCE OF oPaneBtn //TRANSPARENT COLOR CLR_WHITE
         oTrack := HTrack():New( oPaneBtn,, 54, 0, 112, 28,,, CLR_WHITE, pClr["clr1"], 16 )
         hwg_SetCtrlName( oTrack, "OTRACK" )
         hwg_Addtooltip( oTrack:handle, aMsgs[70] )
         oTrack:cargo := 1
         oTrack:tColor2 := CLR_LIGHTGRAY_1
         oTrack:bChange := bVolChange
         oTrack:bPaint := bTrPaint
         oTrack:Value := 0.5

         @ oPaneBtn:nWidth/2+100, 2 SAY oSayInstr CAPTION aOggPaths[nCurrInstr,1] OF oPaneBtn SIZE 120, 24 ;
            FONT oFontMenu STYLE SS_CENTER BACKCOLOR pClr["clr5"]
         @ oPaneBtn:nWidth/2+220, 2 OWNERBUTTON oBtnI OF oPaneBtn SIZE 20, 24 TEXT ".." ON CLICK bInstr
         oBtnI:aStyle := aStyleBtn

         ChangeOctave( 0 )
      ELSE
         ShowAll( oPaneNote, .T. )
         ShowAll( oPaneVP, .T. )
      ENDIF
   ELSEIF !Empty( oPaneVP )
      ShowAll( oPaneNote, .F. )
      ShowAll( oPaneVP, .F. )
   ENDIF

   RETURN NIL

STATIC FUNCTION AddPianoKeys( x1, y1, yt, nTone )

   LOCAL oBtn

   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "C" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 1 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+1

   @ x1 + 18, y1 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,2], 40 TEXT "" ;
      ON CLICK {|| PlayKey( nTone + 2 ) }
   oBtn:aStyle := aStyleB; oBtn:cargo := nTone+2

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "D" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 3 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+3

   @ x1 + 18, y1 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,2], 40 TEXT "" ;
      ON CLICK {|| PlayKey( nTone + 4 ) }
   oBtn:aStyle := aStyleB; oBtn:cargo := nTone+4

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "E" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 5 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+5

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "F" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 6 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+6

   @ x1 + 18, y1 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,2], 40 TEXT "" ;
      ON CLICK {|| PlayKey( nTone + 7 ) }
   oBtn:aStyle := aStyleB; oBtn:cargo := nTone+7

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "G" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 8 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+8

   @ x1 + 18, y1 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,2], 40 TEXT "" ;
      ON CLICK {|| PlayKey( nTone + 9 ) }
   oBtn:aStyle := aStyleB; oBtn:cargo := nTone+9

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "A" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 10 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+10

   @ x1 + 18, y1 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,2], 40 TEXT "" ;
      ON CLICK {|| PlayKey( nTone + 11 ) }
   oBtn:aStyle := aStyleB; oBtn:cargo := nTone+11

   x1 += aKeySiz[nZoom,1] + 2
   @ x1, y1 + 40 OWNERBUTTON oBtn OF oPaneVP SIZE aKeySiz[nZoom,1], 80 TEXT "B" ;
      COORDINATES Nil,yt,Nil,Nil ON CLICK {|| PlayKey( nTone + 12 ) }
   oBtn:aStyle := aStyleW; oBtn:cargo := nTone+12

   RETURN NIL

STATIC FUNCTION SetPianoKeys()

   LOCAL aCtrl := oPaneVP:aControls, i, n

   FOR i := 1 TO Len( aCtrl )
      IF Valtype( aCtrl[i]:cargo ) == "N"
         n := aCtrl[i]:cargo + ( nOctave - 1 ) * 12
         IF aSounds[n,3]
            IF !aCtrl[i]:Enabled()
               aCtrl[i]:aStyle := Iif( aCtrl[i]:nTop>50, aStyleW, aStyleB )
               aCtrl[i]:Enable()
            ENDIF
         ELSE
            IF aCtrl[i]:Enabled()
               aCtrl[i]:aStyle := aStyleDisabled
               aCtrl[i]:Disable()
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN NIL

STATIC FUNCTION CheckPianoKeys()

   LOCAL i, cFile

   FOR i := 1 TO Len( aSounds )
      cFile := aOggPaths[nCurrInstr,2] + Note2Text( i )
      IF !Empty( hFileMap ) .AND. hb_HHasKey( hFileMap, cFile )
         cFile := hFileMap[cFile]
      ENDIF
      aSounds[i,3] := File( aOggPaths[nCurrInstr,3] + cFile + ".ogg" )
   NEXT

   RETURN NIL

STATIC FUNCTION ChangeOctave( n )

   nOctave += n
   IF nOctave == 0
      nOctave := 1
   ELSEIF nOctave == 7
      nOctave := 6
   ENDIF

   IF nOctave == 1
      oPaneBtn:oBtn1:Title := ""
   ELSEIF nOctave == 6
      oPaneBtn:oBtn2:Title := ""
   ELSE
      oPaneBtn:oBtn1:Title := "<"
      oPaneBtn:oBtn2:Title := ">"
   ENDIF

   oPaneBtn:oSayOct1:SetText( LTrim(Str(nOctave)) + ' - ' + LTrim(Str(nOctave+1)) )
   hwg_Invalidaterect( oPaneBtn:oBtn1:handle, 0 )
   hwg_Invalidaterect( oPaneBtn:oBtn2:handle, 0 )
   SetPianoKeys()

   RETURN NIL

STATIC FUNCTION SetTest( lShow )

   LOCAL oSay1, oSay2, oGet4, oGet5, oTestVal1, oTestVal2, oTestVal3, oTestTotal
   LOCAL oTestNote, oTestBmp2, oTestBmp3, oBtnPlay, oBtnResu, oBtnResu1, oBtnResu2, oBtnResu3, oBtnResu4, oBtnResu5
   LOCAL nIntDir := 1, nInterval := 1, nw

   IF lShow
      IF Empty( oPaneTst1 )
         @ 10, TOPPANE_HEIGHT*2 + 8 PANEL oPaneTst1 OF oMainWindow SIZE nWndWidth - 20, 114 ;
            HSTYLE HStyle():New( { pClr["clr2"] }, 1 ) ON SIZE {|| .T. }
         oPaneTst1:oFont := oFontWnd

         @ 20, 4 SAY oSay1 CAPTION "" OF oPaneTst1 SIZE nWndWidth - 60, 20+nZoom*2 FONT oFontBold COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

         @ 20, 64 SAY "" of oPaneTst1 SIZE oPaneTst1:nWidth-40, 2

         @ 120, 74 OWNERBUTTON oBtnPlay OF oPaneTst1 SIZE 40, 32 ;
            BITMAP "play_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
            ON CLICK {|| Test_Interval() } TOOLTIP aMsgs[20]
         ATail( oPaneTst1:aControls ):aStyle := aStyleBtn

         @ 180, 74 OWNERBUTTON OF oPaneTst1 SIZE 40, 32 ;
            BITMAP "repeat_20" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
            ON CLICK {|| Test_Interval( nTest1, nTest2 ) } TOOLTIP aMsgs[21]
         ATail( oPaneTst1:aControls ):aStyle := aStyleBtn

         // ---- oPaneTst2 Begin ----
         @ 10, oPaneTst1:nTop+oPaneTst1:nHeight+2 PANEL oPaneTst2 OF oMainWindow SIZE nWndWidth - 20, 80 ;
            HSTYLE HStyle():New( { pClr["clr2"] }, 1 ) ON SIZE {|| .T. }
         oPaneTst2:oFont := oFontWnd

         @ 20, 4 SAY oSay2 CAPTION "" OF oPaneTst2 SIZE oPaneTst2:nWidth - 40, 24 FONT oFontWnd COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

         @ (oPaneTst2:nWidth-380)/2, 34 OWNERBUTTON oBtnResu1 OF oPaneTst2 SIZE 180, 36 TEXT "" ;
            ON CLICK {|| nAns1 := 1, TestVal() }
         oBtnResu1:aStyle := aStyleBtn
         @ (oPaneTst2:nWidth-380)/2+200, 34 OWNERBUTTON oBtnResu2 OF oPaneTst2 SIZE 180, 36 ;
            TEXT "" ON CLICK {|| nAns1 := 2, TestVal() }
         oBtnResu2:aStyle := aStyleBtn
         // ---- oPaneTst2 End ----

         // ---- oPaneTst3 Begin ----
         @ 10, oPaneTst1:nTop+oPaneTst1:nHeight+2 PANEL oPaneTst3 OF oMainWindow SIZE nWndWidth - 20, 80 ;
            HSTYLE HStyle():New( { pClr["clr2"] }, 1 ) ON SIZE {|| .T. }
         oPaneTst3:oFont := oFontWnd

         nw := Int( (oPaneTst1:nWidth - 40) / 3 )

         @ 10, 34 OWNERBUTTON oBtnResu3 OF oPaneTst3 SIZE nw, 36 TEXT "" FONT oFontMenu ;
            ON CLICK {|| nAns1 := 1, TestVal() }
         oBtnResu3:aStyle := aStyleBtn
         @ nw+20, 34 OWNERBUTTON oBtnResu4 OF oPaneTst3 SIZE nw, 36 TEXT "" FONT oFontMenu ;
            ON CLICK {|| nAns1 := 2, TestVal() }
         oBtnResu4:aStyle := aStyleBtn
         @ nw*2+30, 34 OWNERBUTTON oBtnResu5 OF oPaneTst3 SIZE nw, 36 TEXT "" FONT oFontMenu ;
            ON CLICK {|| nAns1 := 3, TestVal() }
         oBtnResu5:aStyle := aStyleBtn
         // ---- oPaneTst3 End ----

         // ---- oPaneTst5 Begin ----
         @ 10, oPaneTst1:nTop+oPaneTst1:nHeight+2 PANEL oPaneTst5 OF oMainWindow SIZE nWndWidth - 20, 80 ;
            HSTYLE HStyle():New( { pClr["clr2"] }, 1 ) ON SIZE {|| .T. }
         oPaneTst5:oFont := oFontWnd

         @ 20, 10 SAY aMsgs[23] OF oPaneTst5 SIZE 110, 20+nZoom*2 FONT oFontWnd COLOR CLR_WHITE BACKCOLOR pClr["clr2"]
         @ 130,10 GET COMBOBOX oGet4 VAR nIntDir ITEMS aIntDir OF oPaneTst5 SIZE 120, 28
         @ 260,10 GET COMBOBOX oGet5 VAR nInterval ITEMS aIntervals OF oPaneTst5 SIZE 180, 28

         @ oPaneTst5:nWidth-80, 8 OWNERBUTTON oBtnResu OF oPaneTst5 SIZE 60, 36 ;
            BITMAP "compare" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
            ON CLICK {|| TestVal() } TOOLTIP aMsgs[22]
         oBtnResu:aStyle := aStyleBtn
         // ---- oPaneTst5 End ----

         // ---- oPaneTstRes Begin ----
         @ 10, oPaneTst2:nTop+oPaneTst2:nHeight+2 PANEL oPaneTstRes OF oMainWindow SIZE nWndWidth - 20, 140 ;
            HSTYLE HStyle():New( { pClr["clr2"] }, 1 ) ON SIZE {|| .T. }
         oPaneTstRes:oFont := oFontWnd

         @ 20, 10 SAY oTestVal1 CAPTION "" OF oPaneTstRes SIZE 280, 20+nZoom*2 BACKCOLOR pClr["clr5"]
         @ 20, 36 SAY oTestVal2 CAPTION "" OF oPaneTstRes SIZE 220, 20+nZoom*2 BACKCOLOR pClr["clr5"]
         @ 244, 36 BITMAP oTestBmp2 SHOW "dummy" FROM RESOURCE OF oPaneTstRes TRANSPARENT COLOR CLR_WHITE BACKCOLOR pClr["clr2"]
         @ 20, 62 SAY oTestVal3 CAPTION "" OF oPaneTstRes SIZE 220, 20+nZoom*2 BACKCOLOR pClr["clr5"]
         @ 244, 62 BITMAP oTestBmp3 SHOW "dummy" FROM RESOURCE OF oPaneTstRes TRANSPARENT COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

         @ oPaneTstRes:nWidth-200, 10 SAY oTestNote CAPTION "" OF oPaneTstRes SIZE 180, 122 STYLE SS_OWNERDRAW BACKCOLOR CLR_WHITE
         oTestNote:bPaint := {|o,lpdis| oTest_Paint( o,lpdis )}
         oTestNote:cargo := Score():New()
         oTestNote:cargo:aMetre[1] := oTestNote:cargo:aMetre[2] := 0
         oTestNote:cargo:SetScrKol( oTestNote )

         @ 20, 104 SAY oTestTotal CAPTION "" OF oPaneTstRes SIZE 200, 22+nZoom*2 FONT oFontBold BACKCOLOR pClr["clr3"]
         // ---- oPaneTstRes End ----

      ENDIF
      ShowAll( oPaneTst1, .T. )
      ShowAll( oPaneTstRes, .T. )
      PaneResClean()
      IF nTestMode == 1 .OR. nTestMode == 2 .OR. nTestMode == 3
         ShowAll( oPaneTst3, .F. )
         ShowAll( oPaneTst5, .F. )
         ShowAll( oPaneTst2, .T. )
         IF nTestMode == 1
            oPaneTst2:oBtnResu1:title := aMsgs[44]
            oPaneTst2:oBtnResu2:title := aMsgs[45]
         ELSE
            oPaneTst2:oBtnResu1:title := ""
            oPaneTst2:oBtnResu2:title := ""
         ENDIF
         oPaneTst2:oBtnResu1:Disable()
         oPaneTst2:oBtnResu2:Disable()
      ELSEIF nTestMode == 4
         ShowAll( oPaneTst2, .F. )
         ShowAll( oPaneTst5, .F. )
         ShowAll( oPaneTst3, .T. )
         oPaneTst3:oBtnResu3:title := ""
         oPaneTst3:oBtnResu4:title := ""
         oPaneTst3:oBtnResu5:title := ""
         oPaneTst3:oBtnResu3:Disable()
         oPaneTst3:oBtnResu4:Disable()
         oPaneTst3:oBtnResu5:Disable()
      ELSE
         ShowAll( oPaneTst2, .F. )
         ShowAll( oPaneTst3, .F. )
         ShowAll( oPaneTst5, .T. )
         oPaneTst5:oGet4:Disable()
         oPaneTst5:oGet5:Disable()
         oPaneTst5:oBtnResu:Disable()
      ENDIF
      SetLevel( nTestLevel )

   ELSEIF !Empty( oPaneTst1 )
      ShowAll( oPaneTst1, .F. )
      ShowAll( oPaneTst2, .F. )
      ShowAll( oPaneTst3, .F. )
      ShowAll( oPaneTst5, .F. )
      ShowAll( oPaneTstRes, .F. )
   ENDIF

   RETURN NIL

STATIC FUNCTION Test_Interval( n1, n2 )

   LOCAL i, j, lRepeat := .T., nSec
   LOCAL nFrom, nTo, nInterv, n2a, a

   IF Empty( n1 )
      IF PCount() > 0
         RETURN NIL
      ENDIF

      lRepeat := .F.
      IF nTestMode == 1
         IF nTestLevel <= 2
            nFrom := 25
            nTo := 48
         ELSE
            nFrom := 20
            nTo := 60
         ENDIF
      ELSEIF nTestMode == 2
         nFrom := 25
         nTo := 36
      ELSEIF nTestMode == 3
         IF nTestLevel <= 4
            nFrom := 25
            nTo := 48
         ELSE
            nFrom := 20
            nTo := 60
         ENDIF
      ELSEIF nTestMode == 4
         IF nTestLevel <= 2
            nFrom := 25
            nTo := 48
         ELSE
            nFrom := 20
            nTo := 60
         ENDIF
      ELSEIF nTestMode == 5
         IF nTestLevel == 1
            nFrom := 25
            nTo := 48
            nInterv := 2.5
         ELSE
            nFrom := 20
            nTo := 60
            nInterv := Iif( nTestLevel == 1, 2.5, 6 )
         ENDIF
      ENDIF
      n1 := 0
      DO WHILE ++n1 <= Len(aSounds) .AND. !aSounds[n1,3]; ENDDO
      n2 := Len( aSounds ) + 1
      DO WHILE --n2 >= 1 .AND. !aSounds[n2,3]; ENDDO
      nFrom := Max( n1, nFrom ); nTo := Min( n2, nTo )

      i := 20
      DO WHILE --i > 0
         IF nTestMode == 1
            n1 := hb_RandomInt( nFrom+2, nTo - 2 )
            IF nTestLevel == 1
               n2 := Iif( hb_RandomInt(1,2)==1, n1-5, n1+5 )
            ELSEIF nTestLevel == 2
               n2 := Min( n1 - Max( n1 - 4, nFrom ), Min( n1 + 4, nTo ) - n1 )
               n2 := hb_RandomInt( 1, n2 )
               n2 := Iif( hb_RandomInt(1,2)==1, n1-n2, n1+n2 )
            ELSEIF nTestLevel == 3
               n2 := Min( n1 - Max( n1 - 2, nFrom ), Min( n1 + 2, nTo ) - n1 )
               n2 := hb_RandomInt( 1, n2 )
               n2 := Iif( hb_RandomInt(1,2)==1, n1-n2, n1+n2 )
            ENDIF
         ELSEIF nTestMode == 2
            IF nTestLevel < 4
               IF nTestLevel == 1
                  n1 := 25
               ELSEIF nTestLevel == 2
                  n1 := Iif( (n2:=hb_RandomInt(1,3))==1, 25, Iif( n2==2, 27, 29 ) )
               ELSE
                  n1 := Iif( (n2:=hb_RandomInt(1,4))==1, 25, Iif( n2==2, 27, Iif( n2==3, 29, 30 ) ) )
               ENDIF
               n2 := hb_RandomInt( n1+1, Iif( nTestLevel < 3, n1+4, n1+5 ) )
               a := {}
               FOR j := n1+1 TO Iif( nTestLevel < 3, n1+4, n1+5 )
                  IF j != n2
                     AAdd( a, j )
                  ENDIF
               NEXT
               j := hb_RandomInt( 1, Iif( nTestLevel < 3, 3, 4 ) )
               n2a := a[j]
            ELSE
               n1 := hb_RandomInt( nFrom, nTo - 12 )
               n2 := n1 + Iif( hb_RandomInt(1,2)==1, aTest3Data[nTestLevel-3,1], aTest3Data[nTestLevel-3,2] )
            ENDIF
         ELSEIF nTestMode == 3
            n1 := hb_RandomInt( nFrom, nTo - 12 )
            n2 := n1 + Iif( hb_RandomInt(1,2)==1, aTest3Data[nTestLevel,1], aTest3Data[nTestLevel,2] )
         ELSEIF nTestMode == 4
            n1 := hb_RandomInt( nFrom, nTo - 12 )
            n2 := n1 + Iif( (j:=hb_RandomInt(1,3))==1, aTest4Data[nTestLevel,1], ;
               Iif( j==2, aTest4Data[nTestLevel,2], aTest4Data[nTestLevel,3] ) )
         ELSE
            n1 := hb_RandomInt( nFrom, nTo - 2 )
            j := 20
            DO WHILE --j > 0
               n2 := hb_RandomInt( Max( n1 - nInterv * 2, nFrom ), Min( n1 + nInterv * 2, nTo ) )
               IF n2 != n1
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF aSounds[n1,3] .AND. aSounds[n2,3]
            EXIT
         ENDIF
      ENDDO

      PaneResClean()
      IF nTestMode == 1 .OR. nTestMode == 2 .OR. nTestMode == 3
         oPaneTst2:oBtnResu1:Disable()
         oPaneTst2:oBtnResu2:Disable()
      ELSEIF nTestMode == 4
         oPaneTst3:oBtnResu3:Disable()
         oPaneTst3:oBtnResu4:Disable()
         oPaneTst3:oBtnResu5:Disable()
      ELSE
         oPaneTst5:oGet4:Disable()
         oPaneTst5:oGet5:Disable()
         oPaneTst5:oBtnResu:Disable()
      ENDIF
   ENDIF

   IF !LoadNote( n1 ) .OR. !LoadNote( n2 )
      RETURN NIL
   ENDIF
   nTest1 := n1
   nTest2 := n2
   Play( n1 )
   nSec := Seconds()
   DO WHILE Seconds() - nSec < 1
      hwg_ProcessMessage()
      hb_gcStep()
      hwg_Sleep( 1 )
   ENDDO

   sf_ChangeData( aSounds[n1,1], aSounds[ n2, 1 ] )
   hwg_Sleep( 1000 )
   StopSound( n1 )

   IF !lRepeat
      IF nTestMode == 1 .OR. nTestMode == 2  .OR. nTestMode == 3
         IF nTestMode == 2
            oPaneTst2:oSay2:SetText( aMsgs[63] + " " + note2Text( nTest1,.T. ) + ", " + aMsgs[64] )
            IF nTestLevel < 4
               j := hb_RandomInt( 1,2 )
               oPaneTst2:oBtnResu1:title := note2Text( aTest2Data[1] := Iif( j==1, n2, n2a ),.T. )
               oPaneTst2:oBtnResu2:title := note2Text( aTest2Data[2] := Iif( j==1, n2a, n2 ),.T. )
            ELSE
               oPaneTst2:oBtnResu1:title := note2Text( nTest1+aTest3Data[nTestLevel-3,1],.T. )
               oPaneTst2:oBtnResu2:title := note2Text( nTest1+aTest3Data[nTestLevel-3,2],.T. )
            ENDIF
         ELSEIF nTestMode == 3
            oPaneTst2:oBtnResu1:title := aIntervals[aTest3Data[nTestLevel,1]+1]
            oPaneTst2:oBtnResu2:title := aIntervals[aTest3Data[nTestLevel,2]+1]
         ENDIF
         oPaneTst2:oBtnResu1:Enable()
         oPaneTst2:oBtnResu2:Enable()
      ELSEIF nTestMode == 4
         oPaneTst3:oBtnResu3:title := aIntervals[aTest4Data[nTestLevel,1]+1]
         oPaneTst3:oBtnResu4:title := aIntervals[aTest4Data[nTestLevel,2]+1]
         oPaneTst3:oBtnResu5:title := aIntervals[aTest4Data[nTestLevel,3]+1]
         oPaneTst3:oBtnResu3:Enable()
         oPaneTst3:oBtnResu4:Enable()
         oPaneTst3:oBtnResu5:Enable()
      ELSE
         oPaneTst5:oGet4:Enable()
         oPaneTst5:oGet5:Enable()
         oPaneTst5:oBtnResu:Enable()
      ENDIF
      oPaneTst1:oBtnPlay:Disable()
   ENDIF

   RETURN NIL

STATIC FUNCTION PaneResClean()

   oPaneTstRes:oTestVal1:SetText( "" )
   oPaneTstRes:oTestVal2:SetText( "" )
   oPaneTstRes:oTestVal3:SetText( "" )
   oPaneTstRes:oTestBmp2:ReplaceBitmap( "dummy.bmp", .T. )
   oPaneTstRes:oTestBmp3:ReplaceBitmap( "dummy.bmp", .T. )
   oPaneTstRes:oTestBmp2:Refresh()
   oPaneTstRes:oTestBmp3:Refresh()
   oPaneTstRes:oTestNote:cargo:Clean()
   oPaneTstRes:oTestNote:cargo:SetScrKol( oPaneTstRes:oTestNote )
   oPaneTstRes:oTestNote:cargo:aMetre[1] := oPaneTstRes:oTestNote:cargo:aMetre[2] := 0
   hwg_Redrawwindow( oPaneTstRes:oTestNote:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   RETURN NIL

STATIC FUNCTION TestVal()

   LOCAL n1, n2, s
   LOCAL nCorrect, nCorrect2, nTotal

   IF Empty( nTest1 )
      RETURN NIL
   ENDIF

   n1 := Iif( nTest2 > nTest1, 1, 2 )
   n2 := Abs( nTest2-nTest1 ) + 1
   oPaneTstRes:oTestVal1:SetText( Note2Text( nTest1,.T. ) + " - " + Note2Text( nTest2,.T. ) )
   oPaneTstRes:oTestVal2:SetText( aIntDir[n1] )
   oPaneTstRes:oTestVal3:SetText( aIntervals[n2] )

   IF nTestMode == 1 .OR. nTestMode == 2 .OR. nTestMode == 3
      oPaneTst2:oSay2:SetText( "" )
      oPaneTst2:oBtnResu1:Disable()
      oPaneTst2:oBtnResu2:Disable()
      IF nTestMode == 1
         oPaneTstRes:oTestBmp2:ReplaceBitmap( Iif( nAns1 == n1, "ok", "cancel" ), .T. )
      ELSEIF nTestMode == 2
         IF nTestLevel < 4
            oPaneTstRes:oTestBmp3:ReplaceBitmap( ;
               Iif( aTest2Data[nAns1] == nTest2, "ok", "cancel" ), .T. )
         ELSE
            oPaneTstRes:oTestBmp3:ReplaceBitmap( ;
               Iif( aTest3Data[nTestLevel-3,nAns1] == nTest2-nTest1, "ok", "cancel" ), .T. )
         ENDIF
      ELSE
         oPaneTstRes:oTestBmp3:ReplaceBitmap( ;
            Iif( aTest3Data[nTestLevel,nAns1] == nTest2-nTest1, "ok", "cancel" ), .T. )
      ENDIF
   ELSEIF nTestMode == 4
      oPaneTst3:oBtnResu3:Disable()
      oPaneTst3:oBtnResu4:Disable()
      oPaneTst3:oBtnResu5:Disable()
      oPaneTstRes:oTestBmp3:ReplaceBitmap( ;
         Iif( aTest4Data[nTestLevel,nAns1] == nTest2-nTest1, "ok", "cancel" ), .T. )
   ELSE
      nAns1 := oPaneTst5:oGet4:Value
      oPaneTstRes:oTestBmp2:ReplaceBitmap( Iif( nAns1 == n1, "ok", "cancel" ), .T. )
      oPaneTstRes:oTestBmp3:ReplaceBitmap( Iif( oPaneTst5:oGet5:Value == n2, "ok", "cancel" ), .T. )
      oPaneTst5:oGet4:Disable()
      oPaneTst5:oGet5:Disable()
      oPaneTst5:oBtnResu:Disable()
   ENDIF

   oPaneTst1:oBtnPlay:Enable()
   oPaneTstRes:oTestBmp2:Refresh()
   oPaneTstRes:oTestBmp3:Refresh()
   oPaneTstRes:oTestNote:cargo:aNotes := { {nTest1,1}, {nTest2,1} }
   oPaneTstRes:oTestNote:cargo:CheckBas()
   hwg_Redrawwindow( oPaneTstRes:oTestNote:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   s := "t" + Ltrim(Str(nTestMode)) + ;
      "l" + Ltrim(Str(nTestLevel)) + "i" + aOggPaths[nCurrInstr,2]
   IF nTestMode == 1
      Aadd( aTests, { s, nTest1, nTest2, ;
         Iif( (nTest2>nTest1.AND.nAns1==1).OR.(nTest2<nTest1.AND.nAns1==2), 1, 0 ) } )
   ELSEIF nTestMode == 2
      IF nTestLevel < 4
         Aadd( aTests, { s, nTest1, nTest2, ;
            Iif( nTest2==aTest2Data[nAns1], 1, 0 ) } )
      ELSE
         Aadd( aTests, { s, nTest1, nTest2, ;
            Iif( nTest2-nTest1==aTest3Data[nTestLevel-3,nAns1], 1, 0 ) } )
      ENDIF
   ELSEIF nTestMode == 3
      Aadd( aTests, { s, nTest1, nTest2, ;
         Iif( nTest2-nTest1==aTest3Data[nTestLevel,nAns1], 1, 0 ) } )
   ELSEIF nTestMode == 4
      Aadd( aTests, { s, nTest1, nTest2, ;
         Iif( nTest2-nTest1==aTest4Data[nTestLevel,nAns1], 1, 0 ) } )
   ELSE
      Aadd( aTests, { s, nTest1, nTest2, ;
         Iif( (nTest2>nTest1.AND.nAns1==1).OR.(nTest2<nTest1.AND.nAns1==2), 1, 0 ), ;
         Iif( oPaneTst5:oGet5:Value == n2, 1, 0 ) } )
   ENDIF

   TestCalc( aTests, s, @nCorrect, @nCorrect2, @nTotal )
   oPaneTstRes:oTestTotal:SetText( Ltrim(Str(nCorrect)) + '/' + Ltrim(Str(nTotal)) + ;
      "  (" + Ltrim(Str(Int(nCorrect*100/nTotal))) + "%)" )

   RETURN NIL

STATIC FUNCTION TestCalc( arr, s, nCorrect, nCorrect2, nTotal )

   LOCAL i, nLen := Len ( s ), aRes := {}, n

   nCorrect := nCorrect2 := nTotal := 0
   FOR i := 1 TO Len( arr )
      IF nLen == 0 .OR. ( nLen == Len(arr[i,1]) .AND. arr[i,1] == s ) .OR. ;
         Left(arr[i,1],nLen) == s
         nTotal ++
         IF arr[i,4] == 1
            nCorrect ++
         ENDIF
         IF Len( arr[i] ) > 4 .AND. arr[i,5] == 1
            nCorrect2 ++
         ENDIF
         IF nLen < Len( arr[i,1] )
            IF ( n := Ascan( aRes, {|a|a[1]==arr[i,1]} ) ) == 0
               Aadd( aRes, { arr[i,1], 0, 0, 0 } )
               n := Len( aRes )
            ENDIF
            aRes[n,2] ++
            IF arr[i,4] == 1
               aRes[n,3] ++
            ENDIF
            IF Len( arr[i] ) > 4 .AND. arr[i,5] == 1
               aRes[n,4] ++
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN Iif( Empty( aRes ), NIL, aRes )

STATIC FUNCTION SeleRange( n )

   IF oScore:nCurr == 0 .OR. oScore:nCurr > Len( oScore:aNotes ) .OR. (n < 0 .AND. oScore:nCurr == 1) ;
      .OR. ( n > 0 .AND. oScore:nCurr == Len( oScore:aNotes ) )
      RETURN NIL
   ENDIF
   IF n < 0
      IF oScore:nSeleEnd == 0 .OR. oScore:nCurr != oScore:nSeleStart
         oScore:nSeleStart := oScore:nSeleEnd := oScore:nCurr
      ELSE
         oScore:nSeleStart --
         //oScore:nCurr --
         SetNotesCursor( VK_LEFT )
      ENDIF
   ELSE
      IF oScore:nSeleStart == 0 .OR. oScore:nCurr != oScore:nSeleEnd
         oScore:nSeleStart := oScore:nSeleEnd := oScore:nCurr
      ELSE
         oScore:nSeleEnd ++
         //oScore:nCurr ++
         SetNotesCursor( VK_RIGHT )
      ENDIF
   ENDIF
   SetDlgEdi()
   hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   RETURN NIL

STATIC FUNCTION SetNotesCursor( nKey )

   IF nCurrMode == 2
      RETURN NIL
   ENDIF

   IF nKey == VK_LEFT
      IF oScore:nCurr > 1
         oScore:nCurr --
         IF lScoreLong
            IF oScore:nCurr < oScore:nScrStart
               ScorePage( -1, .T. )
            ENDIF
         ELSEIF oScore:nScrStart > oScore:nCurr
            oScore:nScrStart --
         ENDIF
      ENDIF
   ELSEIF nKey == VK_RIGHT
      IF oScore:nCurr <= Len(oScore:aNotes)
         oScore:nCurr ++
         IF lScoreLong
            IF oScore:nScrStart + Iif( oScore:nScrStart == 1, oScore:nScrKol1, oScore:nScrKol2 ) + ;
               oScore:nScrKol2*(oScore:nLines-1) <= oScore:nCurr
               ScorePage( 1, .T. )
            ENDIF
         ELSEIF oScore:nCurr >= oScore:nScrStart + oScore:nScrKol1
            oScore:nScrStart += 3
         ENDIF
      ENDIF
   ELSEIF nKey == VK_UP
      IF oScore:nCurr - oScore:nScrKol2 > 0
         oScore:nCurr -= oScore:nScrKol2
         IF lScoreLong
            IF oScore:nCurr < oScore:nScrStart
               ScorePage( -1, .T. )
            ENDIF
         ELSE
            oScore:nScrStart -= oScore:nScrKol2
         ENDIF
      ELSE
         oScore:nCurr := oScore:nScrStart := oScore:nCurrPage := 1
      ENDIF
   ELSEIF nKey == VK_DOWN
      IF oScore:nCurr + oScore:nScrKol2 <= Len(oScore:aNotes)
         oScore:nCurr += oScore:nScrKol2
         IF lScoreLong
            IF oScore:nScrStart + Iif( oScore:nScrStart == 1, oScore:nScrKol1, oScore:nScrKol2 ) + ;
               oScore:nScrKol2*(oScore:nLines-1) <= oScore:nCurr
               ScorePage( 1, .T. )
            ENDIF
         ELSE
            oScore:nScrStart += oScore:nScrKol2
         ENDIF
      ELSE
         IF !lScoreLong
            oScore:nCurr := Len( oScore:aNotes ) + 1
            IF oScore:nCurr >= oScore:nScrStart + oScore:nScrKol1
               oScore:nScrStart := oScore:nCurr - oScore:nScrKol1 + 1
            ENDIF
         ENDIF
      ENDIF
   ELSEIF nKey == VK_HOME
      oScore:nCurr := oScore:nScrStart := oScore:nCurrPage := 1
      IF lScoreLong
         oPaneBtn:oBtnPrevPage:Disable()
      ENDIF
   ELSEIF nKey == VK_END
      IF lScoreLong
         DO WHILE ScorePage( 1, .T. )
         ENDDO
         oScore:nCurr := Len( oScore:aNotes )
      ELSE
         oScore:nCurr := Len( oScore:aNotes ) + 1
         IF oScore:nCurr >= oScore:nScrStart + oScore:nScrKol1
            oScore:nScrStart := oScore:nCurr - oScore:nScrKol1 + 1
         ENDIF
      ENDIF
   ELSEIF nKey == VK_BACK
      IF oScore:nCurr > 1 .AND. oScore:nCurr <= Len(oScore:aNotes)+1
         oScore:nCurr --
         hb_ADel( oScore:aNotes, oScore:nCurr, .T. )
      ENDIF
   ELSEIF nKey == VK_INSERT
      oIns:cargo := !oIns:cargo
   ENDIF
   SetDlgEdi()
   ShowNote( Iif( oScore:nCurr>0 .AND. oScore:nCurr<=Len(oScore:aNotes), oScore:aNotes[oScore:nCurr,1], 0 ) )
   hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   RETURN NIL

STATIC FUNCTION KeyPressSim( n, lPress )

   LOCAL nNote := Int( Iif( n % 12 == 0, 12, n % 12 ) )
   LOCAL nOct := Int( ( n - 1 ) / 12 ) + 1

   IF !oPaneVP:lHide
      IF nOct >= nOctave .AND. nOct - nOctave < 2
         n := ( nOct-nOctave ) * 12 + nNote
         IF lPress
            oPaneVP:aControls[ n ]:Press()
            hwg_Redrawwindow( oPaneVP:aControls[ n ]:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
         ELSE
            oPaneVP:aControls[ n ]:Release()
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION KeyPress( nNote )

   LOCAL n := nNote + ( nOctave - 1 ) * 12

   IF nCurrMode == 1
      KeyPressSim( n, .T. )
      PlayKey( nNote )
      aSounds[n, 4] := 4
   ENDIF

   RETURN NIL

FUNCTION PlayKey( n )

   LOCAL aMenu := { aMsgs[85], aMsgs[86], aMsgs[14] }, i
   LOCAL lAdd := .F., nTDur, nDur, lAcco := .F.

   IF Valtype(n) == "A"
      lAcco := .T.
      ASort( n )
   ENDIF
   IF nCurrMode == 1 .AND. Iif( lAcco, PlayAccord( n ),  Play( n := (n + ( nOctave - 1 ) * 12) ) )

      ShowNote( n )
      //oMainWindow:oSayNote1:SetText( Note2Text( n,.T. ) )
      WITH OBJECT oScore
         IF :nCurr == 0 .OR. :nCurr > Len( :aNotes )
            Aadd( :aNotes, {n,3} )
            lAdd := .T.
            :nCurr := Len( :aNotes ) + 1
            IF !lScoreLong
               :nScrStart := Max( 1, Len( :aNotes ) - :nScrKol1 + 1 )
            ENDIF
         ELSEIF oIns:cargo
            hb_Ains( :aNotes, :nCurr, {n,3}, .T. )
            oIns:cargo := !oIns:cargo
         ELSEIF lAcco
            :aNotes[:nCurr,1] := n
         ELSE
            IF Valtype( :aNotes[:nCurr,1] ) == "A"
               AAdd( :aNotes[:nCurr,1], n )
               ASort( :aNotes[:nCurr,1] )
            ELSEIF ( i := FileMenu( oPaneNote:nLeft+200,oPaneNote:nTop+16,120,90,,,, aMenu ) ) == 2
                  :aNotes[:nCurr,1] := { :aNotes[:nCurr,1], n }
                  ASort( :aNotes[:nCurr,1] )
            ELSEIF i == 1
               :aNotes[:nCurr,1] := n
            ENDIF
         ENDIF
         :lUpdate := .T.
         :CheckBas()
         IF lAdd
            i := Len(:aNotes)
            nTDur := Int( aDur[1] * :aMetre[1] / :aMetre[2] )
            DO WHILE i > 0
               IF i < Len(:aNotes) .AND. Len( :aNotes[i] ) > 2 .AND. 't/' $ :aNotes[i,3]
                  EXIT
               ENDIF
               nTDur -= Int( aDur[Int(nDur := :aNotes[i,2])] + Iif( nDur-Int(nDur)==0.5, aDur[Int(nDur)]/2, 0 ) )
               i --
            ENDDO
            IF nTDur <= 0
               noteSetAttr( ATail(:aNotes), "t" )
            ENDIF
         ENDIF
      ENDWITH
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ENDIF

   RETURN NIL

STATIC FUNCTION Play( n )

   IF !LoadNote( n )
      RETURN .F.
   ENDIF

   StopSound( n )

   pa_SetVolume( aSounds[ n, 1 ], nCurrVol )
   pa_OpenStream( aSounds[ n, 1 ] )
   pa_StartStream( aSounds[ n, 1 ] )
   aSounds[ n, 2 ] := .T.

   RETURN .T.

STATIC FUNCTION PlayNotes( lSele )

   LOCAL nStart := 1, nEnd := Len( oScore:aNotes )
   LOCAL i, n, nSec, nDur, nOrig, arr, lPause := .F., lErr, lAccord

   IF nCurrMode == 3 .OR. Empty( oScore:aNotes )
      RETURN Nil
   ENDIF

   SET DECIMALS TO 3
   StopAllSounds()

   lMnm := .F.
   lStopBtn := .F.
   nCurrMode := 3
   IF lSele
      IF oScore:nSeleStart != 0
         nStart := oScore:nSeleStart
         nEnd := oScore:nSeleEnd
      ELSEIF oScore:nCurr > 0 .AND. oScore:nCurr <= nEnd
         nStart := oScore:nCurr
      ENDIF
   ENDIF
   IF !lScoreLong .AND. ( nStart < oScore:nScrStart .OR. nStart > oScore:nScrStart+oScore:nScrKol1-1 )
      oScore:nScrStart := nStart
   ENDIF
   oScore:nCurr := nStart

   FOR i := nStart TO nEnd
      IF lStopBtn
         EXIT
      ENDIF

      lErr := .F.
      nSec := Seconds()
      IF Valtype( arr := oScore:aNotes[i,1] ) == "A"
         lAccord := .T.
         FOR n := 1 TO Len( arr )
            IF !LoadNote( arr[n] )
               oMsg:MsgStop( "Wrong note in accord" )
               lErr := .T.
               EXIT
            ENDIF
         NEXT
         IF lErr
         ELSE
            IF nOrig == Nil
               nOrig := arr[1]
               sf_SetAccord( aSounds[nOrig,1], Iif( Len(arr)>1,aSounds[arr[2],1],Nil ), ;
                  Iif( Len(arr)>2,aSounds[arr[3],1],Nil ), Iif( Len(arr)>3,aSounds[arr[4],1],Nil ), ;
                  Iif( Len(arr)>4,aSounds[arr[5],1],Nil ), Nil, Iif( noteCheckAttr( oScore:aNotes[i], "arp" ), nDelayArp, 0 ) )
               Play( nOrig )
            ELSE
               pa_SetVolume( aSounds[ nOrig, 1 ], nCurrVol )
               sf_SetAccord( aSounds[nOrig,1], Iif( Len(arr)>1,aSounds[arr[2],1],Nil ), ;
                  Iif( Len(arr)>2,aSounds[arr[3],1],Nil ), Iif( Len(arr)>3,aSounds[arr[4],1],Nil ), ;
                  Iif( Len(arr)>4,aSounds[arr[5],1],Nil ), Nil, Iif( noteCheckAttr( oScore:aNotes[i], "arp" ), nDelayArp, 0 ) )
               sf_ChangeData( aSounds[nOrig,1], aSounds[ arr[1], 1 ] )
            ENDIF
         ENDIF
      ELSE
         lAccord := .F.
         n := oScore:aNotes[i,1]
         IF n == 0
            IF nOrig != Nil
               pa_SetPause( aSounds[nOrig,1], .T. )
               lPause := .T.
            ENDIF
         ELSE
            KeyPressSim( n, .T. )
            IF nOrig == Nil
               IF Play( n )
                  pa_SetKeep( aSounds[ n, 1 ], .T. )
                  nOrig := n
               ENDIF
            ELSE
               IF LoadNote( n )
                  pa_SetVolume( aSounds[ n, 1 ], nCurrVol )
                  //IF Len( oScore:aNotes[i] ) == 2 .OR. !("ti2/" $ oScore:aNotes[i,3])
                  IF !noteCheckAttr( oScore:aNotes[i], "ti2" )
                     sf_ChangeData( aSounds[nOrig,1], aSounds[ n, 1 ] )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF i > nStart
         SetNotesCursor( VK_RIGHT )
      ENDIF
      lMnm := oDlgPlay != Nil .AND. oDlgPlay:oCheck1:value
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      nDur := aDur[Int(oScore:aNotes[i,2])] * Iif( oScore:aNotes[i,2] - Int(oScore:aNotes[i,2]) == 0.5, 1.5, 1 ) * ( 120/oScore:nBPM )
      DO WHILE Seconds() - nSec < nDur/1000
         hwg_ProcessMessage()
         hb_gcStep()
         hwg_Sleep( 1 )
      ENDDO
      IF n > 0
         KeyPressSim( n, .F. )
      ENDIF
      IF lPause
         pa_SetPause( aSounds[nOrig,1], .F. )
      ENDIF
      IF lAccord
         sf_SetAccord( aSounds[nOrig,1] )
      ENDIF
   NEXT
   lMnm := .F.
   IF !Empty( nOrig )
      StopSoundSoftly( nOrig )
   ENDIF
   nCurrMode := 1

   RETURN NIL

STATIC FUNCTION ShowNote( n )

   oMainWindow:oSayNote1:SetText( Iif( Valtype(n) == "N", Note2Text( n, .T. ), "" ) )
   IF !Empty( bPlugNote )
      Eval( bPlugNote, n )
   ENDIF

   RETURN NIL

STATIC FUNCTION LoadNote( n )

   LOCAL cFile

   IF Empty( aSounds[ n, 1 ] )
      cFile := aOggPaths[nCurrInstr,2] + Note2Text( n )
      IF !Empty( hFileMap ) .AND. hb_HHasKey( hFileMap, cFile )
         cFile := hFileMap[cFile]
      ENDIF
      cFile := aOggPaths[nCurrInstr,3] + cFile + ".ogg"
      IF ( aSounds[ n, 1 ] := sf_initData( cFile ) ) == Nil
         oMainWindow:oSayNote1:SetText( "Error: " + hb_fnameNameExt( cFile ) )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

FUNCTION note2Text( n, lRu )

   LOCAL nNote := Int( Iif( n % 12 == 0, 12, n % 12 ) )
   LOCAL nOct := Int( ( n - 1 ) / 12 ) + 1, cRes

   IF n == 0
      cRes := ""
   ELSE
      cRes := aNotesEn[nNote] + LTrim( Str( nOct ) )
      IF cLanguage == "русский" .AND. !Empty(lRu)
         cRes += " (" + aNotesRu[nNote] + ")"
      ENDIF
   ENDIF

   RETURN cRes

STATIC FUNCTION Text2Note( cNote )

   LOCAL n, nOct := Val( Right( cNote,1 ) )

   cNote := Lower( Left( cNote, Len(cNote)-1 ) )
   IF ( n := Ascan( aNotesEn, {|s|Lower(s)==cNote} ) ) > 0
      n := n + ( nOct - 1 ) * 12
   ENDIF

   RETURN n

STATIC FUNCTION ShowAll( o, lShow )

   LOCAL i

   IF lShow
      o:Show()
   ENDIF
   FOR i := 1 TO Len( o:aControls )
      IF lShow
         o:aControls[ i ]:Show()
      ELSE
         o:aControls[ i ]:Hide()
      ENDIF
   NEXT
   IF !lShow
      o:Hide()
   ENDIF

   RETURN NIL

STATIC FUNCTION ReleaseSounds()

   LOCAL i

   FOR i := 1 TO Len( aSounds )
      IF !Empty( aSounds[ i, 1 ] )
         StopSound( i )
         sf_FreeData( aSounds[ i, 1 ] )
         aSounds[ i, 1 ] := Nil
      ENDIF
   NEXT
   IF !Empty( mnmSound )
      sf_FreeData( mnmSound )
      mnmSound := Nil
   ENDIF

   RETURN NIL

FUNCTION StopAllSounds()

   LOCAL i

   FOR i := 1 TO Len( aSounds )
      StopSound( i )
   NEXT

   RETURN NIL

STATIC FUNCTION StopSound( n )

   LOCAL nSec
   IF !Empty( aSounds[ n, 2 ] )
      aSounds[ n, 2 ] := .F.
      pa_SetEnd( aSounds[ n, 1 ] )
      nSec := Seconds()
      DO WHILE Seconds() - nSec < 0.5 .AND. !(pa_isStreamActive( aSounds[ n, 1 ] ) == 0)
         hb_gcStep()
         hwg_Sleep( 1 )
      ENDDO
      pa_AbortStream( aSounds[ n, 1 ] )
   ENDIF

   RETURN NIL

STATIC FUNCTION StopSoundSoftly( n )

   LOCAL nSec
   IF !Empty( aSounds[ n, 2 ] )
      aSounds[ n, 2 ] := .F.
      pa_SetEnd( aSounds[ n, 1 ] )
      nSec := Seconds()
      DO WHILE Seconds() - nSec < 0.5
         hb_gcStep()
         hwg_Sleep( 1 )
      ENDDO
      pa_StopStream( aSounds[ n, 1 ] )
   ENDIF

   RETURN NIL

FUNCTION TimerFunc()

   LOCAL i
   STATIC nMnmNext := 0, nMnmStart, lMnmPlay := .F.

   IF nCurrMode == 1
      FOR i := 1 TO Len( aSounds )
         IF !Empty( aSounds[ i, 1 ] )
            IF !Empty( aSounds[ i, 2 ] ) .AND. pa_isStreamActive( aSounds[ i, 1 ] ) == 0
               aSounds[ i, 2 ] := .F.
               pa_AbortStream( aSounds[ i, 1 ] )
            ENDIF
            IF !Empty( aSounds[ i, 4 ] )
               IF --aSounds[ i, 4 ] == 0
                  KeyPressSim( i, .F. )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   IF oDlgPlay != Nil .AND. lMnm
      IF mnmSound == Nil
         IF !Empty( cMnmFile )
            mnmSound := sf_initData( cMnmFile )
            //pa_OpenStream( mnmSound )
         ENDIF
      ENDIF
      IF mnmSound != Nil
         IF (nMnmNext == 0 .OR. ( Seconds() - nMnmStart ) >= nMnmNext) .AND. !lMnmPlay
            nMnmNext := 60 / oScore:nBPM
            nMnmStart := Seconds()
            lMnmPlay := .T.
            pa_SetVolume( mnmSound, nMnmVol )
            pa_OpenStream( mnmSound )
            pa_StartStream( mnmSound )
         ENDIF
      ENDIF
   ELSE
      nMnmNext := 0
   ENDIF
   IF lMnmPlay .AND. pa_isStreamActive( mnmSound ) == 0
      pa_AbortStream( mnmSound )
      lMnmPlay := .F.
   ENDIF

   RETURN NIL

STATIC FUNCTION oScore_Paint()

   LOCAL o := oPaneScore
#ifdef __PLATFORM__UNIX
   LOCAL hDC := hwg_Getdc( o:handle )
#else
   LOCAL pps    := hwg_Definepaintstru()
   LOCAL hDC    := hwg_Beginpaint( o:handle, pps )
#endif
   LOCAL x1 := oScore:x1, y1 := SCORE_Y1, nStart, nLine := 1

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )
   hwg_Drawtransparentbitmap( hDC, Iif( lScoreLong, oBmpFs2:handle, oBmpFs1:handle ), 2, 2, CLR_WHITE )

   PaintNote( o, hDC, x1, y1, oScore )
   PaintLines( o, hDC, x1, y1 )

   nStart := oScore:nScrStart
   IF lScoreLong
      //hwg_writelog( "1 / " + ltrim(str(nStart)) )
      IF nStart + oScore:nScrKol1 <= Len( oScore:aNotes )
         DO WHILE .T.
            nStart += Iif( nStart == 1, oScore:nScrKol1, oScore:nScrKol2 )
            nLine ++
            IF nStart > Len( oScore:aNotes )
               oPaneBtn:oBtnNextPage:Disable()
               EXIT
            ELSEIF y1 + SCORE_Y_LIM > o:nHeight
               oPaneBtn:oBtnNextPage:Enable()
               EXIT
            ELSE
               y1 += SCORE_Y_DELTA
               PaintNote( o, hDC, x1, y1, oScore, nStart )
               PaintLines( o, hDC, x1, y1 )
               //hwg_writelog( ltrim(str(nLine)) + " / " + ltrim(str(nStart)) )
            ENDIF
         ENDDO
      ELSE
         oPaneBtn:oBtnNextPage:Disable()
      ENDIF
   ENDIF

#ifdef __PLATFORM__UNIX
   hwg_Releasedc( o:handle, hDC )
#else
   hwg_Endpaint( o:handle, pps )
#endif
   RETURN NIL

STATIC FUNCTION oScore_Other( o, msg, wp, lp)

   LOCAL xm, ym, x1, nWidth, n := 1, i, nStart, nSeleEnd
   LOCAL aMenu
   STATIC nSelex := 0, nSeleLine := 0, nSeleStart := 0

   IF ( msg == WM_MOUSEMOVE .AND. nSelex > 0 ) .OR. msg == WM_LBUTTONDOWN
      xm := hwg_Loword( lp )
      ym := hwg_Hiword( lp )
      nWidth := oBemol:nWidth + oNote8:nWidth + oScore:nBetween * 2
      x1 := oScore:x1 + oClef1:nWidth + 8
      IF !lScoreLong .OR. ( oScore:nScrStart == 1 .AND. ym < 140 )
         IF oScore:nKey != 0
            x1 += Abs( oScore:nKey ) * Iif( oScore:nKey<0, oBemol:nWidth, oDiez:nWidth )
         ENDIF
         IF oScore:aMetre[1] > 0
            x1 += 20
         ENDIF
      ENDIF
      IF lScoreLong .AND. ym > 140
         n := Int( (ym - 30) / 100 ) + 1
         nStart := oScore:nScrStart
         i := 0
         DO WHILE ++i < n
            nStart += Iif( nStart==1, oScore:nScrKol1, oScore:nScrKol2 )
         ENDDO
         IF nStart > Len( oScore:aNotes )
            RETURN -1
         ENDIF
      ELSE
         nStart := oScore:nScrStart
      ENDIF

      IF msg == WM_MOUSEMOVE
         IF Abs( xm - nSelex ) > nWidth / 3
            IF xm < x1
               xm := x1
            ENDIF
            nSeleEnd := Min( nStart + Int( (xm - x1) / nWidth ), Len( oScore:aNotes ) )
            IF nSeleEnd > nSeleStart
               oScore:nSeleStart := nSeleStart
               oScore:nSeleEnd := nSeleEnd
            ELSE
               oScore:nSeleStart := nSeleEnd
               oScore:nSeleEnd := nSeleStart
            ENDIF
            hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
         ENDIF

      ELSEIF msg == WM_LBUTTONDOWN
         IF xm > x1
            oScore:nCurr := Min( nStart + Int( (xm - x1) / nWidth ), Len( oScore:aNotes ) )
            nSelex := xm
            nSeleLine := n
            nSeleStart := Min( nStart + Int( (nSelex - x1) / nWidth ), Len( oScore:aNotes ) )
            //hwg_writelog( ltrim(str(n)) + " " + ltrim(str(oScore:nCurr)) )
            SetDlgEdi()
            ShowNote( Iif( oScore:nCurr>0, oScore:aNotes[oScore:nCurr,1], 0 ) )
            //oMainWindow:oSayNote1:SetText( Iif( oScore:nCurr>0.AND.oScore:nCurr<=Len(oScore:aNotes), ;
            //   Note2Text( Iif( Valtype(oScore:aNotes[oScore:nCurr,1])=="A",oScore:aNotes[oScore:nCurr,1,1],oScore:aNotes[oScore:nCurr,1]), .T. ),.T. ) )
            hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
         ELSEIF xm <= 26 .AND. ym < 26
            SwitchScore()
         ENDIF
         HB_SYMBOL_UNUSED( wp )
      ENDIF
   ELSEIF msg == WM_LBUTTONUP
      nSelex := nSeleLine := nSeleStart := 0
   ELSEIF msg == WM_RBUTTONUP
      xm := hwg_Loword( lp )
      ym := hwg_Hiword( lp )
      aMenu := { aMsgs[67] }
      IF oScore:nSeleStart > 0
         AAdd( aMenu, aMsgs[74] )
         AAdd( aMenu, aMsgs[84] )
      ENDIF
      xm += o:nLeft + o:oParent:nLeft
      ym += o:nTop + o:oParent:nTop
      IF ( n := FileMenu( xm, ym, 180, 30*Len(aMenu),,,, aMenu ) ) > 0
         IF n == 1
            Player()
            PlayNotes( .F. )
         ELSEIF n == 2
            Player()
            PlayNotes( .T. )
         ELSEIF n == 3
            oScore:nSeleStart := oScore:nSeleEnd := 0
            hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
         ENDIF
      ENDIF
   ENDIF

   RETURN -1

#ifdef __PLATFORM__UNIX
STATIC FUNCTION oTest_Paint( o )

   //LOCAL o := oPaneTstRes:oTestNote
   LOCAL hDC := hwg_Getdc( o:handle )
#else
STATIC FUNCTION oTest_Paint( o, lpdis )

   //LOCAL o := oPaneTstRes:oTestNote
   LOCAL hDC := hwg_Getdrawiteminfo( lpdis )[3]
#endif
   LOCAL x1 := oScore:x1, y1 := 40

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )
   PaintNote( o, hDC, x1, y1, o:cargo )
   PaintLines( o, hDC, x1, y1 )

#ifdef __PLATFORM__UNIX
   hwg_Releasedc( o:handle, hDC )
#endif
   RETURN NIL

STATIC FUNCTION PaintLines( o, hDC, x1, y1 )

   hwg_Selectobject( hDC, oPenGrid:handle )
   hwg_Drawline( hDC, x1, y1, o:nWidth-x1, y1 )
   hwg_Drawline( hDC, x1, y1+10, o:nWidth-x1, y1+10 )
   hwg_Drawline( hDC, x1, y1+20, o:nWidth-x1, y1+20 )
   hwg_Drawline( hDC, x1, y1+30, o:nWidth-x1, y1+30 )
   hwg_Drawline( hDC, x1, y1+40, o:nWidth-x1, y1+40 )

   RETURN NIL

STATIC FUNCTION PaintNote( o, hDC, x1, y1, op, nStart )

   LOCAL i, j, n, nOct, nNote, nDur, nTDur, y, nLineHeight := 10, nBetween := oScore:nBetween
   LOCAL nTones, nWidth, nWBemol, oNote, ld, lBekar
   LOCAL aNotes := op:aNotes, nCurr := op:nCurr, nEnd
   LOCAL aBmps := { oNote1, oNote2, oNote4, oNote8, oNote16, oNote16, oNote16 }
   LOCAL aBmpsD := { oNote2d, oNote4d, oNote8d, oNote16d, oNote16d, oNote16d }
   LOCAL aPauses := { op1, op2, op4, op8, op16, op32, op32 }
   STATIC an := { 0, 0.5, 0.5, 1, 1, 1.5, 2, 2, 2.5, 2.5, 3, 3 }
   STATIC ao1 := { 9.5, 6, 2.5, -1, -4.5, -8, -11.5 }
   STATIC ao2 := { 15.5, 12, 8.5, 5, 1.5, -2, -5.5 }
   STATIC aKeyDiez := { 1.5, 3, 1, 2.5, 4, 2, 3.5 }
   STATIC aKeyBemol := { 2.5, 1, 3, 1.5, 3.5, 2, 4 }
   STATIC abem := { 12, 5, 10, 3, 8, 1, 6 }, adie := { 6, 1, 8, 3, 10, 5, 12 }

   IF !Empty( aNotes )

      IF op:lBas
         hwg_Drawbitmap( hDC, oClef2:handle,, x1, y1 )
      ELSE
         hwg_Drawbitmap( hDC, oClef1:handle,, x1, y1-nLineHeight*2 )
      ENDIF
      x1 += oClef1:nWidth + 8

      nWBemol := oBemol:nWidth
      nWidth := nWBemol + oNote8:nWidth + nBetween * 2
      IF Empty( nStart )
         nStart := op:nScrStart
      ENDIF
      nEnd := Min( Len( aNotes ), nStart + Iif( nStart<2, op:nScrKol1, op:nScrKol2 ) - 1 )
      IF nStart < 2
         // Draw key signature
         IF op:nKey > 0
            FOR i := 1 TO op:nKey
               hwg_Drawtransparentbitmap( hDC, oDiez:handle, x1, ;
                  y1+nLineHeight*aKeyDiez[i]-oDiez:nHeight, CLR_WHITE )
               x1 += oDiez:nWidth
            NEXT
         ELSEIF op:nKey < 0
            FOR i := 1 TO Abs(op:nKey)
               hwg_Drawtransparentbitmap( hDC, oBemol:handle, x1, ;
                  y1+nLineHeight*aKeyBemol[i]-oBemol:nHeight, CLR_WHITE )
               x1 += oBemol:nWidth
            NEXT
         ENDIF
         IF op:aMetre[1] > 0
            // Draw metre
            hwg_Selectobject( hDC, oFontMetre:handle )
            hwg_Drawtext( hDC, Ltrim(Str(op:aMetre[1])), x1, y1-1, x1+16, y1+nLineHeight*2-1 )
            hwg_Drawtext( hDC, Ltrim(Str(op:aMetre[2])), x1, y1+nLineHeight*2-1, x1+16, y1+nLineHeight*4-1 )
            x1 += 20
         ENDIF
      ENDIF

      IF nCurr > 0
         // Draw cursor
         IF nCurr >= nStart .AND. ( nCurr <= nEnd .OR. nCurr == Len(aNotes)+1 )
            hwg_Fillrect( hDC, x1 + (nCurr-nStart)*nWidth+nBetween, y1-30, x1 + (nCurr-nStart+1)*nWidth-nBetween, y1+nLineHeight*4+30, oBrushCursor:handle )
         ENDIF
         IF op:nSeleStart > 0 ;
            .AND. op:nSeleStart < nEnd .AND. op:nSeleEnd > nStart
            // Draw Selection
            hwg_Fillrect( hDC, x1 + (Max(op:nSeleStart,nStart)-nStart)*nWidth, y1-16, ;
               x1 + (Min(op:nSeleEnd,nEnd)-nStart+1)*nWidth, y1+nLineHeight*4+16, oBrushRange:handle )
         ENDIF
         IF oIns:cargo
            hwg_Drawbitmap( hDC, oIns:handle,, x1 + (nCurr-nStart)*nWidth, 0 )
         ENDIF
      ENDIF
      FOR i := nStart TO nEnd
         nDur := Int( aNotes[i,2] )
         j := 0
         DO WHILE .T.
            n := Iif( Valtype(aNotes[i,1]) == "A", aNotes[i,1,++j], aNotes[i,1] )
            IF n == 0
               oNote := aPauses[nDur]
               y := Iif( nDur==2, y1+nLineHeight*2-oNote:nHeight, y1+nLineHeight )
               hwg_Drawtransparentbitmap( hDC, oNote:handle, x1+(i-nStart)*nWidth+nBetween+nWBemol, ;
                  y, CLR_WHITE )
            ELSE
               nNote := Int( Iif( (nNote := (n % 12)) == 0, 12, nNote ) )
               nOct := Int( ( n - 1 ) / 12 ) + 1
               lBekar := .F.
               // Check for a key signature
               IF op:nKey != 0
                  IF op:nKey < 0
                     IF nNote < 12 .AND. an[nNote] == an[nNote+1]
                        IF Ascan( abem, nNote+1, 1, Abs(op:nKey) ) > 0
                           nNote += 1
                        ENDIF
                     ELSE
                        IF Ascan( abem, nNote, 1, Abs(op:nKey) ) > 0
                           lBekar := .T.
                        ENDIF
                     ENDIF
                  ELSEIF op:nKey > 0
                     IF nNote < 12 .AND. an[nNote] == an[nNote+1]
                        IF Ascan( adie, nNote-1, 1, op:nKey ) > 0
                           nNote -= 1
                        ENDIF
                     ELSE
                        IF Ascan( adie, nNote, 1, op:nKey ) > 0
                           lBekar := .T.
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               nTones := Iif( op:lBas, ao1[nOct], ao2[nOct] ) - an[nNote]
               y := y1 + ( nTones - 0.5 ) * nLineHeight

               IF y > 0 .AND. y < o:nHeight
                  ld := Iif( j > 1, ld, Iif( nDur == 1 .OR. nTones > 2, .F., .T. ) )
                  oNote := Iif( ld, aBmpsD[nDur-1], aBmps[nDur] )

                  IF lBekar
                     hwg_Drawtransparentbitmap( hDC, oBekar:handle, x1+(i-nStart)*nWidth+nBetween, ;
                        y+oNote1:nHeight-oBekar:nHeight+nLineHeight, CLR_WHITE )
                  ELSEIF nNote < 12 .AND. an[nNote] == an[nNote+1]
                     hwg_Drawtransparentbitmap( hDC, oBemol:handle, x1+(i-nStart)*nWidth+nBetween, ;
                        y+oNote1:nHeight-oBemol:nHeight, CLR_WHITE )
                  ENDIF
                  hwg_Drawtransparentbitmap( hDC, oNote:handle, x1+(i-nStart)*nWidth+nBetween+nWBemol, ;
                     Iif( ld, y, y+oNote1:nHeight-oNote:nHeight ), CLR_WHITE )

                  hwg_Selectobject( hDC, oPenGrid:handle )
                  IF aNotes[i,2] - nDur == 0.5
                     y := Iif( nTones-Int(nTones) == 0.5, y+nLineHeight/2, y )
                     hwg_Drawtransparentbitmap( hDC, oDot:handle, x1+(i-nStart)*nWidth+nBetween+nWBemol+oNote:nWidth, ;
                        y, CLR_WHITE )
                  ENDIF

                  IF nTones < -0.7
                     y := y1 - nLineHeight
                     DO WHILE nTones < -0.7
                        hwg_Drawline( hDC, x1+(i-nStart)*nWidth+nWBemol, y, x1+(i-nStart)*nWidth+nWidth, y )
                        nTones += 1
                        y -= nLineHeight
                     ENDDO
                  ELSEIF nTones > 4.7
                     y := y1 + nLineHeight * 5
                     DO WHILE nTones > 4.7
                        hwg_Drawline( hDC, x1+(i-nStart)*nWidth+nWBemol, y, x1+(i-nStart)*nWidth+nWidth, y )
                        nTones -= 1
                        y += nLineHeight
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
            IF Valtype(aNotes[i,1]) == "N" .OR. ( Valtype(aNotes[i,1]) == "A" .AND. ;
               j == Len(aNotes[i,1]) )
               EXIT
            ENDIF
         ENDDO
         IF Len( aNotes[i] ) > 2
            IF 'ti1' $ aNotes[i,3]
               // Draw tie
               hwg_Selectobject( hDC, oPen2:handle )
               y := y1 + ( nTones  - 0.5 ) * nLineHeight
               IF ld
                  hwg_Drawtransparentbitmap( hDC, oTied:handle, x1+(i-nStart)*nWidth+nBetween+nWBemol+oNote:nWidth-4, ;
                     y-oTied:nHeight, CLR_WHITE )
               ELSE
                  hwg_Drawtransparentbitmap( hDC, oTie:handle, x1+(i-nStart)*nWidth+nBetween+nWBemol+oNote:nWidth-4, ;
                     y+oNote1:nHeight, CLR_WHITE )
               ENDIF
            ENDIF
            IF 't/' $ aNotes[i,3]
               // Draw measure (tact)
               nTDur := 1
               IF op:aMetre[1] > 0 .AND. op:aMetre[2] > 0
                  j := i
                  nTDur := Int( aDur[1] * op:aMetre[1] / op:aMetre[2] )
                  DO WHILE j > 0
                     IF j < i .AND. Len( aNotes[j] ) > 2 .AND. 't/' $ aNotes[j,3]
                        EXIT
                     ENDIF
                     nTDur -= Int( aDur[Int(nDur := aNotes[j,2])] + Iif( nDur-Int(nDur)==0.5, aDur[Int(nDur)]/2, 0 ) )
                     j --
                  ENDDO
               ENDIF
               IF nTDur == 0
                  hwg_Selectobject( hDC, oPenGrid:handle )
                  y := 0
               ELSE
                  hwg_Selectobject( hDC, oPenRed:handle )
                  y := 8
               ENDIF
               hwg_Drawline( hDC, x1+(i-nStart+1)*nWidth-1, y1-y, x1+(i-nStart+1)*nWidth-1, y1+nLineHeight*4+y )
            ENDIF
            IF 'arp/' $ aNotes[i,3]
               hwg_Drawtransparentbitmap( hDC, oArpeggio:handle, x1+(i-nStart)*nWidth-oArpeggio:nWidth, y1, CLR_WHITE )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

STATIC FUNCTION NewNotes()

   LOCAL n

   IF oScore:lUpdate
      IF ( n := oMsg:MsgYesNoCancel( aMsgs[88] ) ) == 1
         SaveNotes()
      ELSEIF n == 0
         RETURN .F.
      ENDIF
   ENDIF
   oScore:Clean()
   oPaneHea:SetText( aMsgs[1] )
   hwg_Redrawwindow( oPaneHea:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   oScore:SetScrKol( oPaneScore )
   hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )

   RETURN .T.

STATIC FUNCTION AddRecent( cFile )

   LOCAL i

   IF ( i := Ascan( aRecent, cFile ) ) > 0
      ADel( aRecent, i )
   ELSEIF Len( aRecent ) < KOL_RECENT
      Aadd( aRecent, Nil )
   ENDIF
   AIns( aRecent, 1 )
   aRecent[1] := cFile
   lChgRecent := .T.

   RETURN Nil

FUNCTION LoadRecent( i )

   IF i <= Len( aRecent )
      LoadNotes( aRecent[i] )
   ENDIF

   RETURN Nil

STATIC FUNCTION LoadNotes( cFile )

   LOCAL oLM, oNode0, oNode1, cNode, cTemp, arr, n, n1, i, j, cNote, xPitch, nDur, nPos, nPos2

   IF !NewNotes()
      RETURN Nil
   ENDIF
   IF Empty( cFile )
#ifdef __PLATFORM__UNIX
      cFile := hwg_SelectfileEx( , hb_DirBase(), { { "Lm files", "*.lm" } } )
#else
      cFile := hwg_Selectfile( { "Lm files" }, { "*.lm" }, hb_DirBase()  )
#endif
      IF Empty( cFile )
         RETURN NIL
      ENDIF
   ENDIF

   oLM := HXMLDoc():Read( cFile )
   IF !Empty( oLM) .AND. !Empty( oLM:aItems ) .AND. (oNode0 := oLM:aItems[1]):title == 'lm'
      FOR n := 1 TO Len( oNode0:aItems )
         IF ( cNode := oNode0:aItems[n]:title ) == 'notes'
            cTemp := oNode0:aItems[n]:aItems[1]
            arr := hb_ATokens( AllTrim(cTemp), ',' )
            FOR i := 1 TO Len( arr )
               cNote := AllTrim( arr[i] )
               IF ( nPos := At( '(', cNote ) ) == 0
                  arr := Nil
                  EXIT
               ENDIF
               xPitch := Left(cNote,nPos-1)
               nDur := Val( Substr(cNote,nPos+1) )
               IF '-' $ xPitch
                  xPitch := hb_Atokens( xPitch, '-' )
                  FOR j := 1 TO Len( xPitch )
                     xPitch[j] := Text2Note( xPitch[j] )
                  NEXT
                  arr[i] := { xPitch, nDur }
               ELSE
                  arr[i] := { Text2Note( xPitch ), nDur }
               ENDIF
               IF ( nPos := At( '[', cNote ) ) > 0 .AND. ( nPos2 := hb_At( ']', cNote, nPos+1 ) ) > 0
                  AAdd( arr[i], Substr( cNote,nPos+1,nPos2-nPos-1 ) )
               ENDIF
            NEXT
         ELSEIF cNode == "params"
            FOR n1 := 1 TO Len( oNode0:aItems[n]:aItems )
               oNode1 := oNode0:aItems[n]:aItems[n1]
               IF ( cNode := oNode1:title ) == "bpm"
                  oScore:nBPM := Val( oNode1:aItems[1] )
               ELSEIF cNode == "metre"
                  oScore:aMetre := hb_ATokens( AllTrim(oNode1:aItems[1]), '/' )
                  oScore:aMetre[1] := Val( oScore:aMetre[1] )
                  oScore:aMetre[2] := Val( oScore:aMetre[2] )
               ELSEIF cNode == "key"
                  oScore:nKey := Val( oNode1:aItems[1] )
               ENDIF
            NEXT
         ELSEIF cNode == "title"
            oScore:cTitle := oNode0:aItems[n]:aItems[1]
         ENDIF
      NEXT
   ENDIF

   IF !Empty( arr )
      IF oDlgPlay != Nil
         oDlgPlay:oTrackBPM:Value := Round( (oScore:nBPM-5) / 164.2857, 2 )
         oDlgPlay:oSayBPM:SetText( Ltrim(Str(oScore:nBPM)) )
      ENDIF
      IF oDlgEdi != Nil
         oDlgEdi:oCombo1:Value := Ascan( aMetres, Ltrim(Str(oScore:aMetre[1]))+'/'+Ltrim(Str(oScore:aMetre[2])) )
         oDlgEdi:oCombo2:Value := Ascan( aKeySign, Ltrim(Str(oScore:nKey)) )
      ENDIF
      oScore:aNotes := arr
      oScore:SetScrKol( oPaneScore )
      oScore:CheckBas()
      oScore:nSeleStart := oScore:nSeleEnd := 0
      oScore:nCurrPage := 1
      SetNotesCursor( VK_HOME )
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      oPaneHea:SetText( aMsgs[1] + Iif( Empty(oScore:cTitle), "", ": "+oScore:cTitle ) )
      hwg_Redrawwindow( oPaneHea:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      AddRecent( cFile )
   ELSE
      oMsg:MsgStop( "Wrong file format", "Error" )
   ENDIF

   RETURN Nil

STATIC FUNCTION SaveNotes()

   LOCAL i, j, aNotes := oScore:aNotes, cFile
   LOCAL s := '<?xml version="1.0" encoding="UTF-8"?>' + Chr(10) + '<lm version="0.3">' + Chr(10)

#ifdef __PLATFORM__UNIX
   IF Empty( aNotes ) .OR. Empty( cFile := hwg_Selectfile( "Lm files", "*.lm", hb_DirBase() ) )
#else
   IF Empty( aNotes ) .OR. Empty( cFile := hwg_Savefile( "*.*", "Lm files", "*.lm", hb_DirBase() ) )
#endif
      RETURN Nil
   ENDIF

   IF Empty( hb_fnameExt( cFile ) )
      cFile := hb_fnameExtSet( cFile, "lm" )
   ENDIF
   IF File( cFile ) .AND. !oMsg:MsgYesNo( aMsgs[53] )
      RETURN Nil
   ENDIF

   IF !Empty( oScore:cTitle )
      s += '<title>' + oScore:cTitle + '</title>' + Chr(10)
   ENDIF
   s += '<params>' + Chr(10) + ;
      '  ' + '<bpm>' + Ltrim(Str(oScore:nBPM)) + '</bpm>' + Chr(10) + ;
      '  ' + '<key>' + Ltrim(Str(oScore:nKey)) + '</key>' + Chr(10) + ;
      '  ' + '<metre>' + Ltrim(Str(oScore:aMetre[1])) + '/' + Ltrim(Str(oScore:aMetre[2])) + '</metre>' + Chr(10) + ;
      '</params>' + Chr(10)

   s += '<notes>' + Chr(10) + '  '
   FOR i := 1 TO Len( aNotes )
      s += Iif( i==1, '', ',' )
      IF Valtype( aNotes[i,1] ) == "A"
         FOR j := 1 TO Len( aNotes[i,1] )
            s += Iif( j==1, '', '-' ) + note2Text( aNotes[i,1,j] )
         NEXT
      ELSE
         s += note2Text( aNotes[i,1] )
      ENDIF
      s += '(' + Ltrim(Str(aNotes[i,2])) + ')'
      IF Len( aNotes[i] ) > 2 .AND. !Empty( aNotes[i,3] )
         s += '[' + aNotes[i,3] + ']'
      ENDIF
   NEXT
   s += Chr(10) + '</notes>' + Chr(10)
   s += '</lm>'

   hb_MemoWrit( cFile, s )
   AddRecent( cFile )

   RETURN NIL

STATIC FUNCTION ImportNotes()

   LOCAL cFile, oImp, i, arr, cExt, lRes := .F.

   IF !NewNotes()
      RETURN Nil
   ENDIF
#ifdef __PLATFORM__UNIX
   cFile := hwg_SelectfileEx( , hb_DirBase(), { { "MusicXML (mxl)", "*.mxl" }, { "MusicXML uncompressed", "*.xml" }, { "MuseScore (mscz)", "*.mscz" }, { "MuseScore uncompressed", "*.mscx" }, { "Midi", "*.mid" } } )
#else
   cFile := hwg_Selectfile( { "MusicXML (mxl)", "MusicXML uncompressed", "MuseScore (mscz)", "MuseScore uncompressed", "Midi" }, { "*.mxl", "*.xml", "*.mscz", "*.mscx", "*.mid" }, hb_DirBase()  )
#endif
   IF Empty( cFile )
      RETURN NIL
   ENDIF

   IF ( cExt := hb_fnameExt( cFile ) ) == ".mxl" .OR. cExt == ".xml"
      IF Empty( oImp := Mxl():Open( cFile ) )
         oMsg:MsgStop( "Error reading file (" + Ltrim(Str(Mxl():nError)) + ")" )
      ELSE
         arr := oImp:GetTracks()
         IF !Empty( arr )
            i := 1
            IF Len( arr ) == 1 .OR. ( i := midi_SeleChn( arr ) ) > 0
               oImp:ToLM( oScore, arr[i,1] )
               //SaveNotes()
               lRes := .T.
            ENDIF
         ENDIF
      ENDIF

   ELSEIF cExt == ".mscz" .OR. cExt == ".mscx"
      IF Empty( oImp := Mscz():Open( cFile ) )
         oMsg:MsgStop( "Error reading file (" + Ltrim(Str(Mscz():nError)) + ")" )
      ELSE
         arr := oImp:GetTracks()
         IF !Empty( arr )
            i := 1
            IF Len( arr ) == 1 .OR. ( i := midi_SeleChn( arr ) ) > 0
               oImp:ToLM( oScore, arr[i,1] )
               //SaveNotes()
               lRes := .T.
            ENDIF
         ENDIF
      ENDIF

   ELSEIF cExt == ".mid"
      IF Empty( oImp := Midi():Open( cFile ) )
         oMsg:MsgStop( "Error reading file (" + Ltrim(Str(Midi():nError)) + ")" )
      ELSE
         arr := oImp:GetTracks()
         IF !Empty( arr )
            i := 1
            IF Len( arr ) == 1 .OR. ( i := midi_SeleChn( arr,cFile,oImp ) ) > 0
               IF oImp:ToLM( oScore, arr[i,2], arr[i,3] )
                  lRes := .T.
               ELSE
                  oMsg:MsgStop( "A channel can't be imported" )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF lRes
      oScore:SetScrKol( oPaneScore )
      oScore:CheckBas()
      oScore:nSeleStart := oScore:nSeleEnd := 0
      oScore:nCurrPage := 1
      SetNotesCursor( VK_HOME )
      IF oDlgEdi != Nil
         oDlgEdi:oCombo1:Value := Ascan( aMetres, Ltrim(Str(oScore:aMetre[1]))+'/'+Ltrim(Str(oScore:aMetre[2])) )
         oDlgEdi:oCombo2:Value := Ascan( aKeySign, Ltrim(Str(oScore:nKey)) )
      ENDIF
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      oPaneHea:SetText( aMsgs[1] + Iif( Empty(oScore:cTitle), "", ": "+oScore:cTitle ) )
      hwg_Redrawwindow( oPaneHea:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ENDIF

   RETURN NIL

STATIC FUNCTION midi_SeleChn( arr, cFile, oMidi )

   LOCAL oDlg1, oBrw, nHeight := Min( Len(arr), 6 ) * 29 + 50
   LOCAL bEnter := {||
      oDlg1:lResult := .T.
      oDlg1:Close()
      Return Nil
   }
   LOCAL bKeyDown := {|o,key|
      IF key == VK_ESCAPE
         oDlg1:Close()
      ELSEIF (key == 68 .OR. key == 100) .AND. !Empty( cFile ) .AND. !Empty( oMidi )
         hb_MemoWrit( hb_DirBase() + hb_fnameName(cFile)+"_"+Ltrim(Str(arr[o:nCurrent,2]))+".dmp", oMidi:Dump(arr[o:nCurrent,2]) )
      ENDIF
      Return .T.
   }

   INIT DIALOG oDlg1 TITLE "" AT 100, 100 SIZE 180, nHeight ;
      BACKCOLOR pClr["topdark"] STYLE WND_NOTITLE + WND_NOSIZEBOX
   oDlg1:oParent := oMainWindow

   @ 0, 0 BROWSE oBrw ARRAY OF oDlg1 SIZE 180, nHeight-50 FONT oFontWnd NO VSCROLL

   oBrw:aArray := arr
   oBrw:AddColumn( HColumn():New( ,{ ||oBrw:aArray[oBrw:nCurrent,1] },"C",20 ) )

   oBrw:lDispHead := .F.
   oBrw:bcolorSel := oBrw:htbColor := pClr["topmid"]
   oBrw:bColor := oBrw:sepColor := pClr["topdark"]
   oBrw:tcolorSel := oBrw:httColor := oBrw:tcolor := CLR_WHITE

   oBrw:bEnter := bEnter
   oBrw:bKeyDown := bKeyDown
   oBrw:lInFocus := .T.

   @ 10, oDlg1:nHeight-40 OWNERBUTTON SIZE 70, 30 TEXT aMsgs[13] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| oDlg1:lResult:=.T.,hwg_EndDialog() }
   ATail(oDlg1:aControls):aStyle := aStyleBtn

   @ oDlg1:nWidth-80, oDlg1:nHeight-40 OWNERBUTTON SIZE 70, 30 TEXT aMsgs[14] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg1:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlg1

   IF oDlg1:lResult
      RETURN oBrw:nCurrent
   ENDIF

   RETURN 0

STATIC FUNCTION ExportNotes()

   LOCAL oDlg1, oPanel, oCheck1, oCheck2, oCheck3, oCheck4, oCheck5, l1 := .F., l2 := .F., l3 := .F., l4 := .F., l5 := .F.
   LOCAL cFile, oExp, i, arr := oScore:aNotes, aMidiOpt
   STATIC aMidiInstr := { {"Acoustic Grand Piano",0}, {"Rock Organ",18}, {"Church Organ",19}, {"Accordion",21}, ;
      {"Acoustic Guitar (nylon)",24}, {"Acoustic Guitar (steel)",25}, {"Electric Guitar (jazz)",26}, ;
      {"Electric Guitar (clean)",27}, {"Violin",40}, {"Soprano sax",64}, {"Alto sax",65}, ;
      {"Tenor sax",66}, {"Baritone sax",67}, {"Clarinet",71}, {"Flute",73}, {"Recorder",74}, {"Pan flute",75} }

   IF Empty( arr )
      RETURN Nil
   ENDIF

   INIT DIALOG oDlg1 TITLE "" AT 100, 100 SIZE 300, 240 ;
      BACKCOLOR pClr["topdark"] STYLE WND_NOTITLE + WND_NOSIZEBOX
   oDlg1:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[77] COORS 20 BTN_CLOSE

   @ 20, TOPPANE_HEIGHT+20 GET CHECKBOX oCheck1 VAR l1 CAPTION "Midi" SIZE oDlg1:nWidth-40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

   @ 20, TOPPANE_HEIGHT+50 GET CHECKBOX oCheck2 VAR l2 CAPTION "MusicXML uncompressed" SIZE oDlg1:nWidth-40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

   @ 20, TOPPANE_HEIGHT+80 GET CHECKBOX oCheck3 VAR l3 CAPTION "MusicXML (mxl)" SIZE oDlg1:nWidth-40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

   @ 20, TOPPANE_HEIGHT+110 GET CHECKBOX oCheck4 VAR l4 CAPTION "MuseScore uncompressed" SIZE oDlg1:nWidth-40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

   @ 20, TOPPANE_HEIGHT+140 GET CHECKBOX oCheck5 VAR l5 CAPTION "MuseScore (mscz)" SIZE oDlg1:nWidth-40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"]

   @ 20, oDlg1:nHeight-40 OWNERBUTTON SIZE 70, 30 TEXT aMsgs[13] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| oDlg1:lResult:=.T.,hwg_EndDialog() }
   ATail(oDlg1:aControls):aStyle := aStyleBtn

   @ oDlg1:nWidth-90, oDlg1:nHeight-40 OWNERBUTTON SIZE 70, 30 TEXT aMsgs[14] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg1:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlg1

   IF oDlg1:lResult .AND. ( l1 .OR. l2  .OR. l3  .OR. l4  .OR. l5 )

      IF l1
#ifdef __PLATFORM__UNIX
         cFile := hwg_SelectfileEx( , hb_DirBase(), { { "Midi", "*.mid" } } )
#else
         cFile := hwg_Savefile( "*.*", "Midi", "*.mid", hb_DirBase() )
#endif
         IF !Empty( cFile ) .AND. oMsg:MsgGet( "Midi options", aMidiOpt := {"Instrument",,aMidiInstr} )
            cFile := hb_fnameExtSet( cFile, "mid" )
            oExp := Midi():New( 1, 96, 1 )
            i := oScore:aMetre[2]
            oExp:AddFF( 1, 0x58, Chr(oScore:aMetre[1]) + Chr(Iif(i==2,1,Iif(i==2,4,8))) + Chr(0x18) + Chr(0x8) )
            IF !Empty( oScore:cTitle )
               oExp:AddFF( 1, 5, oScore:cTitle )
            ENDIF
            oExp:AddProgram( 1, 0, aMidiInstr[aMidiOpt[4],2] )
            FOR i := 1 TO Len( arr )
               oExp:AddNote( 1, 0, arr[i,1], arr[i,2] )
            NEXT
            oExp:Save( hb_fnameExtSetDef( cFile, ".mid" ) )
         ENDIF
      ENDIF
      IF l2 .OR. l3
#ifdef __PLATFORM__UNIX
         cFile := hwg_SelectfileEx( , hb_DirBase(), { { "MusicXML", "*.xml" } } )
#else
         cFile := hwg_Savefile( "*.*", "MusicXML", "*.xml", hb_DirBase() )
#endif
         IF !Empty( cFile )
            oExp :=Mxl():New( oScore )
            oExp:AddMeasure( oScore )
            FOR i := 1 TO Len( arr )
               oExp:AddNote( arr[i,1], arr[i,2], Iif( Len(arr[i])>2,arr[i,3],Nil ) )
               IF i < Len( arr ) .AND. noteCheckAttr( arr[i], 't' )
                  oExp:AddMeasure( oScore )
               ENDIF
            NEXT
            IF l2
               oExp:Save( hb_fnameExtSet( cFile, ".xml" ) )
            ENDIF
            IF l3
               oExp:Save( hb_fnameExtSet( cFile, ".mxl" ) )
            ENDIF
         ENDIF
      ENDIF
      IF l4 .OR. l5
#ifdef __PLATFORM__UNIX
         cFile := hwg_SelectfileEx( , hb_DirBase(), { { "MuseScore", "*.mscz" } } )
#else
         cFile := hwg_Savefile( "*.*", "MuseScore", "*.mscz", hb_DirBase() )
#endif
         IF !Empty( cFile )
            oExp :=Mscz():New( 2, oScore )
            oExp:AddMeasure( oScore )
            FOR i := 1 TO Len( arr )
               oExp:AddNote( arr[i,1], arr[i,2], Iif( Len(arr[i])>2,arr[i,3],Nil ) )
               IF i < Len( arr ) .AND. noteCheckAttr( arr[i], 't' )
                  oExp:AddMeasure( oScore )
               ENDIF
            NEXT
            IF l4
               oExp:Save( hb_fnameExtSet( cFile, ".mscx" ) )
            ENDIF
            IF l5
               oExp:Save( hb_fnameExtSet( cFile, ".mscz" ) )
            ENDIF
         ENDIF
      ENDIF

   ENDIF

   RETURN NIL

STATIC FUNCTION Player()

   LOCAL oPanel, oTrack, oCheck1, oTrackBPM, oSayBPM, oTrack2
   LOCAL bBPM := {|o,n|
      oScore:nBpm := Int( 5 + n * 164.2858 )
      oDlgPlay:oSayBPM:SetText( Ltrim(Str(oScore:nBPM)) )
      HB_SYMBOL_UNUSED(o)
      RETURN .T.
   }
   LOCAL bMnmVol := {|o,n|
      IF n > 0.48 .AND. n < 0.52
         nMnmVol := 1
      ELSEIF n >= 0.52
         nMnmVol := n * 3
      ELSE
         nMnmVol := n * 2
      ENDIF
      HB_SYMBOL_UNUSED(o)
      RETURN .T.
   }

   IF !Empty( oDlgPlay )
      RETURN Nil
   ENDIF

   INIT DIALOG oDlgPlay TITLE "Player" BACKCOLOR pClr["clr2"] ;
      AT Int(oMainWindow:nWidth*0.7), Int(oMainWindow:nHeight*0.6) SIZE 360, 340 FONT oFontWnd STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| lStopBtn := .T., lMnm := .F., oDlgPlay := Nil}

   oDlgPlay:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[73] COORS 20 BTN_CLOSE

   @ 20, TOPPANE_HEIGHT+20 OWNERBUTTON SIZE 28, 28 ;
      BITMAP "play_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {|| PlayNotes( .F. ) } TOOLTIP aMsgs[67]
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   @ 50, TOPPANE_HEIGHT+20 OWNERBUTTON SIZE 28, 28 ;
      BITMAP "stop_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {|| lStopBtn := .T. }
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   @ 110, TOPPANE_HEIGHT+20 BITMAP "volume" FROM RESOURCE
   oTrack := HTrack():New( oDlgPlay,, 140, TOPPANE_HEIGHT+20, 150, 28,,, CLR_WHITE, pClr["clr1"], 16 )
   hwg_SetCtrlName( oTrack, "OTRACK" )
   hwg_Addtooltip( oTrack:handle, aMsgs[70] )
   oTrack:cargo := 2
   oTrack:tColor2 := CLR_LIGHTGRAY_1
   oTrack:bChange := oPaneBtn:oTrack:bChange
   oTrack:bPaint := oPaneBtn:oTrack:bPaint
   oTrack:Value := oPaneBtn:oTrack:Value

   @ 300, TOPPANE_HEIGHT+20 CHECKBOX oCheck1 CAPTION " " SIZE 40, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["clr2"] TOOLTIP aMsgs[68]

   @ 20, TOPPANE_HEIGHT+50 OWNERBUTTON SIZE 28, 28 ;
      BITMAP "play_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {|| PlayNotes( .T. ) } TOOLTIP aMsgs[74]
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   // BPM
   @ 20, TOPPANE_HEIGHT+110 SAY "bpm:" SIZE 44, 24 COLOR CLR_WHITE TRANSPARENT
   oTrackBPM := HTrack():New( oDlgPlay,, 70, TOPPANE_HEIGHT+110, 200, 28,,, CLR_WHITE, pClr["clr1"], 16 )
   hwg_SetCtrlName( oTrackBPM, "OTRACKBPM" )
   hwg_Addtooltip( oTrackBPM:handle, aMsgs[72] )
   oTrackBPM:tColor2 := CLR_LIGHTGRAY_1
   oTrackBPM:bChange := bBPM
   oTrackBPM:Value := Round( (oScore:nBPM-5) / 164.2857,2 )
   @ oDlgPlay:nWidth-60, TOPPANE_HEIGHT+110 SAY oSayBPM CAPTION Ltrim(Str(oScore:nBPM)) SIZE 50, 24 ;
      FONT oFontBold COLOR CLR_WHITE TRANSPARENT

   // Metronom play, stop and volume
   @ 20, TOPPANE_HEIGHT+150 OWNERBUTTON SIZE 28, 28 ;
      BITMAP "play_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {|| lMnm := .T. } TOOLTIP aMsgs[69]
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   @ 50, TOPPANE_HEIGHT+150 OWNERBUTTON SIZE 28, 28 ;
      BITMAP "stop_14" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {|| lMnm := .F. }
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   @ 110, TOPPANE_HEIGHT+150 BITMAP "volume" FROM RESOURCE
   oTrack2 := HTrack():New( oDlgPlay,, 140, TOPPANE_HEIGHT+150, 150, 28,,, CLR_WHITE, pClr["clr1"], 16 )
   hwg_Addtooltip( oTrack2:handle, aMsgs[71] )
   oTrack2:cargo := 2
   oTrack2:tColor2 := CLR_LIGHTGRAY_1
   oTrack2:bChange := bMnmVol
   oTrack2:bPaint := oTrack:bPaint
   oTrack2:Value := 0.5

   @ 20, 280 SAY "" SIZE oDlgPlay:nWidth-40, 2

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 130, 300 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| oDlgPlay:Close }
   ATail(oDlgPlay:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlgPlay NOMODAL

   RETURN NIL

STATIC FUNCTION NoteEditor()

   LOCAL oPanel, oCheck1, oCombo1, oCombo2, oLenta
   LOCAL n1 := Ascan( aMetres, Ltrim(Str(oScore:aMetre[1]))+'/'+Ltrim(Str(oScore:aMetre[2])) )
   LOCAL n2 := Ascan( aKeySign, Ltrim(Str(oScore:nKey)) )
   LOCAL bInsPau := {||
      IF oScore:nCurr > Len( oScore:aNotes )
         Aadd( oScore:aNotes, {0,1} )
         oScore:lUpdate := .T.
      ELSEIF oScore:nCurr > 0
         hb_Ains( oScore:aNotes, oScore:nCurr, {0,1}, .T. )
         oScore:lUpdate := .T.
      ENDIF
      oPaneScore:Refresh()
      RETURN .T.
   }
   LOCAL bInsTact := {||
      IF oScore:nCurr > 0 .AND. oScore:nCurr <= Len( oScore:aNotes )
         IF Len( oScore:aNotes[oScore:nCurr] ) == 2
            AAdd( oScore:aNotes[oScore:nCurr], "t/" )
         ELSE
            oScore:aNotes[oScore:nCurr,3] := Iif( Empty(oScore:aNotes[oScore:nCurr]), "t/", "" )
         ENDIF
         oScore:lUpdate := .T.
         oPaneScore:Refresh()
      ENDIF
      RETURN .T.
   }
   LOCAL bSetTie := {||
      IF oScore:nSeleStart > 0 .AND. Abs( oScore:nSeleEnd - oScore:nSeleStart ) == 1 ;
         .AND. oScore:aNotes[oScore:nSeleStart,1] == oScore:aNotes[oScore:nSeleEnd,1]
         noteSetAttr( oScore:aNotes[oScore:nSeleStart], "ti1" )
         noteSetAttr( oScore:aNotes[oScore:nSeleEnd], "ti2" )
         oScore:lUpdate := .T.
         oPaneScore:Refresh()
      ENDIF
      RETURN .T.
   }
   LOCAL bSetArp := {||
      IF !Empty( oScore:aNotes ) .AND. ValType( oScore:aNotes[oScore:nCurr,1] ) == "A"
         noteSetAttr( oScore:aNotes[oScore:nCurr], "arp" )
         oScore:lUpdate := .T.
         oPaneScore:Refresh()
      ENDIF
      RETURN .T.
   }
   LOCAL bMetre := {||
      IF aMetres[n1] != Ltrim(Str(oScore:aMetre[1]))+'/'+Ltrim(Str(oScore:aMetre[2]))
         oScore:aMetre[1] := Val( Left(aMetres[n1],1) )
         oScore:aMetre[2] := Val( Right(aMetres[n1],1) )
         oScore:SetScrKol( oPaneScore )
         oScore:lUpdate := .T.
         oPaneScore:Refresh()
      ENDIF
      RETURN .T.
   }
   LOCAL bKeySign := {||
      IF aKeySign[n2] != Ltrim(Str(oScore:nKey))
         oScore:nKey := Val( aKeySign[n2] )
         oScore:SetScrKol( oPaneScore )
         oScore:lUpdate := .T.
         oPaneScore:Refresh()
      ENDIF
      RETURN .T.
   }

   IF !Empty( oDlgEdi )
      RETURN Nil
   ENDIF

   INIT DIALOG oDlgEdi TITLE "Notes Editor" BACKCOLOR pClr["dlgback"] ;
      AT Int(oMainWindow:nWidth*0.7), Int(oMainWindow:nHeight*0.6) SIZE 360, 340 FONT oFontWnd ;
      STYLE WND_NOTITLE + WND_NOSIZEBOX ON EXIT {|| oDlgEdi:= Nil}

   oDlgEdi:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[54] COORS 20 BTN_CLOSE

   @ oDlgEdi:nWidth/2-40, 40 OWNERBUTTON SIZE 30, 28 TEXT "<" ON CLICK {|| SetNotesCursor(VK_LEFT) } ;
      TOOLTIP aMsgs[56]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ oDlgEdi:nWidth/2+10, 40 OWNERBUTTON SIZE 30, 28 TEXT ">" ON CLICK {|| SetNotesCursor(VK_RIGHT) } ;
      TOOLTIP aMsgs[57]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 20, 80 SAY aMsgs[55]+":" SIZE 140, 24 COLOR CLR_WHITE TRANSPARENT

   @ oDlgEdi:nWidth-120, 80 CHECKBOX oCheck1 CAPTION "  x 1.5" SIZE 100, 24 ;
      COLOR CLR_WHITE BACKCOLOR pClr["dlgback"] ON CLICK {||SetDuration(oCheck1:Value)}

   oLenta := HLenta():New( ,, 24, 110, 300, 26, oFontWnd,,, {|o|SetDuration(o:nSelected)},,, ;
      { "1","1/2","1/4","1/8","1/16" }, 60, aStyleLenta )
   hwg_SetCtrlName( oLenta, "OLENTA" )
   oLenta:Value := 1

   @ 20, 170 OWNERBUTTON SIZE 30, 28 ;
      BITMAP "p1" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK bInsPau TOOLTIP aMsgs[58]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 60, 170 OWNERBUTTON SIZE 30, 28 ;
      TEXT "|" ;
      ON CLICK bInsTact //TOOLTIP aMsgs[58]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 100, 170 OWNERBUTTON SIZE 30, 28 ;
      BITMAP "tie_24d" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK bSetTie TOOLTIP aMsgs[83]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 140, 170 OWNERBUTTON SIZE 30, 28 ;
      TEXT 'a' ;
      ON CLICK bSetArp TOOLTIP aMsgs[92]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 20, 210 SAY "" of oPaneTst1 SIZE oDlgEdi:nWidth-40, 2

   @ 20, 218 OWNERBUTTON SIZE 40, 28 ;
      BITMAP "up_down" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
      ON CLICK {||Transpo()} TOOLTIP aMsgs[78]
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   @ 160,230 GET COMBOBOX oCombo2 VAR n2 ITEMS aKeySign SIZE 80, 28 ON CHANGE bKeySign

   @ 260,230 GET COMBOBOX oCombo1 VAR n1 ITEMS aMetres SIZE 80, 28 ON CHANGE bMetre

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 130, 300 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| oDlgEdi:Close() }
   ATail(oDlgEdi:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlgEdi NOMODAL

   SetDlgEdi()

   RETURN Nil

STATIC FUNCTION SetDuration( n )

   LOCAL nOld

   IF oScore:nCurr > 0 .AND. oScore:nCurr <= Len( oScore:aNotes )
      nOld := oScore:aNotes[oScore:nCurr,2]
      IF Valtype( n ) == "N"
         oScore:aNotes[oScore:nCurr,2] := n + ( nOld - Int(nOld) )
      ELSE
         oScore:aNotes[oScore:nCurr,2] := Int(nOld) + Iif( n, 0.5, 0 )
      ENDIF
      oScore:lUpdate := .T.
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ENDIF

   RETURN .T.

STATIC FUNCTION SetDlgEdi()

   LOCAL n

   IF Empty( oDlgEdi ) .OR. oScore:nCurr == 0 .OR. oScore:nCurr > Len( oScore:aNotes )
      RETURN Nil
   ENDIF

   n := Int( oScore:aNotes[oScore:nCurr,2] )
   oDlgEdi:oCheck1:Value := ( oScore:aNotes[oScore:nCurr,2] - n == 0.5 )
   oDlgEdi:oLenta:Value := n

   RETURN Nil

STATIC FUNCTION Transpo()

   LOCAL oDlg, oPanel, oGet1, oBtnBack, oLenta1, oLenta2
   LOCAL nz := 1, n := 1, aCombo := { "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6" }
   LOCAL aBack := {}
   LOCAL bTransp := {||
      LOCAL i, j, nStart, nEnd, nTranspo
      AAdd( aBack, AClone( oScore:aNotes ) )
      oBtnBack:Enable()
      IF oScore:nSeleStart != 0
         nStart := oScore:nSeleStart
         nEnd := oScore:nSeleEnd
      ELSE
         nStart := 1
         nEnd := Len( oScore:aNotes )
      ENDIF
      nTranspo := Val( aCombo[n] ) * Iif( nz==1, 2, -2 )
      FOR i := nStart TO nEnd
         IF Valtype(oScore:aNotes[i,1]) == "N" .AND. oScore:aNotes[i,1] > 0
            oScore:aNotes[i,1] := oScore:aNotes[i,1] + nTranspo
         ELSEIF Valtype(oScore:aNotes[i,1]) == "A"
            FOR j := 1 TO Len( oScore:aNotes[i,1] )
               oScore:aNotes[i,1,j] := oScore:aNotes[i,1,j] + nTranspo
            NEXT
         ENDIF
      NEXT
      oScore:lUpdate := .T.
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      RETURN Nil
      }
   LOCAL bBack := {||
      oScore:aNotes := ATail( aBack )
      hb_ADel( aBack, Len(aBack), .T. )
      IF Empty( aBack )
         oBtnBack:Disable()
      ENDIF
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      RETURN Nil
      }

   INIT DIALOG oDlg TITLE "Transpo" ;
      AT 100, 100 SIZE 340, 270 ;
      FONT oFontWnd BACKCOLOR pClr["clr2"] STYLE WND_NOTITLE + WND_NOSIZEBOX

   oDlg:oParent := oDlgEdi

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[78] COORS 20 BTN_CLOSE

   oLenta1 := HLenta():New( ,, 90, 60, 160, 28, oFontWnd,,, ;
      {|o|nz:=o:nSelected},,, { aMsgs[79],aMsgs[80] }, 80, aStyleLenta )
   oLenta1:Value := nz

   oLenta2 := HLenta():New( ,, 20, 100, 300, 28, oFontWnd,,, ;
      {|o|n:=o:nSelected},,, aCombo, 40, aStyleLenta )
   oLenta2:Value := n

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 50, 180 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[81] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK bTransp
   ATail(oDlg:aControls):aStyle := aStyleBtn

   @ 190, 180 OWNERBUTTON oBtnBack SIZE 100, 32 TEXT aMsgs[82] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK bBack
   ATail(oDlg:aControls):aStyle := aStyleBtn

   @ 120, 230 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlg ON ACTIVATE {||oBtnBack:Disable()}

   RETURN Nil

STATIC FUNCTION SetPaneBtn()

   LOCAL lScoreBtnExist := !Empty( hwg_GetItemByName( oPaneBtn:aControls, "OBTNNEXTPAGE" ) )
   LOCAL oBtnNextPage, oBtnPrevPage

   IF lScoreLong
      oPaneBtn:oBtn1:Hide()
      oPaneBtn:oBtn2:Hide()
      oPaneBtn:oSayOct1:Hide()
      IF lScoreBtnExist
         oPaneBtn:oBtnPrevPage:Show()
         oPaneBtn:oBtnNextPage:Show()
      ELSE
         @ oPaneBtn:nWidth/2-54, 2 OWNERBUTTON oBtnPrevPage OF oPaneBtn SIZE 30, 24 ;
            BITMAP "up" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
            ON CLICK {|| ScorePage( -1 ) }
         oBtnPrevPage:aStyle := aStyleBtn
         @ oPaneBtn:nWidth/2+24, 2 OWNERBUTTON oBtnNextPage OF oPaneBtn SIZE 30, 24 ;
            BITMAP "down" FROM RESOURCE TRANSPARENT COLOR CLR_WHITE ;
            ON CLICK {|| ScorePage( 1 ) }
         oBtnNextPage:aStyle := aStyleBtn
      ENDIF
      IF oScore:nCurrPage == 1
         oPaneBtn:oBtnPrevPage:Disable()
      ELSE
         oPaneBtn:oBtnPrevPage:Enable()
      ENDIF

   ELSEIF nCurrMode == 1
      IF lScoreBtnExist
         oPaneBtn:oBtnPrevPage:Hide()
         oPaneBtn:oBtnNextPage:Hide()
      ENDIF
      oPaneBtn:oBtn1:Show()
      oPaneBtn:oBtn2:Show()
      oPaneBtn:oSayOct1:Show()

   ELSEIF nCurrMode == 2
      IF lScoreBtnExist
         oPaneBtn:oBtnPrevPage:Hide()
         oPaneBtn:oBtnNextPage:Hide()
      ENDIF
      oPaneBtn:oBtn1:Hide()
      oPaneBtn:oBtn2:Hide()
      oPaneBtn:oSayOct1:Hide()

   ENDIF

   RETURN Nil

STATIC FUNCTION SwitchScore()

   LOCAL lScoreBtnsExists := .T., nSec, y1 := SCORE_Y1, nStart
   STATIC yp1

   IF lScoreLong
      oPaneScore:Move( ,, oPaneScore:nWidth, yp1-8 )
      oPaneNote:Move( ,, oPaneNote:nWidth, yp1 )

      oPaneBtn:Move( ,MAINWND_H1-36 )

      oMainWindow:Move( ,, oMainWindow:nWidth, MAINWND_H1 )
      oMainWindow:oSayNote1:Show()
      nStart := oScore:nScrStart
      DO WHILE nStart + ( nSec := Iif( nStart==1, oScore:nScrKol1, oScore:nScrKol2 ) ) < oScore:nCurr
         nStart += nSec
      ENDDO
      oScore:nScrStart := nStart
      ShowAll( oPaneVP, .T. )
      lScoreLong := .F.
   ELSE
      yp1 := oPaneNote:nHeight
      ShowAll( oPaneVP, .F. )
      oMainWindow:oSayNote1:Hide()
      oMainWindow:Move( ,, oMainWindow:nWidth, MAINWND_H2 )

      oPaneBtn:Move( ,MAINWND_H2-36 )
#ifdef __PLATFORM__UNIX
      oPaneNote:Move( ,, oPaneBtn:nWidth, oPaneBtn:nTop-200-oPaneNote:nTop )
      nSec := Seconds()
      DO WHILE Seconds() - nSec < 0.05
         hwg_ProcessMessage()
         hwg_Sleep( 1 )
      ENDDO
      oPaneNote:Move( ,, oPaneBtn:nWidth, oPaneBtn:nTop-6-oPaneNote:nTop )
      hwg_ProcessMessage()
      oPaneScore:Move( ,,, oPaneNote:nHeight-8 )
      hwg_ProcessMessage()
#endif
      oPaneNote:Move( ,, oPaneBtn:nWidth, oPaneBtn:nTop-4-oPaneNote:nTop )
      oPaneScore:Move( ,,, oPaneNote:nHeight-8 )
      oScore:nLines := 1
      DO WHILE y1 + SCORE_Y_LIM <= oPaneScore:nHeight
         y1 += SCORE_Y_DELTA
         oScore:nLines ++
      ENDDO
      nStart := oScore:nCurrPage := 1
      DO WHILE ( nSec := ( nStart + Iif( nStart == 1, oScore:nScrKol1 + oScore:nScrKol2*(oScore:nLines-1), ;
            oScore:nScrKol2*oScore:nLines ) ) ) < oScore:nCurr
         nStart := nSec
         oScore:nCurrPage ++
      ENDDO
      oScore:nScrStart := nStart
      lScoreLong := .T.
   ENDIF
   SetPaneBtn()

   RETURN Nil

STATIC FUNCTION ScorePage( n, lNoChangeCurr )

   LOCAL nLen := Len( oScore:aNotes ), nPage := 1, nStart := 1
   LOCAL nPages := Int( ( nLen + (oScore:nScrKol2-oScore:nScrKol1) ) / ( oScore:nScrKol2*oScore:nLines ) ) + 1

   //hwg_writelog( ltrim(str(nLen)) + " " + ltrim(str(nPages)) + " " + ltrim(str(oScore:nLines)) )
   IF ( n > 0 .AND. oScore:nCurrPage < nPages ) .OR. ( n < 0 .AND. oScore:nCurrPage > 1 )
      IF n > 0
         oScore:nCurrPage ++
         oPaneBtn:oBtnPrevPage:Enable()
      ELSE
         oScore:nCurrPage --
         IF oScore:nCurrPage == 1
            oPaneBtn:oBtnPrevPage:Disable()
         ENDIF
      ENDIF
      DO WHILE nPage < oScore:nCurrPage
         nStart += Iif( nPage == 1, oScore:nScrKol1 + oScore:nScrKol2*(oScore:nLines-1), ;
            oScore:nScrKol2*oScore:nLines )
         nPage ++
      ENDDO
      oScore:nScrStart := nStart
      IF nCurrMode == 1 .AND. Empty( lNoChangeCurr )
         oScore:nCurr := nStart
      ENDIF
      hwg_Redrawwindow( oPaneScore:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION Options()

   LOCAL oDlg, oPanel, oLenta1, oLenta2
   LOCAL nLang := Ascan( aLangs, cLanguage ) , nz := nZoom, lUpd := .F.
   LOCAL arr1 := Array( Len( aLangs ) ), i

   FOR i := 1 TO Len( arr1 )
      arr1[i] := hwg_Left( aLangs[i], 3 )
   NEXT

   INIT DIALOG oDlg TITLE "Options" ;
      AT Int(oMainWindow:nWidth*0.7), Int(oMainWindow:nHeight*0.6) SIZE 400, 280 ;
      FONT oFontWnd BACKCOLOR pClr["dlgback"] STYLE WND_NOTITLE + WND_NOSIZEBOX

   oDlg:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[4] COORS 20 BTN_CLOSE

   @ 20, 50 SAY aMsgs[10] SIZE 160, 22 COLOR CLR_WHITE TRANSPARENT
   //@ 180, 50 GET COMBOBOX oGet1 VAR nLang ITEMS aLangs SIZE 140, 28 BACKCOLOR pClr["clr5"]
   oLenta1 := HLenta():New( ,, 140, 48, Min( 180,60*Len(aLangs) ), 28, oFontWnd,,, ;
      {|o|nLang:=o:nSelected},,, arr1, 60, aStyleLenta )
   oLenta1:Value := nLang

   @ 20, 90 SAY aMsgs[12] SIZE 100, 22 COLOR CLR_WHITE TRANSPARENT

   oLenta2 := HLenta():New( ,, 140, 88, 150, 28, oFontWnd,,, {|o|nz:=o:nSelected},,, ;
      { "1","2","3" }, 50, aStyleLenta )
   oLenta2:Value := nz

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 50, 240 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[13] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| oDlg:lResult:=.T.,hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := aStyleBtn

   @ 250, 240 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[14] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := aStyleBtn

   ACTIVATE DIALOG oDlg

   IF oDlg:lResult
      IF !(aLangs[nLang] == cLanguage)
         cLangNew := aLangs[nLang]
         lUpd := .T.
      ENDIF
      IF nz != nZoom
         nZoomNew := nz
         lUpd := .T.
      ENDIF
      IF lUpd
         oMsg:MsgInfo( aMsgs[91] )
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION TestAnalyse()

   LOCAL oDlg, oPanel
   LOCAL nz1 := 1, nz2 := 1, nRecs := 10

   INIT DIALOG oDlg TITLE "111" ;
      AT 200, 80 SIZE 400, 340 FONT oFontWnd BACKCOLOR pClr["dlgback"] STYLE WND_NOTITLE
   oDlg:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT 32 TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[46] COORS 20 BTN_CLOSE BTN_MAXIMIZE BTN_MINIMIZE

   GET RADIOGROUP nz1
   @ 30,50 RADIOBUTTON "Current session" SIZE 180, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   @ 30,74 RADIOBUTTON "By days" SIZE 180, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   @ 30,98 RADIOBUTTON "By weeks" SIZE 180, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   @ 30,122 RADIOBUTTON "By months" SIZE 180, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   @ 30,146 RADIOBUTTON "All" SIZE 180, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   END RADIOGROUP

   @ 20, 176 SAY "" SIZE oDlg:nWidth-40, 2

   GET RADIOGROUP nz2
   @ 50,188 RADIOBUTTON "Current test" SIZE 240, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   @ 50,212 RADIOBUTTON "Total" SIZE 240, 22 COLOR CLR_WHITE BACKCOLOR pClr["dlgback"]
#ifdef __PLATFORM__UNIX
   hwg_Setfgcolor( ATail(oDlg:aControls):handle, CLR_WHITE, 1 )
#endif
   END RADIOGROUP

   @ 220, 74 SAY "Last" SIZE 60, 22 COLOR CLR_WHITE TRANSPARENT
   @ 280, 74 GET nRecs SIZE 50, 24 PICTURE "999"

   @ 20, 268 SAY "" SIZE oDlg:nWidth-40, 2

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 50, 300 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[47] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| ShowReport( nz1, nz2, nRecs ) }
   ATail(oDlg:aControls):aStyle := aStyleBtn

   @ 250, 300 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := aStyleBtn
   //HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver ;

   ACTIVATE DIALOG oDlg

   RETURN NIL

STATIC FUNCTION ShowReport( n1, n2, nRecs )

   LOCAL oDlg, oPanelH, oPanelT, oEdit
   LOCAL arr, i, j, cText := "<hwge>", arr1
   LOCAL s, nCorrect, nCorrect2, nTotal

   IF nRecs == 0
      nRecs := 9999
   ENDIF
   cText += '<div class="h3">Tests results</div><div></div>'
   IF n1 == 1
      arr1 := TestCalc( aTests, "", @nCorrect, @nCorrect2, @nTotal )
      cText += "<div>Total tests number: " + Ltrim(Str(nTotal)) + "</div>"
      cText += "<div>Correct results: " + Ltrim(Str(nCorrect)) + " (" + ;
         Ltrim(Str(Int(nCorrect*100/nTotal))) + "%)</div>"
      IF n2 == 2 .AND. !Empty( arr1 )
         cText += '<div></div><table width="100%"><col width="46%"><col width="18%"><col width="18%"><col width="18%">'
         cText += AddRowToTable( "Test", "Total", "Success", "%" )
         FOR i := 1 TO Len( arr1 )
            cText += AddRowToTable( test2string( arr1[i,1] ), ;
               arr1[i,2], arr1[i,3], Int(arr1[i,3]*100/arr1[i,2]) )
         NEXT
         cText += '</table>'
      ENDIF
   ELSE
      arr := GetHistory( n1 - 1 )
      cText += '<div></div><table width="100%"><col width="46%"><col width="18%"><col width="18%"><col width="18%">'
      cText += AddRowToTable( "Date/TestTest", "Total", "Success", "%" )
      s := Iif( n2 == 1, "t" + Ltrim(Str(nTestMode)) + ;
         "l" + Ltrim(Str(nTestLevel)) + "i" + aOggPaths[nCurrInstr,2], "" )
      FOR i := Len( arr ) TO 1 STEP -1
         arr1 := TestCalc( arr[i,2], s, @nCorrect, @nCorrect2, @nTotal )
         IF nTotal > 0
            cText += AddRowToTable( Left(arr[i,1],4) + '-' + Substr(arr[i,1],5,2) + ;
               '-' + Substr(arr[i,1],7,2), nTotal, nCorrect, Int(nCorrect*100/nTotal) )
         ENDIF
         IF n2 == 2
            FOR j := 1 TO Len( arr1 )
               cText += AddRowToTable( test2string( arr1[j,1] ), arr1[j,2], ;
                  arr1[j,3], Int(arr1[j,3]*100/arr1[j,2]) )
            NEXT
         ENDIF
         IF --nRecs == 0
            EXIT
         ENDIF
      NEXT
      cText += '</table>'
   ENDIF

   cText += "<div></div></hwge>"
   //hb_Memowrit( "aa.hwge", cText )

   INIT DIALOG oDlg TITLE "111" ;
      AT 20, 20 SIZE 600, 400 FONT oFontWnd BACKCOLOR pClr["dlgback"] STYLE WND_NOTITLE
   //oDlg:oParent := oMainWindow

   ADD HEADER PANEL oPanelH HEIGHT 32 TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[48] COORS 20 BTN_CLOSE BTN_MAXIMIZE BTN_MINIMIZE

   @ 0, 32 PANEL oPanelT SIZE 600, 32 HSTYLE oStyleDarkNormal ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   @ 0, 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT "+" COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| ZoomRep(oEdit,2) }

   @ (64+nZoom*8), 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT "-" COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| ZoomRep(oEdit,-2) }

   @ (64+nZoom*8)*2, 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[52] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| SaveRepo( oEdit ) }

   @ oPanelT:nWidth-(64+nZoom*8), 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[11] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| hwg_EndDialog() }

   oEdit := HCEdiExt():New( ,,, 0, 64, oDlg:nWidth, oDlg:nHeight-64, ;
         oFontWnd,, {|o,x,y|o:Move( ,,x,y-64 ) } )
   IF hwg__isUnicode()
      oEdit:lUtf8 := .T.
   ENDIF
   oEdit:lReadOnly := .T.
   oEdit:bColorCur := oEdit:bColor
   oEdit:AddClass( "url", "color: #000080;" )
   oEdit:AddClass( "h1", "font-size: 140%; font-weight: bold;" )
   oEdit:AddClass( "h2", "font-size: 130%; font-weight: bold;" )
   oEdit:AddClass( "h3", "font-size: 120%; font-weight: bold;" )
   oEdit:AddClass( "h4", "font-size: 110%; font-weight: bold;" )
   oEdit:AddClass( "h5", "font-weight: bold;" )
   oEdit:AddClass( "i", "font-style: italic;" )
   oEdit:AddClass( "cite", "color: #007800; margin-left: 3%; margin-right: 3%;" )
   oEdit:aDefClasses := { "url","h1","h2","h3","h4","h5","i","cite" }

   oEdit:SetText( cText, "UTF8","UTF8" )

   oPanelH:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   ACTIVATE DIALOG oDlg

   RETURN NIL

STATIC FUNCTION SaveRepo( oEdit )

   LOCAL cFile
#ifdef __PLATFORM__UNIX
   IF Empty( cFile := hwg_Selectfile( "( *.html )", "*.html", hb_DirBase() ) )
#else
   IF Empty( cFile := hwg_Savefile( "*.html", "( *.html )", "*.html", hb_DirBase() ) )
#endif
      RETURN Nil
   ENDIF

   IF Empty( hb_fnameExt( cFile ) )
      cFile := hb_fnameExtSet( cFile, "html" )
   ENDIF
   oEdit:Save( cFile,, .T. )

   RETURN NIL

STATIC FUNCTION AddRowToTable( ... )

   LOCAL ap := hb_aParams(), i, s := "<tr>"

   FOR i := 1 TO Len( ap )
      s += "<td><div>" + Iif( Valtype( ap[i] ) == "C", ap[i], Ltrim(Str(ap[i])) ) + "</div></td>"
   NEXT

   RETURN s + "</tr>"

STATIC FUNCTION test2string( s )

   LOCAL c

   RETURN " test " + Substr(s,2,1) + " level " + ;
            Substr(s,4,1) + ' ' + Iif((c := Substr(s,6,1))=='p', 'piano', ;
            Iif( c=='v', 'violin', Iif( c=='r', 'recorder', '' )))

STATIC FUNCTION Zoomrep( oEdit, n )

   LOCAL nHeight := oEdit:oFont:height

   nHeight := Iif( nHeight<0, nHeight-n, nHeight+n )
   oEdit:SetFont( HFont():Add( oEdit:oFont:name, oEdit:oFont:Width,nHeight,,oEdit:oFont:Charset,,,,,.T. ) )
   RETURN .T.

STATIC FUNCTION GetHistory( nType )

   LOCAL aHis := hb_aTokens( Memoread( cTestHis ), Chr(10) )
   LOCAL cDate, aRes := {}, arrCurr
   LOCAL i, j, j1, nPos, nPos2, aLine, arr

   FOR i := 1 TO Len( aHis )
      IF ( nPos := At( '{', aHis[i] ) ) > 0 .AND. ( nPos := hb_At( '/', aHis[i], nPos ) ) > 0 ;
         .AND. ( nPos2 := hb_At( '}', aHis[i], nPos ) ) > 0
         IF nType == 1
            cDate := Left( aHis[i], 8 )
         ELSEIF nType == 2
            cDate := Dtos( Stod(cDate := Left(aHis[i],8)) - (Dow(cDate)-2) )
         ELSEIF nType == 3
            cDate := Left( aHis[i], 6 )
         ELSE
            cDate := Dtos( Date() )
         ENDIF
         IF Len( aRes ) == 0 .OR. cDate != ATail(aRes)[1]
            AAdd( aRes, arrCurr := { cDate, {} } )
         ENDIF
         aLine := hb_ATokens( Substr( aHis[i], nPos + 1, nPos2 - nPos ), '/' )
         FOR j := 1 TO Len( aLine )
            AAdd( arrCurr[2], arr := hb_aTokens( aLine[j] ) )
            FOR j1 := 2 TO Len( arr )
               arr[j1] := Val( arr[j1] )
            NEXT
         NEXT
      ENDIF
   NEXT

   RETURN aRes

STATIC FUNCTION PlayAccord( arr )

   LOCAL i

   FOR i := 1 TO Len( arr )
      IF ( arr[i] := Text2Note( arr[i] ) ) == 0 .OR. !LoadNote( arr[i] )
         oMsg:MsgStop( "Wrong note in accord" )
         RETURN .F.
      ENDIF
   NEXT

   StopSound( arr[1] )
   sf_SetAccord( aSounds[arr[1],1], Iif( Len(arr)>1,aSounds[arr[2],1],Nil ), ;
      Iif( Len(arr)>2,aSounds[arr[3],1],Nil ), Iif( Len(arr)>3,aSounds[arr[4],1],Nil ), ;
      Iif( Len(arr)>4,aSounds[arr[5],1],Nil ), Nil, nDelayAcc )
   Play( arr[1] )

   RETURN .T.

STATIC FUNCTION Help()

   LOCAL oDlg, oPanelH, oPanelT, oEdit
   LOCAL cText

   INIT DIALOG oDlg TITLE "112" ;
      AT 20, 20 SIZE 640, 440 FONT oFontWnd BACKCOLOR pClr["dlgback"] STYLE WND_NOTITLE
   oDlg:oParent := oMainWindow

   ADD HEADER PANEL oPanelH HEIGHT 32 TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[5] COORS 20 BTN_CLOSE BTN_MAXIMIZE BTN_MINIMIZE

   @ 0, 32 PANEL oPanelT SIZE 640, 32 HSTYLE oStyleDarkNormal ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   @ 0, 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT "+" COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| ZoomRep(oEdit,2) }

   @ (64+nZoom*8), 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT "-" COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| ZoomRep(oEdit,-2) }

   @ oPanelT:nWidth-(64+nZoom*8), 0 OWNERBUTTON OF oPanelT SIZE (64+nZoom*8), 32 ;
      HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver TEXT aMsgs[11] COLOR CLR_WHITE ;
      FONT oFontMenu ON CLICK {|| hwg_EndDialog() }

   oEdit := HCEdiExt():New( ,,, 0, 64, oDlg:nWidth, oDlg:nHeight-64, ;
         oFontWnd,, {|o,x,y|o:Move( ,,x,y-64 ) } )
   IF hwg__isUnicode()
      oEdit:lUtf8 := .T.
   ENDIF
   oEdit:lReadOnly := .T.
   oEdit:bColorCur := oEdit:bColor
   oEdit:AddClass( "url", "color: #000080;" )
   oEdit:AddClass( "h1", "font-size: 140%; font-weight: bold;" )
   oEdit:AddClass( "h2", "font-size: 130%; font-weight: bold;" )
   oEdit:AddClass( "h3", "font-size: 120%; font-weight: bold;" )
   oEdit:AddClass( "h4", "font-size: 110%; font-weight: bold;" )
   oEdit:AddClass( "h5", "font-weight: bold;" )
   oEdit:AddClass( "i", "font-style: italic;" )
   oEdit:AddClass( "cite", "color: #007800; margin-left: 3%; margin-right: 3%;" )
   oEdit:aDefClasses := { "url","h1","h2","h3","h4","h5","i","cite" }
   oEdit:bOther := { |o, m, wp, lp|HlpMessProc( o, m, wp, lp ) }

   oEdit:SetText( cText, "UTF8","UTF8" )

   oPanelH:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   ACTIVATE DIALOG oDlg ON ACTIVATE {|| oEdit:Open( hb_DirBase() + "sounds_"+cSuffix+".hwge" ) }

   RETURN NIL

#define OB_HREF         6

STATIC FUNCTION HlpMessProc( o, msg, wParam, lParam )

   LOCAL arr, arrf

   IF msg == WM_LBUTTONDBLCLK
      IF !Empty( arr := o:GetPosInfo( hwg_LoWord(lParam ), hwg_HiWord(lParam ) ) ) .AND. ;
            !Empty( arr[3] ) .AND. Len( arr[3] ) >= OB_HREF
         hwg_SetCursor( handCursor )
         IF Lower( Left( arr[3,OB_HREF], 8 ) ) == "goto://#"
            IF !Empty( arrf := o:Find( ,Substr( arr[3,OB_HREF],9 ) ) )
               o:Goto( arrf[2] )
            ENDIF
         ENDIF
      ENDIF
      HB_SYMBOL_UNUSED( wParam )
      RETURN 0

   ELSEIF msg == WM_MOUSEMOVE .OR. msg == WM_LBUTTONDOWN
      IF !Empty( arr := o:GetPosInfo( hwg_LoWord(lParam ), hwg_HiWord(lParam ) ) ) .AND. ;
            !Empty( arr[3] ) .AND. Len( arr[3] ) >= OB_HREF
         hwg_SetCursor( handCursor )
      ENDIF

   ENDIF

   RETURN - 1

STATIC FUNCTION About()

   LOCAL oDlg, oPanel, cText, nPos

   INIT DIALOG oDlg TITLE "About" BACKCOLOR pClr["dlgback"] ;
      AT Int(oMainWindow:nWidth*0.7), Int(oMainWindow:nHeight*0.6) SIZE 400, 340 FONT oFontWnd STYLE WND_NOTITLE + WND_NOSIZEBOX

   oDlg:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      FONT oFontHea TEXT aMsgs[87] COORS 20 BTN_CLOSE

   @ 20, 40 SAY aMsgs[1] SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT
   @ 20, 64 SAY "Version " + APP_VERSION SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT
   @ 20, 100 SAY "Copyright 2021 Alexander S.Kresin" SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT
   @ 20, 124 SAY "http://www.kresin.ru" LINK "http://www.kresin.ru" SIZE 360, 24 STYLE SS_CENTER
   @ 20, 160 LINE LENGTH 360
   @ 20, 180 SAY hwg_version() SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT
   cText := pa_getversiontext()
   IF ( nPos := At( ',', cText ) ) > 0
      cText := Left( cText, nPos - 1 )
   ENDIF
   @ 20, 204 SAY cText SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT
   @ 20, 228 SAY sf_GetVersion() SIZE 360, 24 STYLE SS_CENTER COLOR pClr["clr5"] TRANSPARENT

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 140, 300 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := aStyleBtn
   //HSTYLES oStyleDarkNormal, oStyleDarkPressed, oStyleDarkOver ;

   ACTIVATE DIALOG oDlg

   RETURN NIL

FUNCTION noteSetAttr( arr, cAttr )

   cAttr += '/'
   IF Len( arr ) == 2
      AAdd( arr, cAttr )
   ELSEIF cAttr $ arr[3]
      arr[3] := StrTran( arr[3], cAttr, "" )
   ELSE
      arr[3] += cAttr
   ENDIF

   RETURN Nil

FUNCTION noteChangeAttr( arr, cAttrOld, cAttrNew )

   cAttrOld += '/'
   cAttrNew += '/'
   IF Len( arr ) > 2 .AND. cAttrOld $ arr[3]
      arr[3] := StrTran( arr[3], cAttrOld, cAttrNew )
   ENDIF

   RETURN Nil

FUNCTION noteCheckAttr( arr, cAttr )

   IF Len( arr ) > 2
      RETURN (cAttr + '/') $ arr[3]
   ENDIF

   RETURN .F.

CLASS Score

   DATA cTitle      INIT ""
   DATA aNotes      INIT {}
   DATA nSeleStart  INIT 0
   DATA nSeleEnd    INIT 0
   DATA nCurr       INIT 0
   DATA nScrStart   INIT 1
   DATA nScrKol1    INIT 0
   DATA nScrKol2    INIT 0
   DATA aMetre      INIT { 4,4 }
   DATA nBPM        INIT 120
   DATA nKey        INIT 0
   DATA lBas        INIT .F.
   DATA nLines      INIT 1
   DATA nCurrPage   INIT 1
   DATA lUpdate     INIT .F.

   DATA x1          INIT 10
   DATA nBetween   INIT 3

   METHOD New()
   METHOD Clean()
   METHOD CheckBas()
   METHOD SetScrKol( oSheet )
ENDCLASS

METHOD New() CLASS Score

   RETURN Self

METHOD Clean() CLASS Score

   ::cTitle := ""
   ::aNotes := {}
   ::nBPM := 120
   ::nSeleStart := ::nSeleEnd := ::nCurr := ::nScrKol1 := 0
   ::nScrStart := 1
   ::aMetre[1] := ::aMetre[2] := 4
   ::nKey := 0
   ::nCurrPage := 1
   ::lUpdate := .F.

   RETURN NIL

METHOD CheckBas() CLASS Score

   LOCAL i, n, nBas := 0

   FOR i := 1 TO Len( ::aNotes )
      n := Iif( Valtype(::aNotes[i,1]) == "A", ::aNotes[i,1,1], ::aNotes[i,1] )
      IF n > 0
         IF Int( ( n - 1 ) / 12 ) + 1 >= 4
            nBas ++
         ELSE
            nBas --
         ENDIF
      ENDIF
   NEXT
   ::lBas := ( nBas < 0 )

   RETURN Nil

METHOD SetScrKol( oSheet ) CLASS Score

   LOCAL x1 := ::x1 + oClef1:nWidth + 8
   LOCAL nWidth := oBemol:nWidth + oNote8:nWidth + ::nBetween * 2

   ::nScrKol2 := Int( ( oSheet:nWidth - x1 - 16 ) / nWidth )

   IF ::nKey != 0
      x1 += Abs( ::nKey ) * Iif( ::nKey<0, oBemol:nWidth, oDiez:nWidth )
   ENDIF
   IF ::aMetre[1] > 0
      x1 += 20
   ENDIF

   ::nScrKol1 := Int( ( oSheet:nWidth - x1 - 16 ) / nWidth )

   RETURN Nil
