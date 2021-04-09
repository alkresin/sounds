/*
 *
 */

#include "hwgui.ch"
#ifdef __PLATFORM__UNIX
   #include "gtk.ch"
#endif
#include "inkey.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_RED      0x0000ff
#define CLR_BROWN_1  0x154780
#define CLR_BROWN_2  0x6a9cd4
#define CLR_BROWN_3  0xaad2ff
#define CLR_BROWN_4  0x396eaa
#define CLR_BROWN_5  0x9dc7f6
#define CLR_LIGHTGRAY_1 0xdddddd
#define CLR_LIGHTGRAY_2 0xaaaaaa
#define CLR_DARKGRAY_1  0x333333
#define CLR_DARKGRAY_2  0x666666
#define CLR_TOPDARK 0x7b7680
#define CLR_TOPMID  0x5b5760
#define CLR_DLGBACK 0x154780
#define CLR_DLGHEA  0x2F343F

STATIC oBmpCheck

FUNCTION FileMenu( x1, y1, nWidth, nHeight, aChoices, aFuncs )

   LOCAL oMainWindow := HWindow():GetMain()
   LOCAL oDlg1, oBrw, lCapture := .F., i, lMulti := .F., lbline := .F., nRes := 0
   LOCAL bEnter := {||
      oDlg1:Close()
      IF aFuncs == Nil
         nRes := oBrw:nCurrent
      ELSE
         Eval( aFuncs[oBrw:nCurrent] )
      ENDIF
      Return Nil
   }
   LOCAL bKeyDown := {|o,key|
      IF key == VK_ESCAPE
         HB_SYMBOL_UNUSED(o)
         oDlg1:Close()
      ENDIF
      Return .T.
   }
   LOCAL bLostF := {||
      oDlg1:Close()
      Return .T.
   }
   LOCAL bOther := {|o,msg,wp,lp|
      LOCAL ym, xm, h, w, hl, y1
      IF msg == WM_MOUSEMOVE
         IF !lCapture
            hwg_SetCapture(oBrw:handle)
            lCapture := .T.
         ENDIF
         ym := hwg_Hiword( lp )
         xm := hwg_Loword( lp )
         IF ym < 0 .OR. ym > oBrw:nHeight+1 .OR. xm < 0 .OR. xm > oBrw:nWidth+1
         ELSE
            ym := Iif( ym < oBrw:y1, 0, Int( (ym - oBrw:y1 ) / (oBrw:height + 1 ) ) + 1 )
            IF ym > Len( aChoices )
               ym := Len( aChoices )
            ENDIF
            oBrw:rowPos := oBrw:nCurrent := ym
            oBrw:Refresh()
         ENDIF
      ELSEIF msg == WM_LBUTTONDOWN
         h := oBrw:nHeight
         w := oBrw:nWidth
         hl := oBrw:height
         y1 := oBrw:y1
         ym := hwg_Hiword( lp )
         xm := hwg_Loword( lp )
         oDlg1:Close()
         IF ym < 0 .OR. ym > h+1 .OR. xm < 0 .OR. xm > w+1
         ELSE
            ym := Iif( ym < y1, 0, Int( (ym - y1 ) / (hl + 1 ) ) + 1 )
            IF ym > Len( aChoices )
               ym := Len( aChoices )
            ENDIF
            IF aFuncs == Nil
               nRes := ym
            ELSE
               Eval( aFuncs[ym] )
            ENDIF
         ENDIF
         HB_SYMBOL_UNUSED(o)
         HB_SYMBOL_UNUSED(wp)
      ENDIF
      Return .T.
   }
   LOCAL bLine := {|o,h,x1,y1,x2,y2|
      LOCAL cText := Iif( lMulti, oBrw:aArray[oBrw:nCurrent,2], oBrw:aArray[oBrw:nCurrent] ), nPos
      LOCAL oBrush := Iif( oBrw:nPaintRow==oBrw:rowPos, oBrw:brushSel, oBrw:brush )
      hwg_Fillrect( h, x1, y1, x2, y2, oBrush:handle )
      hwg_Settransparentmode( h, .T. )
      IF ( nPos := At( ',', cText ) ) > 0
         hwg_Drawtext( h, Left(cText,nPos-1), x1+2, y1+2, x2-2, y2-2, DT_LEFT )
         hwg_Drawtext( h, Substr(cText,nPos+1), x1+2, y1+2, x2-2, y2-2, DT_RIGHT )
      ELSE
         hwg_Drawtext( h, cText, x1+2, y1+2, x2-4, y2-2, DT_LEFT )
      ENDIF
      hwg_Settransparentmode( h, .F. )
      HB_SYMBOL_UNUSED(o)
      Return .T.
   }

   INIT DIALOG oDlg1 TITLE "" AT x1, y1 SIZE nWidth, nHeight STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {||hwg_Releasecapture()}
   oDlg1:oParent := oMainWindow

   @ 0, 0 BROWSE oBrw ARRAY OF oDlg1 SIZE nWidth, nHeight FONT oMainWindow:oFont NO VSCROLL

   oBrw:aArray := aChoices
   IF Valtype( aChoices[1] ) == "A"
      FOR i := 1 TO Len( aChoices )
         IF ',' $ aChoices[i,2]
            lbline := .T.
            EXIT
         ENDIF
      NEXT
      IF Empty( oBmpCheck )
#ifdef __PLATFORM__UNIX
         oBmpCheck := HBitmap():AddStandard( "gtk-apply" )
#else
         oBmpCheck := HBitmap():AddStandard( OBM_CHECK )
#endif
      ENDIF
      oBrw:AddColumn( HColumn():New( ,{ ||oBrw:aArray[oBrw:nCurrent,1] },"L",1 ) )
      oBrw:AddColumn( HColumn():New( ,{ ||oBrw:aArray[oBrw:nCurrent,2] },"C",8 ) )
      oBrw:aColumns[1]:aBitmaps := { { {|l|l}, oBmpCheck } }
      IF lbline
         oBrw:aColumns[2]:setPaintCB( PAINT_LINE_ALL, bLine )
      ENDIF
      lMulti := .T.
   ELSE
      FOR i := 1 TO Len( aChoices )
         IF ',' $ aChoices[i]
            lbline := .T.
            EXIT
         ENDIF
      NEXT
      oBrw:AddColumn( HColumn():New( ,{ ||oBrw:aArray[oBrw:nCurrent] },"C",8 ) )
      IF lbline
         oBrw:aColumns[1]:setPaintCB( PAINT_LINE_ALL, bLine )
      ENDIF
   ENDIF
   oBrw:lDispHead := .F.
   oBrw:bcolorSel := oBrw:htbColor := CLR_TOPMID
   oBrw:bColor := oBrw:sepColor := CLR_TOPDARK
   oBrw:tcolorSel := oBrw:httColor := oBrw:tcolor := CLR_WHITE

   oBrw:bEnter := bEnter
   oBrw:bKeyDown := bKeyDown
   oBrw:bOther := bOther
   oBrw:bLostFocus := bLostF
   oBrw:lInFocus := .T.

#ifdef __PLATFORM__UNIX
   ACTIVATE DIALOG oDlg1
#else
   ACTIVATE DIALOG oDlg1 ON ACTIVATE {||hwg_SetCursorPos( oMainWindow:nLeft+x1+24, oMainWindow:nTop+y1+12 )}
#endif

   RETURN nRes

FUNCTION _IniRead( cFileName )

   LOCAL cText := Memoread( cFileName ), aText, i, s, nPos
   LOCAL hIni, hSect

   IF Empty( cText )
      RETURN Nil
   ENDIF

   aText := hb_aTokens( cText, Chr(10) )
   hIni := hb_Hash()

   FOR i := 1 TO Len( aText )
      s := Iif( Left( aText[i],1 ) == ' ', Ltrim( aText[i] ), aText[i] )
      IF Left( s, 1 ) $ ";#"
         LOOP
      ENDIF
      s := Trim( Iif( Right(s,1)==Chr(13), Left( s,Len(s)-1 ), s ) )
      IF Empty( s )
         LOOP
      ENDIF

      IF Left( s,1 ) == '[' .AND. Right( s,1 ) == ']'
         hSect := hIni[Substr( s,2,Len(s)-2 )] := hb_Hash()
      ELSE
         IF ( nPos := At( '=', s ) ) > 0
            hSect[Trim(Left(s,nPos-1))] := Ltrim( Substr( s,nPos+1 ) )
         ENDIF
      ENDIF
   NEXT

   RETURN hIni

