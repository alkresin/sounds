/*
 * A Guitar plugin for Sounds.
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hwgui.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_LIGHTGRAY_2 0xaaaaaa

#define TOPPANE_HEIGHT  28

#define LAD_KOL   4
#define DELTA_X   30

STATIC bPlugNote_Orig, oDlgGuitar, oPaneGuitar, oEdHlp
STATIC oPen1, oPen1W, oPen2, oFontNote, oBrushWhite, oBrushBlack, aGradient
STATIC cPlugDir
STATIC nCurrNote := 0, nCurrMode := 1, nAccSele := 0, nTabStart := 1, nTabEnd := 1, nTabPosX := 1, nTabPosY := 0
STATIC pAccords, aAccords, aCurrAcc := {}, aAcco1, aAcco2
STATIC aStrings := { {"E",53}, {"B",48}, {"G",44}, {"D",39}, {"A",34}, {"E",29} }
STATIC arrSounds[6]
STATIC cSoundsPath := "Sounds_guitar_ogg"
STATIC oScore
STATIC lAutoRun := .F., lSetGuitar := .F., nDefMode := 1, lOpenLast := .F., cLastTab

MEMVAR oMsg, pClr, aPlugMenu, bPlugNote, nCurrVol, nDelayAcc

FUNCTION Plug_guitar( lInit, oSc )

   LOCAL i, j, s

   IF !lInit
      RETURN Nil
   ENDIF

   cPlugDir := hb_DirBase() + "plugins" + hb_ps()
   oScore := oSc

   nCurrMode := nDefMode
   Guitar_ReadIni()
   aAcco1 := {}
   aAcco2 := {}
   FOR i := 1 TO Len( aAccords )
      j := Iif( Substr(aAccords[i],2,1) == '#', 2, 1 )
      s := Left( aAccords[i], j )
      IF hb_Ascan( aAcco1, s, .T. ) == 0
         AAdd( aAcco1, s )
      ENDIF
      s := Substr( aAccords[i], j+1 )
      IF hb_Ascan( aAcco2, s, .T. ) == 0
         AAdd( aAcco2, s )
      ENDIF
   NEXT

   AAdd( aPlugMenu, { "Guitar", {|l|guitar_Dlg(l)} } )
   IF !Empty( bPlugNote )
      bPlugNote_Orig := bPlugNote
   ENDIF
   bPlugNote := {|n| guitar_Show(n) }
   cSoundsPath += hb_ps()

   IF lAutoRun
      guitar_Dlg( .T. )
   ENDIF

   RETURN Nil

STATIC FUNCTION guitar_Dlg( lOpen )

   LOCAL oMainWindow := HWindow():GetMain(), oPanel, oLentaM, oLenta1, oLenta2, oBtnOpen, oBtnSave, oBtnBack, oBtnForw, h
   LOCAL aStyle := { HStyle():New( { pClr["clr3"], pClr["clr4"] }, 3 ), ;
      HStyle():New( { pClr["clr2"] }, 3,, 1, CLR_LIGHTGRAY_2 ), HStyle():New( { pClr["clr4"] }, 3 ) }
   LOCAL bLClick := {||
      LOCAL s, n
      IF oLenta1:nSelected > 0 .AND. oLenta2:nSelected > 0
         s := aAcco1[oLenta1:nSelected] + aAcco2[oLenta2:nSelected]
         IF ( n := hb_Ascan( aAccords, s,,, .T. ) ) > 0
            Guitar_Acco_Show( n )
         ENDIF
      ENDIF
      RETURN .T.
   }
   LOCAL bMenu := {||
      IF nCurrMode == oLentaM:nSelected
         RETURN .T.
      ENDIF
      IF nCurrMode == 2
         oLenta1:Hide()
         oLenta2:Hide()
      ELSEIF nCurrMode == 3
         oBtnOpen:Hide()
         oBtnSave:Hide()
         oBtnBack:Hide()
         oBtnForw:Hide()
      ELSEIF nCurrMode == 4
         oEdHlp:Hide()
         oPaneGuitar:Show()
      ENDIF
      nCurrMode := oLentaM:nSelected
      IF nCurrMode == 1
         oPaneGuitar:Move( 0,, oDlgGuitar:nWidth )
         hwg_ProcessMessage()
         hwg_ProcessMessage()
         oPaneGuitar:Move( 0,, oDlgGuitar:nWidth-2 )
      ELSEIF nCurrMode == 2
         oPaneGuitar:Move( 70,, oDlgGuitar:nWidth-70 )
         oLenta1:Show()
         oLenta2:Show()
         Eval( bLClick )
      ELSEIF nCurrMode == 3
         oPaneGuitar:Move( 0,, oDlgGuitar:nWidth )
         hwg_ProcessMessage()
         hwg_ProcessMessage()
         oPaneGuitar:Move( 0,, oDlgGuitar:nWidth-2 )
         oBtnOpen:Show()
         oBtnSave:Show()
         oBtnBack:Show()
         oBtnForw:Show()
         IF lOpenLast .AND. !Empty( cLastTab ) .AND. ( Empty( oScore:aNotes ) .OR. !noteCheckAttr( oScore:aNotes[1], "tg" ) )
            guitar_OpenTab( cLastTab )
         ENDIF
      ELSEIF nCurrMode == 4
         oPaneGuitar:Hide()
         IF Empty( oEdHlp )
            oEdHlp := HCEdiExt():New( ,,, 0, TOPPANE_HEIGHT, oDlgGuitar:nWidth, oDlgGuitar:nHeight-TOPPANE_HEIGHT, ;
                  oMainWindow:oFont )
            IF hwg__isUnicode()
               oEdHlp:lUtf8 := .T.
            ENDIF
            oEdHlp:lReadOnly := .T.
            oEdHlp:bColorCur := oEdHlp:bColor
            oEdHlp:AddClass( "h1", "font-size: 140%; font-weight: bold;" )
            oEdHlp:AddClass( "h2", "font-size: 130%; font-weight: bold;" )
            oEdHlp:AddClass( "h3", "font-size: 120%; font-weight: bold;" )
            oEdHlp:AddClass( "h4", "font-size: 110%; font-weight: bold;" )
            oEdHlp:AddClass( "h5", "font-weight: bold;" )
            oEdHlp:AddClass( "i", "font-style: italic;" )
            oEdHlp:aDefClasses := { "h1","h2","h3","h4","h5","i","cite" }
            oEdHlp:SetText( MemoRead( cPlugDir+"plug_guitar.hwge" ), "UTF8","UTF8" )
         ELSE
            oEdHlp:Show()
         ENDIF
      ELSEIF nCurrMode == 5
         nCurrMode := nDefMode
         oDlgGuitar:Close()
         RETURN .T.
      ENDIF
      oPaneGuitar:Refresh()
      RETURN .T.
   }
   LOCAL bActivate := {||
      oLenta1:Hide(); oLenta2:Hide()
      oBtnOpen:Hide(); oBtnSave:Hide(); oBtnBack:Hide(); oBtnForw:Hide()
      Eval( bMenu )
      RETURN .T.
  }

   IF lOpen != Nil .AND. !lOpen
      RETURN oDlgGuitar
   ENDIF
   IF !Empty( oDlgGuitar )
      RETURN Nil
   ENDIF

   IF Empty( oPen1 )
      oPen1 := HPen():Add( PS_SOLID, 1, CLR_BLACK )
      oPen1W := HPen():Add( PS_SOLID, 1, CLR_WHITE )
      oPen2 := HPen():Add( PS_SOLID, 2, CLR_BLACK )
      oBrushWhite := HBrush():Add( pClr["clr5"] ) //CLR_WHITE )
      oBrushBlack := HBrush():Add( CLR_BLACK )
      oFontNote := oMainWindow:oFont:SetFontStyle( ,,,,, Iif( (h := oMainWindow:oFont:height) < 0, h+2, h-2 ) )
      //aGradient := { pClr["topdark"], pClr["topmid"] }
      aGradient := { pClr["clr1"] }
      IF lSetGuitar
         SetInstr( "g" )
      ENDIF
   ENDIF

   nTabStart := nTabEnd := nTabPosX := 1; nTabPosY := 0

   INIT DIALOG oDlgGuitar TITLE "Guitar" BACKCOLOR pClr["dlgback"] ;
      AT 20, oMainWindow:nHeight SIZE 560, 220 FONT oMainWindow:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| DlgGuitarExit() }

   oDlgGuitar:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      TEXT "" COORS 20 //BTN_CLOSE

   oLenta1 := HLenta():New( ,, 0, TOPPANE_HEIGHT, 30, oDlgGuitar:nHeight-TOPPANE_HEIGHT, ;
      oMainWindow:oFont,,, bLClick,,, aAcco1, 30, aStyle )
   oLenta1:Value := 1

   oLenta2 := HLenta():New( ,, oLenta1:nWidth+2, TOPPANE_HEIGHT, 38, oDlgGuitar:nHeight-TOPPANE_HEIGHT, ;
      oMainWindow:oFont,,, bLClick,,, aAcco2, 30, aStyle )
   oLenta2:Value := 1

   oLentaM := HLenta():New( oPanel,, 160, 4, 390, 22, ;
      oFontNote,,, bMenu,,, { "CurrNote","Accords","Tabs","Help","Exit" }, 78, aStyle )
   oLentaM:Value := nCurrMode
   nCurrMode := 1

   @ 0, TOPPANE_HEIGHT PANEL oPaneGuitar SIZE oDlgGuitar:nWidth, oDlgGuitar:nHeight-TOPPANE_HEIGHT ;
      STYLE SS_OWNERDRAW BACKCOLOR pClr["clr3"] ON SIZE {||.t.}
   oPaneGuitar:bPaint := {|| guitar_Paint()}
   oPaneGuitar:bOther := {|o,msg,wp,lp| Guitar_Pane_Other(o,msg,wp,lp)}

   @ 0, oPaneGuitar:nHeight-24 OWNERBUTTON oBtnBack OF oPaneGuitar SIZE 24, 24 TEXT "<" ;
      FONT oFontNote ON CLICK {|o,n| guitar_DragTab(o,n,.F.) }
   oBtnBack:aStyle := aStyle
   oBtnBack:SetTimer( 200 )
   @ 160, oPaneGuitar:nHeight-24 OWNERBUTTON oBtnOpen OF oPaneGuitar SIZE 90, 24 TEXT "Open" ;
      FONT oFontNote ON CLICK {|| guitar_OpenTab() }
   oBtnOpen:aStyle := aStyle
   @ 250, oPaneGuitar:nHeight-24 OWNERBUTTON oBtnSave OF oPaneGuitar SIZE 90, 24 TEXT "Play" ;
      FONT oFontNote ON CLICK {|| guitar_PlayTab() }
   oBtnSave:aStyle := aStyle
   @ oPaneGuitar:nWidth-24, oPaneGuitar:nHeight-24 OWNERBUTTON oBtnForw OF oPaneGuitar SIZE 24, 24 TEXT ">" ;
      FONT oFontNote ON CLICK {|o,n| guitar_DragTab(o,n,.T.) }
   oBtnForw:aStyle := aStyle
   oBtnForw:SetTimer( 200 )

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   SET KEY 0, VK_LEFT TO guitar_SetCursor( VK_LEFT )
   SET KEY 0, VK_RIGHT TO guitar_SetCursor( VK_RIGHT )
   SET KEY 0, VK_UP TO guitar_SetCursor( VK_UP )
   SET KEY 0, VK_DOWN TO guitar_SetCursor( VK_DOWN )
   SET KEY 0, VK_HOME TO guitar_SetCursor( VK_HOME )
   SET KEY 0, VK_END TO guitar_SetCursor( VK_END )

   SET KEY 0, Asc( "1" ) TO guitar_LadInput( 1 )
   SET KEY 0, Asc( "2" ) TO guitar_LadInput( 2 )
   SET KEY 0, Asc( "3" ) TO guitar_LadInput( 3 )
   SET KEY 0, Asc( "4" ) TO guitar_LadInput( 4 )
   SET KEY 0, Asc( "5" ) TO guitar_LadInput( 5 )
   SET KEY 0, Asc( "6" ) TO guitar_LadInput( 6 )
   SET KEY 0, Asc( "7" ) TO guitar_LadInput( 7 )
   SET KEY 0, Asc( "8" ) TO guitar_LadInput( 8 )
   SET KEY 0, Asc( "9" ) TO guitar_LadInput( 9 )
   SET KEY 0, Asc( "0" ) TO guitar_LadInput( 0 )

   ACTIVATE DIALOG oDlgGuitar NOMODAL ON ACTIVATE bActivate

   RETURN Nil

STATIC FUNCTION guitar_Show( n )

   IF !Empty( bPlugNote_Orig )
      Eval( bPlugNote_Orig, n )
   ENDIF
   nCurrNote := Iif( Valtype(n) == "N", n, 0 )
   IF Empty( oDlgGuitar )
      RETURN Nil
   ENDIF

   IF nCurrMode == 1
      oDlgGuitar:oPanel:SetText( Note2Text( nCurrNote, .T. ) )
      oDlgGuitar:oPanel:Refresh()
      oPaneGuitar:Refresh()
   ELSEIF nCurrMode == 3 .AND. oScore:nCurr > 0 .AND. ;
      Len(oScore:aNotes) >= oScore:nCurr .AND. noteCheckAttr( oScore:aNotes[oScore:nCurr], "tg" )
      IF oScore:nCurr < nTabStart .OR. oScore:nCurr > nTabEnd
         nTabStart := oScore:nCurr
      ENDIF
      nTabPosX := oScore:nCurr
      nTabPosY := 0
      oPaneGuitar:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION guitar_Paint()

   LOCAL o := oPaneGuitar
#ifdef __PLATFORM__UNIX
   LOCAL hDC := hwg_Getdc( o:handle )
#else
   LOCAL pps    := hwg_Definepaintstru()
   LOCAL hDC    := hwg_Beginpaint( o:handle, pps )
#endif
   LOCAL x1 := 30, y1 := 20, i, n, nMin, nMax, nLad, nBarre, nBarreLast
   LOCAL aNotes, xc, nTPos, c, nPos

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )

   IF nCurrMode == 1
      hwg_Selectobject( hDC, oPen1:handle )
      hwg_Selectobject( hDC, oDlgGuitar:oFont:handle )
      hwg_SetTransparentMode( hDC, .T. )
      FOR i := 1 TO 6
         hwg_Drawline( hDC, x1, y1+(i-1)*20, o:nWidth-10, y1+(i-1)*20 )
         IF nCurrNote > 0
            IF nCurrNote >= aStrings[i,2] .AND. nCurrNote <= aStrings[i,2] + 12
               n := nCurrNote - aStrings[i,2]
               hwg_Fillrect( hDC, x1+n*20, y1-10+(i-1)*20, x1+n*20+24, y1+12+(i-1)*20, oBrushBlack:handle )
               hwg_Fillrect( hDC, x1+n*20+1, y1-10+(i-1)*20+1, x1+n*20+24-1, y1+12+(i-1)*20-1, oBrushWhite:handle )
               hwg_Drawtext( hDC, Ltrim(Str(n)), x1+n*20+1, y1-8+(i-1)*20, x1+n*20+24-1, y1+10+(i-1)*20, DT_CENTER+DT_VCENTER )
            ELSE
               n := Int( Abs( nCurrNote - aStrings[i,2] ) % 12 )
               IF nCurrNote < aStrings[i,2]
                  n := 12 - n
               ENDIF
               hwg_Fillrect( hDC, x1+n*20, y1-10+(i-1)*20, x1+n*20+24, y1+12+(i-1)*20, oBrushBlack:handle )
               hwg_Fillrect( hDC, x1+n*20+1, y1-10+(i-1)*20+1, x1+n*20+24-1, y1+12+(i-1)*20-1, o:brush:handle )
               hwg_Drawtext( hDC, Ltrim(Str(n)), x1+n*20+6, y1-8+(i-1)*20, x1+n*20+22, y1+10+(i-1)*20, DT_CENTER+DT_VCENTER )
            ENDIF
         ENDIF
      NEXT
      IF nCurrNote > 0
         hwg_Drawtext( hDC, "Click any rectangle to play", 10, o:nHeight-30, o:nWidth-10, o:nHeight-8, DT_CENTER+DT_VCENTER )
      ENDIF
      hwg_Selectobject( hDC, oFontNote:handle )
      FOR i := 1 TO 6
         hwg_Drawtext( hDC, aStrings[i,1], x1-16, y1-8+(i-1)*20, x1, y1+10+(i-1)*20, DT_LEFT )
      NEXT
      hwg_SetTransparentMode( hDC, .F. )
      hwg_Selectobject( hDC, oPen2:handle )
      hwg_Drawline( hDC, x1, y1, x1, y1+100 )

   ELSEIF nCurrMode == 2
      hwg_Selectobject( hDC, oFontNote:handle )
      hwg_SetTransparentMode( hDC, .T. )
      FOR n := 1 TO Len( aCurrAcc )
         //hwg_writelog( "---------" )
         nMin := 99; nMax := 1
         nBarre := 0; nBarreLast := 0
         FOR i := 1 TO Len( aCurrAcc[n] )
            IF Left( aCurrAcc[n,i],1 ) == "B"
               nBarre := Val( Substr(aCurrAcc[n,i],2) )
            ELSE
               nMin := Min( nMin, Iif( (nLad := Val(aCurrAcc[n,i])) == 0, nMin, nLad ) )
               nMax := Max( nMax, nLad )
            ENDIF
         NEXT
         IF nMax <= 4
            nMin := 1
         ENDIF
         //hwg_writelog( "nmin: " + ltrim(str(nmin)) + " nBarre: " + ltrim(str(nBarre)) )
         hwg_Selectobject( hDC, Iif( n == nAccSele, oPen1W:handle, oPen1:handle ) )
         FOR i := 1 TO LAD_KOL
            hwg_Drawtext( hDC, Ltrim(Str(nMin+i-1)), x1-20, y1+i*30-24, x1-4, y1+i*30-6, DT_LEFT )
            hwg_Drawline( hDC, x1, y1+i*30, x1+100, y1+i*30 )
         NEXT
         FOR i := 1 TO 6
            hwg_Drawline( hDC, x1+(i-1)*20, y1, x1+(i-1)*20, y1+120 )
         NEXT
         FOR i := 1 TO 6
            IF i > Len( aCurrAcc[n] ) .OR. aCurrAcc[n,i] > '9'
               hwg_Drawtext( hDC, 'X', x1+(6-i)*20-6, 2, x1+(6-i)*20+8, y1, DT_LEFT )
            ELSE
               IF aCurrAcc[n,i] == '0'
                  hwg_Drawtext( hDC, 'O', x1+(6-i)*20-6, 2, x1+(6-i)*20+8, y1, DT_LEFT )
               ELSE
                  nLad := Val( aCurrAcc[n,i] )
                  IF nBarre == nLad
                     nBarreLast := i
                     //hwg_writelog( "  " + aCurrAcc[n,i] + " " + ltrim(str(i)) )
                  ELSE
                     nLad -= nMin
                     hwg_drawGradient( hDC, x1+(6-i)*20-8, y1+nLad*30+8, x1+(6-i)*20+8, y1+nLad*30+24, 1, ;
                        aGradient,, {8,8,8,8} )
                  ENDIF
               ENDIF
            ENDIF
         NEXT
         IF nBarreLast > 0
            hwg_drawGradient( hDC, x1+(6-nBarreLast)*20-8, y1+(nBarre-nMin)*30+8, ;
               x1+5*20+8, y1+(nBarre-nMin)*30+24, 1, aGradient,, {8,8,8,8} )
         ENDIF
         hwg_Selectobject( hDC, oPen2:handle )
         hwg_Drawline( hDC, x1, y1, x1+100, y1 )
         x1 += 140
      NEXT
      hwg_Selectobject( hDC, oDlgGuitar:oFont:handle )
      hwg_Drawtext( hDC, "Click any accord to play", 10, o:nHeight-30, o:nWidth-10, o:nHeight-8, DT_CENTER+DT_VCENTER )
      hwg_SetTransparentMode( hDC, .F. )

   ELSEIF nCurrMode == 3

      y1 := 30
      hwg_Selectobject( hDC, oPen1:handle )
      hwg_Selectobject( hDC, oDlgGuitar:oFont:handle )
      hwg_SetTransparentMode( hDC, .T. )
      FOR i := 1 TO 6
         hwg_Drawline( hDC, x1, y1+(i-1)*20, o:nWidth-10, y1+(i-1)*20 )
      NEXT
      aNotes := oScore:aNotes
      IF Empty( aNotes ) .OR. noteCheckAttr( aNotes[1], "tg" )
         xc := x1 + 10
         nTPos := nTabStart - 1
         DO WHILE ++nTPos <= Len( aNotes ) .AND. xc < o:nWidth - 10
            IF Len( aNotes[nTPos] ) >= 3 .AND. ( nPos := At( "tg/", aNotes[nTPos,3] ) ) > 0
               IF nTabPosX == nTPos
                  hwg_Fillrect( hDC, xc-4, y1-12+(nTabPosY-1)*20+1, xc+28, y1+14+(nTabPosY-1)*20-1, oBrushWhite:handle )
               ENDIF
               nPos += 2
               FOR i := 1 TO 6
                  IF ( c := Substr( aNotes[nTPos,3], nPos+i, 1 ) ) != 'X'
                     //hwg_writelog( str(nTPos)+" "+str(n)+" "+c )
                     IF c > '9'
                        c := Str( Asc(c)-55,2 )
                     ENDIF
                     hwg_Fillrect( hDC, xc, y1-10+(i-1)*20+1, xc+24, y1+12+(i-1)*20-1, o:brush:handle )
                     hwg_Drawtext( hDC, c, xc, y1-8+(i-1)*20, xc+24, y1+10+(i-1)*20, DT_CENTER+DT_VCENTER )
                  ENDIF
               NEXT
               IF "t/" $ aNotes[nTPos,3]
                  hwg_Drawline( hDC, xc+28, y1, xc+28, y1+100 )
               ENDIF
            ENDIF
            xc += DELTA_X
         ENDDO
         IF nTabPosX == nTPos
            hwg_Fillrect( hDC, xc-4, y1-12+(nTabPosY-1)*20+1, xc+28, y1+14+(nTabPosY-1)*20-1, oBrushWhite:handle )
         ENDIF
         nTabEnd := nTPos - 1
         //hwg_writelog( str(nTabStart)+" "+str(nTabEnd)+" "+str(nTabPosX))
      ENDIF
      IF nTabStart == 1
         hwg_Selectobject( hDC, oFontNote:handle )
         FOR i := 1 TO 6
            hwg_Drawtext( hDC, aStrings[i,1], x1-16, y1-8+(i-1)*20, x1, y1+10+(i-1)*20, DT_LEFT )
         NEXT
         hwg_SetTransparentMode( hDC, .F. )
         hwg_Selectobject( hDC, oPen2:handle )
         hwg_Drawline( hDC, x1, y1, x1, y1+100 )
      ENDIF
   ENDIF

#ifdef __PLATFORM__UNIX
   hwg_Releasedc( o:handle, hDC )
#else
   hwg_Endpaint( o:handle, pps )
#endif

   RETURN Nil

STATIC FUNCTION Guitar_Acco_Show( n )

   LOCAL i, s

   IF n == 0 .OR. n > Len( aAccords )
      RETURN Nil
   ENDIF
   s := GetInBrackets( pAccords[aAccords[n]], '[', ']' )
   oDlgGuitar:oPanel:SetText( aAccords[n] )
   //nCurrMode := 1
   aCurrAcc := Iif( Empty(s), {}, hb_ATokens( s, ',' ) )
   FOR i := 1 TO Len( aCurrAcc )
      aCurrAcc[i] := hb_ATokens( aCurrAcc[i],'-' )
   NEXT
   nAccSele := 0
   IF nCurrMode == 2
      oDlgGuitar:oPanel:Refresh()
      oPaneGuitar:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION Guitar_Pane_Other( o, msg, wp, lp )

   LOCAL x1 := 30, y1 := 20, xm, ym, xc, i, j, n, cFile

   IF msg == WM_LBUTTONDOWN
      HB_SYMBOL_UNUSED( o )
      HB_SYMBOL_UNUSED( wp )
      xm := hwg_Loword( lp )
      ym := hwg_Hiword( lp )
      Guitar_ReleaseSounds()
      IF nCurrMode == 1
         FOR i := 1 TO 6
            IF nCurrNote > 0
               IF nCurrNote >= aStrings[i,2] .AND. nCurrNote <= aStrings[i,2] + 12
                  n := nCurrNote - aStrings[i,2]
               ELSE
                  n := Int( Abs( nCurrNote - aStrings[i,2] ) % 12 )
                  IF nCurrNote < aStrings[i,2]
                     n := 12 - n
                  ENDIF
               ENDIF
               IF xm > x1+n*20 .AND. xm < x1+n*20+24 .AND. ym > y1-10+(i-1)*20 .AND. ym < y1+12+(i-1)*20
                  cFile := hb_DirBase() + cSoundsPath + "g" + ;
                     ltrim(str(i)) + "_" + PAdl( Ltrim(Str(n)),2,"0" ) + "_" + ;
                     Note2Text( aStrings[i,2] + n ) + ".ogg"
                  IF File( cFile ) .AND. ( arrSounds[1] := sf_initData( cFile ) ) != Nil
                     pa_SetVolume( arrSounds[1], nCurrVol )
                     pa_OpenStream( arrSounds[1] )
                     pa_StartStream( arrSounds[1] )
                  ENDIF
               ENDIF
            ENDIF
         NEXT

      ELSEIF nCurrMode == 2
         FOR i := 1 TO Len( aCurrAcc )
            IF xm > x1+(i-1)*140 .AND. xm < x1+(i-1)*140+100 .AND. ym > y1 .AND. ym < y1+120
               j := 0
               FOR n := Len( aCurrAcc[i] ) TO 1 STEP -1
                  IF Left( aCurrAcc[i,n],1 ) <= '9'
                     cFile := hb_DirBase() + cSoundsPath + "g" + ;
                        ltrim(str(n)) + "_" + PAdl( aCurrAcc[i,n],2,"0" ) + "_" + ;
                        Note2Text( aStrings[n,2] + Val(aCurrAcc[i,n]) ) + ".ogg"
                     arrSounds[++j] := sf_initData( cFile )
                     //hwg_writelog(cFile)
                  ENDIF
               NEXT
               sf_SetAccord( arrSounds[1], arrSounds[2], ;
                  arrSounds[3], arrSounds[4], arrSounds[5], arrSounds[6], nDelayAcc )
               pa_SetVolume( arrSounds[1], nCurrVol )
               pa_OpenStream( arrSounds[1] )
               pa_StartStream( arrSounds[1] )
               nAccSele := i
               oPaneGuitar:Refresh()
            ENDIF
         NEXT
      ELSEIF nCurrMode == 3
         IF Empty( oScore:aNotes ) .OR. noteCheckAttr( oScore:aNotes[1], "tg" )
            y1 := 30
            ym := Int( ym / 20 )
            IF ym < 7
               xc := x1 + 10
               nTabPosY := ym
               n := nTabStart - 1
               DO WHILE ++n <= Len( oScore:aNotes ) .AND. xc < o:nWidth - 10
                  IF xm > xc .AND. xm < xc + DELTA_X
                     nTabPosX := n
                     EXIT
                  ENDIF
                  xc += DELTA_X
               ENDDO
               oPaneGuitar:Refresh()
               oScore:nCurr := nTabPosX
               HWindow():GetMain:oPaneNote:oPaneScore:Refresh()
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN -1


STATIC FUNCTION guitar_OpenTab( cFile )

   LOCAL aTabs, i := 0, j, n, nLen, arr[6], l, c, nDist, aNotes := {}

   IF Empty( cFile )
#ifdef __PLATFORM__UNIX
      cFile := hwg_SelectfileEx( , cPlugDir, { { "Tab files", "*.tab" }, , { "Lm files", "*.lm" } } )
#else
      cFile := hwg_Selectfile( { "Tab files","Lm files" }, { "*.tab","*.lm" }, cPlugDir  )
#endif
   ENDIF

   IF Empty( cFile )
      RETURN Nil
   ENDIF

   aTabs := hb_aTokens( Memoread( cFile ), Chr(10) )
   DO WHILE ++i < Len( aTabs )
      IF Upper( Left( (aTabs[i] := AlLTrim( aTabs[i] )), 2 ) ) == "E|"
         FOR n := 1 TO 5
            aTabs[i+n] = AlLTrim( aTabs[i+n] )
         NEXT
         j := 2
         nLen := Len( aTabs[i] )
         nDist := 0
         DO WHILE ++j <= nLen
            AFill( arr, 0 )
            l := .F.
            FOR n := 0 TO 5
               IF IsDigit( c := Substr( aTabs[i+n], j, 1 ) )
                  arr[n+1] := Val( Substr( aTabs[i+n], j, 2 ) )
                  IF !l
                     IF !Empty( aNotes ) .AND. ATail(aNotes)[2] == 0
                        ATail(aNotes)[2] := Iif( nDist > 6, 1, 7 - nDist )
                     ENDIF
                  ENDIF
                  nDist := 0
                  l := .T.
               ELSEIF c == '|' .AND. !Empty( aNotes )
                  noteSetAttr( ATail( aNotes ), "t" )
                  ATail(aNotes)[2] := Iif( nDist > 6, 1, 7 - nDist )
                  nDist := 0
                  EXIT
               ELSE
                  arr[n+1] := -1
               ENDIF
            NEXT
            IF l
               AAdd( aNotes, { guitar_toNote(arr), 0, "tg/" + guitar_toTab(arr) } )
            ELSE
               nDist ++
            ENDIF
         ENDDO
         i += 5
      ENDIF
   ENDDO
   IF !Empty( aNotes )
      cLastTab := cFile
      oDlgGuitar:oPanel:SetText( hb_fnameName(cFile) )
      oDlgGuitar:oPanel:Refresh()
      SetScore( aNotes )
      nTabStart := 1
      oPaneGuitar:Refresh()
   ELSE
      oMsg:MsgStop( "Wrong file format", "Error" )
   ENDIF

   RETURN Nil

STATIC FUNCTION guitar_toNote( arr )

   LOCAL n := 0, i, j := 0, xRes

   FOR i := 1 TO 6
      IF arr[i] >= 0
         xRes := aStrings[i,2] + arr[i]
         n ++
      ENDIF
   NEXT
   IF n > 1
      xRes := Array( n )
   ELSE
      RETURN xRes
   ENDIF

   FOR i := 1 TO 6
      IF arr[i] >= 0
         xRes[++j] := aStrings[i,2] + arr[i]
      ENDIF
   NEXT

   RETURN xRes

STATIC FUNCTION guitar_toTab( arr )

   LOCAL s := "", i

   FOR i := 1 TO 6
      s += Iif( arr[i]==-1, 'X', Chr( arr[i] + Iif( arr[i]<10, 48, 55 ) ) )
   NEXT

   RETURN s

STATIC FUNCTION guitar_PlayTab()

   IF !Empty( oScore:aNotes ) .AND. noteCheckAttr( oScore:aNotes[1], "tg" )
      SetInstr( "g" )
      Player()
      PlayNotes( .F. )
   ENDIF

   RETURN Nil

STATIC FUNCTION guitar_DragTab( oBtn, n, lForw )

   IF n < 2
      IF !lForw .AND. nTabStart > 1
         nTabStart --
         oPaneGuitar:Refresh()
      ELSEIF lForw .AND. nTabStart < ( Len( oScore:aNotes ) - 4 )
         nTabStart ++
         oPaneGuitar:Refresh()
      ENDIF
   ENDIF
   HB_SYMBOL_UNUSED(oBtn)

   RETURN Nil

STATIC FUNCTION guitar_SetCursor( nKey )

   LOCAL l := .F.
   //IF nTabStart == nTabEnd .AND. nTabEnd == 1
   //   RETURN Nil
   //ENDIF
   IF nKey == VK_UP
      IF nTabPosY > 0
         nTabPosY --
         l := .T.
      ENDIF
   ELSEIF nKey == VK_DOWN
      IF nTabPosY < 6
         nTabPosY ++
         l := .T.
      ENDIF
   ELSEIF nKey == VK_LEFT
      IF nTabPosX > 1
         nTabPosX --
         IF nTabPosX < nTabStart
            nTabStart := nTabPosX
         ENDIF
         l := .T.
      ENDIF
   ELSEIF nKey == VK_RIGHT
      IF nTabPosX < Len( oScore:aNotes )
         nTabPosX ++
         IF nTabPosX > nTabEnd
            nTabStart ++
         ENDIF
         l := .T.
      ENDIF
   ELSEIF nKey == VK_HOME
      nTabPosX := nTabStart := 1
      oPaneGuitar:Refresh()
   ELSEIF nKey == VK_END
      nTabPosX := Len( oScore:aNotes )
      nTabStart := Max( 1, nTabPosX - 8 )
      l := .T.
   ENDIF
   IF l
      oPaneGuitar:Refresh()
      oScore:nCurr := nTabPosX
      HWindow():GetMain:oPaneNote:oPaneScore:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION guitar_LadInput( n )

   HB_SYMBOL_UNUSED( n )
   IF nTabPosY == 0
      RETURN Nil
   ENDIF

   RETURN Nil

STATIC FUNCTION Guitar_ReadIni()

   LOCAL cFile := cPlugDir + "guitar.ini"
   LOCAL hIni := _iniRead( cFile ), aIni, nSect, aSect, cTemp

   IF !Empty( hIni )
      aIni := hb_HKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[ nSect ] ) == "START"
            IF !Empty( aSect := hIni[ aIni[ nSect ] ] )
               IF hb_hHaskey( aSect, cTemp := "auto" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lAutoRun := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "setguitar" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lSetGuitar := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "mode" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nDefMode := nCurrMode := Val( cTemp )
                  IF nCurrMode < 1 .OR. nCurrMode > 3
                     nDefMode := nCurrMode := 1
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "openlast" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lOpenLast := ( Lower(cTemp) == "on" )
               ENDIF
            ENDIF
         ELSEIF Upper( aIni[ nSect ] ) == "ACCORDS"
            pAccords := hIni[ aIni[nSect] ]
            aAccords := hb_HKeys( pAccords )
         ENDIF
      NEXT
   ENDIF

   cFile := cPlugDir + "guitar.his"
   hIni := _iniRead( cFile )
   IF !Empty( hIni )
      aIni := hb_HKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[ nSect ] ) == "FILES"
            IF !Empty( aSect := hIni[ aIni[ nSect ] ] )
               IF hb_hHaskey( aSect, cTemp := "f00" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cLastTab := cTemp
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION Guitar_ReleaseSounds()

   LOCAL i
   FOR i := 1 TO 6
      IF !Empty( arrSounds[i] )
         IF i == 1
            pa_AbortStream( arrSounds[i] )
         ENDIF
         sf_FreeData( arrSounds[i] )
         arrSounds[i] := Nil
      ENDIF
   NEXT
   RETURN Nil

STATIC FUNCTION dlgGuitarExit()

   LOCAL s

   oDlgGuitar := Nil
   Guitar_ReleaseSounds()
   IF !Empty( cLastTab )
      s := "[FILES]" + Chr(13) + Chr(10) + "f00=" + cLastTab
      hb_MemoWrit( cPlugDir + "guitar.his", s )
   ENDIF

   RETURN .T.
