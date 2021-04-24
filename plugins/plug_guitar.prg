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

STATIC bPlugNote_Orig, oDlgGuitar, oPaneGuitar
STATIC oPen1, oPen2, oFontNote, oBrushWhite, oBrushBlack, aGradient
STATIC cPlugDir
STATIC nCurrNote := 0, nCurrMode := 0
STATIC pAccords, aAccords, aCurrAcc, aAcco1, aAcco2

MEMVAR oMsg, pClr, aPlugMenu, bPlugNote

FUNCTION Plug_guitar()

   LOCAL i, s

   cPlugDir := hb_DirBase() + "plugins" + hb_ps()

   Guitar_ReadIni()
   //aAcco1 := Array( Len( aAccords ), 1 )
   //aAcco2 := Array( Len( aAccords ), 1 )
   aAcco1 := {}
   aAcco2 := {}
   FOR i := 1 TO Len( aAccords )
      s := Left( aAccords[i], 1 )
      IF hb_Ascan( aAcco1, s, .T. ) == 0
         AAdd( aAcco1, s )
      ENDIF
      s := Substr( aAccords[i], 2 )
      IF hb_Ascan( aAcco2, s, .T. ) == 0
         AAdd( aAcco2, s )
      ENDIF
   NEXT

   AAdd( aPlugMenu, { "Guitar", {||guitar_Dlg()} } )
   IF !Empty( bPlugNote )
      bPlugNote_Orig := bPlugNote
   ENDIF
   bPlugNote := {|n| guitar_Show(n) }

   RETURN Nil

STATIC FUNCTION guitar_Dlg()

   LOCAL oMainWindow := HWindow():GetMain(), oPanel, oLenta1, oLenta2, h
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

   IF !Empty( oDlgGuitar )
      RETURN Nil
   ENDIF

   IF Empty( oPen1 )
      oPen1 := HPen():Add( PS_SOLID, 1, CLR_BLACK )
      oPen2 := HPen():Add( PS_SOLID, 2, CLR_BLACK )
      oBrushWhite := HBrush():Add( CLR_WHITE )
      oBrushBlack := HBrush():Add( CLR_BLACK )
      oFontNote := oMainWindow:oFont:SetFontStyle( ,,,,, Iif( (h := oMainWindow:oFont:height) < 0, h+2, h-2 ) )
      //aGradient := { pClr["topdark"], pClr["topmid"] }
      aGradient := { pClr["clr1"] }
   ENDIF

   INIT DIALOG oDlgGuitar TITLE "Guitar" BACKCOLOR pClr["dlgback"] ;
      AT 20, 400 SIZE 500, 210 FONT oMainWindow:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| oDlgGuitar := Nil}

   oDlgGuitar:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      TEXT "" COORS 20 BTN_CLOSE

   oLenta1 := HLenta():New( ,, 0, TOPPANE_HEIGHT, 30, oDlgGuitar:nHeight-TOPPANE_HEIGHT, ;
      oMainWindow:oFont,,, bLClick,,, aAcco1, 30, aStyle )

   oLenta2 := HLenta():New( ,, oLenta1:nWidth+2, TOPPANE_HEIGHT, 38, oDlgGuitar:nHeight-TOPPANE_HEIGHT, ;
      oMainWindow:oFont,,, bLClick,,, aAcco2, 30, aStyle )
   oLenta2:Value := 1

   @ 70, TOPPANE_HEIGHT PANEL oPaneGuitar SIZE oDlgGuitar:nWidth-70, oDlgGuitar:nHeight-TOPPANE_HEIGHT ;
      STYLE SS_OWNERDRAW BACKCOLOR pClr["clr3"] ON SIZE {||.t.}
   oPaneGuitar:bPaint := {|| guitar_Paint()}

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   ACTIVATE DIALOG oDlgGuitar NOMODAL

   RETURN Nil

STATIC FUNCTION guitar_Show( n )

   IF !Empty( bPlugNote_Orig )
      Eval( bPlugNote_Orig, n )
   ENDIF
   IF Empty( oDlgGuitar )
      RETURN Nil
   ENDIF

   nCurrMode := 0
   nCurrNote := Iif( Valtype(n) == "N", n, 0 )
   oDlgGuitar:oPanel:SetText( Note2Text( nCurrNote, .T. ) )
   oDlgGuitar:oPanel:Refresh()
   oPaneGuitar:Refresh()

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
   STATIC aStrings := { {"E",53}, {"B",48}, {"G",44}, {"D",39}, {"A",34}, {"E",29} }

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )

   IF nCurrMode == 0
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
      hwg_Selectobject( hDC, oFontNote:handle )
      FOR i := 1 TO 6
         hwg_Drawtext( hDC, aStrings[i,1], x1-16, y1-8+(i-1)*20, x1, y1+10+(i-1)*20, DT_LEFT )
      NEXT
      hwg_SetTransparentMode( hDC, .F. )
      hwg_Selectobject( hDC, oPen2:handle )
      hwg_Drawline( hDC, x1, y1, x1, y1+100 )

   ELSEIF nCurrMode == 1
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
         hwg_Selectobject( hDC, oPen1:handle )
         FOR i := 1 TO LAD_KOL
            hwg_Drawtext( hDC, Ltrim(Str(nMin+i-1)), x1-20, y1+i*30-24, x1-4, y1+i*30-6, DT_LEFT )
            hwg_Drawline( hDC, x1, y1+i*30, x1+100, y1+i*30 )
         NEXT
         FOR i := 1 TO 6
            //hwg_Selectobject( hDC, oPen1:handle )
            hwg_Drawline( hDC, x1+(i-1)*20, y1, x1+(i-1)*20, y1+120 )
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
      hwg_SetTransparentMode( hDC, .F. )
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
   nCurrMode := 1
   aCurrAcc := Iif( Empty(s), {}, hb_ATokens( s, ',' ) )
   FOR i := 1 TO Len( aCurrAcc )
      aCurrAcc[i] := hb_ATokens( aCurrAcc[i],'-' )
   NEXT
   oDlgGuitar:oPanel:Refresh()
   oPaneGuitar:Refresh()

   RETURN Nil

STATIC FUNCTION Guitar_ReadIni()

   LOCAL cFile := cPlugDir + "guitar.ini"
   LOCAL hIni := _iniRead( cFile ), aIni, nSect

   IF !Empty( hIni )
      aIni := hb_HKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[ nSect ] ) == "ACCORDS"
            pAccords := hIni[ aIni[nSect] ]
            aAccords := hb_HKeys( pAccords )
         ENDIF
      NEXT
   ENDIF

   RETURN Nil
