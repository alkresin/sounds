/*
 * A plugin for Sounds.
 */

#include "hwgui.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_RED      0x0000ff
#define CLR_LIGHTGRAY_1 0xdddddd
#define CLR_LIGHTGRAY_2 0xaaaaaa
#define CLR_DARKGRAY_1  0x333333
#define CLR_DARKGRAY_2  0x666666

#define CLR_BROWN_1  0x154780
#define CLR_BROWN_2  0x6a9cd4
#define CLR_BROWN_3  0xaad2ff
#define CLR_BROWN_4  0x396eaa
#define CLR_BROWN_5  0x9dc7f6
#define CLR_TOPDARK 0x7b7680
#define CLR_TOPMID  0x5b5760
#define CLR_DLGBACK 0x154780
#define CLR_DLGHEA  0x2F343F

#define TOPPANE_HEIGHT  28

#define LAD_KOL   4

STATIC bPlugNote_Orig, oDlgGuitar, oPaneGuitar
STATIC oPen1, oPen2, oFontNote, oBrushWhite, oBrushBlack
STATIC cPlugDir
STATIC nCurrNote := 0, nCurrMode := 0
STATIC pAccords, aAccords, aCurrAcc

MEMVAR oMsg, aPlugMenu, bPlugNote

FUNCTION Plug_guitar()

   cPlugDir := hb_DirBase() + "plugins" + hb_ps()
   Guitar_ReadIni()
   AAdd( aPlugMenu, { "Guitar", {||guitar_Dlg()} } )
   IF !Empty( bPlugNote )
      bPlugNote_Orig := bPlugNote
   ENDIF
   bPlugNote := {|n| guitar_Show(n) }

   RETURN Nil

STATIC FUNCTION guitar_Dlg()

   LOCAL oMainWindow := HWindow():GetMain(), oPanel, h

   IF !Empty( oDlgGuitar )
      RETURN Nil
   ENDIF

   IF Empty( oPen1 )
      oPen1 := HPen():Add( PS_SOLID, 1, CLR_BLACK )
      oPen2 := HPen():Add( PS_SOLID, 2, CLR_BLACK )
      oBrushWhite := HBrush():Add( CLR_WHITE )
      oBrushBlack := HBrush():Add( CLR_BLACK )
      oFontNote := oMainWindow:oFont:SetFontStyle( ,,,,, Iif( (h := oMainWindow:oFont:height) < 0, h+2, h-2 ) )
   ENDIF

   INIT DIALOG oDlgGuitar TITLE "Guitar" BACKCOLOR CLR_DLGBACK ;
      AT 20, 400 SIZE 480, 240 FONT oMainWindow:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| oDlgGuitar := Nil}

   oDlgGuitar:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DLGHEA ;
      TEXT "" COORS 20 BTN_CLOSE

   @ 2, TOPPANE_HEIGHT PANEL oPaneGuitar SIZE oDlgGuitar:nWidth-4, oDlgGuitar:nHeight-TOPPANE_HEIGHT*2 ;
      STYLE SS_OWNERDRAW BACKCOLOR CLR_BROWN_3 ON SIZE {||.t.}
   oPaneGuitar:bPaint := {|| guitar_Paint()}

   @ 0, oDlgGuitar:nHeight-TOPPANE_HEIGHT OWNERBUTTON SIZE 120, TOPPANE_HEIGHT ;
      TEXT "Accord" COLOR CLR_WHITE FONT oMainWindow:oPaneTop:aControls[1]:oFont ON CLICK {||Guitar_Accords()}
   ATail( oDlgGuitar:aControls ):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   oPanel:SetSysbtnColor( CLR_WHITE, CLR_TOPDARK )

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
   LOCAL x1 := 20, y1 := 20, i, n, nMin, nLad, lBarre, nBarre
   STATIC aStrings := { {"E",53}, {"B",48}, {"G",44}, {"D",39}, {"A",34}, {"E",29} }

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )

   IF nCurrMode == 0
      hwg_Selectobject( hDC, oPen1:handle )
      hwg_Selectobject( hDC, oFontNote:handle )
      hwg_SetTransparentMode( hDC, .T. )
      FOR i := 1 TO 6
         hwg_Drawline( hDC, x1, y1+(i-1)*20, o:nWidth-10, y1+(i-1)*20 )
         hwg_Drawtext( hDC, aStrings[i,1], 4, y1-8+(i-1)*20, 20, y1+10+(i-1)*20, DT_LEFT )
         IF nCurrNote > 0
            IF nCurrNote >= aStrings[i,2] .AND. nCurrNote <= aStrings[i,2] + 12
               n := nCurrNote - aStrings[i,2]
               hwg_Fillrect( hDC, x1+n*20, y1-8+(i-1)*20, x1+n*20+24, y1+10+(i-1)*20, oBrushBlack:handle )
               hwg_Fillrect( hDC, x1+n*20+1, y1-8+(i-1)*20+1, x1+n*20+24-1, y1+10+(i-1)*20-1, oBrushWhite:handle )
               //hwg_Rectangle( hDC, x1+n*20, y1-8+(i-1)*20, x1+n*20+24, y1+10+(i-1)*20 )
               hwg_Drawtext( hDC, Ltrim(Str(n)), x1+n*20+1, y1-6+(i-1)*20, x1+n*20+24-1, y1+8+(i-1)*20, DT_CENTER )
            ELSE
               n := Int( Abs( nCurrNote - aStrings[i,2] ) % 12 )
               IF nCurrNote < aStrings[i,2]
                  n := 12 - n
               ENDIF
               hwg_Fillrect( hDC, x1+n*20, y1-8+(i-1)*20, x1+n*20+24, y1+10+(i-1)*20, oBrushBlack:handle )
               hwg_Fillrect( hDC, x1+n*20+1, y1-8+(i-1)*20+1, x1+n*20+24-1, y1+10+(i-1)*20-1, o:brush:handle )
               //hwg_Rectangle( hDC, x1+n*20, y1-8+(i-1)*20, x1+n*20+24, y1+10+(i-1)*20 )
               hwg_Drawtext( hDC, Ltrim(Str(n)), x1+n*20+6, y1-6+(i-1)*20, x1+n*20+22, y1+8+(i-1)*20, DT_CENTER )
            ENDIF
         ENDIF
      NEXT
      hwg_SetTransparentMode( hDC, .F. )
      hwg_Selectobject( hDC, oPen2:handle )
      hwg_Drawline( hDC, x1, y1, x1, y1+100 )

   ELSEIF nCurrMode == 1
      hwg_Selectobject( hDC, oFontNote:handle )
      hwg_SetTransparentMode( hDC, .T. )
      FOR n := 1 TO Len( aCurrAcc )
         nMin := 99
         lBarre := .T.; nBarre := 0
         FOR i := 1 TO Len( aCurrAcc[n] )
            nMin := Min( nMin, Iif( (nLad := Val(aCurrAcc[n,i])) == 0, nMin, nLad ) )
            IF aCurrAcc[n,i] == "0"
               lBarre := .F.
            ENDIF
         NEXT
         hwg_Selectobject( hDC, oPen1:handle )
         FOR i := 1 TO LAD_KOL
            hwg_Drawtext( hDC, Ltrim(Str(nMin+i-1)), x1-16, y1+i*30-24, x1-2, y1+i*30-6, DT_RIGHT )
            hwg_Drawline( hDC, x1, y1+i*30, x1+100, y1+i*30 )
         NEXT
         FOR i := 1 TO 6
            hwg_Drawline( hDC, x1+(i-1)*20, y1, x1+(i-1)*20, y1+120 )
            IF i > Len( aCurrAcc[n] ) .OR. aCurrAcc[n,i] == 'X'
               hwg_Drawtext( hDC, 'X', x1+(6-i)*20-6, 2, x1+(6-i)*20+8, y1, DT_LEFT )
            ELSE
               IF aCurrAcc[n,i] == '0'
                  hwg_Drawtext( hDC, 'O', x1+(6-i)*20-6, 2, x1+(6-i)*20+8, y1, DT_LEFT )
               ELSE
                  nLad := Val( aCurrAcc[n,i] ) - nMin
                  IF lBarre .AND. nLad == 0
                     nBarre := i
                  ELSE
                     hwg_drawGradient( hDC, x1+(6-i)*20-8, y1+nLad*30+8, x1+(6-i)*20+8, y1+nLad*30+24, 1, ;
                        { 0 },, {12,12,12,12} )
                  ENDIF
               ENDIF
            ENDIF
         NEXT
         IF nBarre > 0
            hwg_drawGradient( hDC, x1+(6-nBarre)*20-8, y1+nLad*30+8, x1+5*20+8, y1+nLad*30+24, 1, ;
               { 0 },, {12,12,12,12} )
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

STATIC FUNCTION Guitar_Accords()

   LOCAL i, s
   LOCAL x1, y1
#ifndef __PLATFORM__UNIX
   LOCAL aCoors1 := hwg_GetWindowRect( oDlgReco:oParent:handle )
   LOCAL aCoors2 := hwg_GetWindowRect( oDlgGuitar:handle )
#endif

#ifdef __PLATFORM__UNIX
   x1 := oDlgGuitar:nLeft-oDlgGuitar:oParent:nLeft
   y1 := oDlgGuitar:nTop-oDlgGuitar:oParent:nTop+oDlgGuitar:nHeight-TOPPANE_HEIGHT-120
#else
   x1 := aCoors2[1] - aCoors1[1]; y1 := aCoors2[2] - aCoors1[2] + oDlgGuitar:nHeight-TOPPANE_HEIGHT-120
#endif

   IF ( i := FileMenu( x1, y1, 120, 120,,,, aAccords,, .T. ) ) > 0
      oDlgGuitar:oPanel:SetText( aAccords[i] )
      nCurrMode := 1
      s := GetInBrackets( pAccords[aAccords[i]], '[', ']' )
      aCurrAcc := Iif( Empty(s), {}, hb_ATokens( s, ',' ) )
      FOR i := 1 TO Len( aCurrAcc )
         aCurrAcc[i] := hb_ATokens( aCurrAcc[i],'-' )
      NEXT
      oDlgGuitar:oPanel:Refresh()
      oPaneGuitar:Refresh()
   ENDIF

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
