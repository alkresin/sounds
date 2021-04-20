/*
 * A Recorder plugin for Sounds.
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hwgui.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000

#define TOPPANE_HEIGHT  28

STATIC bPlugNote_Orig, oDlgReco, oPaneReco
STATIC oBmpFull, oBmpHalf, oBmpEmpty, oBmpFull_2, oBmpEmpty_2
STATIC oPen
STATIC cPlugDir
STATIC nCurrNote := 0
STATIC nCurrType := 2, nCurrAppl := 2
STATIC aTable := { { { 0,0,0,0,0,0,0,0,0,0 }, { 1,1,1,1,1,1,1,1,1,1 }, { 1,1,1,1,1,1,1,1,1,0 }, ;
   { 1,1,1,1,1,1,1,1,0,0 }, { 1,1,1,1,1,1,1,0,0,0 }, { 1,1,1,1,1,1,0,0,0,0 }, { 1,1,1,1,1,0,0,0,0,0 }, ;
   { 1,1,1,1,0,1,1,1,1,1 }, { 1,1,1,1,0,0,0,0,0,0 }, { 1,1,1,0,1,1,1,0,0,0 }, { 1,1,1,0,0,0,0,0,0,0 }, ;
   { 1,1,0,1,1,0,0,0,0,0 }, { 1,1,0,0,0,0,0,0,0,0 }, { 1,0,1,0,0,0,0,0,0,0 }, { 0,1,1,0,0,0,0,0,0,0 }, ;
   { 0,0,1,0,0,0,0,0,0,0 }, { 0,0,1,1,1,1,1,1,0,0 }, { 2,1,1,1,1,1,0,0,0,0 }, { 2,1,1,1,1,0,0,0,0,0 }, ;
   { 2,1,1,1,0,1,0,0,1,1 }, { 2,1,1,1,0,0,0,0,0,0 }, { 2,1,1,1,0,1,1,1,1,1 }, { 2,1,1,0,0,0,0,0,0,0 }, ;
   { 2,1,1,0,1,1,1,1,0,0 }, { 2,1,1,0,1,1,0,0,0,0 }, { 2,1,0,0,1,1,0,0,0,0 }, { 2,1,0,1,1,0,1,1,1,1 }, ;
   { 2,1,0,1,1,0,1,1,0,0 } }, ;
   { { 0,0,0,0,0,0,0,0,0,0 }, { 1,1,1,1,1,1,1,1,1,1 }, { 1,1,1,1,1,1,1,1,1,0 }, ;
   { 1,1,1,1,1,1,1,1,0,0 }, { 1,1,1,1,1,1,1,0,0,0 }, { 1,1,1,1,1,1,0,0,0,0 }, { 1,1,1,1,1,0,1,1,1,1 }, ;
   { 1,1,1,1,0,1,1,1,0,0 }, { 1,1,1,1,0,0,0,0,0,0 }, { 1,1,1,0,1,1,1,0,0,0 }, { 1,1,1,0,0,0,0,0,0,0 }, ;
   { 1,1,0,1,1,0,0,0,0,0 }, { 1,1,0,0,0,0,0,0,0,0 }, { 1,0,1,0,0,0,0,0,0,0 }, { 0,1,1,0,0,0,0,0,0,0 }, ;
   { 0,0,1,0,0,0,0,0,0,0 }, { 0,0,1,1,1,1,1,1,0,0 }, { 2,1,1,1,1,1,0,0,0,0 }, { 2,1,1,1,1,0,1,1,0,0 }, ;
   { 2,1,1,1,0,1,0,0,0,0 }, { 2,1,1,1,0,0,0,0,0,0 }, { 2,1,1,0,1,0,0,0,0,0 }, { 2,1,1,0,0,0,0,0,0,0 }, ;
   { 2,1,1,0,1,1,1,1,0,0 }, { 2,1,1,0,1,1,0,0,0,0 }, { 2,1,0,0,1,1,0,0,0,0 }, { 2,1,0,1,1,0,1,1,1,1 }, ;
   { 2,1,0,1,1,0,1,1,0,0 } } }

MEMVAR oMsg, pClr, aPlugMenu, bPlugNote

FUNCTION Plug_recorder()

   cPlugDir := hb_DirBase() + "plugins" + hb_ps()
   AAdd( aPlugMenu, { "Recorder", {||recorder_Dlg()} } )
   IF !Empty( bPlugNote )
      bPlugNote_Orig := bPlugNote
   ENDIF
   bPlugNote := {|n| recorder_Show(n) }

   RETURN Nil

STATIC FUNCTION recorder_Dlg()

   LOCAL oMainWindow := HWindow():GetMain(), oPanel, oBinCnt

   IF !Empty( oDlgReco )
      RETURN Nil
   ENDIF

   IF Empty( oBmpFull )
      IF !Empty( oBinCnt := HBinC():Open( cPlugDir + "plug_recorder.bin" ) )
         oBmpFull := HBitmap():AddString( "c1", oBinCnt:Get( "c1" ) )
         oBmpEmpty := HBitmap():AddString( "c0", oBinCnt:Get( "c0" ) )
         oBmpHalf := HBitmap():AddString( "c2", oBinCnt:Get( "c2" ) )
         oBmpFull_2 := HBitmap():AddString( "c1_2", oBinCnt:Get( "c1_2" ) )
         oBmpEmpty_2 := HBitmap():AddString( "c0_2", oBinCnt:Get( "c0_2" ) )
         oBinCnt:Close()
         /*
         oBmpFull := HBitmap():AddFile( cPlugDir + "c1.bmp" )
         oBmpEmpty := HBitmap():AddFile( cPlugDir + "c0.bmp" )
         oBmpHalf := HBitmap():AddFile( cPlugDir + "c2.bmp" )
         oBmpFull_2 := HBitmap():AddFile( cPlugDir + "c1_2.bmp" )
         oBmpEmpty_2 := HBitmap():AddFile( cPlugDir + "c0_2.bmp" )
         */
      ENDIF
      oPen := HPen():Add( PS_SOLID, 1, CLR_BLACK )
   ENDIF

   INIT DIALOG oDlgReco TITLE "Recorder" BACKCOLOR pClr["dlgback"] ;
      AT oMainWindow:nWidth+10, oMainWindow:nTop SIZE 120, 360 FONT oMainWindow:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| oDlgReco := Nil}

   oDlgReco:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR pClr["dlghea"] ;
      TEXT "" COORS 20 BTN_CLOSE

   @ 2, TOPPANE_HEIGHT PANEL oPaneReco SIZE oDlgReco:nWidth-4, oDlgReco:nHeight-TOPPANE_HEIGHT*2 ;
      STYLE SS_OWNERDRAW BACKCOLOR pClr["clr3"] ON SIZE {||.t.}
   oPaneReco:bPaint := {|| recorder_Paint()}

   oPanel:SetSysbtnColor( CLR_WHITE, pClr["topdark"] )

   @ 0, oDlgReco:nHeight-TOPPANE_HEIGHT OWNERBUTTON  SIZE 60, TOPPANE_HEIGHT ;
      TEXT "S" COLOR CLR_WHITE FONT oMainWindow:oPaneTop:aControls[1]:oFont ON CLICK {|| recorder_Type() }
   ATail( oDlgReco:aControls ):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   @ 60, oDlgReco:nHeight-TOPPANE_HEIGHT OWNERBUTTON  SIZE 60, TOPPANE_HEIGHT ;
      TEXT "B" COLOR CLR_WHITE FONT oMainWindow:oPaneTop:aControls[1]:oFont ON CLICK {|| recorder_Appl() }
   ATail( oDlgReco:aControls ):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   ACTIVATE DIALOG oDlgReco NOMODAL

   RETURN Nil

STATIC FUNCTION recorder_Type()

   LOCAL i, oBtn, x1, y1
#ifndef __PLATFORM__UNIX
   LOCAL aCoors1 := hwg_GetWindowRect( oDlgReco:oParent:handle )
   LOCAL aCoors2 := hwg_GetWindowRect( oDlgReco:handle )
#endif
   STATIC aTypes := { "Sn", "S", "Alt", "Tnr" }

#ifdef __PLATFORM__UNIX
   x1 := oDlgReco:nLeft-oDlgReco:oParent:nLeft
   y1 := oDlgReco:nTop-oDlgReco:oParent:nTop+oDlgReco:nHeight-TOPPANE_HEIGHT-120
#else
   //x1 := 0; y1 := oDlgReco:nHeight-TOPPANE_HEIGHT-120
   x1 := aCoors2[1] - aCoors1[1]; y1 := aCoors2[2] - aCoors1[2] + oDlgReco:nHeight-TOPPANE_HEIGHT-120
#endif

   i := FileMenu( x1, y1, oDlgReco:nWidth, 120,,,, ;
      { {(nCurrType==1),"Sopranino"}, {(nCurrType==2),"Soprano"}, {(nCurrType==3),"Alto"}, {(nCurrType==4),"Tenor"} } )
   IF i > 0
      nCurrType := i
      oBtn := oDlgReco:aControls[Len(oDlgReco:aControls)-1]
      oBtn:title := aTypes[i]
      oBtn:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION recorder_Appl()

   LOCAL i, oBtn, x1, y1
#ifndef __PLATFORM__UNIX
   LOCAL aCoors1 := hwg_GetWindowRect( oDlgReco:oParent:handle )
   LOCAL aCoors2 := hwg_GetWindowRect( oDlgReco:handle )
#endif

#ifdef __PLATFORM__UNIX
   x1 := oDlgReco:nLeft-oDlgReco:oParent:nLeft
   y1 := oDlgReco:nTop-oDlgReco:oParent:nTop+oDlgReco:nHeight-TOPPANE_HEIGHT-60
#else
   //x1 := 0; y1 := oDlgReco:nHeight-TOPPANE_HEIGHT-60
   x1 := aCoors2[1] - aCoors1[1]; y1 := aCoors2[2] - aCoors1[2] + oDlgReco:nHeight-TOPPANE_HEIGHT-60
#endif
   i := FileMenu( x1, y1, oDlgReco:nWidth, 60,,,, { {(nCurrAppl==1),"German"}, {(nCurrAppl==2),"Barocco"} } )
   IF i > 0
      nCurrAppl := i
      oBtn := oDlgReco:aControls[Len(oDlgReco:aControls)]
      oBtn:title := Iif( i == 1, "G", "B" )
      oBtn:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION recorder_Show( n )

   IF !Empty( bPlugNote_Orig )
      Eval( bPlugNote_Orig, n )
   ENDIF
   IF Empty( oDlgReco )
      RETURN Nil
   ENDIF

   nCurrNote := Iif( Valtype(n) == "N", n, 0 )
   oDlgReco:oPanel:SetText( Note2Text( nCurrNote ) )
   oDlgReco:oPanel:Refresh()
   oPaneReco:Refresh()

   RETURN Nil

STATIC FUNCTION recorder_Paint()

   LOCAL o := oPaneReco
#ifdef __PLATFORM__UNIX
   LOCAL hDC := hwg_Getdc( o:handle )
#else
   LOCAL pps    := hwg_Definepaintstru()
   LOCAL hDC    := hwg_Beginpaint( o:handle, pps )
#endif
   LOCAL xc := Int( o:nWidth/2 ), y1 := 10, n, arr

   IF nCurrType == 2 .OR. nCurrType == 4
      n := Iif( nCurrNote < 37 .OR. nCurrNote > 63, 0, nCurrNote - 36 ) + 1
   ELSE
      n := Iif( nCurrNote < 42 .OR. nCurrNote > 68, 0, nCurrNote - 41 ) + 1
   ENDIF

   hwg_Fillrect( hDC, 0, 0, o:nWidth, o:nHeight, o:brush:handle )

   IF Len( aTable[nCurrAppl] ) >= n
      arr := aTable[nCurrAppl,n]
      hwg_Drawtransparentbitmap( hDC, Iif(arr[1]==0,oBmpEmpty:handle,Iif(arr[1]==1,oBmpFull:handle,oBmpHalf:handle)), o:nLeft+10, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+16
      hwg_Selectobject( hDC, oPen:handle )
      hwg_Drawline( hDC, xc-20, y1-8, xc+20, y1-8 )
      hwg_Drawtransparentbitmap( hDC, Iif(arr[2]==0,oBmpEmpty:handle,oBmpFull:handle), xc-oBmpEmpty:nWidth/2, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+8
      hwg_Drawtransparentbitmap( hDC, Iif(arr[3]==0,oBmpEmpty:handle,oBmpFull:handle), xc-oBmpEmpty:nWidth/2, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+8
      hwg_Drawtransparentbitmap( hDC, Iif(arr[4]==0,oBmpEmpty:handle,oBmpFull:handle), xc-oBmpEmpty:nWidth/2, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+16
      hwg_Selectobject( hDC, oPen:handle )
      hwg_Drawline( hDC, xc-20, y1-8, xc+20, y1-8 )
      hwg_Drawtransparentbitmap( hDC, Iif( nCurrAppl==1, Iif(arr[5]==0,oBmpEmpty:handle,oBmpFull:handle), Iif(arr[5]==0,oBmpEmpty_2:handle,oBmpFull_2:handle) ), xc-oBmpEmpty_2:nWidth/2, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+8
      hwg_Drawtransparentbitmap( hDC, Iif( nCurrAppl==1, Iif(arr[6]==0,oBmpEmpty_2:handle,oBmpFull_2:handle), Iif(arr[6]==0,oBmpEmpty:handle,oBmpFull:handle) ), xc-oBmpEmpty:nWidth/2, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+8
      hwg_Drawtransparentbitmap( hDC, Iif(arr[7]==0,oBmpEmpty:handle,oBmpFull:handle), xc-oBmpEmpty:nWidth, y1, CLR_WHITE )
      hwg_Drawtransparentbitmap( hDC, Iif(arr[8]==0,oBmpEmpty_2:handle,oBmpFull_2:handle), xc, y1, CLR_WHITE )
      y1 += oBmpFull:nHeight+8
      hwg_Drawtransparentbitmap( hDC, Iif(arr[9]==0,oBmpEmpty:handle,oBmpFull:handle), xc-oBmpEmpty:nWidth, y1, CLR_WHITE )
      hwg_Drawtransparentbitmap( hDC, Iif(arr[10]==0,oBmpEmpty_2:handle,oBmpFull_2:handle), xc, y1, CLR_WHITE )
   ENDIF

#ifdef __PLATFORM__UNIX
   hwg_Releasedc( o:handle, hDC )
#else
   hwg_Endpaint( o:handle, pps )
#endif

   RETURN Nil
