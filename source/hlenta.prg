/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HLenta class
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "hwgui.ch"
#include "hbclass.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_GRAY_1   0xcccccc
#define CLR_GRAY_2   0x999999

CLASS HLenta INHERIT HControl

CLASS VAR winclass INIT "PANEL"

#ifdef __PLATFORM__UNIX
   DATA hBox
#endif
   DATA lVertical
   DATA aItems
   DATA nItemSize
   DATA aItemStyle
   DATA oFont
   DATA lCaptured    INIT .F.
   DATA lMoved       INIT .F.
   DATA nFirst       INIT 1
   DATA nSelected    INIT 0
   DATA nOver        INIT 0
   DATA nShift       INIT 0
   DATA nDragKoef    INIT 1
   DATA xPos, yPos
   DATA bClick

   METHOD New( oWndParent, nId, nLeft, nTop, nWidth, nHeight, oFont, ;
               bSize, bPaint, bClick, color, bcolor, aItems, nItemSize, aItemStyle )
   METHOD Activate()
   METHOD onEvent( msg, wParam, lParam )
   METHOD Init()
   METHOD onEvent( msg, wParam, lParam )
   METHOD Paint()
   METHOD Drag( xPos, yPos )
   METHOD Value( nValue ) SETGET

ENDCLASS

METHOD New( oWndParent, nId, nLeft, nTop, nWidth, nHeight, oFont, ;
            bSize, bPaint, bClick, color, bcolor, aItems, nItemSize, aItemStyle ) CLASS HLenta

   color := Iif( color == Nil, CLR_BLACK, color )
   bColor := Iif( bColor == Nil, CLR_WHITE, bColor )
   ::Super:New( oWndParent, nId, WS_CHILD + WS_VISIBLE + SS_OWNERDRAW, nLeft, nTop, nWidth, nHeight, oFont,, ;
              bSize, bPaint,, color, bcolor )

   ::title  := ""
   ::lVertical := ( ::nHeight > ::nWidth )
   ::bClick := bClick
   ::aItems := aItems
   ::aItemStyle := Iif( Empty(aItemStyle), { HStyle():New( { CLR_WHITE, CLR_GRAY_1 }, 3 ), ;
      HStyle():New( { CLR_WHITE, CLR_GRAY_2 }, 3 ), HStyle():New( { CLR_GRAY_1 }, 3 )  }, aItemStyle )
   IF Len( ::aItemStyle ) == 1
      AAdd( ::aItemStyle, Nil )
   ENDIF
   IF Len( ::aItemStyle ) == 2
      AAdd( ::aItemStyle, Nil )
   ENDIF
   ::nItemSize := nItemSize

   ::Activate()

   RETURN Self

METHOD Activate() CLASS HLenta
   LOCAL handle := ::oParent:handle

   IF !Empty( handle )
#ifdef __PLATFORM__UNIX
      ::handle := hwg_Createpanel( Self, ::id, ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
#else
      ::handle := hwg_Createpanel( handle, ::id, ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
#endif
      ::Init()
   ENDIF

   RETURN Nil

METHOD Init() CLASS HLenta

   IF !::lInit

      ::Super:Init()
      hwg_Setwindowobject( ::handle, Self )
#ifndef __PLATFORM__UNIX
      ::nHolder := 1
      Hwg_InitWinCtrl( ::handle )
#endif
   ENDIF

   RETURN Nil

METHOD onEvent( msg, wParam, lParam ) CLASS HLenta

   LOCAL xPos, yPos, lRedraw := .F., y1

   HB_SYMBOL_UNUSED( wParam )

   IF msg == WM_MOUSEMOVE
      xPos := hwg_Loword( lParam ); yPos := hwg_Hiword( lParam )
      IF xPos < 0 .OR. xPos > ::nWidth .OR. yPos < 0 .OR. yPos > ::nHeight
         ::lCaptured := .F.
         ::nOver := 0
         hwg_Releasecapture()
         lRedraw := .T.
      ELSE
         IF ::lCaptured
            ::Drag( xPos, yPos )
         ELSE
            IF ::lVertical
               y1 := Int( ::nShift % ::nItemSize )
               IF y1 > 0; y1 := ::nItemSize - y1; ENDIF
               yPos -= y1
               IF yPos > 0
                  lRedraw := ( ::nOver != Int( yPos / ::nItemSize ) + ::nFirst ) .AND. !Empty( ::aItemStyle[2] )
                  ::nOver := Int( yPos / ::nItemSize ) + ::nFirst
               ENDIF
            ELSE
            ENDIF
         ENDIF
      ENDIF

   ELSEIF msg == WM_PAINT
      ::Paint()

   ELSEIF msg == WM_LBUTTONDOWN
      xPos := hwg_Loword( lParam ); yPos := hwg_Hiword( lParam )
      ::lCaptured := .T.
      hwg_SetCapture( ::handle )
      ::lMoved := .F.
      ::xPos := xPos
      ::yPos := yPos

   ELSEIF msg == WM_LBUTTONUP
      ::lCaptured := .F.
      hwg_Releasecapture()
      IF !::lMoved
         IF ::nSelected != ::nOver
            ::nSelected := ::nOver
            lRedraw := .T.
            IF !Empty( ::bClick )
               Eval( ::bClick, Self, ::nSelected )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF msg == WM_DESTROY
      ::End()
   ENDIF

   IF lRedraw
      hwg_Redrawwindow( ::handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ENDIF

   RETURN - 1

METHOD Paint() CLASS HLenta

#ifdef __PLATFORM__UNIX
   LOCAL hDC := hwg_Getdc( ::handle )
#else
   LOCAL pps := hwg_Definepaintstru()
   LOCAL hDC := hwg_Beginpaint( ::handle, pps )
#endif
   LOCAL i, y1, nCurr, nItemSize := ::nItemSize, oStyle

   IF ::bPaint != Nil
      Eval( ::bPaint, Self, hDC )
   ELSE

      IF !Empty( ::aItems )
         IF ::oFont != Nil
            hwg_Selectobject( hDC, ::oFont:handle )
         ENDIF
         y1 := Int( ::nShift % nItemSize )
         IF y1 > 0; y1 := nItemSize - y1; ENDIF
         IF ::lVertical
            IF y1 > 0
               ::aItemStyle[1]:Draw( hDC, 0, 0, ::nWidth, y1 )
            ENDIF
            i := 1
            DO WHILE y1 + nItemSize <= ::nHeight .AND. ( nCurr := i + ::nFirst - 1 ) <= Len( ::aItems )
               oStyle := Iif( nCurr == ::nSelected .AND. ::aItemStyle[3] != Nil, ::aItemStyle[3], ;
                  Iif( nCurr == ::nOver .AND. ::aItemStyle[2] != Nil, ::aItemStyle[2], ::aItemStyle[1] ) )
               oStyle:Draw( hDC, 0, y1, ::nWidth, y1 + nItemSize )

               hwg_Settransparentmode( hDC, .T. )
               hwg_Drawtext( hDC, ::aItems[nCurr,1], 4, y1+4, ::nWidth-4, y1+nItemSize-4, ;
                  DT_LEFT + DT_VCENTER + DT_SINGLELINE )
               hwg_Settransparentmode( hDC, .F. )

               y1 += nItemSize
               i ++
            ENDDO
            IF y1 < ::nHeight
               ::aItemStyle[1]:Draw( hDC, 0, y1, ::nWidth, ::nHeight )
            ENDIF
         ELSE
         ENDIF
      ENDIF

   ENDIF

#ifdef __PLATFORM__UNIX
      hwg_Releasedc( ::handle, hDC )
#else
      hwg_Endpaint( ::handle, pps )
#endif

   RETURN Nil

METHOD Drag( xPos, yPos ) CLASS HLenta

   //hwg_Writelog( "   " + Ltrim(Str(yPos)) + " " + Ltrim(Str(::yPos)) + " " + Ltrim(Str(::nShift)) )
   IF ( ::lVertical .AND. Abs( yPos-::yPos ) > 2 ) .OR. ( !::lVertical .AND. Abs( xPos-::xPos ) > 2 )
      ::lMoved := .T.
      ::nOver := 0
      ::nShift += Int( Iif( ::lVertical, ( ::yPos-yPos ), ( ::xPos-xPos ) ) * ::nDragKoef )
      ::xPos := xPos; ::yPos := yPos
      IF ::nShift < 0
         ::nShift := 0
      ELSEIF ::nShift + ::nHeight > Int( Len( ::aItems ) * ::nItemSize ) + 2
         ::nShift := Max( 0, Int( Len( ::aItems ) * ::nItemSize ) - ::nHeight + 2 )
      ENDIF
      ::nFirst := Int( ::nShift/::nItemSize )
      ::nFirst += Iif( ::nShift > ::nFirst * ::nItemSize, 2, 1 )
      //hwg_Writelog( Ltrim(Str(::nShift)) + " " + Ltrim(Str(::nFirst))  )
      hwg_Redrawwindow( ::handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ENDIF

   RETURN Nil

METHOD Value( nValue ) CLASS HLenta

   IF nValue != Nil .AND. nValue >= 0 .AND. !Empty( ::aItems ) .AND. nValue <= Len( ::aItems )
      ::nSelected := nValue
      hwg_Redrawwindow( ::handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   ELSE
      nValue := ::nSelected
   ENDIF

   RETURN nValue
