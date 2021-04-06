/*
 * A plugin for Sounds.
 */

#include "hwgui.ch"

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

#define TOPPANE_HEIGHT  28

STATIC oDlgAcco
STATIC accords := {}, lAccordsChg := .F.
STATIC cPlugDir

MEMVAR oMsg, aMsgs, aPlugMenu

FUNCTION Plug_accords()

   cPlugDir := hb_DirBase() + "plugins" + hb_ps()
   Accords_ReadIni()
   AAdd( aPlugMenu, { "Accords", {||dlgAccords()} } )

   RETURN Nil

STATIC FUNCTION DlgSetAccord( oDlgParent, arr )

   LOCAL oMainWindow := HWindow():GetMain()
   LOCAL oDlg, oPanel, i, n1, n2, n3, n4
   STATIC aAllNotes

   INIT DIALOG oDlg TITLE "SetAccord" BACKCOLOR CLR_DLGBACK ;
      AT 150, 150 SIZE 300, 240 FONT oDlgAcco:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX

   oDlg:oParent := oDlgParent

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DLGHEA ;
      TEXT "Set Accord" COORS 20 BTN_CLOSE

   IF Empty( aAllNotes )
      aAllNotes := Array( 89 )
      aAllNotes[1] := " "
      FOR i := 2 TO Len( aAllNotes )
         aAllNotes[i] := Note2Text( i-1 )
      NEXT
   ENDIF
   n1 := Iif( arr[1]==0, 1, arr[1]+1 )
   n2 := Iif( arr[2]==0, 1, arr[2]+1 )
   n3 := Iif( arr[2]==3, 1, arr[3]+1 )
   n4 := Iif( arr[2]==4, 1, arr[4]+1 )

   @ 30,50 GET COMBOBOX n1 ITEMS aAllNotes SIZE 90, 28
   @ 180,50 GET COMBOBOX n2 ITEMS aAllNotes SIZE 90, 28
   @ 30,100 GET COMBOBOX n4 ITEMS aAllNotes SIZE 90, 28
   @ 180,100 GET COMBOBOX n4 ITEMS aAllNotes SIZE 90, 28

   oPanel:SetSysbtnColor( CLR_WHITE, CLR_TOPDARK )

   @ 30, 200 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[13] COLOR CLR_BLACK ;
      ON CLICK {|| Iif( n1>1.AND.n2>2, (oDlg:lResult:=.T.,hwg_EndDialog()), .F. ) }
   ATail(oDlg:aControls):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   @ 170, 200 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[14] COLOR CLR_BLACK ;
      ON CLICK {|| hwg_EndDialog() }
   ATail(oDlg:aControls):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   ACTIVATE DIALOG oDlg

   IF oDlg:lResult
      arr[1] := n1-1; arr[2] := n2-1; arr[3] := n3-1; arr[4] := n4-1
      lAccordsChg := .T.
   ENDIF

   RETURN oDlg:lResult

STATIC FUNCTION DlgAccords()

   LOCAL oMainWindow := HWindow():GetMain()
   LOCAL oPanel, oBtn, i, j, cName, nPos, nPos2
   LOCAL bAcco := {|o|
      LOCAL arr, i
      IF Len(accords) >= o:cargo .AND. !Empty(accords[o:cargo])
         StopAllSounds()
         cName := accords[o:cargo]
         cName := Iif( (nPos := At( '[', cName )) > 0, Left( cName,nPos-1 ), cName )
         PlayKey( hb_ATokens( cName, '-' ) )
      ELSE
         arr := { 0, 0, 0, 0 }
         IF dlgSetAccord( oDlgAcco, arr )
            FOR i := Len( accords ) TO o:cargo
               Aadd( accords, "" )
            NEXT
            accords[o:cargo] := Note2Text(arr[1]) + '-' + Note2Text(arr[2]) + ;
               Iif( arr[3]==0,"",'-' + Note2Text(arr[3]) + Iif( arr[4]==0,"",'-' + Note2Text(arr[4]) ) )
            o:title := accords[o:cargo]
            hwg_Redrawwindow( o:handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
         ENDIF
      ENDIF
      RETURN .T.
      }

   IF !Empty( oDlgAcco )
      RETURN Nil
   ENDIF

   INIT DIALOG oDlgAcco TITLE "Accords" BACKCOLOR CLR_DLGBACK ;
      AT Int(oMainWindow:nWidth*0.7), Int(oMainWindow:nHeight*0.6) SIZE 400, 410 ;
      FONT oMainWindow:oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {|| oDlgAcco := Nil}

   oDlgAcco:oParent := oMainWindow

   ADD HEADER PANEL oPanel HEIGHT TOPPANE_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DLGHEA ;
      TEXT aMsgs[76] COORS 20 BTN_CLOSE

   FOR i := 1 TO 7
       FOR j := 1 TO 3
          cName := Iif( !Empty(accords).AND.Len(accords)>=(i-1)*3+j,accords[(i-1)*3+j],"" )
          IF ( nPos := At( '[', cName ) ) > 0 .AND. ( nPos2 := hb_At( ']', cName, nPos ) ) > 0
             cName := Substr( cName, nPos+1, nPos2-nPos-1 )
          ENDIF
          @ 10+(j-1)*130, TOPPANE_HEIGHT+20+(i-1)*44 OWNERBUTTON oBtn SIZE 120, 32 ;
             TEXT cName ;
             COLOR CLR_BLACK ON CLICK bAcco
          oBtn:aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle
          oBtn:cargo := (i-1)*3+j
       NEXT
   NEXT

   oPanel:SetSysbtnColor( CLR_WHITE, CLR_TOPDARK )

   @ 140, oDlgAcco:nHeight-40 OWNERBUTTON SIZE 100, 32 TEXT aMsgs[11] COLOR CLR_BLACK ;
      ON CLICK {|| oDlgAcco:Close() }
   ATail(oDlgAcco:aControls):aStyle := oMainWindow:oPaneTop:aControls[1]:aStyle

   ACTIVATE DIALOG oDlgAcco NOMODAL

   RETURN NIL

STATIC FUNCTION Accords_ReadIni()

   LOCAL cFile := cPlugDir + "accords.ini"
   LOCAL hIni := _iniRead( cFile ), aIni, nSect, aSect, cTemp

   IF !Empty( hIni )
      aIni := hb_HKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[ nSect ] ) == "ACCORDS"
            IF !Empty( aSect := hIni[ aIni[ nSect ] ] )
               IF hb_HHasKey( aSect, cTemp := "list" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  accords := hb_ATokens( cTemp, ';' )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION Accords_WriteIni()

   LOCAL cFile := cPlugDir + "accords.ini"
   LOCAL s, cLangs, i

   IF !Empty( accords )
      cLangs := ""
      FOR i := 1 TO Len( accords )
         cLangs += Iif( i==1,"",";" ) + accords[i]
      NEXT
      s := Chr(10) + "[ACCORDS]" + Chr(10) + "list=" + cLangs + Chr(10)
   ENDIF

   hb_MemoWrit( cFile, s )

   RETURN Nil
