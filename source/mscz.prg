/*
 * Mscz (MuseScore) parsing
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"
#include "hxml.ch"

#define ERR_HEAD      1
#define ERR_XML       2
#define ERR_NOPART    3
#define ERR_NOMEASURE 4

#define VERSION     "0.2"

STATIC aDurType := { "whole", "half", "quarter", "eighth", "16th" }
STATIC aTpc := { 14, 9, 16, 11, 18, 13, 8, 15, 10, 17, 12, 19 }

CLASS Mscz

   CLASS VAR nError SHARED
   DATA oXML
   DATA nVersion
   DATA oNodePart, oMeasure
   DATA nDivisions
   DATA nTieId    INIT 0
   DATA nMeasure

   METHOD New( nVer, oLM )
   METHOD Open( cFile )
   METHOD GetTracks()
   METHOD ToLM( oLM, cPartId )
   METHOD AddMeasure( oLM )
   METHOD AddNote( nNote, nDur, cAttr )
   METHOD Save( cFile )

ENDCLASS

METHOD New( nVer, oLM ) CLASS Mscz

   LOCAL oNode0, oNode1, oNode2

   ::oXML := HXMLDoc():New()
   ::nVersion := Iif( Empty(nVer), 2, nVer )
   ::oXML:Add( oNode0 := HXMLNode():New( "museScore",, { { "version", Ltrim(Str(::nVersion,3,1)) } } ) )
   IF ::nVersion > 1
      oNode0:Add( oNode0 := HXMLNode():New( "Score" ) )
   ENDIF

   oNode0:Add( oNode1 := HXMLNode():New( "Part" ) )
   oNode1:Add( oNode1 := HXMLNode():New( "Staff",, { { "id", "1" } } ) )
   oNode1:Add( HXMLNode():New( "StaffType",, { { "group", "pitched" } } ) )

   oNode0:Add( ::oNodePart := HXMLNode():New( "Staff",, { { "id", "1" } } ) )
   IF !Empty( oLM ) .AND. !Empty( oLM:cTitle )
      ::oNodePart:Add( oNode1 := HXMLNode():New( "VBox" ) )
      oNode1:Add( oNode2 := HXMLNode():New( "Text" ) )
      oNode2:Add( HXMLNode():New( "style",,, "Title" ) )
      oNode2:Add( HXMLNode():New( "text",,, oLM:cTitle ) )
   ENDIF
   ::nMeasure := 0

   RETURN Self

METHOD Open( cFile ) CLASS Mscz

   LOCAL hUnzip, nSize, nErr, cBuff, oImp

   IF hb_fnameExt( cFile ) == ".mscz"
      IF !Empty( hUnzip := hb_unzipOpen( cFile ) )
         nErr := hb_unzipFileFirst( hUnzip )
         DO WHILE nErr == 0
            hb_unzipFileInfo( hUnzip, @cFile,,,,,, @nSize, )
            IF !( '/' $ cFile ) .AND. !( '\' $ cFile ) .AND. !Empty( hb_fnameExt( cFile ) )
               IF hb_unzipFileOpen( hUnzip, Nil ) == 0

                  cBuff := Space( nSize + 1 )
                  nSize := hb_unzipFileRead( hUnzip, @cBuff, Len( cBuff ) )
                  hb_unzipFileClose( hUnzip )
                  EXIT
               ENDIF
            ENDIF
            nErr := hb_unzipFileNext( hUnzip )
         ENDDO
         hb_unzipClose( hUnzip )
      ENDIF
   ELSE
      cBuff := MemoRead( cFile )
   ENDIF
   IF Empty( cBuff ) .OR. !(Left( cBuff, 6 ) == "<?xml ")
      ::nError := ERR_HEAD
      RETURN Nil
   ENDIF

   oImp := HXMLDoc():Read( , cBuff )
   IF Empty( oImp) .OR. Empty( oImp:aItems ) .OR. !( oImp:aItems[1]:title == "museScore" )
      ::nError := ERR_HEAD
      RETURN Nil
   ENDIF

   ::nVersion := Val( oImp:aItems[1]:GetAttribute( "version", "C" ) )
   ::oXML := oImp

   RETURN Self

METHOD GetTracks() CLASS Mscz

   LOCAL arr := {}, n, i, j, oNode0, oNode1, oNode2, oNode3, cPartId, lNote

   FOR n := 1 TO Len( ::oXML:aItems[1]:aItems )
      IF Lower( ::oXML:aItems[1]:aItems[n]:title ) == "score"
         oNode0 := ::oXML:aItems[1]:aItems[n]
         EXIT
      ENDIF
   NEXT

   IF ::nVersion < 2 .AND. Empty( oNode0 )
      oNode0 := ::oXML:aItems[1]
   ENDIF
   FOR n := 1 TO Len( oNode0:aItems )
      IF Lower( (oNode1 := oNode0:aItems[n]):title ) == "staff"
         cPartId := oNode1:GetAttribute( "id", "C" )
         lNote := .F.
         FOR i := 1 TO Len( oNode1:aItems )
            IF Lower( (oNode2 := oNode1:aItems[i]):title ) == "measure"
               IF !Empty( oNode3 := oNode2:Find( "voice" ) )
                  oNode2 := oNode3
               ENDIF
               FOR j := 1 TO Len( oNode2:aItems )
                  IF Lower( oNode2:aItems[j]:title ) == "chord"
                     lNote := .T.
                     EXIT
                  ENDIF
               NEXT
               IF lNote
                  EXIT
               ENDIF
            ENDIF
         NEXT
         IF lNote
            AAdd( arr, {cPartId} )
         ENDIF
      ENDIF
   NEXT

   RETURN arr

METHOD ToLM( oLM, cPartId ) CLASS Mscz

   LOCAL arr := {}, oNode0, oNode1, oNode2, oNode3, oNode4, n, i, j, j1, j2, nm, cTitle, cTmp, nPos, nType
   LOCAL nNote, nDur, l1_5, lArpeggio, cTi1, cTi2, aChord

   FOR n := 1 TO Len( ::oXML:aItems[1]:aItems )
      IF Lower( ::oXML:aItems[1]:aItems[n]:title ) == "score"
         oNode0 := ::oXML:aItems[1]:aItems[n]
         EXIT
      ENDIF
   NEXT

   IF ::nVersion < 2 .AND. Empty( oNode0 )
      oNode0 := ::oXML:aItems[1]
   ENDIF

   FOR n := 1 TO Len( oNode0:aItems )
      IF Lower( (oNode1 := oNode0:aItems[n]):title ) == "staff" .AND. ;
            cPartId == oNode1:GetAttribute( "id", "C" )
         FOR i := 1 TO Len( oNode1:aItems )
            IF ( cTitle := Lower( (oNode2 := oNode1:aItems[i]):title ) ) == "measure"
               nm := oNode1:aItems[i]:GetAttribute( "number", "N", 0 )
               IF Len( arr ) > 0
                  noteSetAttr( ATail(arr), "t" )
               ENDIF
               IF !Empty( oNode3 := oNode2:Find( "voice" ) )
                  oNode2 := oNode3
               ENDIF
               FOR j := 1 TO Len( oNode2:aItems )
                  IF ( cTitle := Lower( (oNode3 := oNode2:aItems[j]):title ) ) == "chord"
                     nNote := nDur := 0
                     l1_5 := lArpeggio := .F.
                     cTi1 := cTi2 := ""
                     aChord := Nil
                     FOR j1 := 1 TO Len( oNode3:aItems )
                        IF ( cTitle := Lower( (oNode4 := oNode3:aItems[j1]):title ) ) == "durationtype"
                           nDur := Ascan( aDurType, AllTrim(oNode4:aItems[1]) )
                        ELSEIF cTitle == "note"
                           FOR j2 := 1 TO Len( oNode4:aItems )
                              IF ( cTitle := Lower( oNode4:aItems[j2]:title ) ) == "pitch"
                                 IF nNote > 0
                                    IF Empty( aChord )
                                       aChord := { nNote }
                                    ENDIF
                                 ENDIF
                                 nNote := Val( AllTrim( oNode4:aItems[j2]:aItems[1] ) ) - 23
                                 IF !Empty( aChord )
                                    AAdd( aChord, nNote )
                                 ENDIF
                              ELSEIF cTitle == "tie"
                                 cTi1 := oNode4:aItems[j2]:GetAttribute( "id", "C" )
                              ELSEIF cTitle == "endspanner"
                                 cTi2 := oNode4:aItems[j2]:GetAttribute( "id", "C" )
                              ENDIF
                           NEXT
                        ELSEIF cTitle == "dots"
                           l1_5 := .T.
                        ELSEIF cTitle == "arpeggio"
                        ENDIF
                     NEXT
                     Aadd( arr, { Iif( !Empty( aChord ), aChord, nNote ), Iif( l1_5, nDur+0.5, nDur ) } )
                     IF Empty( cTi2 )
                        IF Len( arr ) > 1 .AND. Len( arr[Len(arr)-1] ) > 2 .AND. ;
                           ( nPos := At( "ti1_", arr[Len(arr)-1,3] ) ) > 0
                           j2 := hb_At( '/', arr[Len(arr)-1,3], nPos )
                           arr[Len(arr)-1,3] := Left( arr[Len(arr)-1,3],nPos-1 ) + ;
                              Substr( arr[Len(arr)-1,3], j2+1 )
                        ENDIF
                     ELSE
                        IF noteCheckAttr( arr[Len(arr)-1], "ti1_"+cTi2 )
                           noteChangeAttr( arr[Len(arr)-1], "ti1_"+cTi2, "ti1" )
                           NoteSetAttr( arr[Len(arr)], "ti2" )
                        ENDIF
                     ENDIF
                     IF !Empty( cTi1 )
                        noteSetAttr( arr[Len(arr)], "ti1_"+cTi1 )
                     ENDIF
                     IF lArpeggio
                        noteSetAttr( arr[Len(arr)], "arp" )
                     ENDIF

                  ELSEIF cTitle == "rest"
                     nDur := 0
                     FOR j1 := 1 TO Len( oNode3:aItems )
                        IF ( cTitle := Lower( (oNode4 := oNode3:aItems[j1]):title ) ) == "durationtype"
                           nDur := Ascan( aDurType, AllTrim(oNode4:aItems[1]) )
                        ENDIF
                     NEXT
                     IF nDur > 0
                        Aadd( arr, { 0, nDur } )
                     ENDIF

                  ELSEIF cTitle == "keysig"
                     IF nm == 1
                        FOR j1 := 1 TO Len( oNode3:aItems )
                           IF Lower( oNode3:aItems[j1]:title ) == "accidental"
                              oLM:nKey := Val( AllTrim( oNode3:aItems[j1]:aItems[1] ) )
                           ENDIF
                        NEXT
                     ENDIF
                  ELSEIF cTitle == "timesig"
                     IF nm == 1
                        FOR j1 := 1 TO Len( oNode3:aItems )
                           cTmp := AllTrim( oNode3:aItems[j1]:aItems[1] )
                           IF ( cTitle := Lower( oNode3:aItems[j1]:title ) ) == "sign"
                              oLM:aMetre[1] := Val( cTmp )
                           ELSEIF cTitle == "sigd"
                              oLM:aMetre[2] := Val( cTmp )
                           ENDIF
                        NEXT
                     ENDIF
                  ELSEIF cTitle == "tempo"
                     IF nm == 1
                        FOR j1 := 1 TO Len( oNode3:aItems )
                           IF Lower( oNode3:aItems[j1]:title ) == "tempo"
                              oLM:nBPM := Int( Val( AllTrim( oNode3:aItems[j1]:aItems[1] ) ) * 60 )
                           ENDIF
                        NEXT
                     ENDIF
                  ENDIF
               NEXT
            ELSEIF cTitle == "vbox"
               FOR j := 1 TO Len( oNode2:aItems )
                  IF Lower( (oNode3 := oNode2:aItems[j]):title ) == "text"
                     nType := 0
                     FOR j1 := 1 TO Len( oNode3:aItems )
                        IF ( cTitle := Lower( (oNode4 := oNode3:aItems[j1]):title ) ) == "style"
                           IF Lower(oNode4:aItems[1]) == "title"
                              nType := 1
                           ENDIF
                        ELSEIF cTitle == "text"
                           IF nType == 1
                              FOR j2 := 1 TO Len( oNode4:aItems )
                                 IF Valtype( oNode4:aItems[j2] ) == "C"
                                    oLM:cTitle := oNode4:aItems[j2]
                                 ENDIF
                              NEXT
                           ENDIF
                        ENDIF
                     NEXT
                  ENDIF
               NEXT
            ENDIF
         NEXT
      ENDIF
   NEXT

   IF Len( arr ) > 0
      noteSetAttr( ATail(arr), "t" )
   ENDIF
   oLM:aNotes := arr

   RETURN Nil

METHOD AddMeasure( oLM ) CLASS Mscz

   LOCAL oNode1

   IF Empty( ::oNodePart )
      ::nError := ERR_NOPART
      RETURN .F.
   ENDIF

   ::oNodePart:Add( ::oMeasure := HXMLNode():New( "Measure",, ;
      { { "number", Ltrim(Str(++::nMeasure)) } } ) )
   IF ::nVersion > 3
      ::oMeasure:Add( oNode1 := HXMLNode():New( "voice" ) )
      ::oMeasure := oNode1
   ENDIF
   IF Len( ::oNodePart:aItems ) == 1
      ::oMeasure:Add( oNode1 := HXMLNode():New( "Clef" ) )
      oNode1:Add( HXMLNode():New( "concertClefType",,, Iif(oLM:lBas,"F","G") ) )
      oNode1:Add( HXMLNode():New( "transposingClefType",,, Iif(oLM:lBas,"F","G") ) )

      ::oMeasure:Add( oNode1 := HXMLNode():New( "KeySig" ) )
      oNode1:Add( HXMLNode():New( "accidental",,, LTrim(Str(oLM:nKey)) ) )

      ::oMeasure:Add( oNode1 := HXMLNode():New( "TimeSig" ) )
      oNode1:Add( HXMLNode():New( "sigN",,, Ltrim(Str(oLM:aMetre[1])) ) )
      oNode1:Add( HXMLNode():New( "sigD",,, Ltrim(Str(oLM:aMetre[2])) ) )

      ::oMeasure:Add( oNode1 := HXMLNode():New( "Tempo" ) )
      oNode1:Add( HXMLNode():New( "tempo",,, LTrim(Str(oLM:nBPM/60,7,5)) ) )

   ENDIF

   RETURN .T.

METHOD AddNote( xNote, nDur, cAttr ) CLASS Mscz

   LOCAL oNode1, oNode2, n, l1_5, j, nNote

   IF Empty( ::oNodePart )
      ::nError := ERR_NOPART
      RETURN .F.
   ENDIF
   IF Empty( ::oMeasure )
      ::nError := ERR_NOMEASURE
      RETURN .F.
   ENDIF
   IF cAttr == Nil; cAttr := ""; ENDIF

   l1_5 := ( nDur - Int( nDur ) == 0.5 )
   IF Valtype(xNote) == "N" .AND. xNote == 0
      ::oMeasure:Add( oNode1 := HXMLNode():New( "Rest" ) )
      oNode1:Add( HXMLNode():New( "durationType",,, aDurType[nDur] ) )
   ELSE
      ::oMeasure:Add( oNode1 := HXMLNode():New( "Chord" ) )
      oNode1:Add( HXMLNode():New( "durationType",,, aDurType[nDur] ) )
      j := 0
      DO WHILE .T.
         nNote := Iif( Valtype(xNote) == "A", xNote[++j], xNote )
         n := Int( Iif( (n := (nNote % 12)) == 0, 12, n ) )
         nNote += 23

         IF l1_5
            oNode1:Add( HXMLNode():New( "dots",,, "1" ) )
         ENDIF
         oNode1:Add( oNode2 := HXMLNode():New( "Note" ) )
         IF "ti2" $ cAttr
            oNode2:Add( HXMLNode():New( "endSpanner",, { {"id", Ltrim(Str(::nTieId))} } ) )
         ENDIF
         IF "ti1" $ cAttr
            ::nTieId ++
            oNode2:Add( HXMLNode():New( "Tie",, { {"id", Ltrim(Str(::nTieId))} } ) )
         ENDIF
         oNode2:Add( HXMLNode():New( "pitch",,, LTrim(Str(nNote)) ) )
         oNode2:Add( HXMLNode():New( "tpc",,, LTrim(Str(aTpc[n])) ) )
         IF Valtype(xNote) == "N" .OR. j == Len(xNote)
            EXIT
         ENDIF
      ENDDO
      IF "arp" $ cAttr
         oNode1:Add( oNode2 := HXMLNode():New( "Arpeggio" ) )
         oNode2:Add( HXMLNode():New( "subtype",,, "0" ) )
      ENDIF
   ENDIF

   RETURN .T.

METHOD Save( cFile ) CLASS Mscz

   LOCAL hZip, s

   IF Lower( hb_fnameExt( cFile ) ) == ".mscz"
      hZip := hb_zipOpen( cFile )
      IF !Empty( hZip )

         hb_zipFileCreate( hZip, "META-INF",,,, 0x10 )
         hb_zipFileClose( hZip )

         s := '<?xml version="1.0" encoding="UTF-8"?>' + Chr(10) + '<container>' ;
             + Chr(10) + ' <rootfiles>' + Chr(10) + '  <rootfile full-path="score.mscx"></rootfile>' ;
             + Chr(10) + ' </rootfiles>' + Chr(10) + '</container>'
         hb_zipFileCreate( hZip, "META-INF/container.xml" )
         hb_zipFileWrite( hZip, s, Len(s) )
         hb_zipFileClose( hZip )

         s := ::oXML:Save()
         hb_zipFileCreate( hZip, "score.mscx" )
         hb_zipFileWrite( hZip, s, Len(s) )
         hb_zipFileClose( hZip )

         hb_zipClose( hZip )
      ENDIF
   ELSE
      ::oXML:Save( cFile )
   ENDIF

   RETURN Nil
