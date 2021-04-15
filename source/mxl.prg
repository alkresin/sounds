/*
 * Mxl (musicXML) parsing
 */

#include "hbclass.ch"
#include "hxml.ch"

#define ERR_HEAD      1
#define ERR_XML       2
#define ERR_NOPART    3
#define ERR_NOMEASURE 4

#define VERSION     "0.2"

STATIC aNotesEn := { 'C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb', 'G', 'Ab', 'A', 'Bb', 'B' }
STATIC aDurType := { "whole", "half", "quarter", "eighth", "16th" }
STATIC aDivi := { 1, 1, 1, 2, 4, 8, 16 }

CLASS Mxl

   CLASS VAR nError SHARED
   DATA oXML
   DATA oNodePart, oMeasure
   DATA nDivisions

   METHOD New( oLM )
   METHOD Open( cFile )
   METHOD GetTracks()
   METHOD ToLM( oLM, cPartId )
   METHOD AddMeasure( oLM )
   METHOD AddNote( nNote, nDur, cAttr )
   METHOD Save( cFile )
ENDCLASS

METHOD New( oLM ) CLASS Mxl

   LOCAL oNode0, oNode1, oNode2, xTemp

   ::oXML := HXMLDoc():New()
   ::oXML:Add( oNode0 := HXMLNode():New( "score-partwise",, { { "version", "3.1" } } ) )

   IF !Empty( oLM ) .AND. !Empty( oLM:cTitle )
      oNode0:Add( oNode1 := HXMLNode():New( "work" ) )
      oNode1:Add( oNode2 := HXMLNode():New( "work-title",,, oLM:cTitle ) )
   ENDIF

   oNode0:Add( oNode1 := HXMLNode():New( "identification" ) )
   oNode1:Add( oNode2 := HXMLNode():New( "encoding" ) )
   oNode2:Add( HXMLNode():New( "software",,, "hbMXL library "+VERSION ) )
   oNode2:Add( HXMLNode():New( "encoding-date",,, Left(xTemp:=Dtos(Date()),4)+'-'+Substr(xTemp,5,2)+'-'+Right(xTemp,2) ) )

   oNode0:Add( oNode1 := HXMLNode():New( "part-list" ) )
   oNode1:Add( oNode2 := HXMLNode():New( "score-part",, { { "id", "P1" } } ) )
   oNode2:Add( HXMLNode():New( "part-name",,, "Music" ) )

   oNode0:Add( ::oNodePart := HXMLNode():New( "part",, { { "id", "P1" } } ) )

   RETURN Self

METHOD Open( cFile ) CLASS Mxl

   LOCAL hUnzip, nSize, nErr, cBuff, oImp

   IF hb_fnameExt( cFile ) == ".mxl"
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
   IF Empty( oImp) .OR. Empty( oImp:aItems ) .OR. !( oImp:aItems[1]:title == "score-partwise" )
      ::nError := ERR_HEAD
      RETURN Nil
   ENDIF

   ::oXML := oImp

   RETURN Self

METHOD GetTracks() CLASS Mxl

   LOCAL arr := {}, n, i, j, oNode1, oNode2, cPartId, lNote

   FOR n := 1 TO Len( ::oXML:aItems[1]:aItems )
      IF (oNode1 := ::oXML:aItems[1]:aItems[n]):title == "part"
         cPartId := oNode1:GetAttribute( "id", "C" )
         lNote := .F.
         FOR i := 1 TO Len( oNode1:aItems )
            IF oNode1:aItems[i]:title == "measure"
               FOR j := 1 TO Len( oNode1:aItems[i]:aItems )
                  IF (oNode2 := oNode1:aItems[i]:aItems[j]):title == "note"
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

METHOD ToLM( oLM, cPartId ) CLASS Mxl

   LOCAL oNode1, oNode2
   LOCAL n, i, j, j1, j2, nm, cTitle, cTmp, nNote, nOct, nDur, nDur1
   LOCAL lTi1, lTi2, lChord, lArpeggio, arr := {}

   FOR n := 1 TO Len( ::oXML:aItems[1]:aItems )
      IF (oNode1 := ::oXML:aItems[1]:aItems[n]):title == "part" .AND. ;
         oNode1:GetAttribute( "id", "C" ) == cPartId
         FOR i := 1 TO Len( oNode1:aItems )
            IF oNode1:aItems[i]:title == "measure"
               nm := oNode1:aItems[i]:GetAttribute( "number", "N", 0 )
               IF Len( arr ) > 0
                  noteSetAttr( ATail(arr), "t" )
               ENDIF
               FOR j := 1 TO Len( oNode1:aItems[i]:aItems )
                  IF (oNode2 := oNode1:aItems[i]:aItems[j]):title == "note"
                     nNote := nDur := nDur1 := 0
                     lTi1 := lTi2 := lChord := lArpeggio := .F.
                     FOR j1 := 1 TO Len( oNode2:aItems )
                        IF (cTitle := oNode2:aItems[j1]:title ) == "pitch"
                           FOR j2 := 1 TO Len( oNode2:aItems[j1]:aItems )
                              cTmp := AllTrim( oNode2:aItems[j1]:aItems[j2]:aItems[1] )
                              IF (cTitle := oNode2:aItems[j1]:aItems[j2]:title ) == "step"
                                 nNote := Ascan( aNotesEn, {|s1|s1==cTmp} )
                              ELSEIF cTitle == "octave"
                                 nOct := Val( cTmp )
                              ELSEIF cTitle == "alter"
                                 nNote += Val( cTmp )
                              ENDIF
                           NEXT
                        ELSEIF cTitle == "tie"
                           IF ( cTmp := oNode2:aItems[j1]:GetAttribute( "type","C" ) ) == "start"
                              lTi1 := .T.
                           ELSEIF cTmp == "stop"
                              lTi2 := .T.
                           ENDIF
                        ELSEIF cTitle == "duration"
                           nDur1 := Val( oNode2:aItems[j1]:aItems[1] )
                        ELSEIF cTitle == "type"
                           nDur := Ascan( aDurType, AllTrim(oNode2:aItems[j1]:aItems[1]) )
                        ELSEIF cTitle == "chord"
                           lChord := .T.
                        ELSEIF cTitle == "notations"
                           FOR j2 := 1 TO Len( oNode2:aItems[j1]:aItems )
                              IF (cTitle := oNode2:aItems[j1]:aItems[j2]:title ) == "arpeggiate"
                                 lArpeggio := .T.
                              ENDIF
                           NEXT
                        ENDIF
                     NEXT
                     IF nDur > 0
                        IF nNote > 0
                           nNote := (nOct-1) * 12 + nNote
                        ENDIF
                        IF nDur1 > 0 .AND. !Empty( ::nDivisions )
                           j1 := Ascan( aDivi, ::nDivisions )
                           IF j1 > 0 .AND. j1 >= nDur
                              IF Abs( nDur1 - (2 ** ( j1 - nDur )) * 1.5 ) < 0.1
                                 nDur += 0.5
                              ENDIF
                           ENDIF
                        ENDIF
                        IF lChord
                           IF Valtype( ATail( arr )[1] ) == "A"
                              AAdd( ATail( arr )[1], nNote )
                           ELSE
                              ATail( arr )[1] := { ATail( arr )[1], nNote }
                           ENDIF
                        ELSE
                           Aadd( arr, { nNote, nDur } )
                           IF lTi2
                              noteSetAttr( ATail(arr), "ti2" )
                           ENDIF
                           IF lTi1
                              noteSetAttr( ATail(arr), "ti1" )
                           ENDIF
                        ENDIF
                        IF lArpeggio
                           noteSetAttr( ATail(arr), "arp" )
                        ENDIF
                     ENDIF

                  ELSEIF oNode2:title == "attributes"
                     FOR j1 := 1 TO Len( oNode2:aItems )
                        IF (cTitle := oNode2:aItems[j1]:title ) == "time"
                           IF nm == 1
                              FOR j2 := 1 TO Len( oNode2:aItems[j1]:aItems )
                                 cTmp := AllTrim( oNode2:aItems[j1]:aItems[j2]:aItems[1] )
                                 IF (cTitle := oNode2:aItems[j1]:aItems[j2]:title ) == "beats"
                                    oLM:aMetre[1] := Val( cTmp )
                                 ELSEIF cTitle == "beat-type"
                                    oLM:aMetre[2] := Val( cTmp )
                                 ENDIF
                              NEXT
                           ENDIF
                        ELSEIF cTitle == "divisions"
                           ::nDivisions := Val( AllTrim(oNode2:aItems[j1]:aItems[1]) )
                        ELSEIF cTitle == "key"
                           FOR j2 := 1 TO Len( oNode2:aItems[j1]:aItems )
                              cTmp := AllTrim( oNode2:aItems[j1]:aItems[j2]:aItems[1] )
                              IF (cTitle := oNode2:aItems[j1]:aItems[j2]:title ) == "fifths"
                                 oLM:nKey := Val( cTmp )
                              ENDIF
                           NEXT
                        ENDIF
                     NEXT
                  ENDIF
               NEXT
            ENDIF
         NEXT
      ELSEIF oNode1:title == "work"
         IF !Empty( oNode2 := oNode1:Find( "work-title" ) ) .AND. !Empty( oNode2:aItems )
            oLM:cTitle := AllTrim( oNode2:aItems[1] )
         ENDIF
      ENDIF
   NEXT
   IF Len( arr ) > 0
      noteSetAttr( ATail(arr), "t" )
   ENDIF
   oLM:aNotes := arr

   RETURN Nil

METHOD AddMeasure( oLM ) CLASS Mxl

   LOCAL oAttr, oNode, i, nMin := 1, aNotes := oLM:aNotes

   IF Empty( ::oNodePart )
      ::nError := ERR_NOPART
      RETURN .F.
   ENDIF

   ::oNodePart:Add( ::oMeasure := HXMLNode():New( "measure",, ;
      { { "number", Ltrim(Str(Len(::oNodePart:aItems)+1)) } } ) )
   IF Len( ::oNodePart:aItems ) == 1
      FOR i := 1 TO Len( aNotes )
         nMin := Max( nMin, aNotes[i,2] )
      NEXT
      ::nDivisions := Int( 2 ** Max( 0, nMin - 3 ) )
      ::oMeasure:Add( oAttr := HXMLNode():New( "attributes" ) )
      oAttr:Add( HXMLNode():New( "divisions",,, LTrim(Str(::nDivisions)) ) )
      oAttr:Add( oNode := HXMLNode():New( "key" ) )
      oNode:Add( HXMLNode():New( "fifths",,, LTrim(Str(oLM:nKey)) ) )
      oAttr:Add( oNode := HXMLNode():New( "time" ) )
      oNode:Add( HXMLNode():New( "beats",,, Ltrim(Str(oLM:aMetre[1])) ) )
      oNode:Add( HXMLNode():New( "beat-type",,, Ltrim(Str(oLM:aMetre[2])) ) )
      oAttr:Add( oNode := HXMLNode():New( "clef" ) )
      oNode:Add( HXMLNode():New( "sign",,, Iif(oLM:lBas,"F","G") ) )
      oNode:Add( HXMLNode():New( "line",,, Iif(oLM:lBas,"4","2") ) )
   ENDIF

   RETURN .T.

METHOD AddNote( xNote, nDur, cAttr ) CLASS Mxl

   LOCAL oNode, oPitch, oNode1
   LOCAL nNote, nOct, cRes, cType, n, l1_5, j

   IF Empty( ::oNodePart )
      ::nError := ERR_NOPART
      RETURN .F.
   ENDIF
   IF Empty( ::oMeasure )
      ::nError := ERR_NOMEASURE
      RETURN .F.
   ENDIF
   IF cAttr == Nil; cAttr := ""; ENDIF

   j := 0
   DO WHILE .T.
      nNote := Iif( Valtype(xNote) == "A", xNote[++j], xNote )

      nOct := Int( ( nNote - 1 ) / 12 ) + 1
      nNote := Int( Iif( nNote==0, 0, Iif( nNote % 12 == 0, 12, nNote % 12 ) ) )
      cRes := Iif( nNote==0, "", aNotesEn[nNote] )
      l1_5 := ( nDur - Int( nDur ) == 0.5 )
      nDur := Int( nDur )
      cType := aDurType[nDur]
      n := Ascan( aDivi, ::nDivisions )
      IF n > 0 .AND. n >= nDur
         nDur := Int( 2 ** ( n - nDur ) )
         IF l1_5
            nDur *= 1.5
         ENDIF
      ENDIF

      ::oMeasure:Add( oNode := HXMLNode():New( "note" ) )
      IF j > 1
         oNode:Add( HXMLNode():New( "chord", HBXML_TYPE_SINGLE ) )
         IF "arp" $ cAttr
            oNode:Add( oNode1 := HXMLNode():New( "notations" ) )
            oNod1:Add( HXMLNode():New( "arpeggiate", HBXML_TYPE_SINGLE ) )
         ENDIF
      ENDIF
      IF Empty( cRes )
         oNode:Add( HXMLNode():New( "rest", HBXML_TYPE_SINGLE ) )
      ELSE
         oNode:Add( oPitch := HXMLNode():New( "pitch" ) )
         IF Len( cres ) == 2
            oPitch:Add( HXMLNode():New( "step",,, Left(cres,1) ) )
            oPitch:Add( HXMLNode():New( "alter",,, "-1" ) )
         ELSE
            oPitch:Add( HXMLNode():New( "step",,, cres ) )
         ENDIF
         oPitch:Add( HXMLNode():New( "octave",,, Ltrim(Str(nOct)) ) )
      ENDIF
      oNode:Add( HXMLNode():New( "duration",,, Ltrim(Str(nDur)) ) )
      IF "ti2" $ cAttr
         oNode:Add( HXMLNode():New( "tie", HBXML_TYPE_SINGLE, { { "type", "stop" } } ) )
      ENDIF
      IF "ti1" $ cAttr
         oNode:Add( HXMLNode():New( "tie", HBXML_TYPE_SINGLE, { { "type", "start" } } ) )
      ENDIF
      oNode:Add( HXMLNode():New( "type",,, cType ) )
      IF "ti1" $ cAttr .OR. "ti2" $ cAttr
         oNode:Add( oNode1 := HXMLNode():New( "notations" ) )
         IF "ti2" $ cAttr
            oNode1:Add( HXMLNode():New( "tied", HBXML_TYPE_SINGLE, { { "type", "stop" } } ) )
         ENDIF
         IF "ti1" $ cAttr
            oNode1:Add( HXMLNode():New( "tied", HBXML_TYPE_SINGLE, { { "type", "start" } } ) )
         ENDIF
      ENDIF
      IF Valtype(xNote) == "N" .OR. j == Len(xNote)
         EXIT
      ENDIF
   ENDDO

   RETURN .T.

METHOD Save( cFile ) CLASS Mxl

   LOCAL hZip, s

   IF Lower( hb_fnameExt( cFile ) ) == ".mxl"
      hZip := hb_zipOpen( cFile )
      IF !Empty( hZip )

         hb_zipFileCreate( hZip, "META-INF",,,, 0x10 )
         hb_zipFileClose( hZip )

         s := '<?xml version="1.0" encoding="UTF-8"?>' + Chr(10) + '<container>' ;
             + Chr(10) + ' <rootfiles>' + Chr(10) + '  <rootfile full-path="score.xml"></rootfile>' ;
             + Chr(10) + ' </rootfiles>' + Chr(10) + '</container>'
         hb_zipFileCreate( hZip, "META-INF/container.xml" )
         hb_zipFileWrite( hZip, s, Len(s) )
         hb_zipFileClose( hZip )

         s := ::oXML:Save()
         hb_zipFileCreate( hZip, "score.xml" )
         hb_zipFileWrite( hZip, s, Len(s) )
         hb_zipFileClose( hZip )

         hb_zipClose( hZip )
      ENDIF
   ELSE
      ::oXML:Save( cFile )
   ENDIF

   RETURN Nil

