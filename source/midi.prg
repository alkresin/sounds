/*
 * Midi parsing
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"

#define NOTE_OFF   0x80
#define NOTE_ON    0x90

#define ERR_HEAD      1
#define ERR_MTRKPOS   2
#define ERR_MTRKLEN   3
#define ERR_FLEN      4
#define ERR_STATUSB   5
#define ERR_NOTEPOS   6

STATIC cMidiHead := e"MThd\x00\x00\x00\x06"
STATIC aDur1 := { 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96 }
STATIC aDur2 := { 7, 7.5, 6, 6.5, 5, 5.5, 4, 4.5, 3, 3.5, 2, 2.5, 1, 1.5 }
STATIC aDur := { 2048, 1024, 512, 256, 128, 64, 32 }

CLASS Midi

   CLASS VAR nError SHARED
   DATA type
   DATA ppqn
   DATA nTracks
   DATA aTracks   INIT {}
   DATA nPause

   METHOD New( nType, ppqn, nTracks )
   METHOD Open( cFileName )
   METHOD GetTracks()
   METHOD ToLM( oLM, n, nChan )
   METHOD Dump( n )
   METHOD AddNote( nTrack, nChan, nNote, nDur )
   METHOD AddProgram( nTrack, nChan, nInstr )
   METHOD AddFF( nTrack, nDop, cData )
   METHOD Save( cFile )
ENDCLASS

METHOD New( nType, ppqn, nTracks ) CLASS Midi

   LOCAL i

   ::type := nType
   ::ppqn := ppqn
   ::nTracks := nTracks
   ::aTracks := Array( ::nTracks )
   FOR i := 1 TO ::nTracks
      ::aTracks[i] := {}
   NEXT
   ::nPause := 0

   RETURN Self

METHOD Open( cFileName ) CLASS Midi

   LOCAL cBuff := MemoRead( cFileName ), n := 0, nPos, nPos1, nSiz, nLen, nTrackLen, arr, arr1
   LOCAL nDelta, nEvent, nMess, nDop, lEnd, nRunStat := 0
   LOCAL lNotes, aChans

   IF Empty( cBuff ) .OR. !( Left(cBuff,8) == cMidiHead )
      ::nError := ERR_HEAD
      RETURN Nil
   ENDIF

   ::type := hb_BPeek( cBuff,10 )
   ::nTracks := hb_BPeek( cBuff,12 )
   ::ppqn := hb_BPeek( cBuff,13 ) * 256 + hb_BPeek( cBuff,14 )

   nLen := Len( cBuff )
   nPos := 15

   DO WHILE ++n <= ::nTracks
      IF !( Substr( cBuff,nPos,4 ) == "MTrk" )
         ::nError := ERR_MTRKPOS
         RETURN Nil
      ENDIF
      nSiz := hb_BPeek(cBuff,nPos+4) * 256*256*256 + hb_BPeek(cBuff,nPos+5) * 256*256 + ;
         hb_BPeek(cBuff,nPos+6) * 256 + hb_BPeek(cBuff,nPos+7)
      //hwg_writelog( ltrim(str(nsiz)) + ltrim(str(nsiz)) + ltrim(str(nsiz)) )
      IF nPos + nSiz > nLen
         //hwg_writelog( ltrim(str(nPos)) + " " + ltrim(str(nsiz)) + " " + ltrim(str(nLen)) )
         ::nError := ERR_MTRKLEN
         RETURN Nil
      ENDIF
      AAdd( ::aTracks, Substr( cBuff, nPos + 8, nSiz ) )
      nPos += nSiz + 8
   ENDDO

   FOR n := 1 TO ::nTracks
      cBuff := ::aTracks[n]
      nTrackLen := Len( cBuff )
      lNotes := .F.
      aChans := {}
      arr := {}
      nPos := 1
      lEnd := .F.
      DO WHILE !lEnd .AND. nPos < nTrackLen
         nDelta := midi_ReadVarLen( cBuff, @nPos )
         nEvent := hb_BPeek( cBuff, ++nPos )
         AAdd( arr, arr1 := { nDelta, nEvent, Nil } )
         nMess := hb_BitAnd( nEvent, 0xF0 )
         IF nMess < 0x80
            IF nRunStat == 0
               ::nError := ERR_STATUSB
               //hwg_writelog( ltrim(str(n))+": "+Ltrim(Str(nPos)) )
               RETURN Nil
            ELSE
               arr1[2] := nRunStat
               nMess := hb_BitAnd( nRunStat, 0xF0 )
               IF nMess == 0xC0 .OR. nMess == 0xD0
                  arr1[3] := Substr( cBuff, nPos, 1 )
                  nPos ++
               ELSE
                  arr1[3] := Substr( cBuff, nPos, 2 )
                  nPos += 2
               ENDIF
               LOOP
            ENDIF
         ENDIF
         nPos ++
         SWITCH nMess
         CASE 0xF0         // Sys
            nPos1 := nPos
            IF nEvent <= 0xF7
               nRunStat := 0
            ENDIF
            IF nEvent == 0xFF
               nDop := hb_BPeek( cBuff, nPos )
               nPos ++
               IF nDop == 0x2F
                  arr1[3] := SubStr( cBuff,nPos1,2 )
                  lEnd := .T.
                  EXIT
               ELSE
                  nDelta := midi_ReadVarLen( cBuff, @nPos )
                  IF nDelta + nPos > nTrackLen
                     ::nError := ERR_FLEN
                     RETURN Nil
                  ENDIF
                  nPos ++
                  nPos += nDelta
                  arr1[3] := Substr( cBuff, nPos1, nPos-nPos1 )
               ENDIF
            ELSEIF nEvent == 0xF0
               nDelta := midi_ReadVarLen( cBuff, @nPos )
               IF nDelta + nPos > nTrackLen
                  ::nError := ERR_FLEN
                  RETURN Nil
               ENDIF
               nPos += nDelta
               arr1[3] := Substr( cBuff, nPos1, nPos-nPos1 )
            ENDIF
            EXIT

         CASE NOTE_ON
            arr1[3] := Substr( cBuff, nPos, 2 )
            nRunStat := nEvent
            lNotes := .T.
            IF Ascan( aChans, hb_BitAnd( nEvent,0xF ) ) == 0
               AAdd( aChans, hb_BitAnd( nEvent,0xF ) )
            ENDIF
            nPos += 2
            EXIT

         CASE NOTE_OFF
            arr1[3] := Substr( cBuff, nPos, 2 )
            nRunStat := nEvent
            nPos += 2
            EXIT

         CASE 0xA0         // Polyphonic key pressure
            arr1[3] := Substr( cBuff, nPos, 2 )
            nRunStat := nEvent
            nPos += 2
            EXIT

         CASE 0xB0         // Control change
            arr1[3] := Substr( cBuff, nPos, 2 )
            nRunStat := nEvent
            nPos += 2
            EXIT

         CASE 0xC0         // Program change
            arr1[3] := Substr( cBuff, nPos, 1 )
            nRunStat := nEvent
            nPos ++
            EXIT

         CASE 0xD0         // Channel pressure
            arr1[3] := Substr( cBuff, nPos, 1 )
            nRunStat := nEvent
            nPos ++
            EXIT

         CASE 0xE0         // Pitch wheel change
            arr1[3] := Substr( cBuff, nPos, 2 )
            nRunStat := nEvent
            nPos += 2
            EXIT

         OTHERWISE
         END
      ENDDO
      AAdd( ATail(arr), lNotes )
      IF lNotes
         AAdd( ATail(arr), aChans )
      ENDIF
      ::aTracks[n] := arr
   NEXT

   RETURN Self


METHOD GetTracks() CLASS Midi

   LOCAL i, j, arr := {}, arr1

   FOR i := 1 TO ::nTracks
      IF ATail(::aTracks[i])[4]
         arr1 := ATail( ::aTracks[i] )[5]
         FOR j := 1 TO Len( arr1 )
            AAdd( arr, { "Track " + Ltrim(Str(i)) + " channel " + Ltrim(Str(arr1[j])), i, arr1[j] } )
         NEXT
      ENDIF
   NEXT

   RETURN arr

METHOD ToLM( oLM, n, nChan ) CLASS Midi

   LOCAL i, j, aTracks, arr := {}, nNote, nc1, nc2, nTime := 0, nTimeLast, nTimeNote, nPause
   LOCAL pp64 := ::ppqn / 16
   LOCAL nTDur, nDur, nDurSum

   IF n == 0 .OR. n > ::nTracks .OR. !Atail(::aTracks[n])[4] .OR. Ascan( Atail(::aTracks[n])[5], nChan ) == 0
      RETURN Nil
   ENDIF

   oLM:aMetre[1] := oLM:aMetre[2] := 4
   aTracks := ::aTracks[n]
   FOR i := 1 TO Len( aTracks )
      nTime += aTracks[i,1]
      IF hb_BitAnd( aTracks[i,2], 0xF0 ) == NOTE_ON .AND. hb_BitAnd( aTracks[i,2], 0xF ) == nChan ;
         .AND. hb_BPeek( aTracks[i,3], 2 ) != 0
         nPause := 0
         IF !Empty( arr )
            IF ATail(arr)[2] - (nTime - nTimeLast) > 10
               IF (nTime - nTimeLast) < 4
                  nNote := hb_BPeek( aTracks[i,3], 1 )
                  IF Valtype( ATail(arr)[1] ) == "A"
                     AAdd( ATail(arr)[1], nNote )
                  ELSE
                     ATail(arr)[1] := { ATail(arr)[1], nNote }
                  ENDIF
                  nTimeLast := nTime
                  LOOP
               ELSE
                  /*
                  hwg_Writelog( "pos: "+ltrim(str(i))+" nTime: "+ltrim(str(nTime))+;
                     " nTimeLast: "+ltrim(str(nTimeLast))+" Len: "+ltrim(str(Len(arr)))+;
                     " ATail(arr)[2]: "+ltrim(str(ATail(arr)[2]))+" "+Valtype(ATail(arr)[1]) )
                  ::nError := ERR_NOTEPOS
                  RETURN .F.
                  */
               ENDIF
            ENDIF
            nPause := nTime - nTimeLast - ATail(arr)[2]
         ELSE
            nPause := nTime
         ENDIF
         IF nPause > pp64 * 2 //pp64/2
            AAdd( arr, { 0, nPause } )
         ENDIF
         nTimeLast := nTime
         nNote := hb_BPeek( aTracks[i,3], 1 )
         nTimeNote := midi_NoteEnd( aTracks, i, nNote )
         AAdd( arr, { nNote, nTimeNote } )

      ELSEIF aTracks[i,2] == 0xFF
         IF ( nc1 := hb_BPeek( aTracks[i,3], 1 ) ) == 0x58
            oLM:aMetre[1] := hb_BPeek( aTracks[i,3], 3 )
            oLM:aMetre[2] := Int( 2 ** hb_BPeek( aTracks[i,3], 4 ) )
         ELSEIF nc1 == 0x59      // Key signature
         ELSEIF nc1 == 0x51      // Tempo
         ELSEIF nc1 == 5         // Title
            IF Empty( oLM:cTitle )
               oLM:cTitle := hb_strReplace( Substr( aTracks[i,3], 3 ), {'"',Chr(10),Chr(13)}, {"","",""} )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   FOR i := 1 TO Len( arr )
      IF Valtype( arr[i,1] ) == "A"
         FOR j := 1 TO Len( arr[i,1] )
            arr[i,1,j] := arr[i,1,j] - 23
         NEXT
      ELSE
         IF arr[i,1] > 0
            arr[i,1] := arr[i,1] - 23
         ENDIF
      ENDIF
      nc1 := Round( arr[i,2] / pp64, 1 )
      IF ( j := Ascan( aDur1, {|nc| nc>nc1 } ) ) == 0
         arr[i,2] := 1.5
      ELSEIF j == 1
         arr[i,2] := 7
      ELSE
         j := Iif( aDur1[j]-nc1 < nc1-aDur1[j-1], j, j-1 )
         arr[i,2] := aDur2[j]
      ENDIF
   NEXT

   FOR i := 1 TO Len( arr ) - 3
      // search for tremolo
      IF (arr[i,2] == 6 .OR. arr[i,2] == 7) .AND. arr[i,2] == arr[i+1,2]
         j := 1
         DO WHILE i+3 <= Len(arr)-j+1 .AND. arr[i+2,1] == arr[i,1] .AND. arr[i+2,2] == arr[i,2] ;
            .AND. arr[i+3,1] == arr[i+1,1] .AND. arr[i+3,2] == arr[i+1,2]
            j += 2
            ADel( arr, i+2 )
            ADel( arr, i+2 )
         ENDDO
         IF j > 1
            ADel( arr, i+1 )
            j ++
            nc1 := j * Iif( arr[i,2] == 7, 1, 2 )
            nc2 := Ascan( aDur1, {|nc| nc>nc1 } )
            //hwg_writelog( ltrim(str(j)) + " " + ltrim(str(nc1)) + " " + ltrim(str(nc2)) )
            nc2 := Iif( aDur1[nc2]-nc1 < nc1-aDur1[nc2-1], nc2, nc2-1 )
            arr[i,2] := aDur2[nc2]
            Aadd( arr[i], "r" )
            arr := ASize( arr, Len(arr) - j + 1 )
         ENDIF
      ENDIF
   NEXT

   nTDur := Int( aDur[1] * oLM:aMetre[1] / oLM:aMetre[2] )
   nDurSum := 0
   FOR i := 1 TO Len( arr )
      // Set measures
      nDurSum += Int( aDur[Int(nDur := arr[i,2])] + Iif( nDur-Int(nDur)==0.5, aDur[Int(nDur)]/2, 0 ) )
      IF nDurSum >= nTDur .OR. i == Len( arr )
         noteSetAttr( arr[i], "t" )
         nDurSum := 0
      ENDIF
   NEXT

   oLM:aNotes := arr

   RETURN .T.

METHOD Dump( n ) CLASS Midi

   LOCAL s := "", i, j

   IF n <= ::nTracks
      FOR i := 1 TO Len( ::aTracks[n] )
         s += Str( ::aTracks[n,i,1],5 ) + " " + hb_NumToHex( ::aTracks[n,i,2] )
         FOR j := 1 TO Len( ::aTracks[n,i,3] )
            s += " " + hb_NumToHex( hb_BPeek( ::aTracks[n,i,3],j ) )
         NEXT
         s += Chr( 10 )
      NEXT
   ENDIF

   RETURN s

METHOD AddNote( nTrack, nChan, nNote, nDur, nVol ) CLASS Midi

   LOCAL i

   IF nTrack == 0 .OR. nTrack > ::nTracks
      RETURN Nil
   ENDIF

   IF ( i := Ascan( aDur2, nDur ) ) == 0
      i := 9   // 1/4
   ENDIF
   nDur := Int( aDur1[i] * ::ppqn / 16 )

   IF nVol == Nil
      nVol := 0x7F
   ENDIF
   IF Valtype( nNote ) == "A"
      FOR i := 1 TO Len( nNote )
         AAdd( ::aTracks[nTrack], { ::nPause, hb_BitOr( 0x90,nChan ), Chr(nNote[i]+23)+Chr(nVol) } )
      NEXT
      FOR i := 1 TO Len( nNote )
         AAdd( ::aTracks[nTrack], { nDur, hb_BitOr( 0x90,nChan ), Chr(nNote[i]+23)+Chr(0) } )
      NEXT
   ELSEIF nNote == 0
      ::nPause += nDur
   ELSE
      nNote += 23
      AAdd( ::aTracks[nTrack], { ::nPause, hb_BitOr( 0x90,nChan ), Chr(nNote)+Chr(nVol) } )
      AAdd( ::aTracks[nTrack], { nDur, hb_BitOr( 0x90,nChan ), Chr(nNote)+Chr(0) } )
      ::nPause := 0
   ENDIF

   RETURN Nil

METHOD AddProgram( nTrack, nChan, nInstr ) CLASS Midi

   AAdd( ::aTracks[nTrack], { 0, hb_BitOr( 0xC0,nChan ), Chr(nInstr) } )
   RETURN Nil

METHOD AddFF( nTrack, nDop, cData )

   IF nTrack == 0 .OR. nTrack > ::nTracks
      RETURN Nil
   ENDIF

   AAdd( ::aTracks[nTrack], { 0, 0xFF, Chr(nDop)+Chr(Len(cData))+cData } )

   RETURN Nil

METHOD Save( cFile ) CLASS Midi

   LOCAL aTracks := Array( ::nTracks ), i, j, s, cLen

   FOR i := 1 TO ::nTracks
      s := ""
      FOR j := 1 TO Len( ::aTracks[i] )
         s += midi_WriteVarLen( ::aTracks[i,j,1] ) + Chr( ::aTracks[i,j,2] ) + ::aTracks[i,j,3]
      NEXT
      s += Chr(0) + Chr(0xFF) + Chr(0x2F) + Chr(0)
      cLen := L2Bin( Len( s ) )
      aTracks[i] := "MTrk" + Substr(cLen,4,1) + Substr(cLen,3,1) + Substr(cLen,2,1) + Substr(cLen,1,1) + s
   NEXT

   s := cMidiHead + Chr(0) + Chr(1) + Chr(0) + Chr(::nTracks) + Chr(0) + Chr(::ppqn)

   FOR i := 1 TO ::nTracks
      s += aTracks[i]
   NEXT

   hb_Memowrit( cFile, s )

   RETURN Nil

STATIC FUNCTION midi_NoteEnd( aTracks, n, nNote )

   LOCAL nc1 := aTracks[n,2], nc2 := hb_BitAnd( nc1, 0xEF )
   LOCAL nTimeNote := 0, i

   FOR i := n+1 TO Len( aTracks )
      nTimeNote += aTracks[i,1]
      IF ( ( aTracks[i,2]==nc1 .AND. hb_BPeek(aTracks[i,3],2)==0 ) .OR. aTracks[i,2]==nc2 ) ;
         .AND. nNote == hb_BPeek( aTracks[i,3], 1 )
         EXIT
      ENDIF
   NEXT

   RETURN nTimeNote

STATIC FUNCTION midi_ReadVarLen( cBuff, nPos )

   LOCAL nVal := hb_BPeek( cBuff, nPos ), n

   IF hb_BitAnd( nVal, 0x80 ) != 0
      nVal := hb_BitAnd( nVal, 0x7F )
      DO WHILE .T.
         n := hb_BPeek( cBuff, ++nPos )
         nVal := hb_BitShift( nVal, 7 ) + hb_BitAnd( n, 0x7F )
         IF hb_BitAnd( n, 0x80 ) == 0
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN nVal

STATIC FUNCTION midi_WriteVarLen( nVal )

   LOCAL s := Chr( hb_BitAnd( nVal, 0x7F ) )

   DO WHILE ( nVal := hb_BitShift( nVal, -7 ) ) > 0
      s := Chr( hb_BitOr( 0x80, hb_BitAnd( nVal, 0x7F ) ) ) + s
   ENDDO

   RETURN s
