/*
 * Harbour wrapper for portaudio library
 * Copyright 2021 Alexander S. Kresin <alex / at / kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

#include "sndfile.h"
#include "portaudio.h"

#ifndef SF_SEEK_SET
   #define SF_SEEK_SET   SEEK_SET
#endif
#define BUFFER_SIZE   512

typedef struct
{
   SNDFILE  *sndFile;
   SNDFILE  *sndFileNew;
   SNDFILE  *sndFileRepl;
   SNDFILE  *sndAcc1, *sndAcc2, *sndAcc3, *sndAcc4;
   SF_INFO   sfInfo;
   int       bPause;
   int       bKeep;
   int       bEnd;
   int       position;
   float     fVolume;
   float     *output;
   PaStream *stream;
} StreamData;

#include "hbapifs.h"
static void _writelog( const char * sFile, int n, const char * s, ... )
{

   if( !sFile )
      return;

   if( n )
   {
      HB_FHANDLE handle;
      if( hb_fsFile( sFile ) )
         handle = hb_fsOpen( sFile, FO_WRITE );
      else
         handle = hb_fsCreate( sFile, 0 );

      hb_fsSeek( handle,0, SEEK_END );
      hb_fsWrite( handle, s, n );
      hb_fsWrite( handle, "\r\n", 2 );
      hb_fsClose( handle );
   }
   else
   {
      FILE * hFile = hb_fopen( sFile, "a" );

      va_list ap;
      if( hFile )
      {
         va_start( ap, s );
         vfprintf( hFile, s, ap );
         va_end( ap );
         fclose( hFile );
      }
   }
}

HB_FUNC( SF_INITDATA )
{

   StreamData *data =
         ( StreamData * ) malloc( sizeof( StreamData ) );

   data->bPause = data->bEnd = data->bKeep = data->position = 0;
   data->sfInfo.format = 0;
   data->fVolume = 1;
   data->sndFile = sf_open( hb_parc(1), SFM_READ, &data->sfInfo );
   data->sndFileNew = NULL;
   data->sndFileRepl = NULL;
   data->sndAcc1 = data->sndAcc2 = data->sndAcc3 = data->sndAcc4 = NULL;
   data->output = NULL;

   if( !data->sndFile )
   {
      free( data );
      hb_ret();
   }
   else
      hb_retptr( data );

}

HB_FUNC( SF_FREEDATA )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   sf_close( data->sndFile );
   if( data->output )
      free( data->output );
   free( data );
}

HB_FUNC( SF_CHANGEDATA )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   StreamData *dataNew = (StreamData *) hb_parptr(2);

   data->sndFileNew = dataNew->sndFile;
   data->bPause = data->bEnd = 0;
}

HB_FUNC( SF_SETACCORD )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   StreamData *data1 = (StreamData *) ( HB_ISNIL(2)? NULL : hb_parptr(2) );
   StreamData *data2 = (StreamData *) ( HB_ISNIL(3)? NULL : hb_parptr(3) );
   StreamData *data3 = (StreamData *) ( HB_ISNIL(4)? NULL : hb_parptr(4) );
   StreamData *data4 = (StreamData *) ( HB_ISNIL(5)? NULL : hb_parptr(5) );

   data->sndAcc1 = (data1)? data1->sndFile : NULL;
   data->sndAcc2 = (data2)? data2->sndFile : NULL;
   data->sndAcc3 = (data3)? data3->sndFile : NULL;
   data->sndAcc4 = (data4)? data4->sndFile : NULL;
   data->bPause = data->bEnd = 0;
}

HB_FUNC( SF_GETVERSION )
{
   hb_retc( sf_version_string() );
}

HB_FUNC( SF_SEEK )
{
   StreamData *data = (StreamData *) hb_parptr(1);

   sf_seek( data->sndFile, hb_parni( 2 ), SF_SEEK_SET );
}

/*
 * sf_read_float( pData, arr, [iOffset], [iStart], [iKol] ) -> iCount
 */
HB_FUNC( SF_READ_FLOAT )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   PHB_ITEM pArr = hb_param( 2, HB_IT_ARRAY ), pSubArr;
   int iOffset = (HB_ISNUM(3))? hb_parni(3) : -1;
   int iStart = (HB_ISNUM(4))? hb_parni(4)-1 : 0;
   int iKol = (HB_ISNUM(5))? hb_parni(5) : -1;
   int iLen = hb_arrayLen( pArr );
   int iCount, i, j;
   float * out, * pOut;

   HB_TYPE type = hb_arrayGetType( pArr, 1 );
   int bArr = ( type & HB_IT_ARRAY );
   int iChan = (bArr)? hb_arrayLen( hb_arrayGetItemPtr( pArr, 1 ) ) : 1;

   //_writelog( "ac.log", 0, "%d %d %d %d\r\n", iKol, iChan, iStart, iLen );
   if( iOffset >= 0 )
      sf_seek( data->sndFile, iOffset, SF_SEEK_SET );

   if( iKol < 0 || iStart + iKol > iLen )
      iKol = iLen - iStart;
   iKol *= iChan;

   out = (float *) malloc( iKol * sizeof( float ) );
   iCount = sf_read_float( data->sndFile, out, iKol );

   //_writelog( "ac.log", 0, "%d %d %d\r\n", iKol, iCount, iChan );
   if( iCount )
   {
      if( iChan > 1 )
         iCount /= iChan;
      pOut = out;
      for( i = 1; i <= iCount; i += 1 )
      {
         if( bArr )
         {
            pSubArr = hb_arrayGetItemPtr( pArr, i+iStart );
            for( j = 1; j <= iChan; j++, pOut++ )
               hb_arraySetND( pSubArr, j, (double) *pOut );
         }
         else
         {
            hb_arraySetND( pArr, i+iStart, (double) *pOut );
            pOut ++;
         }
      }
   }

   //_writelog( "ac.log", 0, ": %d\r\n", iCount );
   free( out );
   hb_retni( iCount );
}

/*
 * sf_read_int( pData, arr, [iOffset], [iStart], [iKol] ) -> iCount
 */
HB_FUNC( SF_READ_INT )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   PHB_ITEM pArr = hb_param( 2, HB_IT_ARRAY );
   int iOffset = (HB_ISNUM(3))? hb_parni(3) : -1;
   int iStart = (HB_ISNUM(3))? hb_parni(4)-1 : 0;
   int iKol = (HB_ISNUM(3))? hb_parni(5) : -1;
   int iLen = hb_arrayLen( pArr );
   int iCount, i;
   int * out, * pOut;

   if( iOffset >= 0 )
      sf_seek( data->sndFile, iOffset, SF_SEEK_SET );

   if( iKol < 0 || iStart + iKol > iLen )
      iKol = iLen - iStart;

   out = (int *) malloc( iKol * sizeof( int ) );
   iCount = sf_read_int( data->sndFile, out, iKol );

   if( iCount )
   {
      pOut = out;
      for( i = 1; i <= iCount; i ++ )
      {
         hb_arraySetNI( pArr, i+iStart, *pOut );
         pOut ++;
      }
   }

   free( out );
   hb_retni( iCount );
}

/*
 * sf_GetInfo( pData ) -> { channels, samplerate, frames }
 */
HB_FUNC( SF_GETINFO )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   PHB_ITEM aInfo = hb_itemArrayNew( 3 );

   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 1 ), data->sfInfo.channels );
   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 2 ), data->sfInfo.samplerate );
   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 3 ), data->sfInfo.frames );
   hb_itemRelease( hb_itemReturn( aInfo ) );
}

/*
 * sf_GetRange( pData, @ymax, @ymin )
 */
HB_FUNC( SF_GETRANGE )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   float * out, * pOut;
   float fMax = 0, fMin = 0;
   int iKol = 2048, iCount, i, iOffset = 0;

   out = (float *) malloc( iKol * sizeof( float ) );

   do {
      sf_seek( data->sndFile, iOffset, SF_SEEK_SET );
      iOffset += iKol;
      iCount = sf_read_float( data->sndFile, out, iKol );
      pOut = out;
      for( i = 0; i < iCount; i ++, pOut ++ )
      {
         if( *pOut > fMax )
            fMax = *pOut;
         if( *pOut < fMin )
            fMin = *pOut;
      }
   }
   while( iCount == iKol );

   free( out );

   hb_stornd( (double) fMax, 2 );
   hb_stornd( (double) fMin, 3 );
}

int StreamCallback( const void *input, void *output, unsigned long frameCount,
      const PaStreamCallbackTimeInfo * paTimeInfo, PaStreamCallbackFlags statusFlags, void *userData )
{
   StreamData *data = ( StreamData * ) userData;
   float *out = (float*) output;
   int iChannels = data->sfInfo.channels;
   int iCount, i;

   (void) input;
   (void) frameCount;
   (void) paTimeInfo;
   (void) statusFlags;

   //hwg_writelog( NULL,"%lu\r\n", frameCount );
   if( data->sndFileNew )
   {
      data->sndFileRepl = data->sndFileNew;
      data->sndFileNew = NULL;
      //data->position = 0;
      data->position = BUFFER_SIZE * 5;
   }

   if( data->bPause )
   {
      for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
         out[i] = 0;
      return paContinue;
   }
   if( data->bEnd )
   {
      data->bEnd = 0;
      return paAbort;
   }

   if( data->sndFileRepl )
   {
      sf_seek( data->sndFileRepl, data->position, SF_SEEK_SET );
      iCount = sf_read_float( data->sndFileRepl, out, BUFFER_SIZE * iChannels );
   }
   else
   {
      sf_seek( data->sndFile, data->position, SF_SEEK_SET );
      iCount = sf_read_float( data->sndFile, out, BUFFER_SIZE * iChannels );
   }

   if( iCount == 0 && data->bKeep )
   {
      data->position -= BUFFER_SIZE * 5;
      if( data->sndFileRepl )
      {
         sf_seek( data->sndFileRepl, data->position, SF_SEEK_SET );
         iCount = sf_read_float( data->sndFileRepl, out, BUFFER_SIZE * iChannels );
      }
      else
      {
         sf_seek( data->sndFile, data->position, SF_SEEK_SET );
         iCount = sf_read_float( data->sndFile, out, BUFFER_SIZE * iChannels );
      }
   }

   if( data->fVolume != 1 )
      for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
         out[i] *= data->fVolume;

   if( data->sndAcc1 )
   {
      if( !data->output )
         data->output = (float *) malloc( BUFFER_SIZE * iChannels * sizeof( float ) );
      sf_seek( data->sndAcc1, data->position, SF_SEEK_SET );
      sf_read_float( data->sndAcc1, data->output, BUFFER_SIZE * iChannels );
      for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
         out[i] += data->output[i] * data->fVolume;
      if( data->sndAcc2 )
      {
         sf_seek( data->sndAcc2, data->position, SF_SEEK_SET );
         sf_read_float( data->sndAcc2, data->output, BUFFER_SIZE * iChannels );
         for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
            out[i] += data->output[i] * data->fVolume;
         if( data->sndAcc3 )
         {
            sf_seek( data->sndAcc3, data->position, SF_SEEK_SET );
            sf_read_float( data->sndAcc3, data->output, BUFFER_SIZE * iChannels );
            for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
               out[i] += data->output[i] * data->fVolume;
            if( data->sndAcc4 )
            {
               sf_seek( data->sndAcc4, data->position, SF_SEEK_SET );
               sf_read_float( data->sndAcc4, data->output, BUFFER_SIZE * iChannels );
               for( i = 0; i < BUFFER_SIZE * iChannels; i++ )
                  out[i] += data->output[i] * data->fVolume;
            }
         }
      }
   }
   data->position += BUFFER_SIZE;

   if( iCount > 0 )
      return paContinue;
   else
      return paAbort;
}

HB_FUNC( PA_INITIALIZE )
{

   hb_retni( Pa_Initialize() );
}

HB_FUNC( PA_TERMINATE )
{

   hb_retni( Pa_Terminate() );
}

HB_FUNC( PA_GETVERSION )
{

   hb_retni( Pa_GetVersion() );
}

HB_FUNC( PA_GETVERSIONTEXT )
{

  //hb_retc( Pa_GetVersionInfo()->versionText );
  hb_retc( Pa_GetVersionText() );
}

HB_FUNC( PA_GETERRORTEXT )
{

  hb_retc( Pa_GetErrorText( hb_parni(1) ) );
}

HB_FUNC( PA_GETHOSTAPICOUNT )
{

   hb_retni( Pa_GetHostApiCount() );
}

HB_FUNC( PA_GETDEFAULTHOSTAPI )
{

   hb_retni( Pa_GetDefaultHostApi() );
}

HB_FUNC( PA_GETHOSTAPIINFO )
{

   hb_retptr( (void *)Pa_GetHostApiInfo( hb_parni(1) ) );
}

HB_FUNC( PA_GETDEVICECOUNT )
{

   hb_retni( Pa_GetDeviceCount() );
}

HB_FUNC( PA_GETDEVICEINFO )
{

   hb_retptr( (void *) Pa_GetDeviceInfo( hb_parni(1) ) );
}


HB_FUNC( PA_GETDEFAULTINPUTDEVICE )
{

   hb_retni( Pa_GetDefaultInputDevice() );
}

HB_FUNC( PA_GETDEFAULTOUTPUTDEVICE )
{

   hb_retni( Pa_GetDefaultOutputDevice() );
}

HB_FUNC( PA_OPENSTREAM )
{

   StreamData *data = (StreamData *) hb_parptr(1);
   PaStreamParameters outputParameters;

   data->bPause = data->bEnd = data->bKeep = data->position = 0;
   outputParameters.device = Pa_GetDefaultOutputDevice();
   outputParameters.channelCount = data->sfInfo.channels;
   outputParameters.sampleFormat = paFloat32;
   outputParameters.suggestedLatency = 0.2; //Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;  // 0.2
   outputParameters.hostApiSpecificStreamInfo = 0;

   hb_retni( Pa_OpenStream( &(data->stream),
                       0, &outputParameters,
                       data->sfInfo.samplerate,
                       BUFFER_SIZE,  //  paFramesPerBufferUnspecified,
                       0,  //paNoFlag,
                       StreamCallback,
                       data ) );
}

HB_FUNC( PA_STARTSTREAM )
{

   StreamData *data = (StreamData *) hb_parptr(1);

   data->bPause = data->bEnd = data->bKeep = data->position = 0;
   hb_retni( Pa_StartStream( data->stream ) );
}

HB_FUNC( PA_STOPSTREAM )
{

   StreamData *data = (StreamData *) hb_parptr(1);

   data->sndFileRepl = data->sndFileNew = NULL;
   data->sndAcc1 = data->sndAcc2 = data->sndAcc3 = data->sndAcc4 = NULL;
   data->bPause = data->bKeep = 0;

   Pa_StopStream( data->stream );
   hb_retni( Pa_CloseStream( data->stream ) );
}

HB_FUNC( PA_ABORTSTREAM )
{

   StreamData *data = (StreamData *) hb_parptr(1);

   data->sndFileRepl = data->sndFileNew = NULL;
   data->sndAcc1 = data->sndAcc2 = data->sndAcc3 = data->sndAcc4 = NULL;
   data->bPause = data->bKeep = 0;

   Pa_AbortStream( data->stream );
   hb_retni( Pa_CloseStream( data->stream ) );
}

HB_FUNC( PA_CLOSESTREAM )
{

   StreamData *data = (StreamData *) hb_parptr(1);

   data->sndFileRepl = data->sndFileNew = NULL;
   data->sndAcc1 = data->sndAcc2 = data->sndAcc3 = data->sndAcc4 = NULL;
   data->bPause = data->bKeep = 0;

   hb_retni( Pa_CloseStream( data->stream ) );
}

HB_FUNC( PA_ISSTREAMACTIVE )
{

   StreamData *data = (StreamData *) hb_parptr(1);

   hb_retni( Pa_IsStreamActive( data->stream ) );
}

HB_FUNC( PA_SETVOLUME )
{

   StreamData *data = (StreamData *) hb_parptr(1);
   data->fVolume = (float) hb_parnd(2);
}

HB_FUNC( PA_SETPAUSE )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   data->bPause = hb_parl(2);
}

HB_FUNC( PA_SETKEEP )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   data->bKeep = hb_parl(2);
}

HB_FUNC( PA_SETEND )
{
   StreamData *data = (StreamData *) hb_parptr(1);
   data->bEnd = 1;
}
