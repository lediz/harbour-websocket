/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
//request HB_GT_WEB
#include "hbclass.ch"
#include "common.ch"

#define CRLF chr(13)+chr(10)
#define MAGIC_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"



function Main()

   local cTest := "Hello world"
   local n, nRow := 1

   hb_gtSelect( hb_gtCreate( "WEB" ) )
   hb_threadStart( @hb_gtBuildHost(), 2000 )
   ShellExecute( 0, 'Open', "http://localhost/web" )   
   hb_idleSleep( 0.5 )

   FOR n = 1 TO 22
   
      @ nRow, 1    SAY cTest
      @ nRow, 120  SAY cTest
      @ nRow, 240  SAY cTest
      @ nRow, 360  SAY cTest 
      @ nRow, 480  SAY cTest 
      @ nRow, 600  SAY cTest 
      nRow += 25      
   NEXT
return nil


PROCEDURE hb_gtBuildHost( nPort )
   
   HBGtWebHost():New( nPort )
   
 
RETURN 

//-----------------------------------------//

CLASS HBGtWebHost FROM HBSocket

   DATA oClient

   METHOD New()

   METHOD Handshake()
   METHOD GetContext( cData, cContext )
   METHOD OnReadProcess()
   METHOD HBSendData( cData )

ENDCLASS

//-----------------------------------------//

METHOD New( nPort ) CLASS HBGtWebHost

   local oPP 
   local cInclude

   cInclude = hb_getEnv( "INCLUDE" )
   if Empty( cInclude )
      cInclude = hb_getEnv( "INCLUDE_DIR" )
      if Empty( cInclude )
         cInclude = "\harbour\include"
      endif      
   endif   

   Super:New( nPort, __pp_Init( cInclude, "std.ch" ) )
   
   ::lDebug    = .T.
   ::bDebug    = {| aParam | LogFile( "debug.txt", aParam ) }
   ::bOnRead   = {| oSrv, oClient | ::OnReadProcess( oClient ) }
   ::bOnAccept = {| oSrv, oClient | ::oClient := oClient }
   hb_gtSetHost( @Self )
   ::Listen()
   
   ::End()   
   

RETURN Self

//-----------------------------------------//

METHOD HandShake( ) CLASS HBGtWebHost
   local nLen, cBuffer, cContext, cKey, cSend
   
   cBuffer := ::oClient:cBuffer

   cContext = ::GetContext( cBuffer, "Sec-WebSocket-Key" )
   cKey     = hb_Base64Encode( hb_sha1( cContext + MAGIC_KEY, .T. ) ) // + "." add something to check that the handshake gets wrong
   
   cSend = "HTTP/1.1 101 Switching Protocols" + CRLF + ;
           "Upgrade: websocket" + CRLF + ;
           "Connection: Upgrade" + CRLF + ;
           "Sec-WebSocket-Accept: " + cKey + CRLF + CRLF   

   nLen = ::oClient:SendData( cSend )

   IF nLen > 0
      ::oClient:Cargo = hb_Hash()
      ::oClient:cargo[ "HANDSHAKE" ] = .T.    
      
   ENDIF

   
RETURN 

//-----------------------------------------//

METHOD GetContext( cData, cContext ) CLASS HBGtWebHost

  local nLen := Len( cContext )
  local cValue := ""
  local aLines
  local aSubLine
  local cRow
  
  aLines := hb_ATokens( cData, CRLF )
  for each cRow in aLines
     if cContext $ cRow
        aSubLine = hb_ATokens( cRow, ":" )
        cValue = AllTrim( aSubLine[ 2 ] )
        exit
     endif
  next

return cValue

//-----------------------------------------//

METHOD OnReadProcess(  ) CLASS HBGtWebHost
   
   local cMask, cData, n, cMsg:="", oError
   local nMaskPos := 1, cAnswer
   local oPP := ::oPP
   
   //handshake
   if ::oClient:Cargo == NIL .OR. ( hb_HHasKey( ::oClient:Cargo, "HANDSHAKE" ) .AND. ! ::oClient:Cargo[ "HANDSHAKE" ] )
      ::Handshake( ) 
   else 
      cData = ::oClient:cBuffer
      cMask = SubStr( cData, 3, 4 )
      for n = 1 to Len( SubStr( cData, 7 ) )
          cMsg += Chr( hb_BitXOR( Asc( SubStr( cMask, nMaskPos++, 1 ) ), Asc( SubStr( cData, 6 + n, 1 ) ) ) )
          if nMaskPos == 5
             nMaskPos = 1
          endif   
      next 
   
      ::HBSendData( cMsg )
      
      BEGIN SEQUENCE
         cMsg = __pp_Process( oPP, cMsg )
         cAnswer = uValToChar( &cMsg ) 
      RECOVER USING oError
         cAnswer = "Error: " + oError:Description
      ENDSEQUENCE      

      ::HBSendData( cAnswer )
   endif

RETURN 

//-----------------------------------------//

METHOD HBSendData( cData, oClient ) CLASS HBGtWebHost

RETURN ::oClient:SendData( Chr( 129 ) + Chr( Len( cData ) ) + hb_StrToUtf8( cData ) )

//-----------------------------------------//


#include "hbsocket.prg"

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "windows.h"
#include "hbjson.h"

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
static PHB_ITEM      s_pGtServer, s_pGtClient;

static int         s_iCurRow;
static int         s_iCurCol;

#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

#define HB_GT_NAME      WEB

HB_FUNC( SHELLEXECUTE )
{
   hb_retnl( ( LONG ) ShellExecute( ( HWND ) hb_parnl( 1 ), ( char * ) hb_parc( 2 ), ( char * ) hb_parc( 3 ), 
                                    ( char * ) hb_parc( 4 ), ( char * ) hb_parc( 5 ), hb_parnl( 6 ) ) );            	   	
}

HB_FUNC( HB_GTSETHOST )
{
   s_pGtServer = hb_param( 1, HB_IT_OBJECT );
}

HB_FUNC( HB_GTSETCLIENT )
{
   s_pGtClient = hb_param( 1, HB_IT_OBJECT );
}

static void hb_gt_web_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_web_Init(%p,%p,%p,%p)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   /* TODO: */

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   printf( "initiated\n" );
   
}

static void hb_gt_web_Exit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_web_Exit(%p)", pGT));

   HB_GTSUPER_EXIT( pGT );

   /* TODO: */
}

int hb_gt_web_MaxCol( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   printf( "MaxCol\n" );
   
   return 80;
}   

int hb_gt_web_MaxRow( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   printf( "MaxRow\n" );
   
   return 25;
}   
   
static int hb_gt_web_ReadKey( PHB_GT pGT, int iEventMask )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iEventMask );

//   printf( "ReadKey\n" );

   /* TODO: check the input queue (incoming mouse and keyboard events)
            and return the inkey code if any */

   return 13;
}

static const char * hb_gt_web_Version( PHB_GT pGT, int iType )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iType );

   return HB_GT_DRVNAME( HB_GT_NAME );
}

static void hb_gt_web_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, HB_USHORT usChar, int iRows, int iCols )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( usChar );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   printf( "Scroll\n" );
}
                                 
static HB_BOOL hb_gt_web_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   printf( "SetMode\n" );
   /* TODO: if possible change the size of the screen and return HB_TRUE */

   return HB_FALSE;
}

static void hb_gt_web_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   HB_SYMBOL_UNUSED( iSize );

   printf( "Redraw\n" );
}

static void hb_gt_web_Refresh( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   printf( "Refresh\n" );

   /* TODO: set cursor position and shape */
}

static void hb_gt_web_SetPos( PHB_GT pGT, int iCol, int iRow )
{
   HB_SYMBOL_UNUSED( pGT );

   printf( "SetPos %i %i\n", iRow, iCol );
   s_iCurRow = iRow;
   s_iCurCol = iCol;
   
   /* TODO: set cursor position and shape */
}

static void hb_gt_web_sendData( const char * szText, HB_SIZE nLength  )
{
      
   hb_vmPushSymbol( hb_dynsymGetSymbol( "HBSENDDATA" ) );
   hb_vmPush( s_pGtServer ); 
   hb_vmPushString( szText, nLength );
   hb_vmSend( 1 );

//   printf( szText );
}

static void hb_gt_web_WriteCon( PHB_GT pGT, const char * szText, HB_SIZE nLength )
{
   HB_SIZE nLen;
   char * szRet;
   PHB_ITEM pJSon    = hb_hashNew( hb_itemNew( NULL ) );
   PHB_ITEM pKey     = NULL,
            pValue   = NULL;
   PHB_ITEM pJSonPar = hb_hashNew( hb_itemNew( NULL ) );

   HB_SYMBOL_UNUSED( pGT );
   
   pKey   = hb_itemPutC( pKey, "function" );
   pValue = hb_itemPutC( pValue, "drawText" );

   hb_hashAdd( pJSon, pKey, pValue );

   pKey   = hb_itemPutC( pKey, "text" );
   pValue = hb_itemPutCL( pValue, szText, nLength );
   hb_hashAdd( pJSonPar, pKey, pValue );

   pKey   = hb_itemPutC( pKey, "row" );
   pValue = hb_itemPutNI( pValue, s_iCurRow );
   hb_hashAdd( pJSonPar, pKey, pValue );

   pKey   = hb_itemPutC( pKey, "col" );
   pValue = hb_itemPutNI( pValue, s_iCurCol );
   hb_hashAdd( pJSonPar, pKey, pValue );
   
   
   pKey   = hb_itemPutC( pKey, "parameter" );
   hb_hashAdd( pJSon, pKey, pJSonPar );

   hb_itemRelease( pJSonPar );
   
   szRet = hb_jsonEncode( pJSon, &nLen, HB_FALSE );
   
   printf( "WriteCon\n" );
   
   hb_gt_web_sendData( szRet, nLen );

//   printf( szText );
   printf( "\n" );
   hb_itemRelease( pJSon );
   hb_xfree( szRet );
}

static void hb_gt_web_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * szText, HB_SIZE nLength )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( nLength );

   printf( "WriteAt %i %i\n", iRow, iCol );
   printf( szText );
   printf( "\n" );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_web_Init;
   pFuncTable->Exit                       = hb_gt_web_Exit;
   pFuncTable->MaxCol                     = hb_gt_web_MaxCol;
   pFuncTable->MaxRow                     = hb_gt_web_MaxRow;
   pFuncTable->ReadKey                    = hb_gt_web_ReadKey;
   pFuncTable->Version                    = hb_gt_web_Version;
   pFuncTable->Scroll                     = hb_gt_web_Scroll;
   pFuncTable->SetMode                    = hb_gt_web_SetMode;
   pFuncTable->Redraw                     = hb_gt_web_Redraw;
   pFuncTable->SetPos                     = hb_gt_web_SetPos;
   pFuncTable->Refresh                    = hb_gt_web_Refresh;
   pFuncTable->Write                      = hb_gt_web_WriteCon;
   pFuncTable->WriteCon                   = hb_gt_web_WriteCon;
   pFuncTable->WriteAt                    = hb_gt_web_WriteAt;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */

#pragma ENDDUMP