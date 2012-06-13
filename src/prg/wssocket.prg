/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/

#ifdef __PLATFORM__WINDOWS
#include "hbwin.ch"
#endif

#include "hbclass.ch"
#include "common.ch"


#define CRLF chr(13)+chr(10)
#define H5_MAX_ID 8
                
#define MAGIC_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define TYPE_CONT    0x0
#define TYPE_TEXT    0x1
#define TYPE_BINA    0x2
#define TYPE_CLOSE   0x8
#define TYPE_PING    0x9
#define TYPE_PONG    0xA
#define WS_MAXLENGTH 2^63

#define MSG_CONTROL 0x400

#ifdef __PLATFORM__LINUX
#define PID_FILE "pid.pid"
#define SIGHUP   1
#define SIGTERM 15
#define SIGKILL  9
#else
#define _SERVICE_NAME "hbwsocket"
#define _SERVICE_DESCRIBE "Web Socket"
#endif

#define PORT_FILE hb_dirBase() + "port.txt"

//MESSAGES
#define H5_STARTED       0x1

static oHost
static lWorking := .F.

FUNCTION H5_GetHost()
RETURN oHost

CLASS WebSocketServer FROM HBSocket

   DATA aMaskingKey
   DATA aData

   DATA bOnCloseClient
   DATA bOnStarted 
   DATA bOnNewClient
   DATA bOnReadProcess
   DATA bOnInit
   
   DATA hMainWnd 
   DATA hMutexWork
   DATA hObjects INIT hb_Hash()
   
   DATA lBackground
   DATA lStarted
   
   METHOD New( nPort, cMode )
   
   METHOD HandleEvent( hEvents, oClient )
   METHOD HandShake( oClient )
   METHOD HybiEncode( cData, lBinary ) 
   METHOD HyBiDecode( cData )
   
   METHOD KillBack()

   METHOD generateMaskingKey()
   METHOD Mask()
   METHOD UnMask()
   
   METHOD NewClient( oClient ) 
   METHOD OnReadProcess( oClient )
   METHOD OnClose( oClient )
   
   METHOD Activate()
//#ifdef __PLATFORM__LINUX
//   METHOD SignalHandler( nSignal )
//#endif   


   METHOD StartControl( oClient )
   METHOD SetObject( o ) INLINE ::hObjects[ o:cID ] := o
   

ENDCLASS

//-----------------------------------------//

METHOD New( cPort, cMode, bOnInit, bOnStarted ) CLASS WebSocketServer

   LOCAL nError
   LOCAL cMsg
   LOCAL nProcess
   LOCAL nFork
   
   oHost      = Self
   ::hMainWnd = hb_Hash()
   ::hMutexWork = hb_MutexCreate()
   
   ::lStarted    = .F.
   ::lBackground = .T.
   
   ::bOnInit = bOnInit
   ::bOnStarted = bOnStarted
   
   IF ! File( PORT_FILE ) .OR. ! Empty( cPort )
      DEFAULT cPort TO "2000"   
      MemoWrit( PORT_FILE, AllTrim( cPort ) )
   ENDIF

   ::nPort = Val( cPort )
   
#ifdef __PLATFORM__LINUX
   DEFAULT cMode TO "start"
   cMode = Upper( cMode )
   cls
   if cMode == "START"
     if File( PID_FILE )
        QOut(  "daemon is already running" + CRLF )
        return nil
     else
        MemoWrit( PID_FILE, AllTrim( Str( getpid() ) ) )
     endif
   elseif cMode == "STOP"
     if File( PID_FILE )
        nProcess = Val( MemoRead( PID_FILE ) ) + 1
        hb_tracelog( "finishing daemon", nProcess )
        KillTerm( nProcess )
        return nil
     endif
   elseif Upper( cMode ) == "NONE"
      ::lBackground = .F.
   else
     QOut(  CRLF + "daemon. Syntax:" + CRLF )
     QOut(  "   ./daemon start" + CRLF )
     QOut(  "   ./daemon stop" + CRLF + CRLF  )
     return nil
   endif   

   IF ::lBackground

      SetSignalsHandler()
      nFork = Fork()
      if nFork  > 0
        return nil  // Parent process ends and child process continues
      endif
      umask( 0 ) // In order to write to any files (including logs) created by the daemon
      
      SetSignalAlarm()
      
      QOut( "daemon starts" + CRLF )      
   ENDIF

   // Close standard files descriptors
   // CloseStandardFiles()
#endif

#ifdef __PLATFORM__WINDOWS
   DEFAULT cMode TO "S" /* NOTE: Must be the default action */
   SWITCH Upper( cMode )
   CASE "INSTALL"

      IF win_serviceInstall( _SERVICE_NAME, _SERVICE_DESCRIBE )
         ? "Service has been successfully installed"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error installing service: " + hb_ntos( nError ) + " " + cMsg
      ENDIf
      EXIT

   CASE "UNINSTALL"

      IF win_serviceDelete( _SERVICE_NAME )
         ? "Service has been deleted"
         ferase( PORT_FILE )
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error deleting service: " + hb_ntos( nError ) + " " + cMsg
      ENDIf
      EXIT
   CASE "STOP"
      if win_serviceStoped( _SERVICE_NAME )
          ? "Service has stoped"
      else 
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Service has had some problems: " + hb_ntos( nError ) + " " + cMsg      
      endif      
      exit
   CASE "START"
      if( win_serviceInitiate( _SERVICE_NAME ) )
          ? "Service has started OK"
      else 
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Service has had some problems: " + hb_ntos( nError ) + " " + cMsg      
      endif
      exit
   CASE "NONE"
      /* NOTE: Used run like a app */      
      ::lBackground = .F.
//      SrvMain()
      EXIT
   CASE "S"
      /* NOTE: Used when starting up as service.
               Do not invoke the executable manually with this option */

      IF win_serviceStart( _SERVICE_NAME, {|| ::Activate() }, , 2 /*WIN_SERVICE_AUTO_START*/ )
         ? "Service has started OK"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Service has had some problems: " + hb_ntos( nError ) + " " + cMsg
      ENDIF    
   OTHERWISE
      ? "Invalid parameter"
      EXIT

   ENDSWITCH
#endif
   ::Activate()
RETURN Self

//-----------------------------------------//

METHOD KillBack() CLASS WebSocketServer
   local nProcess
   
   IF ::lBackground
#ifdef __PLATFORM__LINUX
   QOut(  CRLF + "daemon ends" + CRLF )
   nProcess = Val( MemoRead( PID_FILE ) ) + 1
   FErase( PID_FILE )
   KillKill( nProcess )
#endif   
#ifdef __PLATFORM__WINDOWS
    win_serviceSetExitCode( 0 )
    win_serviceStop()
#endif
   ENDIF
   
RETURN NIL

//-----------------------------------------//

METHOD Activate() CLASS WebSocketServer
   
   local oError

   if ::lStarted
      RETURN NIL 
   endif
   
   ::lStarted = .T.
   
   BEGIN SEQUENCE WITH {| oErr | UErrorHandler( oErr, Self ) }
   
   Super:new( ::nPort )
   
   if ::lBackground
#ifdef  __PLATFORM__WINDOWS
      ::bOnProccess = {|| ! ( win_serviceGetStatus() == WIN_SERVICE_RUNNING ) }      
#endif    
   else 
      cls
//      ::bOnProccess = {|| inkey( 0.3 ), LastKey() == 27 }      
      ? "Press <ESC> to exit!!!"
   endif
   ::lDebug    = .t.
   ::bDebug    = {| aParam | LogFile( "debug_socket.txt", aParam ) }
   ::bOnRead   = {| oSrv, oClient | ::OnReadProcess( oClient ) }
   ::bOnAccept = {| oSrv, oClient | ::NewClient( oClient )  }
   ::bOnKillClient = {| oClient | ::OnClose( oClient ) }
   if hb_isBlock( ::bOnInit )
      Eval( ::bOnInit, Self )
   endif
   ::Listen()
   ::End()
   RECOVER USING oError
 //     LogFile( "error.log", { oError:description } )
   ENDSEQUENCE

   ::KillBack()
   
   ::lStarted = .F.
   
RETURN NIL   

//-----------------------------------------//

METHOD HandleEvent( hEvents, oClient ) CLASS WebSocketServer

   local nEvent 
   local wParam 
   local lParam 
   local cParam 
   
   IF hEvents != NIL 
      nEvent := hEvents[ "message" ]
      wParam := hEvents[ "wParam"]
      lParam := hEvents[ "lParam" ]
      cParam := hEvents[ "cParam" ]
      

      SWITCH nEvent
         CASE H5_STARTED /*STARTED*/
            IF hb_isBlock( ::bOnStarted )
               Eval( ::bOnStarted, oClient )
            ENDIF
            EXIT
         CASE 0x2 //CLICK ON MENU ITEM      
            H5_GetMainMenu()[ lParam ]:HandleEvent( nEvent, wParam, lParam )
            EXIT
         CASE 0x3 //MOUSE MOVE
            ::hObjects[ lParam ]:OnMouseMove( H5_HighWord( wParam ), H5_LowWord( wParam ) )         
            EXIT         
      ENDSWITCH
      
   ELSE 
      ? "ERROR"
   ENDIF
   
RETURN NIL

//-----------------------------------------//

METHOD HandShake(  oClient ) CLASS WebSocketServer
   local nLen, cBuffer, cContext, cKey, cSend
   
   cBuffer := oClient:cBuffer

   cContext = GetContext( cBuffer, "Sec-WebSocket-Key" )
   cKey     = hb_Base64Encode( hb_sha1( cContext + MAGIC_KEY, .T. ) ) // + "." add something to check that the handshake gets wrong
   
   cSend = "HTTP/1.1 101 Switching Protocols" + CRLF + ;
           "Upgrade: websocket" + CRLF + ;
           "Connection: Upgrade" + CRLF + ;
           "Sec-WebSocket-Accept: " + cKey + CRLF + CRLF   

   nLen = ::SendData( oClient, cSend )

   IF nLen > 0
      oClient:lHandShake = .T.          
   ENDIF

RETURN NIL 

//-----------------------------------------//
#define TMR_PINGPONG 5

METHOD NewClient( oClient ) CLASS WebSocketServer

   __objAddData( oClient, "lHandShake" )  
   __objAddData( oClient, "nPingPongControl" ) 
   __objAddData( oClient, "nDataLength" ) 
   __objAddData( oClient, "oMainWnd" ) 
   __objModMethod( oClient, "SendData", @Send() )
   __objAddMethod( oClient, "PingPongControl", @PingPongControl() )
   __objAddMethod( oClient, "doPing", @doPing() )
   __objAddMethod( oClient, "doPong", @doPong() )
   
   oClient:lHandShake = .F.
   oClient:nTimeOut   = 0
   
//   ::StartControl( oClient )
   IF hb_isBlock( ::bOnNewClient )
      Eval( ::bOnNewClient, oClient )
   ENDIF

RETURN NIL

//-----------------------------------------//

METHOD StartControl( oClient ) CLASS WebSocketServer

   oClient:nPingPongControl = Seconds() + TMR_PINGPONG

   if oClient:hSocket != NIL
      hb_ThreadStart( {| nTime | oClient:PingPongControl( nTime ) }, TMR_PINGPONG )
   endif

RETURN NIL

//-----------------------------------------//

METHOD OnClose( oClient ) CLASS WebSocketServer
   
   IF hb_isBlock( ::bOnCloseClient )
      Eval( ::bOnCloseClient, oClient )
   ENDIF

   oClient = NIL
   
RETURN NIL

//-----------------------------------------//

METHOD OnReadProcess( oClient ) CLASS WebSocketServer

   LOCAL cDecodeData := ""
   LOCAL cBuffer := oClient:cBuffer
   LOCAL nLenMsg
   LOCAL hJSon, nJSon

   IF ! oClient:lHandShake
      ::handshake( oClient )
   ELSE 
      IF HB_At( Chr( 129 ), cBuffer ) > 0 .OR. ;
         HB_At( Chr( 1 ), cBuffer ) > 0 .OR. ;
         HB_At( Chr( 130 ), cBuffer ) > 0
         cDecodeData = ::HyBiDecode( cBuffer, oClient )
      ELSEIF HB_At( Chr( 137 ), cBuffer ) > 0
         cDecodeData = chr( 0x8A ) + ::HyBiDecode( cBuffer, oClient )
         ::oServer:SendData( oClient, cDecodeData )
         oClient:End()
         RETURN .F.
      ELSE 
         ::Debug( 'Data incorrectly framed. Dropping connection' )
         oClient:End()
         RETURN .F.
      ENDIF

         nJSon = hb_jsonDecode( cDecodeData, @hJSon )
         IF hJSon != NIL
            ::HandleEvent( hJSon, oClient )
         ENDIF         
               
      IF hb_isBlock( ::bOnReadProcess )
         Eval( ::bOnReadProcess, cDecodeData, oClient )

      ENDIF   
   ENDIF

RETURN .T.

//-----------------------------------------//

METHOD unMask() CLASS WebSocketServer
   local cUnMask

   /* We need 4 bytes in the masking key */
   IF Len( ::aMaskingKey ) == 4
      /* Unmasking uses the same algorithm as masking */
      cUnMask = ::Mask()
   ENDIF
    
RETURN cUnMask

//-----------------------------------------//

METHOD Mask() CLASS WebSocketServer

   local aMaskingKey
   local i 
   local cMask := ""
   
   /* Do we have a valid masking key? */
   IF Len( ::aMaskingKey) == 4
       /* If so, use that key */
       aMaskingKey = ::aMaskingKey
   ELSE
       /* If not, generate one */
       aMaskingKey = ::generateMaskingKey()
   ENDIF
 
   /* Mask using XOR encryption */
   FOR i = 0 TO Len( ::aData ) - 1
      cMask += Chr( hb_BitXOR( Asc( ::aData[ i + 1 ] ), Asc( aMaskingKey[ ( i%4 ) + 1 ] ) ) )
   NEXT
   
RETURN cMask

//-----------------------------------------//

METHOD generateMaskingKey() CLASS WebSocketServer
   local aMaskingKey := {}
   local i 
    
   FOR i = 1 TO 4
       AAdd( aMaskingKey,  int( hb_random( 0, 0xFF ) ) )
   NEXT 
       
RETURN aMaskingKey

//-----------------------------------------//

METHOD HyBiDecode( cData, oClient ) CLASS WebSocketServer
   LOCAL bytes := cData
   LOCAL nDataLength := 0
   LOCAL cMask := ""
   LOCAL Coded_Data := ""
   LOCAL cDecodeData := ""
   LOCAL secondByte := Asc( SubStr( bytes, 2, 1 ) )
   LOCAL lMasked := hb_BitAnd( 0x80, secondByte ) == 0x80
   
   nDataLength = hb_BitAnd( 0x7F, secondByte )     
   
   
   IF lMasked
      IF nDataLength == 126
         cMask = SubStr( bytes, 5, 4 )
         Coded_Data = SubStr( bytes, 9 )
      ELSEIF nDataLength == 127 
         cMask = SubStr( bytes, 11, 4 )
         Coded_Data = SubStr( bytes, 15 )
      ELSE 
         cMask = SubStr( bytes, 3, 4 )
         Coded_Data = SubStr( bytes, 7 )
      ENDIF 
      ::aMaskingKey = SToA( cMask )
      ::aData = SToA( Coded_Data )
      cDecodeData = ::UnMask()
   ELSE 
      IF nDataLength == 126 
         cDecodeData = SubStr( bytes, 5 )
      ELSEIF nDataLength == 127 
         cDecodeData = SubStr( bytes, 11 )
      ELSE 
         cDecodeData = SubStr( bytes, 3 )
      ENDIF 
   ENDIF
   
   oClient:nDataLength = nDataLength

RETURN cDecodeData

//-----------------------------------------//

METHOD HybiEncode( cData, lBinary ) CLASS WebSocketServer
   LOCAL aDataBuffer := {}
   LOCAL nSendLength := Len( cData )
   LOCAL nRawBytesSend := nSendLength + 2 
   LOCAL cPacket := ""
   LOCAL lo, hi, c
   
   IF nSendLength > 0xFFFF
      //64 bits
      aDataBuffer = Array( 10 )
      aDataBuffer[ 2 ] = 127 
      lo = hb_bitor( nSendLength, 0 )
      hi = ( nSendLength - lo ) / 0x100000000
      aDataBuffer[ 3 ] = hb_bitAnd( hb_bitShift( hi, -24 ), 0xFF )
      aDataBuffer[ 4 ] = hb_bitAnd( hb_bitShift( hi, -16 ), 0xFF )
      aDataBuffer[ 5 ] = hb_bitAnd( hb_bitShift( hi, -8 ), 0xFF )
      aDataBuffer[ 6 ] = hb_bitAnd( hi, 0xFF )

      aDataBuffer[ 7 ] = hb_bitAnd( hb_bitShift( lo, -24 ), 0xFF )
      aDataBuffer[ 8 ] = hb_bitAnd( hb_bitShift( lo, -16 ), 0xFF )
      aDataBuffer[ 9 ] = hb_bitAnd( hb_bitShift( lo, -8 ), 0xFF )
      aDataBuffer[ 10 ] = hb_bitAnd( lo, 0xFF )  
      nRawBytesSend += 8          
   ELSEIF nSendLength > 0xFF
      //16 bits
      aDataBuffer = Array( 4 )
      aDataBuffer[ 2 ] = 126 
      aDataBuffer[ 3 ] = hb_bitAnd( hb_bitShift( nSendLength, -8 ), 0xFF )
      aDataBuffer[ 4 ] = hb_bitAnd( nSendLength, 0xFF )
      nRawBytesSend += 2
   ELSE
      aDataBuffer = Array( 2 )
      aDataBuffer[ 2 ] = nSendLength
   ENDIF
   
   aDataBuffer[ 1 ] = ( 128 + If( lBinary, 2, 1 ) )
   
   FOR EACH c IN aDataBuffer 
      cPacket += Chr( c )
   NEXT 
   FOR EACH c IN cData
      cPacket += c
   NEXT 
//   ? cPacket
RETURN cPacket    


//#ifdef __PLATFORM__LINUX
//METHOD SignalHandler( nSignal ) CLASS WebSocketServer
//
//  DO CASE 
//     CASE nSignal == SIGHUP
//          QOut(  "Received SIGHUP signal" + CRLF  )
//     CASE nSignal == SIGKILL .or. nSignal == SIGTERM
//          QOut( "to exit..." + CRLF )
//          ::lExit = .T. 
//     OTHERWISE
//          QOut(  "Unhandled SIGNAL " + AllTrim( Str( nSignal ) ) + CRLF )
//  ENDCASE
//
//RETURN NIL
//
//#endif

function SignalHandler( nSignal )
  local cOut := ""
  do case 
     case nSignal == SIGHUP
          cOut = "Received SIGHUP signal" + CRLF
     case nSignal == SIGKILL .or. nSignal == SIGTERM
          cOut = "Received SIGTERM signal" + CRLF
          FErase( PID_FILE )
     otherwise
          cOut = "Unhandled SIGNAL " + AllTrim( Str( nSignal ) ) + CRLF 
  endcase


return nil

//-----------------------------------------//

STATIC FUNCTION doPing()
   local Self := QSelf()
   ::oServer:Debug( "HACINENDO PING" )
   ::SendData( , TYPE_PING )
   
RETURN NIL

//-----------------------------------------//

STATIC FUNCTION doPong()
   local Self := QSelf()
   ::oServer:Debug( "HACINENDO PONG" )
   ::SendData( , TYPE_PONG )
   
RETURN NIL

//-----------------------------------------//

STATIC FUNCTION PingPongControl( nTimeOut ) 
   
   local nSec
   local Self := QSelf()

   DO WHILE ::nPingPongControl > ( nSec := seconds() ) .AND. ::hSocket != NIL

      hb_idlesleep( 1 )
   enddo
   
   ::doPing()
   ::oServer:StartControl( Self )
   
RETURN NIL

//-----------------------------------------//

STATIC FUNCTION Send( cData, nType ) 
   LOCAL Self := QSelf()
   LOCAL encodedData := ::oServer:hybiEncode( cData, .F.)
   
   ::oServer:SendData( Self, encodedData )
    
RETURN NIL


//-----------------------------------------//

STATIC FUNCTION GetContext( cData, cContext )

  local nLen := Len( cContext )
  local cValue := ""
  local aLines
  local aSubLine
  local cRow
  
  aLines := hb_ATokens( cData, CRLF )
  FOR each cRow IN aLines
     IF cContext $ cRow
        aSubLine = hb_ATokens( cRow, ":" )
        cValue = AllTrim( aSubLine[ 2 ] )
        EXIT
     ENDIF
  NEXT

RETURN cValue

//-----------------------------------------//

FUNCTION H5_GetClient()

   local oClient, o
   local pThread := hb_ThreadSelf()
   
   FOR EACH o IN oHost:hClients
      oClient = o
      IF oClient:pThread == pThread         
         EXIT 
      ENDIF
   NEXT

RETURN oClient


//-----------------------------------------//

FUNCTION H5_HIGHWORD( a )
RETURN hb_BitShift( ( a - H5_LOWWORD( a ) ), -16 );

//-----------------------------------------//

FUNCTION H5_LOWWORD( a )
RETURN hb_BitAnd( a, ( hb_BitShift( 1, 16 ) -1 ) ) ;


//-----------------------------------------//


#pragma BEGINDUMP
#if defined( HB_OS_UNIX )	
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>
PHB_ITEM pSelf;
#endif //HB_OS_UNIX
#include <hbapi.h>
#include <hbvm.h>
#include <hbapiitm.h>
#include <hbapierr.h>

#if defined( HB_OS_UNIX )	
HB_FUNC( FORK )
{
 hb_retnl( fork() );
}

HB_FUNC( UMASK )
{
  umask( hb_parnl( 1 ) );
}

HB_FUNC( EXIT )
{
  exit( EXIT_SUCCESS );
}

HB_FUNC( GETPPID )
{
  hb_retnl( getppid() );
}

HB_FUNC( GETPID )
{
  hb_retnl( getpid() );
}

HB_FUNC( KILLTERM )
{
  kill( hb_parnl( 1 ), SIGTERM );
}

HB_FUNC( KILLKILL )
{
  kill( hb_parnl( 1 ), SIGKILL );
}

HB_FUNC( CLOSESTANDARDFILES )
{
  close( STDIN_FILENO );
  close( STDOUT_FILENO );
  close( STDERR_FILENO );
}

void CatchAlarm( int sig )
{
 HB_SYMBOL_UNUSED( sig );
}

void SignalHandler( int sig )
{
  hb_vmPushSymbol( hb_dynsymGetSymbol( "SIGNALHANDLER" ) );
  hb_vmPushNil();
  hb_vmPushLong( sig );
  hb_vmFunction( 1 );
}

HB_FUNC( SETSIGNALALARM )
{
 signal( SIGALRM, CatchAlarm );
}

HB_FUNC( SETSIGNALSHANDLER )
{
  signal( SIGHUP, SignalHandler );
  signal( SIGTERM, SignalHandler );
  signal( SIGINT, SignalHandler );
  signal( SIGQUIT, SignalHandler );
  signal( SIGKILL, SignalHandler );
}

HB_FUNC( ALARM )
{
 alarm( hb_parnl( 1 ) );
}

HB_FUNC( SLEEP )
{
 sleep( hb_parnl( 1 ) );
}

HB_FUNC( SYSLOG )
{
  syslog( hb_parnl( 1 ), hb_parc( 2 ), hb_parc( 3 ) );
}

HB_FUNC( TEST )
{
  hb_retnl( LOG_WARNING );
}
#endif //HB_OS_UNIX

HB_FUNC( STOA ){

   const char * cText = hb_parc( 1 );
   if( HB_ISCHAR( 1 ) )
   {
      HB_SIZE nTextLength = hb_parclen( 1 );
      PHB_ITEM pOut = hb_itemArrayNew( nTextLength );
      HB_SIZE nPos;

      for( nPos = 0; nPos < nTextLength; nPos ++ )
         hb_arraySetCL( pOut, nPos + 1, cText++, 1 );
      
       hb_itemReturnRelease( pOut );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1108, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

}

#pragma ENDDUMP



//#include "hbsocket.prg"
