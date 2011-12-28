//wssocket.prg
#include "hbcompat.ch"
#include "hbwin.ch"
#include "common.ch"

#define _SERVICE_NAME "hbwsocket"
#define _SERVICE_DESCRIBE "Harbour Web Socket"

#define CRLF chr(13)+chr(10)
                
#define MAGIC_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

#ifdef HB_HOST_PLAT_UNIX
#define PID_FILE "pid.pid"
#define SIGHUP   1
#define SIGTERM 15
#define SIGKILL  9
#endif

#define PORT_FILE hb_dirBase() + "port.txt"
static oServer, oPP
static nPort, lBackground

extern hb_version

PROCEDURE Main( cMode, cPuerto )
   LOCAL nError
   LOCAL cMsg
   
   lBackground = .T.
   
   IF ! File( PORT_FILE ) .OR. ! Empty( cPuerto )
      DEFAULT cPuerto TO "2000"   
      MemoWrit( PORT_FILE, AllTrim( cPuerto ) )
   ENDIF
   //adjust include path
   
#ifdef HB_HOST_PLAT_UNIX
   DEFAULT cMode TO "start"
   cls
   if cMode == "start"
     if File( PID_FILE )
        QOut(  "daemon is already running" + CRLF )
        return nil
     else
        MemoWrit( PID_FILE, AllTrim( Str( getpid() ) ) )
     endif
   elseif cMode == "stop"
     if File( PID_FILE )
        nProcess = Val( MemoRead( PID_FILE ) ) + 1
        hb_tracelog( "finishing daemon", nProcess )
        KillTerm( nProcess )
        return nil
     endif
   elseif Upper( cMode ) == "N"
      lBackground = .F.
   else
     QOut(  CRLF + "FiveTech daemon. Syntax:" + CRLF )
     QOut(  "   ./daemon start" + CRLF )
     QOut(  "   ./daemon stop" + CRLF + CRLF  )
     return nil
   endif   

   SetSignalsHandler()

   if Fork() > 0
     return nil  // Parent process ends and child process continues
   endif

   umask( 0 ) // In order to write to any files (including logs) created by the daemon

   SetSignalAlarm()

   QOut(  "daemon starts" + CRLF )

   // Close standard files descriptors
   // CloseStandardFiles()
#endif

#ifdef __PLATFORM__WINDOWS
   DEFAULT cMode TO "S" /* NOTE: Must be the default action */
   SWITCH Upper( cMode )
   CASE "I"

      IF win_serviceInstall( _SERVICE_NAME, _SERVICE_DESCRIBE )
         ? "Service has been successfully installed"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error installing service: " + hb_ntos( nError ) + " " + cMsg
      ENDIf
      EXIT

   CASE "U"

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
   
   CASE "N"
      /* NOTE: Used run like a app */      
      lBackground = .F.
      SrvMain()
      EXIT
   CASE "S"
      /* NOTE: Used when starting up as service.
               Do not invoke the executable manually with this option */

      IF win_serviceStart( _SERVICE_NAME, @SrvMain() )
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
   RETURN

PROCEDURE SrvMain()
   local oServer
   local cInclude
   local oError
   
   nPort = Val( MemoRead( PORT_FILE ) )
   
   cInclude = hb_getEnv( "INCLUDE" )
   if Empty( cInclude )
      cInclude = hb_getEnv( "INCLUDE_DIR" )
      if Empty( cInclude )
         cInclude = "\harbour\include"
      endif      
   endif
   BEGIN SEQUENCE
   
   oPP = __pp_Init( cInclude, "std.ch" )
   oServer = HB_Socket():new( nPort )
      if lBackground
#ifdef  __PLATFORM__WINDOWS
         oServer:bOnProccess = {|| ! ( win_serviceGetStatus() == WIN_SERVICE_RUNNING ) }
#endif    
      else 
         ? "Press <ESC> to exit!!!"
      endif
      oServer:lDebug = .T.
      oServer:bDebug = {| aParam | LogFile( "debug.txt", aParam ) }
      oServer:bOnRead = {| oSrv, oClient | OnReadProcess( oClient ) }
      oServer:Listen()
      oServer:End()
   RECOVER USING oError
      LogFile( "error.log", { oError:description } )
   ENDSEQUENCE

   KillBack()

RETURN

PROCEDURE KillBack()
#ifdef HB_HOST_PLAT_UNIX
   local nProcess
   QOut(  CRLF + "daemon ends" + CRLF )
   nProcess = Val( MemoRead( PID_FILE ) ) + 1
   FErase( PID_FILE )
   KillKill( nProcess )
#endif   
#ifdef __PLATFORM__WINDOWS
    win_serviceSetExitCode( 0 )
    win_serviceStop()
#endif

RETURN

#ifdef HB_HOST_PLAT_UNIX
function SignalHandler( nSignal )

  do case 
     case nSignal == SIGHUP
          QOut(  "Received SIGHUP signal" + CRLF  )
     case nSignal == SIGKILL .or. nSignal == SIGTERM
          QOut( "to exit..." + CRLF )
          oServer:lExit = .T. 
     otherwise
          QOut(  "Unhandled SIGNAL " + AllTrim( Str( nSignal ) ) + CRLF )
  endcase

return nil

#endif

static function OnReadProcess( oClient )
   
   local cMask, cData, n, cMsg:="", oError
   local nMaskPos := 1, cAnswer
   
   //handshake
   if oClient:Cargo == NIL .OR. ( hb_HHasKey( oClient:Cargo, "HANDSHAKE" ) .AND. ! oClient:Cargo[ "HANDSHAKE" ] )
      HandShake( oClient ) 
   else 
      cData = oClient:cBuffer
      cMask = SubStr( cData, 3, 4 )
      for n = 1 to Len( SubStr( cData, 7 ) )
          cMsg += Chr( hb_BitXOR( Asc( SubStr( cMask, nMaskPos++, 1 ) ), Asc( SubStr( cData, 6 + n, 1 ) ) ) )
          if nMaskPos == 5
             nMaskPos = 1
          endif   
      next 
   
      oClient:SendData( chr( 129 ) + chr( len( cMsg ) ) + hb_StrToUTF8( cMsg ) )
      
      BEGIN SEQUENCE
         cMsg = __pp_Process( oPP, cMsg )
         cAnswer = uValToChar( &cMsg ) 
      RECOVER USING oError
         cAnswer = "Error: " + oError:Description
      ENDSEQUENCE      

      oClient:SendData( Chr( 129 ) + Chr( Len( cAnswer ) ) + hb_StrToUtf8( cAnswer ) )
   endif

return nil

//-----------------------------------------//

static function HandShake(  oClient )
   local nLen, cBuffer, cContext, cKey, cSend
   
   cBuffer := oClient:cBuffer

   cContext = GetContext( cBuffer, "Sec-WebSocket-Key" )
   cKey     = hb_Base64Encode( hb_sha1( cContext + MAGIC_KEY, .T. ) ) // + "." add something to check that the handshake gets wrong
   
   cSend = "HTTP/1.1 101 Switching Protocols" + CRLF + ;
           "Upgrade: websocket" + CRLF + ;
           "Connection: Upgrade" + CRLF + ;
           "Sec-WebSocket-Accept: " + cKey + CRLF + CRLF   

   nLen = oClient:SendData( cSend )

   IF nLen > 0
      oClient:Cargo = hb_Hash()
      oClient:cargo[ "HANDSHAKE" ] = .T.    
      
   ENDIF

   
return nil 

//-----------------------------------------//

static function GetContext( cData, cContext )

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

#ifdef HB_HOST_PLAT_UNIX

#pragma BEGINDUMP

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

#include <hbapi.h>
#include <hbvm.h>

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

#pragma ENDDUMP

#endif 

#include "hbsocket.prg"