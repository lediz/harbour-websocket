/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/

#include "hbclass.ch"
#include "hbsocket.ch"
#include "fileio.ch"
#include "common.ch"
#include "error.ch"


#define CRLF chr(13)+chr(10)
#define BUFFER_SIZE 4096

//-----------------------------------------//

CLASS HBSocket

   DATA   cBindAddress INIT "0.0.0.0"

   DATA bDebug
   DATA bOnAccept
   DATA bOnClose
   DATA bOnListen
   DATA bOnRead
   DATA bOnWrite
   DATA bOnProccess
   DATA bOnKillClient
 
   DATA cBuffer
   DATA cLogFile
   DATA cErrorLog
   DATA cError
 
   DATA hClients
   DATA hMutexUser
   DATA hMutexServer

   DATA nClientId
   DATA nPort

   DATA lDebug
   DATA lExit        

   DATA pSocket
   DATA oPP

   METHOD New()
   METHOD End()


   METHOD KillClient( oClient )
   
   METHOD Listen()

   METHOD NewId() INLINE ::nClientId++
   
   METHOD OnAccept( oClient )
   METHOD OnClose() VIRTUAL
   METHOD OnRead( oClient )
   
   METHOD SendData( cText ) 
   
//   HIDDEN:
   
   METHOD Debug( cText ) 


ENDCLASS

//-----------------------------------------//

METHOD New( nPort, oPP ) CLASS HBSocket

   DEFAULT nPort TO 8880
   
   ::nClientId = 1
   ::nPort     = nPort
   ::hClients  = hb_Hash()
   ::cErrorLog = "error.log"

   ::lExit     = .F.
   ::lDebug    = .F. 
   
   ::oPP       = oPP
   

RETURN Self

//-----------------------------------------//

METHOD End() CLASS HBSocket

   LOCAL oClient
   LOCAL hClone := hb_HClone( ::hClients )
   
   for each oClient in hClone
      ::Debug( "CLIENT CLOSED", oClient:hSocket, oClient:nID )
      ::KillClient( oClient )
   next
   
   if ::pSocket != NIL
      HB_SocketClose( ::pSocket )
      ::pSocket = NIL
   endif
   
   if ::bOnClose != NIL 
      Eval( ::bOnClose, Self )
   endif      

RETURN nil

//-----------------------------------------//

METHOD Debug( ... ) CLASS HBSocket
   
   local aParams := hb_aParams()

   IF ::lDebug 
   
      IF hb_IsBlock( ::bDebug )
         Eval( ::bDebug, aParams )
      ELSE
         AEval( aParams, {| u | QOut( u ) } )
      ENDIF
      
   ENDIF

RETURN NIL

//------------------------------------------------------//

METHOD KillClient( oClient ) CLASS HBSocket
   
   local nID 
   if oClient:hSocket != NIL
   IF hb_IsBlock( ::bOnKillClient )
      Eval( ::bOnKillClient, oClient )
   ENDIF
   
   ::Debug( "KILLCLIENT", oClient:hSocket )
   
   nID = oClient:nID   
   
//   oClient:End()
//   oClient = NIL
   hb_mutexLock( ::hMutexUser )   
   hb_HDEL( ::hClients, nID )
   hb_mutexUnlock( ::hMutexUser )
   endif

return nil

//-----------------------------------------//

METHOD Listen() CLASS HBSocket

   ::pSocket     = HB_SocketOpen( )
   ::hMutexUser  = HB_MutexCreate()   
   ::hMutexServer = HB_MutexCreate()

   IF ! HB_SocketBind( ::pSocket, { HB_SOCKET_AF_INET, ::cBindAddress, ::nPort } )
      QOut( ::cError :=  "Bind error " + hb_ntos( HB_SocketGetError() ) )
      HB_SocketClose( ::pSocket )
      RETURN .F.
   ENDIF

   IF ! HB_SocketListen( ::pSocket )
      QOut( ::cError :=  "Listen error " + hb_ntos( HB_SocketGetError() ) )
      HB_SocketClose( ::pSocket )
      RETURN .F.
   ENDIF
      
   if hb_IsBlock( ::bOnListen )
      Eval( ::bOnListen, Self )
   endif
   ::Debug( "LISTEN" )

   hb_ThreadStart( {|| ::OnAccept() } )

   DO WHILE ! ::lExit
      
      inkey( 0.5 )

      if ::bOnProccess != nil 
         ::lExit = eval( ::bOnProccess, Self )
      else 
         ::lExit := LastKey() == 27
      endif
      
               
   ENDDO    

   ::End()
   
RETURN .T.

//-----------------------------------------//

METHOD OnAccept() CLASS HBSocket

   local pClientSocket
   local oClient
   
   ::Debug( "ONACCEPT" )
      
   do while ! ::lExit

      if ! Empty( pClientSocket := HB_SocketAccept( ::pSocket,, 500 ) )
         ::Debug( "ACCEPTED", pClientSocket )
         hb_mutexLock( ::hMutexUser )
         ::NewId()
         oClient = HBSocketClient():New( Self )
         oClient:nID = ::nClientId         
         oClient:hSocket = pClientSocket
         HB_SOCKETSETSNDBUFSIZE( oClient:hSocket, BUFFER_SIZE )
         HB_SOCKETSETRCVBUFSIZE( oClient:hSocket, BUFFER_SIZE )
         hb_HSET( ::hClients, ::nClientId, oClient )
         hb_mutexUnlock( ::hMutexUser )
         if ::bOnAccept != NIL
            Eval( ::bOnAccept, Self, oClient )
         endif 
         hb_ThreadStart( {| oClient | ::OnRead( oClient ), oClient:= NIL, hb_GCAll( .T. ) }, oClient )          
//         hb_mutexNotify( Self:hMutexServer, pClientSocket )
                                  
      elseif ! ::lExit
         //? "Catched error ",  hb_ntos( HBSocketGetError() )
         //EXIT
      endif
   enddo
   
RETURN nil

//------------------------------------------------------//

METHOD OnRead( oClient ) CLASS HBSocket

   local lMyExit    := .F.
   local cData, oError
   local nLength  := 0
   local nRetry   := 0
   local lActive  := .T.
   local cBuffer
   local hSocket
   
   ErrorBlock( {| o | UErrorHandler( o, Self ) } )
   
   oClient:pThread = hb_ThreadSelf()
//   hb_mutexSubscribe( Self:hMutexServer, , @hSocket )      
//   if hSocket != NIL         
//      oClient:hSocket = hSocket
      oClient:TimerConnection()
      ::Debug( "CLIENT LISTEN START", oClient:hSocket )
      do while ! lMyExit .and. oClient:hSocket != NIL
      
         cBuffer = Space( BUFFER_SIZE )   
         BEGIN SEQUENCE WITH {| oErr | UErrorHandler( oErr, Self ) }
            if oClient:hSocket != NIL
               if ( nLength := HB_SocketRecv( oClient:hSocket, @cBuffer, BUFFER_SIZE, 0, 1000 ) ) > 0
                  hb_idlesleep( 0.1 )
                  oClient:cBuffer = RTrim( cBuffer )
//                  oClient:cBuffer = cBuffer
                  oClient:nBufferLength = Len( Trim( oClient:cBuffer ) )
               endif         
            else 
               lMyExit = .T.
            endif 
         RECOVER USING oError
//            ::Debug( oError:Description )
            lMyExit := .t.
         ENDSEQUENCE
      
         if lMyExit
            EXIT
         endif
         
         if nLength > 0
            ::Debug( "ONREAD", oClient:hSocket, "<" + AllTrim( oClient:cBuffer ) + ">", Len( AllTrim( oClient:cBuffer ) ) )
         endif
      
         if nLength == 0
            lMyExit = .T.         
         elseif nLength > 1      
            oClient:RestartTimerConnection()   
            if ::bOnRead != NIL
               Eval( ::bOnRead, Self, oClient )
            endif
         endif
      
      enddo  
      
      ::Debug( "CLIENT LISTEN FINISHED", oClient:hSocket )
//   else 
//      LogFile( ::cErrorLog, { "ERROR EN MUTEXSERVER" }  )
//   endif

   oClient:End()
   hb_threadJoin( oClient:pThread )

//   ::KillClient( oClient )


RETURN nil

//-----------------------------------------//

METHOD SendData( oClient, cSend ) CLASS HBSocket

   local nLen 

   ::Debug( "SENDING...", oClient:hSocket, cSend )
   
   DO WHILE Len( cSend ) > 0 .and. oClient:hSocket != NIL

      IF oClient:hSocket != NIL
      IF ( nLen := HB_SocketSend( oClient:hSocket, @cSend ) ) == - 1
         EXIT
      ELSEIF nLen > 0
         cSend = SubStr( cSend, nLen + 1 )     
      ENDIF
      ENDIF
   ENDDO
   
   if ::bOnWrite != NIL 
      Eval( ::bOnWrite, self, oClient, cSend )
   endif
   
   
RETURN nLen   

//-----------------------------------------//
//-----------------------------------------//

CLASS HBSocketClient

   DATA hSocket
   DATA nID
   DATA Cargo
   DATA oServer
   DATA cBuffer
   DATA cOutPut
   DATA nTimeOut      //time in seconds
   DATA nCurrentTime
   DATA bOnClose
   DATA nBufferLength
   DATA pThread
   DATA lTimerOn
   
   METHOD New( oServer )
   
   METHOD End() 
   
   METHOD CloseConnection()   INLINE ::oServer:KillClient( Self )
   
   METHOD RestartTimerConnection() INLINE ::nCurrentTime := Seconds() + ::nTimeOut
   
   METHOD SendData( cSend ) INLINE ::oServer:SendData( Self, cSend )
   
   METHOD TimerConnection( nTimeOut ) 
   METHOD TimeOut()

ENDCLASS

//-----------------------------------------//

METHOD New( oSrv ) CLASS  HBSocketClient

   ::oServer  = oSrv
   ::nTimeOut = 0
   ::nBufferLength = 0
   ::lTimerOn = .T.

RETURN Self

//------------------------------------------------------//

METHOD End() CLASS HBSocketClient 
   
   if ::hSocket != NIL
      ::lTimerOn = .F.
      IF hb_IsBlock( ::bOnClose )
         Eval( ::bOnClose, Self )
      ENDIF
      ::oServer:Debug( "CLIENT END ", ::hSocket )
      ::CloseConnection()
      
      if ::hSocket != NIL 
         hb_socketShutdown( ::hSocket )
         HB_SocketClose( ::hSocket )
         ::hSocket = NIL
      endif
      
   endif 
   
RETURN NIL

//------------------------------------------------------//

METHOD TimerConnection( nTimeOut ) CLASS HBSocketClient
   
   DEFAULT nTimeOut TO ::nTimeOut
   
   IF nTimeOut > 0
      ::lTimerOn = .T.
      ::nTimeOut = nTimeOut
      
      ::nCurrentTime = Seconds() + ::nTimeOut
      
      hb_ThreadStart( {|| ::TimeOut() } , self )
   ENDIF

return nil

//----------------------------------------------------------//

METHOD TimeOut() CLASS HBSocketClient

   DO WHILE ::nCurrentTime > seconds() .AND. ::lTimerOn .and. ::hSocket != NIL
      hb_idlesleep( 1 )
   enddo
   
   if ::lTimerOn .and. ::hSocket != NIL
      ::oServer:Debug( "TIME OUT ", ::hSocket  )
      ::End()
//      ::CloseConnection()
   endif 
   ::oServer:Debug( "SALIDA, " + uValToChar( ::hSocket ) + ", ", ::lTimerOn )
   
RETURN NIL

//-----------------------------------------//

FUNCTION LogFile( cFileName, aInfo )

   local hFile, cLine := DToC( Date() ) + " " + Time() + ": ", n
   
   cFileName = hb_dirBase() + cFileName
   
   for n = 1 to Len( aInfo )
      cLine += uValToChar( aInfo[ n ] ) + Chr( 9 )
   next
   cLine += CRLF

   if ! File( cFileName )
      FClose( FCreate( cFileName ) )
   endif

   if( ( hFile := FOpen( cFileName, FO_WRITE ) ) != -1 )
      FSeek( hFile, 0, FS_END )
      FWrite( hFile, cLine, Len( cLine ) )
      FClose( hFile )
   endif

RETURN NIL

//---------------------------------------------------------------------------//

static function uValToChar( uVal )

   local cType := ValType( uVal )

   do case
      case cType == "C" .or. cType == "M"
           return uVal

      case cType == "D"
           #ifdef __XHARBOUR__
              if HasTimePart( uVal )
                 return If( Year( uVal ) == 0, TToC( uVal, 2 ), TToC( uVal ) )
              endif
           #endif
           return DToC( uVal )

      #ifdef __HARBOUR__
         #ifndef __XHARBOUR__
            case cType == "T"
               return If( Year( uVal ) == 0, HB_TToC( uVal, '', Set( _SET_TIMEFORMAT ) ), HB_TToC( uVal ) )
         #endif
      #endif

      case cType == "L"
           return If( uVal, ".T.", ".F." )

      case cType == "N"
           return TStr( uVal )

      case cType == "B"
           return "{|| ... }"

      case cType == "A"
           return "{...}"

      case cType == "O"
           return If( __ObjHasData( uVal, "cClassName" ), uVal:cClassName, uVal:ClassName() )

      case cType == "H"
           return "{=>}"

      case cType == "P"
           #ifdef __XHARBOUR__
              return "0x" + NumToHex( uVal )
           #else
              return "0x" + hb_NumToHex( uVal )
           #endif

      otherwise

           return ""
   endcase

return nil

FUNCTION UErrorHandler( oErr, oServer )

   IF     oErr:genCode == EG_ZERODIV;  RETURN 0
   ELSEIF oErr:genCode == EG_LOCK;     RETURN .T.
   ELSEIF ( oErr:genCode == EG_OPEN .AND. oErr:osCode == 32 .OR. ;
         oErr:genCode == EG_APPENDLOCK ) .AND. oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF
   LogFile( oServer:cErrorLog, { GetErrorDesc( oErr ) } )
   IF oErr != NIL  // Dummy check to avoid unreachable code warning for RETURN NIL
      BREAK( oErr )
   ENDIF

   RETURN NIL

STATIC FUNCTION GetErrorDesc( oErr )

   LOCAL cRet, nI, cI, aPar, nJ, xI

   cRet := "ERRORLOG ============================================================" + hb_eol() + ;
      "Error: " + oErr:subsystem + "/" + ErrDescCode( oErr:genCode ) + "(" + hb_ntos( oErr:genCode ) + ") " + ;
      hb_ntos( oErr:subcode ) + hb_eol()
   IF !Empty( oErr:filename );      cRet += "File: " + oErr:filename + hb_eol()
   ENDIF
   IF !Empty( oErr:description );   cRet += "Description: " + oErr:description + hb_eol()
   ENDIF
   IF !Empty( oErr:operation );     cRet += "Operation: " + oErr:operation + hb_eol()
   ENDIF
   IF !Empty( oErr:osCode );        cRet += "OS error: " + hb_ntos( oErr:osCode ) + hb_eol()
   ENDIF
   IF hb_isArray( oErr:args )
      cRet += "Arguments:" + hb_eol()
      AEval( oErr:args, {| X, Y | cRet += Str( Y, 5 ) + ": " + HB_CStr( X ) + hb_eol() } )
   ENDIF
   cRet += hb_eol()

   cRet += "Stack:" + hb_eol()
   nI := 2
#if 0
   DO WHILE ! Empty( ProcName( ++nI ) )
      cRet += "    " + ProcName( nI ) + "(" + hb_ntos( ProcLine( nI ) ) + ")" + hb_eol()
   ENDDO
#else
   DO WHILE ! Empty( ProcName( ++nI ) )
      cI := "    " + ProcName( nI ) + "(" + hb_ntos( ProcLine( nI ) ) + ")"
      cI := PadR( cI, Max( 32, Len( cI ) + 1 ) )
      cI += "("
      aPar := __dbgvmParLList( nI )
      FOR nJ := 1 TO Len( aPar )
         cI += cvt2str( aPar[nJ] )
         IF nJ < Len( aPar )
            cI += ", "
         ENDIF
      NEXT
      cI += ")"
      nJ := Len( aPar )
      DO WHILE !( ValType( xI := __dbgvmVarLGet( nI, ++nJ ) ) == "S" )
         cI += ", " + cvt2str( xI )
      ENDDO
      xI := NIL
      cRet += cI + hb_eol()
   ENDDO
#endif
   cRet += hb_eol()

   cRet += "Executable:  " + HB_PROGNAME() + hb_eol()
   cRet += "Versions:" + hb_eol()
   cRet += "  OS: " + OS() + hb_eol()
   cRet += "  Harbour: " + Version() + ", " + HB_BUILDDATE() + hb_eol()
   cRet += hb_eol()

   IF oErr:genCode != EG_MEM
      cRet += "Database areas:" + hb_eol()
      cRet += "    Current: " + hb_ntos( Select() ) + "  " + Alias() + hb_eol()

      BEGIN SEQUENCE WITH {| o | BREAK( o ) }
         IF Used()
            cRet += "    Filter: " + dbFilter() + hb_eol()
            cRet += "    Relation: " + dbRelation() + hb_eol()
            cRet += "    Index expression: " + OrdKey( OrdSetFocus() ) + hb_eol()
            cRet += hb_eol()
            BEGIN SEQUENCE WITH {| o | BREAK( o ) }
               FOR nI := 1 TO FCount()
                  cRet += Str( nI, 6 ) + " " + PadR( FieldName( nI ), 14 ) + ": " + HB_VALTOEXP( FieldGet( nI ) ) + hb_eol()
               NEXT
            RECOVER
               cRet += "!!! Error reading database fields !!!" + hb_eol()
            END SEQUENCE
            cRet += hb_eol()
         ENDIF
      RECOVER
         cRet += "!!! Error accessing current workarea !!!" + hb_eol()
      END SEQUENCE

      FOR nI := 1 TO 250
         BEGIN SEQUENCE WITH {| o | BREAK( o ) }
            IF Used()
               dbSelectArea( nI )
               cRet += Str( nI, 6 ) + " " + rddName() + " " + PadR( Alias(), 15 ) + ;
                  Str( RecNo() ) + "/" + Str( LastRec() ) + ;
                  iif( Empty( OrdSetFocus() ), "", " Index " + OrdSetFocus() + "(" + hb_ntos( OrdNumber() ) + ")" ) + hb_eol()
               dbCloseArea()
            ENDIF
         RECOVER
            cRet += "!!! Error accessing workarea number: " + Str( nI, 4 ) + "!!!" + hb_eol()
         END SEQUENCE
      NEXT
      cRet += hb_eol()
   ENDIF

   RETURN cRet

STATIC FUNCTION ErrDescCode( nCode )

   LOCAL cI := NIL

   IF nCode > 0 .AND. nCode <= 41
      cI := { "ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
      NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
      "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
      "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
      "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
      "LOCK"    }[nCode]                                                                                            // 41
   ENDIF

   RETURN iif( cI == NIL, "", "EG_" + cI )

STATIC FUNCTION cvt2str( xI, lLong )

   LOCAL cValtype, cI, xJ

   cValtype := ValType( xI )
   lLong := ! Empty( lLong )
   IF     cValtype == "U"
      RETURN iif( lLong, "[U]:NIL", "NIL" )
   ELSEIF cValtype == "N"
      RETURN iif( lLong, "[N]:" + Str( xI ), hb_ntos( xI ) )
   ELSEIF cValtype $ "CM"
      IF Len( xI ) <= 260
         RETURN iif( lLong, "[" + cValtype + hb_ntos( Len( xI ) ) + "]:", "" ) + '"' + xI + '"'
      ELSE
         RETURN iif( lLong, "[" + cValtype + hb_ntos( Len( xI ) ) + "]:", "" ) + '"' + Left( xI, 100 ) + '"...'
      ENDIF
   ELSEIF cValtype == "A"
      RETURN "[A" + hb_ntos( Len( xI ) ) + "]"
   ELSEIF cValtype == "H"
      RETURN "[H" + hb_ntos( Len( xI ) ) + "]"
   ELSEIF cValtype == "O"
      cI := ""
      IF __objHasMsg( xI, "ID" )
         xJ := xI:ID
         IF ! hb_isObject( xJ )
            cI += ",ID=" + cvt2str( xJ )
         ENDIF
      ENDIF
      IF __objHasMsg( xI, "nID" )
         xJ := xI:nID
         IF ! hb_isObject( xJ )
            cI += ",NID=" + cvt2str( xJ )
         ENDIF
      ENDIF
      IF __objHasMsg( xI, "xValue" )
         xJ := xI:xValue
         IF ! hb_isObject( xJ )
            cI += ",XVALUE=" + cvt2str( xJ )
         ENDIF
      ENDIF
      RETURN "[O:" + xI:ClassName + cI + "]"
   ELSEIF cValtype == "D"
      RETURN iif( lLong, "[D]:", "" ) + DToC( xI )
   ELSEIF cValtype == "L"
      RETURN iif( lLong, "[L]:", "" ) + iif( xI, ".T.", ".F." )
   ELSEIF cValtype == "P"
      RETURN iif( lLong, "[P]:", "" ) + "0p" + HB_NumToHex( xI )
   ELSE
      RETURN  "[" + cValtype + "]"   // BS,etc
   ENDIF

   RETURN NIL


//---------------------------------------------------------------------------//

static function TStr( n )
return AllTrim( Str( n ) )

//---------------------------------------------------------------------------//


#pragma BEGINDUMP
#if defined( HB_OS_WIN )	
#include "hbapi.h"
#include "hbwapi.h"
#include "hbvm.h"
#include "hbapiitm.h"

void * pfnCallBack_OnRead;

void set_callback_Onread( void * p )
{
	void * pfnCallBack_OnRead = p;   
}

HB_FUNC( SET_CALLBACK_ONREAD )
{
   set_callback_Onread( ( void * ) hb_parptr( 1 ) );   
}

HB_FUNC( HBSOCKET_CALLBACK_ONREAD )
{
   PHB_ITEM pServer = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pClient = hb_param( 2, HB_IT_OBJECT );
   
   if( pfnCallBack_OnRead )
   {
      void (*pfn)( PHB_ITEM, PHB_ITEM );
  	  pfn = pfnCallBack_OnRead;
  	  pfn( pServer, pClient );      	
   }
   
}

HB_FUNC( WIN_SERVICEINITIATE )
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )	
	// open a handle to the SCM
	SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

	if( schSCM )
	{
      // open a handle to the service
      void * hServiceName;
      
      SC_HANDLE schSrv = OpenService( schSCM, HB_PARSTRDEF( 1, &hServiceName, NULL ), GENERIC_EXECUTE );
      if( schSrv )
      {
         if( ! ( bRetVal = StartService( schSrv, 0, NULL ) ) )
            hbwapi_SetLastError( GetLastError() );
            
      }else 
         hbwapi_SetLastError( GetLastError() );
         
      hb_strfree( hServiceName );
      
      CloseServiceHandle( schSrv );
      CloseServiceHandle( schSCM );      
               
   }else
      hbwapi_SetLastError( GetLastError() );
      
#endif	
   hb_retl( bRetVal );
}

HB_FUNC( WIN_SERVICESTOPED )
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )	
	// open a handle to the SCM
	SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );	
	if( schSCM )
	{
	   void * hServiceName;
	   // open a handle to the service
	   SC_HANDLE schSrv = OpenService( schSCM, HB_PARSTRDEF( 1, &hServiceName, NULL ), GENERIC_EXECUTE );
	   if( schSrv ){ 
	      // send the STOP control request to the service
	      SERVICE_STATUS status;
	      ControlService( schSrv, SERVICE_CONTROL_STOP, &status );
	      CloseServiceHandle( schSrv );
	      CloseServiceHandle( schSCM );
         
         if( ( bRetVal = status.dwCurrentState != SERVICE_STOPPED ) )
            hbwapi_SetLastError( GetLastError() );
         
         hb_strfree( hServiceName );
         
	   }else
         hbwapi_SetLastError( GetLastError() );
      
   }else
      hbwapi_SetLastError( GetLastError() );
#endif  
   hb_retl( bRetVal ); 
}
#endif
#pragma ENDDUMP
