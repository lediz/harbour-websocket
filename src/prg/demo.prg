/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#include "h5.ch"


PROCEDURE Main( cPuerto, cMode )
   LOCAL nError
   LOCAL cMsg
   local oWebSocket
   
   DEFAULT cPuerto TO "8090" 
   DEFAULT cMode   TO "NONE" 
   
   oWebSocket = WebSocketServer():New( cPuerto, cMode, , {|| Comienza( ) } )
   
//   DEFINE MAINAPP oWebSocket PORT cPuerto MODE cMode
//? cPuerto, ValType( oWebSocket ), oWebSocket, cMode
//   oWebSocket:bOnStarted    = {| oClient | Comienza( ) }
//   oWebSocket:bOnNewClient  = {| oClient | QOut( "Client Accepted!!!" ) }
//   oWebSocket:bOnCloseClient = {| oClient | QOut( "Client Closed!!!" ) }
   
//   ACTIVATE MAINAPP oWebSocket

RETURN

STATIC FUNCTION Comienza( )

   local oMenu, oWnd, oSay, n, j
   local oRow, oCol
   
   MENU oMenu
      MENUITEM "Test 1"
      MENU 
         MENUITEM "Date" ACTION ( MsgAlert( DToC( date() ) ) )
         MENUITEM "Time" ACTION ( MsgAlert( time() ) )
         MENUITEM "Harbour Version" ACTION ( MsgAlert( hb_version() ) )
         MENUITEM "Test 1.2"
         MENU 
            MENUITEM "Test 1.2.1"
            MENUITEM "Test 1.2.2"
            MENUITEM "Test 1.2.3"
            MENUITEM "Test 1.2.4"
            MENU
               MENUITEM "Test 1.2.4.1"
               MENUITEM "Test 1.2.4.2"
               MENUITEM "Test 1.2.4.3"
               MENUITEM "Test 1.2.4.4"  ACTION ( MsgAlert( "Item Clicked " + oSender:cPrompt ) )          
            ENDMENU            
         ENDMENU
         MENUITEM "Test 1.3"
         MENUITEM "Test 1.4"
      ENDMENU
      MENUITEM "Test 2"      
      MENUITEM "Test 3"
      MENUITEM "Test 4"

   ENDMENU
   
   
   oWnd = C_Window():New( oMenu )
   
   
   C_Say():New( 50, 20, "Row:", oWnd )
   C_Say():New( 70, 20, "Col:", oWnd )
   oRow = C_Say():New( 50, 50, ".", oWnd )
   oCol = C_Say():New( 70, 50, ".", oWnd )   

//   oWnd:bOnMouseMove = {| r, c | Mueve( r, c, oRow, oCol ) }
//   ? "pasooo1"
   
   oWnd:Activate()
   

RETURN NIL

FUNCTION Mueve( nRow, nCol, oRow, oCol )
   oRow:SetText( Str( nRow ) )
   oCol:SetText( Str( nCol ) )

RETURN NIL
  
