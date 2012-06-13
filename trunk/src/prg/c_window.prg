/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#include "h5.ch"

CLASS C_Window FROM C_Object
   
   CLASSDATA nID INIT 0
   
   DATA oMenu
      
   METHOD New() 
   METHOD End()
   METHOD Activate()

ENDCLASS

//-----------------------------------------//

METHOD New( oMenu ) CLASS C_Window

   ::oMenu = oMenu
   ::GetID( "WND" )
   ::SetObjects( Self )

RETURN Self

//-----------------------------------------//

METHOD End() CLASS C_Window

   IF ::oMenu != NIL 
      ::oMenu:End()
   ENDIF

RETURN NIL

//-----------------------------------------//

METHOD Activate() CLASS C_Window

   local oClient := H5_GetClient()
   local cJSon, nlen, oObjects, hJ
//   ? "paso2222"
   hJ = hb_Hash()

   hJ[ "ACTION" ] = "BUILDCONTROLS"
   hJ[ "PARAMETERS" ] = {}

   ::hJSon[ "ACTION" ] = "CONFIGWND"
   ::hJSon[ "PARAMETERS" ] = { "ID" => ::cID, "ONMOUSEMOVE" => ::bOnMouseMove }
   
   AAdd( hJ[ "PARAMETERS" ], ::hJSon ) 
   
   IF ::oMenu != NIL
   
      ::oMenu:hJSon[ "PARENT" ] = ::cID
      AAdd( hJ[ "PARAMETERS" ], ::oMenu:hJSon )  
      
   ENDIF

   IF oClient:oMainWnd == NIL 
      oClient:oMainWnd = Self      
   ENDIF
   
   FOR EACH oObjects IN ::hObjects      
      AAdd( hJ[ "PARAMETERS" ], oObjects:BuildJSon() )      
   NEXT
   
   cJSon := hb_JSonEncode( hJ, @nlen, .T. )

   oClient:SendData( cJSon )

RETURN hJ

//-----------------------------------------//
