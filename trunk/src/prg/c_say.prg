/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#include "h5.ch"

CLASS C_Say FROM C_Object

   CLASSDATA nID INIT 0

   DATA nRow, nCol
   DATA cPrompt

   METHOD New()
   METHOD BuildJSon()
   METHOD SetText( cText ) 

ENDCLASS

//-----------------------------------------//

METHOD New( nRow, nCol, cText, oParent ) CLASS C_Say

   DEFAULT nRow TO 1
   DEFAULT nCol TO 1
   DEFAULT cText TO "Text"
   
   ::nRow    = nRow
   ::nCol    = nCol
   ::cPrompt = cText
   ::GetId( "SAY" )
   ::oParent = oParent
   oParent:AddObject( Self )

RETURN Self

//-----------------------------------------//

METHOD BuildJSon() CLASS C_Say
   
   ::hJSon[ "ACTION" ]   = "CREATESAY"
   ::hJSon[ "PARAMETER"] = { "PARENT"=>::oParent:cID, "nRow" => ::nRow, "nCol" => ::nCol, "cText" => ::cPrompt, "ID" => ::cID }

RETURN ::hJSon

//-----------------------------------------//

METHOD SetText( cText ) CLASS C_Say
   
   local cJSon, nLen, oClient := H5_GetClient()
   
   ::cPrompt = cText
   ::hJSon[ "ACTION" ]   = "SETTEXTSAY"
   ::hJSon[ "PARAMETER"] = { "cText" => ::cPrompt, "ID" => ::cID }   
   
   cJSon := hb_JSonEncode( ::hJSon, @nlen, .T. )
   
   oClient:SendData( cJSon )   

RETURN NIL

//-----------------------------------------//