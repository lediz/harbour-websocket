/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#include "h5.ch"

CLASS C_Object

   CLASSDATA hControls INIT hb_Hash()

   DATA cID
   DATA oParent

   DATA hObjects INIT hb_Hash()
   DATA hJSon    INIT hb_Hash()
   DATA bOnMouseMove
   
   METHOD AddObject( o )

   METHOD GetId( cType  ) INLINE ::cID := Upper( AllTrim( cType ) ) + AllTrim( StrZero( ++::nID, H5_MAX_ID ) )   
   
   METHOD OnMouseMove( nRow, nCol )
   
   METHOD SetObjects( ) INLINE H5_GetHost():SetObject( Self )
   
ENDCLASS


//-----------------------------------------//

METHOD AddObject( o ) CLASS C_Object

   ::hObjects[ o:nID ] = o

RETURN NIL

//-----------------------------------------//


METHOD OnMouseMove( nRow, nCol ) CLASS C_Object

   IF hb_isBlock( ::bOnMouseMove )
      Eval( ::bOnMouseMove, nRow, nCol )
   ENDIF
   

RETURN NIL

//-----------------------------------------//