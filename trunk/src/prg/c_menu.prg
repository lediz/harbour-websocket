#include "h5.ch"

static hMainMenu := {=>}

CLASS C_Menu FROM C_Object

   CLASSDATA nID INIT 0
   
   DATA cPrompt
   
   DATA hItems
   DATA hJSon
   DATA ptrItems
   
   DATA lCompleted

   METHOD New()
   METHOD End() 
   
   METHOD AddMenu()
   METHOD AddMenuItem()
   
   METHOD HandleEvent()
   

ENDCLASS

//--------------------------------------------------------------//

METHOD New() CLASS C_Menu
   
   ::GetId( "MNU" )
   ::hItems            = hb_Hash()
   ::lCompleted        = .F.
   ::hJSon             = hb_Hash()
   ::hJSon[ "items" ]  = {}
   ::hJSon[ "ID" ]     = ::cID
   ::hJSon[ "ACTION" ] = "CREATEMENU"
   ::ptrItems          = hb_Hash()
   
RETURN Self

//--------------------------------------------------------------//

METHOD End() CLASS C_Menu
   
   hb_HDel( hMainMenu, ::cID )

RETURN NIL

//--------------------------------------------------------------//

METHOD AddMenu( cPrompt ) CLASS C_Menu

   local oMenu
   
   oMenu              = C_Menu():New()
   oMenu:cPrompt      = cPrompt

RETURN oMenu

//--------------------------------------------------------------//

METHOD AddMenuItem( cPrompt, bAction ) CLASS C_Menu

   local oItem
   local aRow 
   
   IF ! ::lCompleted
      oItem = C_MenuItem():New( Self, cPrompt, bAction )
      
      aRow = { "text" => oItem:cPrompt, "id" => oItem:cID }
      ::hItems[ oItem:nID ] = oItem
      
      AAdd( ::hJSon[ "items" ], aRow ) 
      ::ptrItems[ oItem:cID ] = oItem 
      
   ENDIF

RETURN oItem

//--------------------------------------------------------------//

METHOD HandleEvent( nEvent, wParam, lParam ) 

   local oItem
   
   SWITCH nEvent
      CASE 0x2 //CLICK ON items
         oItem = ::ptrItems[ wParam ]
         IF hb_IsBlock( oItem:bAction )
            Eval( oItem:bAction, oItem )
         ENDIF
         EXIT 
   ENDSWITCH


RETURN NIL
//--------------------------------------------------------------//

CLASS C_MenuItem

   CLASSDATA nID INIT 0

   DATA bAction
   
   DATA cID 
   
   DATA cPrompt
   
   DATA hItems
   DATA hJSon
   
   DATA lChecked
   DATA lEnable
   DATA lSelected
   DATA lCompleted

   DATA oParent
   DATA oRoot
      
   METHOD New()
   
   METHOD AddMenuItem()
   
   METHOD GetId()      INLINE ::cID := "ITM" + AllTrim( StrZero( ++::nID, H5_MAX_ID ) )
   
   METHOD isParent()   INLINE ValType( ::hItems ) == "H"
   
   METHOD MakeParent() 
   

ENDCLASS

//--------------------------------------------------------------//

METHOD New( oMenu, cPrompt, bAction, lChecked, lEnable, lSelected ) CLASS C_MenuItem

   DEFAULT lChecked  TO .F.
   DEFAULT lEnable   TO .T.
   DEFAULT lSelected TO .F.
   
   ::lChecked  = lChecked 
   ::lEnable   = lEnable  
   ::lSelected = lSelected   
   ::cPrompt   = cPrompt
   ::oParent   = oMenu
   ::bAction   = bAction
   ::GetId()
   ::lCompleted = .F.
   ::oRoot      = H5_GetLastMenu()
   
RETURN Self

//--------------------------------------------------------------//

METHOD AddMenuItem( cPrompt, bAction ) CLASS C_MenuItem

   local oItem
   local aRow 
   local nLen
   
   IF ! ::lCompleted
   
      oItem = C_MenuItem():New( Self, cPrompt, bAction )
         
      ::hItems[ oItem:cID ] = oItem
      aRow = { "text" => oItem:cPrompt, "id" => oItem:cID }
      AAdd( ::hJSon[ "submenu" ][ "itemdata" ], aRow )
      ::oRoot:ptrItems[ oItem:cID ] = oItem 
            
   ENDIF

RETURN oItem

//--------------------------------------------------------------//

METHOD MakeParent() CLASS C_MenuItem
   local aRow 
   local hLast
    
   ::hItems = hb_Hash()
   
   IF ::oParent:isKindOf( "C_MENU" ) //IS ROOT
      ::hJSon = ATail( ::oRoot:hJSon[ "items" ] )
   ELSE 
      ::hJSon = ATail( ::oParent:hJSon[ "submenu" ][ "itemdata" ] )   
   ENDIF

   ::hJSon[ "submenu" ] = {  "id" => "SUB" + ::cID, "itemdata" => { } }  
   
RETURN NIL

//--------------------------------------------------------------//

FUNCTION H5_EndMenu()

   local oMenu
   local oLastItem
   
   oMenu     = H5_GetLastMenu()
   oLastItem = H5_GetLastParent( oMenu )
   
   oLastItem:lCompleted = .T.

RETURN NIL

//--------------------------------------------------------------//

FUNCTION H5_AddMenuItem( oParent, cPrompt, bAction ) 
   local oMenu
   local oLastItem, oMenuItem
   
   oMenu     = H5_GetLastMenu()
   oLastItem = H5_GetLastParent( oMenu )

   DEFAULT oParent TO oLastItem
   
   oMenuItem = oParent:AddMenuItem( cPrompt, bAction )

RETURN oMenuItem

//--------------------------------------------------------------//

FUNCTION  H5_BeginMenu( lRoot )
   local oMenu, oLastItem
   local nLenItems
   
   DEFAULT lRoot TO .F.
   
   oMenu     = H5_GetLastMenu()
   
   IF oMenu != NIL .AND. ! lRoot
   
      oLastItem = H5_GetLastItem( oMenu )   

      IF oLastItem != NIL
         oLastItem:MakeParent()
      ENDIF
            
   ELSE   
   
      oMenu = C_Menu():New( .T. /*IS PARENT*/ )
      hMainMenu[ oMenu:cID ] = oMenu
      
   ENDIF
   
RETURN oMenu

//--------------------------------------------------------------//

STATIC FUNCTION H5_GetLastParent( oMenu )
   local oItem 
   local nLenItems
   
   IF ValType( oMenu:hItems ) == "H" .AND. ! oMenu:lCompleted
      nLenItems = Len( oMenu:hItems )
      //? oMenu:cPrompt, oMenu:nID, nLenItems
      IF nLenItems > 0
         oItem = hb_HValueAT( oMenu:hItems, Len( oMenu:hItems ) )
         oItem = H5_GetLastParent( oItem )
      ELSE 
         oItem = oMenu
      ENDIF
   ELSE       
      oItem = oMenu:oParent
   ENDIF
   
RETURN oItem 

//--------------------------------------------------------------//

STATIC FUNCTION H5_GetLastItem( oMenu )
   local oItem 
   local nLenItems
   
   IF ValType( oMenu:hItems ) == "H" 
      nLenItems = Len( oMenu:hItems )
      IF nLenItems > 0
         oItem = hb_HValueAT( oMenu:hItems, Len( oMenu:hItems ) )
         oItem = H5_GetLasTItem( oItem )
      ELSE 
         oItem = oMenu
      ENDIF
   ELSE       
      oItem = oMenu
   ENDIF
   
RETURN oItem 

//--------------------------------------------------------------//

STATIC FUNCTION H5_GetLastMenu()

   local oMenu
   local nLen := Len( hMainMenu )
   
   if nLen > 0
      oMenu     = hb_HValueAt( hMainMenu, nLen )
   ENDIF

RETURN oMenu

//--------------------------------------------------------------//


FUNCTION H5_GetMainMenu()
RETURN hMainMenu