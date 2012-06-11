FUNCTION MsgAlert( cMsg )
   
   local cJSon
   local hJSon := hb_Hash()
   local nLen
   local oClient
   
   oClient = H5_GetClient()
   
   hJSon[ "ACTION" ] = "MSGALERT"
   hJSon[ "MSG" ]    = cMsg
   cJSon := hb_jsonEncode( hJSon, @nLen, .F. )
   
   oClient:SendData( cJSon )

RETURN NIL 