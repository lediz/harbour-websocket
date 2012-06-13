/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#ifndef _C_MENU_CH
#define _C_MENU_CH

//----------------------------------------------------------------------------//

#xcommand MENU => H5_BeginMenu()
          
#xcommand MENU <oMenu> ;
       => ;
          <oMenu> := H5_BeginMenu( .T. )          

#xcommand MENUITEM [ <oMenuItem> PROMPT ] <cPrompt>;          
                   [ <of: OF, SUBMENU> <oSubMenu> ] ;
                   [ ACTION <uAction> ] ;
       => ;
          [<oMenuItem> := ] H5_AddMenuItem( [<oSubMenu>], <cPrompt>, [ { | oSender | <uAction> } ] )
      

#xcommand ENDMENU => H5_EndMenu()      

#xcommand DEFINE  MAINAPP [ <oObj> ] ;
             [ PORT <cPort> ] ;
             [ <cMode: TOP, LEFT, BOTTOM, RIGHT> ] ;//             [ <cMode:INSTALL,UNINSTALL,START,STOP,NONE> ];
       => ;
          [ <oObj> := ] WebSocketServer():New( [<cPort>], Upper( <cMode> ) )

#endif
