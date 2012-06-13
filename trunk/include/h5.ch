/*
(c)2012 Daniel Garcia-Gil <danielgarciagil@gmail.com>
*/
#ifndef _H5_CH
#define _H5_CH

#ifdef __PLATFORM__WINDOWS
#include "hbwin.ch"
#endif

#include "hbclass.ch"
#include "common.ch"
#include "c_menu.ch"


#define CRLF chr(13)+chr(10)
#define H5_MAX_ID 8


#xcommand DEFINE MAINAPP [ <oObj> ] ;
             [ PORT <cPort> ];
             [ MODE <cMode> ];
      => ;
         [ <oObj> := ]WebSocketServer():New( [<cPort>], [ Upper( <cMode> ) ] )

#xcommand ACTIVATE MAINAPP <oObj> ;
      => ;
         <oObj>:Activate()

#endif
