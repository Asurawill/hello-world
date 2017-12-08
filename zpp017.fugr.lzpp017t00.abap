*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.04.2015 at 12:35:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPP017..........................................*
DATA:  BEGIN OF STATUS_ZPP017                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPP017                        .
CONTROLS: TCTRL_ZPP017
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPP017                        .
TABLES: ZPP017                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
