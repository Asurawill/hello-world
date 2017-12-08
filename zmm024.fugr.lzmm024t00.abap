*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.09.2015 at 14:44:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM024..........................................*
DATA:  BEGIN OF STATUS_ZMM024                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM024                        .
CONTROLS: TCTRL_ZMM024
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM024                        .
TABLES: ZMM024                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
