*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.09.2015 at 13:33:37
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM009..........................................*
DATA:  BEGIN OF STATUS_ZMM009                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM009                        .
CONTROLS: TCTRL_ZMM009
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM009                        .
TABLES: ZMM009                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
