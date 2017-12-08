*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.01.2015 at 16:19:23
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM001..........................................*
DATA:  BEGIN OF STATUS_ZMM001                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM001                        .
CONTROLS: TCTRL_ZMM001
            TYPE TABLEVIEW USING SCREEN '1011'.
*.........table declarations:.................................*
TABLES: *ZMM001                        .
TABLES: ZMM001                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
