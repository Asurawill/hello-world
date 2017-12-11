*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.04.21 at 10:23:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPSCOST.........................................*
DATA:  BEGIN OF STATUS_ZPSCOST                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPSCOST                       .
CONTROLS: TCTRL_ZPSCOST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPSCOST                       .
TABLES: ZPSCOST                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
