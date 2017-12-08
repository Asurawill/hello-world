*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.07.27 at 11:35:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPST001.........................................*
DATA:  BEGIN OF STATUS_ZPST001                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPST001                       .
CONTROLS: TCTRL_ZPST001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPST001                       .
TABLES: ZPST001                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
