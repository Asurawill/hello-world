*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.08.03 at 20:03:07
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPST002.........................................*
DATA:  BEGIN OF STATUS_ZPST002                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPST002                       .
CONTROLS: TCTRL_ZPST002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPST002                       .
TABLES: ZPST002                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
