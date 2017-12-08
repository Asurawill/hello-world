*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.08.25 at 18:22:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMMT001.........................................*
DATA:  BEGIN OF STATUS_ZMMT001                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT001                       .
CONTROLS: TCTRL_ZMMT001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT001                       .
TABLES: ZMMT001                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
