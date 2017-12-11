*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.02.27 at 15:57:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCS102..........................................*
DATA:  BEGIN OF STATUS_ZCS102                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCS102                        .
CONTROLS: TCTRL_ZCS102
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCS102                        .
TABLES: ZCS102                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
