*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.02.27 at 14:09:38
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCS003..........................................*
DATA:  BEGIN OF STATUS_ZCS003                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCS003                        .
CONTROLS: TCTRL_ZCS003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCS003                        .
TABLES: ZCS003                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
