*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.02.27 at 15:57:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCS101..........................................*
DATA:  BEGIN OF STATUS_ZCS101                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCS101                        .
CONTROLS: TCTRL_ZCS101
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCS101                        .
TABLES: ZCS101                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
