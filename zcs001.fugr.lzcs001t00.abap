*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.02.27 at 14:07:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCS001..........................................*
DATA:  BEGIN OF STATUS_ZCS001                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCS001                        .
CONTROLS: TCTRL_ZCS001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCS001                        .
TABLES: ZCS001                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
