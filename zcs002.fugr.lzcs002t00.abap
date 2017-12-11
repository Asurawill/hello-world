*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.02.27 at 14:08:42
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCS002..........................................*
DATA:  BEGIN OF STATUS_ZCS002                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCS002                        .
CONTROLS: TCTRL_ZCS002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCS002                        .
TABLES: ZCS002                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
