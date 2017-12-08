*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.02.2015 at 14:04:35
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSD002..........................................*
DATA:  BEGIN OF STATUS_ZSD002                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD002                        .
CONTROLS: TCTRL_ZSD002
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSD002                        .
TABLES: ZSD002                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
