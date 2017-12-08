*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 28.01.2015 at 09:50:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSD001..........................................*
DATA:  BEGIN OF STATUS_ZSD001                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD001                        .
CONTROLS: TCTRL_ZSD001
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSD001                        .
TABLES: ZSD001                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
