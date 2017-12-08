*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.03.21 at 16:55:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSD008..........................................*
DATA:  BEGIN OF STATUS_ZSD008                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD008                        .
CONTROLS: TCTRL_ZSD008
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD008                        .
TABLES: ZSD008                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
