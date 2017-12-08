*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 24.08.2017 at 18:07:17
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZS_ZSD020......................................*
DATA:  BEGIN OF STATUS_ZZS_ZSD020                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZS_ZSD020                    .
CONTROLS: TCTRL_ZZS_ZSD020
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZZS_ZSD020                    .
TABLES: ZZS_ZSD020                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
