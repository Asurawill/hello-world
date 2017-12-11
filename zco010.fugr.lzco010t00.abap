*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2016.04.05 at 12:29:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCO010_5_1......................................*
DATA:  BEGIN OF STATUS_ZCO010_5_1                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCO010_5_1                    .
CONTROLS: TCTRL_ZCO010_5_1
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCO010_5_1                    .
TABLES: ZCO010_5_1                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
