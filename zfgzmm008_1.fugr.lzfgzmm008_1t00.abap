*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 28.02.2015 at 13:33:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM002_1........................................*
DATA:  BEGIN OF STATUS_ZMM002_1                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM002_1                      .
CONTROLS: TCTRL_ZMM002_1
            TYPE TABLEVIEW USING SCREEN '1011'.
*.........table declarations:.................................*
TABLES: *ZMM002_1                      .
TABLES: ZMM002_1                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
