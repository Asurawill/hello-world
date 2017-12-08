*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.04.2015 at 20:50:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM007_1........................................*
DATA:  BEGIN OF STATUS_ZMM007_1                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM007_1                      .
CONTROLS: TCTRL_ZMM007_1
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZMM007_2........................................*
DATA:  BEGIN OF STATUS_ZMM007_2                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM007_2                      .
CONTROLS: TCTRL_ZMM007_2
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZMM007_3........................................*
DATA:  BEGIN OF STATUS_ZMM007_3                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM007_3                      .
CONTROLS: TCTRL_ZMM007_3
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM007_1                      .
TABLES: *ZMM007_2                      .
TABLES: *ZMM007_3                      .
TABLES: ZMM007_1                       .
TABLES: ZMM007_2                       .
TABLES: ZMM007_3                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
