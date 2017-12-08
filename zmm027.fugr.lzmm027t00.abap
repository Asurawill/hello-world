*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.11.02 at 16:13:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM027..........................................*
DATA:  BEGIN OF STATUS_ZMM027                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM027                        .
CONTROLS: TCTRL_ZMM027
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZMM027                        .
TABLES: ZMM027                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
