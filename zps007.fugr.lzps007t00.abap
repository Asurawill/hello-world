*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2015.10.29 at 17:29:39
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPS007A.........................................*
DATA:  BEGIN OF STATUS_ZPS007A                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPS007A                       .
CONTROLS: TCTRL_ZPS007A
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZPS007A                       .
TABLES: ZPS007A                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
