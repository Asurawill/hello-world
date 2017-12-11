*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.09.2015 at 11:09:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPSPROD.........................................*
DATA:  BEGIN OF STATUS_ZPSPROD                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPSPROD                       .
CONTROLS: TCTRL_ZPSPROD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPSPROD                       .
TABLES: ZPSPROD                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
