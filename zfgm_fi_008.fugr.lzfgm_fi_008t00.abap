*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2017.11.09 at 14:33:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFIV008_1.......................................*
TABLES: ZFIV008_1, *ZFIV008_1. "view work areas
CONTROLS: TCTRL_ZFIV008_1
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIV008_1. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIV008_1.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIV008_1_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIV008_1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIV008_1_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIV008_1_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIV008_1.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIV008_1_TOTAL.

*.........table declarations:.................................*
TABLES: SKA1                           .
TABLES: SKAT                           .
TABLES: T001                           .
TABLES: TVAK                           .
TABLES: TVAKT                          .
TABLES: TVKO                           .
TABLES: TVKOT                          .
TABLES: ZFI008_1                       .
