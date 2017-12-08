*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI_V12_003
*   generation date: 2015.03.16 at 12:52:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI_V12_003        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
