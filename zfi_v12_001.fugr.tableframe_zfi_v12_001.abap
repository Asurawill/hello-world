*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI_V12_001
*   generation date: 2015.01.30 at 11:42:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI_V12_001        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
