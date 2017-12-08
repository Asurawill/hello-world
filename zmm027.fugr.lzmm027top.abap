* regenerated at 2015.11.02 16:13:43
FUNCTION-POOL ZMM027                     MESSAGE-ID SV.

* INCLUDE LZMM027D...                        " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZMM027T00                              . "view rel. data dcl.
*&---------------------------------------------------------------------*
*&      Module  CHANGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change INPUT.
TABLES:MAKT,MARA.

CLEAR ZMM027-MAKTX.
SELECT MAKTX
  INTO ZMM027-MAKTX
  FROM MAKT
  WHERE MATNR = ZMM027-MATNR
  AND SPRAS = SY-LANGU.
ENDSELECT.
    CLEAR ZMM027-XMMS.
    DATA T_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
    CLEAR T_TLINE[].
    DATA:TNAME TYPE THEAD-TDNAME.
    CLEAR TNAME.
    TNAME = ZMM027-MAT_KDAUF.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'Z001'
        LANGUAGE                = SY-LANGU
        NAME                    = TNAME
        OBJECT                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        LINES                   = T_TLINE[]
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
    ENDIF.
    IF T_TLINE[] IS NOT INITIAL.
      LOOP AT T_TLINE.
        CONCATENATE ZMM027-XMMS T_TLINE-TDLINE INTO ZMM027-XMMS.
        CLEAR T_TLINE.
      ENDLOOP.
    ENDIF.

 CLEAR ZMM027-MEINS.
  SELECT MEINS
    INTO ZMM027-MEINS
    FROM MARA
    WHERE MATNR = ZMM027-MATNR.

  ENDSELECT.
ENDMODULE.
