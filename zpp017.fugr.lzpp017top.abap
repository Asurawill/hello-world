* regenerated at 30.03.2015 11:52:05
FUNCTION-POOL ZPP017                     MESSAGE-ID SV.

* INCLUDE LZPP017D...                        " Local class definition
INCLUDE LSVIMDAT                                . "general data decl.
INCLUDE LZPP017T00                              . "view rel. data dcl.
*&---------------------------------------------------------------------*
*&      Module  ZMATNRTXT_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZMATNRTXT_GET INPUT.
  SELECT SINGLE MAKTX FROM MAKT INTO ZPP017-MAKTX
   WHERE MATNR = ZPP017-MATNR
   AND SPRAS = SY-LANGU.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZMATNRTXT_GET_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZMATNRTXT_GET_1 INPUT.
  SELECT SINGLE MAKTX FROM MAKT INTO ZPP017-MAKTX_B
   WHERE MATNR = ZPP017-MATNR_B
   AND SPRAS = SY-LANGU.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREAT_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREAT_DATE INPUT.
*保存日志记录
  IF STATUS = 'EALX' OR  STATUS = 'ECLX'
  OR STATUS = 'EULG'.
    IF ZPP017-ZNAME IS INITIAL.
      ZPP017-ZNAME  = SY-UNAME.
      ZPP017-ZDATE  = SY-DATUM.
      ZPP017-ZTIME  = SY-UZEIT.
    ELSE.
      ZPP017-ZNAME_C = SY-UNAME.
      ZPP017-ZDATE_C = SY-DATUM.
      ZPP017-ZTIME_C = SY-UZEIT.
    ENDIF.

  ENDIF.
ENDMODULE.
