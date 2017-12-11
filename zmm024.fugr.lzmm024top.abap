* regenerated at 16.09.2015 14:40:35
FUNCTION-POOL ZMM024                     MESSAGE-ID SV.

* INCLUDE LZMM024D...                        " Local class definition
INCLUDE LSVIMDAT                                . "general data decl.
INCLUDE LZMM024T00                              . "view rel. data dcl.
*&---------------------------------------------------------------------*
*&      Module  ZMATNRTXT_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZMATNRTXT_GET INPUT.
  SELECT SINGLE MAKTX FROM MAKT INTO ZMM024-MAKTX
   WHERE MATNR = ZMM024-MATNR
   AND SPRAS = SY-LANGU.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZPSPIDTET_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZPSPIDTET_GET INPUT.
  SELECT SINGLE POST1 FROM PROJ INTO ZMM024-POST1
  WHERE  PSPID = ZMM024-POSID.
ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  CREAT_DATE  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE CREAT_DATE INPUT.
**保存日志记录
*  IF STATUS = 'EALX' OR  STATUS = 'ECLX'
*  OR STATUS = 'EULG'.
*    IF ZPP017-ZNAME IS INITIAL.
*      ZPP017-ZNAME  = SY-UNAME.
*      ZPP017-ZDATE  = SY-DATUM.
*      ZPP017-ZTIME  = SY-UZEIT.
*    ELSE.
*      ZPP017-ZNAME_C = SY-UNAME.
*      ZPP017-ZDATE_C = SY-DATUM.
*      ZPP017-ZTIME_C = SY-UZEIT.
*    ENDIF.
*
*  ENDIF.
*ENDMODULE.
