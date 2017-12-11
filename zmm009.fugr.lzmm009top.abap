* regenerated at 29.09.2015 13:33:37
FUNCTION-POOL ZMM009                     MESSAGE-ID SV.

* INCLUDE LZMM009D...                        " Local class definition
INCLUDE LSVIMDAT                                . "general data decl.
INCLUDE LZMM009T00                              . "view rel. data dcl.
*&---------------------------------------------------------------------*
*&      Module  ZMATNRTXT_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZMATNRTXT_GET INPUT.
  SELECT SINGLE MAKTX FROM MAKT INTO ZMM009-MAKTX
   WHERE MATNR = ZMM009-MATNR
   AND SPRAS = SY-LANGU.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZPSPIDTET_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZPSPIDTET_GET INPUT.
  SELECT SINGLE POST1 FROM PROJ INTO ZMM009-POST1
  WHERE  PSPID = ZMM009-PSPHI.
ENDMODULE.
