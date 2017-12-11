REPORT ZFI999.
*&---------------------------------------------------------------------*
*& Report  ZMM010
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/11/05
*& Request       :
*& Descriptions  :
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
DATA GT_BSEG TYPE TABLE OF BSEG.
DATA GS_BSEG TYPE BSEG.
DATA L_PROJK TYPE STRING.


REFRESH GT_BSEG.

SELECT * FROM BSEG
INTO CORRESPONDING FIELDS OF TABLE GT_BSEG
WHERE BUKRS = '1800'
AND ( HKONT LIKE '1121%'
OR    HKONT LIKE '1122%'
OR    HKONT LIKE '1123%'
OR    HKONT LIKE '2201%'
OR    HKONT LIKE '2202%'
OR    HKONT LIKE '2203%'
OR    HKONT LIKE '2241%')
AND   PROJK = '00000000'
AND   ZUONR <> ''.

LOOP AT GT_BSEG INTO GS_BSEG.
  L_PROJK = GS_BSEG-ZUONR.

*转换WBS元素内码
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
    EXPORTING
      INPUT     = L_PROJK
    IMPORTING
      OUTPUT    = GS_BSEG-PROJK
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.

  MODIFY GT_BSEG FROM GS_BSEG.
  CLEAR GS_BSEG.
ENDLOOP.

IF GT_BSEG IS NOT INITIAL .
  MODIFY BSEG FROM TABLE GT_BSEG.
ENDIF.
