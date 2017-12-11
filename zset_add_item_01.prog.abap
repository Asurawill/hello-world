*----------------------------------------------------------------------*
***INCLUDE ZSET_ADD_ITEM_01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZSET_ADD_ITEM_01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSET_ADD_ITEM_01 INPUT.

*****屏体尺寸计算逻辑*****
*DATA ZSD10     TYPE CHAR30.
  DATA L_ZSD0107 TYPE VBAP-ZSD0107.
  DATA L_ZSD0109 TYPE VBAP-ZSD0109.

  CLEAR L_ZSD0107.
  CLEAR L_ZSD0109.

  IF VBAP-ZSD0101 IS NOT INITIAL.
    VBAP-ZSD0103 = VBAP-ZSD0105 * 1000 / VBAP-ZSD0101.
  ENDIF.

  IF  VBAP-ZSD0102 IS NOT INITIAL.
    VBAP-ZSD0104 = VBAP-ZSD0106 * 1000 / VBAP-ZSD0102.
  ENDIF.

*四舍五入处理 BY HANDWY 2015-03-29
  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 3
      INPUT         = ( VBAP-ZSD0105 * VBAP-ZSD0106 ) / 1000
      SIGN          = '+'
    IMPORTING
      OUTPUT        = L_ZSD0107
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

    CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 3
      INPUT         = ( VBAP-ZSD0108 * VBAP-ZSD0105 * VBAP-ZSD0106 ) / 1000000
      SIGN          = '+'
    IMPORTING
      OUTPUT        = L_ZSD0109
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  VBAP-ZSD0107 = L_ZSD0107 / 1000.
  VBAP-ZSD0109 = L_ZSD0109 / 1000.


  IF VBAP-ZSD0110  IS NOT INITIAL.
    SELECT SINGLE ZSD002
         FROM ZSD001
         INTO ZSD10
         WHERE ZSD001 = VBAP-ZSD0110.
  ENDIF.
*****屏体尺寸计算逻辑*******


  BREAK HANDLJ.
  IF VBAP-PSTYV = 'Z01' OR VBAP-PSTYV = 'Z02'OR VBAP-PSTYV = 'Z31'OR VBAP-PSTYV = 'Z32' OR VBAP-PSTYV = 'Z41' OR VBAP-PSTYV = 'Z42'.
    VBAP-ZMENG = VBAP-ZSD0109.

*    MODIFY xvbap TRANSPORTING abgru updkz.
*    VBAP-KWMENG = VBAP-ZSD0109.
*   VBAP-KBMENG = VBAP-ZSD0109.
*   VBAP-KLMENG = VBAP-ZSD0109.
*    VBAP-ZMENG  = VBAP-ZSD0109.
  ENDIF.

ENDMODULE.
