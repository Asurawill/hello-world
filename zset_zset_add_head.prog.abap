*----------------------------------------------------------------------*
***INCLUDE ZSET_ZSET_ADD_HEAD.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZSET_ADD_HEAD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSET_ADD_HEAD OUTPUT.

LOOP AT SCREEN.
  IF SY-tCODE = 'VA03' OR SY-tCODE = 'VA23' OR SY-tCODE = 'VA13'.
    IF SCREEN-NAME = 'VBAK-COUNTRY' OR SCREEN-NAME = 'VBAK-REGION' OR SCREEN-NAME = 'VBAK-ZSD0303' OR SCREEN-NAME = 'VBAK-ZSD0301'
      OR SCREEN-NAME = 'VBAK-ZSD0302'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN .
    ENDIF.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'Z_SD_PRICE' ID 'ZPRICE' FIELD 'X'.
  IF sy-subrc <> 0."不等于0就表示没有权限，下面会屏蔽金额
    IF SCREEN-NAME = 'VBAK-COUNTRY' OR SCREEN-NAME = 'VBAK-REGION' OR SCREEN-NAME = 'VBAK-ZSD0303' OR SCREEN-NAME = 'VBAK-ZSD0301'
      OR SCREEN-NAME = 'VBAK-ZSD0302'.
        SCREEN-INPUT = 0.
      MODIFY SCREEN .
    ENDIF.
  ENDIF.
ENDLOOP.




ENDMODULE.
