*&---------------------------------------------------------------------*
*&  包含                ZXQEVU10
*&---------------------------------------------------------------------*


IF SY-TCODE = 'QA13' .
  SUBMIT ZQEVAC40 WITH PRUEFLOS = I_QALS-PRUEFLOS AND RETURN.
  WAIT UP TO 1 SECONDS.
  SUBMIT RQEVAC50 WITH PRUEFLOS = I_QALS-PRUEFLOS AND RETURN.
else.
  MESSAGE '请使用TCODE:QA13，取消UD进行冲销。' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
