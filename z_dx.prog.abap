*&---------------------------------------------------------------------*
*& Report  Z_DX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_DX.
DATA : TEXT TYPE STRING,
      TIME TYPE I.
TIME = 0.
CONCATENATE 'Please open another session for working' '!' INTO TEXT.
DO.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = TIME
      TEXT       = TEXT
    EXCEPTIONS
      OTHERS     = 1.
  TIME = TIME + 1.
*  IF TIME = 101.
*    TIME = 0.
*  ENDIF.
  TIME = TIME MOD 100.
  WAIT UP TO 10 SECONDS.
ENDDO.
