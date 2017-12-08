*&---------------------------------------------------------------------*
*&  包含                ZCO004_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  DATA: L_SUBRC TYPE SY-SUBRC.

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'BACK' OR 'RETURN'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM FRM_POST_ACCDOC.
      PERFORM FRM_REFRESH_ABOVE_ALV.
*切换ML
    WHEN 'C1'.
      SUBMIT ZCO004_1  VIA SELECTION-SCREEN
      WITH P_BUKRS = P_BUKRS
      WITH P_GJAHR = P_GJAHR
      WITH P_MONAT = P_MONAT
      AND RETURN.
*切换WL
    WHEN 'C2'.
      SUBMIT ZCO004_2  VIA SELECTION-SCREEN
        WITH S_BUKRS = P_BUKRS
        WITH S_GJAHR = P_GJAHR
        WITH S_MONAT = P_MONAT
        AND RETURN.
*    WHEN 'REVERSAL'.
*      PERFORM FRM_REVERSAL_ACCDOC.
  ENDCASE.
ENDMODULE.
