*----------------------------------------------------------------------*
***INCLUDE ZXCN1I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  DATA LS_ZPMLOGINFO TYPE ZPMLOGINFO.

  CLEAR LS_ZPMLOGINFO.
  CLEAR FLAG.
  CLEAR FLAG1.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'RETURN'.
      LEAVE TO TRANSACTION 'ZCJ20N'.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '&OK'.
      BREAK HANDWY.

      IF L_ZPMLOGINFO-VERNR IS NOT INITIAL
      AND L_ZPMLOGINFO-LOGPWD IS NOT INITIAL.
        SELECT SINGLE * FROM ZPMLOGINFO
        INTO CORRESPONDING FIELDS OF LS_ZPMLOGINFO
        WHERE  VERNR = L_ZPMLOGINFO-VERNR.

        IF  ( LS_ZPMLOGINFO-LOGPWD = L_ZPMLOGINFO-LOGPWD
        AND  L_ZPMLOGINFO-LOGPWD IS NOT INITIAL
        AND  L_ZPMLOGINFO-VERNR = LS_PROJ-VERNR )
        OR   LS_ZPMLOGINFO-ZSFJC = 'X'
         .

          FLAG  = 'X'.
          FLAG1 = 'X'.
          LEAVE TO SCREEN 0.
        ELSE.
          MESSAGE '密码错误，请重新输入' TYPE 'I' .
        ENDIF.

      ELSE.
        MESSAGE '密码错误，请重新输入' TYPE 'I' .
      ENDIF.

    WHEN '&OUT'.
      LEAVE TO TRANSACTION 'ZCJ20N'.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.
