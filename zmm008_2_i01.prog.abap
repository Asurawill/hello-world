*----------------------------------------------------------------------*
***INCLUDE ZMM008_2_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  DATA L_MESSAGE TYPE STRING.
  DATA L_TABIX TYPE SY-TABIX.

  CLEAR L_TABIX.

  IF SY-UCOMM = '&CANCLE'.
    LEAVE TO SCREEN 0.
  ELSEIF SY-UCOMM = '&CONTINUE'.
    IF UP_MODE = 'I'.
      CLEAR: MAX_DBDH,IT_ZMM002_2DB[].
      SELECT SINGLE MAX( DBDH ) FROM ZMM002_2
        INTO MAX_DBDH.
      L_MESSAGE = MAX_DBDH + 1.
      LOOP AT IT_ZMM002_2 ASSIGNING <WA_ZMM002_2>.

        L_TABIX = SY-TABIX.

        IF <WA_ZMM002_2>-MATNR = ''.
          CONTINUE.
        ENDIF.
        <WA_ZMM002_2>-DBDH = MAX_DBDH + 1.
        IF <WA_ZMM002_2>-TIMESTAMP = ''.
          PERFORM CONVERT_DATUM_TO_TIMESTAMPS CHANGING <WA_ZMM002_2>-TIMESTAMP.
          <WA_ZMM002_2>-TIMESTAMP = <WA_ZMM002_2>-TIMESTAMP + L_TABIX.
        ENDIF.
        MOVE-CORRESPONDING <WA_ZMM002_2> TO WA_ZMM002_2DB.
        APPEND WA_ZMM002_2DB TO IT_ZMM002_2DB.
      ENDLOOP.
      IF SY-SUBRC = 0.
        MODIFY ZMM002_2 FROM TABLE IT_ZMM002_2DB[].
        IF SY-SUBRC = 0.
          COMMIT WORK.
          CLEAR S_DBDH[].
          S_DBDH-SIGN = 'I'.
          S_DBDH-OPTION = 'EQ'.
          S_DBDH-LOW = MAX_DBDH + 1.
          APPEND S_DBDH.
          CONCATENATE '更新成功,生成调拨单号：' L_MESSAGE INTO L_MESSAGE.
          MESSAGE S000(SV) WITH L_MESSAGE.
          PERFORM FRM_PRINT_DATA TABLES IT_ZMM002_2DB[].
          CLEAR IT_ZMM002_2DB[].
          LEAVE TO SCREEN 0.
        ELSE.
          ROLLBACK WORK.
          MESSAGE S000(SV) WITH '更新失败' DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ELSEIF UP_MODE = 'U'.
      CLEAR IT_ZMM002_2DB[].
      DELETE FROM ZMM002_2 WHERE DBDH = S_DBDH-LOW.
      LOOP AT IT_ZMM002_2 ASSIGNING <WA_ZMM002_2>.

        L_TABIX = SY-TABIX.

        IF <WA_ZMM002_2>-MATNR = ''.
          CONTINUE.
        ENDIF.
        IF <WA_ZMM002_2>-DBDH = ''.        "对于新加行 dbdh处理
          READ TABLE S_DBDH INDEX 1.
          <WA_ZMM002_2>-DBDH = S_DBDH-LOW.
        ENDIF.
        IF <WA_ZMM002_2>-TIMESTAMP = ''.   "对于新加行 timestamp处理
          PERFORM CONVERT_DATUM_TO_TIMESTAMPS CHANGING <WA_ZMM002_2>-TIMESTAMP.
          <WA_ZMM002_2>-TIMESTAMP = <WA_ZMM002_2>-TIMESTAMP + L_TABIX.
        ENDIF.
        MOVE-CORRESPONDING <WA_ZMM002_2> TO WA_ZMM002_2DB.
        APPEND WA_ZMM002_2DB TO IT_ZMM002_2DB.
      ENDLOOP.
      IF SY-SUBRC = 0.
        IF SY-SUBRC = 0.
          COMMIT WORK AND WAIT.
          MODIFY ZMM002_2 FROM TABLE IT_ZMM002_2DB[].
          IF SY-SUBRC = 0.
            COMMIT WORK.
            MESSAGE S000(SV) WITH '更新成功'.
          ELSE.
            ROLLBACK WORK.
            MESSAGE S000(SV) WITH '更新失败'.
          ENDIF.
        ELSE.
          ROLLBACK WORK.
          MESSAGE S000(SV) WITH '更新失败'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF SY-UCOMM = '&PRT'.
    CLEAR IT_ZMM002_2DB[].
    LOOP AT IT_ZMM002_2 ASSIGNING <WA_ZMM002_2>.
      IF <WA_ZMM002_2>-MATNR = ''.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING <WA_ZMM002_2> TO WA_ZMM002_2DB.
      APPEND WA_ZMM002_2DB TO IT_ZMM002_2DB.
    ENDLOOP.
    IF IT_ZMM002_2DB[] IS NOT INITIAL.
      PERFORM FRM_PRINT_DATA TABLES IT_ZMM002_2DB[].
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UCOMM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE UCOMM_EXIT INPUT.
  IF SY-UCOMM = '&CANCLE'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
