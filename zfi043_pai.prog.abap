*&---------------------------------------------------------------------*
*&  包含                ZFI043_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BUKRS_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUKRS_INPUT INPUT.

  IF GS_HEAD-BUKRS IS INITIAL .
    MESSAGE '公司代码必输' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ELSE.
    PERFORM FRM_AUTH_CHECK USING '10' GS_HEAD-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E010(ZFICO01) WITH GS_HEAD-BUKRS.
    ENDIF.
  ENDIF .

*  SELECT SINGLE LAND1
*    INTO G_LAND1
*    FROM T001
*    WHERE BUKRS = GS_HEAD-BUKRS.
*  IF G_LAND1 IS NOT INITIAL.
*    SELECT SINGLE KALSM
*      INTO G_KALSM
*      FROM T005
*      WHERE LAND1 = G_LAND1.
*  ENDIF.
*
*  G_CHANGED = ABAP_TRUE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  WAERS_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE WAERS_INPUT INPUT.
  IF GS_HEAD-WAERS IS INITIAL .
    MESSAGE '货币必输' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF.
*  IF GS_HEAD-WAERS IS NOT INITIAL.
*    LOOP AT GT_ITEM ASSIGNING <FS_ITEM>.
*      <FS_ITEM>-WAERS = GS_HEAD-WAERS.
*    ENDLOOP.
*  ENDIF.
*
*  G_CHANGED = ABAP_TRUE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PSPID_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PSPID_INPUT INPUT.

  IF GS_HEAD-PSPID IS INITIAL .
    MESSAGE '项目定义必输' TYPE 'W' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF .

  DATA L_MSG TYPE STRING .
  SELECT SINGLE *
    INTO GS_PROJ
    FROM PROJ
   WHERE PSPID = GS_HEAD-PSPID
     AND VBUKR = GS_HEAD-BUKRS .
  IF SY-SUBRC NE 0 .
    CONCATENATE GS_HEAD-BUKRS '公司代码下不存在' GS_HEAD-PSPID '项目' INTO L_MSG .
    MESSAGE L_MSG TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF.

*  IF GT_ITEM IS INITIAL.
*    " 初始化三行行项目
*    PERFORM FRM_ITEM_INIT.
*  ELSE.
*    LOOP AT GT_ITEM ASSIGNING <FS_ITEM>.
*      IF <FS_ITEM>-VBELN IS NOT INITIAL.
*        <FS_ITEM>-VBELN = GS_HEAD-VBELN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF .

*  G_CHANGED = ABAP_TRUE.
ENDMODULE.
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
    WHEN 'BACK'.
      PERFORM FRM_CHECK_CHANGED CHANGING L_SUBRC.
      IF L_SUBRC = 0.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXIT'.
      PERFORM FRM_CHECK_CHANGED CHANGING L_SUBRC.
      IF L_SUBRC = 0.
        LEAVE PROGRAM.
      ENDIF.
    WHEN 'POST'.
      PERFORM FRM_POST_ACCDOC.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_ITEM INPUT.
  IF GT_ITEM IS INITIAL .
    PERFORM FRM_ITEM_INIT.
  ELSE .

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BLDAT_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BLDAT_INPUT INPUT.
  IF GS_HEAD-BLDAT IS INITIAL .
    MESSAGE '发票日期必输' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUDAT_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUDAT_INPUT INPUT.
  IF GS_HEAD-BUDAT IS INITIAL .
    MESSAGE '过账日期必输' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BLART_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BLART_INPUT INPUT.
  IF GS_HEAD-BLART IS INITIAL .
    MESSAGE '凭证类型必输' TYPE 'W' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'OK'.
      IF G_STGRD IS INITIAL.
        MESSAGE '冲销原因必输！' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        GS_REVERSAL-REASON_REV = G_STGRD.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: GS_REVERSAL-REASON_REV,
             GS_REVERSAL-PSTNG_DATE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
