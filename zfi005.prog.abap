*&---------------------------------------------------------------------*
*& Report  ZFI005
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2015/1/18                                            *
*& Created By   : 汉得-唐博                                            *
*& Description  :销售回款认领平台                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZFI005.

TABLES: BKPF,ZFI005.
TYPE-POOLS: SLIS.

PARAMETERS P_BUKRS TYPE T001-BUKRS OBLIGATORY MEMORY ID BUK."公司代码
SELECT-OPTIONS: S_DATLR FOR BKPF-BUDAT, "款项录入日期
                S_DATGZ FOR BKPF-BUDAT."认领过账日期
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_YELLOW AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(30) ICONYEL FOR FIELD P_YELLOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_GREEN AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(30) ICONGRE FOR FIELD P_GREEN.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_RED AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(30) ICONRED FOR FIELD P_RED.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

DATA: OK_CODE   TYPE CHAR20,
      G_SUCCESS TYPE C,
      G_USER    TYPE USR02-BNAME,
      SAVE_OK   TYPE SY-UCOMM.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA GRID1  TYPE REF TO CL_GUI_ALV_GRID.
DATA G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA BEGIN OF GT_ZFI005 OCCURS 0 .
        INCLUDE STRUCTURE ZFI005 .
DATA CELLSTYLE TYPE LVC_T_STYL.
DATA END OF GT_ZFI005.

DATA LS_STYLEROW TYPE LVC_S_STYL.
DATA:
*      GT_ZFI005 TYPE TABLE OF ZFI005,
  ZFI005_OLD     LIKE TABLE OF ZFI005 WITH HEADER LINE,
  ZFI005_NEW     LIKE TABLE OF ZFI005 WITH HEADER LINE,
  ZFI005_DEL     LIKE TABLE OF ZFI005 WITH HEADER LINE,
  ZFI005_ADD     LIKE TABLE OF ZFI005 WITH HEADER LINE,
  ZFI005_MOD     LIKE TABLE OF ZFI005 WITH HEADER LINE,
  GT_ZFI005_EDIT LIKE TABLE OF GT_ZFI005,
  GT_ZFI005_INS  LIKE TABLE OF GT_ZFI005,
  GW_ZFI005      LIKE GT_ZFI005,
  W_LAYOUT       TYPE        LVC_S_LAYO,
  W_VARIANT      TYPE        DISVARIANT, "变式
  T_UIFUNC       TYPE        UI_FUNCTIONS,
  T_FIELDCAT     TYPE        LVC_T_FCAT WITH HEADER LINE.
DATA GO_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
RANGES R_PRGS FOR ZFI005-STEP.


DATA: BEGIN OF GT_SKA1 OCCURS 1,
        SAKNR TYPE GL_ACCT_CA_TEXT-SAKNR,
        TXT50 TYPE GL_ACCT_CA_TEXT-TXT50,
      END OF GT_SKA1.
*&--代码添加 BY HANDYBY 12.06.2017 20:27:02  BEGIN
DATA: BEGIN OF GT_SKA2 OCCURS 1 ,
        MCODF TYPE GL_ACCT_CC_TEXT-MCODF,
        SPRAS TYPE GL_ACCT_CC_TEXT-SPRAS,
        BUKRS TYPE GL_ACCT_CC_TEXT-BUKRS,
        SAKNR TYPE GL_ACCT_CC_TEXT-SAKNR,
      END OF GT_SKA2 .
*&--代码添加 BY HANDYBY 12.06.2017 20:27:02  END

DATA: BEGIN OF GT_VBELN OCCURS 1,
        VBELN TYPE VBAK-VBELN,
      END OF GT_VBELN.

"设置稳定刷新
DATA G_IS_STABLE TYPE LVC_S_STBL.
DATA L_STEP TYPE CHAR30.

DATA W_VBAK TYPE VBAK.

DATA G_ANSWER     TYPE STRING. "控制弹出框
* BAPI_ACC_DOCUMENT_REV_POST
DATA: WA_REVERSAL TYPE BAPIACREV,
      WA_BUS      TYPE BAPIACHE09.
DATA: G_SUC.
DATA: WA_OBJ TYPE BAPIACHE09.

DATA: G_STGRD TYPE BKPF-STGRD.
"款项处理进度
PARAMETERS: X_STEP1 RADIOBUTTON GROUP G1, "出纳/应收录入款项及修改
            X_STEP2 RADIOBUTTON GROUP G1, "销售助理认领
            X_STEP3 RADIOBUTTON GROUP G1. "审核过账/冲销

AT SELECTION-SCREEN OUTPUT.

  WRITE ICON_RED_LIGHT AS ICON TO ICONRED.
  WRITE ICON_YELLOW_LIGHT AS ICON TO ICONYEL.
  WRITE ICON_GREEN_LIGHT AS ICON TO ICONGRE.
  CONCATENATE ICONYEL '未处理' INTO ICONYEL SEPARATED BY SPACE.
  CONCATENATE ICONGRE '已过帐' INTO ICONGRE SEPARATED BY SPACE.
  CONCATENATE ICONRED '已冲销' INTO ICONRED SEPARATED BY SPACE.

INITIALIZATION.
  CLEAR G_IS_STABLE.
  G_IS_STABLE-ROW = 'X'.
  G_IS_STABLE-COL = 'X'.

START-OF-SELECTION.
  CASE 'X'.
    WHEN X_STEP1.
      L_STEP = 'STEP1'.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD P_BUKRS.
      IF SY-SUBRC <> 0.
        MESSAGE S899(MM) WITH '您没有公司代码' P_BUKRS '的权限' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      AUTHORITY-CHECK OBJECT 'Z_SD_XSHK'
               ID 'ZSTEP' FIELD '1'.
      IF SY-SUBRC <> 0.
        MESSAGE '你没有操作出纳/应收录入款项及修改的权限。' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN X_STEP2.
      L_STEP = 'STEP2'.
      AUTHORITY-CHECK OBJECT 'Z_SD_XSHK'
               ID 'ZSTEP' FIELD '2'.
      IF SY-SUBRC <> 0.
        MESSAGE '你没有操作销售助理认领的权限。' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN X_STEP3.
      L_STEP = 'STEP3'.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD P_BUKRS.
      IF SY-SUBRC <> 0.
        MESSAGE S899(MM) WITH '您没有公司代码' P_BUKRS '的权限' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      AUTHORITY-CHECK OBJECT 'Z_SD_XSHK'
               ID 'ZSTEP' FIELD '3'.
      IF SY-SUBRC <> 0.
        MESSAGE '你没有操作审核过账/冲销的权限。' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  ENDCASE.

  CLEAR: GT_ZFI005[], GT_ZFI005_EDIT[], R_PRGS[].
  IF P_YELLOW IS INITIAL
    AND P_RED IS INITIAL
    AND P_GREEN IS INITIAL
    AND X_STEP1 IS INITIAL.
    MESSAGE S001(ZFICO01) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF P_YELLOW EQ 'X'.
    PERFORM APPEND_RANGE USING 'R_PRGS' 'I' 'EQ' ICON_YELLOW_LIGHT ''.
  ENDIF.
  IF P_RED EQ 'X'.
    PERFORM APPEND_RANGE USING 'R_PRGS' 'I' 'EQ' ICON_RED_LIGHT ''.
  ENDIF.
  IF P_GREEN EQ 'X'.
    PERFORM APPEND_RANGE USING 'R_PRGS' 'I' 'EQ' ICON_GREEN_LIGHT ''.
  ENDIF.
  IF R_PRGS[] IS NOT INITIAL.
    SELECT * FROM ZFI005
      INTO CORRESPONDING FIELDS OF TABLE GT_ZFI005
      WHERE BUKRS EQ P_BUKRS
      AND KXLRDAT IN S_DATLR  "款项录入日期
      AND BUDAT IN S_DATGZ  "认领过账日期
      AND STEP IN R_PRGS.
    IF X_STEP2 EQ 'X'.
      LOOP AT GT_ZFI005."权限控制（第二步）
        IF GT_ZFI005-VKORG IS NOT INITIAL.
          AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
                   ID 'VKORG' FIELD GT_ZFI005-VKORG.
          IF SY-SUBRC <> 0.
            DELETE GT_ZFI005.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    APPEND LINES OF GT_ZFI005[] TO GT_ZFI005_EDIT[].
  ENDIF.
  PERFORM LOCK_ZFI005 CHANGING G_SUCCESS G_USER.
  IF G_SUCCESS EQ 'X'.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.

CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ZFI005_KEYS.
    TYPES:   XSHKNO TYPE ZFI005-XSHKNO.
    TYPES:   BUKRS TYPE ZFI005-BUKRS.
    TYPES: END OF ZFI005_KEYS.

    METHODS:
      HANDLE_DOUBLE_CLICK
                    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW ES_ROW_NO E_COLUMN.

    METHODS:
      HANDLE_TOOLBAR
                    FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS:
      HANDLE_TOOLBAR2
                    FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS:
      HANDLE_USER_COMMAND
                    FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

    METHODS:
      HANDLE_DATA_CHANGED
                    FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                    E_ONF4
                    E_ONF4_BEFORE
                    E_ONF4_AFTER
                    E_UCOMM.
    METHODS:
      HANDLE_DATA_CHANGED_FINISHED
                    FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED
                    ET_GOOD_CELLS.

    METHODS:
      HANDLE_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.

ENDCLASS.
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_ONF4.
    DATA: LS_MODI    TYPE LVC_S_MODI,
          LT_RET_TAB TYPE TABLE OF DDSHRETVAL,
          LW_RET_TAB LIKE LINE OF LT_RET_TAB,
          T_ZSKA1_1  TYPE TABLE OF ZSKA1_1.
    FIELD-SYMBOLS <MODTAB> TYPE LVC_T_MODI.
    READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX ES_ROW_NO-ROW_ID.
    IF X_STEP1 EQ 'X' AND GW_ZFI005-STEP EQ ICON_YELLOW_LIGHT AND (
       (  E_FIELDNAME = 'XJKM' AND GW_ZFI005-KXZL = '现金' )
      OR ( E_FIELDNAME = 'YHKM' AND GW_ZFI005-KXZL = '银行存款' )
      OR  ( E_FIELDNAME = 'XYZKM' AND GW_ZFI005-KXZL = '信用证' )
      OR ( E_FIELDNAME = 'PJKM' AND  ( GW_ZFI005-KXZL = '银行承兑汇票' OR GW_ZFI005-KXZL = '商业承兑汇票' ) ) ).
      CHECK SY-SUBRC = 0.
      CLEAR GT_SKA1[].
*&--代码添加 BY HANDYBY 12.06.2017 20:33:19  BEGIN
      CLEAR GT_SKA2[].
*&--代码添加 BY HANDYBY 12.06.2017 20:33:19  END
      IF E_FIELDNAME = 'XJKM'.
        SELECT * FROM GL_ACCT_CA_TEXT INTO CORRESPONDING FIELDS OF TABLE GT_SKA1[]
          WHERE SPRAS EQ SY-LANGU
          AND KTOPL EQ '1000'
          AND SAKNR GE '1001010101'
          AND SAKNR LE '1001010601'.
      ELSEIF E_FIELDNAME = 'YHKM'.
*&--代码注释 BY HANDYBY 12.06.2017 20:34:21  BEGIN
*       SELECT * FROM GL_ACCT_CA_TEXT INTO CORRESPONDING FIELDS OF TABLE GT_SKA1[]
*          WHERE SPRAS EQ SY-LANGU
*          AND KTOPL EQ '1000'
*          AND SAKNR GE '1002010101'
*          AND SAKNR LE '1002999999'.
*&--代码注释 BY HANDYBY 12.06.2017 20:34:21  END
*&--代码添加 BY HANDYBY 12.06.2017 20:34:37  BEGIN
        SELECT * FROM GL_ACCT_CC_TEXT INTO CORRESPONDING FIELDS OF TABLE GT_SKA2[]
            WHERE SPRAS EQ SY-LANGU
            AND SAKNR GE '1002010101'
            AND SAKNR LE '1002999999'.
*&--代码添加 BY HANDYBY 12.06.2017 20:34:37  END

      ELSEIF E_FIELDNAME = 'XYZKM'.
        SELECT * FROM GL_ACCT_CA_TEXT INTO CORRESPONDING FIELDS OF TABLE GT_SKA1[]
          WHERE SPRAS EQ SY-LANGU
          AND KTOPL EQ '1000'
          AND SAKNR GE '1012010101'
          AND SAKNR LE '1012999999'.
      ELSE.
        SELECT * FROM GL_ACCT_CA_TEXT INTO CORRESPONDING FIELDS OF TABLE GT_SKA1[]
           WHERE SPRAS EQ SY-LANGU
           AND KTOPL EQ '1000'
           AND SAKNR GE '1121010101'
           AND SAKNR LE '1121020101'.
      ENDIF.
      CLEAR LT_RET_TAB[].
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'SAKNR'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = GT_SKA2[]
          RETURN_TAB      = LT_RET_TAB
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.
      IF SY-SUBRC = 0.
**  Update the value in ALV cell
        READ TABLE LT_RET_TAB INTO LW_RET_TAB INDEX 1.
        IF SY-SUBRC = 0. " USER DIDN'T CANCEL
          LS_MODI-ROW_ID = ES_ROW_NO-ROW_ID.
          LS_MODI-FIELDNAME = E_FIELDNAME.
          LS_MODI-VALUE = LW_RET_TAB-FIELDVAL.
          ASSIGN ER_EVENT_DATA->M_DATA->* TO <MODTAB>.
          APPEND LS_MODI TO <MODTAB>.
        ENDIF.
      ENDIF.
**  Inform ALV Grid that event 'onf4' has been processed
      ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
    ELSEIF E_FIELDNAME = 'XJKM'
          OR E_FIELDNAME = 'YHKM'
          OR E_FIELDNAME = 'XYZKM'
          OR E_FIELDNAME = 'PJKM' .
      .
      ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
    ENDIF.
    IF E_FIELDNAME = 'XMBH'."搜索帮助-订单号
      CLEAR GT_VBELN.
      SELECT VBELN FROM VBAK INTO TABLE GT_VBELN
        WHERE VKORG IN ( SELECT VKORG FROM TVKO
        WHERE BUKRS EQ P_BUKRS ).
      CLEAR LT_RET_TAB[].
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'VBELN'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = GT_VBELN[]
          RETURN_TAB      = LT_RET_TAB
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.
      IF SY-SUBRC = 0.
**  Update the value in ALV cell
        READ TABLE LT_RET_TAB INTO LW_RET_TAB INDEX 1.
        IF SY-SUBRC = 0. " USER DIDN'T CANCEL
          LS_MODI-ROW_ID = ES_ROW_NO-ROW_ID.
          LS_MODI-FIELDNAME = E_FIELDNAME.
          LS_MODI-VALUE = LW_RET_TAB-FIELDVAL.
          ASSIGN ER_EVENT_DATA->M_DATA->* TO <MODTAB>.
          APPEND LS_MODI TO <MODTAB>.
        ENDIF.
        ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD
    HANDLE_TOOLBAR."第三步
    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'POST' TO LS_TOOLBAR-FUNCTION.
    MOVE '审核过账' TO LS_TOOLBAR-QUICKINFO.
    MOVE '审核过账' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'CANCEL' TO LS_TOOLBAR-FUNCTION.
    MOVE '审核冲销' TO LS_TOOLBAR-QUICKINFO.
    MOVE '审核冲销' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'DELETE' TO LS_TOOLBAR-FUNCTION.
    MOVE '删除' TO LS_TOOLBAR-QUICKINFO.
    WRITE ICON_DELETE AS ICON TO LS_TOOLBAR-ICON.
    MOVE '删除' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'RECOVER' TO LS_TOOLBAR-FUNCTION.
    MOVE '恢复' TO LS_TOOLBAR-QUICKINFO.
    WRITE ICON_SYSTEM_UNDO AS ICON TO LS_TOOLBAR-ICON.
    MOVE '恢复' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.

  METHOD
    HANDLE_TOOLBAR2."第一、二步
    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'DELETE' TO LS_TOOLBAR-FUNCTION.
    MOVE '删除' TO LS_TOOLBAR-QUICKINFO.
    WRITE ICON_DELETE AS ICON TO LS_TOOLBAR-ICON.
    MOVE '删除' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'RECOVER' TO LS_TOOLBAR-FUNCTION.
    MOVE '恢复' TO LS_TOOLBAR-QUICKINFO.
    WRITE ICON_SYSTEM_UNDO AS ICON TO LS_TOOLBAR-ICON.
    MOVE '恢复' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.

  METHOD
    HANDLE_DOUBLE_CLICK.
    DATA L_GJAHR TYPE BSEG-GJAHR.
    READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX ES_ROW_NO-ROW_ID.
    IF SY-SUBRC EQ 0.
      CASE E_COLUMN.
        WHEN 'XMBH'."项目编号（销售订单号）
          IF GW_ZFI005-XMBH IS NOT INITIAL.
            SET PARAMETER ID  'AUN' FIELD GW_ZFI005-XMBH.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'BELNR'."过账会计凭证编号
          IF GW_ZFI005-BELNR IS NOT INITIAL.
            L_GJAHR = GW_ZFI005-BUDAT(4).
            SET PARAMETER ID 'BLN' FIELD GW_ZFI005-BELNR .
            SET PARAMETER ID 'BUK' FIELD GW_ZFI005-BUKRS.
            SET PARAMETER ID 'GJR' FIELD L_GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'BELNR_CX'."冲销凭证编号
          IF GW_ZFI005-BELNR_CX IS NOT INITIAL.
            L_GJAHR = GW_ZFI005-BUDAT_CX(4).
            SET PARAMETER ID 'BLN' FIELD GW_ZFI005-BELNR_CX.
            SET PARAMETER ID 'BUK' FIELD GW_ZFI005-BUKRS.
            SET PARAMETER ID 'GJR' FIELD L_GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD
    HANDLE_USER_COMMAND.
    DATA: LT_ROWS TYPE LVC_T_ROW.
    DATA: LW_ROWS TYPE LINE OF LVC_T_ROW.
    DATA: L_SUBRC TYPE SY-SUBRC.
    DATA: L_MESSTAB TYPE TABLE OF BDCMSGCOLL.
    DATA: LW_MESSTAB TYPE BDCMSGCOLL.
    DATA: L_BLDAT_001 TYPE BDCDATA-FVAL.
    DATA: L_BLART_002 TYPE BDCDATA-FVAL.
    DATA: L_BUKRS_003 TYPE BDCDATA-FVAL.
    DATA: L_BUDAT_004 TYPE BDCDATA-FVAL.
    DATA: L_MONAT_005 TYPE BDCDATA-FVAL.
    DATA: L_WAERS_006 TYPE BDCDATA-FVAL.
    DATA: L_XBLNR_007 TYPE BDCDATA-FVAL.
    DATA: L_NEWUM_008 TYPE BDCDATA-FVAL.
    DATA: L_NEWBS_009 TYPE BDCDATA-FVAL.
    DATA: L_NEWKO_010 TYPE BDCDATA-FVAL.
    DATA: L_WRBTR_011 TYPE BDCDATA-FVAL.
    DATA: L_FVAL12 TYPE BDCDATA-FVAL.
    DATA: L_SGTXT_013 TYPE BDCDATA-FVAL.
    DATA: L_NEWBS_014 TYPE BDCDATA-FVAL.
    DATA: L_NEWKO_015 TYPE BDCDATA-FVAL.
    DATA: L_FMORE_016 TYPE BDCDATA-FVAL.
    DATA: L_FVAL17 TYPE BDCDATA-FVAL.
    DATA: L_RSTGR_018 TYPE BDCDATA-FVAL.
    DATA: L_FVAL19 TYPE BDCDATA-FVAL.
    DATA: L_FVAL20 TYPE BDCDATA-FVAL.
    DATA: L_HKONT_021 TYPE BDCDATA-FVAL.
    DATA: L_WRBTR_022 TYPE BDCDATA-FVAL.
    DATA: L_ZFBDT_023 TYPE BDCDATA-FVAL.
    DATA: L_ZUONR_024 TYPE BDCDATA-FVAL.
    DATA: L_SGTXT_025 TYPE BDCDATA-FVAL.
    DATA: L_FVAL26 TYPE BDCDATA-FVAL.
    DATA: L_FVAL27 TYPE BDCDATA-FVAL.
    DATA: L_ZUONR_025 TYPE BDCDATA-FVAL.
    DATA: L_SGTXT_014 TYPE BDCDATA-FVAL.
    DATA: L_ZFBDT_015 TYPE BDCDATA-FVAL.
    DATA: L_WNAME_016 TYPE BDCDATA-FVAL.
    DATA: L_WBZOG_018 TYPE BDCDATA-FVAL.
    DATA: L_ZUONR_013 TYPE BDCDATA-FVAL.
    DATA: L_NEWBS_020 TYPE BDCDATA-FVAL.
    DATA: L_NEWKO_021 TYPE BDCDATA-FVAL.
    DATA: L_WRBTR_023 TYPE BDCDATA-FVAL.
    DATA: L_ZFBDT_024 TYPE BDCDATA-FVAL.
    DATA: L_NEWUM_027 TYPE BDCDATA-FVAL.
    DATA: L_HKONT_022 TYPE BDCDATA-FVAL.
    DATA: L_BELNS_001 TYPE BDCDATA-FVAL.
    DATA: L_BUKRS_002 TYPE BDCDATA-FVAL.
    DATA: L_GJAHS_003 TYPE BDCDATA-FVAL.
    DATA: L_STGRD_004 TYPE BDCDATA-FVAL.
    DATA: L_CXBUDAT_005 TYPE BDCDATA-FVAL.
    DATA: L_CXMONAT_006 TYPE BDCDATA-FVAL.
    DATA: L_BKTXT TYPE BDCDATA-FVAL.
    DATA: L_WDATE TYPE BDCDATA-FVAL."出票日期
    DATA: L_WLZBP TYPE BDCDATA-FVAL."出票银行
    DATA: L_NEWUM_011 TYPE BDCDATA-FVAL."
    DATA: L_WRBTR_012 TYPE BDCDATA-FVAL."
    DATA IT_MESSAGE TYPE USMD_T_MESSAGE.
    DATA IW_MESSAGE LIKE LINE OF IT_MESSAGE.
    CLEAR L_MESSTAB[].
    CALL METHOD GRID1->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS.
    CASE E_UCOMM.
      WHEN 'POST'.
        LOOP AT LT_ROWS INTO LW_ROWS.
          CLEAR L_MESSTAB[].
          READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
          IF SY-SUBRC EQ 0.
            IF GW_ZFI005-STEP NE ICON_YELLOW_LIGHT OR GW_ZFI005-DEL_ICON NE SPACE.
              MESSAGE '请检查该行的状态！' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
            DATA: L_GL TYPE C."X表示关联，空表示非关联
            DATA: L_AKONT TYPE KNB1-AKONT."统驭科目
            CLEAR: L_AKONT.
            "l_gl = 'X'."关联
            SELECT SINGLE AKONT FROM KNB1 INTO L_AKONT WHERE KUNNR = GW_ZFI005-KUNNR AND BUKRS = GW_ZFI005-BUKRS.
*            IF l_akont EQ '1122010101'."如果科目为1122010101，则为非关联，否则为关联
*              CLEAR l_gl.
*            ENDIF.
            IF L_AKONT EQ  '1122020101'  . "如果统驭科目为1122020101 则为关联，否则为 非关联
              L_GL = 'X'."关联
            ELSE.
              CLEAR L_GL. "非关联
            ENDIF.
            IF GW_ZFI005-KXZL EQ '现金' OR GW_ZFI005-KXZL EQ '信用证' OR GW_ZFI005-KXZL EQ '银行存款'.
              "GW_ZFI005-DELFLAG = 'X'.
              L_BLDAT_001 = GW_ZFI005-KXLRDAT."到款日期
              IF L_GL = 'X'.
                L_BLART_002 = 'ZA'.
              ELSE.
                L_BLART_002 = 'SA'.
              ENDIF.
              L_BUKRS_003 = GW_ZFI005-BUKRS.
              IF GW_ZFI005-POSTDAT IS INITIAL.
                L_BUDAT_004 = SY-DATUM.
              ELSE.
                L_BUDAT_004 = GW_ZFI005-POSTDAT."过账日期
              ENDIF.
              L_MONAT_005 = GW_ZFI005-KXLRDAT+4(2)."到款日期
              L_WAERS_006 = GW_ZFI005-WAERS.
              L_XBLNR_007 = GW_ZFI005-GZCKH(14).
*             L_NEWUM_008 = '*'.
              L_NEWUM_008 = ''. "第二行SGL标识
              L_NEWBS_009 = '40'."'第一行记帐码
              CASE GW_ZFI005-KXZL.
                WHEN '现金'.
                  L_NEWKO_010 = GW_ZFI005-XJKM.
                WHEN '信用证'.
                  L_NEWKO_010 = GW_ZFI005-XYZKM.
                WHEN '银行存款'.
                  L_NEWKO_010 = GW_ZFI005-YHKM.
              ENDCASE.
              L_WRBTR_011 = GW_ZFI005-HSL.
              SHIFT L_WRBTR_011 LEFT DELETING LEADING SPACE.
              L_FVAL12 = GW_ZFI005-XMBH.
              CONCATENATE GW_ZFI005-KXZL GW_ZFI005-YWZL INTO L_SGTXT_013 SEPARATED BY SPACE.
              L_NEWBS_014 = '11'."第二行记帐码
              L_NEWKO_015 = GW_ZFI005-KUNNR.
              L_FMORE_016 = 'X'.
              L_RSTGR_018 = 'A01'.
*             L_HKONT_021 = '1122010101'.
              L_WRBTR_022 = GW_ZFI005-HSL.
              L_ZFBDT_023 = GW_ZFI005-HPDQDAT.
              L_ZUONR_024 = GW_ZFI005-XMBH.
              L_SGTXT_025 = L_SGTXT_013.
              L_BKTXT = GW_ZFI005-BKTXT.

****************BEGAIN增加付款字段到抬头文本  BY HANDWY 2015-07-15.
              CONCATENATE GW_ZFI005-FKR L_SGTXT_025 INTO L_SGTXT_025.
              CONCATENATE GW_ZFI005-FKR L_SGTXT_013 INTO L_SGTXT_013.
****************END***********************************************

              IF GW_ZFI005-YWZL EQ '预收'.
                L_NEWUM_008 = 'A'."第二行SGL标识
                L_NEWBS_014 = '19'."第二行记帐码
              ENDIF.
              "设置记帐码和SGL标识
              CASE GW_ZFI005-YWZL.
                WHEN '项目回款'.
                  L_NEWBS_009 = '40'."第一行记帐码
                  L_NEWUM_008 = ''."第二行SGL标识
                  L_NEWBS_014 = '11' ."第二行记帐码
                WHEN '项目预收'.
                  L_NEWBS_009 = '40'."第一行记帐码
                  L_NEWUM_008 = 'A'."第二行SGL标识
                  L_NEWBS_014 = '19' ."第二行记帐码
                WHEN '维修回款'.
                  IF L_GL EQ 'X'.
                    L_NEWBS_009 = '40'."第一行记帐码
                    L_NEWUM_008 = ''."第二行SGL标识
                    L_NEWBS_014 = '11' ."第二行记帐码
                  ELSE.
                    L_NEWBS_009 = '40'."第一行记帐码
                    L_NEWUM_008 = 'T'."第二行SGL标识
                    L_NEWBS_014 = '19' ."第二行记帐码
                  ENDIF.
                WHEN '维修预收'.
                  IF L_GL EQ 'X'.
                    L_NEWBS_009 = '40'."第一行记帐码
                    L_NEWUM_008 = 'A'."第二行SGL标识
                    L_NEWBS_014 = '19' ."第二行记帐码
                  ELSE.
                    L_NEWBS_009 = '40'."第一行记帐码
                    L_NEWUM_008 = 'C'."第二行SGL标识
                    L_NEWBS_014 = '19' ."第二行记帐码
                  ENDIF.
              ENDCASE.
              CONDENSE: L_BLDAT_001,L_BLART_002,L_BUKRS_003,L_BUDAT_004,L_MONAT_005,L_WAERS_006,L_XBLNR_007,L_NEWUM_008,L_NEWBS_009,L_NEWKO_010.
              CONDENSE: L_WRBTR_011,L_FVAL12,L_SGTXT_013,L_NEWBS_014,L_NEWKO_015,L_FMORE_016,L_FVAL17,L_RSTGR_018,L_FVAL19,L_FVAL20.
              CONDENSE: L_HKONT_021,L_WRBTR_022,L_ZFBDT_023,L_ZUONR_024,L_SGTXT_025 ,L_BKTXT.
              CALL FUNCTION 'ZFI005_1'
                EXPORTING
*                 CTU       = 'X'
                  MODE      = 'N'
*                 MODE      = 'E'
*                 UPDATE    = 'L'
*                 GROUP     =
*                 USER      =
*                 KEEP      =
*                 HOLDDATE  =
*                 NODATA    = '/'
                  BLDAT_001 = L_BLDAT_001 "到款日期
                  BLART_002 = L_BLART_002
                  BUKRS_003 = L_BUKRS_003
                  BUDAT_004 = L_BUDAT_004 "到款日期
                  MONAT_005 = L_MONAT_005 "到款日期
                  WAERS_006 = L_WAERS_006
                  XBLNR_007 = L_XBLNR_007 "参照文本
                  NEWUM_008 = L_NEWUM_008 "'W'
*                 DOCID_008 = L_NEWUM_008
                  NEWBS_009 = L_NEWBS_009
                  NEWKO_010 = L_NEWKO_010
                  WRBTR_011 = L_WRBTR_011
                  ZUONR_012 = L_ZUONR_024 "'' "项目编号
                  SGTXT_013 = L_SGTXT_013
                  NEWBS_014 = L_NEWBS_014
                  NEWKO_015 = L_NEWKO_015
                  FMORE_016 = L_FMORE_016
                  RSTGR_018 = L_RSTGR_018
                  HKONT_021 = L_HKONT_021
                  WRBTR_022 = L_WRBTR_022
                  ZFBDT_023 = L_ZFBDT_023
                  ZUONR_024 = L_ZUONR_024 "项目编号
                  SGTXT_025 = L_SGTXT_025
                  BKTXT     = L_BKTXT
                IMPORTING
                  SUBRC     = L_SUBRC
                TABLES
                  MESSTAB   = L_MESSTAB[].
            ELSE."票据
              L_BLDAT_001 = GW_ZFI005-KXLRDAT."到款日期
              IF L_GL = 'X'.
                L_BLART_002 = 'ZA'.
              ELSE.
                L_BLART_002 = 'SA'.
              ENDIF.
              L_BUKRS_003 = GW_ZFI005-BUKRS.
              IF GW_ZFI005-POSTDAT IS INITIAL.
                L_BUDAT_004 = SY-DATUM.
              ELSE.
                L_BUDAT_004 = GW_ZFI005-POSTDAT."过账日期
              ENDIF.
              L_MONAT_005 = GW_ZFI005-KXLRDAT+4(2)."期间
              L_WAERS_006 = GW_ZFI005-WAERS.
              L_XBLNR_007 = GW_ZFI005-GZCKH(14).
*              L_FVAL08 = '*'.
              L_NEWBS_009 = '09'.
              L_NEWKO_010 = GW_ZFI005-KUNNR.
              L_NEWUM_011 = 'W'.
              L_WRBTR_012 = GW_ZFI005-HSL.
              SHIFT L_WRBTR_012 LEFT DELETING LEADING SPACE.
              L_ZUONR_025 = GW_ZFI005-XMBH.
              CONCATENATE GW_ZFI005-KXZL GW_ZFI005-YWZL INTO L_SGTXT_014 SEPARATED BY SPACE.
              L_ZFBDT_015 = GW_ZFI005-HPDQDAT.
              L_WNAME_016 = GW_ZFI005-CPR."出票人 W
              L_WBZOG_018 = GW_ZFI005-SPR."受票人 W
              L_WDATE = GW_ZFI005-WDATE."出票日期 W
              L_WLZBP = GW_ZFI005-WLZBP."出票银行 W
              L_ZUONR_013 = GW_ZFI005-PJBH."票据编号
              L_NEWBS_020 = '11'.
*              L_FVAL25 = ''.
              L_NEWKO_021 = GW_ZFI005-KUNNR.
*              L_HKONT_022 = GW_ZFI005-PJKM.
              L_WRBTR_023 = L_WRBTR_012.
              L_ZFBDT_024 = GW_ZFI005-KXLRDAT."到款日期
              L_BKTXT = GW_ZFI005-BKTXT.

****************BEGAIN增加付款字段到抬头文本  BY HANDWY 2015-07-15.
              CONCATENATE GW_ZFI005-FKR L_SGTXT_014  INTO L_SGTXT_014.
****************END***********************************************

              IF GW_ZFI005-YWZL EQ '预收'.
                L_NEWUM_027 = 'A'."
                L_NEWBS_020 = '19'.
              ENDIF.
              CASE GW_ZFI005-YWZL.
                WHEN '项目回款'.
                  L_NEWBS_009 = '09'."第一行记帐码
                  L_NEWUM_011 = 'W'."第一行SGL标识
                  L_NEWBS_020 = '11'."第二行记帐码
                  L_NEWUM_027 = ''."第二行SGL标识
                WHEN '项目预收'.
                  L_NEWBS_009 = '09'."第一行记帐码
                  L_NEWUM_011 = 'W'."第一行SGL标识
                  L_NEWBS_020 = '19'."第二行记帐码
                  L_NEWUM_027 = 'A'."第二行SGL标识
                WHEN '维修回款'.
                  IF L_GL EQ 'X'.
                    L_NEWBS_009 = '09'."第一行记帐码
                    L_NEWUM_011 = 'W'."第一行SGL标识
                    L_NEWBS_020 = '11'."第二行记帐码
                    L_NEWUM_027 = ''."第二行SGL标识
                  ELSE.
                    L_NEWBS_009 = '09'."第一行记帐码
                    L_NEWUM_011 = 'W'."第一行SGL标识
                    L_NEWBS_020 = '19'."第二行记帐码
                    L_NEWUM_027 = 'T'."第二行SGL标识
                  ENDIF.
                WHEN '维修预收'.
                  IF L_GL EQ 'X'.
                    L_NEWBS_009 = '09'."第一行记帐码
                    L_NEWUM_011 = 'W'."第一行SGL标识
                    L_NEWBS_020 = '19'."第二行记帐码
                    L_NEWUM_027 = 'A'."第二行SGL标识
                  ELSE.
                    L_NEWBS_009 = '09'."第一行记帐码
                    L_NEWUM_011 = 'W'."第一行SGL标识
                    L_NEWBS_020 = '19'."第二行记帐码
                    L_NEWUM_027 = 'C'."第二行SGL标识
                  ENDIF.
              ENDCASE.
              CONDENSE: L_BLDAT_001,L_BLART_002,L_BUKRS_003,L_BUDAT_004,L_MONAT_005,L_WAERS_006,L_XBLNR_007,L_NEWBS_009,L_NEWKO_010.
              CONDENSE: L_NEWUM_011,L_WRBTR_012,L_ZUONR_025,L_SGTXT_014,L_ZFBDT_015,L_WNAME_016,L_FVAL17,L_WBZOG_018,L_ZUONR_013,L_NEWBS_020.
              CONDENSE: L_NEWKO_021,L_HKONT_022,L_WRBTR_023,L_ZFBDT_024,L_FVAL26,L_NEWUM_027, L_BKTXT, L_WDATE, L_WLZBP.
              BREAK HANDTB.
              CALL FUNCTION 'ZFI005_3'
                EXPORTING
*                 CTU       = 'X'
                  MODE      = 'N'
*                 UPDATE    = 'L'
*                 GROUP     =
*                 USER      =
*                 KEEP      =
*                 HOLDDATE  =
*                 NODATA    = '/'
                  BLDAT_001 = L_BLDAT_001 "'2015.01.22'
                  BLART_002 = L_BLART_002 "'SA'
                  BUKRS_003 = L_BUKRS_003 "'1000'
                  BUDAT_004 = L_BUDAT_004 "过账日期
                  MONAT_005 = L_MONAT_005 "'"期间
                  WAERS_006 = L_WAERS_006 "'CNY'
                  XBLNR_007 = L_XBLNR_007 "'参照文本'
*                 DOCID_008 = L_FVAL08"'*'
                  NEWBS_009 = L_NEWBS_009 "'09'
                  NEWKO_010 = L_NEWKO_010 "'100000'
                  NEWUM_011 = L_NEWUM_011 "'W'
                  WRBTR_012 = L_WRBTR_012 "'123'
                  ZUONR_013 = L_ZUONR_013 "票据编号
                  SGTXT_014 = L_SGTXT_014
                  ZFBDT_015 = L_ZFBDT_015 "'2015.02.14'
                  WNAME_016 = L_WNAME_016
                  WORT1_017 = ''
                  WBZOG_018 = L_WBZOG_018
                  WDATE     = L_WDATE "出票日期 W
                  WLZBP     = L_WLZBP "出票银行 W
                  WBANK_019 = L_ZUONR_013 "票据编号
                  NEWBS_020 = L_NEWBS_020 "'11'
                  NEWKO_021 = L_NEWKO_021 "'100000'
                  HKONT_022 = L_HKONT_022 "'2203010101'
                  WRBTR_023 = L_WRBTR_023 "'123'
                  ZFBDT_024 = L_ZFBDT_024 "'2015.01.22'
                  ZUONR_025 = L_ZUONR_025 "项目编号
                  SGTXT_026 = L_SGTXT_014 "'文本2'
                  NEWUM_027 = L_NEWUM_027 "'W'
                  BKTXT     = L_BKTXT
                IMPORTING
                  SUBRC     = L_SUBRC
                TABLES
                  MESSTAB   = L_MESSTAB[].
            ENDIF.
*            ENDLOOP.
            CLEAR IT_MESSAGE[].
            LOOP AT L_MESSTAB INTO LW_MESSTAB WHERE MSGTYP EQ 'E' OR MSGTYP EQ 'A'."错误或终止
              IW_MESSAGE-MSGID = LW_MESSTAB-MSGID.
              IW_MESSAGE-MSGTY = LW_MESSTAB-MSGTYP.
              IW_MESSAGE-MSGNO = LW_MESSTAB-MSGNR.
              IW_MESSAGE-MSGV1 = LW_MESSTAB-MSGV1.
              IW_MESSAGE-MSGV2 = LW_MESSTAB-MSGV2.
              IW_MESSAGE-MSGV3 = LW_MESSTAB-MSGV3.
              IW_MESSAGE-MSGV4 = LW_MESSTAB-MSGV4.
              APPEND IW_MESSAGE TO IT_MESSAGE.
              CLEAR IW_MESSAGE.
            ENDLOOP.
            IF IT_MESSAGE IS NOT INITIAL."错误或终止
              CALL FUNCTION 'USMD_MESSAGE_POPUP'
                EXPORTING
                  IT_MESSAGE        = IT_MESSAGE
                  IF_SAVE_NECESSARY = SPACE
*               IMPORTING
*                 EF_CONTINUE       =
                .
*              WRITE ICON_RED_LIGHT AS ICON TO GW_ZFI005-STEP.
*              GW_ZFI005-DELFLAG = 'X'.
            ELSE.
              READ TABLE L_MESSTAB INTO LW_MESSTAB WITH KEY MSGTYP = 'S' MSGID = 'F5' MSGNR = '312'.
              IF SY-SUBRC EQ 0.
                GW_ZFI005-BUDAT = SY-DATUM.
                GW_ZFI005-BELNR = LW_MESSTAB-MSGV1.
                MESSAGE ID LW_MESSTAB-MSGID TYPE 'S' NUMBER LW_MESSTAB-MSGNR
                WITH LW_MESSTAB-MSGV1 LW_MESSTAB-MSGV2 LW_MESSTAB-MSGV3 LW_MESSTAB-MSGV4.
                WRITE ICON_GREEN_LIGHT AS ICON TO GW_ZFI005-STEP.
              ELSE.
                MESSAGE '凭证未生成！' TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.
            ENDIF.
            MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_ROWS-INDEX.
          ENDIF.
        ENDLOOP.
      WHEN 'CANCEL'.
        LOOP AT LT_ROWS INTO LW_ROWS.
          CLEAR L_MESSTAB[].
          READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
          IF SY-SUBRC EQ 0.
            IF GW_ZFI005-STEP NE ICON_GREEN_LIGHT OR GW_ZFI005-DEL_ICON NE SPACE.
              MESSAGE '请检查该行的状态！' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
            "GW_ZFI005-DELFLAG = 'X'.
            L_BELNS_001 = GW_ZFI005-BELNR.
            L_BUKRS_002 = GW_ZFI005-BUKRS.
            L_GJAHS_003 = GW_ZFI005-KXLRDAT(4)."到款日期
            L_STGRD_004 = '03'.
            CONDENSE: L_BELNS_001,L_BUKRS_002,L_GJAHS_003,L_STGRD_004.
            BREAK HANDTB.CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
          "*      TITLEBAR       = ' '
           "*     DIAGNOSE_OBJECT             = ' '
                 TEXT_QUESTION  = '确认要执行冲销操作'
               IMPORTING
                  ANSWER        = G_ANSWER
          "*   TABLES
          "*         PARAMETER      =
               EXCEPTIONS
                 TEXT_NOT_FOUND = 1
               OTHERS         = 2.
            IF G_ANSWER <> '1'.
              EXIT.
            ENDIF.
            "*取冲销原因和过账日期
            CLEAR WA_REVERSAL.
            PERFORM FRM_GET_REV_REASON.

            IF WA_REVERSAL-REASON_REV IS INITIAL.
              MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.
            PERFORM FRM_MESSAGE_INITIAL.
            CLEAR G_SUC.

            L_BELNS_001 = GW_ZFI005-BELNR.
            L_BUKRS_002 = GW_ZFI005-BUKRS.
            L_GJAHS_003 = GW_ZFI005-KXLRDAT(4)."到款日期
            L_STGRD_004 = WA_REVERSAL-REASON_REV ."冲销原因
            IF L_STGRD_004 EQ '04'.
              L_CXBUDAT_005 = WA_REVERSAL-PSTNG_DATE. "过账日期
              L_CXMONAT_006 = WA_REVERSAL-PSTNG_DATE+4(2). "过账期间
            ENDIF.
            CONDENSE: L_BELNS_001,L_BUKRS_002,L_GJAHS_003,L_STGRD_004.
            BREAK HANDTB.
            CALL FUNCTION 'ZFI005_2'
              EXPORTING
*               CTU         = 'X'
                MODE        = 'N'
*               UPDATE      = 'L'
*               GROUP       =
*               USER        =
*               KEEP        =
*               HOLDDATE    =
*               NODATA      = '/'
                BELNS_001   = L_BELNS_001
                BUKRS_002   = L_BUKRS_002
                GJAHS_003   = L_GJAHS_003
                STGRD_004   = L_STGRD_004
                CXBUDAT_005 = L_CXBUDAT_005
                CXMONAT_006 = L_CXMONAT_006
              IMPORTING
                SUBRC       = L_SUBRC
              TABLES
                MESSTAB     = L_MESSTAB[].
*            ENDLOOP.
            CLEAR IT_MESSAGE[].
            LOOP AT L_MESSTAB INTO LW_MESSTAB WHERE MSGTYP EQ 'E' OR MSGTYP EQ 'A'."错误或终止
              IW_MESSAGE-MSGID = LW_MESSTAB-MSGID.
              IW_MESSAGE-MSGTY = LW_MESSTAB-MSGTYP.
              IW_MESSAGE-MSGNO = LW_MESSTAB-MSGNR.
              IW_MESSAGE-MSGV1 = LW_MESSTAB-MSGV1.
              IW_MESSAGE-MSGV2 = LW_MESSTAB-MSGV2.
              IW_MESSAGE-MSGV3 = LW_MESSTAB-MSGV3.
              IW_MESSAGE-MSGV4 = LW_MESSTAB-MSGV4.
              APPEND IW_MESSAGE TO IT_MESSAGE.
              CLEAR IW_MESSAGE.
            ENDLOOP.
            IF IT_MESSAGE IS NOT INITIAL."错误或终止
              CALL FUNCTION 'USMD_MESSAGE_POPUP'
                EXPORTING
                  IT_MESSAGE        = IT_MESSAGE
                  IF_SAVE_NECESSARY = SPACE
*               IMPORTING
*                 EF_CONTINUE       =
                .
            ELSE.
              DATA LW_ZFI005 LIKE GW_ZFI005.
              CLEAR LW_ZFI005.
              READ TABLE L_MESSTAB INTO LW_MESSTAB WITH KEY MSGTYP = 'S' MSGID = 'F5' MSGNR = '312'.
              IF SY-SUBRC EQ 0.
                GW_ZFI005-BELNR_CX = LW_MESSTAB-MSGV1. "冲销凭证
                "modify 冲销日期原由系统日期 更改为
                IF WA_REVERSAL-REASON_REV  EQ '04'.
                  GW_ZFI005-BUDAT_CX = WA_REVERSAL-PSTNG_DATE.      "冲销过账日期
                ENDIF.
                IF WA_REVERSAL-REASON_REV  EQ '03'.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = GW_ZFI005-BELNR_CX
                    IMPORTING
                      OUTPUT = GW_ZFI005-BELNR_CX.

                  SELECT SINGLE BUDAT INTO GW_ZFI005-BUDAT_CX "冲销过账日期
                    FROM BKPF
                    WHERE  BUKRS = P_BUKRS
                    AND   BELNR = GW_ZFI005-BELNR_CX
                    AND   GJAHR = GW_ZFI005-BUDAT+0(4)."过账日期年度
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      INPUT  = GW_ZFI005-BELNR_CX
                    IMPORTING
                      OUTPUT = GW_ZFI005-BELNR_CX.

                ENDIF.
                "冲销凭证复制为处理中状态
                MOVE-CORRESPONDING GW_ZFI005 TO LW_ZFI005.
                WRITE ICON_YELLOW_LIGHT AS ICON TO LW_ZFI005-STEP.
                CLEAR: LW_ZFI005-XSHKNO, LW_ZFI005-BELNR, LW_ZFI005-BUDAT, LW_ZFI005-GZCKH,
                LW_ZFI005-BELNR_CX, LW_ZFI005-BUDAT_CX, LW_ZFI005-DELFLAG.
                APPEND LW_ZFI005 TO GT_ZFI005_EDIT.
                PERFORM PROCESS_SAVEDATA.
                PERFORM UPDATE_DATABASE.
                MESSAGE ID LW_MESSTAB-MSGID TYPE 'S' NUMBER LW_MESSTAB-MSGNR
                WITH LW_MESSTAB-MSGV1 LW_MESSTAB-MSGV2 LW_MESSTAB-MSGV3 LW_MESSTAB-MSGV4.
              ENDIF.
              READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
              IF SY-SUBRC = 0.
                WRITE ICON_RED_LIGHT AS ICON TO GW_ZFI005-STEP.
                GW_ZFI005-BELNR_CX = LW_MESSTAB-MSGV1.  "冲销凭证号
                "modify 冲销日期原由系统日期 更改为
                IF WA_REVERSAL-REASON_REV  EQ '04'.
                  GW_ZFI005-BUDAT_CX = WA_REVERSAL-PSTNG_DATE.      "冲销过账日期
                ENDIF.
                IF WA_REVERSAL-REASON_REV  EQ '03'.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = GW_ZFI005-BELNR_CX
                    IMPORTING
                      OUTPUT = GW_ZFI005-BELNR_CX.

                  SELECT SINGLE BUDAT INTO GW_ZFI005-BUDAT_CX "冲销过账日期
                    FROM BKPF
                    WHERE  BUKRS = P_BUKRS
                    AND   BELNR = GW_ZFI005-BELNR_CX
                    AND   GJAHR = GW_ZFI005-BUDAT+0(4)."过账日期年度

                ENDIF.
*              GW_ZFI005-DELFLAG = 'X'.
                MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_ROWS-INDEX.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      WHEN 'DELETE'."删除
        LOOP AT LT_ROWS INTO LW_ROWS.
          READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
          IF SY-SUBRC EQ 0.
            WRITE ICON_DELETE AS ICON TO GW_ZFI005-DEL_ICON.
            MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_ROWS-INDEX.
          ENDIF.
        ENDLOOP.
      WHEN 'RECOVER'."恢复
        LOOP AT LT_ROWS INTO LW_ROWS.
          READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
          IF SY-SUBRC EQ 0.
            CLEAR GW_ZFI005-DEL_ICON.
            MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_ROWS-INDEX.
          ENDIF.
        ENDLOOP.
    ENDCASE.
    "更新单元格可编辑状态
    PERFORM FRM_CELLSTYPE.
    "刷新ALV
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = G_IS_STABLE.
    PERFORM UPDATE_DATABASE.
*    MESSAGE S000(ZFICO01).

  ENDMETHOD.

  METHOD
    HANDLE_DATA_CHANGED.
  ENDMETHOD.
  METHOD
    HANDLE_DATA_CHANGED_FINISHED.
    IF E_MODIFIED EQ 'X'.
      DATA L_LINES TYPE I.
      DATA LW_CELL LIKE LINE OF ET_GOOD_CELLS.
      L_LINES = LINES( GT_ZFI005_EDIT[] ).
      "如果已修改的行数>2，则判断为新增行
      IF LINES( ET_GOOD_CELLS[] ) > 2.
        "获取新添加的行
        READ TABLE ET_GOOD_CELLS INTO LW_CELL INDEX 1.
        "为新行设置默认值
        READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_CELL-ROW_ID.
        GW_ZFI005-BUKRS = P_BUKRS.
        SELECT SINGLE BUTXT FROM T001 INTO GW_ZFI005-BUKRST WHERE BUKRS EQ P_BUKRS.
        WRITE ICON_YELLOW_LIGHT AS ICON TO GW_ZFI005-STEP.
        GW_ZFI005-KXLRDAT = SY-DATUM."到款日期
        GW_ZFI005-POSTDAT = SY-DATUM."过账日期
        GW_ZFI005-LINE_NO = '0001'.
        GW_ZFI005-INIT_LINE = 'X'.
        MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_CELL-ROW_ID.
      ENDIF.
      LOOP AT ET_GOOD_CELLS INTO LW_CELL WHERE FIELDNAME EQ 'KUNNR'.
        READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_CELL-ROW_ID.
        SELECT SINGLE NAME1 FROM KNA1 INTO GW_ZFI005-KUNNRT WHERE KUNNR EQ LW_CELL-VALUE.
        MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_CELL-ROW_ID.
      ENDLOOP.
      LOOP AT ET_GOOD_CELLS INTO LW_CELL WHERE FIELDNAME EQ 'XMBH'.
        READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_CELL-ROW_ID.
        DATA L_KUNNR TYPE VBPA-KUNNR.
        SELECT SINGLE KUNNR FROM VBPA INTO GW_ZFI005-KUNNR WHERE VBELN EQ LW_CELL-VALUE AND PARVW EQ 'RE' AND KUNNR NE SPACE."收票方
        IF SY-SUBRC EQ 0.
          SELECT SINGLE NAME1 FROM KNA1 INTO GW_ZFI005-KUNNRT WHERE KUNNR EQ GW_ZFI005-KUNNR.
          MODIFY GT_ZFI005_EDIT FROM GW_ZFI005 INDEX LW_CELL-ROW_ID.
        ENDIF.
      ENDLOOP.
      PERFORM FRM_CELLSTYPE.
      "刷新ALV
      CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = G_IS_STABLE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
  IF GRID1 IS INITIAL.
    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CL_GUI_CONTAINER=>DEFAULT_SCREEN.
    CREATE OBJECT GO_RECEIVER.
    IF X_STEP3 EQ 'X'.
      SET HANDLER GO_RECEIVER->HANDLE_TOOLBAR FOR GRID1.
    ELSE.
      SET HANDLER GO_RECEIVER->HANDLE_TOOLBAR2 FOR GRID1.
    ENDIF.
    SET HANDLER GO_RECEIVER->HANDLE_USER_COMMAND FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_DOUBLE_CLICK FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_DATA_CHANGED FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_ONF4 FOR GRID1.
    DATA: LT_F4 TYPE LVC_T_F4 WITH HEADER LINE.
    CLEAR LT_F4.
    "注意，这里要按FIELDNAME升序APPEND，因为LT_F4是排序表
    LT_F4-FIELDNAME = 'PJKM'.
    LT_F4-REGISTER = 'X'.
    LT_F4-CHNGEAFTER = 'X'.
    APPEND LT_F4.
    LT_F4-FIELDNAME = 'XJKM'.
    APPEND LT_F4.
    LT_F4-FIELDNAME = 'XMBH'.
    APPEND LT_F4.
    LT_F4-FIELDNAME = 'XYZKM'.
    APPEND LT_F4.
    LT_F4-FIELDNAME = 'YHKM'.
    APPEND LT_F4.
    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].
    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER. "回车触发编辑事件
    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED. "失去焦点触发编辑事件
  ENDIF.
  "start保存原始数据
  LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005.
    MOVE-CORRESPONDING GW_ZFI005 TO ZFI005_OLD.
    APPEND ZFI005_OLD.
  ENDLOOP.


  "end
  "显示数据
  CLEAR: T_UIFUNC[],T_FIELDCAT[].
  PERFORM FRM_EXCLUDE_BUTTON.
  PERFORM FRM_FIELDCAT.
  PERFORM FRM_CELLSTYPE.
  W_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
  W_LAYOUT-ZEBRA = 'X'.
  W_VARIANT-REPORT = SY-REPID.
  CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE                        = 'A'
      IS_LAYOUT                     = W_LAYOUT
      IS_VARIANT                    = W_VARIANT
      IT_TOOLBAR_EXCLUDING          = T_UIFUNC
    CHANGING
      IT_OUTTAB                     = GT_ZFI005_EDIT[]
      IT_FIELDCATALOG               = T_FIELDCAT[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  CALL METHOD GRID1->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.
  CASE 'X'.
    WHEN X_STEP1.
      SET TITLEBAR 'TIT01'."出纳/应收录入款项及修改
    WHEN X_STEP2.
      SET TITLEBAR 'TIT02'."销售助理认领
    WHEN X_STEP3.
      SET TITLEBAR 'TIT03'."审核过账/冲销
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  frm_exclude_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE_BUTTON .
  DATA: LW_UIFUNC TYPE UI_FUNC."工具栏标准按钮工作区
  REFRESH T_UIFUNC.

  "剪贴按钮
  LW_UIFUNC = CL_GUI_ALV_GRID=>MC_MB_PASTE.
  APPEND LW_UIFUNC TO T_UIFUNC.

  IF X_STEP1 NE 'X'.
*  附加行按钮
    CLEAR LW_UIFUNC.
    LW_UIFUNC = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND LW_UIFUNC TO T_UIFUNC.
  ENDIF.

*  插入行按钮
  CLEAR LW_UIFUNC.
  LW_UIFUNC = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LW_UIFUNC TO T_UIFUNC.

*  删除行按钮
  CLEAR LW_UIFUNC.
  LW_UIFUNC = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LW_UIFUNC TO T_UIFUNC.

*  复制行按钮
  CLEAR LW_UIFUNC.
  LW_UIFUNC = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LW_UIFUNC TO T_UIFUNC.

*  剪切行按钮
  CLEAR LW_UIFUNC.
  LW_UIFUNC = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LW_UIFUNC TO T_UIFUNC.

ENDFORM. " FRM_EXCLUDE_BUTTON
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE OK_CODE.
    WHEN '&DATA_SAVE'.
      PERFORM SAVE_DATA.
    WHEN '&F03' OR '&F15'."返回
      PERFORM UNLOCK_ZFI005.
      LEAVE TO SCREEN 0.
    WHEN '&F12'."退出
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
FORM SAVE_DATA.
  DATA L_ERR TYPE C.
  IF X_STEP3 NE 'X'."如果不是第三步，检查必输项
    PERFORM CHECK_INPUT CHANGING L_ERR.
    IF L_ERR EQ 'X'.
      RETURN.
    ENDIF.
  ENDIF.
  PERFORM PROCESS_SAVEDATA.
  PERFORM UPDATE_DATABASE.
  MESSAGE S000(0K) WITH TEXT-S01.
ENDFORM.
FORM UPDATE_DATABASE.
  DATA LT_ZFI005 TYPE TABLE OF ZFI005 WITH HEADER LINE.
  DATA:ZFI005_NEW LIKE TABLE OF ZFI005 WITH HEADER LINE.
  DATA: LT_ROWS TYPE LVC_T_ROW.
  DATA: LW_ROWS TYPE LINE OF LVC_T_ROW.
  CLEAR: LT_ZFI005[], LT_ROWS[].
  DATA:FLAG(1) TYPE C.
  IF X_STEP2 EQ 'X'."第二步
    CALL METHOD GRID1->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS.
    LOOP AT LT_ROWS INTO LW_ROWS.
      READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING GW_ZFI005 TO LT_ZFI005.
        APPEND LT_ZFI005.
        CLEAR LT_ZFI005.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005.
      MOVE-CORRESPONDING GW_ZFI005 TO LT_ZFI005.
      APPEND LT_ZFI005.
      CLEAR LT_ZFI005.
    ENDLOOP.
  ENDIF.
  ZFI005_NEW[] = LT_ZFI005[].
*  "开始对比内表变化情况
*   CALL FUNCTION 'CTVB_COMPARE_TABLES'
*    EXPORTING
*      table_old  = ZFI005_OLD[]
*      table_new  = LT_ZFI005[]
*       key_length = 33
*    IMPORTING
*      table_del  = ZFI005_DEL[]
*      table_add  = ZFI005_ADD[]
*     table_mod  = ZFI005_MOD[]
*      no_changes = flag.
*  "结束
*  IF ZFI005_DEL[] IS NOT INITIAL.
*    DELETE ZFI005 FROM TABLE ZFI005_DEL.
*   ENDIF.
  MODIFY ZFI005 FROM TABLE LT_ZFI005.
  COMMIT WORK.
ENDFORM.
FORM UNLOCK_ZFI005.
  CALL FUNCTION 'DEQUEUE_EZFI005'
    EXPORTING
*     MODE_ZFI005       = 'E'
      MANDT  = SY-MANDT
      XSHKNO = L_STEP
      BUKRS  = P_BUKRS
*     X_BUKRS           = ' '
      _SCOPE = '3'
*     _SYNCHRON         = ' '
*     _COLLECT          = ' '
    .
ENDFORM.
FORM LOCK_ZFI005 CHANGING P_SUCCESS TYPE C
                          P_USER.
  P_SUCCESS = SPACE.
  CALL FUNCTION 'ENQUEUE_EZFI005'
    EXPORTING
*     MODE_ZFI005    = 'E'
      MANDT          = SY-MANDT
      XSHKNO         = L_STEP
      BUKRS          = P_BUKRS
*     X_BUKRS        = ' '
      _SCOPE         = '3'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC NE 0.
    P_SUCCESS = SPACE.
  ELSE.
    P_SUCCESS = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT .
  CLEAR T_FIELDCAT[].
  PERFORM ADD_FIELDCAT USING:
*  'CHECKBOX' '' '' '' '' '' 'X',
  'DEL_ICON' '删除标记' '' '' '' '' '',
  'BUKRS' '公司代码' '' '4' '' '' '',
  'BUKRST' '公司代码描述' '' '4' '' '' '',
  'STEP' '处理进度' '' '4' '' '' '',
  'KXLRDAT' '到款日期' '' '1' '' '' '',
  'POSTDAT' '过账日期' '' '3' '' '' '',
  'VKORG' '销售组织' '' '1' '' '' '',
  'LINE_NO' '行号' '' '4' '' '' '',
  'KXZL' '款项种类' '' '1' '' '' '',
  'XJKM' '现金科目' '' '1' 'BSEG' 'HKONT' '',
  'YHKM' '银行科目' '' '1' 'BSEG' 'HKONT' '',
  'XYZKM' '信用证科目' '' '1' 'BSEG' 'HKONT' '',
*  'PJKM' ' 票据科目' '' '1' 'BSEG' 'HKONT' '',
  'YWZL' '业务种类' '' '2' '' '' '',
  'UMSKZ' '特别总账标识' '' '4' '' '' '',
  'HSL' '金额' 'WAERS' '1' '' '' '',
  'WAERS' '货币类型' '' '1' '' '' '',
  'CPR' '出票人' '' '1' '' '' '',
  'SPR' '受票人' '' '1' '' '' '',
  'WDATE' '出票日期' '' '1' '' '' '',
  'WLZBP' '出票银行' '' '1' '' '' '',
  'FKR' '付款人' '' '1' '' '' '',
  'HPDQDAT' '汇票到期日' '' '1' '' '' '',
  'PJBH' '票据编号' '' '1' '' '' '',
  'KUNNR' '客户编号' '' '2' '' '' '',
  'KUNNRT' '客户描述' '' '' '' '' '',
  'XMBH' '项目编号（销售订单号）' '' '2' 'VBAK' 'VBELN' '',
  'GZCKH' '过账参考号' '' '' '' '' '',
  'BELNR' '过账会计凭证编号' '' '' '' '' '',
  'BUDAT' '过账日期' '' '' '' '' '',
  'BKTXT' '摘要' '' '3' '' '' '',
  'BELNR_CX' '冲销凭证编号' '' '' '' '' '',
  'BUDAT_CX' '冲销过账日期' '' '' '' '' ''.
*  'DELFLAG' '删除标记' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_FIELDCAT USING FIELDNAME TEXT CFIELDNAME EDIT REF_TABLE REF_FIELD CHECKBOX.
  CLEAR T_FIELDCAT.
  IF CHECKBOX EQ 'X'.
    T_FIELDCAT-CHECKBOX = CHECKBOX.
    T_FIELDCAT-FIELDNAME = FIELDNAME.
  ELSE.
    T_FIELDCAT-FIELDNAME = FIELDNAME.
    IF REF_FIELD IS INITIAL.
      T_FIELDCAT-REF_FIELD = FIELDNAME.
    ELSE.
      T_FIELDCAT-REF_FIELD = REF_FIELD.
    ENDIF.
    IF REF_TABLE IS INITIAL .
      T_FIELDCAT-REF_TABLE = 'ZFI005'.
    ELSE.
      T_FIELDCAT-REF_TABLE = REF_TABLE.
    ENDIF.
    T_FIELDCAT-COLTEXT = TEXT.
    T_FIELDCAT-REPTEXT = TEXT.
    T_FIELDCAT-SCRTEXT_L = TEXT.
    T_FIELDCAT-SCRTEXT_M = TEXT.
    T_FIELDCAT-SCRTEXT_S = TEXT.
    T_FIELDCAT-CFIELDNAME = CFIELDNAME.
    IF ( EDIT EQ '1'  AND X_STEP1 EQ 'X' "第一步可编辑的字段
      OR EDIT EQ '2'  AND X_STEP2 EQ 'X' "第二步可编辑的字段
      OR EDIT EQ '3'  AND X_STEP3 EQ 'X' )."第三步可编辑的字段
      T_FIELDCAT-EDIT = 'X'.
    ELSEIF EDIT = '4'. "自动设置值
      T_FIELDCAT-AUTO_VALUE = 'X'.
    ENDIF.
  ENDIF.
  T_FIELDCAT-CHECKTABLE = '!'.
  IF FIELDNAME EQ 'XJKM'
    OR FIELDNAME EQ 'YHKM'
    OR FIELDNAME EQ 'XYZKM'
    OR FIELDNAME EQ 'PJKM'.
    T_FIELDCAT-F4AVAILABL = 'X'.
  ENDIF.
  APPEND T_FIELDCAT.
  CLEAR T_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SAVEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_SAVEDATA .
  DATA L_TABIX TYPE I.
  CLEAR L_TABIX.
  LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005 WHERE XSHKNO IS INITIAL.
    ADD 1 TO L_TABIX.
    GW_ZFI005-XSHKNO = L_TABIX.
    SHIFT GW_ZFI005-XSHKNO LEFT DELETING LEADING SPACE.
    CONDENSE GW_ZFI005-XSHKNO.
    CONCATENATE SY-DATUM SY-UZEIT '_' GW_ZFI005-XSHKNO INTO GW_ZFI005-XSHKNO.
    GW_ZFI005-GZCKH = GW_ZFI005-XSHKNO.
    MODIFY GT_ZFI005_EDIT FROM GW_ZFI005.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_HKONT
*&---------------------------------------------------------------------*
*       HKONT
*----------------------------------------------------------------------*
*      -->P_0794   text
*----------------------------------------------------------------------*
FORM APPEND_RANGE USING VALUE(RANGE) VALUE(SIGN) VALUE(OPTION) VALUE(LOW) VALUE(HIGH).
  FIELD-SYMBOLS <FS_RANGE> TYPE ANY.
  FIELD-SYMBOLS <FS_RANGE_TAB> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <FS_COMP> TYPE ANY.
  DATA L_NAME TYPE STRING.
  ASSIGN (RANGE) TO <FS_RANGE>.
  CONCATENATE RANGE '[]' INTO L_NAME.
  ASSIGN (L_NAME) TO <FS_RANGE_TAB>.
  ASSIGN COMPONENT 'SIGN' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = SIGN.
  ASSIGN COMPONENT 'OPTION' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = OPTION.
  ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = LOW.
  ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = HIGH.
  APPEND <FS_RANGE> TO <FS_RANGE_TAB>.
  CLEAR <FS_RANGE>.
ENDFORM. " APPEND_HKONT
*&---------------------------------------------------------------------*
*&      Form  FRM_CELLSTYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CELLSTYPE .
  DATA LT_STYLETAB TYPE LVC_T_STYL.
  CLEAR: LS_STYLEROW, LT_STYLETAB.
  LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005.
    REFRESH GW_ZFI005-CELLSTYLE[].
    MODIFY GT_ZFI005_EDIT FROM GW_ZFI005.
  ENDLOOP.
  LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005 WHERE STEP NE ICON_YELLOW_LIGHT OR DEL_ICON EQ ICON_DELETE."如果不是黄灯，或者是已删除行，则显示为不可输入
*    REFRESH GW_ZFI005-CELLSTYLE[].
    IF GW_ZFI005-CELLSTYLE[] IS INITIAL.
      LOOP AT T_FIELDCAT.
        LS_STYLEROW-FIELDNAME = T_FIELDCAT-FIELDNAME.
        LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_STYLEROW INTO TABLE LT_STYLETAB.
      ENDLOOP.
      GW_ZFI005-CELLSTYLE = LT_STYLETAB[].
      MODIFY GT_ZFI005_EDIT FROM GW_ZFI005.
    ENDIF.
  ENDLOOP.
  DATA L_STYPE1 LIKE CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  DATA L_STYPE2 LIKE CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  DATA L_STYPE3 LIKE CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  DATA L_STYPE4 LIKE CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  IF X_STEP1 EQ 'X'."出纳填写
    LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005 WHERE STEP EQ ICON_YELLOW_LIGHT AND DEL_ICON NE ICON_DELETE."只处理黄灯未删除项目
*      REFRESH GW_ZFI005-CELLSTYLE[].
      L_STYPE1 = L_STYPE2 = L_STYPE3 = L_STYPE4 = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      CASE GW_ZFI005-KXZL.
        WHEN '现金'."
          L_STYPE1 = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        WHEN '银行存款'."
          L_STYPE2 = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        WHEN '银行承兑汇票' OR '商业承兑汇票'."
          L_STYPE4 = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        WHEN '信用证'."
          L_STYPE3 = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ENDCASE.
      PERFORM SET_CELLSTYPE USING:
            'XJKM'  L_STYPE1,
            'YHKM'  L_STYPE2,
            'XYZKM' L_STYPE3,
            'PJKM' L_STYPE4,
            'CPR' L_STYPE4,
            'SPR' L_STYPE4,
            'WDATE' L_STYPE4,
            'WLZBP' L_STYPE4,
*            'FKR' L_STYPE4,
            'HPDQDAT' L_STYPE4,
            'PJBH' L_STYPE4.
      MODIFY GT_ZFI005_EDIT FROM GW_ZFI005.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_CELLSTYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FIELDNAME   text
*      -->STYLE  text
*----------------------------------------------------------------------*
FORM SET_CELLSTYPE  USING FIELDNAME
                          STYLE.
  DATA LS_STYLEROW TYPE LVC_S_STYL.
  DATA LT_STYLETAB TYPE LVC_T_STYL.
  CLEAR: LS_STYLEROW, LT_STYLETAB.
  READ TABLE GW_ZFI005-CELLSTYLE INTO LS_STYLEROW WITH KEY FIELDNAME = FIELDNAME.
  IF SY-SUBRC EQ 0.
    LS_STYLEROW-FIELDNAME = FIELDNAME.
    LS_STYLEROW-STYLE = STYLE.
    MODIFY TABLE GW_ZFI005-CELLSTYLE FROM LS_STYLEROW.
  ELSE.
    LS_STYLEROW-FIELDNAME = FIELDNAME.
    LS_STYLEROW-STYLE = STYLE.
    INSERT LS_STYLEROW INTO TABLE GW_ZFI005-CELLSTYLE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT CHANGING P_ERR.
  CLEAR P_ERR.
  DATA: LT_ROWS TYPE LVC_T_ROW.
  DATA: LW_ROWS TYPE LINE OF LVC_T_ROW.
  DATA L_XMBH LIKE GW_ZFI005-XMBH.
  IF X_STEP2 EQ 'X'."第二步
    CLEAR LT_ROWS[].
    CALL METHOD GRID1->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS.
    IF LT_ROWS[] IS INITIAL."没有行被选中
      MESSAGE S009(ZFICO01) DISPLAY LIKE 'E'.
      "009  请选择要保存的行！
      P_ERR = 'X'.
      RETURN.
    ELSE.
      LOOP AT LT_ROWS INTO LW_ROWS.
        READ TABLE GT_ZFI005_EDIT INTO GW_ZFI005 INDEX LW_ROWS-INDEX.
        IF SY-SUBRC EQ 0.
          IF GW_ZFI005-XMBH IS NOT INITIAL AND GW_ZFI005-DEL_ICON EQ SPACE.
            SELECT SINGLE * FROM VBAK INTO W_VBAK WHERE VBELN EQ GW_ZFI005-XMBH.
            IF SY-SUBRC NE 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = GW_ZFI005-XMBH
                IMPORTING
                  OUTPUT = L_XMBH.
              MESSAGE S899(MM) WITH '销售订单号' L_XMBH '不存在' DISPLAY LIKE 'E'.
              P_ERR = 'X'.
              RETURN.
            ENDIF.
          ENDIF.
          PERFORM CHECK_INPUT_LINE CHANGING P_ERR.
          IF P_ERR EQ 'X'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    LOOP AT GT_ZFI005_EDIT INTO GW_ZFI005.
      PERFORM CHECK_INPUT_LINE CHANGING P_ERR.
      IF P_ERR EQ 'X'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IS_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_EDIT  text
*----------------------------------------------------------------------*
FORM IS_EDIT  CHANGING P_EDIT.
  CLEAR P_EDIT.
  READ TABLE GW_ZFI005-CELLSTYLE INTO LS_STYLEROW WITH KEY FIELDNAME = T_FIELDCAT-FIELDNAME.
  IF SY-SUBRC EQ 0.
    IF LS_STYLEROW-STYLE EQ CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      P_EDIT = 'X'.
    ENDIF.
  ELSE.
    IF T_FIELDCAT-EDIT = 'X'.
      P_EDIT = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ERR  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_LINE  CHANGING P_ERR.
  DATA: STR_FIELD TYPE CHAR30.
  FIELD-SYMBOLS: <FS_FIELD> TYPE ANY.
  DATA L_EDIT TYPE C.
  LOOP AT T_FIELDCAT.
    CONCATENATE 'GW_ZFI005-' T_FIELDCAT-FIELDNAME INTO STR_FIELD.
    CONDENSE STR_FIELD.
    ASSIGN (STR_FIELD) TO <FS_FIELD>.
    CLEAR L_EDIT.
    PERFORM IS_EDIT CHANGING L_EDIT.
    IF L_EDIT EQ 'X' AND <FS_FIELD> IS INITIAL.
      MESSAGE S899(MM) WITH '请输入' T_FIELDCAT-COLTEXT DISPLAY LIKE 'E'.
      P_ERR = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_INITIAL .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      LOG_NOT_ACTIVE       = 1
      WRONG_IDENTIFICATION = 2
      OTHERS               = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_REV_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_REV_REASON .
  CALL SCREEN 9002 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'STA9002'.
  "  WA_REVERSAL-PSTNG_DATE = SY-DATUM.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
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
        WA_REVERSAL-REASON_REV = G_STGRD.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: WA_REVERSAL-REASON_REV,
             WA_REVERSAL-PSTNG_DATE.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
