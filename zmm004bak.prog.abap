*&-----------------------------------------------------------------------------*
*& Report  ZMM004
*&
*& 参考销售订单或公司间采购订单进行调拨及进行调拨时的过账控制                 *
*& 进行库存移库调拨将物料从备料仓移到整备仓时，需要参考销售订单或公司间采购订单
*& 的带出行项目的物料和数量；物料不在销售订单或公司间采购订单范围内或者物料的累
*& 计移库数量大于对应订单数量，不允许过账；其余情况允许过账
*&
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
*& Date Created : 2015/1/24                                                   *
*& Created By   : 汉得-唐博                                                   *
*& Description  :参考销售订单进行调拨时的过账控制   *
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
REPORT ZMM004BAK.

"销售组织
PARAMETERS P_VKORG TYPE VBAK-VKORG OBLIGATORY.
"销售订单号
PARAMETERS P_VBELN TYPE VBAK-VBELN OBLIGATORY.
"工厂
PARAMETERS P_WERKS TYPE MSEG-WERKS OBLIGATORY.
"移出仓
PARAMETERS P_LGORTO TYPE MSEG-LGORT OBLIGATORY.
"移入仓
PARAMETERS P_LGORTI TYPE MSEG-LGORT OBLIGATORY.

"OO ALV 变量
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA GRID1  TYPE REF TO CL_GUI_ALV_GRID.
DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      W_LAYOUT    TYPE        LVC_S_LAYO,
      W_VARIANT   TYPE        DISVARIANT, "变式
      T_UIFUNC    TYPE        UI_FUNCTIONS,
      T_FIELDCAT TYPE        LVC_T_FCAT WITH HEADER LINE.
DATA GO_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
DATA: LW_UIFUNC TYPE UI_FUNC."工具栏标准按钮工作区
"设置稳定刷新
DATA G_IS_STABLE TYPE LVC_S_STBL.
DATA: OK_CODE TYPE CHAR20.
DATA: BEGIN OF GT_ALV OCCURS 0,
        VBELN TYPE VBAP-VBELN,"销售订单号
        POSNR TYPE VBAP-POSNR,"行项目号
        MATNR TYPE VBAP-MATNR,"物料
        CHARG TYPE VBAP-CHARG,"批次号
        KWMENG TYPE VBAP-KWMENG,"数量
        VRKME TYPE VBAP-VRKME,"单位
        MAKTX TYPE MAKT-MAKTX,"物料描述
        MENGE_MSEG TYPE VBAP-KWMENG,"可移库的数量
        MENGE_CHARG TYPE VBAP-KWMENG,"批次的可用数量
      END OF GT_ALV,
      G_SUCCESS TYPE C,
      GT_ZMM004 TYPE TABLE OF ZMM004 WITH HEADER LINE,
      GT_ZMM004_SUM TYPE TABLE OF ZMM004 WITH HEADER LINE,
      GT_MSEG TYPE TABLE OF MSEG WITH HEADER LINE.

START-OF-SELECTION.
  CLEAR: GT_ALV[].
  SELECT
    VBAP~VBELN
    VBAP~POSNR
    VBAP~MATNR
    VBAP~KWMENG
    VBAP~VRKME
    MAKT~MAKTX
    FROM VBAK INNER JOIN VBAP
    ON VBAK~VBELN EQ VBAP~VBELN
    LEFT JOIN MAKT ON VBAP~MATNR EQ MAKT~MATNR
    AND MAKT~SPRAS EQ SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    WHERE VBAP~VBELN EQ P_VBELN
    AND VBAK~VKORG EQ P_VKORG.
  IF GT_ALV[] IS INITIAL.
    MESSAGE S000(ZMM01) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
*   没有符合条件的记录。
  ENDIF.
  PERFORM LOCK_ZMM004 CHANGING G_SUCCESS.
  IF G_SUCCESS EQ 'X'.
    CLEAR GT_ZMM004[].
    SELECT * FROM ZMM004 INTO CORRESPONDING FIELDS OF TABLE GT_ZMM004
      WHERE VBELN EQ P_VBELN.
    SORT GT_ZMM004 BY VBELN POSNR.
    IF GT_ZMM004[] IS NOT INITIAL.
      SELECT * FROM MSEG INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
        FOR ALL ENTRIES IN GT_ZMM004
        WHERE SJAHR = GT_ZMM004-MJAHR
        AND SMBLN = GT_ZMM004-MBLNR
*        AND SMBLP = GT_ZMM004-ZEILE
        AND BWART = '312'.
        SORT GT_MSEG BY MBLNR SMBLN SMBLP.
        LOOP AT GT_ZMM004.
          READ TABLE GT_MSEG WITH KEY MBLNR = GT_ZMM004-MJAHR SMBLN = GT_ZMM004-MBLNR BINARY SEARCH.
          IF SY-SUBRC EQ 0."已经被冲销
            DELETE GT_ZMM004.
          ENDIF.
        ENDLOOP.
        IF GT_ZMM004[] IS NOT INITIAL.
*          CLEAR GT_MSEG[].
*          SELECT * FROM MSEG INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
*            FOR ALL ENTRIES IN GT_ZMM004
*            WHERE MJAHR = GT_ZMM004-MJAHR
*            AND MBLNR = GT_ZMM004-MBLNR
*            AND ZEILE = GT_ZMM004-ZEILE.
*            SORT GT_MSEG BY MJAHR MBLNR ZEILE.
*            LOOP AT GT_ZMM004.
*              READ TABLE GT_MSEG WITH KEY MJAHR = GT_ZMM004-MJAHR ZEILE = GT_ZMM004-ZEILE BINARY SEARCH.
*              IF SY-SUBRC EQ 0."获取数量
*                GT_ZMM004-MENGE = GT_MSEG-MENGE.
*                GT_ZMM004-MEINS = GT_MSEG-MEINS.
*                MODIFY GT_ZMM004.
*              ENDIF.
*            ENDLOOP.
            "按销售订单汇总
            CLEAR GT_ZMM004_SUM[].
            SORT GT_ZMM004 BY VBELN POSNR.
            LOOP AT GT_ZMM004.
              AT END OF POSNR.
                SUM.
                APPEND GT_ZMM004 TO GT_ZMM004_SUM.
              ENDAT.
            ENDLOOP.
        ENDIF.
    ENDIF.
    "获取已经过账的凭证
     LOOP AT GT_ALV.
        "获取已经移库的数量
        READ TABLE GT_ZMM004_SUM WITH KEY VBELN = GT_ALV-VBELN POSNR = GT_ALV-POSNR BINARY SEARCH.
        "计算出默认数量
        IF SY-SUBRC = 0.
          SUBTRACT GT_ZMM004_SUM-MENGE FROM GT_ALV-KWMENG.
          IF GT_ALV-KWMENG < 0.
            GT_ALV-KWMENG = 0.
          ENDIF.
        ENDIF.
        GT_ALV-MENGE_MSEG = GT_ALV-KWMENG.
       MODIFY GT_ALV.
     ENDLOOP.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ENDIF.

CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

  METHODS:
    HANDLE_TOOLBAR
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
ENDCLASS.

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD
    HANDLE_TOOLBAR.
    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'POST' TO LS_TOOLBAR-FUNCTION.
    MOVE '过账' TO LS_TOOLBAR-QUICKINFO.
    MOVE '过账' TO LS_TOOLBAR-TEXT.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.

  METHOD
    HANDLE_USER_COMMAND.
    CASE E_UCOMM.
      WHEN 'POST'.
        DATA L_GOODSMVT_HEADER TYPE BAPI2017_GM_HEAD_01.
        DATA L_GOODSMVT_CODE TYPE BAPI2017_GM_CODE.
        DATA L_GOODSMVT_HEADRET TYPE BAPI2017_GM_HEAD_RET.
        DATA L_MATERIALDOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC.
        DATA L_MATDOCUMENTYEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR.
        DATA LT_GOODSMVT_ITEM TYPE TABLE OF BAPI2017_GM_ITEM_CREATE.
        DATA LW_GOODSMVT_ITEM TYPE BAPI2017_GM_ITEM_CREATE.
        DATA LT_GOODSMVT_SERIALNUMBER TYPE TABLE OF BAPI2017_GM_SERIALNUMBER.
        DATA LT_RETURN TYPE TABLE OF BAPIRET2.
        DATA LW_RETURN TYPE BAPIRET2.
        DATA: L_SUBRC TYPE SY-SUBRC.
        DATA: L_MESSTAB TYPE TABLE OF BDCMSGCOLL.
        DATA: LW_MESSTAB TYPE BDCMSGCOLL.
        DATA IT_MESSAGE TYPE USMD_T_MESSAGE.
        DATA IW_MESSAGE LIKE LINE OF IT_MESSAGE.
        DATA L_LINE_NO TYPE I.
        DATA L_MAX TYPE VBAP-KWMENG.
        DATA L_CUR TYPE VBAP-KWMENG.
        CLEAR: L_GOODSMVT_HEADER,L_GOODSMVT_CODE,L_GOODSMVT_HEADRET,L_MATERIALDOCUMENT,L_MATDOCUMENTYEAR,LT_GOODSMVT_ITEM[],LT_RETURN[].
        PERFORM KWMENG_CHECK CHANGING L_SUBRC L_LINE_NO L_MAX L_CUR.
        IF L_SUBRC NE 0.
          MESSAGE S899(MM) WITH L_LINE_NO '行数量' L_CUR '超出可移动数量，请检查！' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        CLEAR L_SUBRC.
        "311,一步法移库
        L_GOODSMVT_HEADER-PSTNG_DATE = SY-DATUM.
        L_GOODSMVT_HEADER-DOC_DATE = SY-DATUM.
        L_GOODSMVT_HEADER-PR_UNAME = SY-UNAME.
*       GMCODE Table T158G
*       01 - MB01 - Goods Receipts for Purchase Order
*       02 - MB31 - Goods Receipts for Prod Order
*       03 - MB1A - Goods Issue
*       04 - MB1B - Transfer Posting
*       05 - MB1C - Enter Other Goods Receipt
*       06 - MB11
        L_GOODSMVT_CODE-GM_CODE = '04'.
        CLEAR LT_GOODSMVT_ITEM[].
        LOOP AT GT_ALV INTO GT_ALV .
          LW_GOODSMVT_ITEM-MATERIAL = GT_ALV-MATNR."物料号
*          LT_GOODSMVT_ITEM-EXPIRYDATE = GT_QRDATA-DATE_SX."失效日期
*          LT_GOODSMVT_ITEM-PROD_DATE = GT_QRDATA-PRDDATE."生产日期
          LW_GOODSMVT_ITEM-STGE_LOC = P_LGORTO."移出仓
          LW_GOODSMVT_ITEM-PLANT = P_WERKS."工厂
          LW_GOODSMVT_ITEM-MOVE_PLANT = P_WERKS.
          LW_GOODSMVT_ITEM-MOVE_STLOC = P_LGORTI."移入仓
          LW_GOODSMVT_ITEM-BATCH = GT_ALV-CHARG."批次号
          LW_GOODSMVT_ITEM-MOVE_TYPE = '311'."移动类型
          LW_GOODSMVT_ITEM-ENTRY_QNT = GT_ALV-KWMENG."数量
          LW_GOODSMVT_ITEM-ENTRY_UOM = GT_ALV-VRKME."单位
          APPEND LW_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM.
        ENDLOOP.
        "过账
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            GOODSMVT_HEADER               = L_GOODSMVT_HEADER
            GOODSMVT_CODE                 = L_GOODSMVT_CODE
*           TESTRUN                       = ' '
*           GOODSMVT_REF_EWM              =
          IMPORTING
            GOODSMVT_HEADRET              = L_GOODSMVT_HEADRET
            MATERIALDOCUMENT              = L_MATERIALDOCUMENT
            MATDOCUMENTYEAR               = L_MATDOCUMENTYEAR
          TABLES
            GOODSMVT_ITEM                 = LT_GOODSMVT_ITEM
*           GOODSMVT_SERIALNUMBER         =
            RETURN                        = LT_RETURN
*           GOODSMVT_SERV_PART_DATA       =
*           EXTENSIONIN                   =
                  .
        IF SY-SUBRC EQ 0.
*          CLEAR G_MESSAGE.
          CLEAR IT_MESSAGE[].
          LOOP AT LT_RETURN INTO LW_RETURN WHERE TYPE EQ 'E' OR TYPE EQ 'A'.
*            IF G_MESSAGE IS INITIAL.
*              CONCATENATE LW_RETURN-ID LW_RETURN-NUMBER LW_RETURN-MESSAGE INTO G_MESSAGE.
*            ELSE.
*              CONCATENATE G_MESSAGE LW_RETURN-ID LW_RETURN-NUMBER LW_RETURN-MESSAGE INTO G_MESSAGE SEPARATED BY SPACE.
*            ENDIF.
            IW_MESSAGE-MSGID = LW_RETURN-ID.
            IW_MESSAGE-MSGTY = LW_RETURN-TYPE.
            IW_MESSAGE-MSGNO = LW_RETURN-NUMBER.
            IW_MESSAGE-MSGV1 = LW_RETURN-MESSAGE_V1.
            IW_MESSAGE-MSGV2 = LW_RETURN-MESSAGE_V2.
            IW_MESSAGE-MSGV3 = LW_RETURN-MESSAGE_V3.
            IW_MESSAGE-MSGV4 = LW_RETURN-MESSAGE_V4.
            APPEND IW_MESSAGE TO IT_MESSAGE.
            CLEAR IW_MESSAGE.
          ENDLOOP.
          IF IT_MESSAGE IS NOT INITIAL."错误或终止
            CALL FUNCTION 'USMD_MESSAGE_POPUP'
                EXPORTING
                  IT_MESSAGE              = IT_MESSAGE
                  IF_SAVE_NECESSARY       = SPACE
*               IMPORTING
*                 EF_CONTINUE             =
                        .
          ELSE.
            IF L_MATERIALDOCUMENT IS NOT INITIAL.
              MESSAGE S899(MM) WITH '物料凭证' L_MATERIALDOCUMENT '已成功保存！'.
              "调用BAPI_TRANSACTION_COMMIT
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*               EXPORTING
*                 WAIT          =
*               IMPORTING
*                 RETURN        =
                        .
              "显示物料凭证XXXX成功保存
*              CONCATENATE '物料凭证' L_MATERIALDOCUMENT '已成功保存！' INTO G_MESSAGE.
              PERFORM UPDATE_DB USING L_MATDOCUMENTYEAR L_MATERIALDOCUMENT.
            ELSE.
              MESSAGE S899(MM) WITH '过账失败！' DISPLAY LIKE 'E'.
*              MOVE '过账失败！' TO G_MESSAGE.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE S899(MM) WITH '过账失败！' DISPLAY LIKE 'E'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD
    HANDLE_DATA_CHANGED.
  ENDMETHOD.

  METHOD
    HANDLE_DATA_CHANGED_FINISHED.
    DATA L_MOFIFY_FLAG VALUE ''.
    CLEAR L_MOFIFY_FLAG.
    IF E_MODIFIED EQ 'X'.
*      LOOP AT GT_ALV INTO GT_ALV.
*        IF GT_ALV-CHARG IS NOT INITIAL.
*          "获取批次可用库存
*          PERFORM GET_STORAGE USING P_WERKS P_LGORTO GT_ALV-CHARG CHANGING GT_ALV-MENGE_CHARG.
*          IF GT_ALV-MENGE_CHARG IS INITIAL.
*            MESSAGE S899(MM) WITH '批次' GT_ALV-CHARG '无可用库存！' DISPLAY LIKE 'E'.
*            RETURN.
*          ELSEIF GT_ALV-KWMENG > GT_ALV-MENGE_CHARG.
*             GT_ALV-KWMENG = GT_ALV-MENGE_CHARG.
*             MODIFY GT_ALV FROM GT_ALV.
*             L_MOFIFY_FLAG = 'X'.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
      IF L_MOFIFY_FLAG EQ 'X'.
        "刷新ALV
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING IS_STABLE = G_IS_STABLE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM LOCK_ZMM004 CHANGING P_SUCCESS TYPE C.
  P_SUCCESS = SPACE.
  CALL FUNCTION 'ENQUEUE_EZMM004'
   EXPORTING
*     MODE_ZFI005          = 'E'
     MANDT                = SY-MANDT
     VBELN                = P_VBELN
*     X_BUKRS              = ' '
     _SCOPE               = '3'
*     _WAIT                = ' '
*     _COLLECT             = ' '
   EXCEPTIONS
     FOREIGN_LOCK         = 1
     SYSTEM_FAILURE       = 2
     OTHERS               = 3
            .
  IF SY-SUBRC NE 0.
    P_SUCCESS = SPACE.
  ELSE.
    P_SUCCESS = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
  IF GRID1 IS INITIAL.
    CLEAR: T_UIFUNC[],T_FIELDCAT[].
    PERFORM FRM_EXCLUDE_BUTTON.
    PERFORM FRM_FIELDCAT.
    CREATE OBJECT GRID1
    EXPORTING I_PARENT = CL_GUI_CONTAINER=>DEFAULT_SCREEN.
    CREATE OBJECT GO_RECEIVER.
    SET HANDLER GO_RECEIVER->HANDLE_TOOLBAR FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_USER_COMMAND FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_DATA_CHANGED FOR GRID1.
    SET HANDLER GO_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR GRID1.
    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER."回车触发编辑事件
    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED."失去焦点触发编辑事件
  ENDIF.
   "显示数据
   W_LAYOUT-ZEBRA = 'X'.
   CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING
*       IS_VARIANT                    = W_VARIANT
       I_SAVE                        = 'A'
       IS_LAYOUT                     = W_LAYOUT
       IT_TOOLBAR_EXCLUDING          = T_UIFUNC
     CHANGING
       IT_OUTTAB                     = GT_ALV[]
       IT_FIELDCATALOG               = T_FIELDCAT[]
     EXCEPTIONS
       INVALID_PARAMETER_COMBINATION = 1
       PROGRAM_ERROR                 = 2
       TOO_MANY_LINES                = 3
       OTHERS                        = 4.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE OK_CODE.
    WHEN '&DATA_SAVE'.
*      PERFORM SAVE_DATA.
    WHEN '&F03' OR '&F15'."返回
       PERFORM UNLOCK_ZMM004.
      LEAVE TO SCREEN 0.
    WHEN '&F12'."退出
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
FORM UNLOCK_ZMM004.
  CALL FUNCTION 'DEQUEUE_EZMM004'
     EXPORTING
*       MODE_ZFI005       = 'E'
       MANDT             = SY-MANDT
       VBELN             = P_VBELN
*       X_BUKRS           = ' '
       _SCOPE            = '3'
*       _SYNCHRON         = ' '
*       _COLLECT          = ' '
            .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_exclude_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_exclude_button .
  REFRESH T_UIFUNC.
  PERFORM APPEND_EXCLUDE USING:
        CL_GUI_ALV_GRID=>MC_MB_PASTE,"剪贴按钮
        CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,"附加行按钮
        CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,"插入行按钮
        CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,"删除行按钮
        CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW, "复制行按钮
        CL_GUI_ALV_GRID=>MC_FC_LOC_CUT."剪切行按钮
ENDFORM. " FRM_EXCLUDE_BUTTON
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
          'VBELN' '销售订单号' '' '' 'VBAK' 'VBELN',
          'POSNR' '行项目号'   '' '' 'VBAP' 'POSNR',
          'MATNR' '物料号'     '' '' 'VBAP' 'MATNR',
          'KWMENG' '数量' 'VRKME' 'X' 'VBAP' 'KWMENG',
          'VRKME' '单位' '' '' 'VBAP' 'VRKME',
          'CHARG' '批次号'     '' 'X' 'MSEG' 'CHARG',
          'MAKTX' '物料描述' '' '' 'MAKT' 'MAKTX'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_FIELDCAT USING FIELDNAME TEXT QFIELDNAME EDIT REF_TABLE REF_FIELD.
  T_FIELDCAT-FIELDNAME = FIELDNAME.
  T_FIELDCAT-REF_FIELD = REF_FIELD.
  T_FIELDCAT-REF_TABLE = REF_TABLE.
  T_FIELDCAT-COLTEXT = TEXT.
  T_FIELDCAT-REPTEXT = TEXT.
  T_FIELDCAT-SCRTEXT_L = TEXT.
  T_FIELDCAT-SCRTEXT_M = TEXT.
  T_FIELDCAT-SCRTEXT_S = TEXT.
  T_FIELDCAT-QFIELDNAME = QFIELDNAME.
  T_FIELDCAT-EDIT = EDIT.
  T_FIELDCAT-CHECKTABLE = '!'.
  APPEND T_FIELDCAT.
  CLEAR T_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CL_GUI_ALV_GRID=>MC_MB_PASTE  text
*----------------------------------------------------------------------*
FORM APPEND_EXCLUDE  USING  P_MC_MB.
   CLEAR LW_UIFUNC.
   LW_UIFUNC = P_MC_MB.
   APPEND LW_UIFUNC TO T_UIFUNC.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_DB USING P_MATDOCUMENTYEAR P_MATERIALDOCUMENT.
  CLEAR GT_ZMM004[].
  LOOP AT GT_ALV.
    GT_ZMM004-VBELN = GT_ALV-VBELN.
    GT_ZMM004-POSNR = GT_ALV-POSNR.
    GT_ZMM004-MBLNR = P_MATERIALDOCUMENT.
    GT_ZMM004-MJAHR = P_MATDOCUMENTYEAR.
*    GT_ZMM004-ZEILE = GT_ALV-POSNR.
    GT_ZMM004-MENGE = GT_ALV-KWMENG.
    GT_ZMM004-MEINS = GT_ALV-VRKME.
    GT_ZMM004-BUDAT_MKPF = SY-DATUM.
    APPEND GT_ZMM004.
  ENDLOOP.
  MODIFY ZMM004 FROM TABLE GT_ZMM004.
  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  KWMENG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM KWMENG_CHECK
  CHANGING P_SUBRC P_LINE_NO P_MAX P_CUR.
  CLEAR: P_SUBRC,P_LINE_NO,P_MAX,P_CUR.
  LOOP AT GT_ALV.
    P_LINE_NO = SY-TABIX.
    P_CUR = GT_ALV-KWMENG.
    PERFORM KWMENG_MAX CHANGING P_MAX.
    PERFORM KWMENG_CHECK_LINE CHANGING P_SUBRC P_MAX P_CUR.
    IF P_SUBRC NE 0.
      RETURN.
    ENDIF.
    CLEAR: P_SUBRC,P_LINE_NO,P_MAX,P_CUR.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  KWMENG_CHECK_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SUBRC  text
*      <--P_MAX  text
*      <--P_CUR  text
*----------------------------------------------------------------------*
FORM KWMENG_CHECK_LINE  CHANGING P_SUBRC
                                 P_MAX
                                 P_CUR.
  CLEAR P_SUBRC.
  IF P_CUR > P_MAX. "如果当前数量大于最大可用数量，则返回错误
    P_SUBRC = 4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  KWMENG_MAX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MAX  text
*----------------------------------------------------------------------*
FORM KWMENG_MAX  CHANGING P_MAX.
  DATA: L_MENGE_SO TYPE VBAP-KWMENG.
  DATA: L_MENGE_MSEG TYPE VBAP-KWMENG.
  DATA: L_MENGE_CHARG TYPE VBAP-KWMENG.
  P_MAX = GT_ALV-MENGE_MSEG.
  "获取销售订单的数量
*  SELECT SINGLE KWMENG FROM VBAP INTO L_MENGE_SO WHERE VBELN EQ GT_ALV-VBELN AND POSNR EQ GT_ALV-POSNR.
  "获取已经移库的数量
*  SELECT * FROM ZMM004 INTO CORRESPONDING FIELDS OF GT_ZMM004

  "获取批次的数量
  "销售订单的数量和已经移库的数量相减计算出最大可移库数量
  "最大可移库数量和批次的数量，取两者最小值
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  GET_STORAGE
**&---------------------------------------------------------------------*
**       获取批次可用库存
**----------------------------------------------------------------------*
**      -->P_WERKS  text
**      -->P_LGORTO  text
**      -->PT_ALV_CHARG  text
**      <--PT_ALV_MENGE_CHARG  text
**----------------------------------------------------------------------*
*FORM GET_STORAGE  USING    P_WERKS
*                           P_LGORTO
*                           PT_ALV_CHARG
*                  CHANGING PT_ALV_MENGE_CHARG.
*
*ENDFORM.
