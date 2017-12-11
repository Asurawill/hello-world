*&---------------------------------------------------------------------*
*&  包括                ZPP005_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  包含                ZPP005_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  S_DWERK-LOW.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH S_DWERK-LOW.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_HEAD .

  SELECT AFPO~AUFNR
    AFPO~MATNR AS MATNR_HEAD " 物料
    AFPO~DWERK " 工厂
    AFPO~PSMNG " 订单数量
    AFPO~WEMNG " 入库数量
    AFKO~IGMNG " 报工数量
    AFKO~FEVOR " 生产管理员
    AFKO~DISPO " MRP控制者
    AFPO~DAUAT " 订单类型
    AFPO~KDAUF " 销售订单号
    AFPO~KDPOS " 销售订单行号
    AFKO~FTRMI " 下达日期
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV1
    FROM AFKO INNER JOIN AFPO
    ON AFKO~AUFNR = AFPO~AUFNR
    INNER JOIN AUFK
    ON AFKO~AUFNR = AUFK~AUFNR
    WHERE AFKO~AUFNR IN S_AUFNR
    AND AFPO~DWERK   IN S_DWERK
    AND AFPO~MATNR   IN S_MATNR
    AND DISPO        IN S_DISPO
    AND FEVOR        IN S_FEVOR
    AND DAUAT        IN S_DAUAT
    AND FTRMI        IN S_FTRMI
    AND AUFK~AUTYP   = '10'. " 订单类别10，生产订单

  DATA:LT_AUFK TYPE STANDARD TABLE OF AUFK WITH HEADER LINE.
  DATA:LT_MAKT_HEAD TYPE STANDARD TABLE OF MAKT WITH HEADER LINE.
  DATA:LT_T024F TYPE STANDARD TABLE OF T024F WITH HEADER LINE.
  DATA:LT_T024D TYPE STANDARD TABLE OF T024D WITH HEADER LINE.


  CLEAR:LT_AUFK,LT_AUFK[].
  CLEAR:LT_MAKT_HEAD,LT_MAKT_HEAD[].
  CLEAR:LT_T024F,LT_T024F[].
  CLEAR:LT_T024D,LT_T024D[].

  IF GT_ALV1[] IS NOT INITIAL.
    SELECT *
    FROM AUFK
    INTO CORRESPONDING FIELDS OF TABLE LT_AUFK
    FOR ALL ENTRIES IN GT_ALV1
    WHERE AUFNR = GT_ALV1-AUFNR.

    SELECT *
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT_HEAD
      FOR ALL ENTRIES IN  GT_ALV1
      WHERE MATNR = GT_ALV1-MATNR_HEAD
      AND SPRAS = SY-LANGU.

*MRP控制者描述
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_T024D
      FROM T024D
      FOR ALL ENTRIES IN GT_ALV1
      WHERE DISPO = GT_ALV1-DISPO.

*生产管理员描述
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_T024F
      FROM T024F
      FOR ALL ENTRIES IN GT_ALV1
      WHERE FEVOR = GT_ALV1-FEVOR.

  ENDIF.


  LOOP AT GT_ALV1.
    READ TABLE LT_AUFK WITH KEY AUFNR = GT_ALV1-AUFNR.
    IF SY-SUBRC = 0.
      GT_ALV1-KTEXT = LT_AUFK-KTEXT. " 生产订单描述
      GT_ALV1-OBJNR = LT_AUFK-OBJNR." 对象号
    ENDIF.

    READ TABLE LT_MAKT_HEAD WITH KEY MATNR = GT_ALV1-MATNR_HEAD.
    IF SY-SUBRC = 0.
      GT_ALV1-MAKTX_HEAD = LT_MAKT_HEAD-MAKTX. " 物料描述
    ENDIF.

*MRP控制者描述
    READ TABLE LT_T024D WITH KEY DISPO = GT_ALV1-DISPO.
    IF SY-SUBRC = 0.
      GT_ALV1-DSNAM = LT_T024D-DSNAM.
    ENDIF.

*生产管理员描述
    READ TABLE LT_T024F WITH KEY FEVOR = GT_ALV1-FEVOR.
    IF SY-SUBRC = 0.
      GT_ALV1-TXT = LT_T024F-TXT.
    ENDIF.


    MODIFY GT_ALV1.
  ENDLOOP .

*排除状态关闭的订单
  DATA:LT_JEST TYPE STANDARD TABLE OF JEST WITH HEADER LINE .
  CLEAR:LT_JEST,LT_JEST[].

*根据选择屏幕
  SELECT *
    FROM JEST
    INTO CORRESPONDING FIELDS OF TABLE LT_JEST
    WHERE INACT <> 'X'  " 激活
*    AND STAT IN ('I0045' , 'I0046','I0076','I0013')
    AND STAT IN S_ISTAT.

  LOOP AT  GT_ALV1." 排除状态不符合要求的记录
    READ TABLE LT_JEST WITH KEY OBJNR = GT_ALV1-OBJNR.
    IF SY-SUBRC <> 0.
      DELETE GT_ALV1.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_HANDLE_TOOLBAR  USING    P_E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                  P_E_INTERACTIVE.

  DATA:LS_TOOLBAR TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.
  MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  MOVE ICON_SELECT_ALL TO LS_TOOLBAR-ICON.
  MOVE 'ALL' TO LS_TOOLBAR-FUNCTION.
  MOVE '全选' TO LS_TOOLBAR-QUICKINFO.
  MOVE '全选' TO LS_TOOLBAR-TEXT.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  MOVE ICON_DESELECT_ALL TO LS_TOOLBAR-ICON.
  MOVE 'DEALL' TO LS_TOOLBAR-FUNCTION.
  MOVE '反选' TO LS_TOOLBAR-QUICKINFO.
  MOVE '反选' TO LS_TOOLBAR-TEXT.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

*  CLEAR LS_TOOLBAR.
*  MOVE ICON_EXECUTE_OBJECT TO LS_TOOLBAR-ICON.
*  MOVE 'COM_TECH' TO LS_TOOLBAR-FUNCTION.
*  MOVE '批量关闭' TO LS_TOOLBAR-QUICKINFO.
*  MOVE '批量关闭' TO LS_TOOLBAR-TEXT.
*  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  MOVE ICON_SELECT_DETAIL TO LS_TOOLBAR-ICON.
  MOVE 'DETAIL' TO LS_TOOLBAR-FUNCTION.
  MOVE '查看组件' TO LS_TOOLBAR-QUICKINFO.
  MOVE '查看组件' TO LS_TOOLBAR-TEXT.
  APPEND LS_TOOLBAR TO P_E_OBJECT->MT_TOOLBAR.


ENDFORM.                    " FRM_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_HANDLE_USER_COMMAND  USING   P_E_UCOMM.
  CASE P_E_UCOMM.
    WHEN 'ALL'.
      LOOP AT GT_ALV1.
        GT_ALV1-BOX = 'X'.
        MODIFY GT_ALV1.
      ENDLOOP.
      PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV." 刷新ALV

    WHEN 'DEALL'. " 反选
      LOOP AT GT_ALV1.
        GT_ALV1-BOX = ''.
        MODIFY GT_ALV1.
      ENDLOOP.
      PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV." 刷新ALV

    WHEN 'COM_TECH'. " 技术完成
      PERFORM FRM_CHECK_BEFORE_BDC. " 检查数据
      PERFORM FRM_BDC_CO02. " 调用BDC,技术性完成
      PERFORM FRM_REFRESH_ALV CHANGING GCL_ALV." 刷新ALV

    WHEN 'DETAIL'.
      PERFORM FRM_GET_9002_DATA.
      CALL SCREEN 9002.

  ENDCASE.

ENDFORM.                    " FRM_HANDLE_USER_COMMAND

FORM FRM_REFRESH_ALV  CHANGING PCL_ALV TYPE REF TO CL_GUI_ALV_GRID.
*刷新稳定性
  DATA: LW_STBL TYPE LVC_S_STBL.
  LW_STBL-ROW = 'X'.
  LW_STBL-COL = 'X'.
  CALL METHOD PCL_ALV->CHECK_CHANGED_DATA.
  CALL METHOD PCL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LW_STBL
*     I_SOFT_REFRESH =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER  USING    P_CONTAINER_NAME
                                    P_DYNGR
                           CHANGING PCL_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER
                                    PCL_ALV TYPE REF TO CL_GUI_ALV_GRID.

  " 自适应窗口容器
  CREATE OBJECT PCL_CONTAINER
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = P_DYNGR   " '9001' 屏幕号
      EXTENSION = 2050
      SIDE      = CL_GUI_DOCKING_CONTAINER=>PROPERTY_FLOATING.

  CREATE OBJECT PCL_ALV
    EXPORTING
      I_PARENT = PCL_CONTAINER.


ENDFORM.                    " FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  DATA LS_EXCLUDE TYPE UI_FUNC.
  CLEAR GS_EXCLUDE.
*排除打印按钮
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PRINT.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
*排除添加、删除、插入、复制按钮
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW .
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW .
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW .
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW .
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
*排除剪切、粘贴按钮、刷新
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO. " 撤销
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW." 插入新行
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PC_FILE.
  APPEND LS_EXCLUDE TO GS_EXCLUDE.

ENDFORM.                    " FRM_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_PRE_LAYOUT  USING    P_TITLE
                              P_TOOL
                     CHANGING PS_LAYOUT TYPE LVC_S_LAYO.

  PS_LAYOUT-ZEBRA = 'X'.
  PS_LAYOUT-NO_TOOLBAR = P_TOOL.
  PS_LAYOUT-CWIDTH_OPT = 'X'.
  PS_LAYOUT-GRID_TITLE = P_TITLE.
*   PS_LAYOUT-BOX_FNAME = 'BOX'. " 选择框字段

ENDFORM.                    " FRM_PRE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRE_FIELDCAT .
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'BOX' '选择框' '' 'X' '' 'X' '' '' ''  ''.
*  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'CHECK' '检查结果' '' '' '' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'AUFNR' '生产订单' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'MATNR_HEAD' '物料' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'MAKTX_HEAD' '物料描述' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'KTEXT' '订单描述' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'DWERK' '工厂' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'DISPO' 'MRP控制者' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'DSNAM' '描述' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'FEVOR' '生产管理员' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'TXT' '描述' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'PSMNG' '订单数量' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'WEMNG' '入库数量' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'IGMNG' '报工数量' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'KDAUF' '销售订单号' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'KDPOS' '销售订单行号' '' '' 'X' '' '' '' '' ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT USING 'FTRMI' '下达日期' '' '' 'X' '' '' '' '' ''.

ENDFORM.                    " FRM_PRE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT  TABLES   PT_FIELDCAT
                   USING    P_FIELDNAME
                            P_COLTEXT
                            P_KEY
                            P_EDIT
                            P_NOZERO
                            P_CHECKBOX
                            P_REF_TABLE
                            P_REF_FIELD
                            P_OUTPUTLEN
                            P_HOTSPOT.
  DATA W_FCAT TYPE LVC_S_FCAT.
  W_FCAT-FIELDNAME = P_FIELDNAME. " 字段
  W_FCAT-COLTEXT = P_COLTEXT. " 字段描述
  W_FCAT-KEY = P_KEY.
  W_FCAT-EDIT = P_EDIT.
  W_FCAT-NO_ZERO = P_NOZERO.
  W_FCAT-CHECKBOX = P_CHECKBOX.
  W_FCAT-REF_TABLE = P_REF_TABLE.
  W_FCAT-REF_FIELD = P_REF_FIELD.
  W_FCAT-OUTPUTLEN = P_OUTPUTLEN.
  W_FCAT-HOTSPOT = P_HOTSPOT.
  APPEND W_FCAT TO PT_FIELDCAT.

ENDFORM.                    " FRM_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GCL_ALV  text
*----------------------------------------------------------------------*
FORM FRM_UPLOAD_EVENT  CHANGING  PCL_ALV TYPE REF TO CL_GUI_ALV_GRID.

  DATA LR_EVENT_HANDLER TYPE REF TO LCL_EVENT_RECEIVER.

  CREATE OBJECT LR_EVENT_HANDLER. " 注册双击事件

  SET HANDLER LR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK FOR PCL_ALV.

  SET HANDLER LR_EVENT_HANDLER->HANDLE_TOOLBAR FOR PCL_ALV.

  SET HANDLER LR_EVENT_HANDLER->HANDLE_USER_COMMAND FOR PCL_ALV.

ENDFORM.                    " FRM_UPLOAD_EVENT
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_TAB_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_TAB_DISPLAY .

  DATA:GV_ALV_VARIANT LIKE DISVARIANT.

  GV_ALV_VARIANT-REPORT = SY-REPID.

  CALL METHOD GCL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE                        = 'X'
      I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GS_EXCLUDE
      IS_VARIANT                    = GV_ALV_VARIANT " 保存变式
    CHANGING
      IT_OUTTAB                     = GT_ALV1[]
      IT_FIELDCATALOG               = GT_FIELDCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_SET_TAB_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_CO02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BDC_CO02 .

  DATA:  C_SUBRC LIKE SY-SUBRC.
  " A运行模式(全部显示)
  " E运行模式(只显示错误)
  " N运行模式(后台运行)
  " S更新方式(同步)
  LOOP AT GT_ALV1 WHERE BOX = 'X'.
    PERFORM FRM_BDC_DATA USING: 'SAPLCOKO1' '0110' 'X' '' '',
                                  '' '' '' 'BDC_CURSOR' 'CAUFVD-AUFNR',
                                  '' '' '' 'BDC_OKCODE' '/00',
                                  '' '' '' 'CAUFVD-AUFNR' GT_ALV1-AUFNR,
                                  '' '' '' 'R62CLORD-FLG_OVIEW' 'X'.

    PERFORM FRM_BDC_DATA USING: 'SAPLCOKO1' '0115' 'X' '' '',
                                  '' '' '' 'BDC_OKCODE' '=TABS',
                                  '' '' '' 'BDC_SUBSCR' 'SAPLCOKO1                               0120SUBSCR_0115',
                                  '' '' '' 'BDC_CURSOR' 'CAUFVD-GAMNG'.

    PERFORM FRM_BDC_DATA USING: 'SAPLCOKO1' '0115' 'X' '' '',
                                  '' '' '' 'BDC_OKCODE' '=BU'.
    .


    PERFORM CALL_TRANSACTION USING 'CO02'  'N' '' '' CHANGING C_SUBRC.
    CLEAR:GT_BDCDATA,GW_BDCDATA.

    DELETE GT_ALV1. " 删除内表对应记录

  ENDLOOP.
ENDFORM.                    " FRM_BDC_CO02

FORM FRM_BDC_DATA  USING   P_PROGRAM
                     P_DYNPRO
                     P_DYNBEGIN
                     P_FNAM
                     P_FVAL.
*  DATA:l_fval(13) TYPE c. " 临时转换变量

  CLEAR GW_BDCDATA.
  GW_BDCDATA-PROGRAM =  P_PROGRAM.
  GW_BDCDATA-DYNPRO =  P_DYNPRO.
  GW_BDCDATA-DYNBEGIN = P_DYNBEGIN.
  GW_BDCDATA-FNAM = P_FNAM.
  GW_BDCDATA-FVAL = P_FVAL.

  " 金额字段做处理
  CONDENSE GW_BDCDATA-FVAL.

  APPEND GW_BDCDATA TO GT_BDCDATA.

ENDFORM.                    " FRM_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION USING   I_TCODE  TYPE SY-TCODE
                               I_MODE   TYPE C
                               I_RECNO  TYPE I
                               I_MSGTYP TYPE C
                      CHANGING O_SUBRC  TYPE SY-SUBRC.

  DATA OPT TYPE CTU_PARAMS.

  DATA : L_MESSAGE LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA:L_FLAG." 标识位

  CALL TRANSACTION I_TCODE USING GT_BDCDATA
*                            OPTIONS FROM opt
                          MODE I_MODE
                           UPDATE  'S' " 更新方式(同步)
                 MESSAGES INTO L_MESSAGE.
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

  CLEAR:GT_RETURN,GT_RETURN[].
  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
    TABLES
      IMT_BDCMSGCOLL = L_MESSAGE[]
      EXT_RETURN     = GT_RETURN.

ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRE_FIELDCAT2 .
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'AUFNR' '生产订单' '' '' 'X' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'MATNR_HEAD' '物料' '' '' 'X' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'MAKTX_HEAD' '物料描述' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'KTEXT' '订单描述' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'PSMNG' '订单数量' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'WEMNG' '入库数量' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'MATNR_ITEM' '组件' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'MAKTX_ITEM' '组件描述' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'BDMNG' '需求数量' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'RKBL' '组件入库数量' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'ENMNG' '计划内发料' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'ERFMG' '计划外发料' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'JHWMNG' '总计消耗' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'ZFLQL' '发料欠料' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'LLYLSL' '理论用料数量' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'ZZXS' '在线数' '' '' '' '' '' '' ''  ''.
  PERFORM FRM_FIELDCAT TABLES GT_FIELDCAT2 USING 'XLOEK' '删除标记' '' '' '' '' '' '' ''  ''.

*        zflql      TYPE resb-bdmng, " 发料欠料 = 需求数量 - 计划内发料
*        zzxs       TYPE resb-bdmng, " 在线数 = 总计消耗 - 入库数量

ENDFORM.                    " FRM_PRE_FIELDCAT2
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_TAB_DISPLAY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_TAB_DISPLAY2 .
  CALL METHOD GCL_ALV2->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE                        = 'A'
      I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT2
      IT_TOOLBAR_EXCLUDING          = GS_EXCLUDE
    CHANGING
      IT_OUTTAB                     = GT_ALV3[]
      IT_FIELDCATALOG               = GT_FIELDCAT2
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_SET_TAB_DISPLAY2
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA_ITEM .

  CLEAR:GT_ALV2,GT_ALV2[]. " 清空组件内表

  DATA:LT_ALV2 TYPE STANDARD TABLE OF TY_ALV2 WITH HEADER LINE.
  CLEAR:LT_ALV2,LT_ALV2[].

  LOOP AT GT_ALV1.
    LT_ALV2-AUFNR = GT_ALV1-AUFNR.
    LT_ALV2-MATNR_HEAD = GT_ALV1-MATNR_HEAD.
    LT_ALV2-MAKTX_HEAD = GT_ALV1-MAKTX_HEAD.
    LT_ALV2-PSMNG = GT_ALV1-PSMNG. " 订单数量
    LT_ALV2-WEMNG = GT_ALV1-WEMNG.  "入库数量
    LT_ALV2-KTEXT = GT_ALV1-KTEXT. " 订单描述
    APPEND LT_ALV2.
  ENDLOOP.

  DATA:LT_RESB TYPE STANDARD TABLE OF RESB WITH HEADER LINE.
  CLEAR:LT_RESB,LT_RESB[].

  IF LT_ALV2[] IS NOT INITIAL.
    SELECT
      AUFNR " 订单号
      RSNUM
      RSPOS
      MATNR " 物料
      WERKS " 工厂
      BDMNG "  需求数量
      ENMNG "  计划消耗
      MEINS " 单位
      SHKZG " 借贷标识
      XLOEK "删除标记
      FROM RESB
      INTO CORRESPONDING FIELDS OF TABLE LT_RESB
      FOR ALL ENTRIES IN LT_ALV2
      WHERE RESB~AUFNR = LT_ALV2-AUFNR
      AND BWART IN ('261','262','531','532')
      AND SCHGT <> 'X' " 散装物料
      AND DUMPS <> 'X' ." 虚拟项目
*      AND XLOEK <> 'X'. " 排除已删除项目

    DATA:IT_AUFNR TYPE TABLE OF RESB WITH HEADER LINE.
    CLEAR:IT_AUFNR,IT_AUFNR[].

*  SELECT
*      AUFNR " 订单号
*      RSNUM
*      RSPOS
*      MATNR " 物料
*      WERKS " 工厂
*      BDMNG "  需求数量
*      ENMNG "  计划消耗
*      MEINS " 单位
*      SHKZG " 借贷标识
*    FROM RESB
*    INTO CORRESPONDING FIELDS OF TABLE IT_AUFNR                "取出工厂是1100，但是是已经删除的订单号
*    FOR ALL ENTRIES IN LT_ALV2
*    WHERE RESB~AUFNR = LT_ALV2-AUFNR
*    AND BWART IN ('261','262','531','532')
*    AND SCHGT <> 'X' " 散装物料
*    AND DUMPS <> 'X' " 虚拟项目
*    AND WERKS = 1100
*    AND XLOEK = 'X'.

  ENDIF.


*  LOOP AT LT_RESB.
*    READ TABLE IT_AUFNR INDEX SY-TABIX.
*    IF SY-SUBRC = 0.
*      LT_RESB-AUFNR = IT_AUFNR-AUFNR.
*      LT_RESB-RSNUM = IT_AUFNR-RSNUM.
*      LT_RESB-RSPOS = IT_AUFNR-RSPOS.
*      LT_RESB-MATNR = IT_AUFNR-MATNR. " 物料
*      LT_RESB-WERKS = IT_AUFNR-WERKS. " 工厂
*      LT_RESB-BDMNG = IT_AUFNR-BDMNG. "  需求数量
*      LT_RESB-ENMNG = IT_AUFNR-ENMNG. "  计划消耗
*      LT_RESB-MEINS = IT_AUFNR-MEINS. " 单位
*      LT_RESB-SHKZG = IT_AUFNR-SHKZG.
*    ENDIF.
*    MODIFY LT_RESB.
*    CLEAR IT_AUFNR.
*  ENDLOOP.

  LOOP AT LT_ALV2.
    LOOP AT  LT_RESB WHERE AUFNR = LT_ALV2-AUFNR.
      MOVE-CORRESPONDING LT_ALV2 TO GT_ALV2.
      GT_ALV2-MATNR_ITEM = LT_RESB-MATNR.
      GT_ALV2-BDMNG = LT_RESB-BDMNG.
      GT_ALV2-ENMNG = LT_RESB-ENMNG.
      GT_ALV2-XLOEK = LT_RESB-XLOEK.
      " 借贷标识为负号时
      IF LT_RESB-SHKZG = 'S'.
        GT_ALV2-BDMNG = - LT_RESB-BDMNG.
        GT_ALV2-ENMNG = - LT_RESB-ENMNG.
      ENDIF.
      GT_ALV2-MEINS = LT_RESB-MEINS.
      GT_ALV2-DWERK = LT_RESB-WERKS.
      " 入库比率需求 =（AFPO-WEMNG/AFPO-PSMNG）*需求数量（RESB-BDMNG））
      IF GT_ALV2-PSMNG <> 0.
        GT_ALV2-RKBL = GT_ALV2-WEMNG /  GT_ALV2-PSMNG * GT_ALV2-BDMNG.
      ELSE.
        GT_ALV2-RKBL = 0.
      ENDIF.
      APPEND GT_ALV2.
      CLEAR GT_ALV2.
    ENDLOOP.
  ENDLOOP.

  "  总计消耗
  DATA:LT_MSEG   TYPE STANDARD TABLE OF MSEG WITH HEADER LINE."计划内发料数量
  DATA:LT_MSEG_1 TYPE STANDARD TABLE OF MSEG WITH HEADER LINE."计划外发料数量
  DATA:LT_MAKT_ITEM TYPE STANDARD TABLE OF MAKT WITH HEADER LINE.
  CLEAR:LT_MSEG,LT_MSEG[].
  CLEAR:LT_MAKT_ITEM,LT_MAKT_ITEM[].

  IF GT_ALV2[] IS NOT INITIAL.

*计划外发料
    SELECT
     MBLNR  " 物料凭证号
     MJAHR  " 凭证年度
     ZEILE  " 凭证行项目
     AUFNR " 生产订单
     MATNR " 组件
     WERKS " 工厂
     MENGE " 数量
     MEINS " 基本计量单位
     BWART " 移动类型（库存管理）
     RSNUM " 预留号
     SHKZG " 借贷标识
     FROM MSEG
     INTO CORRESPONDING FIELDS OF TABLE LT_MSEG_1
     FOR ALL ENTRIES IN GT_ALV2
     WHERE MATNR = GT_ALV2-MATNR_ITEM
     AND WERKS   = GT_ALV2-DWERK
     AND AUFNR   = GT_ALV2-AUFNR
     AND BWART   IN ('Z05','Z06')
     AND RSNUM   = '0'.

    SELECT  " 取得组件描述
      MATNR
      MAKTX
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT_ITEM
      FOR ALL ENTRIES IN GT_ALV2
      WHERE MATNR = GT_ALV2-MATNR_ITEM
      AND SPRAS  = SY-LANGU.

  ENDIF.

  DATA:L_MENGE TYPE MSEG-MENGE.

  LOOP AT GT_ALV2.
    CLEAR L_MENGE.


*计划外发料数量
    LOOP AT LT_MSEG_1 WHERE MATNR = GT_ALV2-MATNR_ITEM
                      AND WERKS = GT_ALV2-DWERK
                      AND AUFNR = GT_ALV2-AUFNR.
      IF LT_MSEG_1-SHKZG = 'S'.
        LT_MSEG_1-MENGE = LT_MSEG_1-MENGE * -1.
      ENDIF.
      L_MENGE = L_MENGE + LT_MSEG_1-MENGE. " 计划外领 + (-计划外退)
      DELETE LT_MSEG_1. " 删除组件物料号存在的记录
    ENDLOOP.

*计划外发料
    GT_ALV2-ERFMG = L_MENGE.

    GT_ALV2-JHWMNG = GT_ALV2-ENMNG + L_MENGE. " 总计消耗 = 计划消耗 + 计划外领 + (-计划外退)

    " 组件描述
    READ TABLE LT_MAKT_ITEM WITH KEY MATNR = GT_ALV2-MATNR_ITEM.
    IF SY-SUBRC = 0.
      GT_ALV2-MAKTX_ITEM = LT_MAKT_ITEM-MAKTX.
    ENDIF.

    MODIFY GT_ALV2.
  ENDLOOP.

*对于组件存在mseg,r 不存在resb的情况,组件添加到ALV中显示
  DATA:LT_MSEG_TMP TYPE STANDARD TABLE OF MSEG WITH HEADER LINE.
  CLEAR:LT_MSEG_TMP,LT_MSEG_TMP[].

  LOOP AT LT_MSEG_1.
    LT_MSEG_TMP-AUFNR = LT_MSEG-AUFNR." 生产订单
    LT_MSEG_TMP-WERKS = LT_MSEG-WERKS." 工厂
    LT_MSEG_TMP-MATNR = LT_MSEG-MATNR." 组件
    APPEND LT_MSEG_TMP.
    CLEAR LT_MSEG_TMP.
  ENDLOOP.

  SORT LT_MSEG_TMP BY AUFNR WERKS MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MSEG_TMP. " 删除重复项

  IF LT_MSEG_TMP[] IS NOT INITIAL.

    LOOP AT LT_MSEG_TMP.
      CLEAR L_MENGE.

*计划外发料数量
      LOOP AT LT_MSEG_1 WHERE MATNR = GT_ALV2-MATNR_ITEM
                        AND WERKS = GT_ALV2-DWERK
                        AND AUFNR = GT_ALV2-AUFNR.
        IF LT_MSEG_1-SHKZG = 'S'.
          LT_MSEG_1-MENGE = LT_MSEG_1-MENGE * -1.
        ENDIF.
        L_MENGE = L_MENGE + LT_MSEG_1-MENGE. " 计划外领 + (-计划外退)
        DELETE LT_MSEG_1. " 删除组件物料号存在的记录
      ENDLOOP.

      LT_MSEG_TMP-MENGE = L_MENGE. " 计划外领+ (-计划外退)

      READ TABLE GT_ALV2 WITH KEY DWERK = LT_MSEG_TMP-WERKS
                                  AUFNR = LT_MSEG_TMP-AUFNR." 生产订单
      IF SY-SUBRC = 0.
        GT_ALV2-PSMNG = 0. " 订单数量
        GT_ALV2-WEMNG = 0. " 入库数量
        GT_ALV2-MATNR_ITEM = LT_MSEG_TMP-MATNR.
        GT_ALV2-CHECK = 3. " 不可关闭
        GT_ALV2-ERFMG  = LT_MSEG_TMP-MENGE. "计划外消耗
        GT_ALV2-JHWMNG = LT_MSEG_TMP-MENGE. "总计消耗
        APPEND GT_ALV2.
        CLEAR GT_ALV2.
      ENDIF.
    ENDLOOP.

  ENDIF.

*组件检查结果

  " 1 可以关闭
  " 抬头：1）订单数量=入库数量；订单数量=报工数量；
  " 组件：3）入库比率需求=计划消耗；(每一组件行都满足)
  " 组件: 4）入库比率需求=总计消耗；(每一组件行都满足)
  " -------------------------------------------------------
  " 3 不可关闭
  " 抬头
  " 订单类型订单类型（AFPO-DAUAT）为Z110或Z210且满足下列条件之一即"不可关闭"
  " 1）订单数量<入库数量；
  " 2）订单数量<报工数量；
  " 3）入库数量<报工数量；
  " 组件
  " 订单类型订单类型（AFPO-DAUAT）为Z110或Z210且满足下列条件之一即"不可关闭"
  " 4）入库比率需求>计划消耗；(任一组件满足)；
  " 5）入库比率需求>总计消耗；(任一组件满足)；
  " --------------------------------------------------------
  " 2 需要检查
  " 1,3 之外情况
  " --------------------------------------------------------

  LOOP AT GT_ALV1.

    " 判断是否为 3 不可关闭
    CHECK  GT_ALV1-CHECK IS INITIAL.

    IF GT_ALV1-DAUAT = 'Z110' OR GT_ALV1-DAUAT = 'Z210'.
      IF GT_ALV1-PSMNG < GT_ALV1-WEMNG. " 订单数量<入库数量
        GT_ALV1-CHECK = 3.
        MODIFY GT_ALV1.
      ENDIF.
      IF GT_ALV1-PSMNG < GT_ALV1-IGMNG ." 订单数量<报工数量
        GT_ALV1-CHECK = 3.
        MODIFY GT_ALV1.
      ENDIF.
      IF GT_ALV1-WEMNG < GT_ALV1-IGMNG." 入库数量<报工数量
        GT_ALV1-CHECK = 3.
        MODIFY GT_ALV1.
      ENDIF.


      CHECK  GT_ALV1-CHECK IS INITIAL.

      LOOP AT GT_ALV2 WHERE AUFNR = GT_ALV1-AUFNR
                       AND MATNR_HEAD = GT_ALV1-MATNR_HEAD.

        IF GT_ALV2-RKBL > GT_ALV2-ENMNG.   " 入库比率需求>计划消耗；(任一组件满足)
          GT_ALV1-CHECK = 3.
          MODIFY GT_ALV1.
        ENDIF.

        IF  GT_ALV2-RKBL > GT_ALV2-JHWMNG. " 入库比率需求>总计消耗；(任一组件满足)；
          GT_ALV1-CHECK = 3.
          MODIFY GT_ALV1.
        ENDIF.

        IF GT_ALV2-CHECK = 3. " 存在组件检查结果为3的
          GT_ALV1-CHECK = 3.
          MODIFY GT_ALV1.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " 判断是否为 1 不可关闭
    CHECK  GT_ALV1-CHECK IS INITIAL.

    IF  GT_ALV1-PSMNG = GT_ALV1-WEMNG AND GT_ALV1-PSMNG = GT_ALV1-IGMNG .

      GT_ALV1-CHECK = 1.
      " 入库比率需求=计划消耗；(每一组件行都满足)
      " 入库比率需求=总计消耗；(每一组件行都满足)
      LOOP AT GT_ALV2 WHERE AUFNR = GT_ALV1-AUFNR
           AND   MATNR_HEAD = GT_ALV1-MATNR_HEAD.
        IF GT_ALV2-RKBL <> GT_ALV2-ENMNG.
          CLEAR GT_ALV1-CHECK.
          EXIT.
        ENDIF.
        IF GT_ALV2-RKBL <> GT_ALV2-JHWMNG.
          CLEAR GT_ALV1-CHECK.
          EXIT.
        ENDIF.
      ENDLOOP.
      MODIFY GT_ALV1.
    ENDIF.

    " 判断是否为 2 需要检查
    CHECK  GT_ALV1-CHECK IS INITIAL.
    GT_ALV1-CHECK = 2.
    MODIFY GT_ALV1.

  ENDLOOP.

ENDFORM.                    " FRM_GET_DATA2
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_BEFORE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_BEFORE_BDC .
  LOOP AT GT_ALV1 WHERE BOX = 'X'.
    IF GT_ALV1-CHECK = 3.
      MESSAGE '检查结果为3的订单，不能技术完成' TYPE 'E'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FRM_CHECK_BEFORE_BDC
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_9002_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_9002_DATA .
  CLEAR:GT_ALV3,GT_ALV3[].

  GT_ALV3[] = GT_ALV2[].

  LOOP AT GT_ALV3.
    READ TABLE GT_ALV1 WITH KEY AUFNR = GT_ALV3-AUFNR
                                BOX = 'X'.
    IF SY-SUBRC <> 0.
      DELETE GT_ALV3. " 排除没有选中记录
    ENDIF.
  ENDLOOP.

*       " 发料欠料 = 需求数量 - 计划内发料
*       " 在线数 = 总计消耗 - 理论用料数量
*       "理论用料数量 = 入库数量/订单数量×需求数量
  LOOP AT GT_ALV3.
    GT_ALV3-ZFLQL  = GT_ALV3-BDMNG - GT_ALV3-ENMNG.
    GT_ALV3-LLYLSL = GT_ALV3-WEMNG / GT_ALV3-PSMNG * GT_ALV3-BDMNG.
    GT_ALV3-ZZXS   = GT_ALV3-JHWMNG - GT_ALV3-LLYLSL.
    MODIFY GT_ALV3.
  ENDLOOP.

ENDFORM.                    " FRM_GET_9002_DATA
