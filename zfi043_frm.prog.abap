*&---------------------------------------------------------------------*
*&  包含                ZFI043_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DEAL_DATA .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DATA_CLEAR .
  REFRESH: GT_ITEM.
  CLEAR: GS_HEAD, GS_ITEM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DATA_INIT .
  G_EDIT_MOD = GC_EDITABLE.

  GS_HEAD-BLDAT = SY-DATUM.
  GS_HEAD-BLART = 'RV'.

*  SELECT *
*    INTO TABLE IT_SKAT
*    FROM SKAT
*    WHERE SPRAS = SY-LANGU
*      AND KTOPL = '1000'.
*  SORT IT_SKAT BY SAKNR.

*  append wa_item to it_item.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_CONTAINER .
  CREATE OBJECT GR_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'CONTAINER'
*     lifetime       = cl_gui_custom_container=>lifetime_dynpro
    .

  CREATE OBJECT GR_ALVGRID
    EXPORTING
      I_APPL_EVENTS = 'X'
      I_PARENT      = GR_CONTAINER.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY .
  PERFORM INIT_OO_LAYOUT.              "设置输出格式
*  PERFORM INIT_SORT.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'.             "设置变式控制
  PERFORM FRM_INIT_LVC.             " 初始化内表结构/ALV显示结构
  PERFORM EXCLUDE_TB_FUNCTIONS TABLES GT_OO_EXCLUDE.
  PERFORM FRM_OO_BUILD_EVENT.
  PERFORM FRM_OO_OUTPUT TABLES GT_LVC              "输出
                               GT_SORT
                               GT_OO_EXCLUDE
                               GT_ITEM
                        USING GS_LAYOUT
                              GS_VARIANT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_OO_LAYOUT .
  GS_LAYOUT-ZEBRA = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  GS_LAYOUT-SEL_MODE = 'A'.
  GS_LAYOUT-EDIT_MODE = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  GS_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0074   text
*----------------------------------------------------------------------*
FORM INIT_VARIANT  USING  P_HANDLE TYPE SLIS_HANDL .
  CLEAR: GS_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-HANDLE = P_HANDLE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'BUZEI' '' '' '' '' 'BSEG' 'BUZEI' 3.
  INIT_FIELDCAT 'BSCHL' '' 'X' '' '' 'BSEG' 'BSCHL' 4.
  INIT_FIELDCAT 'HKONT' TEXT-F01 'X' '' '' '' '' 10.
  INIT_FIELDCAT 'HKTXT' TEXT-F02 '' '' '' 'SKAT' 'TXT20' 20.
  INIT_FIELDCAT 'KUNNR' '' 'X' '' '' 'BSEG' 'KUNNR' 10.
  INIT_FIELDCAT 'KNAME' TEXT-F03 '' '' '' 'KNA1' 'NAME1' 20.

  INIT_FIELDCAT 'MATNR' '' '' '' '' 'BSEG' 'MATNR' 18.
  INIT_FIELDCAT 'MAKTX' '' '' '' '' 'MAKT' 'MATKX' 20.
  INIT_FIELDCAT 'MENGE' TEXT-F04 '' '' '' 'BSEG' 'MENGE' 10.
  INIT_FIELDCAT 'MENGE2' TEXT-F05 '' '' '' 'BSEG' 'MENGE' 10.
  INIT_FIELDCAT 'MENGE3' TEXT-F06 'X' '' '' 'BSEG' 'MENGE' 10.
  INIT_FIELDCAT 'MEINS' '' '' '' '' 'BSEG' 'MEINS' 10.
  INIT_FIELDCAT 'OPREIS' TEXT-F07 '' '' '' 'CKIS' 'OPREIS' 10.
  INIT_FIELDCAT 'OPREIS2' TEXT-F08 '' '' '' 'CKIS' 'OPREIS' 10.

  INIT_FIELDCAT 'WAERS' '' '' '' '' 'BKPF' 'WAERS' 5.
  INIT_FIELDCAT 'POSID' 'WBS' 'X' '' '' '' '' 24.
  INIT_FIELDCAT 'POST1' TEXT-F09 '' '' '' 'PRPS' 'POST1' 40.
  INIT_FIELDCAT 'SGTXT' '' 'X' '' '' 'BSEG' 'SGTXT' 20.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       弃掉标准按钮（增加、插入）
*----------------------------------------------------------------------*
*      -->P_GT_OO_EXCLUDE  text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS  TABLES   PT_EXCLUDE TYPE UI_FUNCTIONS.
*  DATA: LS_EXCLUDE TYPE UI_FUNC.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.

*  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_average .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_sum .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_subtot .
*  APPEND ls_exclude TO pt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC .
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_find .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_filter .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print_prev .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_export .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_graph .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_view .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_detail .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_help .
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_info .
*  APPEND ls_exclude TO pt_exclude.
*   ls_exclude = cl_gui_alv_grid=>MC_MB_VARIANT.
*  APPEND ls_exclude TO pt_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OO_BUILD_EVENT .
  DATA LR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.
  CREATE OBJECT LR_EVENT_HANDLER.
  SET HANDLER :
*        	      LR_EVENT_HANDLER->HANDLE_TOOLBAR              FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_BEFORE_USER_COMMAND  FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_USER_COMMAND         FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_AFTER_USER_COMMAND   FOR GR_ALVGRID,
                LR_EVENT_HANDLER->HANDLE_ONF4                 FOR GR_ALVGRID ,
*                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED         FOR GR_ALVGRID .
                LR_EVENT_HANDLER->HANDLE_DATA_CHANGED_FINISHED FOR GR_ALVGRID .
*                LR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK         FOR GR_ALVGRID,
*                LR_EVENT_HANDLER->HANDLE_BUTTON_CLICK         FOR GR_ALVGRID.

  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.
*  LS_F4-FIELDNAME = 'ZTERM'.      "F4对应的栏位
*  LS_F4-REGISTER = 'X'.
*  LS_F4-GETBEFORE = 'X'.
*  LS_F4-CHNGEAFTER = 'X'.
*  INSERT LS_F4 INTO TABLE LT_F4.
  LS_F4-FIELDNAME = 'HKONT'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
**  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.
*  LS_F4-FIELDNAME = 'MWSKZ'.      "F4对应的栏位
*  LS_F4-REGISTER = 'X'.
*  LS_F4-GETBEFORE = 'X'.
*  LS_F4-CHNGEAFTER = 'X'.
*  INSERT LS_F4 INTO TABLE LT_F4.

  CALL METHOD GR_ALVGRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = LT_F4[].

  CALL METHOD GR_ALVGRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_OO_EXCLUDE  text
*      -->P_IT_ITEM  text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*----------------------------------------------------------------------*
FORM FRM_OO_OUTPUT  TABLES   PT_LVC TYPE LVC_T_FCAT
                             PT_SORT TYPE LVC_T_SORT
                             PT_EXCLUDE TYPE UI_FUNCTIONS
                             PT_DATA
                      USING  PS_LAYOUT TYPE LVC_S_LAYO
                              PS_VARIANT TYPE DISVARIANT.

  CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
*     IS_VARIANT                    = PS_VARIANT
      I_SAVE                        = 'A'
*     i_default                     = 'X'
      IS_LAYOUT                     = PS_LAYOUT
*     is_print                      =
*     it_special_groups             =
      IT_TOOLBAR_EXCLUDING          = PT_EXCLUDE[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      IT_OUTTAB                     = PT_DATA[]
      IT_FIELDCATALOG               = PT_LVC[]
      IT_SORT                       = PT_SORT[]
*     it_filter                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0012   text
*      -->P_GS_HEAD_BUKRS  text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK  USING    VALUE(P_ACTVT)
                              P_BUKRS.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_CHANGED  CHANGING P_SUBRC TYPE SY-SUBRC.
  P_SUBRC = 0.
  IF G_CHANGED = ABAP_TRUE.
    DATA L_ANS.
    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
*       DEFAULTOPTION       = 'Y'
        TEXTLINE1 = TEXT-M20
        TEXTLINE2 = TEXT-M21
        TITEL     = TEXT-M19
*       START_COLUMN        = 25
*       START_ROW = 6
      IMPORTING
        ANSWER    = L_ANS.
    IF L_ANS = 'N'.
      P_SUBRC = 2.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_ACCDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_POST_ACCDOC .
  DATA L_ANS.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      TEXT_QUESTION         = '确定要保存并过账吗？'
      TEXT_BUTTON_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      ANSWER                = L_ANS
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK L_ANS EQ '1'.

  DATA L_SUBRC TYPE SY-SUBRC.
  PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

  CHECK L_SUBRC EQ 0.

*  PERFORM FRM_AUTH_CHECK USING '10' WA_HEAD-BUKRS.
*  IF SY-SUBRC NE 0.
*    MESSAGE S010(ZFICO01) WITH WA_HEAD-BUKRS DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

*  PERFORM FRM_PRE_SRYS ."收入科目映射   "add it02 20170713
  PERFORM FRM_BAPI_DATA_PREP USING ''.
  PERFORM FRM_CALL_BAPI CHANGING GS_HEAD-BELNR GS_HEAD-GJAHR.

  IF GS_HEAD-BELNR IS NOT INITIAL.  " 创建成功

    PERFORM FRM_SAVE_ZFI043.
*
*    G_CHANGED = ABAP_FALSE.
*
    " 切换为只读模式
    G_EDIT_MOD = GC_READONLY.

    MESSAGE TEXT-M01 TYPE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.
  " 凭证日期
  IF GS_HEAD-BLDAT IS INITIAL.
    MESSAGE TEXT-M02 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF GS_HEAD-BUDAT IS INITIAL.
    MESSAGE TEXT-M03 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 业务类型
  IF GS_HEAD-ZYWLX IS INITIAL.
    MESSAGE TEXT-M04 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 凭证类型
  IF GS_HEAD-BLART IS INITIAL.
    MESSAGE TEXT-M05 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF GS_HEAD-BUKRS IS INITIAL.
    MESSAGE TEXT-M06 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 货币
  IF GS_HEAD-WAERS IS INITIAL.
    MESSAGE TEXT-M07 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 销售订单
  IF GS_HEAD-PSPID IS INITIAL.
    MESSAGE TEXT-M08 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  "ADD BY HANDWY 2015-4-23
  "检查是否已经过账
  IF   GS_HEAD-BELNR  IS NOT INITIAL
  AND  GS_HEAD-GJAHR  IS NOT INITIAL.
    MESSAGE TEXT-M22 TYPE 'W'  DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 行项目
  IF GT_ITEM IS INITIAL.
    MESSAGE TEXT-M09 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  DATA: L_OPREIS TYPE CKIS-OPREIS.

  LOOP AT GT_ITEM ASSIGNING <FS_ITEM>.
    " 记账码
*    IF GS_ITEM-BSTYP = '1' AND <FS_ITEM>-BSCHL NE '01' AND <FS_ITEM>-BSCHL NE '50'.
*      MESSAGE TEXT-M10 TYPE 'W' DISPLAY LIKE 'E'.
*      P_SUBRC = 8.
*      EXIT.
*    ELSEIF GS_ITEM-BSTYP = '2' AND <FS_ITEM>-BSCHL NE '01' AND <FS_ITEM>-BSCHL NE '50' AND <FS_ITEM>-BSCHL NE '11' AND <FS_ITEM>-BSCHL NE '40'.
*      MESSAGE TEXT-M11 TYPE 'W' DISPLAY LIKE 'E'.
*      P_SUBRC = 8.
*      EXIT.
*    ENDIF.

    " 科目
    IF <FS_ITEM>-HKONT IS INITIAL.
      MESSAGE TEXT-M12 TYPE 'W' DISPLAY LIKE 'E'.
      P_SUBRC = 8.
      EXIT.
    ENDIF.

    IF <FS_ITEM>-BSCHL = '01' OR <FS_ITEM>-BSCHL = '11'.
      " 客户
      IF <FS_ITEM>-KUNNR IS INITIAL.
        MESSAGE TEXT-M13 TYPE 'W' DISPLAY LIKE 'E'.
        P_SUBRC = 8.
        EXIT.
      ENDIF.
*      " 销售订单行
*      IF <FS_ITEM>-POSNR IS INITIAL.
*        MESSAGE TEXT-M14 TYPE 'W' DISPLAY LIKE 'E'.
*        P_SUBRC = 8.
*        EXIT.
*      ENDIF.
*      " 付款条件
*      IF <FS_ITEM>-ZTERM IS INITIAL.
*        MESSAGE TEXT-M15 TYPE 'W' DISPLAY LIKE 'E'.
*        P_SUBRC = 8.
*        EXIT.
*      ENDIF.
      " 基准日期
      IF <FS_ITEM>-ZFBDT IS INITIAL.
        MESSAGE TEXT-M16 TYPE 'W' DISPLAY LIKE 'E'.
        P_SUBRC = 8.
        EXIT.
      ENDIF.
    ENDIF.

*    IF <FS_ITEM>-BSCHL = '50'.
*      " 税码
*      IF <FS_ITEM>-MWSKZ IS INITIAL.
*        MESSAGE TEXT-M17 TYPE 'W' DISPLAY LIKE 'E'.
*        P_SUBRC = 8.
*        EXIT.
*      ENDIF.
*    ENDIF.

    IF <FS_ITEM>-BSCHL = '01' OR <FS_ITEM>-BSCHL = '40'.
      L_OPREIS = L_OPREIS + <FS_ITEM>-OPREIS2.
    ELSEIF <FS_ITEM>-BSCHL = '11' OR <FS_ITEM>-BSCHL = '50'.
      L_OPREIS = L_OPREIS - <FS_ITEM>-OPREIS2.
    ENDIF.
  ENDLOOP.

  IF P_SUBRC NE 0.
    EXIT.
  ENDIF.

  IF L_OPREIS NE 0.
    MESSAGE TEXT-M18 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ITEM_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ITEM_INIT .
  CHECK GT_ITEM IS INITIAL.

  " 取项目定义下的1、2层级的WBS元素
  DATA: LT_PRPS TYPE TABLE OF PRPS,
        LS_PRPS TYPE PRPS.
  SELECT *
    INTO TABLE LT_PRPS
    FROM PRPS
   WHERE PSPHI = GS_PROJ-PSPNR
     AND PBUKR = GS_HEAD-BUKRS
     AND STUFE IN ('1','2') .
  IF LT_PRPS IS NOT INITIAL .
    DATA: LT_BSEG TYPE TABLE OF BSEG,
          LS_BSEG TYPE BSEG.
    SELECT *
      INTO TABLE LT_BSEG
      FROM BSEG
       FOR ALL ENTRIES IN LT_PRPS
     WHERE PROJK = LT_PRPS-PSPNR
       AND BUKRS = GS_HEAD-BUKRS
       AND ( HKONT LIKE '6404%'
        OR HKONT = '6001010101' ) .
    IF LT_BSEG IS NOT INITIAL .
      " 发货数量
      DATA: BEGIN OF LS_BSEG_COL,
              BUKRS TYPE BSEG-BUKRS,
              MATNR TYPE BSEG-MATNR,
              PROJK TYPE BSEG-PROJK,
              POSID TYPE PRPS-POSID,
              HKONT TYPE BSEG-HKONT,
              MENGE TYPE BSEG-MENGE,
            END OF LS_BSEG_COL .
      DATA LT_BSEG_COL LIKE TABLE OF LS_BSEG_COL .
      " 已开票数量
      DATA: BEGIN OF LS_BSEG_COL2,
              BUKRS TYPE BSEG-BUKRS,
              MATNR TYPE BSEG-MATNR,
              PROJK TYPE BSEG-PROJK,
              POSID TYPE PRPS-POSID,
              HKONT TYPE BSEG-HKONT,
              MENGE TYPE BSEG-MENGE,
            END OF LS_BSEG_COL2 .
      DATA LT_BSEG_COL2 LIKE TABLE OF LS_BSEG_COL2 .
      " 计算发货数量、已开票数量
      LOOP AT LT_BSEG INTO LS_BSEG .
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY PSPNR = LS_BSEG-PROJK .
        IF LS_BSEG-HKONT+0(4) = '6404' .
          MOVE-CORRESPONDING LS_BSEG TO LS_BSEG_COL .
          LS_BSEG_COL-POSID = LS_PRPS-POSID .
          CLEAR LS_BSEG_COL-PROJK .
          IF LS_BSEG-SHKZG = 'S'.
          ELSEIF LS_BSEG-SHKZG = 'H'.
            LS_BSEG_COL-MENGE = LS_BSEG_COL-MENGE * '-1' .
          ENDIF.
          COLLECT LS_BSEG_COL INTO LT_BSEG_COL .
        ELSEIF LS_BSEG-HKONT = '6001010101' .
          MOVE-CORRESPONDING LS_BSEG TO LS_BSEG_COL2 .
          LS_BSEG_COL2-POSID = LS_PRPS-POSID .
          CLEAR LS_BSEG_COL2-PROJK .
          IF LS_BSEG-SHKZG = 'S'.
            LS_BSEG_COL2-MENGE = LS_BSEG_COL2-MENGE * '-1' .
          ENDIF.
          COLLECT LS_BSEG_COL2 INTO LT_BSEG_COL2 .
        ENDIF.
        CLEAR LS_BSEG .
        CLEAR LS_BSEG_COL .
        CLEAR LS_BSEG_COL2 .
      ENDLOOP.
    ENDIF.
    " 取单价
    DATA: LT_PRECP2 TYPE TABLE OF PRECP2,
          LS_PRECP2 TYPE PRECP2.
    SELECT *
      INTO TABLE LT_PRECP2
      FROM PRECP2
       FOR ALL ENTRIES IN LT_PRPS
     WHERE SUBNR = LT_PRPS-OBJNR
       AND VERSN = 100 .
    IF LT_PRECP2 IS NOT INITIAL .
      DATA: LT_CKIS TYPE TABLE OF CKIS,
            LS_CKIS TYPE CKIS.
      SELECT *
        INTO TABLE LT_CKIS
        FROM CKIS
         FOR ALL ENTRIES IN LT_PRECP2
       WHERE KALNR = LT_PRECP2-KALNR .
    ENDIF.
  ENDIF.

* 客户应收账款 - 第一行
  CLEAR GS_ITEM.
  GS_ITEM-BUZEI = '1'.  "分录序号
  GS_ITEM-BSCHL = '01'. "记账码

  " 货币
  GS_ITEM-WAERS = GS_HEAD-WAERS.
  " 基准付款日期
  GS_ITEM-ZFBDT = SY-DATUM.
  "WBS
  READ TABLE LT_PRPS INTO LS_PRPS WITH KEY STUFE = 1 .
  IF SY-SUBRC = 0 .
    GS_ITEM-POSID = LS_PRPS-POSID .
    GS_ITEM-POST1 = LS_PRPS-POST1 .
    " 客户
    GS_ITEM-KUNNR = LS_PRPS-ZKHBM .
    " 客户描述
    SELECT SINGLE
           NAME1
      INTO GS_ITEM-KNAME
      FROM KNA1
     WHERE KUNNR = LS_PRPS-ZKHBM .

    SELECT SINGLE AKONT
      INTO GS_ITEM-HKONT
      FROM KNB1
     WHERE BUKRS = GS_HEAD-BUKRS
       AND KUNNR = LS_PRPS-ZKHBM .
    " 科目描述
    SELECT SINGLE TXT20
      INTO GS_ITEM-HKTXT
      FROM SKAT
     WHERE SPRAS = SY-LANGU
       AND KTOPL = '1000'
       AND SAKNR = GS_ITEM-HKONT .
  ENDIF.

  PERFORM FRM_CELL_STYLE USING 'KUNNR'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'OPREIS2'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'ZFBDT'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'MENGE3'
                         ''
                         CHANGING GS_ITEM-CELLSTYLE.
*  PERFORM FRM_CELL_STYLE USING 'MWSKZ'
*                         ''
*                         CHANGING GS_ITEM-CELLSTYLE.
*  PERFORM FRM_CELL_STYLE USING 'POSNR'
*                         'X'
*                         CHANGING GS_ITEM-CELLSTYLE.

  APPEND GS_ITEM TO GT_ITEM.

  " 销项税
  CLEAR GS_ITEM.
  GS_ITEM-BUZEI = '2'.   "分录序号
  GS_ITEM-BSCHL = '50'.  "记账码
  GS_ITEM-HKONT = '2221030503'.   "科目
  " 科目描述
  SELECT SINGLE TXT20
    INTO GS_ITEM-HKTXT
    FROM SKAT
   WHERE SPRAS = SY-LANGU
     AND KTOPL = '1000'
     AND SAKNR = '2221030503' .
  " 货币
  GS_ITEM-WAERS = GS_HEAD-WAERS.
  " WBS
  GS_ITEM-POSID = LS_PRPS-POSID.
  " WBS描述
  GS_ITEM-POST1 = LS_PRPS-POST1.

  PERFORM FRM_CELL_STYLE USING 'KUNNR'
                         ''
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'OPREIS2'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'ZFBDT'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'MENGE3'
                         ''
                         CHANGING GS_ITEM-CELLSTYLE.
  PERFORM FRM_CELL_STYLE USING 'POSID'
                         'X'
                         CHANGING GS_ITEM-CELLSTYLE.

  APPEND GS_ITEM TO GT_ITEM.
  CLEAR GS_ITEM.

  " 其余行
*  DATA: LT_SKAT TYPE TABLE OF SKAT,
*        LS_SKAT TYPE SKAT.
  DATA L_TABIX TYPE SY-TABIX .
  L_TABIX = 2 .

  DATA L_TXT20 TYPE SKAT-TXT20 .
  SELECT SINGLE TXT20
    INTO L_TXT20
    FROM SKAT
*     FOR ALL ENTRIES IN LT_BSEG_COL
   WHERE SAKNR = '6001010101'
     AND SPRAS = SY-LANGU
     AND KTOPL = 1000 .
  DATA: LT_MAKT TYPE TABLE OF MAKT,
        LS_MAKT TYPE MAKT.
  SELECT *
    INTO TABLE LT_MAKT
    FROM MAKT
     FOR ALL ENTRIES IN LT_BSEG_COL
   WHERE MATNR = LT_BSEG_COL-MATNR
     AND SPRAS = SY-LANGU .
  " 取物料单位
  DATA: BEGIN OF LS_MARA ,
          MATNR TYPE MARA-MATNR,
          MEINS TYPE MARA-MEINS,
        END OF LS_MARA .
  DATA LT_MARA LIKE TABLE OF LS_MARA .
  SELECT MATNR
         MEINS
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM MARA
     FOR ALL ENTRIES IN LT_BSEG_COL
   WHERE MATNR = LT_BSEG_COL-MATNR .
  SORT LT_MARA BY MATNR .
  LOOP AT LT_BSEG_COL INTO LS_BSEG_COL .
    CLEAR GS_ITEM.

    CLEAR LS_PRPS .
    READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_BSEG_COL-POSID .
    CLEAR LS_BSEG .
    READ TABLE LT_BSEG INTO LS_BSEG WITH KEY PROJK = LS_PRPS-PSPNR .

    L_TABIX = L_TABIX + 1 .
    GS_ITEM-BUZEI = L_TABIX ."分录行号
    GS_ITEM-BSCHL = '50'. "记账码
    GS_ITEM-HKONT = '6001010101' . "科目
*    READ TABLE LT_SKAT INTO LS_SKAT WITH KEY SAKNR = '6001010101' .
*    IF SY-SUBRC = 0 .
    GS_ITEM-HKTXT = L_TXT20 . " 科目描述
*    ENDIF.
    GS_ITEM-MATNR = LS_BSEG_COL-MATNR . "物料
    READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = LS_BSEG_COL-MATNR .
    IF SY-SUBRC = 0 .
      GS_ITEM-MAKTX = LS_MAKT-MAKTX . "物料描述
    ENDIF.
    GS_ITEM-MENGE = LS_BSEG_COL-MENGE . "发货数量
    READ TABLE LT_BSEG_COL2 INTO LS_BSEG_COL2 WITH KEY POSID = LS_BSEG_COL-POSID
                                                       MATNR = LS_BSEG_COL-MATNR .
    IF SY-SUBRC = 0 .
      GS_ITEM-MENGE2 = LS_BSEG_COL2-MENGE . "已发货数量
    ENDIF.
    GS_ITEM-MENGE3 = GS_ITEM-MENGE - GS_ITEM-MENGE2 . "本次发货数量
    READ TABLE LT_MARA INTO LS_MARA WITH KEY MATNR = LS_BSEG_COL-MATNR BINARY SEARCH .
    GS_ITEM-MEINS = LS_MARA-MEINS . "单位
    CLEAR LS_MARA .
    " 取单价
    CLEAR LS_PRECP2 .
    READ TABLE LT_PRECP2 INTO LS_PRECP2 WITH KEY SUBNR = LS_PRPS-OBJNR .
    CLEAR LS_CKIS .
    READ TABLE LT_CKIS INTO LS_CKIS WITH KEY KALNR = LS_PRECP2-KALNR
                                             MATNR = LS_BSEG_COL-MATNR .
    GS_ITEM-OPREIS = LS_CKIS-OPREIS . "单价
    GS_ITEM-OPREIS2 = GS_ITEM-MENGE3 * GS_ITEM-OPREIS . "过账金额
    IF GS_ITEM-OPREIS2 LT 0 .
      GS_ITEM-OPREIS2 = 0 .
    ENDIF.
    GS_ITEM-WAERS = GS_HEAD-WAERS . "货币
    GS_ITEM-POSID = LS_BSEG_COL-POSID . "WBS
    GS_ITEM-POST1 = LS_PRPS-POST1 . "WBS描述

    PERFORM FRM_CELL_STYLE USING 'KUNNR'
                             ''
                             CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'OPREIS2'
                           'X'
                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'ZFBDT'
                           'X'
                           CHANGING GS_ITEM-CELLSTYLE.

    APPEND GS_ITEM TO GT_ITEM.

  ENDLOOP.

  "自动计算凭证余额
  CLEAR GS_HEAD-OPREIS .
  LOOP AT GT_ITEM INTO GS_ITEM .
    IF GS_ITEM-BSCHL = '01' OR GS_ITEM-BSCHL = '40'  .
      GS_HEAD-OPREIS = GS_HEAD-OPREIS + GS_ITEM-OPREIS2 .
    ELSEIF GS_ITEM-BSCHL = '50' OR GS_ITEM-BSCHL = '11' .
      GS_HEAD-OPREIS = GS_HEAD-OPREIS - GS_ITEM-OPREIS2 .
    ENDIF.
  ENDLOOP.

  PERFORM FRM_REFRESH_ALV.

  REFRESH LT_MARA .

*    L_ROW_ID = 1.
*    L_COL_ID = 'WRBTR'.
*    L_ROW_NO-ROW_ID = 1.
*
*    CALL METHOD GR_ALVGRID->SET_CURRENT_CELL_VIA_ID
*      EXPORTING
*        IS_ROW_ID    = L_ROW_ID
*        IS_COLUMN_ID = L_COL_ID
*        IS_ROW_NO    = L_ROW_NO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUKRS_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*----------------------------------------------------------------------*
FORM FRM_BUKRS_EXIST  USING    P_BUKRS.
  IF P_BUKRS IS INITIAL .
    MESSAGE '公司代码必输' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
    LEAVE SCREEN .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CELL_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0843   text
*      -->P_0844   text
*      <--P_WA_ITEM_CELLSTYLE  text
*----------------------------------------------------------------------*
FORM FRM_CELL_STYLE  USING    P_FIELDNAME
                              P_EDITABLE
                     CHANGING PT_CELLSTYLE TYPE LVC_T_STYL.

  DATA: LW_CELLSTYLE TYPE LVC_S_STYL.

  READ TABLE PT_CELLSTYLE INTO LW_CELLSTYLE WITH KEY FIELDNAME = P_FIELDNAME.
  IF SY-SUBRC = 0.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    MODIFY TABLE PT_CELLSTYLE FROM LW_CELLSTYLE.
  ELSE.
    LW_CELLSTYLE-FIELDNAME = P_FIELDNAME.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    INSERT LW_CELLSTYLE INTO TABLE PT_CELLSTYLE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REFRESH_ALV .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
*     i_soft_refresh =
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0487   text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_PREP  USING    VALUE(P_0487).
  " 清空BAPI变量
  PERFORM FRM_BAPI_DATA_CLEAR.

  DATA: L_KATYP TYPE CSKB-KATYP.
  DATA L_KUNNR TYPE KUNNR .

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  GS_DOCUMENTHEADER-DOC_DATE     =  GS_HEAD-BLDAT.
*过账日期
  GS_DOCUMENTHEADER-PSTNG_DATE   =  GS_HEAD-BUDAT.
*凭证类型
  GS_DOCUMENTHEADER-DOC_TYPE     =  GS_HEAD-BLART.
*公司代码
  GS_DOCUMENTHEADER-COMP_CODE    =  GS_HEAD-BUKRS.
*凭证抬头文本
  GS_DOCUMENTHEADER-HEADER_TXT   =  GS_HEAD-BKTXT.
*创建人员
  GS_DOCUMENTHEADER-USERNAME     =  SY-UNAME.
*参考凭证号 - 业务类型
  GS_DOCUMENTHEADER-REF_DOC_NO   =  GS_HEAD-ZYWLX.

**********************************************************************
* 凭证行
**********************************************************************
  " 第二行分配字段需要的客户
  READ TABLE GT_ITEM INTO GS_ITEM WITH KEY BSCHL = '01' .
  L_KUNNR = GS_ITEM-KUNNR .
  CLEAR GS_ITEM .

  LOOP AT GT_ITEM INTO GS_ITEM.
*    IF L_AUTO = 'X'.  " 创建自动凭证，映射记账码
*      IF WA_ITEM-BSCHL = '01'.
*        WA_ITEM-BSCHL = '11'.
*      ELSEIF WA_ITEM-BSCHL = '50'.
*        WA_ITEM-BSCHL = '40'.
*      ENDIF.
*
*      " 科目映射
**      IF wa_item-hkont BETWEEN 6001010101 AND 6001019899.     " 收入科目  "it02 屏蔽 20170713
**        wa_item-hkont = 6001019901.
**      ELSE
*      IF WA_ITEM-HKONT BETWEEN 2221030501 AND 2221030502
*        OR WA_ITEM-HKONT BETWEEN 2221030504 AND 2221030599. " 销项税
*        WA_ITEM-HKONT = 2221030503.
*      ELSEIF WA_ITEM-HKONT BETWEEN 2203010101 AND 2203010399. " 预收帐款科目
*        WA_ITEM-HKONT = 2203010401.
*      ELSEIF WA_ITEM-HKONT BETWEEN 1122010101 AND 1122010299. " 应收账款科目
*        WA_ITEM-HKONT = 1122010301.
*      ELSEIF WA_ITEM-HKONT EQ 1122020101.
*        WA_ITEM-HKONT = 1122020201.
*      ELSE.
*        READ TABLE GT_SRYS INTO GS_SRYS WITH KEY HKONT = WA_ITEM-HKONT
*                                                 BINARY SEARCH.
*        IF SY-SUBRC EQ 0.
*          WA_ITEM-HKONT = GS_SRYS-HKONT_TZ.
*        ENDIF.
*      ENDIF.
*    ENDIF.

    IF GS_ITEM-BSCHL = '01' OR GS_ITEM-BSCHL = '11'.
      " 客户应收账款分录
      CLEAR GS_ACCOUNTRECEIVABLE.
*       行项目号
      GS_ACCOUNTRECEIVABLE-ITEMNO_ACC = GS_ITEM-BUZEI.
*       科目
      GS_ACCOUNTRECEIVABLE-GL_ACCOUNT = GS_ITEM-HKONT.
*       项目文本
      GS_ACCOUNTRECEIVABLE-ITEM_TEXT = GS_ITEM-SGTXT.
*       客户编码
      GS_ACCOUNTRECEIVABLE-CUSTOMER = GS_ITEM-KUNNR.
**       付款条件
*      GS_ACCOUNTRECEIVABLE-PMNTTRMS = GS_ITEM-ZTERM.
*       付款基准日期
      GS_ACCOUNTRECEIVABLE-BLINE_DATE = GS_ITEM-ZFBDT.
*       WBS
      GS_ACCOUNTRECEIVABLE-ALLOC_NMBR = GS_ITEM-POSID.

      APPEND GS_ACCOUNTRECEIVABLE TO GT_ACCOUNTRECEIVABLE.

      CLEAR GS_CURRENCYAMOUNT.
      GS_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
      GS_CURRENCYAMOUNT-CURRENCY = GS_ITEM-WAERS.
*       金额
      GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-OPREIS2.
      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '50'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_CURRENCYAMOUNT-AMT_DOCCUR.
      ENDIF.

      APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

      " 记账码 & 反记账
      CLEAR GS_EXTENSION2.
      CLEAR GS_ZACCDOCUEXT.
      GS_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
      GS_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL.
*      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '40'.
*        WA_ZACCDOCUEXT-XNEGP = 'X'.
*      ENDIF.
      GS_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
      GS_EXTENSION2-VALUEPART1 = GS_ZACCDOCUEXT .
      APPEND GS_EXTENSION2 TO GT_EXTENSION2.

    ELSEIF GS_ITEM-HKONT EQ '2221030503' .
      " 总账 (销项税)
      CLEAR GS_ACCOUNTGL.
*       行项目号
      GS_ACCOUNTGL-ITEMNO_ACC = GS_ITEM-BUZEI.
*       科目
      GS_ACCOUNTGL-GL_ACCOUNT = GS_ITEM-HKONT.
*       项目文本
      GS_ACCOUNTGL-ITEM_TEXT = GS_ITEM-SGTXT.
*       WBS
      GS_ACCOUNTGL-WBS_ELEMENT = GS_ITEM-POSID.
*       分配
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = L_KUNNR
        IMPORTING
          OUTPUT = GS_ACCOUNTGL-ALLOC_NMBR.
*         GS_ACCOUNTGL-ALLOC_NMBR = L_KUNNR .

      APPEND GS_ACCOUNTGL TO GT_ACCOUNTGL.

      CLEAR GS_CURRENCYAMOUNT.
      GS_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
      GS_CURRENCYAMOUNT-CURRENCY = GS_ITEM-WAERS.
*       金额
      GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-OPREIS2.
      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '50'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_CURRENCYAMOUNT-AMT_DOCCUR.
      ENDIF.

      APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

      " 记账码 & 反记账
      CLEAR GS_EXTENSION2.
      CLEAR GS_ZACCDOCUEXT.
      GS_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
      GS_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL.
*      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '40'.
*        WA_ZACCDOCUEXT-XNEGP = 'X'.
*      ENDIF.
      GS_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
      GS_EXTENSION2-VALUEPART1 = GS_ZACCDOCUEXT .
      APPEND GS_EXTENSION2 TO GT_EXTENSION2.

    ELSEIF GS_ITEM-BSCHL = '40' OR GS_ITEM-BSCHL = '50' AND GS_ITEM-HKONT NE '2221030503' .
      " 总账 (其它)
      CLEAR GS_ACCOUNTGL.
*       行项目号
      GS_ACCOUNTGL-ITEMNO_ACC = GS_ITEM-BUZEI.
*       科目
      GS_ACCOUNTGL-GL_ACCOUNT = GS_ITEM-HKONT.
*       项目文本
      GS_ACCOUNTGL-ITEM_TEXT = GS_ITEM-SGTXT.
*       物料
      GS_ACCOUNTGL-MATERIAL = GS_ITEM-MATNR.
*       本次开票数量
      GS_ACCOUNTGL-QUANTITY = GS_ITEM-MENGE3.
*       单位
      GS_ACCOUNTGL-BASE_UOM = GS_ITEM-MEINS.
*       WBS
      GS_ACCOUNTGL-WBS_ELEMENT = GS_ITEM-POSID.

**       税码
*      GS_ACCOUNTGL-TAX_CODE = GS_ITEM-MWSKZ.

**销售订单，销售订单行项目 ADD BY HANDWY 2015-04-15
*      GS_ACCOUNTGL-SALES_ORD = GS_ITEM-VBELN.
*
*      GS_ACCOUNTGL-S_ORD_ITEM = GS_ITEM-POSNR.

*       分配号 - 销售订单号/行号 更改为 所有生成凭证的行项目的“分配”字段都需写入开票抬头的“销售订单号” 150602
*      IF WA_ITEM-VBELN IS NOT INITIAL.
*    "    CONCATENATE WA_ITEM-VBELN '/' WA_ITEM-POSNR INTO WA_ACCOUNTGL-ALLOC_NMBR.
*        WA_ACCOUNTGL-ALLOC_NMBR = WA_ITEM-VBELN .
*      ENDIF.
*      WA_ACCOUNTGL-ALLOC_NMBR = WA_HEAD-VBELN.              "150602

      APPEND GS_ACCOUNTGL TO GT_ACCOUNTGL.

      CLEAR GS_CURRENCYAMOUNT.
      GS_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
      GS_CURRENCYAMOUNT-CURRENCY = GS_ITEM-WAERS.
*       金额
      GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-OPREIS2.
      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '50'.
        GS_CURRENCYAMOUNT-AMT_DOCCUR = - GS_CURRENCYAMOUNT-AMT_DOCCUR.
      ENDIF.

      APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

      " 记账码 & 反记账
      CLEAR GS_EXTENSION2.
      CLEAR GS_ZACCDOCUEXT.
      GS_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
      GS_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL.
*      IF GS_ITEM-BSCHL = '11' OR GS_ITEM-BSCHL = '40'.
*        WA_ZACCDOCUEXT-XNEGP = 'X'.
*      ENDIF.
      GS_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
      GS_EXTENSION2-VALUEPART1 = GS_ZACCDOCUEXT .
      APPEND GS_EXTENSION2 TO GT_EXTENSION2.

    ENDIF.

*    CLEAR L_KATYP.
*    SELECT SINGLE KATYP
*      INTO L_KATYP
*      FROM CSKB
*      WHERE KSTAR = WA_ITEM-HKONT.
*    IF L_KATYP = '11'.
*      " 获利能力段
*      CLEAR WA_CRITERIA.
*      WA_CRITERIA-ITEMNO_ACC = WA_ITEM-BUZEI.
*      WA_CRITERIA-FIELDNAME = 'KAUFN'.  " 销售订单
*      WA_CRITERIA-CHARACTER = WA_ITEM-VBELN.
*      APPEND WA_CRITERIA TO IT_CRITERIA.
*      WA_CRITERIA-FIELDNAME = 'KDPOS'.  " 销售订单项目
*      WA_CRITERIA-CHARACTER = WA_ITEM-POSNR.
*      APPEND WA_CRITERIA TO IT_CRITERIA.
*      WA_CRITERIA-FIELDNAME = 'KNDNR'.  " 客户
*      WA_CRITERIA-CHARACTER = WA_VBAK-KUNNR.
*      APPEND WA_CRITERIA TO IT_CRITERIA.
*      WA_CRITERIA-FIELDNAME = 'ARTNR'.  " 生产
*      WA_CRITERIA-CHARACTER = WA_ITEM-MATNR.
*      APPEND WA_CRITERIA TO IT_CRITERIA.
*    ENDIF.
  ENDLOOP.

  CLEAR L_KUNNR .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_CLEAR .
  REFRESH: GT_ACCOUNTGL, GT_ACCOUNTRECEIVABLE, GT_CURRENCYAMOUNT, GT_CRITERIA, GT_VALUEFIELD, GT_EXTENSION2, GT_RETURN.
  CLEAR: GS_DOCUMENTHEADER ."GS_OBJ.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_HEAD_BELNR  text
*      <--P_GS_HEAD_GJAHR  text
*----------------------------------------------------------------------*
FORM FRM_CALL_BAPI  CHANGING P_BELNR TYPE BELNR_D
                            P_GJAHR TYPE GJAHR.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = GS_DOCUMENTHEADER
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
      OBJ_TYPE          = GS_OBJ-OBJ_TYPE
      OBJ_KEY           = GS_OBJ-OBJ_KEY
      OBJ_SYS           = GS_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL         = GT_ACCOUNTGL
      ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
      CRITERIA          = GT_CRITERIA
      VALUEFIELD        = GT_VALUEFIELD
*     EXTENSION1        =
      RETURN            = GT_RETURN
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      EXTENSION2        = GT_EXTENSION2
*     REALESTATE        =
*     ACCOUNTWT         =
    .

  READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM FRM_MESSAGE_DISPLAY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    " 回写生成的会计凭证号与会计凭证年度
    P_BELNR = GS_OBJ-OBJ_KEY(10).
    P_GJAHR = GS_OBJ-OBJ_KEY+14(4).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_DISPLAY .
  PERFORM FRM_MESSAGE_INITIAL.

  LOOP AT GT_RETURN INTO GS_RETURN.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.
*
*    write: / g_msg.

    "append all error warning messages to below function module
    PERFORM STORE_MESSAGES USING GS_RETURN-ID
                                 GS_RETURN-TYPE
                                 GS_RETURN-MESSAGE_V1
                                 GS_RETURN-MESSAGE_V2
                                 GS_RETURN-MESSAGE_V3
                                 GS_RETURN-MESSAGE_V4
                                 GS_RETURN-NUMBER.
  ENDLOOP.

  PERFORM FRM_MESSAGE_SHOW.
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
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETURN_ID  text
*      -->P_WA_RETURN_TYPE  text
*      -->P_WA_RETURN_MESSAGE_V1  text
*      -->P_WA_RETURN_MESSAGE_V2  text
*      -->P_WA_RETURN_MESSAGE_V3  text
*      -->P_WA_RETURN_MESSAGE_V4  text
*      -->P_WA_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM STORE_MESSAGES  USING P_MSGID
                          P_MSGTY
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                          P_TXTNR.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      ARBGB                  = P_MSGID
      MSGTY                  = P_MSGTY
      MSGV1                  = P_MSGV1
      MSGV2                  = P_MSGV2
      MSGV3                  = P_MSGV3
      MSGV4                  = P_MSGV4
      TXTNR                  = P_TXTNR
    EXCEPTIONS
      MESSAGE_TYPE_NOT_VALID = 1
      NOT_ACTIVE             = 2
      OTHERS                 = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MESSAGE_SHOW .
  "at last call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA L_EXIT_COMMAND TYPE BAL_S_EXCM.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO         = SPACE
    IMPORTING
      E_EXIT_COMMAND     = L_EXIT_COMMAND
    EXCEPTIONS
      INCONSISTENT_RANGE = 1
      NO_MESSAGES        = 2
      OTHERS             = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_ONF4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM HANDLE_ONF4  USING    P_FIELDNAME TYPE LVC_FNAME
                          PS_ROW_NO TYPE LVC_S_ROID
                          PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                          PT_BAD_CELLS TYPE LVC_T_MODI
                          P_DISPLAY.
  FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.

  CASE P_FIELDNAME.
*    WHEN 'ZTERM'.
*      READ TABLE IT_ITEM INTO WA_ITEM INDEX PS_ROW_NO-ROW_ID.
*      IF SY-SUBRC = 0.
*        PERFORM SUB_HELP_ZTERM CHANGING WA_ITEM-ZTERM.
*        IF WA_ITEM-ZTERM IS NOT INITIAL.
*          MODIFY IT_ITEM FROM WA_ITEM INDEX PS_ROW_NO-ROW_ID.
**          perform frm_refresh_alv.
*        ENDIF.
*      ENDIF.
    WHEN 'HKONT'.
      READ TABLE GT_ITEM INTO GS_ITEM INDEX PS_ROW_NO-ROW_ID.
      IF SY-SUBRC = 0.
        PERFORM SUB_HELP_HKONT CHANGING GS_ITEM-HKONT.
        IF GS_ITEM-HKONT IS NOT INITIAL.
          MODIFY GT_ITEM FROM GS_ITEM INDEX PS_ROW_NO-ROW_ID.

          " Trigger data changed event
          ASSIGN PR_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
          LW_MOD_CELL-ROW_ID = PS_ROW_NO-ROW_ID.
          LW_MOD_CELL-SUB_ROW_ID = PS_ROW_NO-SUB_ROW_ID.
          LW_MOD_CELL-FIELDNAME = 'HKONT'.
          LW_MOD_CELL-VALUE = GS_ITEM-HKONT.
          APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

*          perform frm_refresh_alv.
        ENDIF.
      ENDIF.
*    WHEN 'MWSKZ'.
*      READ TABLE IT_ITEM INTO WA_ITEM INDEX PS_ROW_NO-ROW_ID.
*      IF SY-SUBRC = 0.
*        PERFORM SUB_HELP_MWSKZ CHANGING WA_ITEM-MWSKZ.
*        IF WA_ITEM-MWSKZ IS NOT INITIAL.
*          MODIFY IT_ITEM FROM WA_ITEM INDEX PS_ROW_NO-ROW_ID.
**          perform frm_refresh_alv.
*        ENDIF.
*      ENDIF.

  ENDCASE.

**  Inform ALV Grid that event 'onf4' has been processed
  PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.           "告知F4动作结束
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_ITEM_HKONT  text
*----------------------------------------------------------------------*
FORM SUB_HELP_HKONT  CHANGING P_HKONT.
  SET PARAMETER ID 'BUK' FIELD GS_HEAD-BUKRS.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_HKONT
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED_FINISHED  USING    P_E_MODIFIED
                                            P_ET_GOOD_CELLS TYPE  LVC_T_MODI.
  DATA: LW_CELL TYPE LVC_S_MODI.

  " 通过科目带出描述
  READ TABLE P_ET_GOOD_CELLS INTO LW_CELL WITH KEY FIELDNAME = 'HKONT'.
  IF SY-SUBRC = 0.
    READ TABLE GT_ITEM ASSIGNING <FS_ITEM> INDEX LW_CELL-ROW_ID.
    IF SY-SUBRC = 0.
      SELECT SINGLE TXT20
        INTO <FS_ITEM>-HKTXT
        FROM SKAT
       WHERE SAKNR = <FS_ITEM>-HKONT .
    ENDIF.
  ENDIF.

  " 通过客户带出描述
  READ TABLE P_ET_GOOD_CELLS INTO LW_CELL WITH KEY FIELDNAME = 'KUNNR'.
  IF SY-SUBRC = 0.
    READ TABLE GT_ITEM ASSIGNING <FS_ITEM> INDEX LW_CELL-ROW_ID.
    IF SY-SUBRC = 0.
      SELECT SINGLE NAME1
        INTO <FS_ITEM>-KNAME
        FROM KNA1
       WHERE KUNNR = <FS_ITEM>-KUNNR .
    ENDIF.
  ENDIF.

  " 自动计算凭证余额
  CLEAR GS_HEAD-OPREIS .
  LOOP AT GT_ITEM INTO GS_ITEM .
    IF GS_ITEM-BSCHL = '01' OR GS_ITEM-BSCHL = '40'  .
      GS_HEAD-OPREIS = GS_HEAD-OPREIS + GS_ITEM-OPREIS2 .
    ELSEIF GS_ITEM-BSCHL = '50' OR GS_ITEM-BSCHL = '11' .
      GS_HEAD-OPREIS = GS_HEAD-OPREIS - GS_ITEM-OPREIS2 .
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ALV_EDIT  text
*----------------------------------------------------------------------*
FORM FRM_CHANGE_EDIT_MODE  USING    P_ALV_EDIT TYPE INT4.
  CALL METHOD GR_ALVGRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_ALV_EDIT.

  "切换模式后刷新ALV
  PERFORM FRM_REFRESH_ALV.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_KPJL_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_KPJL_REPORT .
  PERFORM FRM_KPJL_GET_DATA.

  PERFORM INIT_LAYOUT2.              "设置输出格式
*  PERFORM INIT_SORT.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0002'.             "设置变式控制
  PERFORM FRM_INIT_LVC_KPJL.        " 初始化内表结构/ALV显示结构
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_KPJL
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GS_LAYOUT
                           GS_VARIANT
                           GS_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_KPJL_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_KPJL_GET_DATA .

  SELECT *
    INTO TABLE GT_ZFI043
    FROM ZFI043
    WHERE BUKRS = P_BUKRS
      AND PSPID IN S_PSPID .

  CHECK GT_ZFI043 IS NOT INITIAL.

  DATA: BEGIN OF LS_PROJ ,
          PSPNR TYPE PROJ-PSPNR,
          PSPID TYPE PROJ-PSPID,
          POST1 TYPE PROJ-POST1,
        END OF LS_PROJ .
  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  SELECT PSPNR
         PSPID
         POST1
    INTO CORRESPONDING FIELDS OF TABLE LT_PROJ
    FROM PROJ
     FOR ALL ENTRIES IN GT_ZFI043
   WHERE PSPID = GT_ZFI043-PSPID .

  DATA: BEGIN OF LS_BKPF,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          BKTXT TYPE BKPF-BKTXT,
          STBLG TYPE BKPF-STBLG,
          STJAH TYPE BKPF-STJAH,
        END OF LS_BKPF .
  DATA LT_BKPF LIKE TABLE OF LS_BKPF .
  SELECT BUKRS
         BELNR
         GJAHR
         BKTXT
         STBLG
         STJAH
    INTO CORRESPONDING FIELDS OF TABLE LT_BKPF
    FROM BKPF
     FOR ALL ENTRIES IN GT_ZFI043
   WHERE BUKRS = GT_ZFI043-BUKRS
     AND BELNR = GT_ZFI043-BELNR
     AND GJAHR = GT_ZFI043-GJAHR .
  IF LT_BKPF IS NOT INITIAL .
    DATA: LT_BSEG TYPE TABLE OF BSEG,
          LS_BSEG TYPE BSEG.
    SELECT *
      INTO TABLE LT_BSEG
      FROM BSEG
       FOR ALL ENTRIES IN LT_BKPF
     WHERE BUKRS = LT_BKPF-BUKRS
       AND BELNR = LT_BKPF-BELNR
       AND GJAHR = LT_BKPF-GJAHR
       AND HKONT LIKE '1122%' .
    DATA: BEGIN OF LS_BSEG_COL ,
            BUKRS    TYPE BSEG-BUKRS,
            BELNR    TYPE BSEG-BELNR,
            GJAHR(4) TYPE C,
            DMBTR    TYPE BSEG-DMBTR,
          END OF LS_BSEG_COL .
    DATA LT_BSEG_COL LIKE TABLE OF LS_BSEG_COL .
    LOOP AT LT_BSEG INTO LS_BSEG .
      MOVE-CORRESPONDING LS_BSEG TO LS_BSEG_COL .
      LS_BSEG_COL-GJAHR = LS_BSEG-GJAHR .
      IF LS_BSEG-SHKZG = 'S' .
      ELSEIF LS_BSEG-SHKZG = 'H' .
        LS_BSEG_COL-DMBTR = - LS_BSEG_COL-DMBTR .
      ENDIF.
      COLLECT LS_BSEG_COL INTO LT_BSEG_COL .
      CLEAR LS_BSEG .
      CLEAR LS_BSEG_COL .
    ENDLOOP.
  ENDIF.

  "IT02 ADD END 150702
  LOOP AT GT_ZFI043 INTO GS_ZFI043.
    CLEAR GS_KPJL.
    MOVE-CORRESPONDING GS_ZFI043 TO GS_KPJL.

    READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPID = GS_ZFI043-PSPID .
    IF SY-SUBRC = 0.
      GS_KPJL-POST1 = LS_PROJ-POST1.
    ENDIF.
    READ TABLE LT_BSEG_COL INTO LS_BSEG_COL WITH KEY BUKRS = GS_ZFI043-BUKRS
                                                     BELNR = GS_ZFI043-BELNR
                                                     GJAHR = GS_ZFI043-GJAHR .
    IF SY-SUBRC = 0 .
      GS_KPJL-DMBTR = LS_BSEG_COL-DMBTR .
    ENDIF.
    " 取抬头文本
    READ TABLE LT_BKPF INTO LS_BKPF WITH KEY BUKRS = GS_ZFI043-BUKRS
                                             BELNR = GS_ZFI043-BELNR
                                             GJAHR = GS_ZFI043-GJAHR .
    IF SY-SUBRC = 0.
      GS_KPJL-BKTXT = LS_BKPF-BKTXT.
      GS_KPJL-STBLG = LS_BKPF-STBLG .
      GS_KPJL-STJAH = LS_BKPF-STJAH .
    ENDIF.

    APPEND GS_KPJL TO GT_KPJL.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT2 .
  CLEAR GS_LAYOUT .

  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-BOX_FNAME = 'SEL'.
  GS_LAYOUT-CWIDTH_OPT  = 'X'.
  GS_LAYOUT-SEL_MODE = 'A'.
*  gw_layout-edit_mode = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
*  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC_KPJL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC_KPJL .
  REFRESH GT_LVC .

  INIT_FIELDCAT2 'BUKRS' '' '' '' '' 'BSEG' 'BUKRS' ''.
  INIT_FIELDCAT2 'BELNR' '' '' '' '' 'BKPF' 'BELNR' ''.
  INIT_FIELDCAT2 'GJAHR' '' '' '' '' 'BKPF' 'GJAHR' ''.
  INIT_FIELDCAT2 'BUDAT' '' '' '' '' 'BKPF' 'BUDAT' ''.
  INIT_FIELDCAT2 'PSPID' '' '' '' '' 'PROJ' 'PSPID' ''.
  INIT_FIELDCAT2 'POST1' '' '' '' '' 'PROJ' 'POST1' ''.
  INIT_FIELDCAT2 'DMBTR' '' '' 'WAERS' '' 'BSEG' 'DMBTR' ''.
  INIT_FIELDCAT2 'BKTXT' '摘要' '' '' '' 'BSEG' 'BKTXT' ''.
  INIT_FIELDCAT2 'STBLG' '冲销凭证' '' '' '' 'BKPF' 'STBLG' ''.
  INIT_FIELDCAT2 'STJAH' '冲销年度' '' '' '' 'BKPF' 'STJAH' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = GT_EVENTS[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_KPJL  text
*      -->P_2386   text
*      -->P_2387   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT  TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA:L_INDEX TYPE SY-TABIX.

  CASE R_UCOMM.
    WHEN '&IC1'."双击
*      read table it_data into wa_data index rs_selfield-tabindex.
*      if sy-subrc = 0.
*        set parameter id 'XXXX' field wa_data-matnr.   "选择屏字段ID
*        call transaction 'XXXX' and skip first screen."填T-code
*      endif.
*    when 'PRINT'."打印
*      perform frm_print_select.
*    when '&ZALL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = 'X'.
*        modify it_data from wa_data index l_index.
*      endloop.
*    when '&ZSAL'.
*      loop at it_data into wa_data.
*        l_index = sy-tabix.
*        wa_data-box = ''.
*        modify it_data from wa_data index l_index.
*      endloop.
    WHEN 'REVERSAL'.
      PERFORM FRM_ACC_REVERSAL.
    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ACC_REVERSAL .
  READ TABLE GT_KPJL TRANSPORTING NO FIELDS WITH KEY SEL = 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE '请先选择需要冲销的凭证！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA: L_COUNT TYPE I.
  CLEAR L_COUNT.
  LOOP AT GT_KPJL INTO GS_KPJL WHERE SEL = 'X'.
    L_COUNT = L_COUNT + 1.
  ENDLOOP.
  IF L_COUNT > 1.
    MESSAGE '请仅选择一条凭证进行冲销！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " 取冲销原因和过账日期
  CLEAR GS_REVERSAL.
  PERFORM FRM_GET_REV_REASON.

  IF GS_REVERSAL-REASON_REV IS INITIAL.
    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM FRM_MESSAGE_INITIAL.
  CLEAR G_SUC.

  READ TABLE GT_KPJL INTO GS_KPJL WITH KEY SEL = 'X'.
  IF SY-SUBRC = 0.
    " 冲销凭证1
    CLEAR GS_BKPF .
    SELECT SINGLE *
      INTO GS_BKPF
      FROM BKPF
     WHERE BUKRS = GS_KPJL-BUKRS
       AND BELNR = GS_KPJL-BELNR
       AND GJAHR = GS_KPJL-GJAHR.

    PERFORM FRM_REV_PREP.
    PERFORM FRM_REVERSAL_BAPI.

*    " 冲销凭证2
*    CLEAR GS_BKPF .
*    SELECT SINGLE *
*      INTO GS_BKPF
*      FROM BKPF
*     WHERE BUKRS = GS_KPJL-BUKRS
*       AND BELNR = GS_KPJL-REFBE
*       AND GJAHR = GS_KPJL-REFGJ.

*    PERFORM FRM_REV_PREP.
*    PERFORM FRM_REVERSAL_BAPI.
  ENDIF.

  IF G_SUC = 'X'.
*    read table it_kpjl into wa_kpjl with key box = 'X'.
*    if sy-subrc = 0.
*      wa_kpjl-sfncx = 'X'.
*      modify it_kpjl from wa_kpjl index sy-tabix transporting sfncx.
*    endif.

    DATA: L_BUKRS TYPE ZFI043-BUKRS,
          L_BELNR TYPE ZFI043-BELNR,
          L_GJAHR TYPE ZFI043-GJAHR.

    L_BUKRS = GS_KPJL-BUKRS.
    L_BELNR = GS_KPJL-BELNR.
    L_GJAHR = GS_KPJL-GJAHR.

    LOOP AT GT_KPJL INTO GS_KPJL WHERE BUKRS = L_BUKRS
                                   AND BELNR = L_BELNR
                                   AND GJAHR = L_GJAHR.
      GS_KPJL-STBLG = GS_OBJ-OBJ_KEY+0(10) .
      GS_KPJL-STJAH = GS_OBJ-OBJ_KEY+14(4) .
      MODIFY GT_KPJL FROM GS_KPJL TRANSPORTING STBLG STJAH .
    ENDLOOP.

    MODIFY ZFI043 FROM GS_KPJL.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  PERFORM FRM_MESSAGE_SHOW.

*  perform frm_refresh_alv.
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
*&      Form  FRM_REV_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REV_PREP .
  CLEAR GS_REVERSAL-OBJ_TYPE .
  CLEAR GS_REVERSAL-OBJ_KEY .
  CLEAR GS_REVERSAL-OBJ_KEY_R .

  GS_REVERSAL-OBJ_TYPE  = GS_BKPF-AWTYP.
  GS_REVERSAL-OBJ_KEY   = GS_BKPF-AWKEY.
  GS_REVERSAL-OBJ_KEY_R = GS_BKPF-AWKEY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSAL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_REVERSAL_BAPI .

  REFRESH GT_RETURN .
  CLEAR GS_RETURN .
  CLEAR GS_OBJ .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      REVERSAL = GS_REVERSAL
      BUS_ACT  = 'RFBU'
    IMPORTING
      OBJ_TYPE = GS_OBJ-OBJ_TYPE
      OBJ_KEY  = GS_OBJ-OBJ_KEY
      OBJ_SYS  = GS_OBJ-OBJ_SYS
    TABLES
      RETURN   = GT_RETURN.

  READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    G_SUC = 'X'.
  ENDIF.

  LOOP AT GT_RETURN INTO GS_RETURN.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.

*    write: / g_msg.
    PERFORM STORE_MESSAGES USING GS_RETURN-ID
                                 GS_RETURN-TYPE
                                 GS_RETURN-MESSAGE_V1
                                 GS_RETURN-MESSAGE_V2
                                 GS_RETURN-MESSAGE_V3
                                 GS_RETURN-MESSAGE_V4
                                 GS_RETURN-NUMBER.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_ZFI043
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SAVE_ZFI043 .
  " 保存到自建表记录两张凭证的对应关系等
  REFRESH GT_ZFI043.
  CLEAR GS_ZFI043.
  GS_ZFI043-BUKRS = GS_HEAD-BUKRS.
  GS_ZFI043-BUDAT = GS_HEAD-BUDAT.
  GS_ZFI043-PSPID = GS_HEAD-PSPID.
  GS_ZFI043-BELNR = GS_HEAD-BELNR.
  GS_ZFI043-GJAHR = GS_HEAD-GJAHR.
*  LOOP AT GT_ITEM INTO GS_ITEM  .
*    WA_ZFI008-BUZEI = WA_ITEM-BUZEI.
*    WA_ZFI008-VBELN = WA_ITEM-VBELN.
*    WA_ZFI008-POSNR = WA_ITEM-POSNR.
*    WA_ZFI008-WRBTR = WA_ITEM-WRBTR.
*    WA_ZFI008-WAERS = WA_ITEM-WAERS.
*    WA_ZFI008-BSCHL = WA_ITEM-BSCHL."IT02 ADD 150623 新增保存记账码
**    IF wa_item-hkont = '6001019901'. ”it02 20170713 注释
**      wa_zfi008-is_adjust = 'X'.   "IT02 ADD 科目为6001019901主营业务收入-非关联方-调整,科目调整设为X ,否则为空
**    ELSE.
**      wa_zfi008-is_adjust = ''.
**    ENDIF.
*    "add it02 20170713 begin
*    READ TABLE GT_SRYS INTO GS_SRYS WITH KEY HKONT = WA_ITEM-HKONT
*                                                 BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      WA_ZFI008-IS_ADJUST = 'X'.
*    ELSE.
*      WA_ZFI008-IS_ADJUST = ''.
*    ENDIF.
*    "add it02 20170713 end
*    APPEND WA_ZFI008 TO IT_ZFI008.
*  ENDLOOP.

  IF GS_ZFI043 IS NOT INITIAL.
    MODIFY ZFI043 FROM GS_ZFI043.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.
ENDFORM.
