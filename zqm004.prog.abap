REPORT zqm004.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160803
*& Request       :
*& Descriptions  : 进料检验报告
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES:qals,makt,lfa1,lfb1 ,zqm004_dyrz.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,

        mblnr      TYPE qals-mblnr,                           "物料凭证
        mjahr      TYPE qals-mjahr,                           "物料凭证年度
        zeile      TYPE qals-zeile,                           "物料凭证项目
        budat      TYPE qals-budat,                           "到货日期
        lifnr      TYPE qals-lifnr,                           "供应商
        name1      TYPE name1,                                "供应商名称
        ebeln      TYPE qals-ebeln,                            "采购凭证
        ebelp      TYPE qals-ebelp,                            "采购凭证项目
        kdauf      TYPE qals-kdauf,                         "销售订单
        kdpos      TYPE qals-ls_kdpos,                      "销售订单项目
        werk       TYPE qals-werk,                             "工厂
        lagortchrg TYPE qals-lagortchrg,                      "库存地点
        prueflos   TYPE qals-prueflos,                       "检验批
        matnr      TYPE qals-matnr,                           "物料
        maktx      TYPE makt-maktx,                           "物料描述
        losmenge   TYPE qals-losmenge,                       "检验批数量
        mengeneinh TYPE qals-mengeneinh,                     "检验批数量的基本单位
        herkunft   TYPE qals-herkunft,                       "检验批来源
        enstehdat  TYPE qals-enstehdat,                     "批量建立时间
        ersteller  TYPE qals-ersteller,                     "创建者
        is_yes     TYPE  c ,                                "是否已打印
        zsel,
      END OF ty_data .

TYPES:BEGIN OF ty_print,
        xh         TYPE i ,                                    "序号
        mblnr      TYPE qals-mblnr,                           "物料凭证
        mjahr      TYPE qals-mjahr,                           "物料凭证年度
        zeile      TYPE qals-zeile,                           "物料凭证项目
        budat      TYPE qals-budat,                           "到货日期
        lifnr      TYPE qals-lifnr,                           "供应商
        name1      TYPE name1,                                "供应商名称
        ebeln      TYPE qals-ebeln,                            "采购凭证
        ebelp      TYPE qals-ebelp,                           "采购凭证行项目
        prueflos   TYPE qals-prueflos,                       "检验批
        matnr      TYPE qals-matnr,                           "物料
        maktx      TYPE makt-maktx,                           "物料描述
        lagortchrg TYPE qals-lagortchrg,                      "库存地点
        losmenge   TYPE qals-losmenge,                       "检验批数量
        mengeneinh TYPE qals-mengeneinh,                     "检验批数量的基本单位
        losmenge_c TYPE char20,                              "检验批数量
      END OF ty_print .

TYPES:BEGIN OF ty_matnr,
        werks TYPE werks_d,   "工厂
        matnr TYPE matnr,     "物料号
        maktx TYPE maktx,     "物料描述
      END OF ty_matnr .

TYPES:BEGIN OF ty_lifnr,
        bukrs TYPE lfb1-bukrs,  "公司代码
        lifnr TYPE lfa1-lifnr,   "供应商编码
        name1 TYPE lfa1-name1,  "供应商名称
      END OF ty_lifnr .

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_print TYPE TABLE OF ty_print,
     gs_print TYPE ty_print.

DATA:lt_prt TYPE TABLE OF ty_print,
     ls_prt TYPE ty_print.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_lifnr TYPE TABLE OF ty_lifnr,
     gs_lifnr TYPE ty_lifnr.

DATA:gt_zqm004_dyrz TYPE TABLE OF  zqm004_dyrz,
     gs_zqm004_dyrz TYPE zqm004_dyrz.

DATA:gt_dyrz TYPE TABLE OF  zqm004_dyrz,
     gs_dyrz TYPE zqm004_dyrz.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

DATA:g_xh  TYPE i .     "全局序号


*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

*打印参数变量
DATA: control    TYPE ssfctrlop,
      ntotalline TYPE i,
      npageline  TYPE i VALUE 9,
      p_index    LIKE sy-tabix.
DATA: emptycount      TYPE i VALUE 0,  "空行数.
      ncurrline       TYPE i,      "中间变量
      job_output_info TYPE ssfcrescl.
DATA: g_name TYPE rs38l_fnam.
DATA:l_formname TYPE tdsfname  VALUE 'ZSFQM004'.
DATA l_line TYPE i. "统计打印的行进行补行
DATA g_line TYPE i. "设定换页行数
DATA name   TYPE char20. "打印人

************************************************************************
*      DEFINITION
************************************************************************
DEFINE init_fieldcat.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

  if gw_lvc-fieldname = 'LOSMENGE'.
      gw_lvc-qfieldname = 'MENGENEINH'.
  endif.
*  IF gw_lvc-fieldname = 'LYTS'
*  OR gw_lvc-fieldname = 'BCLLS'.
*     gw_lvc-NO_ZERO = 'X'.
*  ENDIF.

  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gw_layout        TYPE lvc_s_layo,                    "alv的格式
      gw_variant       TYPE disvariant,
      gw_grid_settings TYPE lvc_s_glay,
      gw_lvc           TYPE lvc_s_fcat,
      gw_sort          TYPE lvc_s_sort,
      gw_grid_setting  TYPE lvc_s_glay,
      g_repid          LIKE sy-repid,                      "SY-REPID 指 当前的主程序
      gt_events        TYPE slis_t_event WITH HEADER LINE, "保存AVL事件
      gw_events        LIKE LINE OF gt_events.
DATA: gt_exclude TYPE slis_t_extab,
      gs_exclude TYPE slis_extab.

DATA: gr_alvgrid TYPE REF TO cl_gui_alv_grid.

DATA: gt_rows TYPE lvc_t_row,
      gt_roid TYPE lvc_t_roid,
      wa_rows TYPE lvc_s_row,
      wa_roid TYPE lvc_s_roid.
DATA: gs_variant TYPE disvariant.

DATA: gw_istable TYPE lvc_s_stbl.

DATA g_edit TYPE c VALUE 'X'. "控制不可编辑

DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.


************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_mblnr   FOR qals-mblnr,"物料凭证
                s_zeile   FOR qals-zeile,"物料凭证项目
                s_werk   FOR qals-werk,"工厂
                s_jyply   FOR qals-herkunft,  "检验批来源
                s_jyp   FOR qals-prueflos,"检验批
                s_pljl   FOR qals-enstehdat DEFAULT sy-datum,"批量建立时间
                s_cjz   FOR qals-ersteller,"创建者
                s_matnr   FOR qals-matnr,"物料
                s_charg  FOR qals-charg."批次
PARAMETERS: p_ydy AS  CHECKBOX,
            p_wdy AS  CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_auth_check.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .

  SELECT werks
    FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE lt_t001w
  WHERE werks IN s_werk.
  LOOP AT lt_t001w WHERE werks IN s_werk.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*            ID 'ACTVT' FIELD '__________'
             ID 'WERKS' FIELD lt_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH lt_t001w-werks.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .


  SELECT prueflos mjahr   mblnr zeile budat lifnr  ebeln ebelp kdauf kdpos werk lagortchrg
         matnr   losmenge mengeneinh herkunft  enstehdat ersteller
 INTO CORRESPONDING FIELDS OF TABLE gt_data
 FROM qals

 WHERE mblnr IN s_mblnr
AND    zeile IN s_zeile
AND    werk IN s_werk
AND    herkunft IN s_jyply
AND    prueflos IN s_jyp
AND    enstehdat IN s_pljl
AND    ersteller IN s_cjz
AND    matnr IN s_matnr
AND    charg IN s_charg .

  SORT gt_data BY mblnr zeile .




  SELECT a~matnr  a~werks b~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
    FROM marc AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    WHERE a~werks IN s_werk
    AND   a~matnr IN s_matnr
    AND   b~spras EQ sy-langu.

  SORT gt_matnr BY werks matnr .

  SELECT a~lifnr a~bukrs b~name1
    INTO CORRESPONDING FIELDS OF TABLE gt_lifnr
    FROM lfb1 AS a
    INNER JOIN  lfa1 AS b
    ON a~lifnr = b~lifnr
    WHERE a~bukrs IN s_werk .

  SORT gt_lifnr BY bukrs  lifnr .

  SELECT * INTO TABLE gt_zqm004_dyrz
    FROM zqm004_dyrz
    WHERE dy_prueflos IN s_jyp .
  SORT gt_zqm004_dyrz BY dy_prueflos  .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .
  DATA:l_tabix TYPE sy-tabix .
  LOOP AT gt_data ASSIGNING <fs_data> .
    l_tabix  = sy-tabix .
    READ TABLE  gt_zqm004_dyrz INTO gs_zqm004_dyrz WITH KEY dy_prueflos = <fs_data>-prueflos BINARY SEARCH.
    IF sy-subrc EQ 0 .
      <fs_data>-is_yes = '是'.
    ELSE.
      <fs_data>-is_yes = '否'.
    ENDIF.

    IF p_ydy NE 'X' AND <fs_data>-is_yes = '是'.
      DELETE gt_data INDEX l_tabix.
      CONTINUE .
    ENDIF.

    IF p_wdy NE 'X' AND <fs_data>-is_yes = '否'.
      DELETE gt_data INDEX l_tabix.
      CONTINUE .
    ENDIF.
    "读取物料描述
    READ TABLE gt_matnr  INTO gs_matnr
                          WITH KEY werks = <fs_data>-werk matnr = <fs_data>-matnr BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-maktx = gs_matnr-maktx .

    ENDIF.
    "读取供应商明长城
    READ TABLE gt_lifnr INTO gs_lifnr
                        WITH KEY bukrs = <fs_data>-werk lifnr = <fs_data>-lifnr BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_data>-name1 = gs_lifnr-name1.

    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_show .
  PERFORM init_layout.             "设置输出格式
  PERFORM init_sort.               "设置排序、合计
  PERFORM init_variant.            "设置变式控制
  PERFORM frm_init_lvc.
  PERFORM frm_exclude.
  PERFORM frm_build_event.
  gw_grid_settings-edt_cll_cb = 'X'.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            gt_data
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           gw_layout
                           gw_variant
                           gw_grid_settings.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_layout .
  gw_layout-zebra         = 'X'.
  gw_layout-cwidth_opt    = 'X'.
  gw_layout-box_fname     = 'ZSEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_sort .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_variant .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'MBLNR'            '物料凭证'        '' '' '' '' '' 'QALS' 'MBLNR'.
  init_fieldcat 'MJAHR'            '物料凭证年度'        '' '' '' '' '' 'QALS' 'MJAHR'.
  init_fieldcat 'ZEILE'            '物料凭证项目'    '' '' '' '' '' 'QALS' 'ZEILE'.
  init_fieldcat 'LIFNR'            '供应商'          '' '' '' '' '' 'QALS' 'LIFNR'.
  init_fieldcat 'EBELN'            '采购凭证'        '' '' '' '' '' 'QALS' 'EBELN'.
  init_fieldcat 'EBELP'            '采购凭证项目'    '' '' '' '' '' 'QALS' 'EBELP'.
  init_fieldcat 'KDAUF'            '销售订单'        '' '' '' '' '' 'QALS' 'KDAUF'.
  init_fieldcat 'KDPOS'            '销售订单项目'    '' '' '' '' '' 'QALS' 'KDPOS'.
  init_fieldcat 'WERK'             '工厂'           '' '' '' '' '' 'QALS' 'WERK'.
  init_fieldcat 'LAGORTCHRG'       '库存地点'           '' '' '' '' '' 'QALS' 'LAGORTCHRG'.
  init_fieldcat 'PRUEFLOS'         '检验批'         '' '' '' '' '' 'QALS' 'PRUEFLOS'.
  init_fieldcat 'MATNR'            '物料'           '' '' '' '' '' 'QALS' 'MATNR'.
  init_fieldcat 'MAKTX'            '物料描述'       '' '' '' '' '' 'MAKT' 'MAKTX'.
  init_fieldcat 'LOSMENGE'         '检验批数量'     '' '' '' '' '' 'QALS' 'LOSMENGE'.
  init_fieldcat 'MENGENEINH'         '检验批数量的基本计量单位'     '' '' '' '' '' 'QALS' 'LOSMENGE'.
  init_fieldcat 'HERKUNFT'         '检验批来源'     '' '' '' '' '' 'QALS' 'HERKUNFT'.
  init_fieldcat 'ENSTEHDAT'        '批量建立时间'   '' '' '' '' '' 'QALS' 'ENSTEHDAT'.
  init_fieldcat 'ERSTELLER'        '创建者'         '' '' '' '' '' 'QALS' 'ERSTELLER'.
  init_fieldcat 'IS_YES'           '是否已打印'     '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_exclude .
  REFRESH gt_exclude.
  CLEAR gs_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_event .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0433   text
*      -->P_0434   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM frm_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_data
                USING pu_status
                      pu_ucomm
                      pw_layout TYPE lvc_s_layo
                      pw_variant TYPE disvariant
                      pw_grid_settings TYPE lvc_s_glay.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = pu_status
      i_callback_user_command  = pu_ucomm
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      i_grid_settings          = pw_grid_settings
      is_layout_lvc            = pw_layout
      it_fieldcat_lvc          = pt_lvc[]
      it_excluding             = gt_exclude
*     IT_SPECIAL_GROUPS_LVC    =
      it_sort_lvc              = pt_sort[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = pw_variant
 "    it_events                = gt_events[]
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS\

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表
  DATA l_subrc TYPE sy-subrc.
  CLEAR l_subrc.

  "CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.


*打印
    WHEN '&PRNT'.

      PERFORM frm_print_data.





  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 6,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFQM004'.
  CLEAR:gs_print,gt_print[],
        ls_prt,lt_prt[].

  FIELD-SYMBOLS <lw_prt>  TYPE ty_print.
  LOOP AT gt_data INTO gs_data  WHERE zsel EQ 'X'.
    CLEAR:gs_print .
    MOVE-CORRESPONDING gs_data TO gs_print .
    APPEND gs_print TO gt_print .
  ENDLOOP.

  SORT  gt_print BY mblnr zeile .

  IF gt_print IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.

  CHECK gt_print IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "SMARTFORMS的名字
    IMPORTING
      fm_name            = g_name                "对应的SMARTFORMS的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open = 'X'.
  control-no_close = 'X'.
* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT gt_print ASSIGNING <lw_prt>.
    g_xh =  g_xh + 1 .  "以物料凭证号维度 统计序列数
    AT NEW mblnr.
      CLEAR ls_prt .
      CLEAR lt_prt[] .
      "    LS_PRT = <lw_prt>.

    ENDAT.
    MOVE-CORRESPONDING <lw_prt> TO ls_prt .
    ls_prt-xh =  g_xh  .  "序号
    APPEND  ls_prt TO lt_prt.
    CLEAR:gs_dyrz.
    gs_dyrz-dy_prueflos = <lw_prt>-prueflos .
    gs_dyrz-dy_date = sy-datum .
    gs_dyrz-dy_time = sy-uzeit .
    gs_dyrz-dy_zh = sy-uname .
    APPEND gs_dyrz TO gt_dyrz .

    AT END OF mblnr.
*      DESCRIBE TABLE IT_PRT LINES NTOTALLINE.
*      NCURRLINE = NTOTALLINE MOD NPAGELINE.
*      IF  NCURRLINE > 0.
*        EMPTYCOUNT = NPAGELINE - NCURRLINE.
*        DO EMPTYCOUNT TIMES.
*          APPEND INITIAL LINE TO IT_PRT.
*        ENDDO.
*      ENDIF.

      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
   "      NPAGE_LINE         = NPAGELINE
*         W_HEAD             = LW_PRT
        TABLES
          t_item             = lt_prt[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        SORT gt_dyrz BY dy_prueflos .
        IF gt_dyrz IS NOT INITIAL.
          MODIFY zqm004_dyrz FROM TABLE gt_dyrz .
          LOOP AT gt_dyrz INTO gs_dyrz .
            READ TABLE gt_data ASSIGNING <fs_data> WITH KEY prueflos  = gs_dyrz-dy_prueflos  .
            IF sy-subrc EQ 0 .
              <fs_data>-is_yes = '是'.
            ENDIF.

          ENDLOOP.

        ENDIF.
        CLEAR:gt_dyrz[] ,gs_dyrz .
      ENDIF.
      CLEAR:g_xh .


    ENDAT.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   ERROR HANDLING
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF job_output_info-outputdone = 'X'.

  ENDIF.
ENDFORM.
