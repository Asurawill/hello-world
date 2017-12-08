REPORT zco012_2.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20161028
*& Request       :
*& Descriptions  : 公司间采购报表
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
TABLES:mkpf,mseg ,makt,zco012_1 .
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        ebeln  TYPE ebeln,             "采购订单
        ebelp  TYPE ebelp,             "采购订单行
        kdauf  TYPE kdauf,             "销售订单
        kdpos  TYPE kdpos,             "销售订单行
        xmms   TYPE string,            "项目描述
        matnr  TYPE matnr,             "物料编码
        maktx  TYPE maktx,             "物料描述
        bqjhsl TYPE menge_d,           "本期交货数量
        bqjhcb TYPE dmbtr,             "本期交货成本
        bqcbcy TYPE dmbtr,             "本期成本差异
        bqsjcb TYPE dmbtr,             "本期实际成本
        bqkpsl TYPE menge_d,           "本期开票数量
        zyywsr TYPE dmbtr,             "主营业务收入
        bqwkps TYPE menge_d,           "本期未开票数
        bqzycb TYPE dmbtr,             "本期主营业务成本
        bqgccb TYPE dmbtr,             "本期工程成本
        mfh    TYPE c,                 "免费行
        cggc   TYPE zco012_1-cggc ,    "采购工厂
        meins  TYPE meins,                                   "单位
        waers  TYPE waers,                                   "本文币
        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_hj,
        ebeln  TYPE ebeln,   "采购订单
        ebelp  TYPE c LENGTH 5,   "采购订单行
        kdauf  TYPE kdauf,   "销售订单
        kdpos  TYPE c LENGTH 6,    "销售订单行
        matnr  TYPE matnr,    "物料编码
        meins  TYPE meins,
        waers  TYPE waers,
        bqjhsl TYPE menge_d,  "本期交货数量
        bqjhcb TYPE dmbtr,   "本期交货成本
        bqcbcy TYPE dmbtr,    "本期成本差异
        bqkpsl TYPE menge_d,   "本期开票数量
        zyywsr TYPE dmbtr,      "主营业务收入
        bqzycb TYPE dmbtr,     "本期主营业务成本

      END OF ty_hj .

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF ty_matnr .


TYPES:BEGIN OF ty_xm,
        kdauf TYPE kdauf,   "销售订单
        xmms  TYPE string,  "项目描述
      END OF ty_xm .

TYPES:BEGIN OF ty_qm,
        ebeln  TYPE ebeln,
        ebelp  TYPE ebelp,
        ndqj   TYPE zco012_1-ndqj,
        bqwkps TYPE menge_d,   "本期未开票数
        bqgccb TYPE dmbtr,      "本期工程成本
      END OF ty_qm .

DATA:gt_qm TYPE TABLE OF ty_qm,
     gs_qm TYPE ty_qm.

DATA:gt_matnr   TYPE TABLE OF ty_matnr,
     gt_matnr_1 TYPE TABLE OF ty_matnr,
     gs_matnr   TYPE ty_matnr.

DATA:gt_xm TYPE TABLE OF ty_xm,
     gs_xm TYPE ty_xm.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_zco012_1 TYPE TABLE OF zco012_1,
     gs_zco012_1 TYPE zco012_1.

DATA:gt_hj TYPE TABLE OF ty_hj,
     gs_hj TYPE ty_hj.

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

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

  if gw_lvc-fieldname eq 'BQJHSL' " OR gw_lvc-fieldname eq 'BCRK'
     OR gw_lvc-fieldname eq 'BQKPSL'
     OR gw_lvc-fieldname eq 'BQWKPS' .
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'MEINS'.
  endif.

  if GW_LVC-FIELDNAME EQ 'BQJHCB' OR GW_LVC-FIELDNAME EQ 'BQCBCY' OR GW_LVC-FIELDNAME EQ 'BQSJCB'
    OR GW_LVC-FIELDNAME EQ 'ZYYWSR'  OR  GW_LVC-FIELDNAME EQ 'BQZYCB'
    OR GW_LVC-FIELDNAME EQ 'BQGCCB' .
    gw_lvc-CFIELDNAME = 'WAERS'.
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
PARAMETER: p_bukrs LIKE  zco012_1-bukrs .
SELECT-OPTIONS:
               " s_bukrs for ZCO012_1-bukrs,
                s_ndqj FOR zco012_1-ndqj,
                s_matnr FOR zco012_1-matnr,
                s_kdauf FOR zco012_1-kdauf.
SELECTION-SCREEN END OF BLOCK b1.


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
 " *权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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
  "查询已保存到ZCO012_1表的数据
  SELECT * INTO TABLE gt_zco012_1
    FROM zco012_1
    WHERE bukrs EQ p_bukrs
    AND   ndqj IN s_ndqj
    AND   matnr IN s_matnr
    AND   kdauf IN s_kdauf .
  SORT gt_zco012_1 BY ebeln ebelp  kdauf  kdpos   matnr .

  MOVE-CORRESPONDING gt_zco012_1 TO gt_qm.
  SORT gt_qm BY ebeln ASCENDING ebelp ASCENDING ndqj DESCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_qm COMPARING ebeln ebelp .

  LOOP AT gt_zco012_1 INTO gs_zco012_1 .
    CLEAR:gs_hj .
    gs_hj-ebeln = gs_zco012_1-ebeln.
    gs_hj-ebelp = gs_zco012_1-ebelp.
    gs_hj-kdauf = gs_zco012_1-kdauf.
    gs_hj-kdpos = gs_zco012_1-kdpos .
    gs_hj-matnr = gs_zco012_1-matnr .
    gs_hj-meins = gs_zco012_1-meins.
    gs_hj-waers = gs_zco012_1-waers .
    gs_hj-bqjhsl = gs_zco012_1-bqjhsl . "本期交货数量
    gs_hj-bqjhcb = gs_zco012_1-bqjhcb . "本期交货成本
    gs_hj-bqcbcy = gs_zco012_1-bqcbcy . "本期成本差异
    gs_hj-bqkpsl = gs_zco012_1-bqkpsl . "本期开票数量
    gs_hj-zyywsr = gs_zco012_1-zyywsr . "主营业务收入
    gs_hj-bqzycb = gs_zco012_1-bqzycb . "本期工程成本
    gs_hj-meins  = gs_zco012_1-meins.   "单位
    gs_hj-waers  = gs_zco012_1-waers.   "货币码
    COLLECT gs_hj INTO gt_hj .
  ENDLOOP.
  SORT gt_hj BY ebeln ebelp kdauf kdpos matnr .

  MOVE-CORRESPONDING gt_hj TO gt_matnr .

  delete gt_matnr where matnr is initial .

  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

  MOVE-CORRESPONDING gt_hj TO gt_xm .

  delete gt_xm where kdauf is initial .

  DELETE ADJACENT DUPLICATES FROM gt_xm COMPARING kdauf .

  sort gt_xm by kdauf .

  IF gt_matnr IS NOT INITIAL .
    SELECT a~matnr b~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr_1
    FROM marc AS a
    INNER JOIN makt AS b
    ON a~matnr  = b~matnr
    FOR ALL ENTRIES IN gt_matnr
    WHERE a~matnr = gt_matnr-matnr
    AND  a~werks = p_bukrs
    AND b~spras = '1'.
    SORT gt_matnr_1 BY matnr .
  ENDIF.

  "读取项目描述

  LOOP AT gt_xm INTO gs_xm .
    PERFORM selxmmc USING gs_xm-kdauf sy-langu CHANGING gs_xm-xmms.

    MODIFY gt_xm FROM gs_xm.
  ENDLOOP.




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

  LOOP AT gt_hj INTO gs_hj .
    CLEAR:gs_data .
    gs_data-ebeln = gs_hj-ebeln.
    gs_data-ebelp = gs_hj-ebelp.
    gs_data-kdauf = gs_hj-kdauf.
    gs_data-kdpos = gs_hj-kdpos.
    gs_data-matnr = gs_hj-matnr.
    gs_data-bqjhsl = gs_hj-bqjhsl.
    gs_data-bqjhcb = gs_hj-bqjhcb.
    gs_data-bqcbcy = gs_hj-bqcbcy.
    gs_data-bqkpsl = gs_hj-bqkpsl .
    gs_data-zyywsr = gs_hj-zyywsr.
    gs_data-bqzycb = gs_hj-bqzycb .
    gs_data-bqsjcb = gs_data-bqjhcb + gs_data-bqcbcy .
    gs_data-meins  = gs_hj-meins.
    gs_data-waers  = gs_hj-waers.
    READ TABLE gt_qm INTO gs_qm WITH KEY ebeln = gs_data-ebeln
                                         ebelp = gs_data-ebelp
                                         BINARY SEARCH.
    IF sy-subrc EQ 0 .
      gs_data-bqwkps = gs_qm-bqwkps . "本期未开票数
      gs_data-bqgccb = gs_qm-bqgccb . "本期工程成本
    ENDIF.
    READ TABLE gt_matnr_1 INTO gs_matnr WITH KEY matnr = gs_data-matnr
                                           BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-maktx = gs_matnr-maktx .
    ENDIF.
    READ TABLE gt_xm INTO gs_xm WITH KEY kdauf = gs_data-kdauf
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-xmms = gs_xm-xmms .
    ENDIF.

    READ TABLE gt_zco012_1 INTO gs_zco012_1 WITH KEY ebeln = gs_data-ebeln
                                                 ebelp = gs_data-ebelp
                                                 .
    IF sy-subrc EQ 0 .
      gs_data-mfh = gs_zco012_1-mfh . "免费行
      gs_data-cggc = gs_zco012_1-cggc . "采购工厂
    ENDIF.

    APPEND gs_data TO gt_data.

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
  init_fieldcat 'EBELN'            '采购订单'            '' '' '' '' '' '' ''.
  init_fieldcat 'EBELP'            '采购订单行'            '' '' '' '' '' '' ''.
  init_fieldcat 'KDAUF'            '销售订单'            '' '' '' '' '' '' ''.
  init_fieldcat 'KDPOS'            '销售订单行'            '' '' '' '' '' '' ''.
  init_fieldcat 'XMMS'            '项目描述'            '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'            '物料编码'            '' '' '' '' '' '' ''.
  init_fieldcat 'MAKTX'            '物料描述'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHSL'            '本期交货数量'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQJHCB'            '本期交货成本'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQCBCY'            '本期成本差异'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQSJCB'            '本期实际成本'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQKPSL'            '本期开票数量'            '' '' '' '' '' '' ''.
  init_fieldcat 'ZYYWSR'            '主营业务收入'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQWKPS'            '本期未开票数'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQZYCB'            '本期主营业务成本'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQGCCB'            '本期工程成本'            '' '' '' '' '' '' ''.
  init_fieldcat 'MFH'            '免费行'            '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'            '单位'            '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'            '本位币'            '' '' '' '' '' '' ''.
  init_fieldcat 'CGGC'            '采购工厂'            '' '' '' '' '' '' ''.

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
*      -->P_0407   text
*      -->P_0408   text
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
      it_events                = gt_events[]
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

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.
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
*
*  CASE r_ucomm.
*    WHEN '&IC1'.
*      "链接到生产订单确认事务：CO14
*     READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*       IF SY-SUBRC EQ 0 .
*           IF RS_SELFIELD-FIELDNAME = 'AUFNR'.
*              SET PARAMETER ID 'ANR' FIELD  GS_DATA-AUFNR .
*              CALL TRANSACTION 'CO14' AND SKIP  FIRST  SCREEN .
*            ENDIF.
*
*        ENDIF.
*
**打印
*    WHEN '&PRNT'.
*
*      PERFORM frm_print_data.
*
*
*
*
*
*  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_XM_KDAUF  text
*      -->P_SY_LANGU  text
*      <--P_GS_XM_XMMS  text
*----------------------------------------------------------------------*
FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = p_yy
      name                    = g_objname
      object                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    READ TABLE it_lines INTO wa_lines INDEX 1.
    IF sy-subrc = 0.
      p_xmmc = wa_lines-tdline.
    ENDIF.
  ENDIF.
ENDFORM.
