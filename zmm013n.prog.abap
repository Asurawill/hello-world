REPORT zmm013n.
*&---------------------------------------------------------------------*
*& Report  ZPS001
*&
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20161202
*& Request       :
*& Descriptions  : 库存周转率表
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
TABLES:mseg,mkpf,ekpo,s031,makt,mara.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        bukrs        TYPE mseg-bukrs,       "公司代码
        butxt        TYPE t001-butxt,       "公司名称
        werks        TYPE ekpo-werks,       "工厂
        name1        TYPE t001w-name1,      "工厂名称
        lgort        TYPE ekpo-lgort,       "库存地点
        lgobe        TYPE t001l-lgobe,      "库存地点描述
        matnr        TYPE ekpo-matnr,       "物料编码
        maktx        TYPE makt-maktx,       "物料描述
        matkl        TYPE mara-matkl,       "物料组
        mtart        TYPE mara-mtart,       "物料类型
        ratio        TYPE p DECIMALS 3,     "周转率
        zxhl         TYPE p DECIMALS 3,     "总消耗量
        avg          TYPE p DECIMALS 3,     "平均库存
        ratio_c      TYPE p DECIMALS 2,     "周转率
        zxhl_c       TYPE p DECIMALS 2,     "总消耗量
        avg_c        TYPE p DECIMALS 2,     "平均库存
        days         TYPE p DECIMALS 4,     "周转天数
        days_1       TYPE p DECIMALS 4,     "周转天数(不考虑期初)
        anfmenge(09) TYPE p    DECIMALS 3,  "期初库存
        endmenge(09) TYPE p    DECIMALS 3,  "期末库存
        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_sl_hj,
        werks TYPE werks_d,    "工厂
        matnr TYPE matnr,      "物料
        lgort TYPE lgort_d,    "库存地点
        menge TYPE menge_d,    "数量
      END OF ty_sl_hj .



TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,       "物料号
        werks TYPE werks_d,     "工厂
        lgort TYPE lgort,       "库存地点
        maktx TYPE maktx,       "物料描述
        matkl TYPE mara-matkl,  "物料组
        mtart TYPE mara-mtart,  "物料类型
      END OF ty_matnr .

"MB5B获取数据用类型定义
TYPES : BEGIN OF stype_totals_flat,
          matnr        LIKE      mbew-matnr,
          maktx        LIKE      makt-maktx,
          bwkey        LIKE      mbew-bwkey,
          werks        LIKE      mseg-werks,
          charg        LIKE      mseg-charg,
          sobkz        LIKE      mslb-sobkz,
          name1        LIKE      t001w-name1,               "n999530

          start_date   LIKE      sy-datlo,
          end_date     LIKE      sy-datlo,

          anfmenge(09) TYPE p    DECIMALS 3,
          meins        LIKE      mara-meins,
          soll(09)     TYPE p    DECIMALS 3,
          haben(09)    TYPE p    DECIMALS 3,
          endmenge(09) TYPE p    DECIMALS 3.

TYPES:    anfwert(09)   TYPE p    DECIMALS 2,
          waers         LIKE t001-waers,
          sollwert(09)  TYPE p    DECIMALS 2,
          habenwert(09) TYPE p    DECIMALS 2,
          endwert(09)   TYPE p    DECIMALS 2,
          color         TYPE      slis_t_specialcol_alv,
          END OF stype_totals_flat.
"从MB5B获取汇总数据用内表和工作区
DATA: g_t_totals_flat TYPE STANDARD TABLE OF stype_totals_flat,
      g_s_totals_flat LIKE LINE OF g_t_totals_flat.


DATA:gt_sl_hj TYPE TABLE OF ty_sl_hj,
     gs_sl_hj TYPE ty_sl_hj.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.


DATA:gt_mseg TYPE TABLE OF mseg,
     gs_mseg TYPE mseg.

DATA:gt_matnr_1 TYPE TABLE OF ty_matnr,
     gt_matnr   TYPE TABLE OF ty_matnr,
     gs_matnr   TYPE ty_matnr.



"include RM07MLBD.
"************************************************************************
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

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
ENDIF.

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
DATA: p_d_l TYPE d,  "最小日期
      p_d_h TYPE d,  "最大日期
      p_d   TYPE i.   "中间差

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
"公司代码
PARAMETERS p_bukrs TYPE mseg-bukrs OBLIGATORY  ."DEFAULT '1000'.
"工厂
PARAMETERS p_werks TYPE ekpo-werks OBLIGATORY ." DEFAULT '1000'.
*"库存地点
*SELECT-OPTIONS S_LGORT FOR EKPO-LGORT.
"物料编码
SELECT-OPTIONS s_matnr FOR makt-matnr.
"物料组
SELECT-OPTIONS s_matkl FOR mara-matkl.
"物料类型
SELECT-OPTIONS : s_mtart FOR mara-mtart .

"移动类型
SELECT-OPTIONS : s_bwart FOR mseg-bwart .

SELECT-OPTIONS : s_lgort FOR mseg-lgort NO INTERVALS NO-EXTENSION.
"PARAMETERS:p_lgort LIKE mseg-lgort . "OBLIGATORY  ."
"日期
SELECT-OPTIONS s_dats FOR sy-datum  OBLIGATORY.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.
*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**权限检查检查公司代码
  PERFORM frm_auth_check.
  PERFORM frm_get_data.
  PERFORM frm_deal_data.
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .
  "DATA WERTE  TYPE TABLE OF  stab_totals_flat .
*set parameter id: 'WRK' FIELD p_werks .
*set parameter id: 'MAT' FIELD S_MATNR-LOW.
*
*call transaction 'MB5B'  with authority-check
*                and skip first screen
*                 EXPORTING LIST TO MEMORY
*                 AND RETURN.
  "最小日期
  p_d_l = s_dats-low .
  IF s_dats-high IS NOT INITIAL .
    p_d_h = s_dats-high.
  ELSE.
    p_d_h = s_dats-low.
  ENDIF.

*if s_lgort is initial.
*  SUBMIT rm07mlbd WITH werks  EQ p_werks
*   WITH  matnr IN s_matnr
*   WITH  datum IN s_dats
*   WITH  lgbst EQ 'X'
*   WITH  bwart in s_bwart
*   WITH  pa_wdzer  EQ 'X'
*   WITH  pa_wdzew  EQ 'X'
*   WITH  pa_wdwiz  EQ 'X'
*   WITH  pa_wdwuw  EQ 'X'
*   WITH  pa_wdwew  EQ 'X'
*   WITH  pa_ndzer  EQ 'X'
*   WITH  pa_ndsto  EQ 'X'
*   WITH pa_sumfl EQ 'X'
*
*   EXPORTING LIST TO MEMORY
*   AND RETURN.
*  else.
  "执行：MB5B 读取 过账日期的库存 的 期初、期末库存
  SUBMIT rm07mlbd WITH werks  EQ p_werks
 WITH  matnr IN s_matnr
 WITH  datum IN s_dats
 WITH  lgort IN s_lgort
 WITH  lgbst EQ 'X'
 WITH  bwbst EQ ''
 WITH  sbbst EQ ''
 WITH  bwart IN s_bwart
 WITH  pa_wdzer  EQ 'X'
 WITH  pa_wdzew  EQ 'X'
 WITH  pa_wdwiz  EQ 'X'
 WITH  pa_wdwuw  EQ 'X'
 WITH  pa_wdwew  EQ 'X'
 WITH  pa_ndzer  EQ 'X'
 WITH  pa_ndsto  EQ 'X'
 WITH pa_sumfl EQ 'X'

 EXPORTING LIST TO MEMORY
 AND RETURN.

  "  endif.

  IMPORT a = g_t_totals_flat  FROM MEMORY ID 'ZMMR035_A'.
  FREE MEMORY ID 'ZMMR035_A'.
  SORT g_t_totals_flat BY matnr werks .

  SELECT * INTO TABLE gt_mseg
    FROM mseg
    WHERE werks EQ p_werks
    AND budat_mkpf IN s_dats
    AND matnr IN s_matnr
    AND lgort IN s_lgort
    AND bwart IN ('201','202','261','262','543','544','551','552','601','602',
                  '653','654','Z09','Z10','Z61','Z62','Z63','Z64','Z65','Z66',
                  'Z67','Z68','Z69','Z71','Z72','Z01','Z02','Z05','Z06','411E',
                  '412E','541','542','643','644','673','674','702',"'Z07','Z08',
                  'Z09','Z10'
                  ) .
  SORT gt_mseg BY mblnr mjahr zeile .

  DELETE gt_mseg WHERE bwart NOT IN s_bwart .

  SELECT * APPENDING TABLE gt_mseg
   FROM mseg
   WHERE werks EQ p_werks
   AND budat_mkpf IN s_dats
      AND matnr IN s_matnr
   AND lgort IN s_lgort
   AND bwart IN ('309','310','311','312','321','322','343','344','349','350','Z11','Z12','Z07','Z08')
   AND shkzg EQ 'H' .
  SORT gt_mseg BY mblnr mjahr zeile .


  DELETE gt_mseg WHERE bwart NOT IN s_bwart .

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
  LOOP AT gt_mseg INTO gs_mseg .
    CLEAR:gs_sl_hj.
    gs_sl_hj-werks = gs_mseg-werks.
    gs_sl_hj-matnr = gs_mseg-matnr.
    IF s_lgort IS NOT INITIAL.
      gs_sl_hj-lgort = gs_mseg-lgort.
    ENDIF.
    gs_sl_hj-menge = gs_mseg-menge.

    IF gs_mseg-shkzg EQ 'S'.
      gs_sl_hj-menge = gs_sl_hj-menge * -1 .
    ENDIF.
    COLLECT gs_sl_hj INTO gt_sl_hj .
  ENDLOOP.
  SORT gt_sl_hj BY werks matnr lgort .
  MOVE-CORRESPONDING gt_sl_hj TO gt_matnr_1.

*  SORT gt_matnr_1 BY matnr .
*  DELETE ADJACENT DUPLICATES FROM gt_matnr_1 COMPARING matnr .


  SELECT a~matnr a~werks b~matkl b~mtart c~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
     FROM marc AS a
     INNER JOIN mara AS b
    ON a~matnr = b~matnr
    INNER JOIN makt AS c
    ON a~matnr = c~matnr
    WHERE a~matnr IN s_matnr
    AND   a~werks EQ p_werks
    AND   b~matkl IN  s_matkl
    AND   b~mtart IN s_mtart
    AND   spras = '1'.
  SORT gt_matnr BY matnr  werks.


  LOOP AT gt_sl_hj INTO gs_sl_hj.
    CLEAR:gs_data.

    gs_data-matnr = gs_sl_hj-matnr.  "物料
    gs_data-werks = gs_sl_hj-werks.  "工厂
    READ TABLE gt_matnr INTO gs_matnr
     WITH KEY matnr = gs_data-matnr
              werks = gs_data-werks
              BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-maktx = gs_matnr-maktx. "物料描述
      gs_data-matkl = gs_matnr-matkl. "物料组
      gs_data-mtart = gs_matnr-mtart. "物料类型
    ELSE.
      CONTINUE.
    ENDIF.
    gs_data-bukrs = p_bukrs .        "公司代码
    IF s_lgort IS NOT INITIAL.
      gs_data-lgort = gs_sl_hj-lgort.  "库存地点
    ENDIF.
    gs_data-zxhl  = gs_sl_hj-menge.  "总消耗量
    "期初库存、期末库存
    READ TABLE g_t_totals_flat INTO g_s_totals_flat WITH KEY matnr = gs_data-matnr
                                                             werks = gs_data-werks
                                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-anfmenge = g_s_totals_flat-anfmenge.
      gs_data-endmenge = g_s_totals_flat-endmenge.
      gs_data-avg   =  (  g_s_totals_flat-anfmenge  +  g_s_totals_flat-endmenge ) / 2.
      IF gs_data-avg IS NOT INITIAL.
        gs_data-ratio = gs_data-zxhl /  gs_data-avg  .
      ENDIF.
      IF gs_data-ratio IS NOT INITIAL .
        gs_data-days = ( p_d_h - p_d_l ) / gs_data-ratio .

      ENDIF.
      p_d = p_d_h - p_d_l .
      IF p_d NE 0 .
        IF gs_data-zxhl /  p_d NE 0 .
          gs_data-days_1 = gs_data-endmenge / ( gs_data-zxhl /  p_d ) .
        ENDIF.
      ENDIF.

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
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
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
*  init_fieldcat  '' '起始日期'               '' '' '' '' '' '' '' .
*  init_fieldcat 'DATE_E' '结束日期'            '' '' '' '' '' '' ''.
  init_fieldcat 'BUKRS'          '公司代码'              '' '' '' '' '' '' ''.
  " init_fieldcat 'BUTXT' '公司名称'              '' '' '' '' '' '' ''.
  init_fieldcat 'WERKS' '工厂'            '' '' '' '' '' '' ''.
  " init_fieldcat  'NAME1' '工厂名称'                '' '' '' '' '' '' ''.
  init_fieldcat  'MATNR' '物料编码'           '' '' '' '' '' 'MSEG' 'MATNR'.
  "init_fieldcat  'LGORT' '库存地点'           '' '' '' '' '' '' ''.

  init_fieldcat  'MAKTX' '物料描述'           '' '' '' '' '' '' ''.
  init_fieldcat  'MATKL' '物料组'           '' '' '' '' '' '' ''.
  init_fieldcat  'MTART' '物料类型' '' '' '' '' '' '' ''.
  init_fieldcat  'RATIO' '周转率'            '' '' '' '' '' '' ''.
  init_fieldcat 'ZXHL' '总消耗量'            '' '' '' '' '' '' ''.
  init_fieldcat 'ANFMENGE' '期初库存'            '' '' '' '' '' '' ''.
  init_fieldcat 'ENDMENGE' '期末库存'            '' '' '' '' '' '' ''.
  init_fieldcat  'AVG' '平均库存'           '' '' '' '' '' '' ''.
  init_fieldcat  'DAYS' '周转天数'           '' '' '' '' '' '' ''.
  init_fieldcat  'DAYS_1' '周转天数(不考虑期初)'           '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0271   text
*      -->P_0272   text
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
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
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

*  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
*
*
*  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*    IMPORTING
*      E_GRID = G_REF_GRID.
*
*  CASE R_UCOMM.
*
*  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM init_variant .
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
              ID 'ACTVT' FIELD '03'
             ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
    MESSAGE e603(fco) WITH p_werks.
    STOP .
  ENDIF.
ENDFORM.
