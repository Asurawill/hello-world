REPORT zmm043_2.

"Created by :IT02
"Request:    项目物料管理变更记录
"Modify by:
"Modify date:
"
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者          请求号         修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2017-06-16  IT02&WEIYUN    ED1K905743     新增项目物料管理变更记录

TABLES:mseg,proj,ekkn,prps,precp2,ckis,ekpo,qbew.

TYPES:BEGIN OF ty_data,
        bukrs    LIKE t001-bukrs  , "公司代码
        pspid    LIKE proj-pspid  , "项目定义
        post1    LIKE proj-post1 , "项目描述
        matnr    LIKE mara-matnr,  "物料号
        maktx    LIKE makt-maktx,  "物料描述
        meins    LIKE mara-meins,  "基本单位
        sjcgsl   LIKE ekpo-menge, "实际采购数量(原)
        sjkcsl   LIKE qbew-lbkum,  "实际库存数量(原)
        sjcksl   LIKE mseg-menge,  "实际出库数量(原)
        sjxmdy   LIKE proj-pspid,   "实际项目定义(原)
        sjcgsl_1 LIKE ekpo-menge, "实际采购数量(新)
        sjkcsl_1 LIKE qbew-lbkum,  "实际库存数量(新)
        sjcksl_1 LIKE mseg-menge,  "实际出库数量(新)
        sjxmdy_1 LIKE proj-pspid,   "实际项目定义(新)
        whrq     TYPE d ,          "维护日期
        whsj     TYPE t,           "维护时间
        whzh     LIKE sy-uname,    "维护账号
        zsel     TYPE c,          "选择项

      END OF ty_data.


TYPES:BEGIN OF ty_matnr,
        matnr TYPE mara-matnr,
      END OF ty_matnr.


TYPES:BEGIN OF ty_xm,
        pspid TYPE proj-pspid,
      END OF ty_xm.

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_xm TYPE TABLE OF ty_xm,
     gs_xm TYPE ty_xm.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_mara TYPE TABLE OF mara,
     gs_mara TYPE mara.

DATA:gt_makt TYPE TABLE OF makt,
     gs_makt TYPE makt.

DATA:gt_t001 TYPE TABLE OF t001,
     gs_t001 TYPE t001.



DATA:gt_proj TYPE TABLE OF proj,
     gs_proj TYPE proj.

DATA:gt_prps TYPE TABLE OF prps,
     gs_prps TYPE prps.


DATA:gt_zmm043_change TYPE TABLE OF zmm043_change,
     gs_zmm043_change TYPE zmm043_change.





DATA ls_stable  TYPE lvc_s_stbl.
DATA ls_celltab TYPE lvc_s_styl.
DATA lt_celltab TYPE lvc_t_styl.


DATA stbl       TYPE lvc_s_stbl.




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


DATA: gt_rows TYPE lvc_t_row,
      gt_roid TYPE lvc_t_roid,
      wa_rows TYPE lvc_s_row,
      wa_roid TYPE lvc_s_roid.
DATA: gs_variant TYPE disvariant.
DATA: gw_istable TYPE lvc_s_stbl.

DATA:E_MSEG TYPE STRING.

FIELD-SYMBOLS:<fs_data> TYPE ty_data .


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS:p_bukrs TYPE t001-bukrs OBLIGATORY .  "公司代码

SELECT-OPTIONS: s_pspid FOR proj-pspid.  "项目定义
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

  "权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
*  IF sy-subrc NE 0.
*    MESSAGE e603(fco) WITH p_werks DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

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
*      -->P_0057   text
*----------------------------------------------------------------------*
FORM frm_auth_check  USING    VALUE(p_0057).
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'WERKS' FIELD p_bukrs.
  IF sy-subrc <> 0.
    CONCATENATE '无权显示公司代码：' P_BUKRS '的报表' INTO E_MSEG.
    MESSAGE E_MSEG TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
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

  "查询自建表的
  SELECT * INTO TABLE gt_zmm043_change
    FROM zmm043_change
    WHERE bukrs EQ p_bukrs
    AND   pspid IN s_pspid.

  SORT gt_zmm043_change  BY pspid matnr .



  IF gt_zmm043_change  IS NOT INITIAL.

    MOVE-CORRESPONDING gt_zmm043_change  TO gt_xm.
    SORT gt_xm BY pspid.
    DELETE ADJACENT DUPLICATES FROM gt_xm COMPARING pspid.

    SELECT * INTO TABLE gt_proj
       FROM proj
       FOR ALL ENTRIES IN gt_xm
       WHERE vbukr = p_bukrs
       AND pspid EQ gt_xm-pspid.

    SORT gt_proj BY pspid.

    MOVE-CORRESPONDING gt_zmm043_change  TO gt_matnr.
    SORT gt_matnr BY matnr.
    DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr.



    IF gt_matnr IS NOT INITIAL.
      SELECT * INTO TABLE gt_makt
     FROM makt
    FOR ALL ENTRIES IN gt_matnr
     WHERE matnr = gt_matnr-matnr.

      SORT gt_makt BY matnr.
    ENDIF.



  ENDIF.

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

  DATA:g_post1 TYPE proj-post1.
  MOVE-CORRESPONDING gt_zmm043_change  TO gt_data.
  SORT gt_data BY pspid.

  LOOP AT gt_data INTO gs_data.

    AT NEW pspid.
      CLEAR:g_post1.
      READ TABLE gt_proj INTO gs_proj WITH KEY pspid = gs_data-pspid
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        g_post1 = gs_proj-post1.
      ENDIF.
    ENDAT.
    gs_data-post1 = g_post1.
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_data-matnr
                                     BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-maktx = gs_makt-maktx.  "物料描述

    ENDIF.
    MODIFY gt_data FROM gs_data.
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

  if    gw_lvc-fieldname  eq 'SJKCSL'
      OR gw_lvc-fieldname eq  'SJCKSL'
      OR gw_lvc-fieldname eq  'SJCGSL'
      OR gw_lvc-fieldname eq 'SJKCSL_1'
      OR gw_lvc-fieldname eq  'SJCKSL_1'
      OR gw_lvc-fieldname eq  'SJCGSL_1'.

      gw_lvc-qfieldname = 'MEINS'.
  endif.



  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc .
  init_fieldcat 'BUKRS'        '公司代码'         '' '' '' '' '' '' ''.
  init_fieldcat 'PSPID'        '项目定义'         '' '' '' '' '' 'PROJ' 'PSPID'.
  init_fieldcat 'POST1'        '项目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'        '物料号'         '' '' '' '' '' 'MARA' 'MATNR'.
  init_fieldcat 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'        '基本单位'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJCGSL'        '采购数量（原）'         '' '' '' '' '' 'EKPO' 'MENGE'.
  init_fieldcat 'SJKCSL'        '库存数量（原）'         '' '' '' '' '' 'QBEW' 'LBKUM'.
  init_fieldcat 'SJCKSL'        '出库数量（原）'         '' '' '' '' '' 'MSEG' 'MENGE'.
  init_fieldcat 'SJXMDY'        '项目定义(原)'         '' '' '' '' '' 'PROJ' 'PSPID'.
  init_fieldcat 'SJCGSL_1'        '采购数量（新）'         '' '' '' '' '' 'EKPO' 'MENGE'.
  init_fieldcat 'SJKCSL_1'        '库存数量（新）'         '' '' '' '' '' 'QBEW' 'LBKUM'.
  init_fieldcat 'SJCKSL_1'        '出库数量（新）'         '' '' '' '' '' 'MSEG' 'MENGE'.
  init_fieldcat 'SJXMDY_1'        '项目定义(新)'         '' '' '' '' '' 'PROJ' 'PSPID'.
  init_fieldcat 'WHRQ'        '维护日期'         '' '' '' '' '' '' ''.
  init_fieldcat 'WHSJ'        '维护时间'         '' '' '' '' '' '' ''.
  init_fieldcat 'WHZH'        '维护账号'         '' '' '' '' '' '' ''.

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
*FORM frm_build_event .
*  gw_events-name =  slis_ev_data_changed.
*  gw_events-form = 'FRM_DATA_CHANGED'.
*  APPEND gw_events TO gt_events.
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0114   text
*      -->P_0115   text
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

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
      READ TABLE gt_data INTO gs_data INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'PSPID'
        AND gs_data-pspid IS NOT INITIAL.
        SET PARAMETER ID 'PSP' FIELD gs_data-pspid.
        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.





ENDFORM.                    "ALV_USER_COMMAND
