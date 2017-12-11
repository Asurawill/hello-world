REPORT zmm036.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20161214
*& Request       :
*& Descriptions  : 采购申请打印
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
TABLES:eban,makt,mara,vbak.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        banfn TYPE eban-banfn,  "采购申请
        pstyp TYPE eban-pstyp,  "项目类别
        knttp TYPE eban-knttp,   "科目分配类别
        bnfpo TYPE eban-bnfpo,   "采购申请行项目
        werks TYPE eban-werks,   "工厂
        ekgrp TYPE eban-ekgrp,   "采购组
        matnr TYPE eban-matnr,   "物料编码
        txz01 TYPE eban-txz01,   "物料描述
        menge TYPE eban-menge,   "申请数量
        flief TYPE eban-flief,   "固定供应商
        badat TYPE eban-badat,   "采购申请日期
        lfdat TYPE eban-lfdat,    "交货日期
        afnam TYPE  eban-afnam,   "申请者
        ernam TYPE eban-ernam,  "创建者
        loekz TYPE eban-loekz,  "删除标识
        matkl TYPE mara-matkl,  "物料组
        meins TYPE eban-meins,  "计量单位
        vbelp TYPE vbap-posnr,  "销售订单行号
        vbeln TYPE vbap-vbeln,  "销售订单号
        xmmc  TYPE string,      "项目名称
        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_xm,
        vbeln TYPE vbak-vbeln,  "销售订单
        xmmc  TYPE string,      "项目名称
      END OF ty_xm.

TYPES:BEGIN OF ty_print,
        banfn TYPE eban-banfn,  "采购申请
        pstyp TYPE eban-pstyp,  "项目类别
        knttp TYPE eban-knttp,   "科目分配类别
        bnfpo TYPE eban-bnfpo,   "采购申请行项目
        werks TYPE eban-werks,   "工厂
        ekgrp TYPE eban-ekgrp,   "采购组
        matnr TYPE eban-matnr,   "物料编码
        txz01 TYPE eban-txz01,   "物料描述
        menge TYPE eban-menge,   "申请数量
        flief TYPE eban-flief,   "固定供应商
        badat TYPE eban-badat,   "采购申请日期
        lfdat TYPE eban-lfdat,    "交货日期
        afnam TYPE  eban-afnam,   "申请者
        ernam TYPE eban-ernam,  "创建者
        matkl TYPE mara-matkl,  "物料组
        meins TYPE eban-meins,  "计量单位
        vbelp TYPE vbap-posnr,  "销售订单行号
        vbeln TYPE vbap-vbeln,  "销售订单号
        xmmc  TYPE string,      "项目名称
        zsel,

      END OF ty_print .

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_sel TYPE TABLE OF ty_print,
     gs_sel TYPE ty_print.

DATA:gt_ebkn TYPE TABLE OF ebkn,
     gs_ebkn TYPE ebkn.

DATA:gt_xm TYPE TABLE OF ty_xm,
     gs_xm TYPE ty_xm.

DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

DATA:lt_prt TYPE TABLE OF ty_print,
     ls_prt TYPE ty_print.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

*获取销售长文本
DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

*打印参数变量
DATA: control    TYPE ssfctrlop,
      ntotalline TYPE i,
      npageline  TYPE i VALUE 9,
      p_index    LIKE sy-tabix.
DATA: emptycount      TYPE i VALUE 0,  "空行数.
      ncurrline       TYPE i,      "中间变量
      job_output_info TYPE ssfcrescl.
DATA: g_name TYPE rs38l_fnam.
DATA:l_formname TYPE tdsfname  .
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

*  if gw_lvc-fieldname eq 'PSMNG' " OR gw_lvc-fieldname eq 'BCRK'
*     OR gw_lvc-fieldname eq 'WEMNG' OR gw_lvc-fieldname eq 'LJRK'
*     OR gw_lvc-fieldname eq 'IGMNG'.
*      gw_lvc-tabname      = 'GT_DATA'.
*      gw_lvc-qfieldname = 'AMEIN'.
*  endif.
*
*   if gw_lvc-fieldname eq 'BCRK' .
*      gw_lvc-qfieldname = 'AMEIN'.
*     gw_lvc-decimals = 3.
*   endif.


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

DATA:g_tabix TYPE i .

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
SELECT-OPTIONS:
                s_banfn FOR eban-banfn ,"OBLIGATORY ,
                s_matnr FOR eban-matnr,
                s_badat FOR eban-badat,
                s_werks FOR eban-werks  OBLIGATORY ,
                s_vbeln FOR vbak-vbeln,
                s_afnam  FOR eban-afnam.
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
  PERFORM frm_auth_check.
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
FORM frm_auth_check .
  SELECT * INTO TABLE gt_t001w
    FROM t001w
    WHERE werks IN s_werks.
  SORT gt_t001w BY werks .

  LOOP AT gt_t001w INTO gs_t001w.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
       ID 'WERKS' FIELD  gs_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gs_t001w-werks.
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
  SELECT banfn pstyp knttp bnfpo werks
         ekgrp matnr txz01 menge flief badat
         lfdat afnam ernam meins matkl loekz
        INTO CORRESPONDING FIELDS OF TABLE gt_data
        FROM eban
        WHERE banfn IN s_banfn
        AND   matnr IN s_matnr
        AND   badat IN s_badat
        AND   werks IN s_werks
        "and   vbeln in s_vbeln
        AND   afnam IN s_afnam
        and   loekz eq ''.
  SORT gt_data BY banfn bnfpo .

  CHECK gt_data IS NOT INITIAL.
  SELECT * INTO TABLE gt_ebkn
    FROM ebkn
    FOR ALL ENTRIES IN gt_data
    WHERE banfn = gt_data-banfn
    AND   bnfpo = gt_data-bnfpo
    AND   vbeln IN s_vbeln
    AND   vbeln NE ''.

  SORT gt_ebkn BY banfn bnfpo .

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





  LOOP AT gt_data ASSIGNING <fs_data> .
    g_tabix = sy-tabix .
    READ TABLE gt_ebkn INTO gs_ebkn WITH KEY banfn = <fs_data>-banfn
                                             bnfpo = <fs_data>-bnfpo
                                             BINARY SEARCH .
    IF sy-subrc EQ 0 .

      <fs_data>-vbeln = gs_ebkn-vbeln .
      <fs_data>-vbelp = gs_ebkn-vbelp.

      "读取项目名称
      READ TABLE gt_xm INTO gs_xm WITH KEY vbeln = <fs_data>-vbeln
                                  BINARY SEARCH .
      IF sy-subrc EQ 0.
        <fs_data>-xmmc = gs_xm-xmmc .
      ENDIF.

    ENDIF.

    IF s_vbeln IS NOT INITIAL AND <fs_data>-vbeln NOT IN s_vbeln.

      DELETE gt_data INDEX g_tabix.
      CONTINUE.
    ENDIF.


  ENDLOOP.

  MOVE-CORRESPONDING gt_data TO gt_xm .
  DELETE gt_xm WHERE vbeln EQ ''.
  SORT gt_xm BY vbeln .
  DELETE ADJACENT DUPLICATES FROM gt_xm COMPARING vbeln .

  LOOP AT gt_xm INTO gs_xm .
    "项目名称
    PERFORM selxmmc USING gs_xm-vbeln CHANGING gs_xm-xmmc.
    MODIFY gt_xm FROM gs_xm .
  ENDLOOP.

  LOOP AT gt_data ASSIGNING <fs_data> WHERE vbeln NE ''.
    "读取项目名称
    READ TABLE gt_xm INTO gs_xm WITH KEY vbeln = <fs_data>-vbeln
                                BINARY SEARCH .
    IF sy-subrc EQ 0.
      <fs_data>-xmmc = gs_xm-xmmc .
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
  init_fieldcat 'BANFN'            '采购申请'        '' '' '' '' '' 'EBAN' 'BANFN'.
  init_fieldcat 'PSTYP'            '项目类别'        '' '' '' '' '' 'EBAN' 'PSTYP'.
  init_fieldcat 'KNTTP'            '科目分配类别'        '' '' '' '' '' 'EBAN' 'KNTTP'.
  init_fieldcat 'BNFPO'            '采购申请行项目'        '' '' '' '' '' 'EBAN' 'BNFPO'.
  init_fieldcat 'WERKS'            '工厂'        '' '' '' '' '' 'EBAN' 'WERKS'.
  init_fieldcat 'EKGRP'            '采购组'        '' '' '' '' '' 'EBAN' 'EKGRP'.
  init_fieldcat 'MATNR'            '物料编码'        '' '' '' '' '' 'EBAN' 'MATNR'.
  init_fieldcat 'TXZ01'            '物料描述'        '' '' '' '' '' 'EBAN' 'TXZ01'.
  init_fieldcat 'MENGE'            '申请数量'        '' '' '' '' '' 'EBAN' 'MENGE'.
  init_fieldcat 'FLIEF'            '固定供应商'        '' '' '' '' '' 'EBAN' 'FLIEF'.
  init_fieldcat 'BADAT'            '采购申请日期'        '' '' '' '' '' 'EBAN' 'BADAT'.
  init_fieldcat 'LFDAT'            '交货日期'        '' '' '' '' '' 'EBAN' 'LFDAT'.
  init_fieldcat 'AFNAM'            '申请者'        '' '' '' '' '' 'EBAN' 'AFNAM'.
  init_fieldcat 'ERNAM'            '创建者'        '' '' '' '' '' 'EBAN' 'ERNAM'.
  init_fieldcat 'MATKL'            '物料组'        '' '' '' '' '' 'MARA' 'MATKL'.
  init_fieldcat 'MEINS'            '计量单位'        '' '' '' '' '' 'EBAN' 'MEINS'.
  init_fieldcat 'VBELN'            '销售订单号'        '' '' '' '' '' 'VBAK' 'VBELN'.
  init_fieldcat 'VBELP'            '销售订单行号'        '' '' '' '' '' '' ''.
  init_fieldcat 'XMMC'            '项目名称'        '' '' '' '' '' '' ''.
  init_fieldcat 'LOEKZ'            '删除标识'        '' '' '' '' '' '' ''.

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
*      -->P_0418   text
*      -->P_0419   text
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

  FIELD-SYMBOLS <lw_prt>  TYPE ty_print.


  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFMM036'.

  MOVE-CORRESPONDING gt_data TO gt_sel.
  DELETE gt_sel WHERE zsel NE 'X'.

  SORT gt_sel BY vbeln  banfn bnfpo .


  IF gt_sel IS INITIAL.
    MESSAGE s001(z001) DISPLAY LIKE 'W'.
  ENDIF.



  CHECK gt_sel IS NOT INITIAL.

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

  CLEAR ls_prt .
  CLEAR lt_prt[] .
  lt_prt[] = gt_sel[].

  CALL FUNCTION g_name
    EXPORTING
      control_parameters = control
      npage_line         = npageline
*     W_HEAD             = LW_PRT
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
  ENDIF.
*  LOOP AT gt_sel into gs_sel.
*
*    AT NEW vbeln.
*      CLEAR ls_prt .
*      CLEAR lt_prt[] .
*      "    LS_PRT = <lw_prt>.
*
*    ENDAT.
*    MOVE-CORRESPONDING gs_sel TO ls_prt .
*  "  ls_prt-xh =  g_xh  .  "序号
*    APPEND  ls_prt TO lt_prt.
*    AT END OF vbeln.
*
*
*      CALL FUNCTION g_name
*        EXPORTING
*          control_parameters = control
*   "      NPAGE_LINE         = NPAGELINE
**         W_HEAD             = LW_PRT
*        TABLES
*          t_item             = lt_prt[]
*        EXCEPTIONS
*          formatting_error   = 1
*          internal_error     = 2
*          send_error         = 3
*          user_canceled      = 4
*          OTHERS             = 5.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ELSE.
*      ENDIF.
*
*    ENDAT.
*
*  ENDLOOP.

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

*  IF job_output_info-outputdone = 'X'.
*
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELXMMC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_XM_VBELN  text
*      <--P_GS_XM_XMMC  text
*----------------------------------------------------------------------*
FORM selxmmc  USING    p_vbeln TYPE vbeln
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = '1'
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
