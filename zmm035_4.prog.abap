REPORT zmm035_4.
"Created by :IT02
"Request:    整备库项目汇总
"Modify by:
"Mo
TABLES:mseg,mkpf,zzbksj,vbak.

TYPES:BEGIN OF ty_data,
        werks TYPE mseg-werks,  "工厂
        lgort TYPE mseg-lgort,  "库存地点
        vbeln TYPE vbak-vbeln,  "项目订单
        xmms  type string ,
        matnr TYPE mseg-matnr,  "物料编码
        maktx TYPE makt-maktx,
        meins TYPE mseg-meins,  "单位
        menge TYPE mseg-menge,  "数量
        zsel,
      END OF ty_data .

TYPES:BEGIN OF ty_matnr,
        matnr TYPE matnr,
        "  WERKS TYPE WERKS,
        maktx TYPE maktx,
        matkl TYPE matkl,
        wgbez TYPE wgbez,
      END OF ty_matnr .

TYPES:BEGIN OF ty_vbeln,
        vbeln TYPE vbeln,
        xmms  TYPE string,
      END OF ty_vbeln .

DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE ty_data.

DATA:gt_zzbksj TYPE TABLE OF zzbksj,
     gs_zzbksj TYPE zzbksj.

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_matnr_1 TYPE TABLE OF ty_matnr,
     gs_matnr_1 TYPE ty_matnr.

DATA:gt_vbeln TYPE TABLE OF ty_vbeln,
     gs_vbeln TYPE ty_vbeln.



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

  if gw_lvc-fieldname eq 'MENGE'.
      gw_lvc-qfieldname = 'MEINS'.
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

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS:p_werks TYPE mseg-werks .  "工厂
SELECT-OPTIONS:
     "      s_mblnr FOR mseg-mblnr,  "物料凭证
           s_lgort FOR mseg-lgort,  "库存地点
           s_vbeln FOR vbak-vbeln,  "销售订单
           s_matnr FOR mseg-matnr.  "物料编码
*           s_bwart FOR mseg-bwart,  "移动类型
*           s_budat FOR mkpf-budat,  "过账日期
*          s_usnam  FOR mseg-usnam_mkpf. "过账人
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

*权限检查检查工厂代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE E603(FCO) WITH P_WERKS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
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
  "只查询处理吗为：3，存储无项目号的物料凭证明细
  SELECT * INTO TABLE gt_zzbksj
    FROM zzbksj
    WHERE werks EQ p_werks
      AND lgort IN s_lgort
      AND matnr IN s_matnr
      AND vbeln IN s_vbeln.
    "  AND vbeln NE ''.
  SORT gt_zzbksj BY werks lgort vbeln matnr .

  CHECK gt_zzbksj IS NOT INITIAL.

  MOVE-CORRESPONDING gt_zzbksj TO gt_matnr.
  SORT gt_matnr BY matnr .
  DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr .

  MOVE-CORRESPONDING gt_zzbksj TO gt_vbeln .
  SORT gt_vbeln BY vbeln.
  DELETE gt_vbeln WHERE vbeln IS INITIAL .
  DELETE ADJACENT DUPLICATES FROM gt_vbeln COMPARING vbeln .


  IF gt_matnr is not initial.
  SELECT a~matnr a~matkl b~maktx c~wgbez
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr_1
    FROM  mara AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
     INNER JOIN t023t AS c
    ON a~matkl = c~matkl
    FOR ALL ENTRIES IN gt_matnr
    WHERE a~matnr = gt_matnr-matnr
    AND  b~spras = '1'
    .
  SORT gt_matnr_1 BY matnr .
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
  "*取项目描述
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  CLEAR t_tline[].
  DATA:tname TYPE thead-tdname.
  LOOP AT gt_vbeln INTO gs_vbeln .

    tname = gs_vbeln-vbeln .
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = sy-langu
        name                    = tname
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*   IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = t_tline[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF t_tline[] IS NOT INITIAL.
      LOOP AT t_tline.
        CONCATENATE gs_vbeln-xmms t_tline-tdline INTO gs_vbeln-xmms.
        CLEAR t_tline.
      ENDLOOP.
    ENDIF.
    MODIFY gt_vbeln FROM gs_vbeln .
  ENDLOOP.
  SORT gt_vbeln  BY vbeln .

 "按工厂、库存地点、项目号、物料号、单位汇总数量
  LOOP AT gt_zzbksj INTO gs_zzbksj.
    CLEAR:gs_data.
    gs_data-werks = gs_zzbksj-werks. "工厂
    gs_data-lgort = gs_zzbksj-lgort. "库存地点
    gs_data-vbeln = gs_zzbksj-vbeln. "项目号
    gs_data-matnr = gs_zzbksj-matnr. "物料号
    gs_data-meins = gs_zzbksj-meins ."单位
      "数量 S:为正  H：为负
    IF gs_zzbksj-shkzg EQ 'H'.
      gs_data-menge = gs_zzbksj-menge * - 1.
      else.
       gs_data-menge = gs_zzbksj-menge .
    ENDIF.

   collect gs_data into gt_data .
  ENDLOOP.

  LOOP AT gt_data into gs_data .
    "物料描述
    read table gt_matnr_1 into gs_matnr_1 with key matnr = gs_data-matnr
                                          binary search .
      if sy-subrc eq 0 .
          gs_data-maktx = gs_matnr_1-maktx .
      endif.
    "项目描述
     read table gt_vbeln into gs_vbeln with key vbeln = gs_data-vbeln
                                        binary search .
      if sy-subrc eq 0.
         gs_data-xmms = gs_vbeln-xmms .
      endif.
    modify gt_data from gs_data .
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
  init_fieldcat 'WERKS'            '工厂'                  '' '' '' '' '' 'MSEG' 'WERKS'.
  init_fieldcat 'LGORT'            '库存地点'              '' '' '' '' '' 'MSEG' 'LGORT'.
    init_fieldcat 'VBELN'            '项目号'               '' '' '' '' '' 'VBAK' 'VBELN'.
  init_fieldcat 'XMMS'             '项目描述'               '' '' '' '' '' '' ''.
  init_fieldcat 'MATNR'            '物料编码'              '' '' '' '' '' 'MSEG' 'MATNR'.
  init_fieldcat 'MAKTX'            '物料描述'               '' '' '' '' '' 'MAKT' 'MAKTX'.
*  init_fieldcat 'MATKL'            '物料组'                '' '' '' '' '' 'MARA' 'MATKL'.
*  init_fieldcat 'WGBEZ'            '物料组描述'            '' '' '' '' '' '' ''.
  init_fieldcat 'MEINS'            '单位'                  '' '' '' '' '' '' ''.
  init_fieldcat 'MENGE'            '数量'                  '' '' '' '' '' '' ''.



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
*      -->P_0185   text
*      -->P_0186   text
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

*  CASE r_ucomm.
*    WHEN '&IC1'.
**      "链接到生产订单确认事务：CO14
**     READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
**       IF SY-SUBRC EQ 0 .
**           IF RS_SELFIELD-FIELDNAME = 'AUFNR'.
**              SET PARAMETER ID 'ANR' FIELD  GS_DATA-AUFNR .
**              CALL TRANSACTION 'CO14' AND SKIP  FIRST  SCREEN .
**            ENDIF.
**
**        ENDIF.
*
**打印
**    WHEN '&PRNT'.
**
**      PERFORM frm_print_data.
*
*
*
*
*
*  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0352   text
*----------------------------------------------------------------------*
FORM frm_auth_check USING VALUE(p_actvt).
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'WERKS' FIELD P_WERKS.
ENDFORM.
