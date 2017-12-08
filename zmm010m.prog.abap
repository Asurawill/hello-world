REPORT zmm010m.
"个人借物归还标记
"date 20160628
"added by  it02

TABLES:mseg,mkpf .

TYPES:BEGIN  OF ty_data,

        mjahr  TYPE mseg-mjahr, "物料凭证年度
        mblnr  TYPE mseg-mblnr, "物料凭证编号
        zeile  TYPE mseg-zeile, "物料凭证行号
        matnr  TYPE mseg-matnr, "物料号
        "    maktx  TYPE makt-maktx, "物料描述
        lifnr  TYPE mseg-lifnr, "借物联系人
        werks  TYPE werks_d,     "工厂
        " name1  TYPE lfa1-name1, "供应商名称
        ghflag TYPE c LENGTH 1, "人为选择归还
        uname  TYPE c LENGTH 30, "操作用户名
        udate  TYPE d , "操作日期
        utime  TYPE t,  "操作时间
        sel,

      END OF ty_data.


TYPES:BEGIN OF ty_upload,
        mjahr(4)  TYPE c,
        mblnr(10) TYPE c,
        zeile(4)  TYPE c,
        matnr     TYPE mseg-matnr, "物料号
        lifnr     TYPE mseg-lifnr, "借物联系人
        ghflag    TYPE c LENGTH 1, "人为选择归还
      END OF ty_upload.



DATA:gs_data TYPE ty_data,
     gt_data TYPE TABLE OF ty_data.



DATA gt_makt TYPE TABLE OF makt.
DATA gs_makt TYPE makt.

DATA gt_lfa1 TYPE TABLE OF lfa1.
DATA gs_lfa1 TYPE lfa1.


DATA:gt_upload TYPE TABLE OF ty_upload,
     gs_upload TYPE ty_upload.

DATA t_raw_data TYPE truxs_t_text_data.

DATA:gt_zmm010m TYPE TABLE OF zmm010m,
     gs_zmm010m TYPE zmm010m.


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
  gw_lvc-ref_table = &8.
  gw_lvc-ref_field = &9.
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
DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

DATA gt_t001w TYPE TABLE OF t001w WITH HEADER LINE.

************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-001.
PARAMETERS: p_werks LIKE mseg-werks DEFAULT '1000',
            p_file  LIKE rlgrap-filename MODIF ID mp1.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_get_filepath.


AT SELECTION-SCREEN OUTPUT.


*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
  "AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*查询工厂
  SELECT * FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE gt_t001w.

  PERFORM frm_auth_check.

  PERFORM frm_input.
*  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑

  PERFORM frm_alv_show. "ALV显示

  " *&---------------------------------------------------------------------*
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
  LOOP AT gt_t001w WHERE werks EQ p_werks.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
             ID 'WERKS' FIELD gt_t001w-werks
             .
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gt_t001w-werks.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_input .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = t_raw_data
      i_filename           = p_file
    TABLES
      i_tab_converted_data = gt_upload
*   EXCEPTIONS
*     CONVERSION_FAILED    = 1
*     OTHERS               = 2
    .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MOVE-CORRESPONDING gt_upload TO gt_data.
    SORT gt_data BY mjahr mblnr .

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
*  SELECT * FROM MAKT
*  INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
*  WHERE MATNR = GT_DATA-MATNR
*    AND SPRAS = SY-LANGU.
*
*sort gt_makt by matnr .
*
* SELECT * FROM LFA1
*   INTO CORRESPONDING  FIELDS OF TABLE GT_LFA1 .
*
* sort gt_lfa1 by lifnr .


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

  LOOP AT  gt_data INTO gs_data.
    gs_data-werks = p_werks.

    IF   strlen( gs_data-lifnr ) < 10 .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_data-lifnr
        IMPORTING
          output = gs_data-lifnr.

    ENDIF.
*    gs_data-uname = sy-uname.
*    gs_data-udate = sy-datum.
*    gs_data-utime = sy-uzeit.
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
  PERFORM frm_build_event.
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
  gw_layout-zebra = 'X'.
  gw_layout-cwidth_opt  = 'X'.

  gw_layout-box_fname = 'SEL'.

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
  init_fieldcat 'WERKS'          '工厂'         '' '' '' '' '' '' ''.
  init_fieldcat 'MJAHR'          '物料凭证年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'MBLNR'          '物料凭证编码'         '' '' '' '' 'X' '' ''.
  init_fieldcat 'ZEILE'          '行号'         '' '' '' '' 'X' '' ''.
  init_fieldcat 'MATNR'          '物料编号'               '' '' '' '' '' '' ''.
  " init_fieldcat 'MAKTX'          '描述'           '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR'          '借物联系人'               '' '' '' '' '' '' ''.
  " init_fieldcat 'NAME1'          '借物联系人描述'               '' '' '' '' '' '' ''.
  init_fieldcat 'GHFLAG'          '人为选择是否归还'               '' '' '' 'X' '' '' ''.

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
*      -->P_0388   text
*      -->P_0389   text
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
ENDFORM.

FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  CASE r_ucomm.

* 双击
    WHEN '&IC1'.
      READ TABLE gt_data INTO gs_data INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'MBLNR'
        AND gs_data-mjahr IS NOT INITIAL
        AND gs_data-mblnr IS NOT INITIAL.
        SET PARAMETER ID 'MBN' FIELD gs_data-mblnr.
        SET PARAMETER ID 'MJA' FIELD gs_data-mjahr.
        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&DATA_SAVE'.
      "保存列表数据到

      CLEAR:gs_zmm010m.
      REFRESH gt_zmm010m.



      LOOP AT gt_data INTO gs_data WHERE GHFLAG NE 'X'..
        MOVE-CORRESPONDING gs_data TO gs_zmm010m.

        gs_zmm010m-uname = sy-uname.  "修改名
        gs_zmm010m-udate = sy-datum.  "修改日期
        gs_zmm010m-utime = sy-uzeit.  "修改时间
        APPEND gs_zmm010m TO gt_zmm010m .
      ENDLOOP.
      "保存数据库表数据
      SORT gt_zmm010m BY mjahr mblnr zeile.
      MODIFY zmm010m FROM TABLE gt_zmm010m.

      IF sy-subrc EQ 0.
        MESSAGE '导入文件数据已保存成功' TYPE 'S'.
      ENDIF.
 DELETE from zmm010m where  werks = p_werks and GHFLAG eq ''.





  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

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
*&      Form  FRM_GET_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_filepath .
  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    EXPORTING
*     DEF_FILENAME     = ' '
*     DEF_PATH         = ' '
      mask             = 'Excel Files,*.xls,All Files,*.*. '
      mode             = 'O'
*     TITLE            = ' '
    IMPORTING
      filename         = p_file
*     PATH             =
*     FILE             =
    EXCEPTIONS
      selection_cancel = 1
      selection_error  = 2
      OTHERS           = 3.
  CASE sy-subrc.
    WHEN 0.
    WHEN 2.
      MESSAGE 'Cancel.' TYPE 'S'.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.
ENDFORM.
