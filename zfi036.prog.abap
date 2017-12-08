REPORT zfi036.
"项目销项税调整
"开发者:IT02(魏云)
"DATE:20160601
TABLES: bkpf,bseg,skat,proj,prps.
TYPES:BEGIN OF ty_item,
        zuonr      TYPE dzuonr,  "项目定义
        bukrs      TYPE bukrs,    "公司代码
        gjahr      TYPE gjahr,    "会计年度
        belnr      TYPE belnr_d,    "会计凭证
        blart      TYPE blart,     "凭证类型
        buzei      TYPE buzei,     "凭证行项目
        budat      TYPE budat,     "过账日期
        monat      TYPE monat,     "过账期间
        waers      TYPE waers,     "凭证货币
        hkont      TYPE hkont,     "凭证科目
        kmms       TYPE string,  "科目描述
        shkzg      TYPE shkzg,   "借贷标识
        wrbtr      TYPE wrbtr,   "凭证货币金额
        pswsl      TYPE pswsl,   "凭证货币
        post1      TYPE ps_post1,  "项目名称
        kunnr      TYPE kunnr,    "客户
        khmc       TYPE string,   "客户名称
        sl         TYPE i,        "税率
        dfkm       TYPE hkont,    "对方科目
        dfkm_txt   TYPE      string,                     "对方科目描述
        tzse       TYPE wrbtr,    "调整税额
        tzysys     TYPE wrbtr,    "调整应收预收金额
        sjtzkm     TYPE hkont,    "税金调整科目
        sjtzkm_txt TYPE string,  "税金调整科目描述
        tzrq       TYPE d,         "调整日期
        tzqj       TYPE monat,     "调整期间
        gzwb       TYPE string,   "过账文本
        gzpz       TYPE belnr_d,   "过账凭证
        gznd       TYPE gjahr,   "过账凭证年度
        cellstyle  TYPE lvc_t_styl,                     "单元格状态
        info_msg   TYPE      string,                   "消息
        statu      TYPE   iconname,
        sel,
      END OF ty_item.

DATA:gs_item TYPE ty_item,
     gt_item TYPE TABLE OF ty_item.

DATA:gt_skat TYPE TABLE OF skat,
     gs_skat TYPE skat.

DATA:gt_kna1 TYPE TABLE OF kna1,
     gs_kna1 TYPE kna1.

DATA:gt_proj TYPE TABLE  OF  proj,
     gs_proj TYPE proj.

DATA:gt_prps TYPE TABLE OF prps,
     gs_prps TYPE prps.

DATA: gr_alvgrid TYPE REF TO cl_gui_alv_grid.

DATA:gt_zfi036 TYPE TABLE OF zfi036,
     gs_zfi036 TYPE zfi036.

DATA:gt_zfi036_2 TYPE TABLE OF zfi036,
     gs_zfi036_2 TYPE zfi036.

"声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data et_bad_cells e_display,
      handle_modify  FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified  et_good_cells ."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
ENDCLASS.

FIELD-SYMBOLS:" <FS_DATA>  LIKE GS_DATA,
               <fs_item> LIKE gs_item.

DEFINE init_fieldcat.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  GW_LVC-KEY = &4.
*  IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
  IF &1 = 'LJCPCB' OR &1 = 'LJGCFY'." OR  &1 = 'YZCB'.
  GW_LVC-CFIELDNAME = 'WAERS'.
 ENDIF.
 IF &1 = 'YZCB'.
   GW_LVC-NO_SIGN = ''.
   GW_LVC-DECIMALS = 2.
 ENDIF.
 IF &1 = 'SJTZKM' OR &1 = 'DFKM' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_TABLE = &8.
  GW_LVC-REF_FIELD = &9.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gw_layout        TYPE lvc_s_layo,                    "ALV的格式
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
DATA gt_event_receiver TYPE REF TO lcl_event_receiver .



DATA: gt_rows TYPE lvc_t_row,
      gt_roid TYPE lvc_t_roid,
      wa_rows TYPE lvc_s_row,
      wa_roid TYPE lvc_s_roid.
DATA: gs_variant TYPE disvariant.
DATA: gw_istable TYPE lvc_s_stbl.

************************************************************************
* BAPI
************************************************************************
DATA: wa_documentheader    TYPE bapiache09,         "表头
      it_accountgl         TYPE TABLE OF bapiacgl09,  "总账
      wa_accountgl         TYPE bapiacgl09,
      it_accountreceivable TYPE TABLE OF bapiacar09,  "客户
      wa_accountreceivable TYPE bapiacar09,
      it_currencyamount    TYPE TABLE OF bapiaccr09,  "货币项目
      wa_currencyamount    TYPE bapiaccr09,
      it_criteria          TYPE TABLE OF bapiackec9,  "分配-科目
      wa_criteria          TYPE bapiackec9,
      it_valuefield        TYPE TABLE OF bapiackev9,
      wa_valuefield        TYPE bapiackev9,
      it_extension2        TYPE TABLE OF bapiparex,
      wa_extension2        TYPE bapiparex,
      it_return            TYPE TABLE OF bapiret2,
      wa_return            TYPE bapiret2.

DATA: wa_obj TYPE bapiache09.
DATA: wa_zaccdocuext TYPE zaccdocuext.

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETER:
p_bukrs  TYPE bkpf-bukrs OBLIGATORY,                      "公司代码
p_gjahr  TYPE bkpf-gjahr OBLIGATORY,                      "年度
p_monat  TYPE bkpf-monat OBLIGATORY.                      "期间
SELECT-OPTIONS: s_pspid FOR proj-pspid.    "项目定义
SELECTION-SCREEN END OF BLOCK blk1.


*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
  IF sy-subrc NE 0.
    MESSAGE i011(zfico01) WITH p_bukrs DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

  " *&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  " *&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_0558   TEXT
*----------------------------------------------------------------------*

FORM frm_auth_check USING VALUE(p_actvt).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD p_actvt
                                      ID 'BUKRS' FIELD p_bukrs.
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
  SELECT bkpf~bukrs bkpf~gjahr bkpf~belnr
         bkpf~monat bkpf~waers bkpf~blart
         bkpf~budat bseg~buzei bseg~hkont
        bseg~shkzg bseg~wrbtr bseg~zuonr
    INTO CORRESPONDING FIELDS OF TABLE gt_item
    FROM bkpf
    INNER JOIN bseg
    ON bkpf~bukrs = bseg~bukrs
    AND  bkpf~belnr = bseg~belnr
    AND  bkpf~gjahr = bseg~gjahr
    WHERE bkpf~bukrs EQ p_bukrs
    AND bkpf~gjahr EQ p_gjahr
    AND bkpf~monat EQ p_monat
    AND  bkpf~blart EQ 'SC'
    AND bkpf~awtyp EQ 'AUAK'
   AND ( bseg~hkont EQ '1241010101' OR  bseg~hkont EQ '2203990201' )
    AND bseg~zuonr IN s_pspid .

  SORT gt_item BY zuonr.

  SELECT * INTO TABLE gt_proj
    FROM proj
    WHERE pspid IN s_pspid.
  SORT gt_proj  BY pspid.
  SELECT * INTO TABLE gt_skat
    FROM skat
    WHERE spras = '1'
    AND  ktopl = '1000'.
  SORT gt_skat BY saknr.

  SELECT * INTO TABLE gt_kna1
    FROM kna1 .
  SORT gt_kna1 BY kunnr.

  SELECT * INTO TABLE gt_zfi036_2
    FROM zfi036
    FOR ALL ENTRIES IN gt_item
    WHERE bukrs = gt_item-bukrs
    AND  belnr = gt_item-belnr
    AND  gjahr = gt_item-gjahr
    AND  buzei = gt_item-buzei
    AND  cxpz EQ ''.

  SORT gt_zfi036_2 BY bukrs belnr gjahr buzei .



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
  DATA:kunnr   TYPE kunnr,  "客户编号
       khmc    TYPE string,  "客户名称
       sl      TYPE i,      "税率
       post1   TYPE ps_post1,
       l_tabix TYPE sy-tabix.
  LOOP AT gt_item ASSIGNING <fs_item>.
    l_tabix = sy-tabix .
    AT NEW  zuonr .
      CLEAR:kunnr,khmc,sl,post1 .
      READ TABLE gt_proj INTO gs_proj
     WITH KEY pspid = <fs_item>-zuonr
              BINARY SEARCH .
      IF sy-subrc EQ 0 .
        "查询项目对应的客户
        kunnr = gs_proj-zkhbm.
        READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = kunnr BINARY SEARCH.
        IF sy-subrc EQ 0 .
          khmc = gs_kna1-name1.

        ENDIF.
        "查询项目对应的名称
        post1 = gs_proj-post1.
      ENDIF.
      "税率
      CASE <fs_item>-zuonr+4(2).
        WHEN '21'.
          sl = 3.
        WHEN '22'.
          sl = 17.
        WHEN '23'.
          sl = 17 .
        WHEN '24'.
          sl = 11.
      ENDCASE.

    ENDAT.

    "判断明细行是否已存在调整过账凭证
    READ TABLE gt_zfi036_2 INTO gs_zfi036_2 WITH KEY bukrs = <fs_item>-bukrs
                        belnr = <fs_item>-belnr
                        gjahr = <fs_item>-gjahr
                        buzei = <fs_item>-buzei.
    IF sy-subrc EQ 0 .
      DELETE gt_item INDEX l_tabix .
      CONTINUE.
    ENDIF.
    <fs_item>-kunnr = kunnr.
    <fs_item>-khmc = khmc.
    <fs_item>-post1 = post1.
    <fs_item>-sl = sl .
    <fs_item>-khmc = khmc.
    "凭证科目名称
    READ TABLE gt_skat INTO gs_skat WITH KEY saknr = <fs_item>-hkont BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_item>-kmms = gs_skat-txt50.
    ENDIF.
    "对方科目描述
    IF <fs_item>-hkont EQ '2203990201'.
      <fs_item>-dfkm = '2203010101'.
    ELSEIF <fs_item>-hkont EQ '1241010101'.
      <fs_item>-dfkm = '1122010101'.
    ENDIF.

    READ TABLE gt_skat INTO gs_skat WITH KEY saknr = <fs_item>-dfkm BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_item>-dfkm_txt = gs_skat-txt50.
    ENDIF.
    "税金调整科目
    <fs_item>-sjtzkm = '2221030503'.
    READ TABLE gt_skat INTO gs_skat WITH KEY saknr = <fs_item>-sjtzkm BINARY SEARCH .
    IF sy-subrc EQ 0 .
      <fs_item>-sjtzkm_txt = gs_skat-txt50.
    ENDIF.
    "调整税额
    <fs_item>-tzse = <fs_item>-wrbtr * <fs_item>-sl / 100 .
    "调整应收预收金额
    <fs_item>-tzysys = <fs_item>-wrbtr + <fs_item>-tzse.
    "调整日期
    <fs_item>-tzrq = <fs_item>-budat.
    "调整期间
    <fs_item>-tzqj = <fs_item>-monat.
    "过账文本
    <fs_item>-gzwb = '项目销项税调整'.
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
  PERFORM frm_build_event.
  PERFORM frm_output TABLES gt_lvc              "输出
                            gt_sort
                            gt_item
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
  gw_layout-zebra        = 'X'.
  gw_layout-cwidth_opt   = 'X'.
  gw_layout-box_fname    = 'SEL'.
  gw_layout-stylefname = 'CELLSTYLE'.

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
  init_fieldcat 'STATU'         '过账状态'             '' '' '' '' '' '' ''.
  init_fieldcat 'BUKRS'         '公司代码'             '' '' '' '' '' '' ''.
  init_fieldcat 'GJAHR'         '会计年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'BELNR'         '会计凭证'         '' '' '' '' '' '' ''.
  init_fieldcat 'BLART'         '凭证类型'         '' '' '' '' '' '' ''.
  init_fieldcat 'BUZEI'         '凭证行项目'         '' '' '' '' '' '' ''.
  init_fieldcat 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'         '过账期间'         '' '' '' '' '' '' ''.
  " init_fieldcat 'BUZEI'         '凭证行项目'         '' '' '' '' '' '' ''.
  init_fieldcat 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  init_fieldcat 'MONAT'         '过账期间'         '' '' '' '' '' '' ''.
  init_fieldcat 'HKONT'         '凭证科目'         '' '' '' '' '' '' ''.
  init_fieldcat 'KMMS'          '科目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'SHKZG'         '借贷标识'         '' '' '' '' '' '' ''.
  init_fieldcat 'WRBTR'         '凭证货币金额'         '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'          '凭证货币吗'         '' '' '' '' '' '' ''.
  init_fieldcat 'ZUONR'         '项目定义'         '' '' '' '' '' '' ''.
  init_fieldcat 'POST1'         '项目名称'         '' '' '' '' '' '' ''.
  init_fieldcat 'KUNNR'          '客户'         '' '' '' '' '' '' ''.
  init_fieldcat 'KHMC'         '客户名称'         '' '' '' '' '' '' ''.
  init_fieldcat 'DFKM'         '对方科目'         '' '' '' 'X' '' '' ''.
  init_fieldcat 'DFKM_TXT'     '对方科目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'TZSE'          '调整税额'         '' '' '' 'X' '' '' ''.
  init_fieldcat 'TZYSYS'       '调整应收预收金额'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJTZKM'       '税金调整科目'         '' '' '' 'X' '' '' ''.
  init_fieldcat 'SJTZKM_TXT'    '税金调整科目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'TZRQ'          '调整日期'         '' '' '' 'X' '' 'BKPF' 'BUDAT'.
  init_fieldcat 'TZQJ'         '调整期间'         '' '' '' 'X' '' 'BKPF' 'MONAT'.
  init_fieldcat 'GZWB'         '过账文本'         '' '' '' 'X' '' '' ''.
  init_fieldcat 'GZPZ'          '过账凭证'         '' '' '' '' '' '' ''.
  init_fieldcat 'GZND'         '过账凭证年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'INFO_MSG'      '过账消息文本'     '' '' '' '' '' '' ''.
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
  gw_events-name =  'CALLER_EXIT' .
  gw_events-form =  'FRM_BUTTON'.   "f4事件
  APPEND gw_events TO gt_events.
  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
  APPEND gw_events TO gt_events.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_ITEM  text
*      -->P_0561   text
*      -->P_0562   text
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
*     I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_modify.
    PERFORM handle_data_changed_finished USING e_modified et_good_cells.
  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_onf4.
    FIELD-SYMBOLS <fs_mod_cells> TYPE lvc_t_modi.
    DATA: lw_mod_cell TYPE lvc_s_modi.

    CASE e_fieldname.
      WHEN 'SJTZKM'.
        READ TABLE gt_item INTO gs_item INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          PERFORM sub_help_hkont CHANGING gs_item-sjtzkm.
          IF gs_item-sjtzkm IS NOT INITIAL.
            MODIFY gt_item FROM gs_item INDEX es_row_no-row_id.
            ASSIGN er_event_data->m_data->* TO <fs_mod_cells>.
            lw_mod_cell-row_id = es_row_no-row_id.
            lw_mod_cell-sub_row_id = es_row_no-sub_row_id.
            lw_mod_cell-fieldname = 'SJTZKM'.
            lw_mod_cell-value = gs_item-sjtzkm.
            APPEND lw_mod_cell TO <fs_mod_cells>.

          ENDIF.
        ENDIF.

      WHEN 'DFKM'.
        READ TABLE gt_item  INTO gs_item INDEX es_row_no-row_id .
        IF sy-subrc EQ 0 .
          PERFORM sub_help_hkont CHANGING gs_item-dfkm.
          IF gs_item-dfkm IS NOT INITIAL.
            MODIFY gt_item FROM gs_item INDEX es_row_no-row_id.
            ASSIGN er_event_data->m_data->* TO <fs_mod_cells>.
            lw_mod_cell-row_id = es_row_no-row_id.
            lw_mod_cell-sub_row_id = es_row_no-sub_row_id.
            lw_mod_cell-fieldname = 'DFKM'.
            lw_mod_cell-value = gs_item-dfkm.
            APPEND lw_mod_cell TO <fs_mod_cells>.

          ENDIF.
        ENDIF.

    ENDCASE.

    "**  Inform ALV Grid that event 'onf4' has been processed
    er_event_data->m_event_handled = 'X'.           "告知F4动作结束
  ENDMETHOD.
ENDCLASS.


FORM handle_data_changed_finished  USING    p_e_modified
                                            p_et_good_cells TYPE  lvc_t_modi.

  DATA:lw_cell TYPE lvc_s_modi.
  DATA: ls_stable TYPE lvc_s_stbl.
  READ TABLE   p_et_good_cells INTO lw_cell INDEX 1."  WITH KEY FIELDNAME = 'DYNBDD'.
  IF sy-subrc EQ 0 .
    CASE lw_cell-fieldname .
      WHEN 'DFKM'.
        READ TABLE gt_item INTO gs_item  INDEX  lw_cell-row_id.
        IF sy-subrc EQ 0 .
          CLEAR:gs_item-dfkm_txt.
          READ TABLE gt_skat INTO gs_skat
                WITH KEY saknr = gs_item-dfkm
                  BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_item-dfkm_txt = gs_skat-txt50 .
          ENDIF.
          "刷新数据到ALV
          MODIFY gt_item FROM gs_item INDEX  lw_cell-row_id.
          CLEAR gs_item.

        ENDIF.

      WHEN 'SJTZKM'.
        READ TABLE gt_item INTO gs_item  INDEX  lw_cell-row_id.
        IF sy-subrc EQ 0 .
          CLEAR:gs_item-sjtzkm_txt.
          READ TABLE gt_skat INTO gs_skat
              WITH KEY saknr = gs_item-sjtzkm
                BINARY SEARCH .
          IF sy-subrc EQ 0 .
            gs_item-sjtzkm_txt = gs_skat-txt50 .
          ENDIF.
          "刷新数据到ALV
          MODIFY gt_item FROM gs_item INDEX  lw_cell-row_id.
          CLEAR gs_item.
        ENDIF.

    ENDCASE.

    "  *   稳定刷新
    ls_stable-row = 'X'." 基于行的稳定刷新
    ls_stable-col = 'X'." 基于列稳定刷新
    CALL METHOD gr_alvgrid->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.


ENDFORM.

FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.

FORM frm_data_changed USING   er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.




  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_alvgrid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件



  CALL METHOD gr_alvgrid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_modify FOR gr_alvgrid.


ENDFORM.

FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  " DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = gr_alvgrid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
*      READ TABLE gt_total INTO gs_total INDEX rs_selfield-tabindex.
*      CHECK sy-subrc = 0.
*      IF rs_selfield-fieldname = 'GZPZ'
*        AND gs_total-gzpz IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD gs_total-gzpz.
*        SET PARAMETER ID 'BUK' FIELD gs_total-bukrs.
*        SET PARAMETER ID 'GJR' FIELD gs_total-gzpznd.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.

    WHEN '&POST'.
      DATA l_ans.
      DATA l_subrc TYPE sy-subrc.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = '确认过账'
*         DIAGNOSE_OBJECT       = ' '
          text_question         = '确定要保存并过账吗？'
          text_button_1         = '是'(B01)
*         ICON_BUTTON_1         = ' '
          text_button_2         = '否'(B02)
*         ICON_BUTTON_2         = ' '
*         DEFAULT_BUTTON        = '1'
          display_cancel_button = ''
*         USERDEFINED_F1_HELP   = ' '
*         START_COLUMN          = 25
*         START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          answer                = l_ans
*   TABLES
*         PARAMETER             =
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CHECK l_ans EQ '1'.

      PERFORM frm_check_doc CHANGING l_subrc.

      CHECK l_subrc EQ 0.

      "根据选中行过帐
      PERFORM post_data.

      "刷新数据到内表
      CALL METHOD gr_alvgrid->refresh_table_display.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_doc  CHANGING p_subrc.
  "判断是否已选中过账的记录
  READ TABLE gt_item INTO gs_item WITH KEY sel = 'X'.
  IF sy-subrc NE 0 .
    p_subrc = 4 .
    MESSAGE '请选中相应的行再过账'  TYPE 'E'.
  ENDIF.

  LOOP AT gt_item INTO gs_item  WHERE sel = 'x'.
    "CHECK 检查必填项
    IF gs_item-dfkm IS INITIAL .
      MESSAGE text-m01 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.
    ENDIF.

    IF gs_item-tzse IS INITIAL.
      MESSAGE text-m02 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.
    ENDIF.

    IF gs_item-sjtzkm  IS INITIAL .

      MESSAGE text-m03 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.
    ENDIF.


    IF gs_item-tzrq IS INITIAL .

      MESSAGE text-m04 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.
    ENDIF.

    IF gs_item-tzqj IS INITIAL .
      MESSAGE text-m05 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.

    ENDIF.
    IF gs_item-gzwb IS INITIAL .
      MESSAGE text-m06 TYPE 'S' DISPLAY LIKE 'E'.
      p_subrc = 4.
      EXIT.

    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_data .
  REFRESH:gt_zfi036.
  LOOP AT gt_item  INTO gs_item WHERE sel = 'X' AND gzpz IS INITIAL.
    "凭证行:项目虚拟行
    "CLEAR:GS_TOTAL-INFO_MSG .
    "付款记账数据抬头
    REFRESH: it_accountgl, it_accountgl, it_currencyamount, it_criteria, it_valuefield, it_extension2, it_return,it_accountreceivable.
    CLEAR: wa_documentheader, wa_obj.

    "* 抬头
**********************************************************************
*凭证日期
    wa_documentheader-doc_date     =  gs_item-tzrq.
*过账日期
    wa_documentheader-pstng_date   =  gs_item-tzrq.
*凭证类型
    wa_documentheader-doc_type     =  'SC'.
*公司代码
    wa_documentheader-comp_code    =  gs_item-bukrs.
*凭证抬头文本
    wa_documentheader-header_txt   =  gs_item-gzwb.
*创建人员
    wa_documentheader-username     =  sy-uname.

*    "参考交易
*    wa_documentheader-obj_type = 'GZTZ'.
*    wa_documentheader-obj_key = '33333333333333333333'.


    "凭证行
    "记账行
    " **********************************************************************

    wa_zaccdocuext-posnr = '0000000010'.
    IF gs_item-shkzg EQ 'S' .
      wa_zaccdocuext-bschl =  '50'.
    ELSE.
      wa_zaccdocuext-bschl =  '40'.
    ENDIF.
    wa_extension2-structure = 'ZACCDOCUEXT'.
    wa_extension2-valuepart1 = wa_zaccdocuext.
    APPEND wa_extension2 TO it_extension2.

    wa_zaccdocuext-posnr = '0000000020'.
    IF gs_item-shkzg EQ 'S' .
      wa_zaccdocuext-bschl =  '01'.
    ELSE.
      wa_zaccdocuext-bschl =  '11'.
    ENDIF..
    wa_extension2-structure = 'ZACCDOCUEXT'.
    wa_extension2-valuepart1 = wa_zaccdocuext.
    APPEND wa_extension2 TO it_extension2.

    wa_zaccdocuext-posnr = '0000000030'.
    IF gs_item-shkzg EQ 'S' .
      wa_zaccdocuext-bschl =  '50'.
    ELSE.
      wa_zaccdocuext-bschl =  '40'.
    ENDIF..
    wa_extension2-structure = 'ZACCDOCUEXT'.
    wa_extension2-valuepart1 = wa_zaccdocuext.
    APPEND wa_extension2 TO it_extension2.

    "科目表
    " **********************************************************************
    CLEAR:wa_accountgl.
    wa_accountgl-itemno_acc = '0000000010'.
    "凭证科目
    wa_accountgl-gl_account = gs_item-hkont.
    "项目文本
    wa_accountgl-item_text = gs_item-gzwb.
    "WBS元素
    wa_accountgl-wbs_element  = gs_item-zuonr.

    "分配
    wa_accountgl-alloc_nmbr = gs_item-zuonr.

    APPEND wa_accountgl TO it_accountgl .
    "科目表
    CLEAR wa_accountreceivable.

    wa_accountreceivable-itemno_acc = '0000000020'.
    "对方科目
    wa_accountreceivable-gl_account = gs_item-dfkm.
    "项目文本
    wa_accountreceivable-item_text = gs_item-gzwb.

    "客户
    wa_accountreceivable-customer = gs_item-kunnr.

    "分配
    wa_accountreceivable-alloc_nmbr = gs_item-zuonr.

    APPEND wa_accountreceivable TO it_accountreceivable.

    "科目表
    CLEAR:wa_accountgl.
    wa_accountgl-itemno_acc = '0000000030'.
    "对方科目
    wa_accountgl-gl_account = gs_item-sjtzkm.
    "项目文本
    wa_accountgl-item_text = gs_item-gzwb.

    "WBS元素
    wa_accountgl-wbs_element  = gs_item-zuonr.

    "分配
    wa_accountgl-alloc_nmbr = gs_item-zuonr.

    APPEND wa_accountgl TO it_accountgl .


    "金额
    " **********************************************************************
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc = '0000000010'.
    " *       货币
    wa_currencyamount-currency = gs_item-waers .

*       金额
    IF gs_item-shkzg EQ 'S'.

      wa_currencyamount-amt_doccur = gs_item-wrbtr  * -1 .
    ELSE.
      wa_currencyamount-amt_doccur = gs_item-wrbtr .
    ENDIF.

    APPEND wa_currencyamount TO it_currencyamount.

    "金额
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc = '0000000020'.
    " *       货币
    wa_currencyamount-currency = gs_item-waers  .
*       金额


    IF gs_item-shkzg EQ 'H'.

      wa_currencyamount-amt_doccur = gs_item-tzysys  * -1 .
    ELSE.
      wa_currencyamount-amt_doccur = gs_item-tzysys .
    ENDIF.



    APPEND wa_currencyamount TO it_currencyamount.

    "金额
    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc = '0000000030'.
    " *       货币
    wa_currencyamount-currency = gs_item-waers .
*       金额

    IF gs_item-shkzg EQ 'S'.

      wa_currencyamount-amt_doccur = gs_item-tzse   * -1 .
    ELSE.
      wa_currencyamount-amt_doccur = gs_item-tzse  .
    ENDIF.



    APPEND wa_currencyamount TO it_currencyamount.
    "* "获利能力段
* CLEAR WA_CRITERIA.
* WA_CRITERIA-ITEMNO_ACC =  '0000000010'.
* WA_CRITERIA-FIELDNAME = 'KAUFN' ."销售订单
* WA_CRITERIA-CHARACTER = GS_TOTAL-VBEL2 .
* APPEND WA_CRITERIA TO IT_CRITERIA.
* WA_CRITERIA-FIELDNAME = 'KDPOS'. "销售订单行项目
* WA_CRITERIA-CHARACTER = GS_TOTAL-POSN2.
* APPEND WA_CRITERIA TO IT_CRITERIA.


    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = wa_documentheader
*       CUSTOMERCPD       =
*       CONTRACTHEADER    =
      IMPORTING
        obj_type          = wa_obj-obj_type
        obj_key           = wa_obj-obj_key
        obj_sys           = wa_obj-obj_sys
      TABLES
        accountgl         = it_accountgl
        accountreceivable = it_accountreceivable
*       ACCOUNTPAYABLE    =
*       ACCOUNTTAX        =
        currencyamount    = it_currencyamount
      " CRITERIA          = IT_CRITERIA
     "  VALUEFIELD        = IT_VALUEFIELD
*       EXTENSION1        =
        return            = it_return
*       PAYMENTCARD       =
*       CONTRACTITEM      =
        extension2        = it_extension2
*       REALESTATE        =
*       ACCOUNTWT         =
      .

    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      "错误消息回写到ALV
      LOOP AT it_return INTO wa_return .
        IF gs_item-info_msg EQ ''.
          CONCATENATE wa_return-id wa_return-type wa_return-message
                      INTO gs_item-info_msg .
        ELSE .
          CONCATENATE gs_item-info_msg ';' wa_return-id
                      wa_return-type wa_return-message
                      INTO gs_item-info_msg .
        ENDIF.
      ENDLOOP.
      "红灯状态：
      gs_item-statu = icon_red_light .
      MODIFY gt_item FROM gs_item.
      CONTINUE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      " 回写生成的会计凭证号与会计凭证年度
      gs_item-gzpz   =  wa_obj-obj_key(10).
      gs_item-gznd =  wa_obj-obj_key+14(4).
      "绿灯状态：
      gs_item-statu = icon_green_light.
      "消息描述
      gs_item-info_msg = '凭证过账成功！'.
      "设置不可以编辑状态
      "*设置不可以编辑状态
      PERFORM frm_cell_style USING 'DFKM'
                              ''
                              CHANGING gs_item-cellstyle.
      PERFORM frm_cell_style USING 'GZWB'
                            ''
                            CHANGING gs_item-cellstyle.

      PERFORM frm_cell_style USING 'TZSE'
                             ''
                             CHANGING gs_item-cellstyle.
      PERFORM frm_cell_style USING 'SJTZKM'
                             ''
                             CHANGING gs_item-cellstyle.
      PERFORM frm_cell_style USING 'TZRQ'
                              ''
                              CHANGING gs_item-cellstyle.
      PERFORM frm_cell_style USING 'TZQJ'
                             ''
                             CHANGING gs_item-cellstyle.
      PERFORM frm_cell_style USING 'GZXXWB'
                             ''
                             CHANGING gs_item-cellstyle.
      "过程成功的行写到GT_ZFI036
      CLEAR :gs_zfi036.
      MOVE-CORRESPONDING gs_item TO :gs_zfi036.

      "创建人、创建日期、 时间

      gs_zfi036-gz_name = sy-uname.
      gs_zfi036-gz_date = sy-datum.
      gs_zfi036-gz_time = sy-uzeit.
      APPEND gs_zfi036 TO gt_zfi036.
    ENDIF.
    MODIFY gt_item FROM gs_item.
  ENDLOOP.

  "保存数据到ZFI036物理表
  IF gt_zfi036 IS NOT INITIAL.
    SORT gt_zfi036 BY bukrs belnr gjahr buzei .
    MODIFY zfi036 FROM TABLE gt_zfi036.
  ENDIF.


ENDFORM.



FORM frm_button  USING e_grid TYPE slis_data_caller_exit.
  DATA lt_f4 TYPE lvc_t_f4.
  DATA ls_f4 TYPE lvc_s_f4.

  ls_f4-fieldname = 'SJTZKM'.      "F4对应的栏位
  ls_f4-register = 'X'.
  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  ls_f4-fieldname = 'DFKM'.      "F4对应的栏位
  ls_f4-register = 'X'.
  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = gr_alvgrid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

  CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_onf4   FOR gr_alvgrid.

  CALL METHOD gr_alvgrid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].
ENDFORM.

FORM sub_help_hkont   CHANGING p_hkont..
  " SET PARAMETER ID 'BUK' FIELD WA_HEAD-BUKRS.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      matchcode_object          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      select_value              = p_hkont
    EXCEPTIONS
      invalid_dictionary_field  = 1
      invalid_matchdcode_object = 2
      no_selection              = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CELL_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2277   text
*      -->P_2278   text
*      <--P_GS_ITEM_CELLSTYLE  text
*----------------------------------------------------------------------*
FORM frm_cell_style  USING    p_fieldname
                              p_editable
                     CHANGING pt_cellstyle TYPE lvc_t_styl.
  DATA: lw_cellstyle TYPE lvc_s_styl.

  READ TABLE pt_cellstyle INTO lw_cellstyle WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    MODIFY TABLE pt_cellstyle FROM lw_cellstyle.
  ELSE.
    lw_cellstyle-fieldname = p_fieldname.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    INSERT lw_cellstyle INTO TABLE pt_cellstyle.
  ENDIF.
ENDFORM.
