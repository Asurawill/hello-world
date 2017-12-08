REPORT zfi037.
"项目销项税调整冲销
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
        cxpz       TYPE belnr_d,  "冲销凭证
        cxnd       TYPE gjahr,     "冲销年度
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


DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

" * BAPI_ACC_DOCUMENT_REV_POST
DATA: wa_reversal TYPE bapiacrev,
      wa_bus      TYPE bapiache09.

DATA: wa_obj TYPE bapiache09.

DATA: it_return TYPE TABLE OF bapiret2,
      wa_return TYPE bapiret2.


DATA: g_stgrd TYPE bkpf-stgrd.

DATA g_answer     TYPE string. "控制弹出框

DATA:gt_bkpf LIKE TABLE OF bkpf WITH HEADER LINE,
     gs_bkpf LIKE bkpf.

"声明类及定义方法来处理data_changed_finished事件


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
      wa_extension2        TYPE bapiparex.




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
if gt_zfi036_2 is not initial.
select * into table gt_bkpf
  from bkpf
  for all entries in gt_zfi036_2
  where bukrs = gt_zfi036_2-bukrs
  and   gjahr = gt_zfi036_2-gznd
  and   belnr = gt_zfi036_2-gzpz.

sort gt_bkpf by bukrs gjahr belnr .

endif.

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
      <fs_item>-gznd = gs_zfi036_2-gznd.
      <fs_item>-gzpz = gs_zfi036_2-gzpz.
       else.
         delete gt_item index l_tabix .
         continue.
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
  "PERFORM frm_build_event.
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
  init_fieldcat 'STATU'         '冲销状态'             '' '' '' '' '' '' ''.
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
  init_fieldcat 'DFKM'         '对方科目'         '' '' '' '' '' '' ''.
  init_fieldcat 'DFKM_TXT'     '对方科目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'TZSE'          '调整税额'         '' '' '' '' '' '' ''.
  init_fieldcat 'TZYSYS'       '调整应收预收金额'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJTZKM'       '税金调整科目'         '' '' '' '' '' '' ''.
  init_fieldcat 'SJTZKM_TXT'    '税金调整科目描述'         '' '' '' '' '' '' ''.
  init_fieldcat 'TZRQ'          '调整日期'         '' '' '' '' '' 'BKPF' 'BUDAT'.
  init_fieldcat 'TZQJ'         '调整期间'         '' '' '' '' '' 'BKPF' 'MONAT'.
  init_fieldcat 'GZWB'         '过账文本'         '' '' '' '' '' '' ''.
  init_fieldcat 'GZPZ'          '过账凭证'         '' '' '' '' '' '' ''.
  init_fieldcat 'GZND'         '过账凭证年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'CXPZ'          '冲销凭证'         '' '' '' '' '' '' ''.
  init_fieldcat 'CXND'         '冲销凭证年度'         '' '' '' '' '' '' ''.
  init_fieldcat 'INFO_MSG'      '冲销消息文本'     '' '' '' '' '' '' ''.
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
*  gw_events-name =  'CALLER_EXIT' .
*  gw_events-form =  'FRM_BUTTON'.   "f4事件
*  APPEND gw_events TO gt_events.
*  gw_events-name =  slis_ev_data_changed.
*  gw_events-form = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
*  APPEND gw_events TO gt_events.
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
    " it_events                = gt_events[]
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


FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.



FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
      READ TABLE gt_item INTO gs_item INDEX rs_selfield-tabindex.
      CHECK sy-subrc = 0.
      IF rs_selfield-fieldname = 'GZPZ'
        AND gs_item-gzpz IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD gs_item-gzpz.
        SET PARAMETER ID 'BUK' FIELD gs_item-bukrs.
        SET PARAMETER ID 'GJR' FIELD gs_item-gznd.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.


      "冲销
    WHEN '&REV'.
      CLEAR g_answer.

      "check
*    READ TABLE GT_DATA  INTO GS_DATA
*    WITH KEY SEL = 'X'
*       IF SY-SUBRC = 0.
*        MESSAGE '选择项已有冲销凭证，请重新选择！' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.

*    READ TABLE GT_DATA  INTO GS_DATA
*    WITH KEY SEL = ''.
*       IF SY-SUBRC = 0.
*        MESSAGE '请选择无冲销的过账凭证项进行操作！' TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.
*      ENDIF.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          text_question  = '确认要执行冲销操作'
        IMPORTING
          answer         = g_answer
*   TABLES
*         PARAMETER      =
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF g_answer <> '1'.
        EXIT.
      ENDIF.
      "*取冲销原因和过账日期
      CLEAR wa_reversal.
      CALL SCREEN 9001 STARTING AT 25 10.
      IF wa_reversal-reason_rev IS INITIAL.
        MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      "冲销凭证
      PERFORM frm_acc_reversal.
      "保存到自建表
      IF gt_zfi036 IS NOT INITIAL.

        SORT gt_zfi036 BY bukrs gjahr belnr buzei.

        "’修改自建表
        MODIFY zfi036 FROM TABLE gt_zfi036.

      ENDIF.
  ENDCASE.
  CALL METHOD g_ref_grid->refresh_table_display.

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



*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'STA9001'.

*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'OK'.
      IF g_stgrd IS INITIAL.
        MESSAGE '冲销原因必输！' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        wa_reversal-reason_rev = g_stgrd.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANL'.
      CLEAR: wa_reversal-reason_rev,
             wa_reversal-pstng_date.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_acc_reversal .
  LOOP AT gt_item INTO gs_item WHERE sel = 'X' AND cxpz = ''.
    CLEAR:gs_bkpf,wa_return,wa_reversal-obj_type,wa_reversal-obj_key,wa_reversal-obj_key_r,wa_obj.
    REFRESH it_return.
    READ TABLE gt_bkpf INTO gs_bkpf WITH KEY bukrs = gs_item-bukrs gjahr = gs_item-gznd belnr = gs_item-gzpz BINARY SEARCH.
    IF  sy-subrc EQ 0 .
      wa_reversal-obj_type  = gs_bkpf-awtyp.
      wa_reversal-obj_key   = gs_bkpf-awkey.
      wa_reversal-obj_key_r = gs_bkpf-awkey.
    ENDIF.
    "  *   取得系统 LOGICAL SYSTEM
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system = wa_reversal-obj_sys.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
      EXPORTING
        reversal = wa_reversal
        bus_act  = 'RFBU'
      IMPORTING
        obj_type = wa_obj-obj_type
        obj_key  = wa_obj-obj_key
        obj_sys  = wa_obj-obj_sys
      TABLES
        return   = it_return.

    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR wa_reversal.
      LOOP AT it_return INTO wa_return .
        IF gs_item-info_msg EQ ''.
          CONCATENATE wa_return-id wa_return-type wa_return-message INTO gs_item-info_msg .


        ELSE.
          CONCATENATE gs_item-info_msg '；' wa_return-id wa_return-type wa_return-message INTO gs_item-info_msg .

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

      "回写生成的冲销凭证号 与冲销凭证年度
      gs_item-cxpz   =  wa_obj-obj_key(10).
      gs_item-cxnd = wa_obj-obj_key+14(4).

      "绿灯状态：
      gs_item-statu = icon_green_light.

      "消息描述
      gs_item-info_msg = '冲销成功！'.

      MODIFY gt_item FROM gs_item.

      "冲销成功的记录保存到GT_ZCO005_1.
      MOVE-CORRESPONDING gs_item TO gs_zfi036.
      "冲销人、冲销日期
      gs_zfi036-cx_name = sy-uname.
      gs_zfi036-cx_date = sy-datum.
      gs_zfi036-cx_time = sy-uzeit.
      APPEND gs_zfi036 TO gt_zfi036.
    ENDIF.
  ENDLOOP.
ENDFORM.
