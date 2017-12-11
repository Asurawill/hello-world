*&---------------------------------------------------------------------*
*& Report  ZMM008_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmm008_2 MESSAGE-ID zmm01.
TABLES : zmm002_2.
TYPE-POOLS : slis.
INCLUDE:<icon>.
**INTERNAL TABLE DECLARTION
DATA :
  gr_alv     TYPE REF TO cl_salv_table,
  gr_columns TYPE REF TO cl_salv_columns_table.

DATA: it_fieldcat TYPE  slis_t_fieldcat_alv WITH HEADER LINE,

      g_save      TYPE c VALUE 'X',
      g_variant   TYPE disvariant,
      gx_variant  TYPE disvariant,
      g_exit      TYPE c,
      gt_events   TYPE slis_t_event,
      gw_events   TYPE slis_alv_event.

DATA:BEGIN OF it_out OCCURS 0.
        INCLUDE TYPE zmm002_2.
DATA zbox TYPE c.
DATA:END OF it_out.
FIELD-SYMBOLS <wa_zmm002_2> LIKE LINE OF it_out.
DATA wa_zmm002_2 like it_out.
DATA it_zmm002_2 LIKE TABLE OF wa_zmm002_2.
data up_mode TYPE c.

DATA max_dbdh TYPE zdbdh.
DATA it_zmm002_2db TYPE TABLE OF zmm002_2.
DATA wa_zmm002_2db LIKE LINE OF it_zmm002_2db.
DATA it_zmm002_2sel TYPE TABLE OF zmm002_2 WITH HEADER LINE.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
SELECTION-SCREEN:  BEGIN OF LINE,
      PUSHBUTTON 1(10)   but3 USER-COMMAND ONLI,
      PUSHBUTTON 14(10)  but1 USER-COMMAND but1,
      PUSHBUTTON 28(10)  but2 USER-COMMAND but2,
    END OF LINE.
SELECT-OPTIONS: s_dbdh   FOR zmm002_2-dbdh no INTERVALS NO-EXTENSION ,
                s_werks  FOR zmm002_2-werks,
                s_lgort  FOR zmm002_2-lgort,
                s_UMLGO  FOR zmm002_2-UMLGO,
                s_matnr  FOR zmm002_2-matnr,
                s_bwart  FOR zmm002_2-bwart.

*PARAMETERS :    p_sel1 TYPE c AS CHECKBOX,
*                p_sel2 TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
**GETTING DEFAULT VARIANT

INITIALIZATION.
  gx_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    g_variant = gx_variant-variant.
  ENDIF.


AT USER-COMMAND.

AT SELECTION-SCREEN OUTPUT.
  MOVE '单据新增'     TO but1. "给PUBU1按钮赋值描述
  MOVE '已有单据修改' TO but2. "给PUBU2按钮赋值描述
  MOVE '已有单据显示' TO but3. "给PUBU3按钮赋值描述

AT SELECTION-SCREEN.
  IF sy-ucomm = 'BUT1' OR sy-ucomm = 'BUT2'.
    IF sy-ucomm = 'BUT2'.
      READ TABLE s_dbdh INDEX 1.
      IF s_dbdh-low = ''.
        MESSAGE '请输入需要修改的单号' TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
      CHECK s_dbdh-low <> ''.
      up_mode = 'U'.
      SELECT * FROM zmm002_2
        INTO CORRESPONDING FIELDS OF TABLE it_zmm002_2
        WHERE dbdh = s_dbdh-low.
      IF sy-subrc = 0.
        delete it_zmm002_2 WHERE zuser <> sy-uname.
        IF it_zmm002_2[] is NOT INITIAL.
          CALL SCREEN 9001 STARTING AT 10 1 ENDING AT 150 18.
         ELSE.
        MESSAGE '不能修改他人所制单据' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.
      ELSE.
        MESSAGE '请输入正确单据号' TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
    ELSE.
      up_mode = 'I'.
      CLEAR: it_zmm002_2[],it_zmm002_2.
      DO 10 TIMES.
        APPEND INITIAL LINE TO it_zmm002_2.
      ENDDO.
      APPEND INITIAL LINE TO it_zmm002_2.
      CALL SCREEN 9001 STARTING AT 10 1 ENDING AT 150 20.
    ENDIF.
  ENDIF.

  PERFORM frm_auth_check.
**PERFORM DECLARATIONS
START-OF-SELECTION.
  PERFORM data_retrivel.
  PERFORM build_fieldcatalog.
  PERFORM display_alv_report.

FORM frm_auth_check.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_retrivel .
  SELECT * FROM zmm002_2 INTO CORRESPONDING FIELDS OF TABLE it_out
    WHERE dbdh IN s_dbdh
    and werks in s_werks
    and lgort in s_lgort
    and umlgo in s_UMLGO
    and matnr in s_matnr
    and bwart in s_bwart
    .


ENDFORM.                    " DATA_RETRIVEL

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .


  CLEAR it_fieldcat[].
  PERFORM frm_fill_cat USING :
       "  16  ''  'PRTFLAG'     text-016   , "是否打印
           1  ''  'DBDH' '调拨单号',"调拨单号
           2  ''  'ZUSER' '用户名 ',"用户名
           3  ''  'ZDATUM' '日期',"日期
           4  ''  'WERKS' '工厂',"工厂
           5  ''  'BWART' '移动类型',"移动类型
           6  ''  'MATNR' '物料',"物料
           7  ''  'MAKTX' '描述',"描述
           8  ''  'KWMENG' '订单数量',"订单数量
           9  ''  'MEINS' '基本单位',"基本单位
           10 ''  'SPCID' '特殊库存标示',"特殊库存标示
           11 ''  'UMLGO' '目标库位',"收货库存地点
           12 ''  'VBELNTO' '销售凭证 ',"销售凭证
           13 ''  'POSNRTO' '项目',"项目
           14 ''  'LGORT' '来源库位',"库存地点
           15 ''  'VBELNFR' '销售凭证 ',"销售凭证
           16 ''  'POSNRFR' '项目',"项目
           17 ''  'XCHPF' '批次标示 ',"批次标示
           18 ''  'CHARG' '批次',"批次
           19 ''  'ZRUSER' '责任人 ',"责任人
           20 ''  'REMARK' '备注'."备注

ENDFORM.                    " BUILD_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  frm_fill_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_POS      text
*      -->U_EDIT     text
*      -->U_FNAME    text
*      -->U_NAME     text
*----------------------------------------------------------------------*
FORM frm_fill_cat USING u_pos u_edit u_fname u_name.
  DATA:lw_fieldcat LIKE LINE OF it_fieldcat.
  lw_fieldcat-col_pos     = u_pos.
  lw_fieldcat-edit        = u_edit.
  lw_fieldcat-fieldname   = u_fname.
  lw_fieldcat-seltext_l   = u_name.
  APPEND lw_fieldcat TO it_fieldcat.
ENDFORM.                    "frm_fill_cat

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report .

  DATA:
    l_layout        TYPE  slis_layout_alv,
    l_grid_settings TYPE  lvc_s_glay.

* l_layout-CWIDTH_OPT = 'X'.
  l_layout-box_fieldname = 'ZBOX'.
  l_layout-colwidth_optimize = 'X'.
  l_grid_settings-edt_cll_cb ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_top_of_page  = 'TOP-OF-PAGE'  "see FORM
      i_callback_user_command = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      it_fieldcat             = it_fieldcat[]
      i_save                  = 'X'
      i_grid_settings         = l_grid_settings
      is_layout               = l_layout
      is_variant              = g_variant
    TABLES
      t_outtab                = it_out
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_REPORT" DISPLAY_ALV_REPORT

*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM top-of-page.
*ALV Header declarations
  DATA: t_header      TYPE slis_t_listheader,
        wa_header     TYPE slis_listheader,
        t_line        LIKE wa_header-info,
        ld_lines      TYPE i,
        ld_linesc(10) TYPE c.
* Title
  wa_header-typ  = 'H'.
  wa_header-info =  sy-title."'进料检验记录表打印'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.
* Date
  wa_header-typ  = 'S'.
  wa_header-key = 'Date: '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command  USING r_ucomm LIKE sy-ucomm
                                   rs_selfield TYPE slis_selfield.
  CASE sy-ucomm.
    WHEN '&ENTER'.
    WHEN '&DATA_SAVE'.
      PERFORM frm_save_data.
    WHEN '&PRNT'.
      CLEAR it_zmm002_2sel[].
      LOOP AT it_out ASSIGNING <wa_zmm002_2> WHERE zbox = 'X'.
        MOVE-CORRESPONDING <wa_zmm002_2> to it_zmm002_2sel.
        APPEND it_zmm002_2sel.
      ENDLOOP.
      IF it_zmm002_2sel[] is NOT INITIAL.
      PERFORM frm_print_data TABLES it_zmm002_2sel.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  frm_save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_save_data.

ENDFORM.                    "frm_save_data

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZMM008_2_STATUS'.
ENDFORM.                    "set_pf_status





*&---------------------------------------------------------------------*
*&      Form  frm_read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_TDNAME   text
*      -->T_TDSPRAS  text
*      -->T_TDID     text
*      -->T_TDOBJECT text
*      -->T_TEXT     text
*----------------------------------------------------------------------*
FORM frm_read_text USING t_tdname t_tdspras t_tdid t_tdobject CHANGING t_text.
  DATA:lt_tline TYPE tline OCCURS 0 WITH HEADER LINE.
*  DATA:stxl LIKE stxl OCCURS 0 WITH HEADER LINE."抬头备注
  DATA l_stxl TYPE stxl.
  l_stxl-tdid     = t_tdid     .
  l_stxl-tdspras  = t_tdspras  .
  l_stxl-tdname   = t_tdname   .
  l_stxl-tdobject = t_tdobject .

*  SELECT SINGLE * FROM STXL INTO STXL
*    WHERE TDNAME = T_TDNAME AND TDID = T_TDID AND TDSPRAS = T_TDSPRAS AND TDOBJECT = T_TDOBJECT.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = l_stxl-tdid    "读取文本的id
      language                = l_stxl-tdspras "读取文本的语言
      name                    = l_stxl-tdname    "读取文本的名字
      object                  = l_stxl-tdobject
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

*  DATA: itemp LIKE thead-tdname."itemp为变量无值

  LOOP AT lt_tline .
    CONCATENATE t_text lt_tline-tdline INTO t_text SEPARATED BY space.  "解决回车事件
  ENDLOOP.

ENDFORM. "readitemtext

INCLUDE zmm008_2_o01.

INCLUDE zmm008_2_i01.

*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE ZMM008_2TAB .
*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE ZMM008_2TAB_O01 .
INCLUDE ZMM008_2TAB_I01 .
INCLUDE ZMM008_2TAB_F01 .

INCLUDE zmm008_2_f01.
