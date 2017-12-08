*----------------------------------------------------------------------*
*  程序名称         :ZFI032
*  创建者           : 吴丽娟
*  创建日期         :2015-11-03
*----------------------------------------------------------------------*
*  概要说明
* 项目成本计划实际明细表
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
* 2015-11-03   HANDYWLJ      ED1K903459     创建
*
*----------------------------------------------------------------------*
REPORT zfi032.
*----------------------------------------------------------------------*
*                  I N C L U D E 程 序 块                              *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  标 准 T Y P E S  P O O L S 引 入 块                 *
*----------------------------------------------------------------------*
*引入标准type pool
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
TABLES: aufk,proj,coep.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*

TYPES:BEGIN OF ty_data,
        pspnr TYPE proj-pspnr, "项目定义
        pspid TYPE proj-pspid, "项目定义（外部）
        post1 TYPE proj-post1, "项目名称
        kstar TYPE coep-kstar, "成本要素
        ktext TYPE csku-ktext, "成本要素名称
        gjahr TYPE coep-gjahr, "年度
        perio TYPE coep-perio, "期间
        jhje  TYPE p LENGTH 16 DECIMALS 2, "计划金额
        sjje  TYPE p LENGTH 16 DECIMALS 2, "实际金额
      END OF ty_data.
TYPES:BEGIN OF ty_kstar,
        pspid TYPE proj-pspid,
        kstar TYPE coep-kstar,
        wertn TYPE kis1-wertn,
      END OF ty_kstar.
TYPES:BEGIN OF ty_prps,
        posid TYPE prps-posid,
        pspnr TYPE prps-pspnr,
        objnr TYPE prps-objnr,
      END OF ty_prps.
TYPES:BEGIN OF ty_aufk,
        pspel TYPE aufk-pspel,
        objnr TYPE aufk-objnr,
      END OF ty_aufk.
TYPES:BEGIN OF ty_coep.
        INCLUDE STRUCTURE coep.
TYPES:posid TYPE prps-posid,
      END OF ty_coep.
TYPES:BEGIN OF ty_ktext,
        kstar TYPE csku-kstar,
        ktext TYPE csku-ktext,
      END OF ty_ktext.
TYPES:BEGIN OF ty_pspel,
        pspnr TYPE proj-pspnr,
        objnr TYPE proj-objnr,
      END OF ty_pspel.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_data TYPE TABLE OF ty_data,
     wa_data TYPE ty_data.
DATA:it_data1 TYPE TABLE OF ty_data,
     wa_data1 TYPE ty_data.
DATA:it_kstar TYPE TABLE OF ty_kstar,
     wa_kstar TYPE ty_kstar.
DATA:it_kstar1 TYPE TABLE OF ty_kstar,
     wa_kstar1 TYPE ty_kstar.
DATA:it_kstar2 TYPE TABLE OF ty_kstar,
     wa_kstar2 TYPE ty_kstar.
DATA:it_kstar3 TYPE TABLE OF ty_kstar,
     wa_kstar3 TYPE ty_kstar.
DATA e_wbs_ecp        TYPE tty_proj_element_ck_items_rdex.
DATA lt_e_wbs_ecp     TYPE TABLE OF proj_element_ck_items_rdexp.
DATA ls_e_wbs_ecp     TYPE proj_element_ck_items_rdexp.
DATA lt_cost_lines    TYPE TABLE OF kis1.
DATA ls_cost_lines    TYPE kis1.
DATA:it_aufk TYPE TABLE OF ty_aufk,
     wa_aufk TYPE ty_aufk.
DATA:it_prps TYPE TABLE OF ty_prps,
     wa_prps TYPE ty_prps.
DATA:it_coep TYPE TABLE OF ty_coep,
     wa_coep TYPE ty_coep.
DATA:it_ktext TYPE TABLE OF ty_ktext,
     wa_ktext TYPE ty_ktext.
DATA:it_pspel TYPE TABLE OF ty_pspel,
     wa_pspel TYPE ty_pspel.
DATA:a TYPE prps-posid.
*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo,

     it_events   TYPE slis_t_event,
     wa_events   LIKE LINE OF it_events.
*----------------------------------------------------------------------*
*                  定义宏
*----------------------------------------------------------------------*
DEFINE init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
PARAMETERS:p_bukrs TYPE aufk-bukrs OBLIGATORY."公司代码
SELECT-OPTIONS:s_pspid FOR proj-pspid."项目编号
PARAMETERS:p_gjahr TYPE coep-gjahr OBLIGATORY, "年度
           p_perio TYPE coep-perio OBLIGATORY. "期间
SELECTION-SCREEN END OF BLOCK text.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_getdata.
  PERFORM frm_dealdata.
  PERFORM frm_layout.
  PERFORM frm_fieldcat.
  PERFORM frm_output.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_getdata .
*取项目定义、项目名称
  SELECT *
    FROM proj
    INTO CORRESPONDING FIELDS OF TABLE it_data
    WHERE pspid IN s_pspid
    AND vbukr = p_bukrs.


  IF it_data IS NOT INITIAL.
*取成本要素(计划金额下)
    LOOP AT it_data INTO wa_data.
      CLEAR:e_wbs_ecp,
         ls_e_wbs_ecp,
         ls_cost_lines.

      REFRESH:lt_e_wbs_ecp,
              lt_cost_lines.

      CALL FUNCTION 'CNECP_READ'
        EXPORTING
          i_proj_def = wa_data-pspid
          i_version  = '000'
        IMPORTING
          e_wbs_ecp  = e_wbs_ecp.

      lt_e_wbs_ecp = e_wbs_ecp.
      IF  lt_e_wbs_ecp IS NOT INITIAL.
        LOOP AT lt_e_wbs_ecp INTO ls_e_wbs_ecp.

          lt_cost_lines = ls_e_wbs_ecp-cost_lines.

          LOOP AT lt_cost_lines INTO ls_cost_lines.
            wa_kstar-kstar = ls_cost_lines-kstar.
            wa_kstar-wertn = ls_cost_lines-wertn.
            wa_kstar-pspid = wa_data-pspid.
            APPEND wa_kstar TO it_kstar.
          ENDLOOP.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

*取成本要素（实际金额下）
    SELECT *
      FROM prps
      INTO CORRESPONDING FIELDS OF TABLE it_prps
      FOR ALL ENTRIES IN it_data
      WHERE psphi = it_data-pspnr.

    IF it_prps IS NOT INITIAL.
      SELECT *
        FROM aufk
        INTO CORRESPONDING FIELDS OF TABLE it_aufk
        FOR ALL ENTRIES IN it_prps
        WHERE pspel = it_prps-pspnr
        AND bukrs = '1800'.
    ENDIF.

    LOOP AT it_prps INTO wa_prps.
      " wa_aufk-pspel = wa_prps-psphi.
      wa_aufk-objnr = wa_prps-objnr.
      APPEND wa_aufk TO it_aufk.
    ENDLOOP.

    IF it_aufk IS NOT INITIAL.
      SELECT *
        FROM coep
        "JOIN aufk ON coep~objnr = aufk~objnr
        INTO CORRESPONDING FIELDS OF TABLE it_coep
        FOR ALL ENTRIES IN it_aufk
        WHERE coep~objnr = it_aufk-objnr
        AND versn = '000'
        AND wrttp = '04'
        AND gjahr <= p_gjahr
        AND perio <= p_perio.
    ENDIF.
  ENDIF.
*取成本要素名称
  MOVE-CORRESPONDING it_kstar TO it_kstar3.
  LOOP AT it_coep INTO wa_coep.
    wa_kstar3-kstar = wa_coep-kstar.
    "wa_kstar-pspnr = wa_coep-pspel.
    APPEND wa_kstar3 TO it_kstar3.
  ENDLOOP.

  IF it_kstar3 IS NOT INITIAL.
    SELECT kstar ktext
      FROM csku
      INTO CORRESPONDING FIELDS OF TABLE it_ktext
      FOR ALL ENTRIES IN it_kstar3
      WHERE kstar = it_kstar3-kstar
      AND ktopl = '1000'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_dealdata .

  LOOP AT it_kstar INTO wa_kstar.
    CLEAR wa_kstar1.
    wa_kstar1-pspid = wa_kstar-pspid.
    wa_kstar1-kstar = wa_kstar-kstar.
    wa_kstar1-wertn = wa_kstar-wertn.
    COLLECT wa_kstar1 INTO it_kstar1.
  ENDLOOP.

  MOVE-CORRESPONDING it_kstar1 TO it_kstar2.

  LOOP AT it_coep INTO wa_coep.
    DELETE it_kstar1 WHERE kstar = wa_coep-kstar.
    CLEAR wa_coep.
  ENDLOOP.

  LOOP AT it_coep INTO wa_coep.
    READ TABLE it_prps INTO wa_prps WITH KEY objnr = wa_coep-objnr.
    IF sy-subrc = 0.
      wa_coep-posid = wa_prps-posid.
    ENDIF.

    IF wa_coep-posid IS INITIAL.
      READ TABLE it_aufk INTO wa_aufk WITH KEY objnr = wa_coep-objnr.
      IF sy-subrc = 0.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            input  = wa_aufk-pspel
          IMPORTING
            output = a.

        wa_coep-posid = a.
      ENDIF.
    ENDIF.
    MODIFY it_coep FROM wa_coep.
    CLEAR wa_coep.
  ENDLOOP.

  LOOP AT it_coep INTO wa_coep.
    wa_data1-pspid = wa_coep-posid.

    SELECT post1
      FROM prps
      INTO wa_data1-post1
      WHERE posid = wa_data1-pspid.
    ENDSELECT.

    wa_data1-kstar = wa_coep-kstar.

    READ TABLE it_ktext INTO wa_ktext WITH KEY kstar = wa_data1-kstar.
    IF sy-subrc = 0.
      wa_data1-ktext = wa_ktext-ktext.
    ENDIF.

    wa_data1-gjahr = wa_coep-gjahr.

    wa_data1-perio = wa_coep-perio.

    READ TABLE it_kstar2 INTO wa_kstar2 WITH KEY kstar = wa_data1-kstar.
    IF sy-subrc = 0.
      wa_data1-jhje = wa_kstar2-wertn.
    ENDIF.

    wa_data1-sjje = wa_coep-wtgbtr.

    APPEND wa_data1 TO it_data1.
    CLEAR wa_data1.
    CLEAR wa_coep.
  ENDLOOP.

  LOOP AT it_kstar1 INTO wa_kstar1.
    wa_data1-pspid = wa_kstar1-pspid.
    SELECT post1
      FROM proj
      INTO wa_data1-post1
      WHERE pspid = wa_data1-pspid.
    ENDSELECT.
    wa_data1-gjahr = p_gjahr.
    wa_data1-perio = p_perio.
    wa_data1-kstar = wa_kstar1-kstar.
    SELECT ktext
      FROM csku
      INTO wa_data1-ktext
      WHERE kstar = wa_data1-kstar
      AND ktopl = '1000'.
    ENDSELECT.
    wa_data1-jhje = wa_kstar1-wertn.
    APPEND wa_data1 TO it_data1.
    CLEAR wa_data1.
    CLEAR wa_kstar1.
  ENDLOOP.

  SORT it_data1 BY pspid kstar.

  IF it_data1 IS INITIAL.
    MESSAGE 'No data!' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_layout .
  wa_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  init_fieldcat 'PSPID' '项目定义' 'PROJ' 'PSPID'.
  init_fieldcat 'POST1' '项目名称' 'PROJ' 'POST1'.
  init_fieldcat 'KSTAR' '成本要素' 'COEP' 'KSTAR'.
  init_fieldcat 'KTEXT' '成本要素名称' 'CKSU' 'KTEXT'.
  init_fieldcat 'GJAHR' '年度' 'COEP' 'GJAHR'.
  init_fieldcat 'PERIO' '期间' 'COEP' 'PERIO'.
  init_fieldcat 'JHJE' '计划金额' '' ''.
  init_fieldcat 'SJJE' '实际金额' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_output .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = it_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        =
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = it_data1
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
