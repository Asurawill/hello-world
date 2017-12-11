REPORT zps004.

TYPE-POOLS:slis.

TABLES:proj.

TYPES: BEGIN OF ty_tab,
         pspid   TYPE proj-pspid, "项目定义
         post1   TYPE proj-post1, "项目名称
         zhtje   TYPE proj-zhtje, "合同总价
         zclf    TYPE proj-zhtje, "材料费
         clf     TYPE proj-zhtje, "材料费
         yzf     TYPE proj-zhtje, "运杂费
         fcf     TYPE proj-zhtje, "辅材费
         rgf     TYPE proj-zhtje, "人工费
         jxsyf   TYPE proj-zhtje, "机械使用费
         qtzjf   TYPE proj-zhtje, "其他直接费
         jyf     TYPE proj-zhtje, "检验费
         xcsdf   TYPE proj-zhtje, "现场水电费
         hjbhf   TYPE proj-zhtje, "环境保护费
         aqwmsg  TYPE proj-zhtje, "安全文明施工
         lsssf   TYPE proj-zhtje, "临时设施费
         cpbhf   TYPE proj-zhtje, "成品保护费
         tzdyf   TYPE proj-zhtje, "图纸打印费
         zbphf   TYPE proj-zhtje, "总包配合费
         jjfy    TYPE proj-zhtje, "间接费用
         glrygz  TYPE proj-zhtje, "管理人员工资/社保
         grbxf   TYPE proj-zhtje, "工人保险费
         chlf    TYPE proj-zhtje, "差旅费
         snjtf   TYPE proj-zhtje, "市内交通费
         xmjyf   TYPE proj-zhtje, "项目经营费
         xcbgf   TYPE proj-zhtje, "现场办公用品费、座机、网络费
         bkyjf   TYPE proj-zhtje, "不可预计费
         lybhf   TYPE proj-zhtje, "履约保函费
         qqfy    TYPE proj-zhtje, "前期费用
         yxywf   TYPE proj-zhtje, "营销业务费
         sjf     TYPE proj-zhtje, "设计费
         xmqqtbf TYPE proj-zhtje, "项目前期投标费
         hjcb    TYPE proj-zhtje, "合计成本
         sjfy    TYPE proj-zhtje, "上交费用
         sjbl    TYPE proj-zhtje, "上交比例
       END OF ty_tab.
TYPES:BEGIN OF ty_prps,
        posid TYPE prps-posid,
        objnr TYPE prps-objnr,
      END OF ty_prps.
TYPES:BEGIN OF ty_pspnr,
        pspnr TYPE proj-pspnr,
      END OF ty_pspnr.
TYPES:BEGIN OF ty_cosp.
        INCLUDE STRUCTURE cosp.
TYPES: psphi TYPE prps-psphi,
       END OF ty_cosp.

DATA:it_tab TYPE TABLE OF ty_tab.
DATA:wa_tab TYPE ty_tab.
DATA:it_prps TYPE TABLE OF ty_prps.
DATA:wa_prps TYPE ty_prps.
DATA:it_cosp TYPE TABLE OF ty_cosp.
DATA:wa_cosp TYPE ty_cosp.
DATA:it_pspnr TYPE TABLE OF ty_pspnr.
DATA:wa_pspnr TYPE ty_pspnr.
DATA e_wbs_ecp        TYPE tty_proj_element_ck_items_rdex.
DATA lt_e_wbs_ecp     TYPE TABLE OF proj_element_ck_items_rdexp.
DATA ls_e_wbs_ecp     TYPE proj_element_ck_items_rdexp.
DATA lt_cost_lines    TYPE TABLE OF kis1.
DATA ls_cost_lines    TYPE kis1.
DATA:g_xmdy(2) TYPE c.

DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo.

DEFINE init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_xmdy FOR proj-pspid.
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

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_tab"项目定义、项目名称、合同总价
    FROM proj
    WHERE pspid IN s_xmdy.

  SELECT *
    FROM cosp
    JOIN prps ON cosp~objnr = prps~objnr
    INTO CORRESPONDING FIELDS OF TABLE it_cosp
    WHERE prps~usr01 = s_xmdy
    AND cosp~wrttp = '04'
    AND ( cosp~kstar LIKE '5%' OR cosp~kstar LIKE '8%' ).

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

  LOOP AT it_tab INTO wa_tab.

    CLEAR:e_wbs_ecp,
          ls_e_wbs_ecp,
          ls_cost_lines.

    REFRESH:lt_e_wbs_ecp,
            lt_cost_lines.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        i_proj_def = wa_tab-pspid
        i_version  = '000'
      IMPORTING
        e_wbs_ecp  = e_wbs_ecp.

    lt_e_wbs_ecp = e_wbs_ecp.

    IF  lt_e_wbs_ecp IS NOT INITIAL.
      READ TABLE lt_e_wbs_ecp INTO ls_e_wbs_ecp INDEX 1.
      IF sy-subrc = 0.
        lt_cost_lines = ls_e_wbs_ecp-cost_lines.

        LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010101'."材料费
          wa_tab-clf = wa_tab-clf + ls_cost_lines-wertn.
        ENDLOOP.

        g_xmdy = wa_tab-pspid+4(2).

        IF g_xmdy = 21.
          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010401'."运杂费
            wa_tab-yzf = wa_tab-yzf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010801'."辅材费
            wa_tab-fcf = wa_tab-fcf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010201'."人工费
            wa_tab-rgf = wa_tab-rgf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010301'."机械使用费
            wa_tab-jxsyf = wa_tab-jxsyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010402'."检验费
            wa_tab-jyf = wa_tab-jyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010513'."现场水电费
            wa_tab-xcsdf = wa_tab-xcsdf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010514'."环境保护费
            wa_tab-hjbhf = wa_tab-hjbhf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010515'."安全文明施工
            wa_tab-aqwmsg = wa_tab-aqwmsg + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010502'."临时设施费
            wa_tab-lsssf = wa_tab-lsssf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010516'."成品保护费
            wa_tab-cpbhf = wa_tab-cpbhf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010511'."图纸打印费
            wa_tab-tzdyf = wa_tab-tzdyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010505'."总包配合费
            wa_tab-zbphf = wa_tab-zbphf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010501'."管理人员工资/社保
            wa_tab-glrygz = wa_tab-glrygz + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010506'."工人保险费
            wa_tab-grbxf = wa_tab-grbxf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010508'."差旅费
            wa_tab-chlf = wa_tab-chlf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010509'."市内交通费
            wa_tab-snjtf = wa_tab-snjtf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010503'."项目经营费
            wa_tab-xmjyf = wa_tab-xmjyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010504'."现场办公用品费、座机、网络费
            wa_tab-xcbgf = wa_tab-xcbgf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '5401010512'."不可预计费
            wa_tab-bkyjf = wa_tab-bkyjf + ls_cost_lines-wertn.
          ENDLOOP.

        ELSEIF g_xmdy = 22 OR g_xmdy = 23.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000014'."运杂费
            wa_tab-yzf = wa_tab-yzf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8005000001'."辅材费
            wa_tab-fcf = wa_tab-fcf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000103'."人工费
            wa_tab-rgf = wa_tab-rgf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000065'."机械使用费
            wa_tab-jxsyf = wa_tab-jxsyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000067'."检验费
            wa_tab-jyf = wa_tab-jyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000020'."现场水电费
            wa_tab-xcsdf = wa_tab-xcsdf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000096'."环境保护费
            wa_tab-hjbhf = wa_tab-hjbhf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000097'."安全文明施工
            wa_tab-aqwmsg = wa_tab-aqwmsg + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000098'."临时设施费
            wa_tab-lsssf = wa_tab-lsssf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000099'."成品保护费
            wa_tab-cpbhf = wa_tab-cpbhf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000100'."图纸打印费
            wa_tab-tzdyf = wa_tab-tzdyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000101'."总包配合费
            wa_tab-zbphf = wa_tab-zbphf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE ( kstar = '8001000001' OR kstar = '8001000103' OR kstar = '8001000008')."管理人员工资/社保
            wa_tab-glrygz = wa_tab-glrygz + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8001000012'."工人保险费
            wa_tab-grbxf = wa_tab-grbxf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000007'."差旅费
            wa_tab-chlf = wa_tab-chlf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000009'."市内交通费
            wa_tab-snjtf = wa_tab-snjtf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000008'."项目经营费
            wa_tab-xmjyf = wa_tab-xmjyf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE ( kstar = '8003000010' OR kstar = '8003000037')."现场办公用品费、座机、网络费
            wa_tab-xcbgf = wa_tab-xcbgf + ls_cost_lines-wertn.
          ENDLOOP.

          LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000061'."不可预计费
            wa_tab-bkyjf = wa_tab-bkyjf + ls_cost_lines-wertn.
          ENDLOOP.

        ENDIF.

        LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '6603060101'."履约保函费
          wa_tab-lybhf = wa_tab-lybhf + ls_cost_lines-wertn.
        ENDLOOP.

        LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000008'."营销业务费
          wa_tab-yxywf = wa_tab-yxywf + ls_cost_lines-wertn.
        ENDLOOP.

        LOOP AT lt_cost_lines INTO ls_cost_lines WHERE kstar = '8003000095'."设计费
          wa_tab-sjf = wa_tab-sjf + ls_cost_lines-wertn.
        ENDLOOP.

      ENDIF.
    ENDIF.

    wa_tab-zclf = wa_tab-clf + wa_tab-yzf + wa_tab-fcf."材料费

    wa_tab-qtzjf = wa_tab-jyf + wa_tab-xcsdf + wa_tab-hjbhf
                   + wa_tab-aqwmsg + wa_tab-lsssf +
                   wa_tab-cpbhf + wa_tab-tzdyf + wa_tab-zbphf."其他直接费

    wa_tab-qqfy = wa_tab-yxywf + wa_tab-sjf."前期费用

    wa_tab-jjfy = wa_tab-glrygz + wa_tab-grbxf + wa_tab-chlf
                  + wa_tab-snjtf + wa_tab-xmjyf +
                  wa_tab-xcbgf + wa_tab-bkyjf + wa_tab-lybhf + wa_tab-qqfy."间接费用

    READ TABLE it_cosp INTO wa_cosp WITH KEY psphi = wa_tab-pspid.
    IF sy-subrc = 0.
      wa_tab-xmqqtbf = wa_cosp-wtg001 + wa_cosp-wtg002 + wa_cosp-wtg003 + wa_cosp-wtg004 +
                       wa_cosp-wtg005 + wa_cosp-wtg006 + wa_cosp-wtg007 + wa_cosp-wtg008 +
                       wa_cosp-wtg009 + wa_cosp-wtg010 + wa_cosp-wtg011 + wa_cosp-wtg012 +
                       wa_cosp-wtg013 + wa_cosp-wtg014 + wa_cosp-wtg015 + wa_cosp-wtg016."项目前期投标费
    ENDIF.

    wa_tab-hjcb = wa_tab-zclf + wa_tab-rgf + wa_tab-jxsyf + wa_tab-qtzjf + wa_tab-jjfy + wa_tab-qqfy."合计费用

    wa_tab-sjfy = wa_tab-zhtje - wa_tab-hjcb."上交费用

    IF wa_tab-zhtje <> 0.
      wa_tab-sjbl = ( wa_tab-zhtje - wa_tab-hjcb ) / wa_tab-zhtje."上交比例
    ELSE.
*      message:'合同总价为0时，上交比例无法计算' type 'I' display like 'W'.
*      continue.
      wa_tab-sjbl = 0.
    ENDIF.
    MODIFY it_tab FROM wa_tab.
    CLEAR wa_tab.
  ENDLOOP.

  IF it_tab IS INITIAL.
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
  init_fieldcat 'ZHTJE' '合同总价' 'PROJ' 'ZHTJE'.
  init_fieldcat 'ZCLF' '材料费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'CLF' '材料费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'YZF' '运杂费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'FCF' '辅材费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'RGF' '人工费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'JXSYF' '机械使用费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'QTZJF' '其他直接费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'JYF' '检验费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'XCSDF' '现场水电费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'HJBHF' '环境保护费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'AQWMSG' '安全文明施工' 'PROJ' 'ZHTJE'.
  init_fieldcat 'LSSSF' '临时设施费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'CPBHF' '成品保护费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'TZDYF' '图纸打印费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'ZBPHF' '总包配合费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'JJFY' '间接费用' 'PROJ' 'ZHTJE'.
  init_fieldcat 'GLRYGZ' '管理人员工资/社保' 'PROJ' 'ZHTJE'.
  init_fieldcat 'CHLF' '差旅费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'SNJTF' '市内交通费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'XMJYF' '项目经营费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'XCBGF' '现场办公用品费、座机、网络费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'BKYJF' '不可预计费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'LYBHF' '履约保函费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'QQFY' '前期费用' 'PROJ' 'ZHTJE'.
  init_fieldcat 'YXYWF' '营销业务费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'SJF' '设计费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'XMQQTBF' '项目前期投标费' 'PROJ' 'ZHTJE'.
  init_fieldcat 'HJCB' '合计成本' 'PROJ' 'ZHTJE'.
  init_fieldcat 'SJFY' '上交费用' 'PROJ' 'ZHTJE'.
  init_fieldcat 'SJBL' '上交比例' 'PROJ' 'ZHTJE'.
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
      t_outtab           = it_tab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
