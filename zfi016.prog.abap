*----------------------------------------------------------------------*
*  程序名称         : ZFI016
*  创建者           : 吴丽娟
*  创建日期         : 2015-08-25
*----------------------------------------------------------------------*
*  概要说明
* 金达完工比计算表
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-08-26  HANDYWLJ       ED1K902548   创建
*
*----------------------------------------------------------------------*
REPORT zfi016.
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
TABLES:prps.
*----------------------------------------------------------------------*
*                  T Y P E S - 输 出 结 构 定 义                        *
*----------------------------------------------------------------------*
TYPES:BEGIN OF ty_tab2,
        psphi TYPE prps-psphi,
        objnr TYPE prps-objnr,
        pspnr TYPE prps-pspnr,
        posid TYPE prps-posid,
        post1 TYPE prps-post1,
      END OF ty_tab2.
TYPES:BEGIN OF ty_tab.
        INCLUDE STRUCTURE zfi016.
TYPES:END OF ty_tab.
*----------------------------------------------------------------------*
*  DATA                                                                *
*----------------------------------------------------------------------*
DATA:it_tab TYPE TABLE OF ty_tab,
     wa_tab TYPE ty_tab.
DATA:it_tab1 TYPE TABLE OF ty_tab,
     wa_tab1 TYPE ty_tab.
DATA:it_tab2 TYPE TABLE OF ty_tab,
     wa_tab2 TYPE ty_tab.
DATA:it_prps TYPE TABLE OF ty_tab2 WITH HEADER LINE.
DATA:it_prps1 TYPE TABLE OF ty_tab2 WITH HEADER LINE.
DATA:it_coepb TYPE TABLE OF coepb,
     wa_coepb TYPE coepb.
CONSTANTS a TYPE p LENGTH 16 DECIMALS 4 VALUE '0.03'.
CONSTANTS b TYPE p LENGTH 16 DECIMALS 4 VALUE '0.05'.
CONSTANTS c TYPE p LENGTH 16 DECIMALS 4 VALUE '0.02'.
CONSTANTS d TYPE p LENGTH 16 DECIMALS 4 VALUE '0.0003'.
CONSTANTS e TYPE p LENGTH 16 DECIMALS 4 VALUE '0.0000'.
CONSTANTS f TYPE p LENGTH 16 DECIMALS 4 VALUE '0.0080'.
CONSTANTS g TYPE p LENGTH 16 DECIMALS 4 VALUE '0.0010'.
CONSTANTS h TYPE p LENGTH 16 DECIMALS 4 VALUE '0.0006'.
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
  wa_fieldcat-edit = &5.
  wa_fieldcat-decimals = &6.
  APPEND wa_fieldcat TO it_fieldcat.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 定 义 块
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE text-001.
PARAMETERS:p_bukrs  TYPE bseg-bukrs OBLIGATORY,
           p_period TYPE bkpf-monat OBLIGATORY,
           p_years  TYPE bseg-gjahr OBLIGATORY.
SELECT-OPTIONS:s_xmbh FOR prps-posid.
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
    INTO CORRESPONDING FIELDS OF TABLE it_tab1
  FROM zfi016.

  SELECT *
    FROM zfi016
  INTO CORRESPONDING FIELDS OF TABLE it_tab2.

  IF it_tab IS INITIAL.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_prps1   "取出所有符合条件的项目号、对象编号
      FROM prps
      WHERE pbukr = p_bukrs
      AND   posid IN s_xmbh
     AND   abgsl IS NOT NULL.

    SORT it_prps1 BY psphi objnr.           "对取出的数据按照项目号、对象编号进行排序

    LOOP AT it_prps1.                      "对于新的一个项目号，取出它对应的最小的对象编号
      AT NEW psphi.
        CLEAR it_prps.
        READ TABLE it_prps1 INDEX sy-tabix.
        IF sy-subrc = 0.
          it_prps-psphi = it_prps1-psphi.
          it_prps-objnr = it_prps1-objnr.
          it_prps-pspnr = it_prps1-pspnr.
          it_prps-posid = it_prps1-posid.
          it_prps-post1 = it_prps1-post1.
          APPEND it_prps.
        ENDIF.
      ENDAT.
    ENDLOOP.

    IF it_prps IS INITIAL.
      MESSAGE '未取到项目、对象编号！' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_coepb        "根据对象编号取出了种类和行项目的金额
    FROM coepb
    FOR ALL ENTRIES IN it_prps
    WHERE objnr = it_prps-objnr
    AND coepb~gjahr LE p_years
    AND coepb~perio LE p_period
    AND coepb~vrgng = 'KABG'
    AND coepb~kokrs = 1000.
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

  LOOP AT it_prps.
    DATA:js TYPE c VALUE 1.
    LOOP AT  it_tab1 INTO wa_tab1 WHERE psphi = it_prps-psphi AND qj = p_period AND gjahr = p_years.
      IF js = 1.
        wa_tab-psphi = wa_tab1-psphi.
        wa_tab-post1 = wa_tab1-post1.
        wa_tab-gjahr = wa_tab1-gjahr. " add it02 gjahr （原漏掉）
        wa_tab-qj = wa_tab1-qj.
        wa_tab-yysr2 = wa_tab1-yysr2.
        wa_tab-yys = wa_tab1-yys.
        wa_tab-cjs = wa_tab1-cjs.
        wa_tab-jyffj = wa_tab1-jyffj.
        wa_tab-dfjyffj = wa_tab1-dfjyffj.
        wa_tab-yhs = wa_tab1-yhs.
        wa_tab-qysds = wa_tab1-qysds.
        wa_tab-grsds = wa_tab1-grsds.
        wa_tab-sljsjjzs = wa_tab1-sljsjjzs.
        wa_tab-jgtjjj = wa_tab1-jgtjjj.
        wa_tab-zys = wa_tab1-zys.
        wa_tab-bkjj = wa_tab1-bkjj.
        wa_tab-yys2 = wa_tab1-bkjj.
        wa_tab-cjs2 = wa_tab1-cjs2.
        wa_tab-jyffj2 = wa_tab1-jyffj2.
        wa_tab-dfjyffj2 = wa_tab1-dfjyffj2.
        wa_tab-yhs2 = wa_tab1-yhs2.
        wa_tab-qysds2 = wa_tab1-qysds2.
        wa_tab-grsds2 = wa_tab1-grsds2.
        wa_tab-sljsjjzs2 = wa_tab1-sljsjjzs2.
        wa_tab-jgtjjj2 = wa_tab1-jgtjjj2.
        wa_tab-zys2 = wa_tab1-zys2.
        "MODIFY it_tab FROM wa_tab.
        APPEND wa_tab TO it_tab.
        CLEAR wa_tab.
        CONTINUE.
      ENDIF.
      js = js + 1.
    ENDLOOP.
    CLEAR js .
    js = 1.
*项目
    wa_tab-psphi = it_prps-psphi.

    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input  = it_prps-psphi
      IMPORTING
        output = wa_tab-psphi2.
*项目描述
    wa_tab-post1 = it_prps-post1.
*期间
    wa_tab-qj = p_period.
*年度
    wa_tab-gjahr = p_years.
*营业收入
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '67'.
      wa_tab-yysr2 = wa_tab-yysr2 + wa_coepb-wogbtr.
    ENDLOOP.

    wa_tab-yysr = -1 * wa_tab-yysr2.
*营业税
    wa_tab-yys = a.
*城建税
    wa_tab-cjs = b.
*教育费附加
    wa_tab-jyffj = a.
*地方教育费附加
    wa_tab-dfjyffj = c.
*印花税
    wa_tab-yhs = d.
*企业所得税
    wa_tab-qysds = e.
*个人所得税
    wa_tab-grsds = f.
*水利建设基金征收
    wa_tab-sljsjjzs = g.
*价格调节基金
    wa_tab-jgtjjj = e.
*资源税
    wa_tab-zys = e.
*帮困基金
    wa_tab-bkjj = h.
*营业税
    wa_tab-yys2 = wa_tab-yysr * wa_tab-yys.
*城建税
    wa_tab-cjs2 =  wa_tab-yysr * wa_tab-cjs.
*教育费附加
    wa_tab-jyffj2 = wa_tab-yysr * wa_tab-jyffj.
*地方教育费附加
    wa_tab-dfjyffj2 =  wa_tab-yysr * wa_tab-dfjyffj.
*印花税
    wa_tab-yhs2 = wa_tab-yysr * wa_tab-yhs.
*企业所得税
    wa_tab-qysds2 = wa_tab-yysr * wa_tab-qysds.
*个人所得税
    wa_tab-grsds2 = wa_tab-yysr * wa_tab-grsds.
*水利建设基金征收
    wa_tab-sljsjjzs2 = wa_tab-yysr * wa_tab-sljsjjzs.
*价格调节基金
    wa_tab-jgtjjj2 = wa_tab-yysr * wa_tab-jgtjjj.
*资源税
    wa_tab-zys2 = wa_tab-yysr * wa_tab-zys.
    APPEND wa_tab TO it_tab.
    CLEAR wa_tab.
  ENDLOOP.
  SORT it_tab BY psphi2.
  " endif.
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
  init_fieldcat 'PSPHI' '项目' 'PRPS' 'PSPHI' '' '4'.
  init_fieldcat 'POST1' '项目描述' 'PRPS' 'POST1' '' '4'.
  init_fieldcat 'GJAHR' '年度' '' '' '' '4'.
  init_fieldcat 'QJ' '期间' '' '' '' '4'.
  init_fieldcat 'YYSR' '营业收入' '' '' '' '4'.
  init_fieldcat 'YYS' '营业税' '' '' 'X' '4'.
  init_fieldcat 'CJS' '城建税' '' '' 'X' '4'.
  init_fieldcat 'JYFFJ' '教育费附加' '' '' 'X' '4'.
  init_fieldcat 'DFJYFFJ' '地方教育费附加' '' '' 'X' '4'.
  init_fieldcat 'YHS' '印花税' '' '' 'X' '4'.
  init_fieldcat 'QYSDS' '企业所得税' '' '' 'X' '4'.
  init_fieldcat 'GRSDS' '个人所得税' '' '' 'X' '4'.
  init_fieldcat 'SLJSJJZS' '水利建设基金征收' '' '' 'X' '4'.
  init_fieldcat 'JGTJJJ' '价格调节基金' '' '' 'X' '4'.
  init_fieldcat 'ZYS' '资源税' '' '' 'X' '4'.
  init_fieldcat 'BKJJ' '帮困基金' '' '' 'X' '4'.
  init_fieldcat 'YYS2' '营业税' '' '' '' '4'.
  init_fieldcat 'CJS2' '城建税' '' '' '' '4'.
  init_fieldcat 'JYFFJ2' '教育费附加' '' '' '' '4'.
  init_fieldcat 'DFJYFFJ2' '地方教育费附加' '' '' '' '4'.
  init_fieldcat 'YHS2' '印花税' '' '' '' '4'.
  init_fieldcat 'QYSDS2' '企业所得税' '' '' '' '4'.
  init_fieldcat 'GRSDS2' '个人所得税' '' '' '' '4'.
  init_fieldcat 'SLJSJJZS2' '水利建设基金征收' '' '' '' '4'.
  init_fieldcat 'JGTJJJ2' '价格调节基金' '' '' '' '4'.
  init_fieldcat 'ZYS2' '资源税' '' '' '' '4'.
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
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = it_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'X'
*     IS_VARIANT               =
      it_events                = it_events
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_tab
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
    WHEN '&DATA_SAVE'.
      LOOP AT it_tab INTO wa_tab.
        READ TABLE it_tab INTO wa_tab INDEX sy-tabix.
*营业税
        wa_tab-yys2 = wa_tab-yysr * wa_tab-yys.
*城建税
        wa_tab-cjs2 =  wa_tab-yysr * wa_tab-cjs.
*教育费附加
        wa_tab-jyffj2 = wa_tab-yysr * wa_tab-jyffj.
*地方教育费附加
        wa_tab-dfjyffj2 =  wa_tab-yysr * wa_tab-dfjyffj.
*印花税
        wa_tab-yhs2 = wa_tab-yysr * wa_tab-yhs.
*企业所得税
        wa_tab-qysds2 = wa_tab-yysr * wa_tab-qysds.
*个人所得税
        wa_tab-grsds2 = wa_tab-yysr * wa_tab-grsds.
*水利建设基金征收
        wa_tab-sljsjjzs2 = wa_tab-yysr * wa_tab-sljsjjzs.
*价格调节基金
        wa_tab-jgtjjj2 = wa_tab-yysr * wa_tab-jgtjjj.
*资源税
        wa_tab-zys2 = wa_tab-yysr * wa_tab-zys.

        MODIFY it_tab FROM wa_tab INDEX sy-tabix.

        "MODIFY zfi016 FROM wa_tab.
        CLEAR wa_tab.
      ENDLOOP.
      MODIFY zfi016 FROM TABLE it_tab.
    WHEN '&REFRESH'.
      rs_selfield-refresh = 'X'.
      SET USER-COMMAND '&OPT'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "ALV_PF_STATUS
