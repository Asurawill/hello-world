*----------------------------------------------------------------------*
*  程序名称         : ZFI015
*  创建者           : 吴丽娟
*  创建日期         : 2015-08-17
*----------------------------------------------------------------------*
*  概要说明
* 项目结果分析报表-税金调整
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  变 更 记 录                                         *
*----------------------------------------------------------------------*
* 日期       修改者         传输请求号     修改内容及原因
*----------  ------------   ------------   ----------------------------*
*2015-08-17  HANDYWLJ       ED1K902480    创建
*20160607 优化 IT02(魏云)
*----------------------------------------------------------------------*
REPORT zfi015.
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
*                  T Y P E S - 输 出 结 构 定 义                       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tab,
         psphi    TYPE prps-psphi, "项目
         post1    TYPE prps-post1, "项目描述
         kh(20),"客户
         khms(50),"客户描述
         qj       TYPE n LENGTH 10, "期间
         zjhcb    TYPE coepb-wogbtr, "总计划成本
         zjhsr    TYPE coepb-wogbtr, "总计划收入
         zjhys    TYPE p LENGTH 16 DECIMALS 2, "总计划应收
         zjhxxse  TYPE p LENGTH 16 DECIMALS 2, "总计划销项税额
         zsjcb    TYPE coepb-wogbtr, "总实际成本
         zsjsr    TYPE coepb-wogbtr, "总实际收入
         zsjys    TYPE bseg-dmbtr, "总实际应收
         bqsjcb   TYPE coepb-wogbtr, "本期实际成本
         bqsjsr   TYPE coepb-wogbtr, "本期实际收入
         bqsjys   TYPE bseg-dmbtr, "本期实际应收
         bqsjxxs  TYPE p LENGTH 16 DECIMALS 2, "本期实际销项税
         bqyjsr   TYPE coepb-wogbtr, "本期应计收入
         bqyjys   TYPE p LENGTH 16 DECIMALS 2, "本期应计应收
         bqyjxxs  TYPE p LENGTH 16 DECIMALS 2, "本期应计销项税
         bqcbbfb  TYPE p LENGTH 16 DECIMALS 6, "本期实际成本百分比
         bqjsce   TYPE coepb-wogbtr, "本期结算收入差额（调整）
         tzys     TYPE p LENGTH 16 DECIMALS 2, "调整-应收
         tzs      TYPE p LENGTH 16 DECIMALS 2, "调整-税
       END OF ty_tab.
TYPES:BEGIN OF ty_tab2,
        psphi TYPE prps-psphi,
        objnr TYPE prps-objnr,
        pspnr TYPE prps-pspnr,
        posid TYPE prps-posid,
        post1 TYPE prps-post1,
      END OF ty_tab2.
TYPES:BEGIN OF ty_tab3,
        clrv TYPE coepb-wogbtr,
        poci TYPE coepb-wogbtr,
        pocs TYPE coepb-wogbtr,
      END OF ty_tab3.
TYPES:BEGIN OF ty_zkunnr,
        pspnr TYPE proj-pspnr,
        zkhbm TYPE proj-zkhbm,
        name1 TYPE kna1-name1,
      END OF ty_zkunnr.
*types:begin of ty_name1,
*        name1 type kna1-name1,
*       kunnr TYPE kna1-kunnr,
*      end of ty_name1.
*----------------------------------------------------------------------*
*  DATA                                                                    *
*----------------------------------------------------------------------*
DATA:it_tab TYPE TABLE OF ty_tab,
     wa_tab TYPE ty_tab.

DATA:it_prps TYPE TABLE OF ty_tab2 WITH HEADER LINE.

DATA:it_prps1 TYPE TABLE OF ty_tab2 WITH HEADER LINE.

DATA:it_coepb TYPE TABLE OF coepb,
     wa_coepb TYPE coepb.

DATA:it_bseg TYPE TABLE OF bseg,
     wa_bseg TYPE bseg.

DATA:it_z0001 TYPE TABLE OF z0001,
     wa_z0001 TYPE z0001.

DATA:it_z0002 TYPE TABLE OF z0001,
     wa_z0002 TYPE z0001.

DATA:it_zkunnr TYPE TABLE OF ty_zkunnr,
     wa_zkunnr TYPE ty_zkunnr.

*data:it_name1 type table of ty_name1,
*     wa_name1 type ty_name1.

CONSTANTS a(10) TYPE p DECIMALS 2 VALUE '1.17'.
CONSTANTS b(10) TYPE p DECIMALS 2 VALUE '0.17'.

DATA:g_psphi(2).

DATA:it_caclu TYPE TABLE OF ty_tab3,
     wa_caclu TYPE ty_tab3.
*----------------------------------------------------------------------*
*                  ALV定义
*----------------------------------------------------------------------*
DATA:it_fieldcat TYPE lvc_t_fcat,
     wa_fieldcat LIKE LINE OF it_fieldcat,

     it_layout   TYPE TABLE OF lvc_s_layo,
     wa_layout   TYPE lvc_s_layo.
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

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_bseg
  FROM bseg
  FOR ALL ENTRIES IN it_prps
  WHERE projk = it_prps-pspnr
  AND bseg~gjahr <= p_years
  AND bseg~hkont BETWEEN '1121010101' AND '1122990101'
  AND bseg~bukrs = p_bukrs.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_z0002
  FROM z0001.

  DELETE z0001 FROM TABLE it_z0002.

  IF it_bseg IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_z0001
      FROM bkpf
      JOIN bseg ON bkpf~belnr = bseg~belnr
      FOR ALL ENTRIES IN it_bseg
      WHERE bkpf~belnr = it_bseg-belnr
      AND  bkpf~gjahr = it_bseg-gjahr
      AND bkpf~bukrs = p_bukrs
      AND bseg~hkont BETWEEN '1121010101' AND '1122990101'.

    LOOP AT it_z0001 INTO wa_z0001.
      READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_z0001-belnr.
      IF sy-subrc = 0.
        wa_z0001-hkont = wa_bseg-hkont.
        wa_z0001-dmbtr = wa_bseg-dmbtr.
        wa_z0001-projk = wa_bseg-projk.
        wa_z0001-shkzg = wa_bseg-shkzg.
      ENDIF.
      MODIFY it_z0001 FROM wa_z0001.
    ENDLOOP.
  ENDIF.

  MODIFY z0001 FROM TABLE it_z0001.

  SELECT proj~pspnr proj~zkhbm kna1~name1
    FROM proj
    JOIN kna1
    ON proj~zkhbm = kna1~kunnr
    INTO CORRESPONDING FIELDS OF TABLE it_zkunnr
    WHERE proj~pspid IN s_xmbh.

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
  LOOP AT it_prps .

*项目
*    wa_tab-psphi = it_prps-psphi.

*项目描述
    wa_tab-post1 = it_prps-post1.
*期间
    wa_tab-qj = p_period.
*总计划成本
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '82'.
      wa_tab-zjhcb = wa_tab-zjhcb + wa_coepb-wogbtr.
    ENDLOOP.
*总计划收入
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '81'.
      wa_tab-zjhsr = wa_tab-zjhsr + wa_coepb-wogbtr.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input  = it_prps-psphi
      IMPORTING
        output = wa_tab-psphi
*       PROJWA =
* EXCEPTIONS
*       NOT_FOUND       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    wa_tab-psphi = it_prps-psphi.

*总计划应收、总计划销项税额
    g_psphi = wa_tab-psphi(2).
    IF g_psphi = 22 OR g_psphi = 23.
      wa_tab-zjhys   = 0 - ( wa_tab-zjhsr  * a ).
      wa_tab-zjhxxse = 0 - ( wa_tab-zjhsr  * b ).
    ELSE.
      wa_tab-zjhys   = 0 - ( wa_tab-zjhsr * 1 ).
      wa_tab-zjhxxse = 0 - ( wa_tab-zjhsr * 0 ).
    ENDIF.
*总实际成本
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '63'.
      wa_tab-zsjcb = wa_tab-zsjcb + wa_coepb-wogbtr.
    ENDLOOP.
*总实际收入
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '67'.
      wa_caclu-clrv = wa_caclu-clrv + wa_coepb-wogbtr.
    ENDLOOP.

    wa_caclu-poci = 0.
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '68'.
      wa_caclu-poci = wa_caclu-poci + wa_coepb-wogbtr.
    ENDLOOP.

    wa_caclu-pocs = 0.
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '69'.
      wa_caclu-pocs = wa_caclu-pocs + wa_coepb-wogbtr.
    ENDLOOP.
    wa_tab-zsjsr = wa_caclu-clrv - ( wa_caclu-poci + wa_caclu-pocs ).
    CLEAR wa_caclu.
*总实际应收
    LOOP AT it_z0001 INTO wa_z0001
        WHERE projk = it_prps-pspnr
        AND   gjahr <= p_years
        AND   monat <= p_period.
      IF wa_z0001-shkzg = 'S'.
        wa_z0001-dmbtr = wa_z0001-dmbtr.
      ELSEIF wa_z0001-shkzg = 'H'.
        wa_z0001-dmbtr = - wa_z0001-dmbtr.
      ENDIF.
      wa_tab-zsjys = wa_tab-zsjys + wa_z0001-dmbtr.
    ENDLOOP.
*本期实际成本
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '63'
    AND   perio = p_period
    AND   gjahr = p_years.
      wa_tab-bqsjcb = wa_tab-bqsjcb + wa_coepb-wogbtr.
    ENDLOOP.
*本期实际应收
    LOOP AT it_z0001 INTO wa_z0001
        WHERE projk = it_prps-pspnr
        AND   gjahr = p_years
        AND   monat = p_period.
      IF wa_z0001-shkzg = 'S'.
        wa_z0001-dmbtr = wa_z0001-dmbtr.
      ELSEIF wa_z0001-shkzg = 'H'.
        wa_z0001-dmbtr = - wa_z0001-dmbtr.
      ENDIF.
      wa_tab-bqsjys = wa_tab-bqsjys + wa_z0001-dmbtr.
    ENDLOOP.
*本期应计收入
    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '67'
    AND   perio = p_period
    AND   gjahr = p_years.
      wa_tab-bqyjsr = wa_tab-bqyjsr + wa_coepb-wogbtr.
    ENDLOOP.
**本期应计应收、本期应计销项税
*    g_psphi = wa_tab-psphi(2).
*    IF g_psphi = 22 OR g_psphi = 23.
*      wa_tab-bqyjys  = 0 - ( wa_tab-bqyjsr * a ).
*      wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * b ).
*    ELSE.
*      wa_tab-bqyjys  = 0 - ( wa_tab-bqyjsr * 1 ).
*      wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * 0 ).
*    ENDIF.
*本期实际成本百分比
    IF  wa_tab-zjhcb NE 0 .
      wa_tab-bqcbbfb = ( wa_tab-bqsjcb ) / ( wa_tab-zjhcb ).
    ENDIF.
*本期结算收入差额（调整-收入）

    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '68'
    AND   perio = p_period
    AND   gjahr = p_years.
      wa_caclu-poci = wa_caclu-poci + wa_coepb-wogbtr.
      CLEAR wa_coepb.
    ENDLOOP.

    LOOP AT it_coepb INTO wa_coepb
    WHERE objnr = it_prps-objnr
    AND   abkat = '69'
    AND   perio = p_period
    AND   gjahr = p_years.
      wa_caclu-pocs = wa_caclu-pocs + wa_coepb-wogbtr.
      CLEAR wa_coepb.
    ENDLOOP.

    wa_tab-bqjsce = wa_caclu-poci + wa_caclu-pocs.
    CLEAR wa_caclu.
*本期实际收入
    wa_tab-bqsjsr = wa_tab-bqyjsr - wa_tab-bqjsce.
*本期实际销项税
    wa_tab-bqsjxxs = wa_tab-bqsjys + wa_tab-bqsjsr.
**调整-应收
**    wa_tab-tzys = wa_tab-bqjsce * a.
*    g_psphi = wa_tab-psphi(2).
*    IF g_psphi = 22 OR g_psphi = 23.
*      wa_tab-tzys = 0 - ( wa_tab-bqjsce * a ).
*      wa_tab-tzs = wa_tab-bqjsce * b.
*    ELSE.
*      wa_tab-tzys = 0 - ( wa_tab-bqjsce * 0 ).
*      wa_tab-tzs = wa_tab-bqjsce * 0.
*    ENDIF.
*调整-税
*    wa_tab-tzs = wa_tab-bqjsce * b.
*客户 客户描述
    READ TABLE it_zkunnr INTO wa_zkunnr WITH KEY pspnr = it_prps-psphi.
    IF sy-subrc = 0.
      wa_tab-kh = wa_zkunnr-zkhbm.
      wa_tab-khms = wa_zkunnr-name1.
    ENDIF.
    CLEAR wa_zkunnr.

    wa_tab-psphi = it_prps-psphi.
    "优化 IT02 20160607 begin
    "优化本期应计应收 、本期应计销项税、调整-应收、调整-税


    CASE g_psphi.
      WHEN '21'.
        wa_tab-bqyjys = 0 - ( wa_tab-bqyjsr * 103  / 1000 ).
        wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * 103  / 1000  ).
        wa_tab-tzys = wa_tab-bqjsce * 103 / 1000.
        wa_tab-tzs = wa_tab-bqjsce * 3  / 100 .

      WHEN '22'.
        wa_tab-bqyjys = 0 - ( wa_tab-bqyjsr * 117  / 1000 ).
        wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * 117  / 1000  ).
        wa_tab-tzys = wa_tab-bqjsce * 117 / 1000.
        wa_tab-tzs = wa_tab-bqjsce * 17  / 100 .

      WHEN '23'.
        wa_tab-bqyjys = 0 - ( wa_tab-bqyjsr * 117  / 1000 ).
        wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * 117  / 1000  ).
        wa_tab-tzys = wa_tab-bqjsce * 117 / 1000.
        wa_tab-tzs = wa_tab-bqjsce * 17  / 100 .

      WHEN '25'.
        wa_tab-bqyjys = 0 - ( wa_tab-bqyjsr * 111  / 1000 ).
        wa_tab-bqyjxxs = 0 - ( wa_tab-bqyjsr * 111  / 1000  ).
        wa_tab-tzys = wa_tab-bqjsce * 111 / 1000.
        wa_tab-tzs = wa_tab-bqjsce * 11  / 100 .

      WHEN OTHERS.
        wa_tab-bqyjys = 0 - wa_tab-bqyjsr .
        wa_tab-bqyjxxs = 0 -  wa_tab-bqyjsr .

    ENDCASE.
    "优化 IT02 20160607 end.
    APPEND wa_tab TO it_tab.
    CLEAR wa_tab.
  ENDLOOP.

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
  init_fieldcat 'PSPHI' '项目' 'PRPS' 'PSPHI'.
  init_fieldcat 'POST1' '项目描述' 'PRPS' 'POST1'.
  init_fieldcat 'KH' '客户' '' ''.
  init_fieldcat 'KHMS' '客户描述' '' ''.
  init_fieldcat 'QJ' '期间' '' ''.
  init_fieldcat 'ZJHCB' '总计划成本' 'COEPB' 'WOGBTR'.
  init_fieldcat 'ZJHSR' '总计划收入' 'COEPB' 'WOGBTR'.
*  init_fieldcat 'ZJHYS' '总计划应收' '' ''.
*  init_fieldcat 'ZJHXXSE' '总计划销项税额' '' ''.
  init_fieldcat 'ZSJCB' '总实际成本' 'COEPB' 'WOGBTR'.
  init_fieldcat 'ZSJSR' '总实际收入' 'COEPB' 'WOGBTR'.
  init_fieldcat 'ZSJYS' '总实际应收' 'BSEG' 'DMBTR'.
  init_fieldcat 'BQSJCB' '本期实际成本' 'COEPB' 'WOGBTR'.
  init_fieldcat 'BQSJSR' '本期实际收入' 'COEPB' 'WOGBTR'.
  init_fieldcat 'BQSJYS' '本期实际应收' 'BSEG' 'DMBTR'.
  init_fieldcat 'BQSJXXS' '本期实际销项税' '' ''.
  init_fieldcat 'BQYJSR' '本期应计收入' 'COEPB' 'WOGBTR'.
  init_fieldcat 'BQYJYS' '本期应计应收' '' ''.
  init_fieldcat 'BQYJXXS' '本期应计销项税' '' ''.
  init_fieldcat 'BQCBBFB' '本期成本百分比' '' ''.
  init_fieldcat 'BQJSCE' '本期结算收入差额（调整-收入）' 'COEPB' 'WOGBTR'.
  init_fieldcat 'TZYS' '调整-应收' '' ''.
  init_fieldcat 'TZS' '调整-税' '' ''.
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
