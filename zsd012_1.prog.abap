*&---------------------------------------------------------------------*
*& 程序名称:ZSD012
*& 作者    :IT02
*& 开发日期:20161228
*& 请求号  :ED1K905131
*& 描述    :订单交货明细表导出：数据逻辑FROMZSD012
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

REPORT zsd012_1.


************************************************************************
* Includes
************************************************************************

************************************************************************
* Tables
************************************************************************
TABLES: vbak, vbap, vbkd, vbpa,lips,likp.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_output.
        INCLUDE STRUCTURE zclv_zsd012.
TYPES:wjhsl      TYPE vbap-kwmeng,
      pjnam(200),
      ptmat      TYPE vbap-matnr,
      ptktx      TYPE vbap-arktx,
      ltext(200),
      vbeln_fp   TYPE vbrp-vbeln,
      bezei      TYPE tvagt-bezei, "拒绝原因描述
      kbetr      TYPE konv-kbetr, "单价
      jetj       TYPE faglflext-tsl01, "金额
      vdatu      TYPE c LENGTH 8, "请求交货日期
      END OF ty_output.

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.

************************************************************************
* Internal Table
************************************************************************
DATA: it_zclv_zsd012 TYPE TABLE OF zclv_zsd012,
      wa_zclv_zsd012 TYPE zclv_zsd012,
      wa_zclv        TYPE zclv_zsd012.

DATA:it_compar LIKE TABLE OF zclv_zsd012 WITH HEADER LINE.
DATA:it_konv LIKE TABLE OF konv WITH HEADER LINE.
DATA:it_vbkd LIKE TABLE OF vbkd WITH HEADER LINE.

DATA: it_vbpa    TYPE TABLE OF vbpa,
      wa_vbpa    TYPE vbpa,
      it_vbrp    TYPE TABLE OF vbrp,
      wa_vbrp    TYPE vbrp,
      it_vbrk    TYPE TABLE OF vbrk,
      wa_vbrk    TYPE vbrk,
      it_vbrk_cx TYPE TABLE OF vbrk,
      wa_vbrk_cx TYPE vbrk,
      it_vbak    TYPE TABLE OF vbak,
      wa_vbak    TYPE vbak.

DATA: it_output TYPE TABLE OF ty_output,
      wa_output TYPE ty_output.

FIELD-SYMBOLS: <fs_zclv_zsd012> TYPE zclv_zsd012.

DATA: it_tvko TYPE TABLE OF tvko,
      wa_tvko TYPE tvko.

DATA: it_tvagt LIKE TABLE OF tvagt WITH HEADER LINE .
************************************************************************
* WorkArea
************************************************************************




************************************************************************
* Global Variant
************************************************************************
*field-symbols: <dyn_table> type standard table,        " 内表结构
*               <dyn_wa>,                               " 表头
*               <dyn_field>.
*
*data: dy_table type ref to data,
*      dy_line  type ref to data.

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

************************************************************************
* Constant
************************************************************************


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_vbeln FOR vbak-vbeln MATCHCODE OBJECT vmva,
                s_auart FOR vbak-auart,
                s_kunnr FOR vbak-kunnr,
                s_erdat FOR vbak-erdat,
                s_matnr FOR vbap-matnr,
                s_bstkd FOR vbkd-bstkd,
                s_ernam FOR vbak-ernam,
                s_jhpz  FOR lips-vbeln,"交货凭证 add IT02 150730
                s_budat FOR likp-wadat_ist."过账日期 add it02 150730
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_vkorg FOR vbak-vkorg OBLIGATORY MEMORY ID vko,
                s_vkbur FOR vbak-vkbur.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_kunnr1 FOR vbpa-kunnr,
                s_kunnr2 FOR vbpa-kunnr.
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE text-004.
PARAMETER: p_allor TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
           p_uncle TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK blk4.


************************************************************************
* Initialization
************************************************************************
INITIALIZATION.


************************************************************************
* At selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN.



************************************************************************
* Event Start of Selection
************************************************************************
START-OF-SELECTION.
  PERFORM frm_auth_check.
  PERFORM frm_get_data. "处理数逻辑
  PERFORM frm_zh_xml.   "转换XML格式
  PERFORM frm_cc_sapfwq."生成SAP服务器文件
************************************************************************
* Event End-of selection
************************************************************************
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
  SELECT *
    INTO TABLE it_zclv_zsd012
    FROM zclv_zsd012
    WHERE vbeln IN s_vbeln
      AND auart IN s_auart
      AND kunnr IN s_kunnr
      AND erdat IN s_erdat
      AND ernam IN s_ernam
      AND matnr IN s_matnr
      AND bstkd IN s_bstkd
      AND vkorg IN s_vkorg
      AND vkbur IN s_vkbur
      AND vbeln_lp IN s_jhpz
      AND wadat_ist IN s_budat
*      and pstyv not in ('Z01', 'Z02', 'Z21', 'Z22', 'Z31', 'Z32', 'Z41', 'Z42')
    .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_tvagt
    FROM  tvagt WHERE spras = sy-langu.  .
  IF it_zclv_zsd012 IS INITIAL.
    MESSAGE '无相关数据！' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "add 销售订单抬头信息 IT02 20160329
  SELECT *
    INTO TABLE it_vbak
    FROM vbak
    WHERE vbeln IN s_vbeln
      AND auart IN s_auart
      AND kunnr IN s_kunnr
      AND erdat IN s_erdat
      AND ernam IN s_ernam
      AND vkorg IN s_vkorg
      AND vkbur IN s_vkbur
      .
  SORT it_vbak BY vbeln.

  " add konv 单价  it02 150730  begin
  SELECT vbeln posnr bstkd
    FROM vbkd
    INTO CORRESPONDING FIELDS OF TABLE it_vbkd
    FOR ALL ENTRIES IN it_zclv_zsd012
    WHERE vbeln = it_zclv_zsd012-vbeln
   .
  SELECT knumv kposn kbetr
    FROM konv
    INTO CORRESPONDING FIELDS OF TABLE it_konv
    FOR ALL ENTRIES IN it_zclv_zsd012
    WHERE knumv = it_zclv_zsd012-knumv
    AND kposn = it_zclv_zsd012-posnr
    AND kschl = 'ZR01'.




                                                            "end 150730

  IF s_kunnr1 IS NOT INITIAL OR s_kunnr2 IS NOT INITIAL.
    SELECT *
      INTO TABLE it_vbpa
      FROM vbpa
      FOR ALL ENTRIES IN it_zclv_zsd012
      WHERE vbeln = it_zclv_zsd012-vbeln.

    LOOP AT it_zclv_zsd012 INTO wa_zclv_zsd012.
      " 判断 项目经理
      LOOP AT it_vbpa INTO wa_vbpa WHERE vbeln = wa_zclv_zsd012-vbeln
                                     AND ( parvw = 'Z1' OR parvw = 'Z2' )
                                     AND kunnr IN s_kunnr1.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        DELETE it_zclv_zsd012.
        CONTINUE.
      ENDIF.

      " 判断 经办人
      LOOP AT it_vbpa INTO wa_vbpa WHERE vbeln = wa_zclv_zsd012-vbeln
                                     AND ( parvw = 'Z3' OR parvw = 'Z4' OR parvw = 'Z5' )
                                     AND kunnr IN s_kunnr2.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        DELETE it_zclv_zsd012.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT *
    INTO TABLE it_vbrp
    FROM vbrp
    FOR ALL ENTRIES IN it_zclv_zsd012
    WHERE vgbel = it_zclv_zsd012-vbeln_lp.

  SELECT *
    APPENDING TABLE it_vbrp
    FROM vbrp
    FOR ALL ENTRIES IN it_zclv_zsd012
    WHERE vgbel = it_zclv_zsd012-vbeln.

  IF it_vbrp IS NOT INITIAL.
    " 排除冲销发票
    SELECT *
      INTO TABLE it_vbrk
      FROM vbrk
      FOR ALL ENTRIES IN it_vbrp
      WHERE vbeln = it_vbrp-vbeln.

    SELECT *
      INTO TABLE it_vbrk_cx
      FROM vbrk
      FOR ALL ENTRIES IN it_vbrk
      WHERE sfakn = it_vbrk-vbeln.
    SORT it_vbrk_cx BY sfakn.

    LOOP AT it_vbrk INTO wa_vbrk.
      IF wa_vbrk-sfakn IS NOT INITIAL.
        LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.
          DELETE it_vbrp.
        ENDLOOP.
      ELSE.
        READ TABLE it_vbrk_cx INTO wa_vbrk_cx WITH KEY sfakn = wa_vbrk-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT it_vbrp INTO wa_vbrp WHERE vbeln = wa_vbrk-vbeln.
            DELETE it_vbrp.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_vbrp BY vgbel vgpos.

  SORT it_zclv_zsd012 BY vbeln posnr.

  DATA: l_count TYPE i.

  DATA: lt_zclv LIKE it_zclv_zsd012.

  lt_zclv = it_zclv_zsd012.
  SORT lt_zclv BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM lt_zclv COMPARING vbeln posnr.

  LOOP AT lt_zclv INTO wa_zclv.
    l_count = 0.

    LOOP AT it_zclv_zsd012 INTO wa_zclv_zsd012 WHERE vbeln = wa_zclv-vbeln
                                                 AND posnr = wa_zclv-posnr.
*      if wa_zclv_zsd012-lfimg = 0.
*        continue.
*      endif.
      wa_zclv-kwmeng = wa_zclv-kwmeng - wa_zclv_zsd012-lfimg.
      l_count = l_count + 1.
    ENDLOOP.

    IF l_count > 1.
      CLEAR: wa_zclv-vbeln_lp,
             wa_zclv-posnr_lp,
             wa_zclv-lfimg,
*             wa_zclv-pstyv,
             wa_zclv-meins.
      MODIFY lt_zclv FROM wa_zclv.
    ELSE.
      DELETE lt_zclv.
    ENDIF.
  ENDLOOP.

  SORT lt_zclv BY vbeln posnr.

  APPEND LINES OF lt_zclv TO it_zclv_zsd012.

  SORT it_zclv_zsd012 BY vbeln posnr.

  LOOP AT it_zclv_zsd012 INTO wa_zclv_zsd012.
    CLEAR wa_output.

    READ TABLE it_vbkd WITH KEY vbeln = wa_zclv_zsd012-vbeln posnr = wa_zclv_zsd012-posnr.
    IF sy-subrc = 0 .
      wa_zclv_zsd012-bstkd = it_vbkd-bstkd.  "先根据销售订单号、行号读取合同编号150731
    ELSE.
      READ TABLE it_vbkd WITH KEY vbeln = wa_zclv_zsd012-vbeln  posnr = '000000'. "根据销售订单号、行号读取失败 ，再改为读取行号为000000的合同编号150731
      IF sy-subrc = 0 .
        wa_zclv_zsd012-bstkd = it_vbkd-bstkd.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING wa_zclv_zsd012 TO wa_output.
    "请求交货日期
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_output-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      wa_output-vdatu = wa_vbak-vdatu.        "add it02 20160329
    ENDIF.
    READ TABLE it_konv WITH KEY knumv = wa_zclv_zsd012-knumv kposn = wa_zclv_zsd012-posnr.
    IF sy-subrc = 0.
      wa_output-kbetr = it_konv-kbetr ."读取单价 it02 150731
    ENDIF.

    IF wa_output-pstyv = 'Z01'
      OR wa_output-pstyv = 'Z02'
      OR wa_output-pstyv = 'Z21'
      OR wa_output-pstyv = 'Z22'
      OR wa_output-pstyv = 'Z31'
      OR wa_output-pstyv = 'Z32'
      OR wa_output-pstyv = 'Z41'
      OR wa_output-pstyv = 'Z42'.
      CONTINUE.
    ENDIF.

    " 未交货数量 = 销售订单行数量 - 交货订单数量
*    wa_output-wjhsl = wa_output-kwmeng.
*
*    l_count = 0.
*    loop at it_zclv_zsd012 into wa_zclv where vbeln = wa_zclv_zsd012-vbeln
*                                          and posnr = wa_zclv_zsd012-posnr.
*      wa_output-wjhsl = wa_output-wjhsl - wa_zclv-lfimg.
*    endloop.
    IF wa_zclv_zsd012-vbeln_lp IS NOT INITIAL.
      READ TABLE lt_zclv INTO wa_zclv WITH KEY vbeln = wa_zclv_zsd012-vbeln
                                               posnr = wa_zclv_zsd012-posnr
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        IF wa_output-lfimg = 0.
          CONTINUE.
        ENDIF.
        wa_output-kwmeng = wa_output-lfimg.
        wa_output-wjhsl = 0.
      ELSE.
        wa_output-wjhsl = wa_output-kwmeng - wa_output-lfimg.
      ENDIF.
    ELSE.
      IF wa_output-kwmeng = 0.
        CONTINUE.
      ENDIF.
      wa_output-wjhsl = wa_output-kwmeng.
    ENDIF.

    IF p_uncle = 'X'. " 只显示未清
      IF wa_output-wjhsl = 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF wa_output-auart = 'ZPO' OR wa_output-auart = 'ZWV' OR wa_output-auart = 'ZF1' OR wa_output-auart = 'ZZG'.
      IF wa_output-uepos = ''.
        CONTINUE.
      ELSE.
        " 取屏体物料及描述
        READ TABLE it_zclv_zsd012 INTO wa_zclv WITH KEY vbeln = wa_output-vbeln
                                                        posnr = wa_output-uepos
                                                        BINARY SEARCH.
        IF sy-subrc = 0.
          wa_output-ptmat = wa_zclv-matnr.
          wa_output-ptktx = wa_zclv-arktx.
        ENDIF.
      ENDIF.

      READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_output-vbeln
                                               vgpos = wa_output-uepos
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-vbeln_fp = wa_vbrp-vbeln.
      ENDIF.
    ELSE.
      READ TABLE it_vbrp INTO wa_vbrp WITH KEY vgbel = wa_output-vbeln_lp
                                               vgpos = wa_output-posnr_lp
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-vbeln_fp = wa_vbrp-vbeln.
      ENDIF.
    ENDIF.

    IF wa_output-bstdk = '00000000'.
      wa_output-bstdk = ''.
    ENDIF.

    IF wa_output-wadat_ist = '00000000'.
      wa_output-wadat_ist = ''.
    ENDIF.

    " 取项目名称 - 销售订单抬头文本
    g_objname = wa_output-vbeln.
    REFRESH it_lines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'Z001'
        language                = sy-langu
        name                    = g_objname
        object                  = 'VBBK'
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
    IF sy-subrc <> 0.
* Implement suitable error handling here
*      message '项目名称读取错误!' type 'W'.
    ENDIF.

    READ TABLE it_lines INTO wa_lines INDEX 1.
    IF sy-subrc = 0.
      wa_output-pjnam = wa_lines-tdline.
    ENDIF.

    " 取备注 - 行项目文本
    g_objname(10) = wa_output-vbeln.
    g_objname+10 = wa_output-posnr.
    REFRESH it_lines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0006'
        language                = sy-langu
        name                    = g_objname
        object                  = 'VBBP'
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
    IF sy-subrc <> 0.
* Implement suitable error handling here
*      message '项目名称读取错误!' type 'W'.
    ENDIF.

    LOOP AT it_lines INTO wa_lines.
      CONCATENATE wa_output-ltext wa_lines-tdline INTO wa_output-ltext.
    ENDLOOP.
    "IT02 add 增加拒绝原因描述 150710 begin
    READ TABLE it_tvagt WITH KEY abgru = wa_output-abgru .
    IF sy-subrc = 0.
      wa_output-bezei = it_tvagt-bezei."拒绝原因描述
    ENDIF.
    "IT02 add 增加拒绝原因描述 150710 end
    "金额添加 it02 150731 begin
    wa_output-jetj = wa_output-kbetr * wa_output-kwmeng .
    "金额添加 it02 150731 end
    APPEND wa_output TO it_output.
  ENDLOOP.

  SORT it_output BY vbeln posnr vbeln_lp posnr_lp.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_auth_check .
  SELECT *
    INTO TABLE it_tvko
    FROM tvko
    WHERE vkorg IN s_vkorg.

  LOOP AT it_tvko INTO wa_tvko.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO' ID 'ACTVT' FIELD '03'
                                        ID 'SPART' DUMMY
                                        ID 'VKORG' FIELD wa_tvko-vkorg
                                        ID 'VTWEG' DUMMY.
    IF sy-subrc NE 0.
      MESSAGE i001(zsd01) WITH wa_tvko-vkorg DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ZH_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_zh_xml .

    CALL TRANSFORMATION zsd012_1_zh
       SOURCE table = it_output
       RESULT XML xmlstr.
    REPLACE FIRST OCCURRENCE OF 'encoding="utf-16"' IN xmlstr WITH 'encoding="gbk"'.
    APPEND xmlstr TO xml_table.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CC_SAPFWQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_cc_sapfwq .

    dname = 'ZSD012订单交货明细表.XLS'.
    CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
    DELETE DATASET dname .
    OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
    IF sy-subrc NE 0. EXIT. ENDIF.
    LOOP AT xml_table INTO wa_xml.
      TRANSFER wa_xml TO dname.
    ENDLOOP.
    CLOSE DATASET dname.

ENDFORM.
