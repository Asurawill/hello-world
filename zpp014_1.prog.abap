REPORT zpp014_1.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160831
*& Request       : ED1K905127
*& Descriptions  : 生产订单报工信息报表查询
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*&
*& 2017-06-02 it02&魏云           ED1K905628   追加未报工的生产订单信息
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:aufk,afpo,afko,mara ,afru,afvv,crhd,crco,resb,afvc.
************************************************************************
* Type Declaration
************************************************************************

TYPES:BEGIN OF ty_data,
        werks     TYPE aufk-werks,  "工厂
        aufnr     TYPE afru-aufnr,  "生产订单
        auart     TYPE aufk-auart,  "订单类型
        matnr     TYPE afpo-matnr,   "物料号
        ktext     TYPE aufk-ktext,   "长文本
        maktx     TYPE makt-maktx,   "物料描述
        kdauf     TYPE afpo-kdauf,  "销售订单
        kdpos     TYPE afpo-kdpos,  "销售订单行号
        xmmc      TYPE string,      "项目名称
        vornr     TYPE afvc-vornr,   "工序编码
        ltxa1     TYPE afvc-ltxa1,    "工序描述
        psmng     TYPE afpo-psmng,    "订单数量
        amein     TYPE afpo-amein,    "订单单位
        gmnga     TYPE afvv-gmnga,   "累计完成数量
        wwsl      TYPE afvv-gmnga,  "未完成数量
        bqwcsl    TYPE afru-gmnga,  "本期完成数量
        flts      TYPE i,
        rksl      TYPE afpo-wemng,   "入库数量
        ddgb      TYPE c LENGTH 10 ,  "订单关闭
        ism01     TYPE afvv-ism01,  "总人工
        bqrg      TYPE afvv-ism01,  "本期人工
        bzzrg     TYPE afvv-ism01,  "标准总人工
        ism02     TYPE afvv-ism01,   "总机器
        bqjq      TYPE afvv-ism02,    "本期机器
        bzzjq     TYPE afvv-ism02,   "标准总机器
        ism03     TYPE afvv-ism03,   "总耗电
        bqhd      TYPE afvv-ism03,   "本期耗电
        bzhd      TYPE afvv-ism03,    "标准耗电
        ism04     TYPE afvv-ism04,  "总制造工时
        bqzzgs    TYPE afvv-ism04,   "本期制造工时
        bzzzgs    TYPE afvv-ism04,   "标准制造工时
        ism05     TYPE afvv-ism05,   "累计加班
        bqjb      TYPE afvv-ism05,    "本期加班
        ism06     TYPE afvv-ism06,   "人数
        arbpl     TYPE crhd-arbpl,    "工作中心
        arbpl_txt TYPE crtx-ktext,  "工作中心描述
        kostl     TYPE crco-kostl,   "成本中心
        kostl_txt TYPE cskt-ltext,  "成本中心描述
        steus     TYPE afvc-steus,   "工序控制码
        ieavd     TYPE afvv-ieavd,   "实际开始日期
        isdd      TYPE afvv-isdd,    "实际完成日期。

        zsel,
      END OF ty_data.

TYPES:BEGIN OF ty_aufnr,
        werks TYPE werks_d,  "工厂
        aufnr TYPE aufnr,    "生产订单
        aufpl TYPE afko-aufpl,  "计划号码
        rsnum TYPE rsnum,   "预留号
        objnr TYPE aufk-objnr,  "对象号
        auart TYPE aufk-auart, "订单类型
        ktext TYPE aufk-ktext,   "长文本
        matnr TYPE afpo-matnr,  "物料号号
        psmng TYPE afpo-psmng,   "订单数量
        amein TYPE afpo-amein,   "订单单位
        wemng TYPE afpo-wemng,  "入库数量
        kdauf TYPE afpo-kdauf,  "销售订单
        kdpos TYPE afpo-kdpos,  "销售订单行号
        xmbh  TYPE afpo-kdauf,  "项目编号
      END OF ty_aufnr .


TYPES:BEGIN OF ty_kdauf,
        kdauf TYPE kdauf,
        xmmc  TYPE string,
      END OF ty_kdauf .

TYPES:BEGIN OF ty_bqz,
        rueck  TYPE afru-rueck,  "确认码
        bqwcsl TYPE afru-gmnga,  "本期完成数量
        bqrg   TYPE afvv-ism01,  "本期人工
        bqjq   TYPE afvv-ism02,    "本期机器
        bqhd   TYPE afvv-ism03,   "本期耗电
        bqzzgs TYPE afvv-ism04,   "本期制造工时
        bqjb   TYPE afvv-ism05,    "本期加班
      END OF ty_bqz .

TYPES:BEGIN OF ty_resb,
        rsnum TYPE resb-rsnum,  "预留号
        rspos TYPE resb-rspos,
        rsart TYPE resb-rsart,
        xloek TYPE resb-xloek,  "删除标识
        kzear TYPE resb-kzear,  "最后发货
        bdmng TYPE resb-bdmng,  "需求数量
        enmng TYPE resb-enmng,  "提货数量
        ts    TYPE resb-enmng,   "套数
      END OF ty_resb .

TYPES:BEGIN OF ty_matnr,
        matnr TYPE mara-matnr,  "物料号
        werks TYPE marc-werks,   "工厂
        maktx TYPE makt-maktx,    "物料描述

      END OF ty_matnr .

DATA:gt_matnr TYPE TABLE OF ty_matnr,
     gs_matnr TYPE ty_matnr.

DATA:gt_resb TYPE TABLE OF ty_resb,
     gs_resb TYPE ty_resb.



DATA:gt_data TYPE TABLE OF ty_data,
     gs_data TYPE  ty_data.

DATA:gt_afru TYPE TABLE OF afru,
     gs_afru TYPE afru.


DATA:gt_afru_1 TYPE TABLE OF afru,
     gs_afru_1 TYPE afru.

DATA:gt_afvc TYPE TABLE OF afvc,
     gs_afvc TYPE afvc.

DATA:gt_afvv TYPE TABLE OF afvv,
     gs_afvv TYPE afvv.

DATA:gt_bqz TYPE TABLE OF ty_bqz,
     gs_bqz TYPE ty_bqz.



DATA:gt_kdauf TYPE TABLE OF ty_kdauf,
     gs_kdauf TYPE ty_kdauf.

DATA:gt_aufnr TYPE TABLE OF ty_aufnr,
     gs_aufnr TYPE ty_aufnr.

DATA:gt_aufnr_2 TYPE TABLE OF ty_aufnr,
     gs_aufnr_2 TYPE ty_aufnr.

DATA:gt_vbak TYPE TABLE OF vbak,
     gs_vbak TYPE vbak.


DATA:gt_jest TYPE TABLE OF jest,
     gs_jest TYPE jest.

DATA:gt_tj02t TYPE TABLE OF tj02t,
     gs_tj02t TYPE tj02t.

DATA:gt_crhd TYPE TABLE OF crhd,
     gs_crhd TYPE crhd.

DATA:gt_crco TYPE TABLE OF crco,
     gs_crco TYPE crco.

DATA:gt_crtx TYPE TABLE OF crtx,
     gs_crtx TYPE crtx.

DATA:gt_cskt TYPE TABLE OF cskt,
     gs_cskt TYPE cskt.

DATA:gt_afko TYPE TABLE OF afko,
     gs_afko TYPE afko.

DATA:gt_t001w type table of t001w,
     gs_t001w type t001w.

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.


DATA:m_werks_d TYPE werks_d ."内存传入工厂

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

DATA: xmlstr    TYPE string,
      xml_table TYPE STANDARD TABLE OF string,
      wa_xml    LIKE LINE OF xml_table.

DATA:dname(120) TYPE c.




DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-015 .
SELECT-OPTIONS:
                s_werks FOR afru-werks, "工厂
                s_aufnr FOR aufk-aufnr ,"OBLIGATORY ,
                s_matnr FOR afpo-matnr,
                s_erdat for aufk-erdat,  "生产订单创建日期
                s_arbpl FOR crhd-arbpl,
                s_kostl FOR crco-kostl,
                 s_budat FOR afru-budat.  "报工日期
SELECTION-SCREEN END OF BLOCK b1.

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
  PERFORM frm_auth_check.
  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_zh_xml.   "转换XML格式
  PERFORM frm_cc_sapfwq."生成SAP服务器文件

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
select * into table gt_t001w
  from t001w
  where werks in s_werks .
sort gt_t001w by werks .

loop at gt_t001w into gs_t001w.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  gs_t001w-werks.
  IF sy-subrc <> 0.
    MESSAGE e603(fco) WITH gs_t001w-werks.
  ENDIF.


endloop.

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
  " clear:M_WERKS_D.
  "  IMPORT M_WERKS_D FROM MEMORY ID 'ZWERKS'.
*
*  if M_WERKS_D is not initial .
*
*    s_werks-low = M_werks_D .
*  endif.


  SELECT a~aufnr
         a~auart a~objnr a~werks a~ktext
         b~aufpl b~rsnum
         c~matnr c~kdauf c~kdpos c~psmng c~wemng c~amein
   INTO CORRESPONDING FIELDS OF TABLE gt_aufnr_2
    FROM aufk AS a
    INNER JOIN afko AS b
    ON a~aufnr = b~aufnr
    INNER JOIN afpo AS c
    ON a~aufnr = c~aufnr
    WHERE a~aufnr in s_aufnr
    AND   a~werks IN s_werks
    AND   a~erdat in s_erdat
    AND   a~loekz EQ ''.
  SORT gt_aufnr_2 BY  aufpl .

  CHECK gt_aufnr_2 IS NOT INITIAL .

  MOVE-CORRESPONDING gt_aufnr_2 TO gt_aufnr .

  SORT gt_aufnr BY werks aufnr .

  DELETE ADJACENT DUPLICATES FROM gt_aufnr COMPARING werks aufnr .

  if gt_aufnr is not initial.
      SELECT * INTO TABLE gt_afru
    FROM afru
    FOR ALL ENTRIES IN gt_aufnr
    WHERE budat IN s_budat
    AND   werks eq gt_aufnr-werks
    AND   aufnr eq gt_aufnr-aufnr .

  SORT gt_afru BY  rueck rmzhl .

  SELECT * INTO TABLE gt_afru_1
   FROM afru
   FOR ALL ENTRIES IN gt_aufnr
   WHERE   werks eq gt_aufnr-werks
   AND  aufnr eq gt_aufnr-aufnr
   AND  stzhl EQ ''
   AND  stokz EQ ''
   AND  ism06  IS NOT NULL .


  SORT gt_afru_1 BY rueck rmzhl .

  DELETE ADJACENT DUPLICATES FROM gt_afru_1  COMPARING rueck  .


   endif.



  SELECT rsnum rspos rsart xloek kzear bdmng enmng INTO TABLE gt_resb
  FROM resb
  FOR ALL ENTRIES  IN gt_aufnr_2
  WHERE rsnum = gt_aufnr_2-rsnum .
  DELETE gt_resb WHERE xloek EQ 'X' OR kzear EQ'X' .


  SORT gt_resb BY rsnum .

  SELECT * INTO TABLE gt_jest
    FROM jest
    FOR ALL ENTRIES IN gt_aufnr_2
    WHERE objnr = gt_aufnr_2-objnr
    AND stat IN ('I0046','I0045')
    AND inact EQ ''.

  SORT gt_jest BY objnr .

  SELECT * INTO TABLE gt_tj02t
    FROM tj02t
    WHERE istat IN ('I0046','I0045')
    AND spras = '1'.
  SORT gt_tj02t BY istat .

  MOVE-CORRESPONDING gt_aufnr_2 TO gt_kdauf .
  DELETE gt_kdauf WHERE kdauf IS INITIAL .
  SORT gt_kdauf BY kdauf .


  DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf .

  SELECT * INTO TABLE gt_vbak FROM vbak .
  SORT gt_vbak BY vbeln .
  LOOP AT gt_aufnr_2 INTO gs_aufnr_2 WHERE  kdauf IS INITIAL.
    CLEAR:gs_kdauf.
    READ TABLE gt_kdauf INTO gs_kdauf WITH KEY  kdauf = gs_aufnr_2-aufnr+0(7)  BINARY SEARCH .
    IF sy-subrc EQ 0 .
      CONTINUE .
    ELSE.
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_aufnr_2-aufnr+0(7) BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_kdauf-kdauf = gs_vbak-vbeln .
        APPEND gs_kdauf TO gt_kdauf .
        SORT gt_kdauf BY kdauf .
      ELSE.
        CONTINUE .
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE gt_kdauf WHERE kdauf IS INITIAL .
  SORT gt_kdauf BY kdauf .


  SELECT * INTO TABLE gt_afvc
    FROM afvc
    FOR ALL ENTRIES IN gt_aufnr_2
    WHERE aufpl = gt_aufnr_2-aufpl
    AND   werks = gt_aufnr_2-werks.

  SORT gt_afvc BY aufpl  aplzl rueck .


  SELECT * INTO TABLE gt_afvv
    FROM afvv
    FOR ALL ENTRIES IN  gt_afvc
    WHERE aufpl = gt_afvc-aufpl
    AND   aplzl = gt_afvc-aplzl.
  " AND   werks = gt_afvc-werks.
  SORT gt_afvv BY aufpl aplzl .



  "读取物料描述

  SELECT a~matnr a~werks b~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_matnr
    FROM marc  AS a
    INNER JOIN makt AS b
    ON a~matnr = b~matnr
    WHERE a~werks IN s_werks
     AND b~spras = sy-langu.

  SORT gt_matnr BY matnr werks .

  "读取工作中心

  SELECT * INTO TABLE gt_crhd
    FROM crhd
    FOR ALL ENTRIES IN gt_afvc
    WHERE objid = gt_afvc-arbid
    AND arbpl IN s_arbpl .

  SORT gt_crhd BY objid .

  "成本中心
  SELECT * INTO TABLE gt_crco
    FROM crco
    FOR ALL ENTRIES IN gt_afvc
    WHERE objid = gt_afvc-arbid
    AND kostl IN s_kostl .

  SORT gt_crco BY objid.


  SELECT * INTO TABLE gt_cskt
    FROM cskt
    FOR ALL ENTRIES IN gt_crco
    WHERE kostl = gt_crco-kostl
    AND spras = sy-langu .

  SORT gt_cskt BY kostl .

  SELECT * INTO TABLE gt_crtx
    FROM crtx
     FOR ALL ENTRIES IN gt_afvc
    WHERE objid = gt_afvc-arbid
       AND spras = sy-langu.

  SORT gt_crtx BY objid .





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
  "处理销售订单
  LOOP AT  gt_kdauf INTO gs_kdauf .

    PERFORM selxmmc USING gs_kdauf-kdauf sy-langu CHANGING gs_kdauf-xmmc .
    MODIFY gt_kdauf FROM gs_kdauf.
  ENDLOOP.
  "统计本期值
  LOOP AT gt_afru INTO gs_afru .
    CLEAR:gs_bqz .

    IF gs_afru-stzhl IS NOT INITIAL.
      gs_bqz-rueck = gs_afru-rueck .
      gs_bqz-bqwcsl = gs_afru-gmnga * -1 .
      gs_bqz-bqrg = gs_afru-ism01 * -1 .
      gs_bqz-bqjq = gs_afru-ism02  * -1 ..
      gs_bqz-bqhd = gs_afru-ism03  * -1 ..
      gs_bqz-bqzzgs = gs_afru-ism04  * -1 ..
      gs_bqz-bqjb = gs_afru-ism05  * -1 ..
    ELSE.
      gs_bqz-rueck = gs_afru-rueck .
      gs_bqz-bqwcsl = gs_afru-gmnga.   "本期完成数量
      gs_bqz-bqrg = gs_afru-ism01.
      gs_bqz-bqjq = gs_afru-ism02.
      gs_bqz-bqhd = gs_afru-ism03.
      gs_bqz-bqzzgs = gs_afru-ism04.
      gs_bqz-bqjb = gs_afru-ism05.

    ENDIF.

    COLLECT gs_bqz INTO gt_bqz .

  ENDLOOP.

  SORT  gt_bqz BY rueck .


  LOOP AT gt_resb INTO gs_resb .
    "   * 方法二
    IF gs_resb-bdmng NE 0 .
      READ TABLE gt_aufnr_2 INTO gs_aufnr_2 WITH KEY rsnum = gs_resb-rsnum  .
      IF sy-subrc EQ 0 .
        gs_resb-ts = gs_resb-enmng / gs_resb-bdmng *  gs_aufnr_2-psmng .
      ENDIF.
    ENDIF.

    MODIFY gt_resb FROM  gs_resb .
  ENDLOOP.
  SORT gt_resb BY rsnum ASCENDING ts ASCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_resb COMPARING rsnum ts .

  LOOP AT gt_aufnr_2 INTO gs_aufnr_2.
    CLEAR:gs_data.
    gs_data-werks = gs_aufnr_2-werks .  "工厂
    gs_data-aufnr = gs_aufnr_2-aufnr .  "生产订单
    gs_data-auart = gs_aufnr_2-auart .   "生产订单类型
    gs_data-matnr = gs_aufnr_2-matnr.    "物料号
    gs_data-ktext = gs_aufnr_2-ktext.    "订单文本
    READ TABLE gt_matnr INTO gs_matnr WITH KEY matnr = gs_aufnr_2-matnr
                                               werks = gs_aufnr_2-werks
                                               BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-maktx = gs_matnr-maktx .  "物料描述
    ENDIF.
    IF gs_aufnr_2-kdauf IS NOT INITIAL .
      gs_data-kdauf = gs_aufnr_2-kdauf  .  "销售订单
      gs_data-kdpos = gs_aufnr_2-kdpos  .  "销售订单行号
      READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_data-kdauf
                                                 BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-xmmc = gs_kdauf-xmmc .
      ENDIF.
    ELSE.
      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-aufnr+0(7)
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kdauf = gs_vbak-vbeln .
        READ TABLE gt_kdauf INTO gs_kdauf WITH KEY kdauf =  gs_data-kdauf
                                             BINARY SEARCH .
        IF sy-subrc EQ 0 .
          gs_data-xmmc = gs_kdauf-xmmc .
        ENDIF.

      ENDIF.

    ENDIF.

    "订单数量
    gs_data-psmng = gs_aufnr_2-psmng .
    "订单单位
    gs_data-amein = gs_aufnr_2-amein .
    "组件投料套数

    READ TABLE gt_resb INTO gs_resb WITH KEY rsnum = gs_aufnr_2-rsnum BINARY SEARCH .
    IF sy-subrc EQ 0 .
      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0       " 保留多少位小数
          input         = gs_resb-ts
          sign          = '+'     " + 向上取舍 - 向下取舍 （负数也一样）
        IMPORTING
          output        = gs_resb-ts     " 输出返回结果
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.
      IF gs_resb-ts >   gs_data-psmng .
        gs_data-flts =   gs_data-psmng.
      ELSE.
        gs_data-flts  = gs_resb-ts .
      ENDIF.
    ELSE.
      gs_data-flts =   gs_data-psmng .
    ENDIF.

    "入库数量
    gs_data-rksl = gs_aufnr_2-wemng .
    "订单关闭
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_aufnr_2-objnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0 .
      READ TABLE gt_tj02t INTO gs_tj02t WITH KEY istat = gs_jest-stat
                                         BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-ddgb = gs_tj02t-txt04 .
      ENDIF.
    ENDIF.

    "统计工序 明细
    LOOP  AT gt_afvc INTO gs_afvc WHERE aufpl = gs_aufnr_2-aufpl.
      "工作中心

      READ TABLE gt_crhd INTO gs_crhd WITH KEY objid = gs_afvc-arbid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-arbpl = gs_crhd-arbpl .
      ELSE.
        CLEAR:gs_data-arbpl .
        CONTINUE.
      ENDIF.

      "总人工、总机器、总耗电、总制造工时、累计加班
      READ TABLE gt_afvv INTO gs_afvv WITH KEY aufpl = gs_afvc-aufpl
                                               aplzl = gs_afvc-aplzl
                                               BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-ism01 = gs_afvv-ism01.
        gs_data-ism02 = gs_afvv-ism02.
        gs_data-ism03 = gs_afvv-ism03.
        gs_data-ism04 = gs_afvv-ism04.
        gs_data-ism05 = gs_afvv-ism05.
      ELSE.
        CLEAR:gs_data-ism01,gs_data-ism02,gs_data-ism03,gs_data-ism04,gs_data-ism05.
      ENDIF.

      "成本中心
      READ TABLE gt_crco INTO gs_crco WITH KEY objid = gs_afvc-arbid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kostl = gs_crco-kostl .
      ELSE.
        CLEAR:gs_data-kostl.
        CONTINUE.
      ENDIF.

      "工作中心描述
      READ TABLE gt_crtx INTO gs_crtx WITH KEY objid = gs_afvc-arbid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-arbpl_txt = gs_crtx-ktext .
      ELSE.
        CLEAR:gs_data-arbpl_txt.
      ENDIF.



      READ TABLE gt_cskt INTO gs_cskt WITH KEY kostl = gs_data-kostl BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kostl_txt = gs_cskt-ltext .
      ELSE.
        CLEAR:gs_data-kostl_txt .
      ENDIF.
      "工序编码
      gs_data-vornr = gs_afvc-vornr.
      "工序描述
      gs_data-ltxa1 = gs_afvc-ltxa1 .
      "累计完成数量
      READ TABLE gt_afvv INTO gs_afvv WITH KEY aufpl = gs_afvc-aufpl
                                                aplzl = gs_afvc-aplzl
                                                BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-gmnga = gs_afvv-gmnga .
        "未完成数量
        gs_data-wwsl = gs_data-psmng -   gs_data-gmnga .
        "总人工
        gs_data-ism01 = gs_afvv-ism01.
        "标准总人工
        IF gs_afvv-bmsch IS NOT INITIAL.
          "标准总人工bzzrg
          gs_data-bzzrg = gs_afvv-mgvrg / gs_afvv-bmsch * gs_afvv-vgw01 .
          "标准总机器
          gs_data-bzzjq = gs_afvv-mgvrg / gs_afvv-bmsch * gs_afvv-vgw02 .
          "标准耗电
          gs_data-bzhd = gs_afvv-mgvrg / gs_afvv-bmsch * gs_afvv-vgw03 .
          "标准制造工时
          gs_data-bzzzgs = gs_afvv-mgvrg / gs_afvv-bmsch * gs_afvv-vgw04 .
          "实际开始日期
          gs_data-ieavd = gs_afvv-ieavd .
          "实际完成日期
          gs_data-isdd = gs_afvv-isdd .
        ELSE.
          CLEAR:gs_data-bzzrg,gs_data-bzzjq,gs_data-bzhd,gs_data-bzzzgs,gs_data-ieavd,gs_data-isdd.
        ENDIF.
      ELSE.
        CLEAR:gs_data-gmnga,gs_data-wwsl,gs_data-ism01.
      ENDIF.
      "本期值
      READ TABLE gt_bqz INTO gs_bqz WITH KEY rueck = gs_afvc-rueck
                                             BINARY SEARCH .
      IF sy-subrc EQ 0 .
        "本期完成数量
        gs_data-bqwcsl = gs_bqz-bqwcsl .
        "本期人工
        gs_data-bqrg = gs_bqz-bqrg .
        "本期机器
        gs_data-bqjq = gs_bqz-bqjq .
        "本期耗电
        gs_data-bqhd = gs_bqz-bqhd .
        "本期制造工时
        gs_data-bqzzgs = gs_bqz-bqzzgs .
        "本期加班
        gs_data-bqjb = gs_bqz-bqjb .
      ELSE.
        CLEAR:gs_data-bqwcsl,gs_data-bqrg,gs_data-bqjq,gs_data-bqhd,gs_data-bqzzgs,gs_data-bqjb.
      ENDIF.

      READ TABLE gt_afru_1 INTO gs_afru_1 WITH KEY rueck = gs_afvc-rueck
                                          BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-ism06 = gs_afru_1-ism06 .
      ELSE.
        CLEAR:gs_data-ism06.
      ENDIF.
      "工序控制码
      gs_data-steus = gs_afvc-steus .



      APPEND gs_data TO gt_data .
    ENDLOOP.




  ENDLOOP.







ENDFORM.


FORM selxmmc  USING    p_vbeln TYPE vbeln
                       p_yy     TYPE spras
              CHANGING p_xmmc TYPE string.


  " 取项目名称 - 销售订单抬头文本
  g_objname = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = 'Z001'
      language                = p_yy
      name                    = g_objname
      object                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
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
  IF sy-subrc = 0.
    READ TABLE it_lines INTO wa_lines INDEX 1.
    IF sy-subrc = 0.
      p_xmmc = wa_lines-tdline.
    ENDIF.
  ENDIF.
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

  CALL TRANSFORMATION zpp014_1_zh
     SOURCE table = gt_data
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

  dname =  'ZPP014生产订单报工信息报表查询.XLS' .
  CONCATENATE 'D:\usr\sap\EP1\tmp\dc\' dname  INTO dname .
  DELETE DATASET dname .
  OPEN DATASET dname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0. EXIT. ENDIF.
  LOOP AT xml_table INTO wa_xml.
    TRANSFER wa_xml TO dname.
  ENDLOOP.
  CLOSE DATASET dname.

ENDFORM.
