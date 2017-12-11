REPORT zpp014.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160831
*& Request       :
*& Descriptions  : 生产订单报 工信息查询
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
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

DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

FIELD-SYMBOLS: <fs_data> TYPE ty_data .

*获取销售长文本
DATA lt_line TYPE TABLE OF tline.
DATA ls_line TYPE tline.
DATA l_name TYPE thead-tdname.

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
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

  if gw_lvc-fieldname eq 'PSMNG' " OR gw_lvc-fieldname eq 'BCRK'
     OR gw_lvc-fieldname eq 'GMNGA' OR gw_lvc-fieldname eq 'WWSL'
     OR gw_lvc-fieldname eq 'BQWCSL' OR gw_lvc-fieldname eq 'RKSL'.
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'AMEIN'.
  endif.




*  IF gw_lvc-fieldname = 'LYTS'
*  OR gw_lvc-fieldname = 'BCLLS'.
*     gw_lvc-NO_ZERO = 'X'.
*  ENDIF.

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

DATA g_edit TYPE c VALUE 'X'. "控制不可编辑

DATA lt_t001w TYPE t001w OCCURS 0 WITH HEADER LINE.

"定义工厂数据
DATA:gt_t001w TYPE TABLE OF t001w,
     gs_t001w TYPE t001w.

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
                s_budat FOR afru-budat,
                s_arbpl FOR crhd-arbpl,
                s_kostl FOR crco-kostl.
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
  PERFORM frm_alv_show. "ALV显示

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
  SELECT * INTO TABLE gt_t001w
    FROM t001w
    WHERE werks IN s_werks.
  SORT gt_t001w BY werks .

  LOOP AT gt_t001w INTO gs_t001w.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
       ID 'WERKS' FIELD  gs_t001w-werks.
    IF sy-subrc <> 0.
      MESSAGE e603(fco) WITH gs_t001w-werks.
    ENDIF.

  ENDLOOP.
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
  SELECT * INTO TABLE gt_afru
    FROM afru
    WHERE budat IN s_budat
    AND   werks IN s_werks
    AND   aufnr IN s_aufnr .

  SORT gt_afru BY  rueck rmzhl .

  CHECK gt_afru IS NOT INITIAL .

  SELECT * INTO TABLE gt_afru_1
   FROM afru
   WHERE   werks IN s_werks
   AND   aufnr IN s_aufnr
   AND   stzhl EQ ''
    AND  stokz EQ ''
    AND ism06  IS NOT NULL .


  SORT gt_afru_1 BY rueck rmzhl .

  DELETE ADJACENT DUPLICATES FROM gt_afru_1  COMPARING rueck  .

  MOVE-CORRESPONDING gt_afru TO gt_aufnr .

  SORT gt_aufnr BY werks aufnr .

  DELETE ADJACENT DUPLICATES FROM gt_aufnr COMPARING werks aufnr .

  SELECT a~aufnr a~aufpl a~rsnum
         b~auart b~objnr b~werks b~ktext
         c~matnr c~kdauf c~kdpos c~psmng c~wemng c~amein
     INTO CORRESPONDING FIELDS OF TABLE gt_aufnr_2
      FROM afko AS a
      INNER JOIN aufk AS b
      ON a~aufnr = b~aufnr
      INNER JOIN afpo AS c
      ON a~aufnr = c~aufnr
      FOR ALL ENTRIES IN gt_aufnr
      WHERE a~aufnr = gt_aufnr-aufnr .
  SORT gt_aufnr_2 BY  aufpl .

  CHECK gt_aufnr_2 IS NOT INITIAL .
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
        clear:gs_data-arbpl .
        CONTINUE.
      ENDIF.

      "总人工、总机器、总耗电、总制造工时、累计加班
      read table gt_afvv into gs_afvv with key aufpl = gs_afvc-aufpl
                                               aplzl = gs_afvc-aplzl
                                               binary search .
         if sy-subrc eq 0 .
              gs_data-ism01 = gs_afvv-ism01.
              gs_data-ism02 = gs_afvv-ism02.
              gs_data-ism03 = gs_afvv-ism03.
              gs_data-ism04 = gs_afvv-ism04.
              gs_data-ism05 = gs_afvv-ism05.
           else.
             clear:gs_data-ism01,gs_data-ism02,gs_data-ism03,gs_data-ism04,gs_data-ism05.
         endif.

      "成本中心
      READ TABLE gt_crco INTO gs_crco WITH KEY objid = gs_afvc-arbid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kostl = gs_crco-kostl .
      ELSE.
        clear:gs_data-kostl.
        CONTINUE.
      ENDIF.

      "工作中心描述
      READ TABLE gt_crtx INTO gs_crtx WITH KEY objid = gs_afvc-arbid BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-arbpl_txt = gs_crtx-ktext .
        else.
         clear:gs_data-arbpl_txt.
      ENDIF.



      READ TABLE gt_cskt INTO gs_cskt WITH KEY kostl = gs_data-kostl BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-kostl_txt = gs_cskt-ltext .
        else.
         clear:gs_data-kostl_txt .
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
          else.
            clear:gs_data-bzzrg,gs_data-bzzjq,gs_data-bzhd,gs_data-bzzzgs,gs_data-ieavd,gs_data-isdd.
        ENDIF.
        else.
          clear:gs_data-gmnga,gs_data-wwsl,gs_data-ism01.
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
        else.
         clear:gs_data-bqwcsl,gs_data-bqrg,gs_data-bqjq,gs_data-bqhd,gs_data-bqzzgs,gs_data-bqjb.
      ENDIF.

      READ TABLE gt_afru_1 INTO gs_afru_1 WITH KEY rueck = gs_afvc-rueck
                                          BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-ism06 = gs_afru_1-ism06 .
        else.
         clear:gs_data-ism06.
      ENDIF.
      "工序控制码
      gs_data-steus = gs_afvc-steus .



      APPEND gs_data TO gt_data .
    ENDLOOP.




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
  gw_grid_settings-edt_cll_cb = 'X'.
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
  gw_layout-zebra         = 'X'.
  gw_layout-cwidth_opt    = 'X'.
  gw_layout-box_fname     = 'ZSEL'.
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
  init_fieldcat 'WERKS'            '工厂'            '' '' '' '' '' 'AUFK' 'WERKS'.
  init_fieldcat 'AUFNR'            '生产订单'        '' '' '' '' 'X' 'AUFK' 'AUFNR'.
  init_fieldcat 'AUART'            '订单类型'        '' '' '' '' '' 'AUFK' 'AUART'.
  init_fieldcat 'AMEIN'            '订单单位'        '' '' '' '' '' 'AFPO' 'AMEIN'.
  init_fieldcat 'KTEXT'            '订单文本'        '' '' '' '' '' 'AUFK' 'KTEXT'.
  init_fieldcat 'MATNR'            '物料号'          '' '' '' '' '' 'AFPO' 'MATNR'.
  init_fieldcat 'MAKTX'            '物料描述'        '' '' '' '' '' 'MAKT' 'MAKTX'.
  init_fieldcat 'KDAUF'            '销售订单'        '' '' '' '' '' 'AFPO' 'KDAUF'.
  init_fieldcat 'KDPOS'            '销售订单行号'     '' '' '' '' '' 'AFPO' 'KDPOS'.
  init_fieldcat 'XMMC'             '项目名称'         '' '' '' '' '' '' ''.
  init_fieldcat 'VORNR'            '工序编码'         '' '' '' '' '' 'AFVC' 'VORNR'.
  init_fieldcat 'LTXA1'            '工序描述'         '' '' '' '' '' 'AFVC' 'LTXA1'.
  init_fieldcat 'PSMNG'            '订单数量'         '' '' '' '' '' 'AFPO' 'PSMNG'.
  init_fieldcat 'GMNGA'            '累计完成数量'      '' '' '' '' '' 'AFVV' 'GMNGA'.
  init_fieldcat 'WWSL'             '未完成数量'        '' '' '' '' '' '' ''.
  init_fieldcat 'BQWCSL'            '本期完成数量'     '' '' '' '' '' '' ''.
  init_fieldcat 'FLTS'             '发料套数'          '' '' '' '' '' '' ''.
  init_fieldcat 'RKSL'             '入库数量'          '' '' '' '' '' '' ''.
  init_fieldcat 'DDGB'             '订单关闭'          '' '' '' '' '' 'MAKT' 'MAKTX'.
  init_fieldcat 'ISM01'            '总人工'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQRG'             '本期人工'          '' '' '' '' '' '' ''.
  init_fieldcat 'BZZRG'            '标准总人工'        '' '' '' '' '' '' ''.
  init_fieldcat 'ISM02'            '总机器'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQJQ'             '本期机器'          '' '' '' '' '' '' ''.
  init_fieldcat 'BZZJQ'            '标准总机器'        '' '' '' '' '' '' ''.
  init_fieldcat 'ISM03'            '总耗电'            '' '' '' '' '' '' ''.
  init_fieldcat 'BQHD'             '本期耗电'          '' '' '' '' '' '' ''.
  init_fieldcat 'BZHD'             '标准耗电'          '' '' '' '' '' '' ''.
  init_fieldcat 'ISM04'            '总制造工时'        '' '' '' '' '' '' ''.
  init_fieldcat 'BQZZGS'           '本期制造工时'       '' '' '' '' '' '' ''.
  init_fieldcat 'BZZZGS'           '标准制造工时'       '' '' '' '' '' '' ''.
  init_fieldcat 'ISM05'            '累计加班'           '' '' '' '' '' '' ''.
  init_fieldcat 'BQJB'             '本期加班'           '' '' '' '' '' '' ''.
  init_fieldcat 'ISM06'            '人数'               '' '' '' '' '' '' ''.
  init_fieldcat 'ARBPL'            '工作中心'           '' '' '' '' '' 'CRHD' 'ARBPL'.
  init_fieldcat 'ARBPL_TXT'        '工作中心描述'        '' '' '' '' '' 'CRTX' 'KTEXT'.
  init_fieldcat 'KOSTL'            '成本中心'            '' '' '' '' '' 'CRCO' 'KOSTL'.
  init_fieldcat 'KOSTL_TXT'        '成本中心描述'        '' '' '' '' '' 'CSKT' 'LTEXT'.
  init_fieldcat 'STEUS'            '工序控制码'          '' '' '' '' '' 'AFVC' 'STEUS'..
  init_fieldcat 'IEAVD'            '实际开始日期'        '' '' '' '' '' 'AFVV' 'IEAVD'.
  init_fieldcat 'ISDD'             '实际完成日期'        '' '' '' '' '' 'AFVV' 'ISDD'.
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0391   text
*      -->P_0392   text
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
ENDFORM.                    "ALV_PF_STATUS\


*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA g_ref_grid TYPE REF TO cl_gui_alv_grid. "刷新行到内表
  DATA l_subrc TYPE sy-subrc.
  CLEAR l_subrc.

  "CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
    WHEN '&IC1'.
      "链接到生产订单确认事务：CO14
     READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
       IF SY-SUBRC EQ 0 .
           IF RS_SELFIELD-FIELDNAME = 'AUFNR'.
              SET PARAMETER ID 'ANR' FIELD  GS_DATA-AUFNR .
              CALL TRANSACTION 'CO14' AND SKIP  FIRST  SCREEN .
            ENDIF.

        ENDIF.

*打印
    WHEN '&PRNT'.

      PERFORM frm_print_data.





  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print_data .

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
