REPORT zfi017_4.
*&---------------------------------------------------------------------*
*& Report  ZFI017
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/6
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
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
TABLES:ekko,rbkp,zfi017,lfm1,prps.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        zbox      TYPE   c,
        statu     TYPE   iconname,       "状态烂
        ebeln     TYPE   ekko-ebeln,     "采购订单号
        belnr     TYPE   rbkp-belnr,     "发票凭证
        gjahr     TYPE   rbkp-gjahr,     "发票会计年度
        zsqd      TYPE   zfi017-zsqd,    "付款申请单
        bukrs     TYPE   ekko-bukrs,     "公司代码
        lifnr     TYPE   ekko-lifnr,     "供应商
        name1     TYPE   lfa1-name1,     "供应商名称
        lifn2     TYPE   lfa1-lifnr,     "供应商开票方
        name2     TYPE   lfa1-name1,     "开票方供应商描述
        netwr     TYPE   ekpo-netwr,     "采购订单总价
        waers     TYPE   ekko-waers,     "货币单位
        netwr_1   TYPE   ekpo-netwr,     "采购订单总价（外币）
        yjhje     TYPE   ekbe-dmbtr,     "已交货金额
        yjhje_1   TYPE   ekbe-dmbtr,     "已交货金额(外币)
        fppzje    TYPE   rbkp-rmwwr,     "发票总金额
        fppzje_1  TYPE   rbkp-rmwwr,     "发票总金额
        yfje      TYPE   ekbe-dmbtr,     "已付金额
        zfkxz     TYPE   zfi017-zfkxz,   "付款性质
        zsqfkje   TYPE   zfi017-zsqfkje, "申请付款金额
        zsqfkjedx TYPE   c LENGTH 150,   "申请付款金额大写
        waers_2   TYPE   zfi017-waers_2, "货币
        zsqrq     TYPE   zfi017-zsqrq,   "申请日期
        zsqfkrq   TYPE   zfi017-zsqfkrq, "申请付款日期
        zzy       TYPE   zfi017-zzy,     "摘要
        zcljd     TYPE   zfi017-zcljd,   "处理进度
        belnr_f   TYPE   zfi017-belnr_f, "付款会计凭证
        gjahr_f   TYPE   zfi017-gjahr_f, "付款会计年度
        waers_1   TYPE   ekko-waers,     "货币单位
        celltab   TYPE   lvc_t_styl,     "控制单元格属性
        fkdq      TYPE   sy-datum ,      "付款到期日IT02150601
        sgtxt     TYPE   rbkp-sgtxt,     "发票抬头文本  IT02 150709
        ysqje     TYPE   zfi017-ysqje,   "已申请金额
        ekgrp     TYPE   lfm1-ekgrp,     "采购组
        eknam     TYPE   t024-eknam,     "采购组
        posid     TYPE   zfi017-posid,   "项目WBS
        post1     TYPE   zfi017-post1,   "项目描述
        mwskz     TYPE   zfi017-mwskz,   "是否含税
        zterm     TYPE   zfi017-zterm,   "子付款条件
        text1     TYPE   zfi017-text1,   "子付款条件描述
        ratpz     TYPE   zfi017-ratpz,   "应付比例
        yfje1     TYPE   zfi017-yfje1,   "应付金额
        yfbl      TYPE   zfi017-yfbl,    "已付比例
        wfje      TYPE   zfi017-wfje,    "未付金额
        zch       TYPE   zfi017-zch,     "拆行
        zname1    TYPE   zfi006-zname1,  "审批人
        htbh      TYPE   string ,        "合同编号
        fpje      TYPE   ekbe-wrbtr,     "发票金额（净值）
        fpje_1    TYPE   ekbe-wrbtr,     "发票金额（含税）
        yzfp      TYPE   ekbe-wrbtr,     "预制发票（净值）
        yzfp_1    TYPE   ekbe-wrbtr,      "预制发票（含税）
        jhje_1    TYPE   ekbe-wrbtr,     "交货金额（含税）
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  BEGIN
        yjfkrq    TYPE ekko-yjfkrq1,
        yjfkje    TYPE ekko-yjfkje1,
*&--代码添加 BY HANDYBY 23.06.2017 21:12:53  END
      END OF ty_data.
TYPES:BEGIN OF ty_sumyf ,
        zsqd  TYPE   zfi017-zsqd,    "付款申请单
        ebeln TYPE   ekko-ebeln,     "采购订单号
        yfje  TYPE   ekbe-dmbtr,     "已付金额
      END OF ty_sumyf.
"发票合计
TYPES:BEGIN OF ty_fp,
        ebeln TYPE ebeln,
        mwskz TYPE mwskz,
        wrbtr TYPE ekbe-wrbtr,
      END OF ty_fp.

TYPES:BEGIN OF ty_se,
        mwskz TYPE ekbe-mwskz,
        kbetr TYPE ftaxp-kbetr,
      END OF ty_se.

TYPES:BEGIN OF ty_ebeln,
        ebeln TYPE ekbe-ebeln,
      END OF ty_ebeln.

TYPES:BEGIN OF ty_jh,
        ebeln TYPE ebeln,
        wrbtr TYPE wrbtr,
      END OF ty_jh.

DATA:gt_ebeln TYPE TABLE OF ty_ebeln,
     gs_ebeln TYPE ty_ebeln.

DATA:gt_se_1 TYPE TABLE OF ty_se,
     gt_se_2 TYPE TABLE OF ty_se,
     gt_se   TYPE TABLE OF ty_se,
     gs_se   TYPE ty_se.

DATA:gt_fp_2 TYPE TABLE OF ty_fp,
     gs_fp_2 TYPE ty_fp.

DATA:gt_fp_p TYPE TABLE OF ty_fp,
     gs_fp_p TYPE ty_fp.

DATA:gt_jh TYPE TABLE OF ty_jh,
     gs_jh TYPE ty_jh.

DATA:gs_sumyf    TYPE ty_sumyf.
DATA:gt_sumyf   TYPE TABLE OF ty_sumyf.

DATA:gt_ekbe_1 TYPE TABLE OF ekbe,
     gs_ekbe_1 TYPE ekbe.

DATA:gt_ekbe_2 TYPE TABLE OF ekbe,
     gs_ekbe_2 TYPE ekbe.

DATA:gt_ekbe_p TYPE TABLE OF ekbe,
     gs_ekbe_p TYPE ekbe.

DATA gt_ekpo1 TYPE TABLE OF ekpo.
DATA gs_ekpo1 TYPE ekpo.

DATA: gt_ftaxp LIKE  TABLE OF ftaxp .
DATA: gs_ftaxp LIKE ftaxp.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA gt_zfi017 TYPE TABLE OF zfi017.
DATA gt_zfi017_3 TYPE TABLE OF zfi017.
DATA gs_zfi017 TYPE zfi017.
DATA gs_zfi017_3 TYPE zfi017.

DATA gt_zfi017_1  TYPE TABLE OF zfi017.
DATA gs_zfi017_1  TYPE zfi017.

DATA gt_data   TYPE TABLE OF ty_data.
DATA gs_data   TYPE ty_data.

DATA gt_bseg   TYPE TABLE OF bseg.
DATA gs_bseg   TYPE bseg.

DATA gt_bkpf   TYPE TABLE OF bkpf.
DATA gs_bkpf   TYPE bkpf.

DATA lt_data TYPE TABLE OF ty_data.
DATA ls_data TYPE ty_data.

DATA g_answer     TYPE string. "控制弹出框

DATA l_string TYPE string.

DATA gt_rbkp   TYPE TABLE OF rbkp.
DATA gs_rbkp   TYPE rbkp.


DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

"采购组名称
TYPES:BEGIN OF ty_lfm1 ,
        lifnr TYPE lfm1-lifnr, "供应商号
        ekorg TYPE lfm1-ekorg, "采购组
        ekgrp TYPE lfm1-ekgrp, "采购组
        eknam TYPE t024-eknam, "采购组ming
      END OF  ty_lfm1 .
DATA : t_lfm1 TYPE TABLE OF ty_lfm1 WITH HEADER LINE.
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
  IF gw_lvc-fieldname =  'NETWR_1' OR  gw_lvc-fieldname = 'YJHJE_1 '  OR gw_lvc-fieldname = 'FPPZJE_1' .
    gw_lvc-CFIELDNAME = 'WAERS_1'.
ENDIF.
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
************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs   FOR ekko-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY ,"会计年度
                s_lifnr   FOR ekko-lifnr ,                                     "供应商
                s_ebeln   FOR ekko-ebeln ,                                     "采购订单
*                S_BELNR   FOR RBKP-BELNR,                                      "发票号
                s_wbs     FOR prps-posid,                                       "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                s_zsqrq   FOR zfi017-zsqrq ,                                   "申请日期
                s_zsqfkr  FOR zfi017-zsqfkrq,                                  "申请付款日期
*                S_ZCLJD   FOR ZFI017-ZCLJD,                                    "处理进度
                s_zsqd    FOR zfi017-zsqd,                                     "申请单
                s_ekgrp FOR lfm1-ekgrp."采购组
SELECTION-SCREEN END OF BLOCK blk1.
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

*权限检查检查公司代码
  PERFORM frm_auth_check USING '03'.
  IF sy-subrc NE 0.
    MESSAGE i011(zfico01) WITH s_bukrs-low DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM frm_get_data. "取数逻辑
  PERFORM frm_deal_data."处理数逻辑
  PERFORM frm_alv_show. "ALV显示

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
FORM frm_auth_check USING VALUE(p_actvt).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD p_actvt
                                      ID 'BUKRS' FIELD s_bukrs-low.
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
*查询已完成付款申请
  SELECT * FROM zfi017
   INTO CORRESPONDING FIELDS OF TABLE gt_zfi017_3
   WHERE bukrs    IN s_bukrs
   AND   lifnr    IN s_lifnr
   AND   ebeln    IN s_ebeln
*   AND   BELNR    IN S_BELNR
*   AND   GJAHR    IN S_GJAHR
   AND   zsqrq    IN s_zsqrq
   AND   zsqfkrq  IN s_zsqfkr
*   AND   ZCLJD    IN S_ZCLJD
   AND   zsqd     IN s_zsqd
   AND   posid    IN s_wbs
   AND   zcljd    = '3' .
*查询自建表数据
  SELECT * FROM zfi017
   INTO CORRESPONDING FIELDS OF TABLE gt_zfi017
   WHERE bukrs    IN s_bukrs
   AND   lifnr    IN s_lifnr
   AND   ebeln    IN s_ebeln
*   AND   BELNR    IN S_BELNR
*   AND   GJAHR    IN S_GJAHR
   AND   zsqrq    IN s_zsqrq
   AND   zsqfkrq  IN s_zsqfkr
*   AND   ZCLJD    IN S_ZCLJD
   AND   zsqd     IN s_zsqd
   AND   posid    IN s_wbs
   AND   zcljd    = '1' .
  IF gt_zfi017[] IS NOT INITIAL.
    "查询采购组名称
    "*查询采购组 及采购组名
    SELECT a~lifnr a~ekorg a~ekgrp  b~eknam
    INTO CORRESPONDING FIELDS OF TABLE t_lfm1
    FROM lfm1 AS a
    LEFT JOIN t024 AS b
    ON a~ekgrp = b~ekgrp
    FOR ALL ENTRIES IN gt_zfi017
    WHERE  a~lifnr IN s_lifnr AND a~ekorg = s_bukrs-low AND lifnr = gt_zfi017-lifnr AND  a~ekgrp IN s_ekgrp.
    SORT t_lfm1 BY  lifnr ekgrp .
    " *查询清帐凭证
    SELECT * FROM bseg
     INTO CORRESPONDING FIELDS OF TABLE gt_bseg
     FOR ALL ENTRIES IN gt_zfi017
     WHERE bukrs = gt_zfi017-bukrs
*   AND   GJAHR = GT_ZFI017-GJAHR
     AND   lifnr = gt_zfi017-lifnr
*   AND   ZUONR = GT_ZFI017-ZSQD
     AND   bschl = '21'.

    IF gt_bseg IS NOT INITIAL .
*查询是否冲销
      SELECT * FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE gt_bkpf
        FOR ALL ENTRIES IN gt_bseg
        WHERE gjahr = gt_bseg-gjahr
        AND   bukrs = gt_bseg-bukrs
        AND   belnr = gt_bseg-belnr.
    ENDIF.
    MOVE-CORRESPONDING gt_zfi017 TO gt_ebeln .
    SORT gt_ebeln BY ebeln .
    DELETE ADJACENT DUPLICATES FROM gt_ebeln COMPARING ebeln .
    IF gt_ebeln IS NOT INITIAL.
      SELECT * FROM ekbe
        INTO CORRESPONDING FIELDS OF TABLE gt_ekbe_2
        FOR ALL ENTRIES IN gt_ebeln
        WHERE ebeln = gt_ebeln-ebeln
        AND   vgabe  EQ '2'.
      SORT gt_ekbe_2 BY ebeln .

      SELECT * FROM ekbe
        INTO CORRESPONDING FIELDS OF TABLE gt_ekbe_p
        FOR ALL ENTRIES IN gt_ebeln
        WHERE ebeln = gt_ebeln-ebeln
        AND vgabe EQ 'P'.
      SORT gt_ekbe_p BY ebeln .

      "*查询订单税码
      SELECT * FROM ekpo
       INTO CORRESPONDING FIELDS OF TABLE gt_ekpo1
       FOR ALL ENTRIES IN gt_ebeln
       WHERE ekpo~ebeln = gt_ebeln-ebeln
       AND  loekz <> 'L'
       AND  mwskz NE ''.
      SORT gt_ekpo1 BY ebeln ebelp.

      SELECT * FROM ekbe
      INTO CORRESPONDING FIELDS OF TABLE gt_ekbe_1
      FOR ALL ENTRIES IN gt_ebeln
      WHERE ebeln = gt_ebeln-ebeln
      AND vgabe EQ '1'.
      SORT gt_ekbe_1 BY ebeln .

    ENDIF.
  ENDIF.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_rbkp
    FROM rbkp
    WHERE bukrs = s_bukrs-low.

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
  DATA l1_tabix TYPE sy-tabix.
  CLEAR l1_tabix.
  DATA l_yfje     TYPE zfi017-yfje.
  DATA l_ysqje    TYPE zfi017-ysqje."已申请金额
  DATA l_zsqfkje  TYPE zfi017-zsqfkje."申请付款金额
  "统计发票金额。
  LOOP AT gt_ekbe_2  INTO gs_ekbe_2.
    CLEAR:gs_fp_2.
    gs_fp_2-ebeln = gs_ekbe_2-ebeln.
    gs_fp_2-mwskz = gs_ekbe_2-mwskz.
    IF gs_ekbe_2-shkzg EQ 'H'.
      gs_fp_2-wrbtr = gs_ekbe_2-wrbtr * -1 .
    ELSE.
      gs_fp_2-wrbtr = gs_ekbe_2-wrbtr .
    ENDIF.
    COLLECT gs_fp_2 INTO gt_fp_2.
  ENDLOOP.
  SORT gt_fp_2 BY ebeln .

  MOVE-CORRESPONDING gt_fp_2 TO gt_se_1.
  APPEND LINES OF gt_se_1 TO gt_se .

  "统计预制发票金额。
  LOOP AT gt_ekbe_p  INTO gs_ekbe_p.
    CLEAR:gs_fp_p.
    gs_fp_p-ebeln = gs_ekbe_p-ebeln.
    gs_fp_p-mwskz = gs_ekbe_p-mwskz.
    IF gs_ekbe_p-shkzg EQ 'H'.
      gs_fp_p-wrbtr = gs_ekbe_p-wrbtr * -1 .
    ELSE.
      gs_fp_p-wrbtr = gs_ekbe_p-wrbtr .
    ENDIF.
    COLLECT gs_fp_p INTO gt_fp_p.
  ENDLOOP.
  SORT gt_fp_p BY ebeln .

  MOVE-CORRESPONDING gt_fp_p TO gt_se_2.
  APPEND LINES OF gt_se_2 TO gt_se .

  SORT gt_se BY mwskz .
  DELETE ADJACENT DUPLICATES FROM gt_se COMPARING mwskz .

  LOOP AT gt_se INTO gs_se .
    REFRESH:gt_ftaxp.
    CLEAR:gs_ftaxp.
    CALL FUNCTION 'GET_TAX_PERCENTAGE'
      EXPORTING
        aland   = 'CN'
        datab   = '20000101'
        mwskz   = gs_se-mwskz
        txjcd   = 'TAXCN'
        "*     EXPORT        = ' '
      TABLES
        t_ftaxp = gt_ftaxp.
    READ TABLE gt_ftaxp INTO gs_ftaxp INDEX 1.
    IF sy-subrc EQ 0.
      gs_se-kbetr = gs_ftaxp-kbetr.
    ENDIF.
    MODIFY gt_se FROM gs_se.

  ENDLOOP.

  "统计交货金额(含税)
  LOOP AT gt_ekbe_1 INTO gs_ekbe_1.
    CLEAR:gs_jh.
    gs_jh-ebeln = gs_ekbe_1-ebeln.
    gs_jh-wrbtr = gs_ekbe_1-wrbtr.
    IF gs_ekbe_1-shkzg EQ 'H'.
      gs_jh-wrbtr = gs_jh-wrbtr * -1 .
    ENDIF.
    COLLECT gs_jh INTO gt_jh.
  ENDLOOP.
  SORT gt_jh BY ebeln  .

  LOOP AT  gt_zfi017 INTO gs_zfi017.
    l1_tabix = sy-tabix .
    READ TABLE t_lfm1 WITH KEY lifnr = gs_zfi017-lifnr.
    IF sy-subrc <> 0 .
      " DELETE GT_ZFI017 INDEX L1_TABIX .     "删除供应商不在T_LFM1表的 GT_ZFI006数据不追加到GT_DATA 150804 IT02
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING gs_zfi017 TO gs_data.
    "添加采购组及采购组名150804 IT02
    READ TABLE t_lfm1 WITH KEY lifnr = gs_data-lifnr.
    IF sy-subrc = 0.
      gs_data-ekgrp = t_lfm1-ekgrp. "
      gs_data-eknam = t_lfm1-eknam.
    ENDIF.
    READ TABLE gt_rbkp INTO gs_rbkp WITH KEY belnr = gs_data-belnr.
    IF sy-subrc = 0.
      gs_data-sgtxt = gs_rbkp-sgtxt.  "发票文本
    ENDIF .
"已交货金额调整夏俊、黄键杭
*已交货金额
    CLEAR: GS_DATA-YJHJE,GS_DATA-YJHJE_1.
    LOOP AT gt_ekbe_1 INTO GS_EKBE_1
    WHERE EBELN = GS_DATA-EBELN
    AND   VGABE = '1'.
      IF GS_EKBE_1-SHKZG = 'S'.
        GS_DATA-YJHJE   = GS_DATA-YJHJE   + GS_EKBE_1-DMBTR.
        GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 + GS_EKBE_1-WRBTR.
      ENDIF.

      IF GS_EKBE_1-SHKZG = 'H' .
        GS_DATA-YJHJE   = GS_DATA-YJHJE   - GS_EKBE_1-DMBTR.
        GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 - GS_EKBE_1-WRBTR.
      ENDIF.
    ENDLOOP.
*初始化凭证
    gs_data-belnr_f = ''.
    gs_data-gjahr_f = ''.

*当初始状态，审批人为空
    IF gs_data-zcljd = '1'.
      gs_data-zname1 = ''.
    ENDIF.
*
**查询清帐凭证
*    LOOP AT GT_BSEG INTO GS_BSEG
*     WHERE  BUKRS = GS_DATA-BUKRS
*     AND    LIFNR = GS_DATA-LIFNR
*     AND    ZUONR = GS_DATA-ZSQD
*     AND    BSCHL = '21'.
*
**排除冲销凭证
*      READ TABLE GT_BKPF INTO GS_BKPF
*      WITH KEY GJAHR = GS_BSEG-GJAHR
*               BUKRS = GS_BSEG-BUKRS
*               BELNR = GS_BSEG-BELNR.
*      IF SY-SUBRC = 0.
*        IF GS_BKPF-STBLG IS  INITIAL.
*          GS_DATA-BELNR_F = GS_BSEG-BELNR.
*          GS_DATA-GJAHR_F = GS_BSEG-GJAHR.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

**把确认的结账，但是没有找到凭证的，更改其状态.（为了排除确认付款后，冲销的情况）
*    IF GS_DATA-ZCLJD = '3'
*     AND GS_DATA-BELNR_F IS INITIAL
*     AND GS_DATA-GJAHR_F IS INITIAL.
*      GS_DATA-ZCLJD = '2'.
*      GS_DATA-STATU = ICON_GREEN_LIGHT.
*    ENDIF.
    "读取合同编号
    "  * 根据采购订单号 调用函数 Read_text 读取文本 ，文本对象-EKKO ，文本标识-F06 ADD IT02 151120

    g_objname = gs_data-ebeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F06'
        language                = '1'
        name                    = g_objname
        object                  = 'EKKO'
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
        CONDENSE wa_lines-tdline NO-GAPS.
        gs_data-htbh = wa_lines-tdline.
      ENDIF.
    ENDIF.
    APPEND gs_data TO gt_data.
    CLEAR gs_data.
  ENDLOOP.

  SORT gt_data BY zsqd belnr ebeln.
  "add:发票金额（净值）、发票金额（含税）、预制发票（净值）、预制发票（含税）
  LOOP AT gt_data INTO gs_data.
    READ TABLE gt_fp_2 INTO gs_fp_2 WITH KEY ebeln = gs_data-ebeln
                                BINARY SEARCH .
    IF sy-subrc EQ 0 .
      gs_data-fpje = gs_fp_2-wrbtr .
      READ TABLE gt_se INTO gs_se WITH KEY gs_fp_2-mwskz
                                     BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-fpje_1 = gs_data-fpje  * ( 1 + gs_se-kbetr / 1000 ).

      ENDIF.
    ENDIF.
    READ TABLE gt_fp_p INTO gs_fp_p WITH KEY ebeln = gs_data-ebeln
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_data-yzfp = gs_fp_p-wrbtr.
      READ TABLE gt_se INTO gs_se WITH KEY gs_fp_p-mwskz
                                    BINARY SEARCH .
      IF sy-subrc EQ 0 .
        gs_data-yzfp_1 = gs_data-yzfp * ( 1 + gs_se-kbetr / 1000 ).
      ENDIF.
    ENDIF.
    "交货金额（含税）
    READ TABLE gt_jh INTO gs_jh
     WITH KEY ebeln = gs_data-ebeln .
    IF sy-subrc EQ 0.

      REFRESH:gt_ftaxp.
      CLEAR:gs_ftaxp.
      CALL FUNCTION 'GET_TAX_PERCENTAGE'
        EXPORTING
          aland   = 'CN'
          datab   = '20000101'
          mwskz   = gs_data-mwskz
          txjcd   = 'TAXCN'
          "*     EXPORT        = ' '
        TABLES
          t_ftaxp = gt_ftaxp.
      READ TABLE gt_ftaxp INTO gs_ftaxp INDEX 1.
      IF sy-subrc EQ 0.
        gs_data-jhje_1 = gs_jh-wrbtr  * ( 1 + gs_ftaxp-kbetr / 1000 ) .

      ENDIF.
    ENDIF.
*已付款金额
    CLEAR l_yfje.
    LOOP AT  gt_zfi017_3 INTO gs_zfi017_3
    WHERE ebeln = gs_data-ebeln
    AND   ZCH   = gs_data-ZCH
    AND   zcljd = '3'.
      " AND         BELNR = ''.
      "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
      l_yfje = l_yfje + gs_zfi017_3-zsqfkje.  "150527 YEFE 字段值为空 才注释
    ENDLOOP.
    gs_data-yfje = l_yfje.
    IF gs_data-yfje1 IS NOT INITIAL.
      gs_data-yfbl = gs_data-yfje / gs_data-yfje1 * 100.
    ENDIF.
*已申请金额
    CLEAR l_ysqje.
    LOOP AT gt_zfi017_3 INTO gs_zfi017_3
     WHERE ebeln = gs_data-ebeln
     AND   ZCH   = gs_data-ZCH
     AND  ebeln = gs_data-ebeln
     AND ( zcljd = '3'
     OR   zcljd  = '2'
     OR   zcljd  = '1')
     AND   statu    <> icon_delete.
      l_ysqje = l_ysqje + gs_zfi017_3-zsqfkje.
    ENDLOOP.
    gs_data-ysqje = l_ysqje.
*未付金额
    gs_data-wfje = gs_data-yfje1 - gs_data-yfje.
    MODIFY gt_data FROM gs_data.
  ENDLOOP.

*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  BEGIN
*  DATA: BEGIN OF LS_EKKO,
*          EBELN   TYPE EKKO-EBELN,
*          YJFKRQ1 TYPE EKKO-YJFKRQ1,
*          YJFKJE1 TYPE EKKO-YJFKJE1,
*          YJFKRQ2 TYPE EKKO-YJFKRQ2,
*          YJFKJE2 TYPE EKKO-YJFKJE2,
*          YJFKRQ3 TYPE EKKO-YJFKRQ3,
*          YJFKJE3 TYPE EKKO-YJFKJE3,
*        END OF LS_EKKO.
*  DATA LT_EKKO LIKE TABLE OF LS_EKKO .
*  SELECT EBELN
*         YJFKRQ1
*         YJFKJE1
*         YJFKRQ2
*         YJFKJE2
*         YJFKRQ3
*         YJFKJE3
*    INTO CORRESPONDING FIELDS OF TABLE LT_EKKO
*    FROM EKKO
*     FOR ALL ENTRIES IN GT_DATA
*   WHERE EBELN = GT_DATA-EBELN .
*  SORT LT_EKKO BY EBELN .
*
*  DATA: LT_DATA2 LIKE GT_DATA,
*        LS_DATA2 LIKE LINE OF LT_DATA2.
*  LOOP AT GT_DATA INTO GS_DATA .
*    IF GS_DATA-BUKRS = '1700' OR GS_DATA-BUKRS = '1710'.
*
*      READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = GS_DATA-EBELN BINARY SEARCH .
*      IF SY-SUBRC = 0 .
*        MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ1 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE1 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ2 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE2 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*
*        LS_DATA2-YJFKRQ = LS_EKKO-YJFKRQ3 .
*        LS_DATA2-YJFKJE = LS_EKKO-YJFKJE3 .
*        INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*        CLEAR LS_DATA2-YJFKRQ .
*        CLEAR LS_DATA2-YJFKJE .
*      ENDIF.
*
*    ELSE .
*
*      MOVE-CORRESPONDING GS_DATA TO LS_DATA2 .
*      INSERT LS_DATA2 INTO TABLE LT_DATA2  .
*
*    ENDIF.
*    CLEAR GS_DATA .
*    CLEAR LS_EKKO .
*    CLEAR LS_DATA2 .
*  ENDLOOP.
*
*  REFRESH GT_DATA .
*  MOVE-CORRESPONDING LT_DATA2 TO GT_DATA .
*&--代码添加 BY HANDYBY 26.06.2017 10:35:01  END

**更新自建表
*  MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
*  MODIFY ZFI017 FROM TABLE GT_ZFI017.
*  REFRESH GT_ZFI017.
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
  gw_layout-zebra        = 'X'.
*  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  gw_layout-box_fname    = 'ZBOX'.
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
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_exclude .
  REFRESH gt_exclude.
  CLEAR gs_exclude.
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
  gw_events-name =  slis_ev_data_changed.
  gw_events-form = 'FRM_DATA_CHANGED'.
  APPEND gw_events TO gt_events.
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
  init_fieldcat 'STATU'          '状态栏'                '' '' '' '' '' '' '' .
  init_fieldcat 'ZSQD'           '申请单号'              '20' '' '' '' '' '' ''.
  init_fieldcat 'BUKRS'          '公司代码'              '' '' '' '' '' '' ''.
  init_fieldcat 'LIFNR'          '供应商'                '10' '' '' '' '' '' ''.
  init_fieldcat 'NAME1'          '供应商名称'            '' '' '' '' '' '' ''.
  init_fieldcat 'LIFN2'          '供应商（开票）'                '10' '' '' '' '' '' ''.
  init_fieldcat 'NAME2'          '供应商名称（开票）'            '' '' '' '' '' '' ''.
  init_fieldcat 'EBELN'          '采购订单号'            '' '' '' '' '' '' ''.
  init_fieldcat 'HTBH'           '合同编号'            '' '' '' '' '' '' ''.
  init_fieldcat 'EKGRP'          '采购组'                '10' '' '' '' '' '' ''.
  init_fieldcat 'EKNAM'          '采购组名'            '' '' '' '' '' '' ''.
  init_fieldcat 'POSID'          '项目编号'            '' '' '' '' '' '' ''.
  init_fieldcat 'POST1'          '项目名称'            '' '' '' '' '' '' ''.
  init_fieldcat 'MWSKZ'          '是否含税'   '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS'          '本位币'   '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS_1'        '凭证货币'   '' '' '' '' '' '' ''.
  init_fieldcat 'NETWR'          '采购订单含税金额（本币）'   '' '' '' '' '' '' ''.
  init_fieldcat 'NETWR_1'        '采购订单含税金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'MWSKZ'          '是否含税'   '' '' '' '' '' '' ''.
  init_fieldcat 'ZTERM'          '子付款条件'   '' '' '' '' '' '' ''.
  init_fieldcat 'TEXT1'          '子付款条件描述'   '' '' '' '' '' '' ''.
  init_fieldcat 'RATPZ'          '应付比例%'   '' '' '' '' '' '' ''.
  init_fieldcat 'YFJE1'          '应付金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'YJHJE'          '已交货金额（本币）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YJHJE_1'        '已交货金额（净值）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YFJE'           '已付金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'YFBL'           '已付比例%'   '' '' '' '' '' '' ''.
  init_fieldcat 'YSQJE'          '已申请金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'WFJE'           '未付金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'ZFKXZ'          '付款性质'   '' '' '' '' ''  'ZFI017' 'ZFKXZ'.
  init_fieldcat 'ZSQFKJE'        '申请付款金额'   '' '' '' '' '' 'ZFI017' 'ZSQFKJE'.
  init_fieldcat 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  init_fieldcat 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  init_fieldcat 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
  init_fieldcat 'WAERS_2'        '货币'   '6' '' '' '' '' 'EKKO' 'WAERS'.
  init_fieldcat 'ZSQRQ'          '申请日期'   '' '' '' '' '' 'ZFI017' 'ZSQRQ'.
  init_fieldcat 'ZSQFKRQ'        '申请付款日期'   '' '' '' '' '' 'ZFI017' 'ZSQFKRQ'.
  init_fieldcat 'ZZY'            '摘要'   '15' '' '' '' '' 'ZFI017' 'ZZY'.
  init_fieldcat 'ZCLJD'          '处理进度'   '6' '' '' '' '' 'ZFI017' 'ZCLJD' .
  init_fieldcat 'BELNR_F'        '付款会计凭证'   '' '' '' '' '' '' ''.
  init_fieldcat 'GJAHR_F'        '付款会计年度'   '' '' '' '' '' '' ''.
  init_fieldcat 'SGTXT'          '发票文本'   '15' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  BEGIN
  init_fieldcat 'YJFKRQ'          '预计付款日期'   '' '' '' '' '' '' ''.
  init_fieldcat 'YJFKJE'          '预计付款金额'   '' '' '' '' '' '' ''.
*&--代码添加 BY HANDYBY 23.06.2017 21:13:26  END

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
*     I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "see FORM
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
ENDFORM.                    " FRM_OUTPUT

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
  DATA l_subrc TYPE sy-subrc."检查输入项
  CLEAR l_subrc.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

  CASE r_ucomm.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*保存,保存的时候产生申请单号
    WHEN '&DATA_SAVE'.

*检查必输项
      PERFORM check_input CHANGING l_subrc.
      IF l_subrc = '4'.
        EXIT.
      ENDIF.

*检查
      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0 .

*选择屏幕采购订单进入
        LOOP AT gt_data INTO gs_data
         WHERE zbox = 'X'.

*更新屏幕
          MODIFY gt_data FROM gs_data.
          MOVE-CORRESPONDING gs_data TO gs_zfi017_1.
          gs_zfi017_1-zname = sy-uname.
          gs_zfi017_1-zdate = sy-datum.
          gs_zfi017_1-ztime = sy-uzeit.
          APPEND gs_zfi017_1 TO gt_zfi017_1.
        ENDLOOP.

*更新数据库表
        MODIFY  zfi017 FROM TABLE gt_zfi017_1.
        REFRESH gt_zfi017_1.
        CLEAR gs_data.

*提示保存成功
        MESSAGE s002(z001).
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
      ENDIF.


    WHEN '&PRINT'.
*      READ TABLE GT_DATA INTO GS_DATA
*       WITH KEY ZBOX = 'X'.
*      IF SY-SUBRC = 0.
*        PERFORM FRM_PRINT.
*      ELSE.
*        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
*      ENDIF.

    WHEN '&DEL'.
*提示对话框
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR       = ' '
*         DIAGNOSE_OBJECT             = ' '
          text_question  = '是否执行删除操作'
        IMPORTING
          answer         = g_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF g_answer <> '1'.
        EXIT.
      ENDIF.

*更新状态
      LOOP AT gt_data INTO gs_data
      WHERE zbox = 'X'.
        gs_data-statu = icon_delete.
        MODIFY gt_data FROM gs_data.

        MOVE-CORRESPONDING gs_data TO gs_zfi017_1.
        gs_zfi017_1-zname = sy-uname.
        gs_zfi017_1-zdate = sy-datum.
        gs_zfi017_1-ztime = sy-uzeit.
        APPEND gs_zfi017_1 TO gt_zfi017_1.
      ENDLOOP.

*更新数据库表
      MODIFY  zfi017 FROM TABLE gt_zfi017_1.
      REFRESH gt_zfi017_1.
      CLEAR gs_data.

    WHEN '&F03' OR '&F15'OR  '&F12'.
      PERFORM frm_unlock.
  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.

ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM frm_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  PERFORM frm_data_enter USING er_data_changed..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_data_enter USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA: g_ref_grid TYPE REF TO cl_gui_alv_grid,
        stbl       TYPE lvc_s_stbl.
  DATA: wa_mod_cell TYPE lvc_s_modi.
  DATA: ls_data     TYPE ty_data.
  DATA: l_zsqfkje   TYPE zfi017-zsqfkje.
  DATA: l_string     TYPE char100.

  FIELD-SYMBOLS:<l_matnr> TYPE any.

  CLEAR l_zsqfkje.

  READ TABLE er_data_changed->mt_mod_cells INTO wa_mod_cell INDEX 1.
  IF wa_mod_cell-fieldname = 'ZSQFKJE'.

*汇总申请金额到抬头行
    READ TABLE gt_data INTO ls_data INDEX  wa_mod_cell-row_id.
    IF sy-subrc = 0.

*去除金额的,号
      l_string = wa_mod_cell-value.
      REPLACE ',' IN l_string WITH ''.
      l_zsqfkje  = l_string.

*汇总申请付款金额（汇总逻辑 =  修改的行 + 其他的行 （对于同一张发票而言））
      LOOP AT gt_data INTO gs_data
      WHERE belnr = ls_data-belnr
      AND   gjahr = ls_data-gjahr
      AND   ebeln <> ''
      AND   ebeln <> ls_data-ebeln.
        l_zsqfkje = l_zsqfkje + gs_data-zsqfkje.
      ENDLOOP.

      LOOP AT gt_data INTO gs_data
      WHERE belnr = ls_data-belnr
      AND   gjahr = ls_data-gjahr
      AND   ebeln = ''.
        gs_data-zsqfkje = l_zsqfkje.
        MODIFY gt_data FROM gs_data.
        CLEAR gs_data.
      ENDLOOP.
    ENDIF.
  ELSEIF  wa_mod_cell-fieldname = 'WAERS_2'.

*    READ TABLE GT_DATA INTO LS_DATA INDEX  WA_MOD_CELL-ROW_ID.
*    IF SY-SUBRC = 0 .
*      TRANSLATE WA_MOD_CELL-VALUE  TO UPPER CASE.
*       IF WA_MOD_CELL-VALUE = 'RMB'.
*        MESSAGE '不允许RMB的货币类型,请更改!'  TYPE 'S' DISPLAY LIKE 'E'.
*         ENDIF.
*    ENDIF.
  ENDIF.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_ref_grid.

*自动定位光标
  stbl-col = 'X'.
  stbl-row = 'X'.
  CALL METHOD g_ref_grid->refresh_table_display
    EXPORTING
      is_stable = stbl.
ENDFORM.                    "frm_data_enter
*-------------------------------------------------------------------*
* Form  TOP-OF-PAGE                                                 *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
*FORM TOP-OF-PAGE.
*
**ALV Header declarations
*  DATA: T_HEADER      TYPE SLIS_T_LISTHEADER,
*        WA_HEADER     TYPE SLIS_LISTHEADER,
*        T_LINE        LIKE WA_HEADER-INFO,
*        LD_LINES      TYPE I,
*        LD_LINESC(10) TYPE C.
** Title
*  WA_HEADER-TYP  = 'H'.
*  WA_HEADER-INFO =  SY-TITLE."'装箱单维护打印平台'.
*  APPEND WA_HEADER TO T_HEADER.
*  CLEAR WA_HEADER.
** Date
*  WA_HEADER-TYP  = 'S'.
*  WA_HEADER-KEY = 'Date: '.
*  CONCATENATE  SY-DATUM+6(2) '.'
*               SY-DATUM+4(2) '.'
*               SY-DATUM(4) INTO WA_HEADER-INFO.   "todays date
*  APPEND WA_HEADER TO T_HEADER.
*  CLEAR: WA_HEADER.
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = T_HEADER.
*ENDFORM.                    "top-of-page
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_print .
  DATA: control    TYPE ssfctrlop,
        ntotalline TYPE i,
        npageline  TYPE i VALUE 9,
        p_index    LIKE sy-tabix.
  DATA: emptycount      TYPE i VALUE 0,  "空行数.
        ncurrline       TYPE i,      "中间变量
        job_output_info TYPE ssfcrescl.
  DATA: g_name TYPE rs38l_fnam.
  DATA:l_formname TYPE tdsfname VALUE 'ZSFFI006'.
  DATA:l_page TYPE i.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_formname         "smartforms的名字
    IMPORTING
      fm_name            = g_name                "对应的smartforms的函数
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  control-no_open  = 'X'.
  control-no_close = 'X'.
* Start Printing

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

*根据订单号，行项目号进行排序
  SORT lt_data  BY zsqd.

  LOOP AT gt_data INTO gs_data WHERE zbox = 'X'.

    AT NEW zsqd .
      REFRESH lt_data.
    ENDAT.

    APPEND gs_data TO lt_data.

    AT END OF zsqd.
      CALL FUNCTION g_name
        EXPORTING
          control_parameters = control
*         npage              = npageline
*         w_head             = lw_prt
*         TABLES
*         t_item             = lt_prt[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input CHANGING l_subrc TYPE sy-subrc.

*检查抬头
  IF s_ebeln IS NOT INITIAL .
    LOOP AT gt_data INTO gs_data
    WHERE zbox = 'X'.

*锁对象检查，加锁
      CALL FUNCTION 'ENQUEUE_EZFI006'
        EXPORTING
*         MODE_ZFI006    = 'E'
*         MANDT          = SY-MANDT
          zsqd           = gs_data-zsqd
*         X_ZSQD         = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        CLEAR l_string.
        CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
        MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-zfkxz IS INITIAL.
        MESSAGE '付款性质不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-waers_2 IS INITIAL.
        MESSAGE '货币不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-zsqrq IS INITIAL .
        MESSAGE '申请日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-zsqfkrq IS INITIAL .
        MESSAGE '申请付款日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT gt_data INTO gs_data
    WHERE zbox = 'X'
    AND ebeln = ''.

*锁对象检查，加锁
      CALL FUNCTION 'ENQUEUE_EZFI006'
        EXPORTING
*         MODE_ZFI006    = 'E'
*         MANDT          = SY-MANDT
          zsqd           = gs_data-zsqd
*         X_ZSQD         = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
        MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      IF gs_data-zfkxz IS INITIAL.
        MESSAGE '付款性质不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-waers_2 IS INITIAL.
        MESSAGE '货币不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-zsqrq IS INITIAL .
        MESSAGE '申请日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.

      IF gs_data-zsqfkrq IS INITIAL .
        MESSAGE '申请付款日期不能为空' TYPE 'S' DISPLAY LIKE 'E'.
        l_subrc = 4.
      ENDIF.
    ENDLOOP.
  ENDIF.

*检查是否有删除标识
  LOOP AT gt_data INTO gs_data
  WHERE zbox = 'X'.
    IF gs_data-statu = icon_delete.
      MESSAGE '请不要保存已经打上删除标记的行项目' TYPE 'S' DISPLAY LIKE 'E'.
      l_subrc = 4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM frm_unlock.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.
