REPORT zfi017_2.
*&---------------------------------------------------------------------*
*& Report  ZFI017
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/09/6
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
*&
*& Modify by     : IT02-魏云
*& Modify date   :20160701
*& Request       :
*& Descriptions  :过账抬头文本及明细文本更改为：需过账明细的合同编号
*&
*&---------------------------------------------------------------------*
************************************************************************
* Tables
************************************************************************
TABLES:ekko,rbkp,zfi017,prps.
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
*&--代码注释 BY HANDYBY 27.06.2017 21:21:56  BEGIN
*    POSID     TYPE   ZFI017-POSID,   "项目WBS
*        POST1     TYPE   ZFI017-POST1,   "项目描述
*&--代码注释 BY HANDYBY 27.06.2017 21:21:56  END
        mwskz     TYPE   zfi017-mwskz,   "是否含税
        zterm     TYPE   zfi017-zterm,   "子付款条件
        text1     TYPE   zfi017-text1,   "子付款条件描述
        ratpz     TYPE   zfi017-ratpz,   "应付比例
        yfje1     TYPE   zfi017-yfje1,   "应付金额
        yfbl      TYPE   zfi017-yfbl,    "已付比例
        wfje      TYPE   zfi017-wfje,    "未付金额
        zch       TYPE   zfi017-zch,     "拆行
        zname1    TYPE   zfi006-zname1,  "审批人
        zname     TYPE   zfi017-zname,   "创建人
        zdate     TYPE   zfi017-zdate,   "创建日期
        ztime     TYPE   zfi017-ztime,   "创建时间
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
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  BEGIN
        posid     TYPE prps-posid , "WBS元素外码
        post1     TYPE prps-post1 , "WBS描述
        pspid     TYPE proj-pspid , "项目定义
        post2     TYPE proj-post1 , "项目描述
*&--代码添加 BY HANDYBY 27.06.2017 21:20:30  END
      END OF ty_data.

TYPES:BEGIN OF ty_head,
        bldat TYPE bkpf-bldat, "凭证日期
        budat TYPE bkpf-budat, "过账日期
        bukrs TYPE bkpf-bukrs, "公司代码
        waers TYPE bkpf-waers, "公司代码
        bktxt TYPE bkpf-bktxt, "抬头凭证
        belnr TYPE bkpf-belnr, "会计凭证
        gjahr TYPE bkpf-gjahr, "会计年度
        blart TYPE bkpf-blart,
      END OF ty_head.

TYPES:BEGIN OF ty_item,
        buzei     TYPE bseg-buzei, "项目号
        bschl     TYPE bseg-bschl, "记账码
        hkont     TYPE bseg-hkont, "科目
        hktxt     TYPE skat-txt20, "科目描述
        lifnr     TYPE bseg-lifnr, "供应商
        kunnr     TYPE bseg-kunnr, "客户
        wrbtr     TYPE bseg-wrbtr, "金额
        pswsl     TYPE bseg-pswsl, "货币
        rstgr     TYPE bseg-rstgr, "原因代码
        zuonr     TYPE bseg-zuonr, "汇票号
        zuonr_1   TYPE bseg-zuonr, "付款申请单号
        sgtxt     TYPE bseg-sgtxt, "行项目摘要
        pspid     TYPE proj-pspid, "项目编号
        cellstyle TYPE lvc_t_styl, "单元格状态
      END OF ty_item.

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

TYPES:BEGIN OF ty_jh,
        ebeln TYPE ebeln,
        wrbtr TYPE wrbtr,
      END OF ty_jh.

TYPES:BEGIN OF ty_ebeln,
        ebeln TYPE ekbe-ebeln,
      END OF ty_ebeln.

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

DATA gt_rbkp    TYPE TABLE OF rbkp.
DATA gs_rbkp    TYPE rbkp.
DATA:gs_sumyf   TYPE ty_sumyf.
DATA:gt_sumyf   TYPE TABLE OF ty_sumyf.

DATA: gt_ftaxp LIKE  TABLE OF ftaxp .
DATA: gs_ftaxp LIKE ftaxp.

DATA:gt_jh TYPE TABLE OF ty_jh,
     gs_jh TYPE ty_jh.
************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA gt_zfi017    TYPE TABLE OF zfi017.
DATA gs_zfi017    TYPE zfi017.

DATA gt_data      TYPE TABLE OF ty_data.
DATA gs_data      TYPE ty_data.

DATA gt_zfi017_1  TYPE TABLE OF zfi017.
DATA gs_zfi017_1  TYPE zfi017.

DATA gt_bseg      TYPE TABLE OF bseg.
DATA gs_bseg      TYPE bseg.

DATA gt_bkpf      TYPE TABLE OF bkpf.
DATA gs_bkpf      TYPE bkpf.

DATA gt_head      TYPE TABLE OF ty_head.
DATA gs_head      TYPE ty_head.

DATA gt_item      TYPE TABLE OF ty_item.
DATA gs_item      TYPE ty_item.

DATA g_wrbtr      TYPE bseg-wrbtr.

DATA g_answer     TYPE string. "控制弹出框

DATA g_edit_mod   TYPE c.
DATA g_changed    TYPE c.

CONSTANTS: gc_editable TYPE c VALUE 'X',
           gc_readonly TYPE c VALUE ''.

DATA gt_skat TYPE TABLE OF skat.
DATA gs_skat TYPE skat.

DATA: g_suc.
DATA l_string TYPE string.
FIELD-SYMBOLS: <fs_item> TYPE ty_item.

* BAPI_ACC_DOCUMENT_REV_POST
DATA: wa_reversal TYPE bapiacrev,
      wa_bus      TYPE bapiache09.

DATA: g_stgrd TYPE bkpf-stgrd.

DATA gt_ekbe   TYPE TABLE OF ekbe.
DATA gs_ekbe   TYPE ekbe.

DATA:gt_ekbe_2 TYPE TABLE OF ekbe,
     gs_ekbe_2 TYPE ekbe.

DATA:gt_ekbe_p TYPE TABLE OF ekbe,
     gs_ekbe_p TYPE ekbe.

DATA:gt_ekbe_1 TYPE TABLE OF ekbe,
     gs_ekbe_1 TYPE ekbe.


DATA: g_objname TYPE thead-tdname.

DATA: it_lines TYPE TABLE OF tline,
      wa_lines TYPE tline.

DATA gt_ekpo1 TYPE TABLE OF ekpo.
DATA gs_ekpo1 TYPE ekpo.
************************************************************************
*      DEFINE CLASS
************************************************************************
INCLUDE zfi017_cls.

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

   IF &1 = 'HKONT' OR &1 = 'RSTGR'.
    gw_lvc-f4availabl = 'X'.
  ENDIF.
IF gw_lvc-fieldname =  'NETWR_1' OR gw_lvc-fieldname = 'YJHJE_1 '  OR gw_lvc-fieldname = 'FPPZJE_1' .
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

DATA: gt_oo_exclude TYPE ui_functions.
DATA: gr_container TYPE REF TO cl_gui_custom_container.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.
************************************************************************
* BAPI
************************************************************************
DATA: wa_documentheader TYPE bapiache09,
      it_accountgl      TYPE TABLE OF bapiacgl09,
      wa_accountgl      TYPE bapiacgl09,
      it_accountpayable TYPE TABLE OF bapiacap09,
      wa_accountpayable TYPE bapiacap09,
      it_currencyamount TYPE TABLE OF bapiaccr09,
      wa_currencyamount TYPE bapiaccr09,
      it_criteria       TYPE TABLE OF bapiackec9,
      wa_criteria       TYPE bapiackec9,
      it_valuefield     TYPE TABLE OF bapiackev9,
      wa_valuefield     TYPE bapiackev9,
      it_extension2     TYPE TABLE OF bapiparex,
      wa_extension2     TYPE bapiparex,
      it_return         TYPE TABLE OF bapiret2,
      wa_return         TYPE bapiret2.

DATA: wa_obj TYPE bapiache09.

DATA: wa_zaccdocuext TYPE zaccdocuext.

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
               " S_BUDAT   FOR RBKP-BUDAT,"过账日期
                s_wbs     FOR prps-posid,                                    "WBS元素
*                S_GJAHR   FOR RBKP-GJAHR NO INTERVALS NO-EXTENSION,            "发票凭证年度
                s_zsqrq   FOR zfi017-zsqrq ,                                   "申请日期
                s_zsqfkr  FOR zfi017-zsqfkrq,                                  "申请付款日期
                s_zcljd   FOR zfi017-zcljd,                                    "处理进度
                s_zsqd    FOR zfi017-zsqd.                                     "申请单
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

*&--代码添加 BY HANDYBY 13.07.2017 15:38:43  BEGIN
  IF s_wbs[] IS NOT INITIAL .
    LOOP AT s_wbs .
      IF s_wbs-low IS NOT INITIAL .
        s_wbs-option = 'CP'.
        CONCATENATE s_wbs-low '*' INTO s_wbs-low .
        MODIFY s_wbs .
      ENDIF.
      IF s_wbs-high IS NOT INITIAL .
*        CONCATENATE S_WBS-HIGH '%' INTO S_WBS-HIGH .
*        MODIFY S_WBS .
      ENDIF.
    ENDLOOP.
  ENDIF.
*&--代码添加 BY HANDYBY 13.07.2017 15:38:43  END

*当有输入采购订单号码
*查询自建表数据
  SELECT * FROM zfi017
   INTO CORRESPONDING FIELDS OF TABLE gt_zfi017
   WHERE bukrs    IN s_bukrs
   AND   lifnr    IN s_lifnr
   AND   ebeln    IN s_ebeln
*   AND   GJAHR    IN S_GJAHR
   AND   zsqrq    IN s_zsqrq
   AND   zsqfkrq  IN s_zsqfkr
   AND   zcljd    IN s_zcljd
   AND   zsqd     IN s_zsqd
   AND   posid    IN s_wbs
   AND   statu    <> icon_delete.

*查询清帐凭证
  IF gt_zfi017[] IS NOT INITIAL.
    MOVE-CORRESPONDING gt_zfi017 TO gt_ebeln .
    SORT gt_ebeln BY ebeln .
    DELETE ADJACENT DUPLICATES FROM gt_ebeln COMPARING ebeln .

    "*查询订单税码
    SELECT * FROM ekpo
     INTO CORRESPONDING FIELDS OF TABLE gt_ekpo1
     FOR ALL ENTRIES IN gt_ebeln
     WHERE ekpo~ebeln = gt_ebeln-ebeln
     AND  loekz <> 'L'
     AND  mwskz NE ''.

    SORT gt_ekpo1 BY ebeln ebelp.

    SELECT * FROM bseg
     INTO CORRESPONDING FIELDS OF TABLE gt_bseg
     FOR ALL ENTRIES IN gt_zfi017
     WHERE bukrs = gt_zfi017-bukrs
*   AND   GJAHR = GT_ZFI017-GJAHR
     AND   lifnr = gt_zfi017-lifnr
*     AND   ZUONR = GT_ZFI017-ZSQD
     AND   bschl = '21'.
    "*查询交货历史  IT02150520
    SELECT * FROM ekbe
     INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
     FOR ALL ENTRIES IN gt_zfi017
     WHERE ebeln = gt_zfi017-ebeln
   .

    MOVE-CORRESPONDING gt_ekbe TO gt_ekbe_2.

    DELETE gt_ekbe_2 WHERE vgabe NE '2'.
    SORT gt_ekbe_2 BY ebeln ebelp .

    MOVE-CORRESPONDING gt_ekbe TO gt_ekbe_p.

    DELETE gt_ekbe_p WHERE vgabe NE 'P'.
    SORT gt_ekbe_p BY ebeln ebelp .

    MOVE-CORRESPONDING gt_ekbe TO gt_ekbe_1.

    DELETE gt_ekbe_1 WHERE vgabe NE '1'.
    SORT gt_ekbe_1 BY ebeln ebelp .
  ENDIF.
  IF gt_bseg IS NOT INITIAL .
*查询是否冲销
    SELECT * FROM bkpf
      INTO CORRESPONDING FIELDS OF TABLE gt_bkpf
      FOR ALL ENTRIES IN gt_bseg
      WHERE gjahr = gt_bseg-gjahr
      AND   bukrs = gt_bseg-bukrs
      AND   belnr = gt_bseg-belnr.
  ENDIF.

*查询总账科目描述
  SELECT *
    INTO TABLE gt_skat
    FROM skat
    WHERE spras = sy-langu
      AND ktopl = '1000'.

  SORT gt_skat BY saknr.
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
  SORT gt_jh BY ebeln .

  LOOP AT  gt_zfi017 INTO gs_zfi017.
*    GS_ZFI017-YJHJE = 0.
*    GS_ZFI017-YJHJE_1 = 0.

*    LOOP AT GT_EKBE INTO  GS_EKBE
*    WHERE EBELN = GS_ZFI017-EBELN
*    AND VGABE = '1'.
*      IF GS_EKBE-BWART = '101'.
*        GS_ZFI017-YJHJE   = GS_ZFI017-YJHJE   + GS_EKBE-DMBTR.
*        GS_ZFI017-YJHJE_1 = GS_ZFI017-YJHJE_1 + GS_EKBE-WRBTR.
*      ENDIF.
*      IF GS_EKBE-BWART = '122' OR GS_EKBE-BWART = '102'.
*        GS_ZFI017-YJHJE   = GS_ZFI017-YJHJE   - GS_EKBE-DMBTR.
*        GS_ZFI017-YJHJE_1 = GS_ZFI017-YJHJE_1 - GS_EKBE-WRBTR.
*      ENDIF.
*    ENDLOOP.
    MOVE-CORRESPONDING gs_zfi017 TO gs_data.
    READ TABLE gt_rbkp INTO gs_rbkp WITH KEY belnr = gs_data-belnr.
    IF sy-subrc = 0.
      gs_data-sgtxt = gs_rbkp-sgtxt.  "发票抬头文本 "IT02 150709 添加 发票文本
    ENDIF .
"已交货金额调整夏俊、黄键杭
*已交货金额
    CLEAR:GS_DATA-YJHJE,GS_DATA-YJHJE_1.
    LOOP AT GT_EKBE INTO GS_EKBE
    WHERE EBELN = GS_DATA-EBELN
    AND   VGABE = '1'.
      IF GS_EKBE-SHKZG = 'S'.
        GS_DATA-YJHJE   = GS_DATA-YJHJE   + GS_EKBE-DMBTR.
        GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 + GS_EKBE-WRBTR.
      ENDIF.

      IF GS_EKBE-SHKZG = 'H' .
        GS_DATA-YJHJE   = GS_DATA-YJHJE   - GS_EKBE-DMBTR.
        GS_DATA-YJHJE_1 = GS_DATA-YJHJE_1 - GS_EKBE-WRBTR.
      ENDIF.
    ENDLOOP.
**初始化凭证
*    GS_DATA-BELNR_F = ''.
*    GS_DATA-GJAHR_F = ''.

*查询清帐凭证
*    LOOP AT GT_BSEG INTO GS_BSEG
*    WHERE BUKRS = GS_DATA-BUKRS
*    AND   LIFNR = GS_DATA-LIFNR
*    AND   ZUONR = GS_DATA-ZSQD
*    AND   BSCHL = '21'.
*
**排除冲销凭证
*      READ TABLE GT_BKPF INTO GS_BKPF
*      WITH KEY GJAHR = GS_BSEG-GJAHR
*               BUKRS = GS_BSEG-BUKRS
*               BELNR = GS_BSEG-BELNR.
*      IF SY-SUBRC = 0.
*        IF GS_BKPF-STBLG IS INITIAL.
*          GS_DATA-BELNR_F = GS_BSEG-BELNR.
*          GS_DATA-GJAHR_F = GS_BSEG-GJAHR.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*当初始状态，审批人为空
    IF gs_data-zcljd = '1'.
      gs_data-zname1 = ''.
    ENDIF.

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

*按照申请单号进行排序
  SORT gt_data BY zsqd belnr ebeln.
  "150601 BEGIN 补充漏掉计算 已付款金额的数据
  SORT gt_zfi017 BY zsqd ebeln.
*  LOOP AT GT_ZFI017 INTO GS_ZFI017 WHERE ZCLJD = '3'.
*     clear GS_SUMYF .
*     GS_SUMYF-ZSQD = GS_ZFI017-ZSQD .
*     GS_SUMYF-EBELN = GS_ZFI017-EBELN.
*     GS_SUMYF-YFJE = GS_ZFI017-ZSQFKJE.
*     COLLECT GS_SUMYF INTO GT_SUMYF.
*  ENDLOOP.
*  LOOP AT GT_DATA INTO GS_DATA WHERE ZCLJD = '3' .
*     READ TABLE GT_SUMYF INTO GS_SUMYF WITH KEY ZSQD = GS_DATA-ZSQD EBELN = GS_DATA-EBELN .
*     IF SY-SUBRC = 0.
*       GS_DATA-YFJE = GS_SUMYF-YFJE .
*       MODIFY GT_DATA FROM GS_DATA.
*      ENDIF.
*
*   ENDLOOP.
                                                            "150601 END
**更新自建表
*  MOVE-CORRESPONDING GT_DATA TO GT_ZFI017.
*  MODIFY ZFI017 FROM TABLE GT_ZFI017.
*  REFRESH GT_ZFI017.
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

*是否含税
    READ TABLE gt_ekpo1 INTO gs_ekpo1
    WITH KEY ebeln = gs_data-ebeln.
    IF sy-subrc = 0.
      gs_data-mwskz = gs_ekpo1-mwskz.
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
    LOOP AT  gt_zfi017 INTO gs_zfi017
    WHERE ebeln = gs_data-ebeln
    AND   zch   = gs_data-zch
    AND   zcljd = '3'.
      " AND         BELNR = ''.
      "  L_YFJE = L_YFJE + GS_ZFI017-YFJE. 150527 YEFE 字段值为空 才注释
      l_yfje = l_yfje + gs_zfi017-zsqfkje.  "150527 YEFE 字段值为空 才注释
    ENDLOOP.
    gs_data-yfje = l_yfje.
*已付款比例
    IF gs_data-yfje1 IS NOT INITIAL.
      gs_data-yfbl = gs_data-yfje / gs_data-yfje1 * 100.
    ENDIF.
*已申请金额
    CLEAR l_ysqje.
    LOOP AT gt_zfi017 INTO gs_zfi017
     WHERE ebeln = gs_data-ebeln
     AND  ebeln = gs_data-ebeln
     AND ( zcljd = '3'
     OR   zcljd  = '2'
     OR   zcljd  = '1')
     AND   zch   = gs_data-zch
     AND   statu    <> icon_delete.
      l_ysqje = l_ysqje + gs_zfi017-zsqfkje.
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
  init_fieldcat 'FPJE'           '发票金额（净值）'   '' '' '' '' '' '' ''.
  init_fieldcat 'FPJE_1'           '发票金额（含税）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YZFP'           '预制发票（净值）'   '' '' '' '' '' '' ''.
  init_fieldcat 'YZFP_1'           '预制发票（含税）'   '' '' '' '' '' '' ''.
  init_fieldcat 'JHJE_1'        '交货金额(含税)'   '' '' '' '' '' '' ''.
  init_fieldcat 'WFJE'           '未付金额'   '' '' '' '' '' '' ''.
  init_fieldcat 'ZFKXZ'          '付款性质'   '' '' '' '' ''  'ZFI017' 'ZFKXZ'.
  init_fieldcat 'ZSQFKJE'        '申请付款金额'   '' '' '' '' '' 'ZFI017' 'ZSQFKJE'.
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
  DATA l_check TYPE c.


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

*审批通过
    WHEN '&OK'.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT gt_data INTO gs_data
       WHERE zbox = 'X'.
        LOOP AT gt_data INTO gs_data
         WHERE belnr = gs_data-belnr
         AND   gjahr = gs_data-gjahr
         AND   bukrs = gs_data-bukrs
         AND   zsqd  = gs_data-zsqd.

          gs_data-zbox = 'X'.
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
      ENDLOOP.

      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0.

*提示对话框
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            text_question  = '是否执行审批通过'
          IMPORTING
            answer         = g_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF g_answer <> '1'.
          EXIT.
        ENDIF.

        LOOP AT gt_data INTO gs_data
         WHERE zbox = 'X'.

*检查处理状态
          IF gs_data-zcljd <> '1'.
            MESSAGE s006(zfico01) DISPLAY LIKE 'E'.
            REFRESH gt_zfi017_1.
            EXIT.
          ENDIF.

*检查锁对象
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              zsqd           = gs_data-zsqd
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            CLEAR l_string.
            CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
            MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          gs_data-zcljd = '2'. "同意，未付款
          gs_data-zname1 = sy-uname. "审批人
          gs_data-statu = icon_green_light.
          MODIFY gt_data FROM gs_data.
          MOVE-CORRESPONDING gs_data TO gs_zfi017_1.
          APPEND gs_zfi017_1 TO gt_zfi017_1.
          CLEAR gs_data.
        ENDLOOP.

        MODIFY  zfi017 FROM TABLE gt_zfi017_1.
        REFRESH gt_zfi017_1.
        CLEAR gs_data.
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
      ENDIF.

*付款过账
    WHEN '&POST'.
*增加付款过账功能  ADD BY HANDWY 2015-3-23
      DATA lt_data TYPE TABLE OF ty_data.
      DATA ls_data TYPE ty_data.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT gt_data INTO gs_data
       WHERE zbox = 'X'.
        LOOP AT gt_data INTO gs_data
         WHERE belnr = gs_data-belnr
         AND   gjahr = gs_data-gjahr
         AND   bukrs = gs_data-bukrs
         AND   zsqd  = gs_data-zsqd.

          gs_data-zbox = 'X'.
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
      ENDLOOP.

      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0.

*检查处理状态
        CLEAR l_check.
        REFRESH lt_data.

        LOOP AT gt_data INTO gs_data
         WHERE zbox = 'X'.
          IF gs_data-zcljd <> '2'.
            MESSAGE s007(zfico01) DISPLAY LIKE 'E'.
            l_check = 'X'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              zsqd           = gs_data-zsqd
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            l_check = 'X'.
            CLEAR l_string.
            CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
            MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*记录选中的行
          MOVE-CORRESPONDING gs_data TO ls_data.
          APPEND ls_data TO lt_data.
          CLEAR ls_data.
        ENDLOOP.
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
        l_check = 'X'.
      ENDIF.

*删除基于发票提的付款申请的行项目
      DELETE lt_data WHERE belnr IS NOT INITIAL
                     AND   ebeln IS NOT INITIAL.

*当检查无误后，(初始化数据)进入过账界面
      IF l_check <> 'X'.
        PERFORM init_alv_9000 TABLES lt_data.
        CALL SCREEN 9000.
      ENDIF.

*冲销
    WHEN '&REV'.
      CLEAR g_answer.
      break handwy.
*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT gt_data INTO gs_data
       WHERE zbox = 'X'.
        LOOP AT gt_data INTO gs_data
         WHERE belnr = gs_data-belnr
         AND   gjahr = gs_data-gjahr
         AND   bukrs = gs_data-bukrs
         AND   zsqd  = gs_data-zsqd.

          gs_data-zbox = 'X'.
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
      ENDLOOP.

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

      PERFORM frm_acc_reversal.

*审批拒绝
    WHEN '&REJECT'.

*财务审批的时候，只针对发票进行审批，采购订单状态会自动改变
      LOOP AT gt_data INTO gs_data
       WHERE zbox = 'X'.
        LOOP AT gt_data INTO gs_data
         WHERE belnr = gs_data-belnr
         AND   gjahr = gs_data-gjahr
         AND   bukrs = gs_data-bukrs
         AND   zsqd  = gs_data-zsqd.

          gs_data-zbox = 'X'.
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
      ENDLOOP.

      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0.

*弹出框提示
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            text_question  = '是否执行审批拒绝'
          IMPORTING
            answer         = g_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF g_answer <> '1'.
          EXIT.
        ENDIF.

        LOOP AT gt_data INTO gs_data
        WHERE zbox = 'X'.

*检查处理状态
          IF gs_data-zcljd <> '1'.
            MESSAGE s006(zfico01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              zsqd           = gs_data-zsqd
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            CLEAR l_string.
            CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
            MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          gs_data-zcljd  = '4'. "同意，未付款
          gs_data-statu  = icon_red_light.
          gs_data-zname1 = sy-uname. "审批人
          MODIFY gt_data FROM gs_data.
          MOVE-CORRESPONDING gs_data TO gs_zfi017_1.
          APPEND gs_zfi017_1 TO gt_zfi017_1.
          CLEAR gs_data.
        ENDLOOP.

        MODIFY  zfi017 FROM TABLE gt_zfi017_1.
        REFRESH gt_zfi017_1.
        CLEAR gs_data.
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
      ENDIF.


    WHEN '&BACK'.
      LOOP AT gt_data INTO gs_data
        WHERE zbox = 'X'.
        LOOP AT gt_data INTO gs_data
         WHERE belnr = gs_data-belnr
         AND   gjahr = gs_data-gjahr
         AND   bukrs = gs_data-bukrs
         AND   zsqd  = gs_data-zsqd.

          gs_data-zbox = 'X'.
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
      ENDLOOP.

      READ TABLE gt_data INTO gs_data
      WITH KEY zbox = 'X'.
      IF sy-subrc = 0.

*弹出框提示
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR       = ' '
*           DIAGNOSE_OBJECT             = ' '
            text_question  = '是否执反行审批操作'
          IMPORTING
            answer         = g_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF g_answer <> '1'.
          EXIT.
        ENDIF.

        LOOP AT gt_data INTO gs_data
        WHERE zbox = 'X'.

*检查处理状态
          IF gs_data-zcljd = '1'.
            MESSAGE s013(zfico01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          IF gs_data-zcljd = '3'.
            MESSAGE s014(zfico01) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

*锁对象检查，加锁
          CALL FUNCTION 'ENQUEUE_EZFI006'
            EXPORTING
*             MODE_ZFI006    = 'E'
*             MANDT          = SY-MANDT
              zsqd           = gs_data-zsqd
*             X_ZSQD         = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            CLEAR l_string.
            CONCATENATE '订单' gs_data-zsqd '已被其他人锁定' INTO l_string.
            MESSAGE l_string TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          gs_data-zcljd  = '1'. "同意，未付款
          gs_data-statu  = icon_yellow_light.
          gs_data-zname1 = ''. "审批人
          MODIFY gt_data FROM gs_data.
          MOVE-CORRESPONDING gs_data TO gs_zfi017_1.
          APPEND gs_zfi017_1 TO gt_zfi017_1.
          CLEAR gs_data.
        ENDLOOP.

        MODIFY  zfi017 FROM TABLE gt_zfi017_1.
        REFRESH gt_zfi017_1.
        CLEAR gs_data.
      ELSE.
        MESSAGE s003(z001) DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&F03' OR '&F15' OR  '&F12'.
      PERFORM frm_unlock.
  ENDCASE.

  CALL METHOD g_ref_grid->refresh_table_display.
ENDFORM.
"ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_9000 TABLES lt_data.
  DATA ls_data TYPE  ty_data.

*初始化抬头

  READ TABLE lt_data INTO ls_data INDEX 1.
  gs_head-bldat = sy-datum.       "凭证日期
  gs_head-budat = sy-datum.       "过账日期
  gs_head-bukrs = ls_data-bukrs.  "公司代码
  gs_head-waers = ls_data-waers_2."货币
  gs_head-bktxt = ls_data-htbh.   "合同编号


  gs_head-blart = 'SA'.

*初始化行项目
  REFRESH gt_item.
  CLEAR g_wrbtr.
  LOOP AT lt_data INTO ls_data.
    gs_item-buzei = sy-tabix.
    gs_item-bschl = '21'.

*&--代码注释 BY HANDYBY 23.06.2017 11:00:02  BEGIN
* *付款性质决定科目
*    IF ls_data-zfkxz = '1'.     "预付
*      gs_item-hkont = '2202010101'.
*    ELSEIF ls_data-zfkxz = '2'. "应付
*      gs_item-hkont = '1123010101'.
*    ENDIF.
*&--代码注释 BY HANDYBY 23.06.2017 11:00:02  END
*&--代码添加 BY HANDYBY 23.06.2017 10:59:35  BEGIN
*付款性质决定科目
    "所有付款性质1、2 的总账科目默认值都是从供应商设置的统御科目
    IF ls_data-zfkxz = '1' OR  ls_data-zfkxz = '2'.     "应付 "预付

      SELECT SINGLE akont INTO gs_item-hkont
           FROM lfb1
           WHERE lifnr = ls_data-lifnr
            AND  bukrs = ls_data-bukrs .

    ENDIF.
*&--代码添加 BY HANDYBY 23.06.2017 10:59:35  END

*获取科目描述
    READ TABLE gt_skat INTO gs_skat
    WITH KEY saknr = gs_item-hkont BINARY SEARCH.
    IF sy-subrc = 0.
      gs_item-hktxt = gs_skat-txt20.
    ENDIF.

*供应商
    gs_item-lifnr   = ls_data-lifn2.

*金额
    gs_item-wrbtr   = ls_data-zsqfkje.

*货币
    gs_item-pswsl   = ls_data-waers_2.

*统计借方的总金额
    g_wrbtr  = g_wrbtr + gs_item-wrbtr.

*付款单号
    gs_item-zuonr_1 = ls_data-zsqd.

**付款申号
*    GS_HEAD-BKTXT   = LS_DATA-ZSQD.

*行项目摘要
    " GS_ITEM-SGTXT   = LS_DATA-ZZY.
    gs_item-sgtxt = ls_data-htbh.

*WBS元素
    gs_item-pspid   = ls_data-posid.

*设置不可以编辑状态
    PERFORM frm_cell_style USING 'BSCHL'
                           ''
                           CHANGING gs_item-cellstyle.
*    PERFORM FRM_CELL_STYLE USING 'HKONT'
*                           ''
*                           CHANGING GS_ITEM-CELLSTYLE.
    PERFORM frm_cell_style USING 'LIFNR'
                           ''
                           CHANGING gs_item-cellstyle.
    PERFORM frm_cell_style USING 'KUNNR'
                           ''
                           CHANGING gs_item-cellstyle.
    PERFORM frm_cell_style USING 'WRBTR'
                           ''
                           CHANGING gs_item-cellstyle.
    PERFORM frm_cell_style USING 'RSTGR'
                           ''
                           CHANGING gs_item-cellstyle.
    PERFORM frm_cell_style USING 'ZUONR_1'
                           ''
                          CHANGING gs_item-cellstyle.
*    PERFORM FRM_CELL_STYLE USING 'SGTXT'
*                           ''
*                          CHANGING GS_ITEM-CELLSTYLE.
    PERFORM frm_cell_style USING 'PSPID'
                           ''
                           CHANGING gs_item-cellstyle.

    APPEND gs_item TO gt_item.
    CLEAR gs_item.
  ENDLOOP.

ENDFORM.
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
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATU_9000'.
  SET TITLEBAR '采购付款记账'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPALY_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_dispaly_9000 OUTPUT.
  IF gr_container IS INITIAL.
    PERFORM frm_create_container.
    PERFORM frm_alv_display.
  ELSE.
    PERFORM frm_refresh_alv.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA: l_subrc TYPE sy-subrc.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'BACK'.
      PERFORM frm_check_changed CHANGING l_subrc.
      IF l_subrc = 0.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXIT'.
      PERFORM frm_check_changed CHANGING l_subrc.
      IF l_subrc = 0.
        LEAVE PROGRAM.
      ENDIF.
    WHEN 'POST'.
      PERFORM frm_post_accdoc.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_container .
  CREATE OBJECT gr_container
    EXPORTING
      container_name = 'CONTAINER'
*     lifetime       = cl_gui_custom_container=>lifetime_dynpro
    .

  CREATE OBJECT gr_alvgrid
    EXPORTING
      i_appl_events = 'X'
      i_parent      = gr_container.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_display .
  PERFORM init_oo_layout.              "设置输出格式
  PERFORM init_sort_1.                "设置排序、合计
*  PERFORM INIT_VARIANT USING '0001'. "设置变式控制
  PERFORM frm_init_lvc_1.             " 初始化内表结构/ALV显示结构
  PERFORM exclude_tb_functions TABLES gt_oo_exclude.
  PERFORM frm_oo_build_event.
  PERFORM frm_oo_output TABLES gt_lvc              "输出
                               gt_sort
                               gt_oo_exclude
                               gt_item
                        USING gw_layout
                              gw_variant.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_OO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_oo_layout .
  gw_layout-zebra = 'X'.
*  GW_LAYOUT-BOX_FNAME = 'BOX'.
*  gw_layout-cwidth_opt  = 'X'.
  gw_layout-sel_mode = 'A'.
  gw_layout-edit_mode = 'X'.
*  gw_layout-ctab_fname = 'CELLCOLOR'.
  gw_layout-stylefname = 'CELLSTYLE'.
*  gw_layout-info_fname = 'LINECOLOR'.
*  GW_LAYOUT-F2CODE = '&ETA'.
*  GW_LAYOUT-INFO_FIELDNAME = 'LINE_COLOR'.
*  GW_LAYOUT-TOTALS_ONLY  = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_sort_1 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_init_lvc_1 .

  REFRESH gt_lvc.
  init_fieldcat 'BUZEI'        '项目号'   8 '' '' '' '' 'BSEG' 'BUZEI'.
  init_fieldcat 'BSCHL'        '记账码'   8 '' '' 'X' '' 'BSEG' 'BSCHL'.
  init_fieldcat 'HKONT'        '科目'   '' '' '' 'X' ''  '' ''.
  init_fieldcat 'HKTXT'        '科目描述'   '' '' '' '' '' 'SKAT' 'TXT20'.
  init_fieldcat 'LIFNR'        '供应商'   '' '' '' 'X' '' 'BSEG' 'LIFNR'.
  init_fieldcat 'KUNNR'        '客户'   '' '' '' 'X' '' 'BSEG' 'KUNNR'.
  init_fieldcat 'WRBTR'        '金额'   '' '' '' 'X' '' 'BSEG' 'WRBTR'.
  init_fieldcat 'PSWSL'        '货币'   '' '' '' '' '' 'BSEG' 'PSWSL'.
  init_fieldcat 'RSTGR'        '原因代码'   8 '' '' 'X' '' '' ''.
  init_fieldcat 'ZUONR'        '汇票号'   '' '' '' '' '' 'BSEG' 'ZUONR'.
  init_fieldcat 'ZUONR_1'      '付款申请单号'   15 '' '' 'X' '' 'BSEG' 'ZUONR'.
  init_fieldcat 'SGTXT'        '行项目摘要'   '' '' '' 'X' '' 'BSEG' 'SGTXT'.
  init_fieldcat 'PSPID'        '项目WBS编号'   '' '' '' 'X' '' 'PROJ' 'PSPID'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       弃掉标准按钮（增加、插入）
*----------------------------------------------------------------------*
*      -->P_GT_EXCLUDE  text
*      -->P_ELSE  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions TABLES pt_exclude TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_oo_build_event .

  DATA lr_event_handler TYPE REF TO lcl_event_handler.
  DATA lt_f4 TYPE lvc_t_f4.
  DATA ls_f4 TYPE lvc_s_f4.

  CREATE OBJECT lr_event_handler.
  SET HANDLER : lr_event_handler->handle_toolbar               FOR gr_alvgrid,
                lr_event_handler->handle_before_user_command   FOR gr_alvgrid,
                lr_event_handler->handle_user_command          FOR gr_alvgrid,
                lr_event_handler->handle_after_user_command    FOR gr_alvgrid,
                lr_event_handler->handle_onf4                  FOR gr_alvgrid,
                lr_event_handler->handle_data_changed          FOR gr_alvgrid,
                lr_event_handler->handle_data_changed_finished FOR gr_alvgrid,
                lr_event_handler->handle_double_click          FOR gr_alvgrid,
                lr_event_handler->handle_button_click          FOR gr_alvgrid.

*F4对应的栏位
  ls_f4-fieldname = 'HKONT'.
  ls_f4-register = 'X'.
*  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

*F4对应的栏位
  ls_f4-fieldname = 'RSTGR'.
  ls_f4-register = 'X'.
*  ls_f4-getbefore = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  CALL METHOD gr_alvgrid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  CALL METHOD gr_alvgrid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OO_OUTPUT
*&---------------------------------------------------------------------*
*       调用ALV函数
*----------------------------------------------------------------------*
FORM frm_oo_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_exclude TYPE ui_functions
                       pt_data
                USING pw_layout TYPE lvc_s_layo
                      pw_variant TYPE disvariant.

  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*     i_buffer_active               =
*     i_bypassing_buffer            =
*     i_consistency_check           =
*     i_structure_name              =
      is_variant                    = pw_variant
      i_save                        = 'A'
*     i_default                     = 'X'
      is_layout                     = pw_layout
*     is_print                      =
*     it_special_groups             =
      it_toolbar_excluding          = pt_exclude[]
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      it_outtab                     = pt_data[]
      it_fieldcatalog               = pt_lvc[]
      it_sort                       = pt_sort[]
*     it_filter                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.                    " FRM_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_refresh_alv .
  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  CALL METHOD gr_alvgrid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     i_soft_refresh =
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_changed  CHANGING p_subrc TYPE sy-subrc.
  p_subrc = 0.
  IF g_changed = abap_true.
    DATA l_ans.
    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
*       DEFAULTOPTION       = 'Y'
        textline1 = text-m01
        textline2 = text-m02
        titel     = text-m03
*       START_COLUMN        = 25
*       START_ROW = 6
      IMPORTING
        answer    = l_ans.
    IF l_ans = 'N'.
      p_subrc = 2.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_ACCDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_post_accdoc .
  DATA l_ans.
  DATA l_subrc TYPE sy-subrc.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      text_question         = '确定要保存并过账吗？'
      text_button_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      text_button_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      answer                = l_ans
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK l_ans EQ '1'.

  PERFORM frm_check_doc CHANGING l_subrc.

  CHECK l_subrc EQ 0.

*付款记账数据准备
  PERFORM frm_bapi_data_prep .

*调用记账BAPI
  PERFORM frm_call_bapi CHANGING gs_head-belnr gs_head-gjahr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM handle_toolbar  USING    p_e_object TYPE REF TO cl_alv_event_toolbar_set
                                                     p_e_interactive.
  DATA: lw_toolbar LIKE LINE OF p_e_object->mt_toolbar.

  READ TABLE p_e_object->mt_toolbar INTO lw_toolbar
  WITH KEY function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  IF sy-subrc = 0.
*加入新增行按钮
    CLEAR lw_toolbar.
    lw_toolbar-function    = 'APPEND_ROW '.
*    LS_TOOLBAR-FUNCTION    = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    lw_toolbar-quickinfo   = '新增行'.
    lw_toolbar-icon        = icon_create.
    lw_toolbar-disabled    = space.
    INSERT lw_toolbar INTO p_e_object->mt_toolbar INDEX sy-tabix.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_user_command  USING    p_ucomm.
  save_ok = p_ucomm.
  CLEAR p_ucomm.

  CASE save_ok.
    WHEN 'APPEND_ROW'.
      PERFORM frm_append_row.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM handle_data_changed_finished  USING    p_e_modified
                                            p_et_good_cells TYPE  lvc_t_modi.
  DATA: lw_cell TYPE lvc_s_modi.

  " 输入记账码时，更改相应字段的可输入属性
  READ TABLE p_et_good_cells INTO lw_cell
  WITH KEY fieldname = 'BSCHL'.
  IF sy-subrc = 0.
    READ TABLE gt_item ASSIGNING <fs_item> INDEX lw_cell-row_id.
    IF sy-subrc = 0.
*记账为50 ,现金,银行科目,
      IF lw_cell-value = '50'.
        PERFORM frm_cell_style USING 'LIFNR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'KUNNR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR'
                               ''
                            CHANGING gs_item-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR_1'
                               ''
                             CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'RSTGR'
                              'X'
                            CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'PSPID'
                            'X'
                          CHANGING <fs_item>-cellstyle.

*记账码31,应付票据
      ELSEIF lw_cell-value = '31'.
        PERFORM frm_cell_style USING 'KUNNR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'RSTGR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR_1'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'LIFNR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'PSPID'
                                  'X'
                                CHANGING <fs_item>-cellstyle.
*记账码为11,应收票据背书
      ELSEIF lw_cell-value = '11'.
        PERFORM frm_cell_style USING 'LIFNR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'RSTGR'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'PSWSL'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR_1'
                               ''
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'KUNNR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'ZUONR'
                               'X'
                               CHANGING <fs_item>-cellstyle.
        PERFORM frm_cell_style USING 'PSPID'
                          'X'
                        CHANGING <fs_item>-cellstyle.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM frm_refresh_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_APPEND_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_append_row .
  DATA: l_index TYPE i.
  DATA: l_row_id TYPE lvc_s_row,
        l_col_id TYPE lvc_s_col,
        l_row_no TYPE lvc_s_roid.

  DATA: l_buzei TYPE bseg-buzei.
  l_buzei = 1.
  DO.
    READ TABLE gt_item TRANSPORTING NO FIELDS WITH KEY buzei = l_buzei.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    l_buzei = l_buzei + 1.
  ENDDO.

  CLEAR gs_item.
  gs_item-buzei = l_buzei.

*贷方金额
  gs_item-wrbtr = g_wrbtr.

*贷方货币
  gs_item-pswsl = gs_head-waers.

  APPEND gs_item TO gt_item.
  CLEAR g_wrbtr.
  CLEAR gs_item.

  PERFORM frm_refresh_alv.

  " 设置光标在过账码
  l_index = lines( gt_item ).

  l_row_id = l_index.
  l_col_id = 'BSCHL'.
  l_row_no-row_id = l_index.

  CALL METHOD gr_alvgrid->set_current_cell_via_id
    EXPORTING
      is_row_id    = l_row_id
      is_column_id = l_col_id
      is_row_no    = l_row_no.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_cell_style
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>_CELLSTYLE  text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM frm_cell_style  USING    p_fieldname
                              p_editable
                     CHANGING pt_cellstyle TYPE lvc_t_styl.
  DATA: lw_cellstyle TYPE lvc_s_styl.

  READ TABLE pt_cellstyle INTO lw_cellstyle WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    MODIFY TABLE pt_cellstyle FROM lw_cellstyle.
  ELSE.
    lw_cellstyle-fieldname = p_fieldname.
    IF p_editable = 'X'.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lw_cellstyle-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    INSERT lw_cellstyle INTO TABLE pt_cellstyle.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'MOD'.
*      IF G_EDIT_MOD = GC_EDITABLE.
*        SCREEN-INPUT = 1.
*      ELSE.
*        SCREEN-INPUT = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

*  DATA L_ALV_EDIT TYPE INT4.
*  L_ALV_EDIT = GR_ALVGRID->IS_READY_FOR_INPUT( ).
*  IF ( L_ALV_EDIT = 1 AND G_EDIT_MOD NE GC_EDITABLE )
*    OR ( L_ALV_EDIT = 0 AND G_EDIT_MOD NE GC_READONLY ).
*    L_ALV_EDIT = 1 - L_ALV_EDIT.
*    PERFORM FRM_CHANGE_EDIT_MODE USING L_ALV_EDIT.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ALV_EDIT  text
*----------------------------------------------------------------------*
FORM frm_change_edit_mode  USING    p_alv_edit TYPE int4.
  CALL METHOD gr_alvgrid->set_ready_for_input
    EXPORTING
      i_ready_for_input = p_alv_edit.

  "切换模式后刷新ALV
  PERFORM frm_refresh_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM frm_check_doc  CHANGING p_subrc.
  " 凭证日期
  IF gs_head-bldat IS INITIAL.
    MESSAGE text-m04 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF gs_head-budat IS INITIAL.
    MESSAGE text-m05 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF gs_head-bukrs IS INITIAL.
    MESSAGE text-m06 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 货币
  IF gs_head-waers IS INITIAL.
    MESSAGE text-m07 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  " 行项目
  IF gt_item IS INITIAL.
    MESSAGE text-m09 TYPE 'W' DISPLAY LIKE 'E'.
    p_subrc = 8.
    EXIT.
  ENDIF.

  IF gs_head-blart IS INITIAL.
    MESSAGE '请输入凭证类型！' TYPE 'W' DISPLAY LIKE 'E'. "凭证类型
    p_subrc = 8.
    EXIT.
  ENDIF.

  LOOP AT gt_item INTO gs_item.
    " 记账码
    IF  gs_item-bschl NE '21' AND gs_item-bschl NE '50'
    AND gs_item-bschl NE '31'  AND gs_item-bschl NE '11'.
      MESSAGE text-m10 TYPE 'W' DISPLAY LIKE 'E'.
      p_subrc = 8.
      EXIT.
    ENDIF.

    " 科目
    IF gs_item-hkont IS INITIAL.
      MESSAGE text-m12 TYPE 'W' DISPLAY LIKE 'E'.
      p_subrc = 8.
      EXIT.
    ENDIF.

    "记账码为50
    IF gs_item-bschl EQ '50'
      AND gs_item-rstgr IS INITIAL.
      MESSAGE text-m11 TYPE 'W' DISPLAY LIKE 'E'.
    ENDIF.

    "记账码为31
    IF gs_item-bschl EQ '31'
      AND gs_item-lifnr IS INITIAL.
      MESSAGE text-m14 TYPE 'W' DISPLAY LIKE 'E'.
    ENDIF.

    "记账码为11
    IF gs_item-bschl EQ '11'.
      IF gs_item-kunnr IS INITIAL.
        MESSAGE text-m15 TYPE 'W' DISPLAY LIKE 'E'.
      ENDIF.
      IF gs_item-zuonr IS INITIAL.
        MESSAGE text-m16 TYPE 'W' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_data_prep .
*清空BAPI变量
  PERFORM frm_bapi_data_clear.

  DATA: l_katyp TYPE cskb-katyp.

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  wa_documentheader-doc_date     =  gs_head-bldat.
*过账日期
  wa_documentheader-pstng_date   =  gs_head-budat.
*凭证类型
  wa_documentheader-doc_type     =  gs_head-blart.
*公司代码
  wa_documentheader-comp_code    =  gs_head-bukrs.
*凭证抬头文本
  wa_documentheader-header_txt   =  gs_head-bktxt.
*创建人员
  wa_documentheader-username     =  sy-uname.

**********************************************************************
* 凭证行
**********************************************************************
  LOOP AT gt_item INTO gs_item.
*当记账码是21
    IF gs_item-bschl = '21' .
      CLEAR wa_accountpayable.
*       行项目号
      wa_accountpayable-itemno_acc  = gs_item-buzei.
*       项目文本
      wa_accountpayable-item_text   = gs_item-sgtxt.
*       供应商编码
      wa_accountpayable-vendor_no   = gs_item-lifnr.
*       根据付款性质确定总账科目
      wa_accountpayable-gl_account  = gs_item-hkont.
*       分配号 = 付款申请单号 （WBS元素）
      wa_accountpayable-alloc_nmbr  = gs_item-pspid.

      APPEND wa_accountpayable TO it_accountpayable.
      CLEAR wa_accountpayable.
    ENDIF.

    CLEAR wa_currencyamount.
    wa_currencyamount-itemno_acc = gs_item-buzei.
*       货币
    wa_currencyamount-currency   = gs_item-pswsl.

*当记账码为50，*当记账码为31，*当记账码为11
    IF gs_item-bschl = '50'
      OR gs_item-bschl = '31'
      OR gs_item-bschl = '11'.
      wa_currencyamount-amt_doccur = gs_item-wrbtr * -1.
    ENDIF.

*当记账码为21
    IF gs_item-bschl = '21'.
      wa_currencyamount-amt_doccur = gs_item-wrbtr.
    ENDIF.

    APPEND wa_currencyamount TO it_currencyamount.
    CLEAR wa_currencyamount.
    CLEAR wa_accountgl.

*应收票据
    IF gs_item-bschl = '11' OR  gs_item-bschl = '31'.
      wa_accountgl-customer   = gs_item-kunnr.
*      WA_ACCOUNTGL-ALLOC_NMBR = GS_ITEM-ZUONR. "汇票号
      wa_accountgl-alloc_nmbr = gs_item-pspid. "汇票号
    ENDIF.

*记账码50，写进WBS元素字段
    IF gs_item-bschl = '50'.
      wa_accountgl-wbs_element  = gs_item-pspid.
    ENDIF.

*应付票据
    IF gs_item-bschl = '31'.
      wa_accountgl-vendor_no  = gs_item-lifnr.
    ENDIF.

*总帐科目项
    IF gs_item-bschl = '50'
      OR gs_item-bschl = '31'
      OR gs_item-bschl = '11'.
      wa_accountgl-itemno_acc       = gs_item-buzei.
      wa_accountgl-gl_account       = gs_item-hkont.    "记账科目
      wa_accountgl-item_text        = gs_item-sgtxt.    "摘要
      APPEND wa_accountgl TO it_accountgl.
      CLEAR wa_accountgl.
    ENDIF.

* 记账码 & 付款原因
    CLEAR wa_extension2.
    CLEAR wa_zaccdocuext.
    wa_zaccdocuext-posnr = gs_item-buzei."行项目
    wa_zaccdocuext-bschl = gs_item-bschl."记账码
    wa_zaccdocuext-rstgr = gs_item-rstgr."付款原因

    wa_extension2-structure  = 'ZACCDOCUEXT'.
    wa_extension2-valuepart1 = wa_zaccdocuext.
    APPEND wa_extension2 TO it_extension2.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_HEAD_BELNR  text
*      <--P_GS_HEAD_GJAHR  text
*----------------------------------------------------------------------*
FORM frm_call_bapi  CHANGING p_belnr TYPE belnr_d
                             p_gjahr TYPE gjahr.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = wa_documentheader
*     CUSTOMERCPD    =
*     CONTRACTHEADER =
    IMPORTING
      obj_type       = wa_obj-obj_type
      obj_key        = wa_obj-obj_key
      obj_sys        = wa_obj-obj_sys
    TABLES
      accountgl      = it_accountgl
*     ACCOUNTRECEIVABLE = IT_ACCOUNTRECEIVABLE
      accountpayable = it_accountpayable
*     ACCOUNTTAX     =
      currencyamount = it_currencyamount
      criteria       = it_criteria
      valuefield     = it_valuefield
*     EXTENSION1     =
      return         = it_return
*     PAYMENTCARD    =
*     CONTRACTITEM   =
      extension2     = it_extension2
*     REALESTATE     =
*     ACCOUNTWT      =
    .

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM frm_message_display.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

* 回写生成的会计凭证号与会计凭证年度
    gs_head-belnr = wa_obj-obj_key(10).
    gs_head-gjahr = wa_obj-obj_key+14(4).

*根据过账的借方行项目，更新会计凭证和状态
    LOOP AT gt_item INTO gs_item.
      LOOP AT  gt_data INTO gs_data
      WHERE zsqd = gs_item-zuonr_1.
        gs_data-belnr_f = gs_head-belnr.
        gs_data-gjahr_f = gs_head-gjahr.
        gs_data-zcljd = '3'.
        gs_data-statu = icon_okay.
        gs_data-yfje = gs_data-yfje + gs_data-zsqfkje ."更新已付款金额数据 ：原已保存已付款数据 + 已记账成功的申请金额数据
        gs_data-wfje = gs_data-wfje - gs_data-yfje.
        MODIFY  gt_data FROM gs_data.
        CLEAR gs_data.
      ENDLOOP.
    ENDLOOP.

*更新自建表状态
    REFRESH gt_zfi017.
    MOVE-CORRESPONDING gt_data TO gt_zfi017.
    MODIFY zfi017 FROM TABLE gt_zfi017.

**更新会计凭证和状态
*    LOOP AT GT_DATA INTO GS_DATA
*     WHERE ZBOX = 'X'.
*      GS_DATA-BELNR = GS_HEAD-BELNR.
*      GS_DATA-GJAHR = GS_HEAD-GJAHR.
*      GS_DATA-ZCLJD = '3'.
*      GS_DATA-STATU = ICON_OKAY.
*      MODIFY GT_DATA FROM GS_DATA.
*      CLEAR GS_DATA.
*    ENDLOOP.

    MESSAGE s312(f5) WITH  gs_head-belnr s_bukrs-low.
    CLEAR gs_head.
    CLEAR gs_item.
    REFRESH gt_item.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_display .
  PERFORM frm_message_initial.

  LOOP AT it_return INTO wa_return.
*    call function 'MESSAGE_TEXT_BUILD'
*      exporting
*        msgid               = wa_return-id
*        msgnr               = wa_return-number
*        msgv1               = wa_return-message_v1
*        msgv2               = wa_return-message_v2
*        msgv3               = wa_return-message_v3
*        msgv4               = wa_return-message_v4
*      importing
*        message_text_output = g_msg.
*
*    write: / g_msg.

    "append all error warning messages to below function module
    PERFORM store_messages USING wa_return-id
                                 wa_return-type
                                 wa_return-message_v1
                                 wa_return-message_v2
                                 wa_return-message_v3
                                 wa_return-message_v4
                                 wa_return-number.
  ENDLOOP.

  PERFORM frm_message_show.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_initial .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      log_not_active       = 1
      wrong_identification = 2
      OTHERS               = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETURN_ID  text
*      -->P_WA_RETURN_TYPE  text
*      -->P_WA_RETURN_MESSAGE_V1  text
*      -->P_WA_RETURN_MESSAGE_V2  text
*      -->P_WA_RETURN_MESSAGE_V3  text
*      -->P_WA_RETURN_MESSAGE_V4  text
*      -->P_WA_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM store_messages USING p_msgid
                          p_msgty
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                          p_txtnr.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      arbgb                  = p_msgid
      msgty                  = p_msgty
      msgv1                  = p_msgv1
      msgv2                  = p_msgv2
      msgv3                  = p_msgv3
      msgv4                  = p_msgv4
      txtnr                  = p_txtnr
    EXCEPTIONS
      message_type_not_valid = 1
      not_active             = 2
      OTHERS                 = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_message_show .
  "at lsat call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA l_exit_command TYPE bal_s_excm.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      show_linno         = space
    IMPORTING
      e_exit_command     = l_exit_command
    EXCEPTIONS
      inconsistent_range = 1
      no_messages        = 2
      OTHERS             = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_bapi_data_clear .
  REFRESH: it_accountgl, it_accountpayable, it_currencyamount, it_criteria, it_valuefield, it_extension2, it_return.
  CLEAR: wa_documentheader, wa_obj.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING     p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                    p_e_onf4
                                    p_e_onf4_before
                                    p_e_onf4_after
                                    p_e_ucomm TYPE sy-ucomm.

  DATA:ls_mod_cell TYPE lvc_s_modi ,  "应用的修改的单元格
       lv_value    TYPE lvc_value ,   "单元格内容
       ls_stable   TYPE lvc_s_stbl.   "刷新稳定性

  DATA:ls_celltab TYPE lvc_s_styl,
       lt_celltab TYPE lvc_t_styl.

  SORT p_er_data_changed->mt_mod_cells BY row_id.

*ITEM
  LOOP AT p_er_data_changed->mt_mod_cells INTO ls_mod_cell.
    AT NEW row_id.
      READ TABLE gt_item INTO gs_item INDEX ls_mod_cell-row_id.
    ENDAT.

    CASE ls_mod_cell-fieldname.
      WHEN 'HKONT'.  "科目
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'HKONT'
          IMPORTING
            e_value     = gs_item-hkont.

*获取科目描述
        READ TABLE gt_skat INTO gs_skat
        WITH KEY saknr = gs_item-hkont BINARY SEARCH.
        IF sy-subrc = 0.
          gs_item-hktxt = gs_skat-txt20.
        ENDIF.

      WHEN 'RSTGR'.  "原因代码
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'RSTGR'
          IMPORTING
            e_value     = gs_item-rstgr.

      WHEN 'WRBTR'.  "金额
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'WRBTR'
          IMPORTING
            e_value     = gs_item-wrbtr.

      WHEN 'SGTXT'.  "摘要
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'RSTGR'
          IMPORTING
            e_value     = gs_item-sgtxt.

      WHEN 'KUNNR'.  "客户
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'KUNNR'
          IMPORTING
            e_value     = gs_item-kunnr.

      WHEN 'LIFNR'.  "供应商
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'LIFNR'
          IMPORTING
            e_value     = gs_item-lifnr.

      WHEN 'ZUONR'.  "汇票号
        "获取指定单元格改动后内容
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cell-row_id
            i_fieldname = 'ZUONR'
          IMPORTING
            e_value     = gs_item-zuonr.
    ENDCASE.

*刷新数据到ALV
    MODIFY gt_item FROM gs_item INDEX ls_mod_cell-row_id.
    CLEAR gs_item.
  ENDLOOP.

*输入检查：1. 记账码限制
  DATA: lt_mod_data TYPE lvc_t_modi,
        wa_mod_data TYPE lvc_s_modi.

  DATA: l_type TYPE c,
        l_msg  TYPE c LENGTH 100.

  lt_mod_data = p_er_data_changed->mt_mod_cells.

  LOOP AT lt_mod_data INTO wa_mod_data.
    IF wa_mod_data-fieldname = 'BSCHL'
      AND wa_mod_data-value IS NOT INITIAL.
      CLEAR: l_type, l_msg.
      IF wa_mod_data-value NE '21' AND wa_mod_data-value NE '11'
          AND wa_mod_data-value NE '31' AND wa_mod_data-value NE '50'.
        l_type = 'E'.
        l_msg = '记账码只能为''11''或''21''或 ''31''或''50'' '.
      ENDIF.

      IF l_type = 'E'.
        CALL METHOD p_er_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '00'
            i_msgty     = 'E'
            i_msgno     = '001'
            i_msgv1     = l_msg
            i_fieldname = wa_mod_data-fieldname
*           i_row_id    = wa_mod_data-row_id
          .
      ENDIF.
    ELSE.
*      call method p_er_data_changed->refresh_protocol.
*      clear p_e_ucomm.
      IF p_er_data_changed->mt_protocol IS NOT INITIAL.
        CALL METHOD p_er_data_changed->display_protocol
*        exporting
*          i_container        =
*          i_display_toolbar  =
*          i_optimize_columns =
          .
      ENDIF.
    ENDIF.
  ENDLOOP.            " handle_data_changed

  PERFORM frm_refresh_alv. "刷新
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM handle_onf4  USING    p_fieldname TYPE lvc_fname
                          ps_row_no TYPE lvc_s_roid
                          pr_event_data TYPE REF TO cl_alv_event_data
                          pt_bad_cells TYPE lvc_t_modi
                          p_display.
  FIELD-SYMBOLS <fs_mod_cells> TYPE lvc_t_modi.
  DATA: lw_mod_cell TYPE lvc_s_modi.
  DATA: gt_t053s TYPE TABLE OF t053s.
  DATA: lt_ddshretval TYPE STANDARD TABLE OF ddshretval,
        lw_ddshretval TYPE ddshretval.

  CASE p_fieldname.
*科目
    WHEN 'HKONT'.
      READ TABLE gt_item INTO gs_item INDEX ps_row_no-row_id.
      IF sy-subrc = 0.
        PERFORM sub_help_hkont CHANGING gs_item-hkont.
        IF gs_item-hkont IS NOT INITIAL.
          MODIFY gt_item FROM gs_item INDEX ps_row_no-row_id.
          " Trigger data changed event
          ASSIGN pr_event_data->m_data->* TO <fs_mod_cells>.
          lw_mod_cell-row_id     = ps_row_no-row_id.
          lw_mod_cell-sub_row_id = ps_row_no-sub_row_id.
          lw_mod_cell-fieldname  = 'HKONT'.
          lw_mod_cell-value      = gs_item-hkont.
          APPEND lw_mod_cell TO <fs_mod_cells>.
        ENDIF.
      ENDIF.

*原因代码
    WHEN 'RSTGR'.
      REFRESH gt_t053s.

*根据公司代码取付款原因
      SELECT * FROM t053s
        INTO CORRESPONDING FIELDS OF TABLE gt_t053s
        WHERE bukrs = gs_head-bukrs
        AND   spras = sy-langu.

      IF gt_t053s IS NOT INITIAL .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'RSTGR'    "指定ALV用F4的字段
            dynpprog        = sy-repid
            value_org       = 'S'
          TABLES
            value_tab       = gt_t053s
            return_tab      = lt_ddshretval
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
      ENDIF.

      READ TABLE gt_item INTO gs_item INDEX ps_row_no-row_id.
      IF sy-subrc = 0.
        READ TABLE lt_ddshretval INTO lw_ddshretval INDEX 1.
        IF sy-subrc = 0.
          gs_item-rstgr = lw_ddshretval-fieldval.
        ENDIF.
        IF gs_item-rstgr IS NOT INITIAL.
          MODIFY gt_item FROM gs_item INDEX ps_row_no-row_id.
          " Trigger data changed event
          ASSIGN pr_event_data->m_data->* TO <fs_mod_cells>.
          lw_mod_cell-row_id     = ps_row_no-row_id.
          lw_mod_cell-sub_row_id = ps_row_no-sub_row_id.
          lw_mod_cell-fieldname  = 'RSTGR'.
          lw_mod_cell-value      = gs_item-rstgr.
          APPEND lw_mod_cell TO <fs_mod_cells>.
        ENDIF.
      ENDIF.

  ENDCASE.

**  Inform ALV Grid that event 'onf4' has been processed
  pr_event_data->m_event_handled = 'X'.           "告知F4动作结束
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_ITEM_HKONT  text
*----------------------------------------------------------------------*
FORM sub_help_hkont  CHANGING p_hkont.
  SET PARAMETER ID 'BUK' FIELD gs_head-bukrs.

*搜索帮助
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      matchcode_object          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      select_value              = p_hkont
    EXCEPTIONS
      invalid_dictionary_field  = 1
      invalid_matchdcode_object = 2
      no_selection              = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_acc_reversal .
  READ TABLE gt_data  TRANSPORTING NO FIELDS WITH KEY zbox = 'X'.
  IF sy-subrc NE 0.
    MESSAGE '请先选择需要冲销的凭证！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA l_belnr    TYPE bseg-belnr.
  DATA l_gjahr    TYPE bseg-gjahr.
  DATA l_check_1  TYPE c.

  CLEAR l_belnr.
  CLEAR l_gjahr.
  CLEAR l_check_1.

*自动选中相同凭证的行
  LOOP AT gt_data INTO gs_data
   WHERE zbox = 'X'.

    LOOP AT gt_data INTO gs_data
     WHERE belnr_f = gs_data-belnr_f
     AND   gjahr_f = gs_data-gjahr_f.
*当遇到不同的凭证，退出
      IF gs_data-belnr_f IS INITIAL.
        l_belnr = gs_data-belnr_f.
      ELSEIF l_belnr <> gs_data-belnr_f
        OR     l_gjahr <> gs_data-gjahr_f.
        l_check_1 = 'X'.
        EXIT.
      ENDIF.

      gs_data-zbox = 'X'.
      MODIFY gt_data FROM gs_data.
    ENDLOOP.
  ENDLOOP.

  IF l_check_1 <> 'X'.
    MESSAGE '请仅选择相同的凭证进行冲销！' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*取冲销原因和过账日期
  CLEAR wa_reversal.
  PERFORM frm_get_rev_reason.

  IF wa_reversal-reason_rev IS INITIAL.
    MESSAGE '用户取消！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM frm_message_initial.
  CLEAR g_suc.

  READ TABLE gt_data INTO gs_data WITH KEY zbox = 'X'.
  IF sy-subrc = 0.

    CLEAR gs_bkpf.
*冲销凭证
    SELECT SINGLE *
      INTO gs_bkpf
      FROM bkpf
      WHERE bukrs = gs_data-bukrs
        AND belnr = gs_data-belnr_f
        AND gjahr = gs_data-gjahr_f.

*冲销数据准备
    PERFORM frm_rev_prep.

*调用冲销BAPI
    PERFORM frm_reversal_bapi.

  ENDIF.

  PERFORM frm_message_show.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_REV_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_rev_reason .
  CALL SCREEN 9001 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REV_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_rev_prep .
  wa_reversal-obj_type  = gs_bkpf-awtyp.
  wa_reversal-obj_key   = gs_bkpf-awkey.
  wa_reversal-obj_key_r = gs_bkpf-awkey.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSAL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_reversal_bapi .

*   取得系统 LOGICAL SYSTEM
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
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    CLEAR wa_reversal.
    g_suc = 'X'.

*更新状态
    LOOP AT gt_data INTO gs_data
     WHERE zbox = 'X'.
      gs_data-zcljd   = '2'.
      gs_data-statu   = icon_green_light.
      gs_data-belnr_f = ''.
      gs_data-gjahr_f = ''.
      gs_data-yfje  = gs_data-yfje - gs_data-zsqfkje ."更新已付款金额数据 ：原已保存已付款数据 + 已记账成功的申请金额数据
      gs_data-ysqje = gs_data-ysqje - gs_data-zsqfkje.

      MODIFY gt_data FROM gs_data.
      CLEAR gs_data.
    ENDLOOP.

*更新自建表状态
    REFRESH gt_zfi017.
    MOVE-CORRESPONDING gt_data TO gt_zfi017.
    MODIFY zfi017 FROM TABLE gt_zfi017.

  ENDIF.

  LOOP AT it_return INTO wa_return.
    PERFORM store_messages USING wa_return-id
                                 wa_return-type
                                 wa_return-message_v1
                                 wa_return-message_v2
                                 wa_return-message_v3
                                 wa_return-message_v4
                                 wa_return-number.
  ENDLOOP.
ENDFORM.
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
FORM frm_unlock.
*DEQUEUE_EZMM002I
*ENQUEUE_EZMM002I
  CALL FUNCTION 'DEQUEUE_ALL'
* EXPORTING
*   _SYNCHRON       = ' '
    .
ENDFORM.
