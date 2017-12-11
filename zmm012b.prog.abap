REPORT zmm012b.
"MODIFIED  by it02 20160129
"库存呆滞表 由MARD历史库存改为从MSEG明细汇总
** 修改日期   开发人员  请求号        描述
" 20170310   IT02     ED1K905293   追加F库存并清除明细库存地点汇总

TYPE-POOLS: slis.
TABLES: mard,mara,mbew,marc,prps.",
************************************************************************
*                       定义类型
************************************************************************

* 定义用与转换的类型
TYPES: BEGIN OF ty_out,
         index(10) TYPE n, "序号
         mtart     TYPE mara-mtart, "物料类型
         matkl     TYPE mara-matkl, "物料组
         extwg     TYPE mara-extwg,
         matnr     TYPE mara-matnr, "物料编号
         maktx     TYPE makt-maktx, "规格说明
         meins     TYPE mara-meins, "单位
         werks     TYPE mard-werks, "工厂
         dispo     TYPE marc-dispo,
         lgort     TYPE mard-lgort, "仓码
         loggr     TYPE marc-loggr, "仓管员代码
         labst     TYPE mard-labst, "总数量
         ndnum     TYPE mard-labst, "需求数量
         sobkz     LIKE mseg-sobkz,
         lifnr     LIKE mseg-lifnr,
         kunnr     LIKE mseg-kunnr,
         kdauf     LIKE mseg-kdauf,
         kdpos     LIKE mseg-kdpos,
         mat_pspnr LIKE mseg-mat_pspnr,
         charg     LIKE mseg-charg,
*         stprs     TYPE p LENGTH 13 DECIMALS 2, "单价
*         peinh     TYPE peinh,
         hstprs    TYPE p LENGTH 13 DECIMALS 2,
         hpeinh    TYPE peinh,
         num1      TYPE mseg-menge, "一个月以内
         stprs1    TYPE p LENGTH 13 DECIMALS 2,
         num2      TYPE mseg-menge, "1-2
         stprs2    TYPE p LENGTH 13 DECIMALS 2,
         num3      TYPE mseg-menge, "2-3
         stprs3    TYPE p LENGTH 13 DECIMALS 2,
         num4      TYPE mseg-menge, "3-6
         stprs4    TYPE p LENGTH 13 DECIMALS 2,
         num5      TYPE mseg-menge, "6-12
         stprs5    TYPE p LENGTH 13 DECIMALS 2,
         num6      TYPE mseg-menge, "12-24
         stprs6    TYPE p LENGTH 13 DECIMALS 2,
         num7      TYPE mseg-menge, ">12
         stprs7    TYPE p LENGTH 13 DECIMALS 2,
         ltdat     TYPE sy-datum, ""最后入库日期"
         ltnum     TYPE mseg-menge, ""最后入库数量"
         ludat     TYPE sy-datum, """最后耗用日期"
         lunum     TYPE mseg-menge, ""最后耗用数量"
         dnum      TYPE mseg-menge, "呆滞数量
         dstrps    TYPE p LENGTH 13 DECIMALS 2, "呆滞金额
         kalnr     TYPE ckmlhd-kalnr, "成本估算号
         stprs     TYPE ckmlcr-stprs, "当期标准价
         pvprs     TYPE ckmlcr-pvprs, "当期实际价
         peinh     TYPE peinh,
         sjkc      TYPE p LENGTH 13 DECIMALS 2, "实际库存
         checkbox  TYPE c LENGTH 1,
         cellcolor TYPE lvc_t_scol,
         post1     TYPE prps-post1,
         kunnr_txt TYPE kna1-name1,
         lifnr_txt TYPE lfa1-name1,
       END OF ty_out.

TYPES:BEGIN OF ty_item,
        mblnr      TYPE mblnr,
        mjahr      TYPE mjahr,
        zeile      TYPE mblpo,
        matnr      TYPE matnr,
        werks      TYPE werks_d,
        lgort      TYPE lgort_d,
        sobkz      LIKE mseg-sobkz,
        lifnr      LIKE mseg-lifnr,
        kunnr      LIKE mseg-kunnr,
        kdauf      LIKE mseg-kdauf,
        kdpos      LIKE mseg-kdpos,
        mat_kdauf  LIKE mseg-mat_kdauf,
        mat_kdpos  LIKE mseg-mat_kdpos,
        mat_pspnr  LIKE mseg-mat_pspnr,
        charg      LIKE mseg-charg,
        budat      TYPE budat,
        cpudt      TYPE cpudt,
        cputm      TYPE cputm,
        bwart      TYPE bwart,
        menge      TYPE menge_d,
        shkzg      TYPE shkzg,
        vgart_mkpf TYPE vgart,
        kzvbr      TYPE mseg-kzvbr,
        kzbew      TYPE mseg-kzbew,
      END OF ty_item.

TYPES:BEGIN OF ty_total,

        werks     TYPE werks,
        matnr     TYPE matnr,
        lgort     TYPE lgort_d,
        sobkz     LIKE mseg-sobkz,
        lifnr     LIKE mseg-lifnr,
        kunnr     LIKE mseg-kunnr,
        kdauf     LIKE mseg-kdauf,
        kdpos     LIKE mseg-kdpos,
        mat_pspnr LIKE mseg-mat_pspnr,
        charg     LIKE mseg-charg,
        labst     TYPE mard-labst, "总数量
        num1      TYPE mseg-menge, "一个月以内
        num2      TYPE mseg-menge, "1-2
        num3      TYPE mseg-menge, "2-3
        num4      TYPE mseg-menge, "3-6
        num5      TYPE mseg-menge, "6-12
        num6      TYPE mseg-menge, "12-24
        num7      TYPE mseg-menge, ">两年
      END OF ty_total.


DATA:gs_item    TYPE ty_item,
     gs_item_s  TYPE ty_item,
     gs_item_h  TYPE ty_item,
     gs_item_rk TYPE ty_item,
     gs_item_hy TYPE ty_item,
     gt_item    TYPE TABLE OF ty_item,
     gt_item_s  TYPE TABLE OF ty_item,
     gt_item_h  TYPE TABLE OF ty_item,
     gt_item_rk TYPE TABLE OF ty_item, "
     gt_item_hy TYPE TABLE OF ty_item.

DATA:gs_total TYPE ty_total,
     gt_total TYPE TABLE OF ty_total.
************************************************************************
*                    声明内表、工作区和全局变量
************************************************************************
DATA: ts_out TYPE STANDARD TABLE OF ty_out,
      tl_out LIKE LINE OF ts_out.
DATA: ts_mard      LIKE TABLE OF mard WITH HEADER LINE,
      ts_mard_mska LIKE TABLE OF mard WITH HEADER LINE,
      ts_mard_mcsd LIKE TABLE OF mard WITH HEADER LINE,
      tt_mard      LIKE TABLE OF mard WITH HEADER LINE,
      ts_mska      LIKE TABLE OF mska WITH HEADER LINE,
      ts_mska_01   LIKE TABLE OF mska WITH HEADER LINE,
      ts_mcsd      LIKE TABLE OF mcsd WITH HEADER LINE,
      ts_mcsd_01   LIKE TABLE OF mcsd WITH HEADER LINE,
      ts_mara      LIKE TABLE OF mara WITH HEADER LINE,
      ts_makt      LIKE TABLE OF makt WITH HEADER LINE,
      ts_mkpf      LIKE TABLE OF mkpf WITH HEADER LINE,
      ts_mseg      LIKE TABLE OF mseg WITH HEADER LINE,
      tt_mseg      LIKE TABLE OF mseg WITH HEADER LINE,
      ts_mbew      LIKE TABLE OF mbew WITH HEADER LINE,
      ts_ckmlcr    LIKE TABLE OF ckmlcr WITH HEADER LINE,
      ts_marc      LIKE TABLE OF marc WITH HEADER LINE,
*      TS_ZTMM_QCKL LIKE TABLE OF ZTMM_QCKL WITH HEADER LINE,
      ts_zmm012    LIKE TABLE OF zmm012 WITH HEADER LINE,
      ts_mardh     LIKE TABLE OF mardh  WITH HEADER LINE,
      ts_mskah     LIKE TABLE OF mska WITH HEADER LINE,
      ts_mcsdh     LIKE TABLE OF mcsd WITH HEADER LINE,
      "增加物料分类账取数逻辑
      gt_ckmlhd    LIKE TABLE OF ckmlhd WITH HEADER LINE,
      gt_ckmlcr    LIKE TABLE OF ckmlcr WITH HEADER LINE,
      gt_ckmlpp    LIKE TABLE OF ckmlpp WITH HEADER LINE.


DATA minyear TYPE pyear.
DATA year TYPE pyear.
DATA month TYPE month.

DATA p_year TYPE pyear.
DATA p_month TYPE month.


DATA: date    LIKE sy-datum,
      datu(8).


DATA: g_datum TYPE sy-datum, "最后一天日期
      g_month TYPE n LENGTH 3, "月份
      g_day   TYPE n LENGTH 2. "天
DATA: i_date TYPE sy-datum.
** 使用 ALV
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,    " ALV 标题内表
      wa_layout   TYPE slis_layout_alv.        " ALV 格式
DATA: gv_last_day TYPE sy-datum.
*      it_events   TYPE slis_t_event.           " ALV事件处理

FIELD-SYMBOLS <p_hsl>.                         " 定义指针


************************************************************************
*                    选择屏幕                                          *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.

SELECT-OPTIONS: s_werks FOR  mbew-bwkey OBLIGATORY,"   工厂
                s_lgort FOR  mard-lgort ,"库存地点
                s_matnr FOR  mard-matnr," 物料
                s_mtart FOR  mara-mtart,
                s_matkl FOR  mara-matkl,"物料组
                s_dispo FOR  marc-dispo,
                s_pspnr FOR  prps-pspnr.
PARAMETERS:p_budat TYPE mseg-budat_mkpf DEFAULT sy-datum OBLIGATORY."过账日期
SELECTION-SCREEN END OF BLOCK blk2.

************************************************************************
*                    初始化事件                                        *
************************************************************************
INITIALIZATION.


************************************************************************
*                    选择屏幕事件                                      *
************************************************************************
AT SELECTION-SCREEN.





************************************************************************
*                   START-OF-SELECTION                                *
************************************************************************
START-OF-SELECTION.
  CALL FUNCTION 'FKK_GET_LAST_DAY_OF_MONTH'
    EXPORTING
*     I_YEAR     =
*     I_MONTH    =
      i_date_in  = p_budat
    IMPORTING
      e_date_out = gv_last_day.

  PERFORM  frm_getdata.
  PERFORM  frm_dealdata.

************************************************************************
*                   END-OF-SELECTION                                   *
************************************************************************
END-OF-SELECTION.
* using alv to display report
  PERFORM frm_alv.

*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*  功能描述：按指定条件从数据库表中取出数据，并传入相应的内表或工作区
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_getdata.
  DATA: lt_matnr TYPE STANDARD TABLE OF matnr.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ts_zmm012 FROM zmm012.
  IF p_budat IS INITIAL.
    p_budat = sy-datum.
  ENDIF.
  SELECT mkpf~mblnr mkpf~mjahr mkpf~budat
         mkpf~cpudt mkpf~cputm
         mseg~zeile mseg~mblnr  mseg~menge
         mseg~bwart mseg~matnr mseg~werks mseg~lgort
         mseg~shkzg mseg~vgart_mkpf
         mseg~kzvbr mseg~kzbew
         mseg~sobkz mseg~lifnr mseg~kunnr mseg~kdauf
         mseg~kdpos mseg~mat_pspnr mseg~mat_kdauf mseg~mat_kdpos
         mseg~charg
       INTO CORRESPONDING FIELDS OF TABLE gt_item
       FROM mkpf
       INNER JOIN mseg
       ON mkpf~mblnr = mseg~mblnr
       AND mkpf~mjahr = mseg~mjahr
       WHERE mkpf~budat <= p_budat
       AND mseg~matnr NE ''
       AND mseg~werks IN s_werks
       AND mseg~lgort IN s_lgort
       AND mseg~matnr IN s_matnr
       AND mseg~bwart NOT IN ('103','104','124','125','315','316')
       AND NOT ( mseg~bwart IN ('101','102') AND mseg~kzvbr IN ('A','V') )
*       AND NOT ( mseg~bwart IN ('313','314') AND mseg~xauto = 'X' )
       AND mseg~mat_pspnr IN s_pspnr
       AND mseg~sobkz NE 'F'.
  SORT gt_item BY  werks matnr lgort lifnr kunnr kdauf kdpos mat_pspnr charg.

  SELECT mkpf~mblnr mkpf~mjahr mkpf~budat
        mkpf~cpudt mkpf~cputm
        mseg~zeile mseg~mblnr  mseg~menge
        mseg~bwart mseg~matnr mseg~werks             " MSEG~LGORT
        mseg~shkzg mseg~vgart_mkpf
        mseg~kzvbr mseg~kzbew
        mseg~sobkz mseg~lifnr mseg~kunnr mseg~kdauf
        mseg~kdpos mseg~mat_pspnr mseg~mat_kdauf mseg~mat_kdpos
        mseg~charg
      APPENDING CORRESPONDING FIELDS OF TABLE gt_item
      FROM mkpf
      INNER JOIN mseg
      ON mkpf~mblnr = mseg~mblnr
      AND mkpf~mjahr = mseg~mjahr
      WHERE mkpf~budat <= p_budat
      AND mseg~matnr NE ''
      AND mseg~werks IN s_werks
      AND mseg~lgort IN s_lgort
      AND mseg~matnr IN s_matnr
      AND mseg~bwart NOT IN ('103','104','124','125','315','316')
      AND NOT ( mseg~bwart IN ('101','102') AND mseg~kzvbr IN ('A','V') )
*      AND NOT ( mseg~bwart IN ('313','314') AND mseg~xauto = 'X' )
      AND mseg~mat_pspnr IN s_pspnr
      AND mseg~sobkz EQ 'F'.
  SORT gt_item BY  werks matnr lgort lifnr kunnr kdauf kdpos mat_pspnr charg.
  "在MSEG中取值时，若特殊库存为以下字段，则需要增加的对应的维度取值。
  "特殊库存	字段
  "B  客户：MSEG-KDAUF
  "E  销售订单+销售订单行项目
  "O  供应商
  "Q  WBS
  "F  供应商+销售订单+销售订单行项目
  CHECK gt_item[] IS NOT INITIAL.
  LOOP AT gt_item INTO gs_item.
    IF gs_item-sobkz = 'E'.
      gs_item-kdauf = gs_item-mat_kdauf.
      gs_item-kdpos = gs_item-mat_kdpos.
    ENDIF.
    CLEAR: gs_item-mat_kdauf,gs_item-mat_kdpos.
    CASE gs_item-sobkz.
      WHEN 'B'.
        CLEAR: gs_item-lifnr,gs_item-kdauf,
         gs_item-kdpos,gs_item-mat_pspnr.
      WHEN 'E'.
        CLEAR: gs_item-lifnr,gs_item-kunnr,gs_item-mat_pspnr.
      WHEN 'O'.
        CLEAR: gs_item-kunnr,gs_item-kdauf,
         gs_item-kdpos,gs_item-mat_pspnr.
      WHEN 'Q'.
        CLEAR: gs_item-lifnr,gs_item-kunnr,gs_item-kdauf,
         gs_item-kdpos.
      WHEN 'F'.
        CLEAR: gs_item-kunnr,gs_item-mat_pspnr,gs_item-lgort.
      WHEN ''.
        CLEAR: gs_item-lifnr,gs_item-kunnr,gs_item-kdauf,
         gs_item-kdpos,gs_item-mat_pspnr.
      WHEN OTHERS.
    ENDCASE.
    MODIFY gt_item FROM gs_item.
    COLLECT gs_item-matnr INTO lt_matnr.
  ENDLOOP.
  APPEND LINES OF gt_item TO gt_item_s .
  APPEND LINES OF gt_item TO gt_item_h.

  DELETE gt_item_s WHERE shkzg EQ 'H'  .
  SORT gt_item_s BY werks ASCENDING matnr ASCENDING lgort ASCENDING
  sobkz ASCENDING lifnr ASCENDING kunnr ASCENDING
  kdauf ASCENDING kdpos ASCENDING mat_pspnr ASCENDING charg ASCENDING
  budat DESCENDING  cpudt DESCENDING cputm DESCENDING.
*  APPEND LINES OF GT_ITEM_S TO GT_ITEM_RK .
*  DELETE GT_ITEM_RK WHERE VGART_MKPF NE 'WE' AND VGART_MKPF NE 'WQ' AND VGART_MKPF NE 'WR' AND VGART_MKPF NE 'WF' .
*  SORT GT_ITEM_RK BY WERKS ASCENDING MATNR ASCENDING LGORT ASCENDING    BUDAT DESCENDING  CPUDT DESCENDING CPUTM DESCENDING.
  DELETE gt_item_h WHERE shkzg EQ 'S'.
*  APPEND LINES OF GT_ITEM_H TO GT_ITEM_HY  .
*  DELETE GT_ITEM_HY WHERE VGART_MKPF NE 'WA' .
*  SORT GT_ITEM_HY BY  WERKS ASCENDING MATNR ASCENDING LGORT ASCENDING   BUDAT DESCENDING CPUDT DESCENDING CPUTM DESCENDING.
  SORT gt_item_h BY werks ASCENDING matnr ASCENDING lgort ASCENDING
  sobkz ASCENDING lifnr ASCENDING kunnr ASCENDING
  kdauf ASCENDING kdpos ASCENDING mat_pspnr ASCENDING charg ASCENDING
  budat DESCENDING  cpudt DESCENDING cputm DESCENDING.
  CHECK gt_item IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ts_mara
    FROM mara
    FOR ALL ENTRIES IN lt_matnr
    WHERE matnr IN s_matnr
      AND matnr = lt_matnr-table_line
      AND mtart IN s_mtart
      AND matkl IN s_matkl.
  SORT ts_mara BY matnr.

*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ts_marc
    FROM marc
    FOR ALL ENTRIES IN lt_matnr
    WHERE matnr IN s_matnr
    AND   matnr = lt_matnr-table_line
    AND   werks IN s_werks
    AND   dispo IN s_dispo.
  SORT ts_marc BY  werks matnr.
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ts_makt
    FROM makt
    FOR ALL ENTRIES IN lt_matnr
    WHERE matnr IN s_matnr
    AND   matnr = lt_matnr-table_line
    AND spras = sy-langu.
  SORT ts_makt BY matnr.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MKPF
*    FROM MKPF .
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MSEG
*    FROM MSEG FOR ALL ENTRIES IN TS_MARD
*    WHERE MATNR = TS_MARD-MATNR
*    AND   WERKS = TS_MARD-WERKS
*    AND   LGORT = TS_MARD-LGORT.
*  REFRESH TT_MSEG.
*  APPEND LINES OF TS_MSEG TO TT_MSEG.
*  SORT TS_MSEG BY MATNR WERKS LGORT  CPUDT_MKPF   DESCENDING .
*  SORT TT_MSEG BY MATNR WERKS LGORT  BUDAT_MKPF   .
*
*
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_ZTMM_QCKL
**    FROM
*.
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ts_mbew
    FROM mbew
    FOR ALL ENTRIES IN lt_matnr
    WHERE  matnr IN s_matnr
    AND    matnr = lt_matnr-table_line
    AND  bwkey IN s_werks
         .
*
  SORT ts_mbew BY  bwkey matnr.

*
*  IF TS_MBEW[] IS NOT INITIAL.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_CKMLCR
*      FROM CKMLCR FOR ALL ENTRIES IN TS_MBEW
*      WHERE KALNR = TS_MBEW-KALN1
*      AND   CURTP = '30'.
*    MINYEAR = SY-DATUM+0(4).
*    IF TS_CKMLCR[] IS NOT INITIAL.
*      SORT TS_CKMLCR BY BDATJ.
*      READ TABLE TS_CKMLCR INDEX 1.
*      MINYEAR = TS_CKMLCR-BDATJ - 1.
*    ENDIF.
*
*  ENDIF.
ENDFORM.                    " FRM_GETDATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_dealdata .
  DATA index(10) TYPE n VALUE 0.
  DATA ts_mdps LIKE TABLE OF mdps WITH HEADER LINE.
  DATA bdate LIKE sy-datum.
  DATA ldate LIKE sy-datum.
  DATA flag TYPE c.
  DATA ukurs TYPE ukurs_curr.
  DATA: BEGIN OF lt_sel OCCURS 0,
          bwkey TYPE ckmlhd-bwkey,
          matnr TYPE ckmlhd-matnr,
          vbeln TYPE ckmlhd-vbeln,
          posnr TYPE ckmlhd-posnr,
          pspnr TYPE ckmlhd-pspnr,
        END OF lt_sel.
  DATA: lt_sel_all LIKE TABLE OF lt_sel WITH HEADER LINE.


  "增加字段 WBS描述、销售订单描述、供应商描述、客户描述
  DATA: lt_pspnr TYPE TABLE OF mat_pspnr,
        lt_kdauf TYPE TABLE OF kdauf,
        lt_kunnr TYPE TABLE OF kunnr,
        lt_lifnr TYPE TABLE OF lifnr.
  DATA: BEGIN OF lt_prps OCCURS 0,
          pspnr TYPE ps_posnr,
          post1 TYPE ps_post1,
        END OF lt_prps.
  DATA: BEGIN OF lt_kna1 OCCURS 0,
          kunnr LIKE kna1-kunnr,
          name1 LIKE kna1-name1,
        END OF lt_kna1.
  DATA: BEGIN OF lt_lfa1 OCCURS 0,
          lifnr LIKE lfa1-lifnr,
          name1 LIKE lfa1-name1,
        END OF lt_lfa1.
  LOOP AT gt_item INTO gs_item.
    IF gs_item-mat_pspnr IS NOT INITIAL.
      COLLECT gs_item-mat_pspnr INTO lt_pspnr.
    ENDIF.
    IF gs_item-kunnr IS NOT INITIAL.
      COLLECT gs_item-kunnr INTO lt_kunnr.
    ENDIF.
    IF gs_item-lifnr IS NOT INITIAL.
      COLLECT gs_item-lifnr INTO lt_lifnr.
    ENDIF.
  ENDLOOP.
  IF lt_pspnr[] IS NOT INITIAL.
    SELECT pspnr post1
      INTO TABLE lt_prps
      FROM prps
      FOR ALL ENTRIES IN lt_pspnr
      WHERE pspnr = lt_pspnr-table_line.
    SORT lt_prps BY pspnr.
  ENDIF.
  IF lt_kunnr[] IS NOT INITIAL.
    SELECT kunnr name1
      INTO TABLE lt_kna1
      FROM kna1
      FOR ALL ENTRIES IN lt_kunnr
      WHERE kunnr = lt_kunnr-table_line.
    SORT lt_kna1 BY kunnr.
  ENDIF.
  IF lt_lifnr[] IS NOT INITIAL.
    SELECT lifnr name1
      INTO TABLE lt_lfa1
      FROM lfa1
      FOR ALL ENTRIES IN lt_lifnr
      WHERE lifnr = lt_lifnr-table_line.
    SORT lt_lfa1 BY lifnr.
  ENDIF.
*     CALL FUNCTION 'READ_EXCHANGE_RATE'
*      EXPORTING
**       CLIENT                  = SY-MANDT
*        DATE                    = SY-DATUM
*        FOREIGN_CURRENCY        = 'CNY'
*        LOCAL_CURRENCY          = 'HKD'
*        TYPE_OF_RATE            = 'M'
**       EXACT_DATE              = ' '
*     IMPORTING
*       EXCHANGE_RATE           =  UKURS
**       FOREIGN_FACTOR          =
**       LOCAL_FACTOR            =
**       VALID_FROM_DATE         =
**       DERIVED_RATE_TYPE       =
**       FIXED_RATE              =
**       OLDEST_RATE_FROM        =
*     EXCEPTIONS
*       NO_RATE_FOUND           = 1
*       NO_FACTORS_FOUND        = 2
**       NO_SPREAD_FOUND         = 3
**       DERIVED_2_TIMES         = 4
**       OVERFLOW                = 5
**       ZERO_RATE               = 6
**       OTHERS                  = 7
* .
  DATA:klts TYPE  i. "库龄天数
  DATA:l_tabix TYPE i.
  "按物料、工厂、库存地 维度统计 总数量 、1个月以内、1-2月以内数量 。。
  LOOP AT gt_item INTO gs_item.
    l_tabix = sy-tabix.
    READ TABLE ts_mara WITH KEY matnr = gs_item-matnr BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    READ TABLE ts_marc WITH KEY werks = gs_item-werks matnr = gs_item-matnr BINARY SEARCH.
    IF sy-subrc NE 0 .
      CONTINUE.
    ENDIF.
    "MSEG-KZVBR等于V，且MSEG-KZBEW不等于L的，被排除
    CLEAR :gs_total,klts.
    IF gs_item-kzvbr EQ 'V' AND gs_item-kzbew NE 'L'.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING gs_item TO gs_total.

    IF gs_item-shkzg EQ 'H'.
      gs_total-labst = gs_item-menge * -1.    "总数量
    ELSE.
      gs_total-labst = gs_item-menge .
      klts = p_budat - gs_item-budat.
      IF klts >= 0 AND klts < 30.
        gs_total-num1 = gs_item-menge .       "1个月以内数量      "
      ELSEIF klts >= 30 AND klts < 60.
        gs_total-num2 = gs_item-menge .    "1-2月以内数量
      ELSEIF klts >= 60 AND klts < 90.
        gs_total-num3 = gs_item-menge .    "2-3个月以内数量
      ELSEIF klts >= 90 AND klts < 180.
        gs_total-num4 = gs_item-menge .    "3-6月以内数量
      ELSEIF klts >= 180 AND klts < 365.
        gs_total-num5 = gs_item-menge .    "6-12月以内数量
      ELSEIF klts >= 365 AND klts < 730.
        gs_total-num6 = gs_item-menge .    "12-24个月以内数量
      ELSEIF klts >= 730.
        gs_total-num7 = gs_item-menge .     "大于两年以上数量
      ENDIF.
    ENDIF.
    COLLECT gs_total INTO gt_total.
    lt_sel_all-bwkey = gs_total-werks.
    lt_sel_all-matnr = gs_total-matnr.
    lt_sel_all-vbeln = gs_total-kdauf.
    lt_sel_all-posnr = gs_total-kdpos.
    lt_sel_all-pspnr = gs_total-mat_pspnr.
    COLLECT lt_sel_all.
  ENDLOOP.
  "增加物料帐相关取数
  IF lt_sel_all[] IS NOT INITIAL.
    "EF类库存取数
    lt_sel[] = lt_sel_all[].
    DELETE lt_sel WHERE vbeln IS INITIAL.
    IF lt_sel[] IS NOT INITIAL.
      SELECT *
        FROM ckmlhd
        INTO TABLE gt_ckmlhd
        FOR ALL ENTRIES IN lt_sel
        WHERE matnr = lt_sel-matnr
        AND   bwkey = lt_sel-bwkey
        AND   vbeln = lt_sel-vbeln
        AND   posnr = lt_sel-posnr.
    ENDIF.
    "Q类库存取数
    lt_sel[] = lt_sel_all[].
    DELETE lt_sel WHERE pspnr IS INITIAL.
    IF lt_sel[] IS NOT INITIAL.
      SELECT *
        FROM ckmlhd
        APPENDING TABLE gt_ckmlhd
        FOR ALL ENTRIES IN lt_sel
        WHERE matnr = lt_sel-matnr
        AND   bwkey = lt_sel-bwkey
        AND   pspnr = lt_sel-pspnr.
    ENDIF.
    "其他库存取数
    lt_sel[] = lt_sel_all[].
    DELETE lt_sel WHERE pspnr IS NOT INITIAL.
    DELETE lt_sel WHERE vbeln IS NOT INITIAL.
    IF lt_sel[] IS NOT INITIAL.
      SELECT *
        FROM ckmlhd
        APPENDING TABLE gt_ckmlhd
        FOR ALL ENTRIES IN lt_sel
        WHERE matnr = lt_sel-matnr
        AND   bwkey = lt_sel-bwkey
        AND   pspnr = lt_sel-pspnr
        AND   vbeln = lt_sel-vbeln
        AND   posnr = lt_sel-posnr.
    ENDIF.

  ENDIF.
  IF gt_ckmlhd[] IS NOT INITIAL AND p_budat IS NOT INITIAL.
    DATA: ls_poper TYPE ckmlcr-poper.
    ls_poper = p_budat+4(2).
    SELECT *
      INTO TABLE gt_ckmlcr
      FROM ckmlcr
      FOR ALL ENTRIES IN gt_ckmlhd
      WHERE kalnr = gt_ckmlhd-kalnr
      AND   bdatj = p_budat(4)
      AND   poper = ls_poper
      AND   curtp = '10'.
    SELECT *
      INTO TABLE gt_ckmlpp
      FROM ckmlpp
      FOR ALL ENTRIES IN gt_ckmlhd
      WHERE kalnr = gt_ckmlhd-kalnr
      AND   bdatj = p_budat(4)
      AND   poper = ls_poper.
  ENDIF.

  SORT gt_total BY  werks matnr lgort .
  DELETE gt_total WHERE  labst EQ 0 .  "删除总数量为0的记录,以下只分析不为0的记录并且展现到ALV
  LOOP AT gt_total INTO gs_total.
    CLEAR flag.

    CLEAR ts_mara.
    AT NEW matnr.
      CLEAR tl_out.
      MOVE-CORRESPONDING gs_total TO tl_out.
      READ TABLE ts_mara WITH KEY matnr = gs_total-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        tl_out-mtart = ts_mara-mtart.   "物料类型
        tl_out-matkl = ts_mara-matkl.   "物料组
        "TL_OUT-EXTWG = TS_MARA-EXTWG.   "外部物料组
        tl_out-meins = ts_mara-meins.  "基本单位
      ENDIF.
*    IF TL_OUT-MTART NOT IN S_MTART .
*      CONTINUE.
*    ENDIF.
*    IF TL_OUT-MATKL NOT IN S_MATKL .
*      CONTINUE.
*    ENDIF.
      READ TABLE ts_marc WITH KEY werks = tl_out-werks
                                  matnr = tl_out-matnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.

        "   TL_OUT-LOGGR = TS_MARC-LOGGR.  "后勤处理组
        tl_out-dispo = ts_marc-dispo.  "MRP控制者
      ENDIF.
*    IF TL_OUT-DISPO NOT IN S_DISPO .
*      CONTINUE.
*    ENDIF.

      READ TABLE ts_makt WITH KEY matnr = tl_out-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        tl_out-maktx = ts_makt-maktx.  "物料描述
      ENDIF.
*       CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*      EXPORTING
*        INPUT  = TL_OUT-MEINS
**       LANGUAGE             = SY-LANGU
*      IMPORTING
**       LONG_TEXT            =
*        OUTPUT = TL_OUT-MEINS
**       SHORT_TEXT           =
**           EXCEPTIONS
**       UNIT_NOT_FOUND       = 1
**       OTHERS = 2
*      .
      "  TL_OUT-LABST = GS_ITEM-MENGE ."总数量
      "20171103取消需求数量取数
*      REFRESH ts_mdps .
*      CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
*        EXPORTING
**         PLSCN =
*          matnr = tl_out-matnr
*          werks = tl_out-werks
*        TABLES
*          mdpsx = ts_mdps
*        .
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ELSE.
*        LOOP AT ts_mdps WHERE plumi = '-'.
*          tl_out-ndnum = tl_out-ndnum + ts_mdps-mng01.  " "需求数量
*        ENDLOOP.
*      ENDIF.
      "20171103取消需求数量取数
      READ TABLE ts_mbew WITH KEY  bwkey = tl_out-werks matnr = tl_out-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        " TL_OUT-HSTPRS =  TS_MBEW-VERPR.
        " TL_OUT-HPEINH =  TS_MBEW-PEINH.
        IF  ts_mbew-vprsv EQ 'S'.
          tl_out-stprs =  ts_mbew-stprs.    "标准单价
        ELSE.
          tl_out-stprs = ts_mbew-verpr. "移动平均价
        ENDIF.
        tl_out-peinh =  ts_mbew-peinh.    "价格单位
      ENDIF.

    ENDAT.
    tl_out-labst = gs_total-labst .
    IF gs_total-num1 > tl_out-labst.
      tl_out-num1 = gs_total-labst.                   "1个月以内数量
    ELSE.
      tl_out-num1 = gs_total-num1.
    ENDIF.
    tl_out-stprs1 = tl_out-num1 * tl_out-stprs / tl_out-peinh.     "1个月以内金额

    IF gs_total-num2 > tl_out-labst - tl_out-num1.
      tl_out-num2 = tl_out-labst - tl_out-num1.                    "1-2月以内数量
    ELSE.
      tl_out-num2 =   gs_total-num2 .
    ENDIF.
    tl_out-stprs2 = tl_out-num2 * tl_out-stprs / tl_out-peinh.     "1-2月以内金额

    IF gs_total-num3 > tl_out-labst - tl_out-num1 - tl_out-num2 .
      tl_out-num3 = tl_out-labst - tl_out-num1 - tl_out-num2.       "2-3个月以内数量
    ELSE.
      tl_out-num3 = gs_total-num3 .
    ENDIF.
    tl_out-stprs3 = tl_out-num3 * tl_out-stprs / tl_out-peinh.      "2-3个月以内金额

    IF gs_total-num4 > tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 .
      tl_out-num4 = tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 ..  "3-6月以内数量
    ELSE.
      tl_out-num4 = gs_total-num4 .
    ENDIF.
    tl_out-stprs4 = tl_out-num4 * tl_out-stprs / tl_out-peinh.                 "3-6月以内金额

    IF gs_total-num5 > tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4 .
      tl_out-num5 = tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4.     "6-12月以内数量
    ELSE.
      tl_out-num5 = gs_total-num5 .
    ENDIF.
    tl_out-stprs5 = tl_out-num5 * tl_out-stprs / tl_out-peinh.                   "6-12个月以内金额

    IF gs_total-num6 > tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4 - tl_out-num5.
      tl_out-num6 = tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4 - tl_out-num5.         "12-24个月以内数量
    ELSE.
      tl_out-num6 = gs_total-num6 .
    ENDIF.
    tl_out-stprs6 = tl_out-num6 * tl_out-stprs / tl_out-peinh.                   "12-24个月以内金额

    IF gs_total-num7 > tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4 - tl_out-num5 - tl_out-num6.
      tl_out-num7 = tl_out-labst - tl_out-num1 - tl_out-num2 - tl_out-num3 - tl_out-num4 - tl_out-num5 - tl_out-num6.            "大于两年以上数量
    ELSE.
      tl_out-num7 = gs_total-num7 .
    ENDIF.
    tl_out-stprs7 = tl_out-num7 * tl_out-stprs / tl_out-peinh.                         "大于两年以上金额



    tl_out-dnum = tl_out-num5 + tl_out-num6 + tl_out-num7."呆滞数量  "TL_OUT-NUM4 +

    tl_out-dstrps  = tl_out-dnum * tl_out-stprs / tl_out-peinh. "呆滞金额
    index = index + 1.
    tl_out-index = index.
    tl_out-lgort = gs_total-lgort.
    tl_out-sobkz = gs_total-sobkz.
    tl_out-lifnr = gs_total-lifnr.
    tl_out-kunnr = gs_total-kunnr.
    tl_out-kdauf = gs_total-kdauf.
    tl_out-kdpos = gs_total-kdpos.
    tl_out-mat_pspnr = gs_total-mat_pspnr.
    tl_out-charg = gs_total-charg.
    READ TABLE gt_item_s INTO gs_item_s WITH KEY  werks = tl_out-werks matnr = tl_out-matnr lgort = tl_out-lgort
    sobkz = tl_out-sobkz lifnr = tl_out-lifnr kunnr = tl_out-kunnr kdauf = tl_out-kdauf kdpos = tl_out-kdpos
    mat_pspnr = tl_out-mat_pspnr charg = tl_out-charg BINARY SEARCH .
    IF sy-subrc = 0.
      tl_out-ltdat = gs_item_s-budat."最后入库日期"
      tl_out-ltnum =  gs_item_s-menge.
    ENDIF.

    READ TABLE gt_item_h INTO gs_item_h WITH KEY  werks = tl_out-werks matnr = tl_out-matnr lgort = tl_out-lgort
    sobkz = tl_out-sobkz lifnr = tl_out-lifnr kunnr = tl_out-kunnr kdauf = tl_out-kdauf kdpos = tl_out-kdpos
    mat_pspnr = tl_out-mat_pspnr charg = tl_out-charg BINARY SEARCH .
    IF sy-subrc = 0.
      tl_out-ludat = gs_item_h-budat."最后耗用日期"
      tl_out-lunum =  gs_item_h-menge. "最后耗用数量
    ENDIF.
    "增加物料分类账数据分配
    CLEAR: gt_ckmlhd,tl_out-kalnr.
    CASE tl_out-sobkz.
      WHEN 'E' OR 'F'.
        READ TABLE gt_ckmlhd WITH KEY matnr = tl_out-matnr
                                      bwkey = tl_out-werks
                                      vbeln = tl_out-kdauf
                                      posnr = tl_out-kdpos.
        IF sy-subrc = 0.
          tl_out-kalnr = gt_ckmlhd-kalnr.
        ENDIF.
      WHEN 'Q'.
        READ TABLE gt_ckmlhd WITH KEY matnr = tl_out-matnr
                                      bwkey = tl_out-werks
                                      pspnr = tl_out-mat_pspnr.
        IF sy-subrc = 0.
          tl_out-kalnr = gt_ckmlhd-kalnr.
        ENDIF.
      WHEN 'B'.
      WHEN OTHERS.
        READ TABLE gt_ckmlhd WITH KEY matnr = tl_out-matnr
                                      bwkey = tl_out-werks
                                      pspnr = ''
                                      vbeln = ''
                                      posnr = ''.
        IF sy-subrc = 0.
          tl_out-kalnr = gt_ckmlhd-kalnr.
        ENDIF.
    ENDCASE.
    CLEAR: tl_out-stprs,tl_out-pvprs,tl_out-peinh,tl_out-sjkc.
    IF tl_out-kalnr IS NOT INITIAL.
      READ TABLE gt_ckmlcr WITH KEY kalnr = tl_out-kalnr.
      IF sy-subrc = 0.
        READ TABLE gt_ckmlpp WITH KEY kalnr = tl_out-kalnr.
        IF sy-subrc = 0 AND gt_ckmlpp-lbkum NE 0.
          tl_out-sjkc  = gt_ckmlcr-salk3 / gt_ckmlpp-lbkum * tl_out-labst.
          CLEAR: tl_out-dnum,tl_out-dstrps.
          tl_out-dnum = tl_out-num5 + tl_out-num6 + tl_out-num7.
          tl_out-dstrps = gt_ckmlcr-salk3 / gt_ckmlpp-lbkum * tl_out-dnum.
        ENDIF.
        tl_out-stprs = gt_ckmlcr-stprs.
        tl_out-pvprs = gt_ckmlcr-pvprs.
        tl_out-peinh = gt_ckmlcr-peinh.
*        IF tl_out-peinh IS NOT INITIAL.
*          tl_out-sjkc  = gt_ckmlcr-pvprs * tl_out-labst / tl_out-peinh.
*        ENDIF.
      ENDIF.
    ENDIF.
*    CLEAR tl_out.
    CLEAR: tl_out-post1,tl_out-kunnr_txt,tl_out-lifnr_txt.
    IF tl_out-mat_pspnr IS NOT INITIAL.
      READ TABLE lt_prps WITH KEY pspnr = tl_out-mat_pspnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        tl_out-post1 = lt_prps-post1.
      ENDIF.
    ELSE.
      CLEAR tl_out-post1.
    ENDIF.
    IF tl_out-kunnr IS NOT INITIAL.
      READ TABLE lt_kna1 WITH KEY kunnr = tl_out-kunnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        tl_out-kunnr_txt = lt_kna1-name1.
      ENDIF.
    ELSE.
      CLEAR tl_out-kunnr_txt.
    ENDIF.
    IF tl_out-lifnr IS NOT INITIAL.
      READ TABLE lt_lfa1 WITH KEY lifnr = tl_out-lifnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        tl_out-lifnr_txt = lt_lfa1-name1.
      ENDIF.
    ELSE.
      CLEAR tl_out-lifnr_txt.
    ENDIF.
    APPEND tl_out TO ts_out.
  ENDLOOP.
ENDFORM.                    " FRM_DEALDATA



FORM get_pvprs USING pyear TYPE pyear
                     pmonth TYPE month
                     pkalnr TYPE ckmlcr-kalnr
              CHANGING     pvprs peinh .
  p_year = pyear.
  p_month = pmonth.
  CLEAR ts_ckmlcr.
  READ TABLE ts_ckmlcr WITH KEY bdatj = pyear
                                poper = pmonth
                                kalnr = pkalnr.
  IF sy-subrc = 0.
    pvprs = ts_ckmlcr-pvprs.
    peinh = ts_ckmlcr-peinh.
  ELSE.
    IF p_month = 1.
      p_month = 12.
      p_year = p_year - 1.
    ELSE.
      p_month = p_month - 1.
    ENDIF.
    IF p_year > minyear.
      PERFORM get_pvprs USING p_year p_month pkalnr
                        CHANGING pvprs peinh.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV
*&---------------------------------------------------------------------*
*  功能描述：ALV显示子程序
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_alv .

  PERFORM frm_fieldcat.
  PERFORM frm_layout.
  PERFORM frm_output.

ENDFORM.                    " FRM_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*  功能描述：显示ALV列的标题、列宽、CHECKBOX
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  PERFORM frm_disp USING:
        'INDEX'  '序号'  '' '' '' '',
        'MTART'   '物料类型'  '' '' '' '',
        'MATKL'   '物料组'  '' '' '' '',
*        'EXTWG'     '外部物料组'  '' '' '' '',
        'MATNR'  '物料编号'  '' 'MATNR' 'MARA' 'X',
        'MAKTX'  '物料描述'  '' '' '' '',
        'MEINS'   '单位'  '' '' '' '',
        'WERKS'  '工厂'  '' '' '' '',
        'DISPO'  'MRP控制者'  '' '' '' '',
        'LGORT'  '库存地点'  '' '' '' '',
        'CHARG'  '批号'  '' '' '' '',
        'SOBKZ    '  '特殊库存'  '' '' '' '',
        'LIFNR    '  '供应商'  '' '' '' '',
        'LIFNR_TXT'  '供应商名称'  '' '' '' '',
        'KUNNR    '  '客户'  '' '' '' '',
        'KUNNR_TXT'  '客户名称'  '' '' '' '',
        'KDAUF    '  '销售订单'  '' '' '' '',
        'KDPOS    '  '销售订单行项目'  '' '' '' '',
        'MAT_PSPNR'  'WBS'  '' 'PSPNR' 'PRPS' '',
        'POST1'      'WBS描述'  '' '' '' '',
*        'LOGGR'  '仓管员代码'   '' '' '' '',
        'LABST'  '总数量'  '' '' '' '',
*        'NDNUM'  '需求数量'  '' '' '' '',
*        'STPRS'  '单价'  '' '' '' '',
*        'PEINH'  '价格单位'  '' '' '' '',
*        'HSTPRS'  '单价(HKD)'  '' '' '' '',
*        'HPEINH'  '价格单位(HKD)'  '' '' '' '',
        'NUM1'   '1个月以内数量'  '' '' '' '',
*        'STPRS1'  '1个月以内金额'  '' '' '' '',
        'NUM2'    '1-2月以内数量'  '' '' '' '',
*        'STPRS2'  '1-2月以内金额'  '' '' '' '',
        'NUM3'   '2-3个月以内数量'  '' '' '' '',
*        'STPRS3' '2-3个月以内金额'   '' '' '' '',
        'NUM4'   '3-6月以内数量'  '' '' '' '',
*        'STPRS4' '3-6月以内金额'   '' '' '' '',
        'NUM5'   '6-12月以内数量'  '' '' '' '',
*        'STPRS5' '6-12个月以内金额'   '' '' '' '',
        'NUM6'   '12-24个月以内数量'  '' '' '' '',
*        'STPRS6'  '12-24个月以内金额'  '' '' '' '',
        'NUM7'   '大于两年以上数量'  '' '' '' '',
*        'STPRS7' '大于两年以上金额'   '' '' '' '',
        'LTDAT'  '最后入库日期'  '' '' '' '',
        'LTNUM'  '最后入库数量'  '' '' '' '',
        'LUDAT'  '最后耗用日期'  '' '' '' '',
        'LUNUM'  '最后耗用数量'  '' '' '' '',
        'KALNR'  '成本估算号'  '' '' '' '',
        'STPRS'  '当期标准价'  '' '' '' '',
        'PVPRS'  '当期实际价'  '' '' '' '',
        'PEINH'  '价格单位'  '' '' '' '',
        'SJKC'  '实际库存'  '' '' '' '',
        'DNUM'  '呆滞数量'  '' '' '' '',
        'DSTRPS' '呆滞金额'  '' '' '' ''.

*价格权限控制
  AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
  IF sy-subrc NE 0.
    DELETE it_fieldcat WHERE seltext_l CP '*金额' OR seltext_l EQ '单价' OR seltext_l EQ '价格单位'.
  ENDIF.
ENDFORM.                    " FRM_FIELDCAT
*&---------------------------------------------------------------------
*&      Form  FRM_FIELD_DISP
*&---------------------------------------------------------------------*
*  功能描述：通过传入字段名、字段名的相应描述，得到ALV视图的列标题
*----------------------------------------------------------------------*
*  入口参数：大写字段名、字段名的描述,列的宽度,参考的系统表字段名，参考
*            的系统表名（不需要的参数传为空）。
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_disp USING fu_name TYPE slis_fieldcat_alv-fieldname" 自定义数据库表字段
                    fu_disp TYPE slis_fieldcat_alv-seltext_l" 字段描述
*                    fu_colwidth TYPE slis_fieldcat_alv-outputlen" 输出列的宽度
                    fu_nozero TYPE slis_fieldcat_alv-no_zero" 是否显示前导零
                    fu_refname TYPE slis_fieldcat_alv-ref_fieldname" 参考的系统表字段
                    fu_reftab TYPE slis_fieldcat_alv-ref_tabname" 参考的系统表
                    fu_just   TYPE slis_fieldcat_alv-just.
  "形参类型与slis_fieldcat_alv中相应字段名的类型保持一致

  DATA: lw_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = fu_name.         " 自定义数据库表字段
  lw_fieldcat-ref_fieldname = fu_refname.  " 参考的系统表字段
  lw_fieldcat-ref_tabname = fu_reftab.     " 参考的系统表
  lw_fieldcat-seltext_l = fu_disp.         " 字段描述
*  lw_fieldcat-outputlen = fu_colwidth.     " 输出列的宽度
  lw_fieldcat-no_zero = fu_nozero.         " 是否显示前导零
  lw_fieldcat-just = fu_just.
  APPEND lw_fieldcat TO it_fieldcat.

ENDFORM.                    " FRM_DISP
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*  功能描述：显示ALV报表中的斑马线，设置ALV报表的第一列为CHECKBOX
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_layout .

*  wa_layout-colwidth_optimize = 'X'.     " ALV显示的列宽自动调整
  wa_layout-zebra             = 'X'.     " ALV中显示斑马线
  wa_layout-box_fieldname = 'CHECKBOX'.  " 第一列设置为CHECKBOX按钮
  wa_layout-coltab_fieldname = 'CELLCOLOR'.

ENDFORM.                    " FRM_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*  功能描述：采用html格式显示ALV表头标题
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM html_top_of_page USING p_cl_dd TYPE REF TO cl_dd_document  ##CALLED.

  DATA: m_p      TYPE i,
        m_buffer TYPE string,
        l_line   TYPE c LENGTH 1400,
        l_length TYPE i.

  m_buffer =
'<HTML><CENTER><H1><STRONG>库龄（呆滞料）查询报表</STRONG></H1></CENTER></HTML>'.
  CALL METHOD p_cl_dd->html_insert
    EXPORTING
      contents = m_buffer
    CHANGING
      position = m_p.
  g_day = sy-datum+6(2).
  CONCATENATE '<P ALIGN = CENTER>'
              sy-datum+0(4) '年' sy-datum+4(2) '月' sy-datum+6(2) '日' INTO m_buffer.
  CALL METHOD p_cl_dd->html_insert
    EXPORTING
      contents = m_buffer
    CHANGING
      position = m_p.


ENDFORM. "HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*  功能描述：输出ALV报表的视图
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_output .

  DATA: l_repid    LIKE sy-repid,
        gs_variant TYPE disvariant.

  gs_variant-report = sy-repid.
  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_pf_status_set    = 'FRM_SET_STATUS' "自定义按钮及事件
      i_callback_user_command     = 'FRM_USER_COMMAND'
      i_callback_program          = l_repid
      is_variant                  = gs_variant
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_html_height_top           = 26
      is_layout                   = wa_layout
      it_fieldcat                 = it_fieldcat[]
      i_save                      = 'A'
    TABLES
      t_outtab                    = ts_out
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " FRM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FRM_SET_STATUS
*&---------------------------------------------------------------------*
*  功能描述：在SE80中定义一个STANDARD_FULLSCREEN的状态栏
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_set_status USING rt_extab TYPE slis_t_extab.
  DATA: ls_extab TYPE slis_extab.
  CLEAR rt_extab.
  IF p_budat NE gv_last_day.
    ls_extab-fcode = '&DATA_SAVE'.
    APPEND ls_extab TO rt_extab.
  ENDIF.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab. "定义GUI状态，添加应用工具栏按钮

ENDFORM.                    " FRM_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*  功能描述：响应用户的操作
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM frm_user_command USING ucomm LIKE sy-ucomm
                         selfield TYPE slis_selfield..


  CASE sy-ucomm.    "sy-ucomm获得按钮的功能码

    WHEN 'BACK' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN '&DATA_SAVE'.
      DATA: lt_zmm012a TYPE TABLE OF zmm012a WITH HEADER LINE.
      lt_zmm012a-gjahr = p_budat(4).
      lt_zmm012a-monat = p_budat+4(2).
      LOOP AT ts_out INTO tl_out.
        MOVE-CORRESPONDING tl_out TO lt_zmm012a.
        APPEND lt_zmm012a.
      ENDLOOP.
      CHECK lt_zmm012a[] IS NOT INITIAL.
      MODIFY zmm012a FROM TABLE lt_zmm012a.
      COMMIT WORK.
      MESSAGE s000(oo) WITH '数据保存成功'.
  ENDCASE.

ENDFORM.                    " FRM_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  FRM_NUMERIC_TO_CHAR
*&---------------------------------------------------------------------*
*  功能描述：将金额数字转换为文本输出，并添加千位分隔符
*----------------------------------------------------------------------*
*  入口参数：金额数字
*  出口参数：文本
*----------------------------------------------------------------------*
FORM frm_numeric_to_char USING ft_num  TYPE tslxx12
                         CHANGING fc_char TYPE char31.

  DATA : lt_num TYPE TABLE OF string WITH HEADER LINE.
  DATA : l_num(50) TYPE c.         " 整数部分
  DATA : l_dec TYPE string.         " 小数部分
  DATA : l_len TYPE i.
  DATA : l_sep TYPE c VALUE '.' .

  DATA : l_temp_str01 TYPE string.
  DATA : l_temp_str02 TYPE c LENGTH 1.
  DATA : l_temp_str03 TYPE string.

  MOVE ft_num TO fc_char.

* 在字符串里加逗号
  SPLIT fc_char AT l_sep INTO TABLE lt_num IN CHARACTER MODE.

* 获取整数部分和小数部分
  LOOP AT lt_num.
    l_temp_str01 = lt_num.
    AT FIRST.
      l_num = l_temp_str01.
    ENDAT.
    AT LAST.
      l_dec = l_temp_str01.
    ENDAT.
  ENDLOOP.

  l_len = strlen( l_num ).

  DO l_len TIMES.
    l_len = l_len - 1.
    l_temp_str02 = l_num+l_len(1).
    CONCATENATE l_temp_str03 l_temp_str02 INTO l_temp_str03.
  ENDDO.

  l_num = l_temp_str03.

  CLEAR : lt_num.
  REFRESH : lt_num.

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = l_num
      delimiter           = ''
      outputlen           = 3
* IMPORTING
*     OUT_LINE1           =
*     OUT_LINE2           =
*     OUT_LINE3           =
    TABLES
      out_lines           = lt_num
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR : l_num.

  LOOP AT lt_num.
    CONCATENATE l_num ',' lt_num INTO l_num.
  ENDLOOP.

  SHIFT l_num BY 1 PLACES LEFT.

  l_len = strlen( l_num ).
  CLEAR : l_temp_str03.

  DO l_len TIMES.
    l_len = l_len - 1.
    l_temp_str02 = l_num+l_len(1).
    CONCATENATE l_temp_str03 l_temp_str02 INTO l_temp_str03.
  ENDDO.

  l_num = l_temp_str03.

* 重新赋值
  CLEAR : fc_char.
  CONCATENATE l_num '.' l_dec INTO fc_char.

  SHIFT fc_char RIGHT DELETING TRAILING space.

ENDFORM.                    " FRM_NUMERIC_TO_CHAR
" FRM_CELLCOLOR
