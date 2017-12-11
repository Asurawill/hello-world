*&------------------------------------------------------------------------*
*& Report  ZMM009
*&
*&-------------------------------------------------------------------------
*&-------------------------------------------------------------------------*
*& Date Created : 2015/3/7                                                 *
*& Created By   : 汉得-唐博                                                 *
*& Description  :采购订单交期跟踪查询报表                                    *
*&-------------------------------------------------------------------------*
*&变更记录：                                                                *
*&Date         Developer           ReqNo       Descriptions                *
*& ==========  ==================  ==========  ============================*
*& 2017-07-24  it02&魏云           ED1K906480 “查询优化：增加判断T_ALV不为空
*&-------------------------------------------------------------------------*
REPORT zmm009.

TABLES: ekko, ekpo, eket, eban.

SELECT-OPTIONS s_bukrs FOR ekko-bukrs OBLIGATORY."公司代码
SELECT-OPTIONS s_ekorg FOR ekko-ekorg."采购组织
SELECT-OPTIONS s_lifnr FOR ekko-lifnr."供应商
SELECT-OPTIONS s_ebeln FOR ekko-ebeln."采购订单编号
SELECT-OPTIONS s_werks FOR ekpo-werks."工厂
SELECT-OPTIONS s_afnam FOR eban-afnam."申请者
SELECT-OPTIONS s_banfn FOR eban-banfn."采购申请号
SELECT-OPTIONS s_matnr FOR ekpo-matnr."物料编码
SELECT-OPTIONS s_matkl FOR ekpo-matkl."物料组
SELECT-OPTIONS s_ekgrp FOR ekko-ekgrp."采购组
SELECT-OPTIONS s_bsart FOR ekko-bsart."采购订单类型
SELECT-OPTIONS s_eindt FOR eket-eindt."到货日期
SELECT-OPTIONS s_hfdat FOR eket-eindt."供应商回复的交货日期
SELECT-OPTIONS s_xddat FOR eket-eindt."采购订单的下达日期
SELECT-OPTIONS s_jhdat FOR eket-eindt."采购订单的交货日期
SELECT-OPTIONS s_rkdat FOR eket-eindt."实际入库日期

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001."交货状态
PARAMETER x_jhywc RADIOBUTTON GROUP g1."交货已完成
PARAMETER x_jhwwc RADIOBUTTON GROUP g1."交货未完成
PARAMETER x_jhall RADIOBUTTON GROUP g1 DEFAULT 'X'."全部
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002."开票状态
PARAMETER x_kpywc RADIOBUTTON GROUP g2."开票已完成
PARAMETER x_kpwwc RADIOBUTTON GROUP g2."开票未完成
PARAMETER x_kpall RADIOBUTTON GROUP g2 DEFAULT 'X'."全部
SELECTION-SCREEN END OF BLOCK b2.

DATA: BEGIN OF t_alv OCCURS 1,
        badat         TYPE eban-badat, "采购申请日期
        banfn         TYPE eban-banfn, "采购申请号
        bnfpo         TYPE eban-bnfpo, "采购申请行项目
        matnr         TYPE eban-matnr, "物料编码
        afnam         TYPE eban-afnam, "申请者
        "  MAKTX TYPE MAKT-MAKTX,"物料描述 改为取TXZ01即可150611
        txz01         TYPE ekpo-txz01, "物料描述
        menge_sg      TYPE eban-menge, "申购数量
        meins         TYPE eban-meins, "计量单位
        lfdat         TYPE eban-lfdat, "申购要求的到货日期
        vbeln         TYPE ebkn-vbeln, "销售订单号（项目号）
        vbelp         TYPE ebkn-vbelp, "销售订单行项目
        ebeln         TYPE ekpo-ebeln, "采购订单号
        ebelp         TYPE ekpo-ebelp, "采购订单行项目
        mblnr         TYPE mseg-mblnr, "物料凭证号 150526
        zeile         TYPE mseg-zeile , "物料凭证行项目 150525
        prueflos      TYPE qamb-prueflos, "检验批号
        netpr         TYPE ekpo-netpr, "采购凭证中的净价(以凭证货币计)
        peinh         TYPE ekpo-peinh, "价格单位
        kbetr         TYPE konv-kbetr, "采购凭证中的含税价(以凭证货币计)
        taxrate       TYPE i, "税率
        taxrate_text  TYPE char10, "税率(文本)
        lifnr         TYPE ekko-lifnr, "供应商
        name1         TYPE lfa1-name1, "供应商描述
        ekgrp         TYPE ekko-ekgrp, "采购组
        matkl         TYPE ekpo-matkl, "物料组
        bprme         TYPE ekpo-bprme, "采购单位
        menge_cg      TYPE ekpo-menge, "采购数量
        aedat         TYPE ekko-aedat, "采购订单下达日期
        bsart         TYPE ekko-bsart, "采购订单类型
        eindt         TYPE ekes-eindt, "供应商回复交期时间
        menge_hf      TYPE ekes-menge, "供应商回复数量
        gstrp         TYPE afko-gstrp, "计划上线日期
        budat_dh      TYPE mseg-budat_mkpf, "物料到货日期
        budat_rk      TYPE mseg-budat_mkpf, "物料实际入库时间
        menge_rk      TYPE mseg-menge, "物料实际入库数量
        xblnr_mkpf    TYPE mseg-xblnr_mkpf, "交货单号
        menge_ydh     TYPE ekbe-menge, "已到货数量
        menge_yth     TYPE ekbe-menge, "已退货数量
        menge_wdh     TYPE ekbe-menge, "未到货数量
        dmbtr_ydh     TYPE mseg-dmbtr, "已到货金额
        dmbtr_ydh_tax TYPE mseg-dmbtr, "已到货金额
        dmbtr_yth     TYPE mseg-dmbtr, "已退货金额
        dmbtr_wdh     TYPE mseg-dmbtr, "未到货金额
        waers         TYPE ekko-waers, "未到货金额
        wdcyyfx       TYPE char50, "未达成原因分析(影响)
*        SZCDYX TYPE CHAR50,"所造成的影响
        dmbtr_ykp     TYPE mseg-dmbtr, "已开票金额
        dmbtr_yfk     TYPE mseg-dmbtr, "已付款金额
        dhjsl         TYPE p DECIMALS 2, "到货及时率
        dhjsl_t       TYPE char20, "到货及时率
        eket_eindt    TYPE eket-eindt, "采购订单的交货日期
        knumv         TYPE ekko-knumv, "单据条件数
        lands         TYPE ekko-lands, "纳税返回国家
        mwskz         TYPE ekpo-mwskz, "销售/购买税代码
        txjcd         TYPE ekpo-txjcd, "地区税务代码
        bz_h          TYPE char100, "抬头备注
        bz_i          TYPE char100, "行备注
        loekz         TYPE ekpo-loekz, "删除标记
      END OF t_alv,
      t_ekbe      TYPE TABLE OF ekbe WITH HEADER LINE,
      t_ekes      TYPE TABLE OF ekes WITH HEADER LINE,
      t_eket      TYPE TABLE OF eket WITH HEADER LINE,
      t_qals      TYPE TABLE OF qals WITH HEADER LINE,
      t_mseg321   TYPE TABLE OF mseg WITH HEADER LINE,
      t_alv_final LIKE TABLE OF t_alv WITH HEADER LINE,
      t_alv_tmp   LIKE TABLE OF t_alv WITH HEADER LINE,
      t_alv_tmp2  LIKE TABLE OF t_alv WITH HEADER LINE,
      t_alv_tmp3  LIKE TABLE OF t_alv WITH HEADER LINE.

DATA: BEGIN OF t_bseg OCCURS 1,
        ebeln TYPE bseg-ebeln,
        ebelp TYPE bseg-ebelp,
        shkzg TYPE bseg-shkzg,
        dmbtr TYPE bseg-dmbtr,
        hkont TYPE bseg-hkont,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        zuonr TYPE bseg-zuonr,
      END OF t_bseg,
      t_bseg_ebeln LIKE TABLE OF t_bseg WITH HEADER LINE,
      t_bseg_po    TYPE TABLE OF zbseg_po WITH HEADER LINE.
DATA t_bseg_ykp LIKE TABLE OF t_bseg WITH HEADER LINE.
DATA t_bseg_yfk LIKE TABLE OF t_bseg WITH HEADER LINE.
DATA t_konv TYPE TABLE OF konv WITH HEADER LINE.
DATA t_ebkn TYPE TABLE OF ebkn WITH HEADER LINE.
"150605更改已开票金额逻辑，原从BSEG汇总有误现更改为rseg
DATA:BEGIN OF w_ebwrbtr ,
       ebeln LIKE bseg-ebeln,
       ebelp LIKE bseg-ebelp,
       wrbtr LIKE rseg-wrbtr,
     END OF w_ebwrbtr.
DATA: t_ebwrbtr LIKE TABLE OF w_ebwrbtr WITH HEADER LINE.
DATA: t_rseg  LIKE TABLE OF rseg WITH HEADER LINE.

START-OF-SELECTION.

  CLEAR: t_alv[], t_ekbe[], t_eket[], t_ekes[], t_qals[], t_mseg321[],
         t_bseg[], t_bseg_ykp[], t_bseg_yfk[].

  "基本数据
  SELECT
    c~badat "采购申请日期
    b~banfn "采购申请号
    b~bnfpo "采购申请的项目编号
    b~matnr "物料编码
   " E~MAKTX "物料描述
    b~txz01 "物料描述
    c~menge AS menge_sg "申购数量
    c~meins "计量单位
    c~lfdat "申购要求的到货日期
    c~afnam "申请者
*  B~VBELN "销售订单号（项目号）
*  B~VBELP "销售订单行项目
    b~ebeln "采购订单号
    b~ebelp "采购订单行项目号
    b~netpr "采购凭证中的净价(以凭证货币计)
    b~peinh "价格单位
    b~loekz "删除标记
    a~lifnr "供应商
    a~knumv "单据条件数
    a~lands "纳税返回国家
    b~mwskz "销售/购买税代码
    b~txjcd "地区税务代码
    d~name1 "供应商描述
    a~ekgrp "采购组
    b~matkl "物料组
    b~bprme "采购单位
    b~menge AS menge_cg "采购数量
    a~aedat "采购订单下达日期
    a~waers "币种
    a~bsart "采购订单类型
*  F~EINDT "供应商回复交期时间
*  F~MENGE AS MENGE_HF "供应商回复数量
    "计划上线日期
*  F~BLDAT "物料到货日期
    "物料实际入库时间
*  F~MENGE AS MENGE_YDH"已到货数量
    "未到货数量
*  F~DMBTR AS DMBTR_YDH"已到货金额
    "未到货金额
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln EQ b~ebeln
    LEFT JOIN eban AS c ON b~banfn EQ c~banfn AND b~bnfpo EQ c~bnfpo
    LEFT JOIN lfa1 AS d ON a~lifnr EQ d~lifnr
   " LEFT JOIN MAKT AS E ON B~MATNR EQ E~MATNR AND E~SPRAS EQ SY-LANGU
*  LEFT JOIN EKES AS F ON F~EBELN EQ B~EBELN AND F~EBELP EQ B~EBELP
*  LEFT JOIN EKBE AS F ON F~EBELN EQ B~EBELN AND F~EBELP EQ B~EBELP AND F~BWART IN ('101','102','161','122')
    INTO CORRESPONDING FIELDS OF TABLE t_alv
    WHERE a~bukrs IN s_bukrs "公司代码
    AND a~ekorg IN s_ekorg   "采购组织
    AND a~lifnr IN s_lifnr   "供应商
    AND a~ebeln IN s_ebeln   "采购订单编号
    AND b~werks IN s_werks   "工厂
    AND b~matnr IN s_matnr   "物料编码
    AND b~matkl IN s_matkl   "物料组
    AND a~ekgrp IN s_ekgrp   "采购组
    AND a~bsart IN s_bsart   "采购订单类型
  .

  IF s_afnam IS NOT INITIAL"申请者
    OR s_banfn IS NOT INITIAL. "申请号
    "150608 begin add it02  更改申请者可大小写匹配查询
    LOOP AT  s_afnam.
      IF s_afnam-low <> '' .
        TRANSLATE s_afnam-low TO UPPER CASE  .
        SHIFT s_afnam-low RIGHT DELETING TRAILING ''.
        SHIFT s_afnam-low LEFT DELETING LEADING '' .
      ENDIF.
      IF s_afnam-high <> '' .
        TRANSLATE s_afnam-high TO UPPER CASE  .
        SHIFT s_afnam-high RIGHT DELETING TRAILING ''.
        SHIFT s_afnam-high LEFT DELETING LEADING '' .
      ENDIF.
      MODIFY s_afnam.
    ENDLOOP.
    LOOP AT t_alv .
      IF t_alv-afnam NE ''.
        TRANSLATE t_alv-afnam TO UPPER CASE  .
        SHIFT t_alv-afnam RIGHT DELETING TRAILING ''.
        SHIFT t_alv-afnam LEFT DELETING LEADING '' .
        MODIFY t_alv.
      ENDIF.
    ENDLOOP.
    "150608 end  add it02  更改申请者可大小写匹配查询
    DELETE t_alv WHERE afnam NOT IN s_afnam
      OR banfn NOT IN s_banfn.
  ENDIF.

  IF t_alv[] IS INITIAL.
    MESSAGE s003(zfico01) DISPLAY LIKE 'E'.
*   无有效数据,请检查选择屏幕！
    LEAVE LIST-PROCESSING.
  ENDIF.

  CLEAR t_ebkn[].

  IF t_alv[] IS NOT INITIAL.
    SELECT * FROM ebkn
   INTO CORRESPONDING FIELDS OF TABLE t_ebkn
   FOR ALL ENTRIES IN t_alv
   WHERE banfn EQ t_alv-banfn
   AND bnfpo EQ t_alv-bnfpo
 AND vbeln NE space.

    SORT t_ebkn BY banfn bnfpo.

    CLEAR t_konv[].

    SELECT * FROM konv
      INTO CORRESPONDING FIELDS OF TABLE t_konv
      FOR ALL ENTRIES IN t_alv
      WHERE knumv EQ t_alv-knumv
      AND kschl IN ('PB00', 'PBXX')
      AND kinak EQ space
    .

    SORT t_konv BY knumv kposn.

    SELECT * FROM ekbe
      INTO CORRESPONDING FIELDS OF TABLE t_ekbe
      FOR ALL ENTRIES IN t_alv
      WHERE ebeln EQ t_alv-ebeln
      AND ebelp EQ t_alv-ebelp
      AND bwart IN ('101','102','161','122','103','104')
    .

*IF T_EKBE[] IS NOT INITIAL.
*  SELECT * FROM MSEG
*ENDIF.

    SELECT * FROM ekes
      INTO CORRESPONDING FIELDS OF TABLE t_ekes
      FOR ALL ENTRIES IN t_alv
      WHERE ebeln EQ t_alv-ebeln
    AND ebelp EQ t_alv-ebelp.

    SELECT * FROM eket
      INTO CORRESPONDING FIELDS OF TABLE t_eket
      FOR ALL ENTRIES IN t_alv
      WHERE ebeln EQ t_alv-ebeln
      AND ebelp EQ t_alv-ebelp
    AND eindt NE space.

    SORT t_eket BY ebeln ebelp.
*SELECT * FROM MSEG
*  INTO CORRESPONDING FIELDS OF TABLE T_MSEG321
*  FOR ALL ENTRIES IN T_ALV
*  WHERE EBELN EQ T_ALV-EBELN
*  AND EBELP EQ T_ALV-EBELP
*  AND BWART EQ '321'
*  AND SHKZG EQ 'S'
*  AND MENGE NE SPACE.

    CLEAR: t_bseg_ebeln[],t_bseg[].

    SELECT * FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE t_bseg_ebeln
      FOR ALL ENTRIES IN t_alv
      WHERE ebeln EQ t_alv-ebeln
      AND ebelp EQ t_alv-ebelp
    .

    SORT t_bseg_ebeln BY bukrs gjahr belnr.
    "it02 insert begin 150605 读取RSEG状态为5过账表数据 已开票数据
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_rseg
      FROM  rbkp AS a
       INNER JOIN rseg AS b  ON a~belnr = b~belnr AND a~gjahr = b~gjahr
     FOR ALL ENTRIES IN t_alv
      WHERE b~ebeln EQ t_alv-ebeln
      AND b~ebelp EQ t_alv-ebelp
    AND a~rbstat = '5'.
    .
    SORT t_rseg BY ebeln ebelp .
    LOOP AT t_rseg .
      CLEAR w_ebwrbtr.
      w_ebwrbtr-ebeln = t_rseg-ebeln.
      w_ebwrbtr-ebelp = t_rseg-ebelp.
      IF t_rseg-shkzg = 'S'.
        w_ebwrbtr-wrbtr = t_rseg-wrbtr.
      ELSE .
        w_ebwrbtr-wrbtr = - t_rseg-wrbtr .
      ENDIF.
      COLLECT w_ebwrbtr INTO t_ebwrbtr.
    ENDLOOP.
    SORT t_ebwrbtr BY ebeln ebelp .
    "IT02 end .0605
    "
    "IT02 insert begin 150526
    "DATA: T_QALS  LIKE TABLE OF QALS WITH HEADER LINE.
    DATA: t_qamb LIKE TABLE OF qamb WITH HEADER LINE.
    DATA: t_321 LIKE TABLE OF mseg WITH HEADER LINE.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_qals
      FROM qals
      FOR ALL ENTRIES IN t_alv
    WHERE ebeln EQ t_alv-ebeln.

  ENDIF.

  IF t_qals IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_qamb
   FROM qamb
   FOR ALL ENTRIES IN  t_qals
 WHERE  prueflos EQ   t_qals-prueflos.
  ENDIF.


  LOOP AT t_qals.
    DELETE  t_qamb WHERE mblnr = t_qals-mblnr.
  ENDLOOP.

  IF t_qamb IS NOT INITIAL.
    SELECT * FROM mseg
 INTO CORRESPONDING FIELDS OF TABLE t_321
 FOR ALL ENTRIES IN t_qamb
 WHERE mblnr EQ t_qamb-mblnr
" AND ZEILE EQ T_QAMB-ZEILE
 AND ( bwart EQ '321'
 OR bwart EQ '105')
 AND shkzg EQ 'H'
AND menge NE space.
  ENDIF.


  "it02 insert end 150526

  IF t_bseg_ebeln[] IS NOT INITIAL."已开票
    SELECT * FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE t_bseg
      FOR ALL ENTRIES IN t_bseg_ebeln
      WHERE bukrs EQ t_bseg_ebeln-bukrs
      AND gjahr EQ t_bseg_ebeln-gjahr
      AND belnr EQ t_bseg_ebeln-belnr
      AND ( hkont LIKE '220201%'
      OR hkont LIKE '220202%' )
    .
  ENDIF.
  "IT02 150605 注释 begin ,因已开票数据改为 RSEG 表取得
*LOOP AT T_BSEG.
*  IF T_BSEG-SHKZG EQ 'S'.
*    MULTIPLY T_BSEG-DMBTR BY -1.
*  ENDIF.
*  READ TABLE T_BSEG_EBELN WITH KEY BUKRS = T_BSEG-BUKRS GJAHR = T_BSEG-GJAHR BELNR = T_BSEG-BELNR BINARY SEARCH."设置采购订单号
*  IF SY-SUBRC EQ 0.
*    T_BSEG-EBELN = T_BSEG_EBELN-EBELN.
*    T_BSEG-EBELP = T_BSEG_EBELN-EBELP.
*  ENDIF.
*  MODIFY T_BSEG.
*ENDLOOP.
*
*SORT T_BSEG BY EBELN EBELP.
*
*LOOP AT T_BSEG.
*  AT END OF EBELP.
*    SUM.
*    APPEND T_BSEG TO T_BSEG_YKP.
*  ENDAT.
*ENDLOOP.
  "IT02 150605 注释 end
  IF t_alv[] IS NOT INITIAL.
    CLEAR: t_bseg_ebeln[],t_bseg_po[], t_bseg[].

    SELECT * FROM zbseg_po
      INTO CORRESPONDING FIELDS OF TABLE t_bseg_po "付款的凭证
      FOR ALL ENTRIES IN t_alv
*  WHERE ZUONR LIKE 'PO%'
      WHERE zpo EQ t_alv-ebeln.

  ENDIF.

  LOOP AT t_bseg_po.
    MOVE-CORRESPONDING t_bseg_po TO t_bseg_ebeln.
    t_bseg_ebeln-ebeln = t_bseg_po-zpo.
    APPEND t_bseg_ebeln.
    CLEAR t_bseg_ebeln.
  ENDLOOP.

  IF t_bseg_ebeln[] IS NOT INITIAL."已付款
    SELECT * FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE t_bseg
      FOR ALL ENTRIES IN t_bseg_ebeln
      WHERE bukrs EQ t_bseg_ebeln-bukrs
      AND gjahr EQ t_bseg_ebeln-gjahr
      AND belnr EQ t_bseg_ebeln-belnr
      AND ( hkont LIKE '112301%'
      OR hkont LIKE '112302%' ).
  ENDIF.

  LOOP AT t_bseg.
    IF t_bseg-shkzg EQ 'H'.
      MULTIPLY t_bseg-dmbtr BY -1.
    ENDIF.
    READ TABLE t_bseg_ebeln WITH KEY bukrs = t_bseg-bukrs gjahr = t_bseg-gjahr belnr = t_bseg-belnr BINARY SEARCH."设置采购订单号
    IF sy-subrc EQ 0.
      t_bseg-ebeln = t_bseg_ebeln-ebeln.
*    T_BSEG-EBELP = T_BSEG_EBELN-EBELP.
    ENDIF.
    MODIFY t_bseg.
  ENDLOOP.

if t_alv[] is not initial.
   SELECT ebeln
         zsqfkje AS dmbtr
         belnr_f AS belnr
         gjahr_f AS gjahr_f
         bukrs
    FROM zfi006
    APPENDING CORRESPONDING FIELDS OF TABLE t_bseg
    FOR ALL ENTRIES IN t_alv
    WHERE zcljd = '3'
  AND ebeln = t_alv-ebeln.
endif.


*SORT T_BSEG BY BUKRS BELNR .
  SORT t_bseg_yfk BY ebeln ebelp.

  LOOP AT t_bseg.
    AT END OF ebelp.
      SUM.
      APPEND t_bseg TO t_bseg_yfk.
    ENDAT.
  ENDLOOP.

  DATA wdh_menge LIKE t_alv-menge_cg.
  DATA l_menge_ydh LIKE t_alv-menge_cg."已到货累计数量
  DATA l_menge_hf LIKE t_alv-menge_cg."回复数量
  DATA l_js TYPE i. "及时到货
  DATA l_bjs TYPE i. "不及时到货
  DATA l_hasitem TYPE c.
  DATA l_tabix TYPE sy-tabix.
  DATA l_lines TYPE i.
  DATA t_ftaxp TYPE TABLE OF ftaxp WITH HEADER LINE.

  DATA amount_tax LIKE t_alv_final-dmbtr_yfk.
  DATA l_tdname TYPE thead-tdname.

  LOOP AT t_alv.
    CLEAR: l_hasitem, l_js, l_bjs, l_menge_ydh, l_menge_hf.
    wdh_menge = t_alv-menge_cg."采购数量
    IF t_alv-peinh IS INITIAL.
      t_alv-peinh = 1.
    ENDIF.
    IF t_alv-loekz NE space.
      t_alv-loekz = 'X'.
    ENDIF.
*  L_DMBTR = T_ALV-MENGE_CG * T_ALV-NETPR.
    CLEAR: t_alv_tmp[],t_alv_tmp2[],t_alv_tmp3[].
    READ TABLE t_konv WITH KEY knumv = t_alv-knumv kposn = t_alv-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      t_alv-kbetr = t_konv-kbetr.
    ENDIF.
    READ TABLE t_ebkn WITH KEY banfn = t_alv-banfn bnfpo = t_alv-bnfpo BINARY SEARCH.
    IF sy-subrc EQ 0.
      t_alv-vbeln = t_ebkn-vbeln.
      t_alv-vbelp = t_ebkn-vbelp.
    ENDIF.
    "IT02 begin 150605
*  READ TABLE T_BSEG_YKP WITH KEY EBELN = T_ALV-EBELN
*    EBELP = T_ALV-EBELP BINARY SEARCH.
*  IF SY-SUBRC EQ 0.
*    T_ALV-DMBTR_YKP = T_BSEG_YKP-DMBTR.
*  ENDIF.
    "IT02 end .150605
    "IT02 modify 已开票金额赋值 begin  读取已开票金额 150605
    READ TABLE t_ebwrbtr WITH KEY ebeln = t_alv-ebeln  ebelp = t_alv-ebelp .
    IF sy-subrc = 0 .
      t_alv-dmbtr_ykp = t_ebwrbtr-wrbtr.
    ENDIF.
    "it02 end 150605
    IF t_alv-lands IS NOT INITIAL.
      CLEAR t_ftaxp[].
      CALL FUNCTION 'GET_TAX_PERCENTAGE'
        EXPORTING
          aland   = t_alv-lands
          datab   = sy-datum
          mwskz   = t_alv-mwskz
          txjcd   = t_alv-txjcd
*         EXPORT  = ' '
        TABLES
          t_ftaxp = t_ftaxp[].
      IF t_ftaxp[] IS NOT INITIAL.
        READ TABLE t_ftaxp INDEX 1.
        t_alv-taxrate = t_ftaxp-kbetr / 10.
        WRITE t_alv-taxrate TO t_alv-taxrate_text.
        CONCATENATE t_alv-taxrate_text '%' INTO t_alv-taxrate_text.
      ENDIF.
    ENDIF.
    CLEAR l_tdname.
    CONCATENATE t_alv-ebeln t_alv-ebelp INTO l_tdname.
    "获取抬头备注
    PERFORM read_text USING 'F00' t_alv-ebeln 'EKKO' '0' CHANGING t_alv-bz_h."抬头备注
*  "获取行备注
    PERFORM read_text USING 'F01' l_tdname 'EKPO' '0' CHANGING t_alv-bz_i."行备注
    LOOP AT t_ekes WHERE ebeln EQ t_alv-ebeln AND ebelp EQ t_alv-ebelp.
      l_hasitem = 'X'.
      t_alv-eindt = t_ekes-eindt. "供应商回复交期时间
      t_alv-menge_hf = t_ekes-menge. "供应商回复数量
      ADD t_ekes-menge TO l_menge_hf.
*    APPEND T_ALV TO T_ALV_FINAL.
      "获取已到货当前累计数量
      CLEAR l_menge_ydh.
      LOOP AT t_ekbe WHERE ebeln EQ t_alv-ebeln AND ebelp EQ t_alv-ebelp
        AND bldat LE t_ekes-eindt.     "已经到货的数量
*      ADD T_EKES-MENGE TO L_MENGE_HF. "供应商回复数量
        IF t_ekbe-shkzg EQ 'S'.
          ADD t_ekbe-menge TO l_menge_ydh.
        ELSE.
          SUBTRACT t_ekbe-menge FROM l_menge_ydh.
        ENDIF.
      ENDLOOP.
      IF l_menge_ydh GE l_menge_hf. "及时到货
        ADD 1 TO l_js.
      ELSE.
        ADD 1 TO l_bjs.
      ENDIF.
      APPEND t_alv TO t_alv_tmp.
      CLEAR: t_alv-eindt, t_alv-menge_hf.
    ENDLOOP.
    IF l_js > 0 OR l_bjs > 0."已经收货
      t_alv-dhjsl = l_js + l_bjs.
      t_alv-dhjsl = l_js / t_alv-dhjsl * 100."到货及时率
      t_alv-dhjsl_t = t_alv-dhjsl.
      CONCATENATE t_alv-dhjsl_t '%' INTO t_alv-dhjsl_t.
    ELSE.
      t_alv-dhjsl_t = '100.00%'.
    ENDIF.
    SORT t_alv_tmp BY eindt.
    LOOP AT t_alv_tmp.
      t_alv_tmp-dhjsl = t_alv-dhjsl."到货及时率
      t_alv_tmp-dhjsl_t = t_alv-dhjsl_t. "到货及时率文本
      READ TABLE t_ekbe WITH KEY ebeln = t_alv-ebeln ebelp = t_alv-ebelp."如果没有到货发生，设置未到货数量和未到货金额
      IF sy-subrc NE 0.
        t_alv_tmp-menge_wdh = wdh_menge."未到货数量
        t_alv_tmp-dmbtr_wdh = wdh_menge * t_alv-netpr / t_alv-peinh."未到货金额
      ENDIF.
      MODIFY t_alv_tmp.
    ENDLOOP.

    LOOP AT t_ekbe WHERE ebeln EQ t_alv-ebeln AND ebelp EQ t_alv-ebelp."“已到货”项目
      l_hasitem = 'X'.
      t_alv-budat_dh = t_ekbe-bldat.

      t_alv-xblnr_mkpf = t_ekbe-xblnr."参照
      IF t_ekbe-shkzg EQ 'S'.
        IF t_ekbe-bwart = '103'.                       " ADD BY HANDWY 2015-8-8
          SUBTRACT t_ekbe-wesbs FROM wdh_menge.
          t_alv-menge_ydh = t_ekbe-wesbs.
        ELSE.
          SUBTRACT t_ekbe-menge FROM wdh_menge.
          t_alv-menge_ydh = t_ekbe-menge.
        ENDIF.
        t_alv-dmbtr_ydh = t_alv-menge_ydh * t_alv-netpr / t_alv-peinh."T_EKBE-DMBTR.
        t_alv-dmbtr_ydh_tax = t_alv-menge_ydh * t_alv-kbetr / t_alv-peinh."含税已收货数量
      ELSE.
        IF t_ekbe-bwart = '104'.
          ADD t_ekbe-wesbs TO wdh_menge.              "ADD BY HANWY 2015-8-8
          t_alv-menge_yth = t_ekbe-wesbs.
        ELSE.
          ADD t_ekbe-menge TO wdh_menge.
          t_alv-menge_yth = t_ekbe-menge.
        ENDIF.
        t_alv-dmbtr_yth = t_alv-menge_yth * t_alv-netpr / t_alv-peinh."T_EKBE-DMBTR.
      ENDIF.
      t_alv-menge_wdh = wdh_menge.
      t_alv-dmbtr_wdh = wdh_menge * t_alv-netpr / t_alv-peinh.
      t_alv-mblnr = t_ekbe-belnr.
      t_alv-zeile = t_ekbe-buzei.
      READ TABLE t_qals WITH KEY mblnr = t_alv-mblnr  zeile = t_alv-zeile.
      IF sy-subrc = 0 .
        t_alv-prueflos = t_qals-prueflos.
      ENDIF.
      APPEND t_alv TO t_alv_tmp2.
      CLEAR: t_alv-budat_dh, t_alv-menge_ydh, t_alv-dmbtr_ydh, t_alv-dmbtr_ydh_tax, t_alv-menge_yth, t_alv-dmbtr_yth, t_alv-menge_wdh, t_alv-dmbtr_wdh.
    ENDLOOP.
    SORT t_alv_tmp2 BY budat_dh.

    LOOP AT t_mseg321 WHERE ebeln EQ t_alv-ebeln AND ebelp EQ t_alv-ebelp."“物料实际入库时间”项目
      l_hasitem = 'X'.
      t_alv-budat_rk = t_mseg321-budat_mkpf. "物料实际入库时间
      t_alv-menge_rk = t_mseg321-menge. "物料实际入库数量
*    APPEND T_ALV TO T_ALV_FINAL.
      APPEND t_alv TO t_alv_tmp3.
      CLEAR: t_alv-budat_rk, t_alv-menge_rk.
    ENDLOOP.
    SORT t_alv_tmp3 BY budat_rk.

    LOOP AT t_alv_tmp2."添加“已到货”项目
      l_tabix = sy-tabix.
      l_lines = lines( t_alv_tmp[] ).
      IF l_lines GE l_tabix.
        READ TABLE t_alv_tmp INDEX l_tabix.
        IF sy-subrc EQ 0.
          t_alv_tmp-xblnr_mkpf  = t_alv_tmp2-xblnr_mkpf. "交货单号
          t_alv_tmp-budat_dh  = t_alv_tmp2-budat_dh. "到货日期
          t_alv_tmp-menge_wdh  = t_alv_tmp2-menge_wdh. "未到货数量
          t_alv_tmp-dmbtr_wdh = t_alv_tmp2-dmbtr_wdh. "未到货金额
          t_alv_tmp-menge_ydh  = t_alv_tmp2-menge_ydh. "已到货数量
          t_alv_tmp-dmbtr_ydh = t_alv_tmp2-dmbtr_ydh. "已到货金额
          t_alv_tmp-dmbtr_ydh_tax = t_alv_tmp2-dmbtr_ydh_tax. "已到货金额（含税）
          IF t_alv_tmp-bsart EQ 'Z02'
            OR t_alv_tmp-bsart EQ 'Z04'."对于无需质检的项目，实际入库数量 = 已到货数量
            t_alv_tmp-budat_rk = t_alv_tmp-budat_dh."到货日期
            t_alv_tmp-menge_rk = t_alv_tmp-menge_ydh."已到货数量
          ENDIF.

          MODIFY t_alv_tmp INDEX l_tabix.
        ENDIF.
      ELSE.
        t_alv_tmp2-dhjsl_t = t_alv-dhjsl_t.
        APPEND t_alv_tmp2 TO t_alv_tmp.
      ENDIF.
    ENDLOOP.

    LOOP AT t_alv_tmp3."添加“物料实际入库时间”项目
      l_tabix = sy-tabix.
      l_lines = lines( t_alv_tmp[] ).
      IF l_lines GE l_tabix.
        READ TABLE t_alv_tmp INDEX l_tabix.
        IF sy-subrc EQ 0.
          t_alv_tmp-budat_rk  = t_alv_tmp3-budat_rk. "物料实际入库时间
          t_alv_tmp-menge_rk = t_alv_tmp3-menge_rk. "物料实际入库数量
          MODIFY t_alv_tmp INDEX l_tabix.
        ENDIF.
      ELSE.
        t_alv_tmp3-dhjsl_t = t_alv-dhjsl_t.
        APPEND t_alv_tmp3 TO t_alv_tmp.
      ENDIF.
    ENDLOOP.
    "根据开票状态筛选
    amount_tax = t_alv-menge_cg * t_alv-kbetr / t_alv-peinh.
    IF ( x_kpywc EQ 'X' AND t_alv-dmbtr_ykp LT amount_tax )"开票已完成
      OR ( x_kpwwc EQ 'X' AND t_alv-dmbtr_ykp GE amount_tax )."开票未完成
      CONTINUE.
    ENDIF.
    "按照“交货已完成、交货未完成、全部”的选项来显示结果
    IF x_jhall EQ 'X'"全部
      OR ( x_jhywc EQ 'X' AND wdh_menge IS INITIAL )"交货已完成
      OR ( x_jhwwc EQ 'X' AND wdh_menge IS NOT INITIAL )"交货未完成
      .
      APPEND LINES OF t_alv_tmp TO t_alv_final.
      IF l_hasitem IS INITIAL.
        t_alv-menge_wdh = wdh_menge."未到货数量
        t_alv-dmbtr_wdh = wdh_menge * t_alv-netpr / t_alv-peinh."未到货金额
        APPEND t_alv TO t_alv_final.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "实际入库日期、入库数量 150526 BEGIN
  LOOP AT t_alv_final.
    READ TABLE t_qamb WITH KEY prueflos = t_alv_final-prueflos.
    IF sy-subrc = 0.
      t_alv_final-mblnr = t_qamb-mblnr.
      t_alv_final-zeile = t_qamb-zeile.

    ENDIF.
    READ TABLE t_321 WITH KEY mblnr = t_alv_final-mblnr zeile = t_alv_final-zeile.
    IF sy-subrc = 0 .
      t_alv_final-budat_rk = t_321-budat_mkpf .
      t_alv_final-menge_rk = t_321-menge.
    ENDIF.
    MODIFY t_alv_final.
  ENDLOOP.

  "实际入库日期、入库数量 150526 END
  "已开票金额 & 已付款金额
  LOOP AT t_alv_final.
    READ TABLE t_bseg_yfk WITH KEY ebeln = t_alv_final-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      t_alv_final-dmbtr_yfk = t_bseg_yfk-dmbtr.
    ENDIF.
    READ TABLE t_eket WITH KEY ebeln = t_alv_final-ebeln
      ebelp = t_alv_final-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      t_alv_final-eket_eindt = t_eket-eindt.
    ENDIF.
    IF t_alv_final-eindt NOT IN s_hfdat "供应商回复的交货日期
      OR t_alv_final-budat_dh NOT IN s_eindt "到货日期
      OR t_alv_final-aedat NOT IN s_xddat "采购订单的下达日期
      OR t_alv_final-eket_eindt NOT IN s_jhdat "采购订单的交货日期
      OR t_alv_final-budat_rk NOT IN s_rkdat. "实际入库日期
      DELETE t_alv_final.
    ELSE.
      MODIFY t_alv_final.
    ENDIF.
  ENDLOOP.

  "设置未达成原因分析（影响）
  LOOP AT t_alv_final.
    PERFORM get_text USING 'F01' CHANGING t_alv_final-wdcyyfx.
    MODIFY t_alv_final.
  ENDLOOP.

  DATA is_layout TYPE slis_layout_alv.
  DATA it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  CLEAR: is_layout,it_fieldcat[].
  is_layout-colwidth_optimize = 'X'.

  PERFORM append_fieldcat USING:
    'BADAT' '采购申请日期' '' ''  '' '',
    'BANFN' '采购申请号' '' ''  '' '',
    'BNFPO' '采购申请行项目' '' ''  '' '',
    'AFNAM' '申请者' '' ''  '' '',
    'MATNR' '物料编码' '' ''  '' '',
   " 'MAKTX' '物料描述' '' ''  '' '',
    'TXZ01' '物料描述' '' ''  '' '',
    'MENGE_SG' '申购数量' '' ''  '' 'BPRME',
    'MEINS' '计量单位' '' ''  '' '',
    'LFDAT' '申购要求的到货日期' '' ''  '' '',
    'VBELN' '销售订单号（项目号）' '' ''  '' '',
    'VBELP' '销售订单行项目' '' ''  '' '',
    'LIFNR' '供应商' '' ''  '' '',
    'NAME1' '供应商描述' '' ''  '' '',
    'EBELN' '采购订单号' '' ''  '' '',
    'EBELP' '采购订单行项目号' '' ''  '' '',
    'EKGRP' '采购组' '' ''  '' '',
    'MATKL' '物料组' '' ''  '' '',
    'BPRME' '采购单位' '' ''  '' '',
    'MENGE_CG' '采购数量' '' ''  '' 'BPRME',
    'NETPR' '未税价格' '' '' '' '',
    'KBETR' '含税价格' '' '' '' '',
    'PEINH' '价格单位' '' '' '' '',
    'TAXRATE_TEXT' '税率' '' '' '' '',
    'AEDAT' '采购订单下达日期' '' ''  '' '',
    'EKET_EINDT' '采购订单的交货日期' '' ''  '' '',
    'BUDAT_DH' '物料到货日期' '' ''  '' '',
    'EINDT' '供应商回复交期时间' '' ''  '' '',
    'MENGE_HF' '供应商回复数量' '' ''  '' 'BPRME',
*  'GSTRP' '计划上线日期' '' ''  '' '',
    'BUDAT_RK' '物料实际入库日期' '' '' '' '',
    'MENGE_RK' '物料实际入库数量' '' '' '' 'BPRME',
    'XBLNR_MKPF' '交货单号' '' '' '' '',
    'MENGE_YDH' '已收货数量' '' ''  '' 'BPRME',
    'MENGE_YTH' '已退货数量' '' ''  '' 'BPRME',
    'MENGE_WDH' '未到货数量' '' ''  '' 'BPRME',
    'DMBTR_YDH' '未税已收货金额' '' ''  'WAERS' '',
    'DMBTR_YDH_TAX' '含税已收货金额' '' ''  'WAERS' '' ,
*  'DMBTR_YTH' '已退货金额' '' ''  'WAERS',
*  'DMBTR_WDH' '未到货金额' '' ''  'WAERS',
    'WAERS' '币种' '' ''  '' '',
    'DMBTR_YKP' '已开票金额' '' '' 'WAERS' '',
    'DMBTR_YFK' '已付款金额' '' '' 'WAERS' '',
*  'WDCYYFX' '未达成原因分析(影响)' '' ''  '' '',
    'DHJSL_T' '到货及时率' '' ''  '' '',
    'BZ_H' '抬头备注' '' ''  '' '',
    'BZ_I' '行备注' '' ''  '' '',
    'LOEKZ' '删除标记' '' ''  '' ''.
*  'SZCDYX' '所造成的影响' '' ''  ''
  .

  AUTHORITY-CHECK OBJECT 'ZMM_JE'"如果有这个权限，则不显示金额
           ID 'ACTVT' FIELD '03'.
  IF sy-subrc EQ 0.
    DELETE it_fieldcat WHERE fieldname CP 'DMBTR*' OR fieldname EQ 'WAERS'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = is_layout
      it_fieldcat        = it_fieldcat[]
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_SO_GRAPHICS     =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = t_alv_final[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
*&---------------------------------------------------------------------*
*&      FORM  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->NAME   TEXT
*      -->TEXT   TEXT
*      -->REF_TABNAME     TEXT
*      -->REF_FIELDNAME   TEXT
*----------------------------------------------------------------------*
FORM append_fieldcat  USING name
                            text
                            ref_tabname
                            ref_fieldname
                            cfieldname
                            qfieldname.
  it_fieldcat-fieldname = name.
  it_fieldcat-seltext_l    =
  it_fieldcat-seltext_m    =
  it_fieldcat-seltext_s    =
  it_fieldcat-reptext_ddic = text.
  it_fieldcat-ref_tabname = ref_tabname.
  it_fieldcat-ref_fieldname = ref_fieldname.
  it_fieldcat-cfieldname = cfieldname.
  it_fieldcat-qfieldname = qfieldname.
*  IT_FIELDCAT-no_zero = 'X'.
  APPEND it_fieldcat.
  CLEAR it_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT
**&---------------------------------------------------------------------*
**&      FORM  USER_COMMAND
**&---------------------------------------------------------------------*
**       TEXT
**----------------------------------------------------------------------*
**      -->R_UCOMM   TEXT
**      -->RS_SELFIELD   TEXT
**----------------------------------------------------------------------*
*FORM USER_COMMAND USING R_UCOMM TYPE SY-UCOMM
*                        RS_SELFIELD TYPE SLIS_SELFIELD.
*  CASE R_UCOMM.
**双击ALV行
*  WHEN '&IC1'.
*    READ TABLE T_ALV INDEX RS_SELFIELD-TABINDEX.
*    IF SY-SUBRC = 0.
*      SET PARAMETER ID 'AUN' FIELD T_ALV-VBELN.
*      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*    ENDIF.
*  ENDCASE.
*ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID   text
*      <--P_TEXT  text
*----------------------------------------------------------------------*
FORM get_text  USING    VALUE(p_id)
               CHANGING p_text.
  DATA l_tdname TYPE thead-tdname.
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  CONCATENATE t_alv_final-ebeln t_alv_final-ebelp INTO l_tdname.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = p_id
      language                = sy-langu
      name                    = l_tdname
      object                  = 'EKPO'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = t_tline[]
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
  ENDIF.
  IF t_tline[] IS NOT INITIAL.
    READ TABLE t_tline INDEX 1.
    p_text = t_tline-tdline.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ID   text
*      -->NAME  text
*      -->OBJECT   text
*      <--TDLINE  text
*----------------------------------------------------------------------*
FORM read_text  USING    VALUE(id)
                         VALUE(name)
                         VALUE(object)
                         VALUE(ind)"第IND行，若IND=0，则取全部
                CHANGING tdline.
  DATA t_tline TYPE TABLE OF tline WITH HEADER LINE.
  DATA tdname TYPE thead-tdname.
  tdname = name.
  CLEAR: tdline, t_tline[].
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = id
      language                = sy-langu
      name                    = tdname
      object                  = object
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = t_tline[]
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
*Implement suitable error handling here
  ENDIF.
  IF t_tline[] IS NOT INITIAL.
    IF ind GT 0.
      READ TABLE t_tline INDEX ind.
      IF sy-subrc EQ 0.
        tdline = t_tline-tdline. "文本内容
      ENDIF.
    ELSE.
      LOOP AT t_tline.
        IF tdline IS INITIAL.
          tdline = t_tline-tdline.
        ELSE.
          CONCATENATE tdline t_tline-tdline INTO tdline SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
