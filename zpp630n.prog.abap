REPORT ZPP630N.
*--------------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Report  ZPP606
*&
*&---------------------------------------------------------------------*
*& NAME:成品BOM展开可用量查询
*& CREAT BY :刘源鑫
*&modify by :黄鑫鹏
*& 2009/2/07
* modify:2009.8.12 于慧 ：将主数据中 “散装物料”显示在zcs13n 结果中
* modify by: 熊婧婧 2009/12/14 将物料主数据中的安全库存值取出来
* modify by:于慧 2010、4、19 显示采购BOM
*}   INSERT
*&---------------------------------------------------------------------*


TABLES :RESB,MARD,MAKT,STPOX,MAST,MARA ,AFPO,MARC.

************************************************************************
*ALV层级关系定义
************************************************************************
TYPE-POOLS: SLIS.
DATA: WT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      WT_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      WT_EVENTS   TYPE SLIS_T_EVENT.
DATA: WS_EVENTS LIKE LINE OF WT_EVENTS.
************************************************************************
*DATA
**************************************************************
DATA: BEGIN OF T_ITEMS OCCURS 0,
        MATNR         LIKE MARA-MATNR, "父件物料编码
        MAKTX         LIKE MAKT-MAKTX,  "父件物料描述
        WERKS         LIKE  MAST-WERKS,
        INDEX         TYPE I,
        STUFE_OUT(12) TYPE C,
        STUFE         TYPE STPOX-STUFE,
        POSNR         LIKE STPOX-POSNR,
        POSTP         LIKE STPOX-POSTP,
        OJTXP         LIKE STPOX-OJTXP, "行项目
        IDNRK         LIKE MARA-MATNR, "子件物料编码
        MNGKO         LIKE STPOX-MNGKO, "组件数量  ？？？
        MEINS         LIKE STPOX-MEINS, "子件计量单位
        DUMPS         LIKE STPOX-DUMPS,  "虚拟项
*      DISMM    LIKE STPOX-DISMM,  "物料需求计划类型
        LGORT         LIKE STPOX-LGORT,  "物料需求计划类型
        KZAUS         LIKE STPOX-KZAUS,  "中止指示符
        NFMAT         LIKE STPOX-NFMAT,  "后续物料
        KZNFP         LIKE STPOX-KZNFP,  "后继项目
        RGEKZ         LIKE STPOX-RGEKZ,  "反冲
        ALPGR         LIKE STPOX-ALPGR,  "组
        ALPRF         LIKE STPOX-ALPRF,  "评比订单
        LABST         LIKE MARD-LABST, "子件库存量
        VMENG         LIKE RESB-VMENG,  "已分配量
        MENGE_S       LIKE RESB-VMENG,  "剩余可用量
        DATUV         LIKE STPOX-DATUV,  "起止
        DATUB         LIKE STPOX-DATUB,  "有效至
        MTART         LIKE STPOX-MTART,  "类型
        AUSCH         LIKE STPOX-AUSCH,  "
        SOBSL         LIKE STPOX-SOBSL,
        EWAHR         LIKE STPOX-EWAHR,
        NFEAG         LIKE STPOX-NFEAG,
        NFGRP         LIKE STPOX-NFGRP,
        ANDAT         LIKE STPOX-ANDAT,
        ANNAM         LIKE STPOX-ANNAM,
        AEDAT         LIKE STPOX-AEDAT,
        AENAM         LIKE STPOX-AENAM,
        AENRA         LIKE STPOX-AENRA,
        POTX1         LIKE STPOX-POTX1,
        DISPO         LIKE MARC-DISPO,  "物料对应的MRP控制者
        FEVOR         LIKE MARC-FEVOR,  "生产调度员
        DSNAM         LIKE T024D-DSNAM,
        TXT           LIKE T024F-TXT,   "生产调度员名称
        BESKZ         LIKE MARC-BESKZ,  "采购类型
        BESKZ_C(15)   TYPE C,
        DZEIT         TYPE MARC-DZEIT,   "自制提前期
        PLIFZ         TYPE MARC-PLIFZ,   "采购提前期
        WEBAZ         TYPE MARC-WEBAZ,   "收货处理时间
*{   INSERT         QASK901284                                        3
        SCHGT         TYPE MARC-SCHGT,    "散装物料  "modify by yuhui 09.8.12
*}   INSERT
        EKGRP         TYPE STPOX-EKGRP,  "采购组
        EKNAM         TYPE T024-EKNAM,   "采购组名称
        MENGE_ZT      TYPE RESB-VMENG,   "在途数量
        MENGE_ZZ      TYPE RESB-VMENG,   "在制数量
        MENGE_JH      TYPE RESB-VMENG,   "计划分配数量
        LGPRO         LIKE MARC-LGPRO,
        LGFSB         LIKE MARC-LGFSB,
        MATKL         LIKE MARA-MATKL,
*{   INSERT         QASK903596                                        5
        EISBE         LIKE MARC-EISBE,    "安全库存   ADD BY 熊婧婧
*}   INSERT
        FLAG_C        TYPE C,   "优先级替代标识
      END OF T_ITEMS.
DATA:T_CJ LIKE STANDARD TABLE OF T_ITEMS WITH HEADER LINE, "插件
     T_ZZ LIKE STANDARD TABLE OF T_ITEMS WITH HEADER LINE. "组装


"非限制库存
DATA:BEGIN OF T_MARD OCCURS 0,
       LABST LIKE MARD-LABST,
       MATNR LIKE MARD-MATNR,
       WERKS LIKE MARD-WERKS,
       LOGRT LIKE MARD-LGORT,
     END OF T_MARD.

"已分配量
DATA:BEGIN OF T_RESB OCCURS 0,
       RSNUM LIKE RESB-RSNUM,
       RSPOS LIKE RESB-RSPOS,
       BDMNG LIKE RESB-BDMNG,  "需求数量
       ENMNG LIKE RESB-ENMNG,   "提货数
       MATNR LIKE RESB-MATNR,
       WERKS LIKE RESB-WERKS,
       VMENG LIKE RESB-VMENG,  "已分配量
       BDART LIKE RESB-BDART,
     END OF T_RESB.

DATA: BEGIN OF T_XTAB OCCURS 10,        "Tabelle der offenen Bestellungen
        WERKS LIKE EKPO-WERKS,
        LGORT LIKE EKPO-LGORT,
        MATNR LIKE EKPO-MATNR,
        MENGE LIKE EKPO-MENGE,
        MENGK LIKE EKPO-MENGE,
      END OF T_XTAB.
DATA:BEGIN OF T_AFPO OCCURS 0 ,
       AUFNR LIKE AFPO-AUFNR,
       MATNR LIKE AFPO-MATNR,
       MEINS LIKE AFPO-MEINS,
       PSMNG LIKE AFPO-PSMNG, "订单数量
       WEMNG LIKE AFPO-WEMNG, "交货数量
       PSAMG LIKE AFPO-PSAMG,  "报废数量
       IAMNG LIKE AFPO-IAMNG,  "交货过量或不足
     END OF T_AFPO.

*----------计算采购在途的数量用
DATA:BEGIN OF T_EKPO OCCURS 0 ,
       BSART LIKE EKKO-BSART,
       EBELN LIKE EKPO-EBELN,
       EBELP LIKE EKPO-EBELP,
       MATNR LIKE EKPO-MATNR,
       WERKS LIKE EKPO-WERKS,
       MENGE LIKE EKPO-MENGE, "订单数量
       EINDT LIKE EKET-EINDT, "交货时间
       WEMNG LIKE EKET-WEMNG, "已交货数量
       ELIKZ LIKE EKPO-ELIKZ, "交货已完成标记
     END OF T_EKPO.

*{   INSERT         QASK900838                                        1
"展开BOM的字阶
DATA: BEGIN OF T_BOM OCCURS 0.
        INCLUDE STRUCTURE  STPOX .
DATA: "BESKZ  TYPE MARC-BESKZ,
  FLAG_C TYPE C,   "优先级替代标识
  FEVOR  TYPE MARC-FEVOR.
DATA: END OF T_BOM.
DATA:T_TEMP_B LIKE STANDARD TABLE OF T_BOM WITH HEADER LINE.
*}   INSERT


*-----------公司间采购订单类型表
DATA:BEGIN OF T_161V OCCURS 0,
       BSART LIKE T161V-BSART,
       RESWK LIKE T161V-RESWK,
     END OF T_161V.

DATA:BEGIN OF T_MAKT OCCURS  0  ,
       MATNR TYPE MAKT-MATNR,
       MAKTX TYPE MAKT-MAKTX,
     END OF T_MAKT.

DATA L_AUFNR LIKE JEST-OBJNR.
DATA MAKTX_F TYPE MAKT-MAKTX.
DATA MEINS_F TYPE MARA-MEINS.
DATA STLAL  TYPE MAST-STLAL.
DATA STLAN  TYPE MAST-STLAN.
DATA BMENG  TYPE STKO-BMENG.
RANGES S_WERKS FOR MAST-WERKS.
DATA:L_FLAG(10) TYPE C.

*&---------------------------------------------------------------------*
*&      SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_MATNR  LIKE MAST-MATNR  MEMORY ID ZMAT , "OBLIGATORY,
                P_WERKS  LIKE MAST-WERKS MEMORY ID ZPWK, "OBLIGATORY,
                P_STLAL  LIKE MAST-STLAL MEMORY ID ZSTLAL, "可选的BOM
                P_CAPID  LIKE TC04-CAPID DEFAULT 'PP01', "OBLIGATORY.        "BOM应用程序
                P_MATNR1 LIKE MAST-MATNR MEMORY ID ZMAT1. " no-display.  "增加了一个隐藏的选项物料编号，用于供BOM反查调用。
PARAMETERS:     P_DATUV LIKE STKO-DATUV DEFAULT SY-DATUM,  "有效起始日
*               P_AENNR LIKE STKO-AENNR,   "更改编号
                P_EMENG LIKE STKO-BMENG. "需求数量
PARAMETERS:     R_ZCS13N TYPE C RADIOBUTTON GROUP RAD,
                R_ZCS12N TYPE C RADIOBUTTON GROUP RAD,
                R_F_BOM  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BLK.



SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
PARAMETERS:
                  P_WERKS1 LIKE MARD-WERKS MEMORY ID ZPWK1.
SELECT-OPTIONS:   S_MATNR FOR MARA-MATNR MEMORY ID ZPMAT,
                  S_MTART FOR MARA-MTART,
                  S_EKGRP FOR MARC-EKGRP,
                  S_DISPO FOR MARC-DISPO.

SELECTION-SCREEN END OF BLOCK BLK3.


INITIALIZATION.
*
AT SELECTION-SCREEN.
*得到物料的子项目
  IF S_MATNR[] IS INITIAL  AND S_MTART[] IS INITIAL AND S_EKGRP[] IS INITIAL
     AND S_DISPO[] IS INITIAL.

    L_FLAG = 'BOM'.
    IF P_MATNR = '' OR P_WERKS = '' OR P_CAPID = ''.
      MESSAGE E000(ZDEV) WITH 'BOM查询时物料编号，工厂，BOM应用程序不能为空!'.
    ENDIF.

  ELSE.
    IF P_MATNR <> ''  .
      MESSAGE E000(ZDEV) WITH 'BOM和物料不能同时查询!'.
    ENDIF.
    L_FLAG = 'MATERIAL'.
    IF P_WERKS1 = '' .
      MESSAGE E000(ZDEV) WITH '物料查询时工厂不能为空!'.
    ENDIF.
  ENDIF.


  IF L_FLAG = 'MATERIAL'.
    PERFORM GET_MATNR.
  ELSE.
    IF R_ZCS13N = 'X'.
      PERFORM GET_BOM_CS13N.

    ELSE.
      PERFORM GET_BOM_CS12N.

    ENDIF.
  ENDIF.
* 增加了一个隐藏的选项物料编号，用于供BOM反查调用。
  IF P_MATNR1 = ''.
    PERFORM GET_DATA.
  ENDIF.

END-OF-SELECTION.
*打印该BOM的数据
  PERFORM PRINT_ALV_DATA.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

**********************************************
*计算各种数量
*******************************************
  "计算子件库存量、 已分配量
  SELECT LABST
         MATNR
         WERKS
         LGORT
    INTO CORRESPONDING FIELDS OF TABLE T_MARD
    FROM MARD
    FOR ALL ENTRIES IN T_ITEMS
    WHERE MATNR = T_ITEMS-IDNRK
      AND WERKS IN S_WERKS
      AND DISKZ <> '1'.

*{   INSERT         QASK900474                                        2
  "MRP控制者（MARC-DISPO）及描述（V_T024D-DSNAM），生产调度员（MARC-FEVOR）字段及描述（V_T024F-FEVORTXXT）,采购类型 BESKZ
  DATA:BEGIN OF T_MARC OCCURS 0 ,
         MATNR LIKE MARC-MATNR,
         WERKS LIKE MARC-WERKS,
         DISPO LIKE MARC-DISPO,
         FEVOR LIKE MARC-FEVOR,
         BESKZ LIKE MARC-BESKZ,
         DZEIT LIKE MARC-DZEIT,
         PLIFZ LIKE MARC-PLIFZ,
         WEBAZ LIKE MARC-WEBAZ,
         EKGRP LIKE MARC-EKGRP,
         LGPRO LIKE MARC-LGPRO,
         LGFSB LIKE MARC-LGFSB,
         SOBSL LIKE MARC-SOBSL,
         SCHGT LIKE MARC-SCHGT,   "modify by yuhui 09.8.12
         EISBE LIKE MARC-EISBE,   "安全库存 add by 熊婧婧
       END OF T_MARC.

*------------取MARC
  SELECT
       MATNR
       WERKS
       DISPO
       FEVOR
       BESKZ
       DZEIT
       PLIFZ
       WEBAZ
       EKGRP
       LGPRO
       LGFSB
       SOBSL
       SCHGT          "modify by yuhui 09.8.12
       EISBE          "安全库存   add by 熊婧婧
       INTO CORRESPONDING FIELDS OF TABLE T_MARC
       FROM MARC
       FOR ALL ENTRIES IN T_ITEMS
       WHERE MATNR = T_ITEMS-IDNRK
       AND WERKS = T_ITEMS-WERKS.

  LOOP AT T_ITEMS.
    READ TABLE T_MARC WITH KEY MATNR = T_ITEMS-IDNRK WERKS = T_ITEMS-WERKS.
    IF SY-SUBRC = 0.
      T_ITEMS-DISPO = T_MARC-DISPO.
      IF T_ITEMS-FEVOR = ''.
        T_ITEMS-FEVOR = T_MARC-FEVOR.
      ENDIF.
      T_ITEMS-BESKZ = T_MARC-BESKZ.
      T_ITEMS-DZEIT = T_MARC-DZEIT.
      T_ITEMS-PLIFZ = T_MARC-PLIFZ.
      T_ITEMS-WEBAZ = T_MARC-WEBAZ.
      T_ITEMS-EKGRP = T_MARC-EKGRP.
      T_ITEMS-LGPRO = T_MARC-LGPRO.
      T_ITEMS-LGFSB = T_MARC-LGFSB.
      T_ITEMS-SOBSL = T_MARC-SOBSL.
      T_ITEMS-SCHGT = T_MARC-SCHGT.   "modify by yuhui 09.8.12
      T_ITEMS-EISBE = T_MARC-EISBE.   "安全库存   add by 熊婧婧
    ENDIF.

    MODIFY T_ITEMS.
  ENDLOOP.


*}   INSERT
  SELECT RSNUM
         RSPOS
         BDMNG    "需求数量
         ENMNG    "提货数
         MATNR
         WERKS
         BDART
    INTO CORRESPONDING FIELDS OF TABLE T_RESB
    FROM RESB
    FOR ALL ENTRIES IN T_ITEMS
    WHERE MATNR = T_ITEMS-IDNRK
      AND WERKS IN S_WERKS
      AND KZEAR <> 'X'  "最后发货
      AND XLOEK <> 'X'   "删除标识
     AND DUMPS <> 'X'   "虚拟项
*         AND XFEHL <> 'X'
      AND ( BDART = 'AR' OR  BDART = 'SB' ) "生产订单预留
       .
*     取得在制数量
  SELECT AUFNR
         MATNR
         MEINS
         PSMNG "订单数量
         WEMNG "交货数量
         PSAMG "报废数量
         IAMNG
   INTO CORRESPONDING FIELDS OF TABLE T_AFPO
    FROM AFPO
    FOR ALL ENTRIES IN T_ITEMS
    WHERE ELIKZ <> 'X'  "交货完成
    AND WEPOS = 'X' "是否收货
    AND MATNR = T_ITEMS-IDNRK
    AND DWERK IN S_WERKS
    .
  DATA:   L_LINE    LIKE BSVX-STTXT,
          L_STAT(4) TYPE C.
  LOOP AT T_AFPO.
    CONCATENATE 'OR' T_AFPO-AUFNR INTO L_AUFNR.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        CLIENT      = SY-MANDT
        OBJNR       = L_AUFNR
        ONLY_ACTIVE = 'X'
        SPRAS       = SY-LANGU
      IMPORTING
        LINE        = L_LINE.


    L_STAT = L_LINE+0(4).

*判断是否为已经下达状态
    IF L_STAT = 'REL' OR L_STAT = 'CRTD'.
      SEARCH L_LINE FOR 'DLID'. "删除标识
      IF SY-SUBRC = 0.
        DELETE T_AFPO.
      ENDIF.
    ELSE.
      DELETE T_AFPO.
    ENDIF.
  ENDLOOP.

  PERFORM GET_PO_QUANTITY .  "取的采购订单的在途未交货数量
  PERFORM GET_T161V."     *取的公司间的采购订单类型
  LOOP AT T_ITEMS.
*   库存数量
    LOOP AT T_MARD WHERE MATNR = T_ITEMS-IDNRK.
      T_ITEMS-LABST = T_ITEMS-LABST + T_MARD-LABST.
    ENDLOOP.
    MODIFY T_ITEMS.

*
    LOOP AT T_RESB WHERE MATNR = T_ITEMS-IDNRK .
      IF  T_RESB-BDART = 'AR'.     "已分配量
        T_ITEMS-VMENG = T_ITEMS-VMENG + ( T_RESB-BDMNG - T_RESB-ENMNG ).
      ELSEIF T_RESB-BDART = 'SB' . "   计划分配量
        T_ITEMS-MENGE_JH = T_ITEMS-MENGE_JH + ( T_RESB-BDMNG - T_RESB-ENMNG ).
      ENDIF.
    ENDLOOP.


*  在制数量   = 订单数量-交货数量-报废数量
    LOOP AT T_AFPO WHERE MATNR = T_ITEMS-IDNRK.
      T_ITEMS-MENGE_ZZ =  T_ITEMS-MENGE_ZZ + ( T_AFPO-PSMNG -  T_AFPO-WEMNG - T_AFPO-IAMNG ).
    ENDLOOP.
*  计算 在途数量

    CLEAR T_ITEMS-MENGE_ZT .
    LOOP AT T_EKPO WHERE MATNR = T_ITEMS-IDNRK.
*-----------如果采购订单是公司间采购 将数量添加到确认分配  else 数量添加到在途数量
      READ TABLE T_161V WITH KEY BSART = T_EKPO-BSART.
      IF SY-SUBRC = 0.
        T_ITEMS-VMENG = T_ITEMS-VMENG + ( T_EKPO-MENGE - T_EKPO-WEMNG ).
      ELSEIF SY-SUBRC <> 0.
        T_ITEMS-MENGE_ZT = T_ITEMS-MENGE_ZT + ( T_EKPO-MENGE - T_EKPO-WEMNG ).
      ENDIF.
    ENDLOOP.

*  剩余可用量  =
    T_ITEMS-MENGE_S = T_ITEMS-LABST - T_ITEMS-VMENG.
*    t_items-werks = p_werks.
    MODIFY T_ITEMS.
  ENDLOOP.

  REFRESH T_MARD.
  REFRESH T_RESB.
  REFRESH T_AFPO.

*{   DELETE         QASK900474                                        1
*\  "MRP控制者（MARC-DISPO）及描述（V_T024D-DSNAM），生产调度员（MARC-FEVOR）字段及描述（V_T024F-FEVORTXXT）,采购类型 BESKZ
*\  data:begin of t_marc occurs 0 ,
*\        matnr like marc-matnr,
*\        werks like marc-werks,
*\        dispo like marc-dispo,
*\        fevor like marc-fevor,
*\        beskz like marc-beskz,
*\        dzeit like marc-dzeit,
*\        plifz like marc-plifz,
*\        webaz like marc-webaz,
*\        ekgrp like marc-ekgrp,
*\        lgpro like marc-lgpro,
*\        lgfsb like marc-lgfsb,
*\    end of t_marc.
*}   DELETE

  DATA:BEGIN OF T_MARA OCCURS 0 ,
         MATNR TYPE MARA-MATNR,
         MATKL LIKE MARA-MATKL,
       END OF T_MARA.

  DATA:T_024D LIKE T024D OCCURS 0 WITH HEADER LINE,
       T_024F LIKE T024F OCCURS 0 WITH HEADER LINE,
       T_024  LIKE T024 OCCURS 0 WITH HEADER LINE.
*{   DELETE         QASK900474                                        3
*\*------------取MARC
*\  select
*\       matnr
*\       werks
*\       dispo
*\       fevor
*\       beskz
*\       dzeit
*\       plifz
*\       webaz
*\       ekgrp
*\       lgpro
*\       lgfsb
*\       into corresponding fields of table t_marc
*\       from marc
*\         for all entries in t_items
*\       where matnr = t_items-idnrk
*\       and werks = t_items-werks.
*}   DELETE

  SELECT
        MATNR
        MATKL
        INTO CORRESPONDING FIELDS OF TABLE T_MARA
        FROM MARA
        FOR ALL ENTRIES IN T_ITEMS
        WHERE MATNR = T_ITEMS-IDNRK.


  LOOP AT T_ITEMS.

*{   DELETE         QASK900474                                        4
*\    read table t_marc with key matnr = t_items-idnrk werks = t_items-werks.
*\    if sy-subrc = 0.
*\      t_items-dispo = t_marc-dispo.
*\      t_items-fevor = t_marc-fevor.
*\      t_items-beskz = t_marc-beskz.
*\      t_items-dzeit = t_marc-dzeit.
*\      t_items-plifz = t_marc-plifz.
*\      t_items-webaz = t_marc-webaz.
*\      t_items-ekgrp = t_marc-ekgrp.
*\      t_items-lgpro = t_marc-lgpro.
*\      t_items-lgfsb = t_marc-lgfsb.
*\    endif.
*}   DELETE
    READ TABLE T_MARA WITH KEY MATNR = T_ITEMS-IDNRK.
    IF  SY-SUBRC = 0.
      T_ITEMS-MATKL = T_MARA-MATKL.
    ENDIF.

    MODIFY T_ITEMS.
  ENDLOOP.

  SELECT *
  INTO TABLE T_024D
  FROM T024D
  FOR ALL ENTRIES IN T_ITEMS
  WHERE DISPO = T_ITEMS-DISPO
    AND WERKS = T_ITEMS-WERKS.

  SELECT *
  INTO  TABLE T_024F
  FROM T024F
  FOR ALL ENTRIES IN T_ITEMS
  WHERE FEVOR = T_ITEMS-FEVOR
    AND WERKS = T_ITEMS-WERKS.

  SELECT *
  INTO  TABLE T_024
  FROM T024
  FOR ALL ENTRIES IN T_ITEMS
  WHERE EKGRP = T_ITEMS-EKGRP.

  LOOP AT T_ITEMS.
    READ TABLE T_024 WITH KEY EKGRP = T_ITEMS-EKGRP.
    IF SY-SUBRC = 0.
      T_ITEMS-EKNAM = T_024-EKNAM.
    ENDIF.

    READ TABLE T_024F WITH KEY FEVOR = T_ITEMS-FEVOR WERKS = T_ITEMS-WERKS..
    IF SY-SUBRC = 0.
      T_ITEMS-TXT = T_024F-TXT.
    ENDIF.

    READ TABLE T_024D WITH KEY DISPO = T_ITEMS-DISPO WERKS = T_ITEMS-WERKS..
    IF SY-SUBRC = 0.
      T_ITEMS-DSNAM = T_024D-DSNAM.
    ENDIF.

    "当物料的采购类型为E时取自制提前期，当为F类型时取采购提前期，X类型时两项时间都取。
    IF T_ITEMS-BESKZ = 'E'.
      T_ITEMS-BESKZ_C =  '自制生产'.
      T_ITEMS-PLIFZ =  ' '.
    ELSEIF  T_ITEMS-BESKZ = 'F'.
      T_ITEMS-BESKZ_C = '外部采购'.
      T_ITEMS-DZEIT =  ' '.
    ELSEIF  T_ITEMS-BESKZ = 'X'.
      T_ITEMS-BESKZ_C =  '自制生产&外部采购'.
    ENDIF.

    MODIFY T_ITEMS.
  ENDLOOP.

ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      SET PF-STATUS
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  IF R_F_BOM = 'X'.
*    SET PF-STATUS 'SET_PF_ALV'.        "采购BOM
*  ELSE.
*    SET PF-STATUS 'SET_PF'.
*  ENDIF.

  SET PF-STATUS 'SET_PF'.
ENDFORM. "Set_pf_status
*&---------------------------------------------------------------------*
*&      Form  PRINT_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_ALV_DATA .

  DATA:  WLT_FIELDCAT LIKE LINE OF WT_FIELDCAT.

  IF P_MATNR1 <> ''.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'IDNRK'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料编号'.
    WLT_FIELDCAT-OUTPUTLEN    = 18.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.


    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'OJTXP'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料描述'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MNGKO'.
*    wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '数量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.



  ELSE.
    IF R_ZCS12N = 'X' AND S_MATNR IS INITIAL.
      CLEAR WLT_FIELDCAT.
      WLT_FIELDCAT-FIELDNAME    = 'STUFE_OUT'.
      WLT_FIELDCAT-SELTEXT_L    = '展开层'.
      WLT_FIELDCAT-KEY          = 'X'.     "水平移动时固定该列
      APPEND WLT_FIELDCAT TO WT_FIELDCAT.
    ENDIF.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'WERKS'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '工厂'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.


    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'IDNRK'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料编号'.
    WLT_FIELDCAT-OUTPUTLEN    = 18.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.


    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'OJTXP'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料描述'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    IF L_FLAG = 'BOM'.
      CLEAR WLT_FIELDCAT.
      WLT_FIELDCAT-FIELDNAME    = 'MNGKO'.
*    wlt_fieldcat-tabname      = 'T_ITEMS'.
      WLT_FIELDCAT-SELTEXT_L    = '模拟数量'.
      APPEND WLT_FIELDCAT TO WT_FIELDCAT.

*    CLEAR wlt_fieldcat.
*    wlt_fieldcat-fieldname    = 'MEINS'.
*    wlt_fieldcat-tabname      = 'T_ITEMS'.
*    wlt_fieldcat-seltext_l    = '组件单位'.
**   WLT_FIELDCAT-SELTEXT_M    = 'Un'.
*    APPEND wlt_fieldcat TO wt_fieldcat.

    ELSE.
      CLEAR WLT_FIELDCAT.
      WLT_FIELDCAT-FIELDNAME    = 'MEINS'.
*    wlt_fieldcat-tabname      = 'T_ITEMS'.
      WLT_FIELDCAT-SELTEXT_L    = '单位'.
*   WLT_FIELDCAT-SELTEXT_M    = 'Un'.
      APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    ENDIF.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'LABST'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '库存量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.



    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'VMENG'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '确认分配量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MENGE_S'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '剩余可用量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MENGE_ZT'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '在途量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MENGE_ZZ'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '在制量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MENGE_JH'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '计划分配量'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'LGPRO'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '生产仓储地点'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'LGFSB'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购仓储地点'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MTART'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料类型'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'MATKL'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '物料组'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'DISPO'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = 'MRP控制者编号'.
*  WLT_FIELDCAT-SELTEXT_M    = 'MRPC'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'DSNAM'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = 'MRP控制者'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.


    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'FEVOR'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '生产调度员编号'.
*  WLT_FIELDCAT-SELTEXT_M    = 'PrCtr'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'TXT'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '生产调度员'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'BESKZ'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购类型'.
*  WLT_FIELDCAT-SELTEXT_M    = '采购'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'BESKZ_C'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购类型'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'EKGRP'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购组'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'EKNAM'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购组描述'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'DZEIT'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '自制提前期'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'PLIFZ'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '采购提前期'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'WEBAZ'.
*  wlt_fieldcat-tabname      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '收货处理时间'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

*  IF l_flag = 'BOM'.
    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'RGEKZ'.
*  WLT_FIELDCAT-TABNAME      = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '反冲'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'DUMPS'.
*  WLT_FIELDCAT-TABNAME       = 'T_ITEMS'.
    WLT_FIELDCAT-SELTEXT_L    = '虚拟件'.
*    WLT_FIELDCAT-CHECKBOX     = 'X'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.
*  endif.
*{   INSERT         QASK901284                                        1
    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'SCHGT'.   ""modify by yuhui 09.8.12
    WLT_FIELDCAT-SELTEXT_L    = '散装物料'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.
*}   INSERT
*{   INSERT         QASK903596                                        2
    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'EISBE'.   ""modify by xiongjingjing
    WLT_FIELDCAT-SELTEXT_L    = '安全库存'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.
*}   INSERT
    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'NFMAT'.
    WLT_FIELDCAT-SELTEXT_L    = '后继物料'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'FLAG_C'.
    WLT_FIELDCAT-SELTEXT_L    = '替代物料标识'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'ALPGR'.
    WLT_FIELDCAT-SELTEXT_L    = '替代组'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.

    CLEAR WLT_FIELDCAT.
    WLT_FIELDCAT-FIELDNAME    = 'ALPRF'.
    WLT_FIELDCAT-SELTEXT_L    = '优先级'.
    APPEND WLT_FIELDCAT TO WT_FIELDCAT.
  ENDIF.

  CLEAR P_MATNR1.
  SET PARAMETER ID 'ZMAT1' FIELD ''.
  SET PARAMETER ID 'ZPMAT' FIELD ''.
  SET PARAMETER ID 'ZMAT' FIELD ''.
  SET PARAMETER ID 'ZSTLAL' FIELD ''.
  WT_LAYOUT-ZEBRA = 'X'.
*  wt_layout-f2code = '&ETA'.
  WT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  IF L_FLAG = 'BOM'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM          = SY-REPID
        IS_LAYOUT                   = WT_LAYOUT
        I_CALLBACK_HTML_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
        I_CALLBACK_PF_STATUS_SET    = 'SET_PF_STATUS'
*       I_DEFAULT                   = 'X'
        I_SAVE                      = 'A'
        I_CALLBACK_USER_COMMAND     = 'EXECUTE_COMMAND'
        IT_FIELDCAT                 = WT_FIELDCAT[]
        IT_EVENTS                   = WT_EVENTS
      TABLES
        T_OUTTAB                    = T_ITEMS.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
        IS_LAYOUT               = WT_LAYOUT
*       i_callback_html_top_of_page = 'ALV_TOP_OF_PAGE'
*       I_CALLBACK_PF_STATUS_SET    = 'SET_PF_STATUS'
*       I_DEFAULT               = 'X'
        I_SAVE                  = 'A'
        I_CALLBACK_USER_COMMAND = 'EXECUTE_COMMAND'
        IT_FIELDCAT             = WT_FIELDCAT[]
        IT_EVENTS               = WT_EVENTS
      TABLES
        T_OUTTAB                = T_ITEMS.
  ENDIF.

ENDFORM.                    " PRINT_ALV_DATA

*&---------------------------------------------------------------------*
*&      Form  alv_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_OUT      text
*----------------------------------------------------------------------*
FORM ALV_OUT TABLES T_OUT.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM          = SY-REPID
      IS_LAYOUT                   = WT_LAYOUT
      I_CALLBACK_HTML_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
      I_CALLBACK_PF_STATUS_SET    = 'SET_PF_STATUS'
*     I_DEFAULT                   = 'X'
      I_SAVE                      = 'A'
      I_CALLBACK_USER_COMMAND     = 'EXECUTE_COMMAND'
      IT_FIELDCAT                 = WT_FIELDCAT[]
      IT_EVENTS                   = WT_EVENTS
    TABLES
      T_OUTTAB                    = T_OUT.
ENDFORM.                    "alv_out
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM EXECUTE_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD. "响应ALV点击自定义按钮之后的事件
  CASE R_UCOMM.

    WHEN '&IC1'.
      READ TABLE T_ITEMS INDEX RS_SELFIELD-TABINDEX .
      SET PARAMETER ID 'MAT' FIELD T_ITEMS-IDNRK.
      SET PARAMETER ID 'BERID' FIELD ''.
      SET PARAMETER ID 'WRK' FIELD T_ITEMS-WERKS.
      CALL TRANSACTION 'MD04' AND SKIP FIRST  SCREEN .

    WHEN '&CJ'.
      IF T_CJ[] IS INITIAL.
        LOOP AT T_ITEMS WHERE FEVOR = '200' . "插件物料
          MOVE-CORRESPONDING T_ITEMS TO T_CJ.
          APPEND T_CJ.
        ENDLOOP.
      ENDIF.

      PERFORM  ALV_OUT TABLES T_CJ.
      PERFORM REFRESH_ALV.

    WHEN '&ZZ'.
      IF T_ZZ[] IS INITIAL.
        LOOP AT T_ITEMS WHERE FEVOR <> '200' . "组装物料
          MOVE-CORRESPONDING T_ITEMS TO T_ZZ.
          APPEND T_ZZ.
        ENDLOOP.
      ENDIF.

      PERFORM  ALV_OUT TABLES T_ZZ.
      PERFORM REFRESH_ALV.

  ENDCASE.
ENDFORM.                    "EXECUTE_COMMAND

*---------------------------------------------------------------------*
*       FORM ALV_TOP_OF_PAGE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_DYDO                                                        *
*  -->  TO                                                            *
*  -->  CL_DD_DOCUMENT                                                *
*---------------------------------------------------------------------*
FORM ALV_TOP_OF_PAGE USING CL_DD TYPE REF TO CL_DD_DOCUMENT.
*  SET PF-STATUS 'STANDARD1'.

  DATA: M_P TYPE I.
  DATA: M_BUFF TYPE STRING.


*表头其实完全可以是一个html文件，自己使用html语言进行格式控制
  M_BUFF = '<html>'.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.

  CONCATENATE '<span style="FONT-weight:bold">物料</span>' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' P_MATNR '<BR>' INTO M_BUFF.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.

  "用途、替代
  SELECT MAST~STLAL
         MAST~STLAN
         BMENG
    INTO (STLAL,STLAN,BMENG)
    FROM MAST
    INNER JOIN STKO ON STKO~STLNR = MAST~STLNR
    WHERE MATNR = P_MATNR
    AND WERKS = P_WERKS.
  ENDSELECT.


  CONCATENATE '<span style="FONT-weight:bold">工厂/用途/替代</span>' ' ' ' ' ' '  P_WERKS '/' STLAN '/' STLAL  '<BR>' INTO M_BUFF.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.
  CLEAR STLAL.
  CLEAR STLAN.

  "读取父物料名称
  SELECT SINGLE
         MAKTX
    INTO MAKTX_F
    FROM MAKT
    WHERE MATNR = P_MATNR
    AND  SPRAS = '1'.

  DATA SPA(5) TYPE C.
  SPA = '    '.
  CONCATENATE '<span style="FONT-weight:bold">描述</span>' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' SPA  MAKTX_F '<BR>' INTO M_BUFF.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.

  "读取单位
  SELECT SINGLE MEINS
    INTO MEINS_F
    FROM MARA
    WHERE MATNR = P_MATNR.

  DATA MENG(10) TYPE C.
  MENG = BMENG.
  CONCATENATE '<span style="FONT-weight:bold">基本数量</span><--'  '(' MEINS_F ')' ' ' ' '  MENG '<BR>' INTO M_BUFF.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.

  IF P_EMENG IS INITIAL.
    CONCATENATE '<span style="FONT-weight:bold">需求数量</span><--' '(' MEINS_F ')' ' '   ' ' ' ' '1'  '<BR>' INTO M_BUFF.
    CALL METHOD CL_DD->HTML_INSERT
      EXPORTING
        CONTENTS = M_BUFF
      CHANGING
        POSITION = M_P.
  ELSE.
    DATA EMENG TYPE N.
    EMENG = P_EMENG.
    CONCATENATE '<span style="FONT-weight:bold">需求数量</span><--' '(' MEINS_F ')' ' ' ' '  ' ' EMENG  '<BR>' INTO M_BUFF.
    CALL METHOD CL_DD->HTML_INSERT
      EXPORTING
        CONTENTS = M_BUFF
      CHANGING
        POSITION = M_P.
  ENDIF.
  CLEAR EMENG.
  CLEAR MEINS_F.
  CLEAR MENG.
  CLEAR BMENG.

  M_BUFF = '</html>'.
  CALL METHOD CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFF
    CHANGING
      POSITION = M_P.

ENDFORM.                    "ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  frm_checkdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM_CS13N .
  S_WERKS-LOW = P_WERKS.
  S_WERKS-OPTION = 'EQ'.
  S_WERKS-SIGN = 'I'.
  APPEND S_WERKS.

*数据定义
  DATA: BEGIN OF WTL_MATNR OCCURS 0,
          WERKS LIKE MARC-WERKS,
          MATNR LIKE MARA-MATNR.
  DATA: END OF WTL_MATNR.

  "计算展开层
  DATA: L_STR   TYPE STRING,
        L_CNT   TYPE I,
        L_CNTDO TYPE I.
  "展开BOM的字阶
*{   DELETE         QASK900838                                        2
*\  data: t_bom like standard table of stpox with header line.
*}   DELETE


  "根据选项产生不同BOM的结构
  "展多层BOM
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = P_CAPID
      DATUV                 = P_DATUV
      EMENG                 = P_EMENG
      MTNRV                 = P_MATNR
      STLAL                 = P_STLAL
      MEHRS                 = 'X'
      WERKS                 = P_WERKS
    TABLES
      STB                   = T_BOM
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      CONVERSION_ERROR      = 8
      OTHERS                = 9.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*{   INSERT         QASK900838

    PERFORM MOVE_F.  "去除外购件

    IF R_F_BOM = 'X'.    "读取采购外之间的
      PERFORM SET_ALPGR.
    ENDIF.

    IF P_MATNR1 = ''.
      LOOP AT T_BOM.
        T_ITEMS-WERKS = P_WERKS.
*     T_ITEMS-MATNR = P_MATNR.   "父项料号
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-IDNRK =  T_BOM-IDNRK . "组件料号
        T_ITEMS-FEVOR =  T_BOM-FEVOR .
        T_ITEMS-MNGKO  = T_BOM-MNGKO . "数量
        T_ITEMS-MEINS  = T_BOM-MEINS . "单位
        T_ITEMS-MTART  = T_BOM-MTART.
        T_ITEMS-RGEKZ  = T_BOM-RGEKZ . "倒冲
        T_ITEMS-DUMPS  = T_BOM-DUMPS .
        T_ITEMS-FLAG_C = T_BOM-FLAG_C .
        T_ITEMS-NFMAT  = T_BOM-NFMAT .
        COLLECT T_ITEMS.
      ENDLOOP.
    ELSE.
      LOOP AT T_BOM WHERE IDNRK = P_MATNR1.
*     T_ITEMS-MATNR = P_MATNR.   "父项料号
        T_ITEMS-WERKS = P_WERKS.
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-IDNRK =  T_BOM-IDNRK . "组件料号
        T_ITEMS-FEVOR =  T_BOM-FEVOR .
        T_ITEMS-MNGKO  = T_BOM-MNGKO . "数量
        T_ITEMS-MEINS  = T_BOM-MEINS . "单位
        T_ITEMS-MTART  = T_BOM-MTART.
        T_ITEMS-RGEKZ  = T_BOM-RGEKZ . "倒冲
        T_ITEMS-DUMPS  = T_BOM-DUMPS .
        T_ITEMS-FLAG_C = T_BOM-FLAG_C .
        T_ITEMS-NFMAT  = T_BOM-NFMAT .
        COLLECT T_ITEMS.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF T_ITEMS[] IS INITIAL.
    MESSAGE '没有找到相关数据，请重新选择' TYPE 'E'.
  ENDIF.

  IF R_F_BOM = 'X'.
    SELECT MAKTX MATNR
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE T_MAKT
      FOR ALL ENTRIES IN T_ITEMS
      WHERE MATNR = T_ITEMS-NFMAT
       AND SPRAS = 1.

    LOOP AT T_ITEMS WHERE NFMAT <> ''.
      READ TABLE T_MAKT WITH KEY MATNR = T_ITEMS-NFMAT.
      IF SY-SUBRC = 0.
        T_ITEMS-OJTXP = T_MAKT-MAKTX.
        T_ITEMS-IDNRK = T_ITEMS-NFMAT.
        MODIFY T_ITEMS.
      ENDIF.
    ENDLOOP.
  ENDIF.
  REFRESH T_MAKT.

ENDFORM.                    " frm_checkdata
*&---------------------------------------------------------------------*
*&      Form  get_bom_cs12n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM_CS12N .
  S_WERKS-LOW = P_WERKS.
  S_WERKS-OPTION = 'EQ'.
  S_WERKS-SIGN = 'I'.
  APPEND S_WERKS.
*数据定义
  DATA: BEGIN OF WTL_MATNR OCCURS 0,
          WERKS LIKE MARC-WERKS,
          MATNR LIKE MARA-MATNR.
  DATA: END OF WTL_MATNR.

  "计算展开层
  DATA: L_STR   TYPE STRING,
        L_CNT   TYPE I,
        L_CNTDO TYPE I.
*{   DELETE         QASK900838                                        1
*\  "展开BOM的字阶
*\  data: t_bom like standard table of stpox with header line.
*}   DELETE

  "根据选项产生不同BOM的结构
  "展多层BOM
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = P_CAPID
      DATUV                 = P_DATUV
      EMENG                 = P_EMENG
      MTNRV                 = P_MATNR
      STLAL                 = P_STLAL
      MEHRS                 = 'X'
      WERKS                 = P_WERKS
    TABLES
      STB                   = T_BOM
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      CONVERSION_ERROR      = 8
      OTHERS                = 9.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*{   INSERT         QASK900838                                        2

    PERFORM MOVE_F.  "去除外购件

    IF R_F_BOM = 'X'.    "读取采购外之间的
      PERFORM SET_ALPGR.
    ENDIF.
*}   INSERT
    IF P_MATNR1 = ''.
      LOOP AT T_BOM.
        T_ITEMS-MATNR = P_MATNR.
        T_ITEMS-WERKS = P_WERKS.
*     T_ITEMS-DGLVL =  T_BOM-DGLVL .
        T_ITEMS-STUFE = T_BOM-STUFE.
        T_ITEMS-POSNR =  T_BOM-POSNR .
        T_ITEMS-POSTP =  T_BOM-POSTP .
*     T_ITEMS-OBJIC =  T_BOM-OBJIC .
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-IDNRK =  T_BOM-IDNRK .
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-MNGKO  = T_BOM-MNGKO .
        T_ITEMS-MEINS  = T_BOM-MEINS .
        T_ITEMS-DUMPS  = T_BOM-DUMPS .
        T_ITEMS-LGORT  = T_BOM-LGORT.
        T_ITEMS-KZAUS  = T_BOM-KZAUS .
        T_ITEMS-NFMAT  = T_BOM-NFMAT .
        T_ITEMS-KZNFP  = T_BOM-KZNFP .
        T_ITEMS-RGEKZ  = T_BOM-RGEKZ .
        T_ITEMS-ALPGR  = T_BOM-ALPGR .
        T_ITEMS-ALPRF  = T_BOM-ALPRF .
        T_ITEMS-WERKS  = T_BOM-WERKS.
        T_ITEMS-DATUV  = T_BOM-DATUV.  "起止
        T_ITEMS-DATUB  = T_BOM-DATUB.  "有效至
        T_ITEMS-MTART  = T_BOM-MTART.
        T_ITEMS-AUSCH  = T_BOM-AUSCH.
        T_ITEMS-SOBSL  = T_BOM-SOBSL.
        T_ITEMS-ALPGR  = T_BOM-ALPGR.
        T_ITEMS-EWAHR  = T_BOM-EWAHR.
        T_ITEMS-NFEAG = T_BOM-NFEAG.
        T_ITEMS-NFGRP = T_BOM-NFGRP.
        T_ITEMS-ANDAT = T_BOM-ANDAT.
        T_ITEMS-ANNAM = T_BOM-ANNAM.
        T_ITEMS-AENAM = T_BOM-AENAM.
        T_ITEMS-AENRA = T_BOM-AENRA.
        T_ITEMS-POTX1 = T_BOM-POTX1.
        T_ITEMS-AEDAT = T_BOM-AEDAT.
        T_ITEMS-INDEX = T_BOM-INDEX.
        T_ITEMS-FLAG_C = T_BOM-FLAG_C .
        T_ITEMS-FEVOR =  T_BOM-FEVOR .

        CLEAR:L_STR,L_CNT,L_CNTDO.
        L_STR = T_BOM-STUFE.
        L_CNT = T_BOM-STUFE.

        DO L_CNT TIMES.                       "层次
          L_CNTDO = L_CNTDO + 1.
          IF L_CNTDO >= 1.
            CONCATENATE '.' L_STR INTO L_STR.
          ENDIF.
          IF L_CNTDO >= 10.
            EXIT.
          ENDIF.
        ENDDO.
        T_ITEMS-STUFE_OUT = L_STR.

        APPEND T_ITEMS.
      ENDLOOP.

    ELSE.
      LOOP AT T_BOM WHERE IDNRK = P_MATNR1.
        T_ITEMS-MATNR = P_MATNR.
        T_ITEMS-WERKS = P_WERKS.
*     T_ITEMS-DGLVL =  T_BOM-DGLVL .
        T_ITEMS-STUFE = T_BOM-STUFE.
        T_ITEMS-POSNR =  T_BOM-POSNR .
        T_ITEMS-POSTP =  T_BOM-POSTP .
*     T_ITEMS-OBJIC =  T_BOM-OBJIC .
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-IDNRK =  T_BOM-IDNRK .
        T_ITEMS-OJTXP =  T_BOM-OJTXP .
        T_ITEMS-MNGKO  = T_BOM-MNGKO .
        T_ITEMS-MEINS  = T_BOM-MEINS .
        T_ITEMS-DUMPS  = T_BOM-DUMPS .
        T_ITEMS-LGORT  = T_BOM-LGORT.
        T_ITEMS-KZAUS  = T_BOM-KZAUS .
        T_ITEMS-NFMAT  = T_BOM-NFMAT .
        T_ITEMS-KZNFP  = T_BOM-KZNFP .
        T_ITEMS-RGEKZ  = T_BOM-RGEKZ .
        T_ITEMS-ALPGR  = T_BOM-ALPGR .
        T_ITEMS-ALPRF  = T_BOM-ALPRF .
        T_ITEMS-WERKS  = T_BOM-WERKS.
        T_ITEMS-DATUV  = T_BOM-DATUV.  "起止
        T_ITEMS-DATUB  = T_BOM-DATUB.  "有效至
        T_ITEMS-MTART  = T_BOM-MTART.
        T_ITEMS-AUSCH  = T_BOM-AUSCH.
        T_ITEMS-SOBSL  = T_BOM-SOBSL.
        T_ITEMS-ALPGR  = T_BOM-ALPGR.
        T_ITEMS-EWAHR  = T_BOM-EWAHR.
        T_ITEMS-NFEAG = T_BOM-NFEAG.
        T_ITEMS-NFGRP = T_BOM-NFGRP.
        T_ITEMS-ANDAT = T_BOM-ANDAT.
        T_ITEMS-ANNAM = T_BOM-ANNAM.
        T_ITEMS-AENAM = T_BOM-AENAM.
        T_ITEMS-AENRA = T_BOM-AENRA.
        T_ITEMS-POTX1 = T_BOM-POTX1.
        T_ITEMS-AEDAT = T_BOM-AEDAT.
        T_ITEMS-INDEX = T_BOM-INDEX.

        CLEAR:L_STR,L_CNT,L_CNTDO.
        L_STR = T_BOM-STUFE.
        L_CNT = T_BOM-STUFE.

        DO L_CNT TIMES.                       "层次
          L_CNTDO = L_CNTDO + 1.
          IF L_CNTDO >= 1.
            CONCATENATE '.' L_STR INTO L_STR.
          ENDIF.
          IF L_CNTDO >= 10.
            EXIT.
          ENDIF.
        ENDDO.
        T_ITEMS-STUFE_OUT = L_STR.

        APPEND T_ITEMS.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF T_ITEMS[] IS INITIAL.
    MESSAGE '没有找到相关数据，请重新选择' TYPE 'E'.
  ENDIF.

  IF R_F_BOM = 'X'.
    SELECT MAKTX MATNR
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE T_MAKT
      FOR ALL ENTRIES IN T_ITEMS
      WHERE MATNR = T_ITEMS-NFMAT
       AND SPRAS = 1.

    LOOP AT T_ITEMS WHERE NFMAT <> ''.
      READ TABLE T_MAKT WITH KEY MATNR = T_ITEMS-NFMAT.
      IF SY-SUBRC = 0.
        T_ITEMS-OJTXP = T_MAKT-MAKTX.
        T_ITEMS-IDNRK = T_ITEMS-NFMAT.
        MODIFY T_ITEMS.
      ENDIF.
    ENDLOOP.
    REFRESH T_MAKT.
  ENDIF.
ENDFORM.                    " get_bom_cs12n
*{   INSERT         QASK900838                                        1
*&---------------------------------------------------------------------*
*&      Form  move_f
*&---------------------------------------------------------------------*
FORM MOVE_F.
  DATA:BEGIN OF T_BOM_C OCCURS 0,
         MATNR TYPE MARC-MATNR,
         WERKS TYPE MARC-WERKS,
         BESKZ TYPE MARC-BESKZ,
         FEVOR TYPE MARC-FEVOR,
       END OF T_BOM_C.

*------------取MARC
  SELECT
       MATNR
       WERKS
       BESKZ
       FEVOR
       INTO CORRESPONDING FIELDS OF TABLE T_BOM_C
       FROM MARC
       FOR ALL ENTRIES IN T_BOM
       WHERE MATNR = T_BOM-IDNRK
       AND WERKS = T_BOM-WERKS.

  LOOP AT T_BOM.
    READ TABLE T_BOM_C WITH KEY MATNR = T_BOM-IDNRK WERKS = T_BOM-WERKS.
    IF SY-SUBRC = 0.
      T_BOM-BESKZ = T_BOM_C-BESKZ.
      T_BOM-FEVOR = T_BOM_C-FEVOR.
      MODIFY T_BOM.
    ENDIF.
  ENDLOOP.
  REFRESH T_BOM_C.

  "过滤掉BOM中的外购件下组件
  DATA:L_STUFE TYPE STPOX-STUFE VALUE 1,
       L_FEVOR TYPE MARC-FEVOR,
       L_INDEX TYPE SY-INDEX,
       L_FLAG  TYPE  C.

  CLEAR L_FLAG.
  "处理生产调度
  T_TEMP_B[] = T_BOM[].
  IF R_F_BOM = 'X'.
    LOOP AT T_BOM WHERE BESKZ = 'E' AND FEVOR <> ''.
      CLEAR:L_STUFE,L_INDEX.
      L_STUFE = T_BOM-STUFE + 1.
      L_INDEX = SY-TABIX.
      DO.
        L_INDEX = L_INDEX + 1.
        READ TABLE T_TEMP_B INDEX L_INDEX.
        IF T_TEMP_B-BESKZ <> 'E' AND T_TEMP_B-STUFE = L_STUFE AND T_TEMP_B-FEVOR = ''.
          T_TEMP_B-FEVOR = T_BOM-FEVOR.
          MODIFY T_TEMP_B INDEX L_INDEX.
        ELSEIF T_TEMP_B-STUFE <= T_BOM-STUFE.
          EXIT.
        ENDIF.

      ENDDO.
      L_FLAG = 'Y'.
    ENDLOOP.

    REFRESH T_BOM.
    T_BOM[] =  T_TEMP_B[]. REFRESH T_TEMP_B.

    IF L_FLAG <> 'Y'.
      SELECT SINGLE FEVOR
        INTO L_FEVOR
        FROM MARC
        WHERE MATNR = P_MATNR
         AND WERKS = P_WERKS.

      "处理组件调度全部为空的情况
      LOOP AT T_BOM WHERE  FEVOR = ''.
        T_BOM-FEVOR = L_FEVOR.
        MODIFY T_BOM.
      ENDLOOP.
    ENDIF.

  ENDIF.

  LOOP AT T_BOM.
    IF T_BOM-STUFE > L_STUFE AND L_STUFE <> ''.
      DELETE T_BOM.
      CONTINUE.
    ENDIF.

    IF T_BOM-BESKZ = 'F' AND T_BOM-SOBSL <> '30' AND ( T_BOM-STUFE <= L_STUFE OR L_STUFE = '').
      L_STUFE = T_BOM-STUFE.

    ELSEIF T_BOM-STUFE <= L_STUFE AND  T_BOM-BESKZ = 'E' OR ( T_BOM-BESKZ = 'F' AND T_BOM-SOBSL = '30').
      CLEAR L_STUFE.

    ENDIF.
  ENDLOOP.



ENDFORM.                    "move_f

*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  get_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATNR .
  S_WERKS-LOW = P_WERKS1.
  S_WERKS-OPTION = 'EQ'.
  S_WERKS-SIGN = 'I'.
  APPEND S_WERKS.


  SELECT MARA~MATNR AS IDNRK
  MAKTX AS OJTXP
  MTART
  MEINS
  RGEKZ
  INTO CORRESPONDING FIELDS OF TABLE T_ITEMS
  FROM MARA JOIN MAKT
  ON MARA~MATNR = MAKT~MATNR
  JOIN MARC
  ON MARA~MATNR = MARC~MATNR
  WHERE MARA~MATNR IN S_MATNR
  AND MTART IN S_MTART
  AND WERKS IN S_WERKS
  AND EKGRP IN S_EKGRP
  AND DISPO IN S_DISPO
  AND SPRAS = '1'.
  .

  LOOP AT T_ITEMS.
    T_ITEMS-WERKS = P_WERKS1.
    MODIFY T_ITEMS.
  ENDLOOP.
  IF T_ITEMS[] IS INITIAL.
    MESSAGE '没有找到相关数据，请重新选择' TYPE 'E'.
  ENDIF.
ENDFORM.                    " get_matnr
*&---------------------------------------------------------------------*
*&      Form  get_po_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_PO_QUANTITY .

  SELECT EKPO~EBELN
  EKPO~EBELP
  EKPO~MATNR
  EKPO~WERKS
  EKPO~MENGE
  EKET~EINDT
  EKET~WEMNG
  INTO CORRESPONDING FIELDS OF TABLE T_EKPO
  FROM EKPO
  JOIN EKET ON EKPO~EBELN = EKET~EBELN AND EKPO~EBELP = EKET~EBELP
  FOR ALL ENTRIES IN T_ITEMS
  WHERE EKPO~MATNR =  T_ITEMS-IDNRK
  AND LOEKZ <> 'L'
  AND EKPO~ELIKZ = ''.

  DATA:BEGIN OF T_EKKO OCCURS 0 ,
         BSART LIKE EKKO-BSART,
         EBELN LIKE EKKO-EBELN,
       END OF T_EKKO.

  IF T_EKPO[] IS NOT INITIAL.
    SELECT BSART
           EBELN
           INTO CORRESPONDING FIELDS OF TABLE T_EKKO
           FROM EKKO
           FOR ALL ENTRIES IN T_EKPO
           WHERE EBELN = T_EKPO-EBELN .
  ENDIF.

  LOOP AT T_EKPO.
    READ TABLE T_EKKO WITH KEY EBELN = T_EKPO-EBELN.
    IF  SY-SUBRC = 0.
      T_EKPO-BSART = T_EKKO-BSART.
      MODIFY T_EKPO.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_po_quantity
*&---------------------------------------------------------------------*
*&      Form  get_t161v
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_T161V .
*-----------取得所有公司间采购订单类型
  SELECT
  BSART
  RESWK
  INTO CORRESPONDING FIELDS OF TABLE T_161V
  FROM T161V.
ENDFORM.                                                    "get_t161v

*&---------------------------------------------------------------------*
*&      Form  set_ALPGR
*&---------------------------------------------------------------------*
*       设置替代组：只显示优先级高的不显示优先级低
*----------------------------------------------------------------------*
FORM SET_ALPGR.
*  DATA:BEGIN OF T_TEMP OCCURS 0,
*       ALPGR TYPE STPO-ALPGR,
*       ALPRF TYPE STPO-ALPRF, "优先级
*       IDNRK TYPE STPO-IDNRK,
*       END OF T_TEMP.
*
*  DATA:BEGIN OF T_ALPGR OCCURS 0,
*       IDNRK TYPE STPO-IDNRK,
*       MATNR TYPE MARA-MATNR,  "替换物料
*       END OF T_ALPGR.
*
*  LOOP AT T_BOM WHERE ALPGR <> ''.
*    MOVE-CORRESPONDING T_BOM TO T_TEMP.
*    APPEND T_TEMP.
*  ENDLOOP.
*
*  SORT T_TEMP BY ALPGR ALPRF DESCENDING.
*  LOOP AT T_TEMP.
*    AT NEW ALPGR.
*      READ TABLE T_TEMP INDEX SY-TABIX.
*      T_ALPGR-IDNRK = T_TEMP-IDNRK.
*    ENDAT.
*
*    AT END OF ALPGR.
*      READ TABLE T_TEMP INDEX SY-TABIX.
*      T_ALPGR-MATNR = T_TEMP-IDNRK.
*      APPEND T_ALPGR.
*      CLEAR T_ALPGR.
*    ENDAT.
*  ENDLOOP.
*  REFRESH T_TEMP.
*
*  LOOP AT T_BOM.
*    READ TABLE T_ALPGR WITH KEY IDNRK = T_BOM-IDNRK.
*    IF SY-SUBRC = 0.
*      DELETE T_BOM.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE T_ALPGR WITH KEY MATNR = T_BOM-IDNRK.
*    IF SY-SUBRC = 0.
*      T_BOM-FLAG_C = 'X'.
*      MODIFY T_BOM.
*    ENDIF.
*
*    IF T_BOM-NFMAT <> ''.
*      T_BOM-IDNRK = T_BOM-NFMAT.
*      MODIFY T_BOM.
*    ENDIF.
*  ENDLOOP.
*  REFRESH T_ALPGR.

  "删除自制件、外件，除带料外协要展开
  LOOP AT T_BOM WHERE BESKZ = 'E' OR ( BESKZ = 'F' AND SOBSL = '30' ).
    DELETE T_BOM.
  ENDLOOP.

  IF T_BOM[] IS INITIAL.
    MESSAGE '没有找到相关数据，请重新选择' TYPE 'E'.
  ENDIF.

ENDFORM.                    "set_ALPGR

*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_ALV .
  DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GR_ALVGRID.


  CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
    EXPORTING
*     IS_STABLE      =
      I_SOFT_REFRESH = 'X'
    EXCEPTIONS
      FINISHED       = 1
      OTHERS         = 2.
ENDFORM.                    "REFRESH_ALV

*Text elements
*----------------------------------------------------------
* 001 BOM查询
* 002 BOM查询
* 003 物料查询


*Selection texts
*----------------------------------------------------------
* P_CAPID         BOM应用程序
* P_DATUV         有效起止日
* P_EMENG         需求数量
* P_MATNR         物料
* P_MATNR1         组件
* P_STLAL         可选的BOM
* P_WERKS         工厂
* P_WERKS1         工厂
* R_F_BOM         采购BOM
* R_ZCS12N         分层
* R_ZCS13N         汇总
* S_DISPO         MRP控制者
* S_EKGRP         采购组
* S_MATNR         物料编号
* S_MTART         物料类型


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   没有找到相关数据，请重新选择
*
* Message class: ZDEV
*000--------------------------------------------------------------------------------
