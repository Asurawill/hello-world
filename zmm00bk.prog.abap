*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
*&  创建人：林源
*&  创建时间：2014-12-31
*&  说明：物料主数据导入程序
*&---------------------------------------------------------------------*

REPORT ZMM00BK.

TYPE-POOLS: SLIS.

* COL.

TABLES: MLGN,
        T001L,
        T100. "MESSAGE
*----------------------------------------------------------------------*
*ALV data declarations
*----------------------------------------------------------------------*

DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
      GD_REPID     LIKE SY-REPID.

DATA: TIND(4) TYPE N.
DATA: ZWFELD(19).
FIELD-SYMBOLS: <FS1>.

DATA: BEGIN OF I_DATA OCCURS 0,
*&---------------------------------------------------------------------*
*& 组织级别
*&---------------------------------------------------------------------*
        IND_SECTOR(1)    TYPE C, "行业领域
        MATL_TYPE(4)     TYPE C, "物料类型
        MATERIAL(18)     TYPE C, "物料号
        MATL_DESC_ZH(40) TYPE C, "物料描述
        OLD_MAT_NO(18)   TYPE C, "旧物料号
        MATL_GROUP(9)    TYPE C, "物料组
        BASE_UOM(3)      TYPE C, "基本计量单位
***********************************NEW
        NET_WEIGHT(13)   TYPE C, "净重
        UNIT_OF_WT(3)    TYPE C, "重量单位
        EXTMATLGRP(18)   TYPE C, "外部物料组
        DOCUMENT(22)     TYPE C, "图纸号
        PROD_MEMO        TYPE STRING, "生产检验备忘录
        STD_DESCR        TYPE STRING, "行业标准描述

***********************************
*&---------------------------------------------------------------------*
*& 附加NEW
*&---------------------------------------------------------------------*
        LONGTEXT(132)    TYPE C, "物料长描述
        ALT_UNIT(3)      TYPE C, "替换单位
        NUMERATOR(5)     TYPE C, "分子
        DENOMINATR(5)    TYPE C, "分母

        PLANT(4)         TYPE C, "工厂
*&---------------------------------------------------------------------*
*& 销售数据1
*&---------------------------------------------------------------------*
        SALES_ORG(4)     TYPE C, "销售组织
        DISTR_CHAN(2)    TYPE C, "分销渠道
        DIVISION(2)      TYPE C, "产品组
        DELYG_PLNT(4)    TYPE C, "交货工厂
        TAXCLASS_1(1)    TYPE C, "税分类
*************************************************
*DISTR_CHAN TYPE STRING,"分销渠道（MVKE）
*SALES_UNIT TYPE STRING,"销售单位（MVKE）

***************************************************
*&---------------------------------------------------------------------*
*& 销售数据2(BAPI_MVKE)
*&---------------------------------------------------------------------*
        MATL_STATS(1)    TYPE C, "物料统计组
******************************************NEW
        ACCT_ASSGT(2)    TYPE C, "科目分配类别
        PR_REF_MAT(18)   TYPE C, "定价参考物料
******************************************NEW
        ITEM_CAT1(4)     TYPE C, "普通项目类别（MARA）
        ITEM_CAT(4)      TYPE C, "项目类别组(MVKE)
***************************************************
        MATL_GRP_1       TYPE STRING, "产品系列
        MATL_GRP_2       TYPE STRING, "研制单位
        MATL_GRP_3       TYPE STRING, "经营责任收入主体
        MATL_GRP_4       TYPE STRING, "经营责任新签主体
        MATL_GRP_5       TYPE STRING, "生产履约主体
***************************************************
*&---------------------------------------------------------------------*
*& 销售：一般工厂
*&---------------------------------------------------------------------*
***************************************************
        TRANS_GRP        TYPE STRING, "运输组（MARA）
        LOADINGGRP       TYPE STRING, "装载组(MARC)
        YZDW(132)        TYPE C, "研制单位
***************************************************
*AVAILCHECK(2) TYPE C, "可用性检查

*&---------------------------------------------------------------------*
*& 采购
*&---------------------------------------------------------------------*

        PUR_GROUP(3)     TYPE C, "采购组
        GR_PR_TIME(3)    TYPE C, "收货处理时间
        PO_UNIT          TYPE STRING,    " 订单单位（MARA）
        SOURCELIST(1)    TYPE C, " 源清单（MARC）
        XCHPF(1)         TYPE C, "批次管理
***NEW
        CRIT_PART(1),"关键部件标识(MARC)
        MATFRGTGRP(8)    TYPE C, "采购统计维度(MARC)
        MANU_MAT         TYPE STRING,
*AUTO_P_ORD(1) TYPE C, " 自动采购订单（MARC）
*QUOTAUSAGE TYPE STRING, " 配额协议（MARC）

*&---------------------------------------------------------------------*
*& MRP 1
*&---------------------------------------------------------------------*

        ABC_ID           TYPE STRING,     " ABC标识（MARC）
        MRP_GROUP(4)     TYPE C,  "MRP组
        MRP_TYPE(2)      TYPE C, "MRP类型
        MRP_CTRLER(3)    TYPE C, "MRP 控制者
        LOTSIZEKEY(2)    TYPE C, "批量大小
        MINLOTSIZE(18)   TYPE C, "最小批量大小(生产)
        MAXLOTSIZE(18)   TYPE C, "最大批量大小（生产）
        ASSY_SCRAP       TYPE STRING, " 装配报废(MARC)
        ROUND_VAL        TYPE STRING,  " 舍入值(MARC)
*REORDER_PT(18) TYPE C, "再订货点
*FIXED_LOT(17) TYPE C, "固定批量大小
*
**********************************************************
*PL_TI_FNCE TYPE STRING, " 计划时界(MARC)
**********************************************************
*&---------------------------------------------------------------------*
*& MRP 2
*&---------------------------------------------------------------------*

        PROC_TYPE(1)     TYPE C, "采购类型
        SPPROCTYPE(2)    TYPE C, "特殊采购类型
        ISS_ST_LOC(4)    TYPE C, "生产仓储地点
        SLOC_EXPRC(4)    TYPE C, "外部采购仓储地点
        BACKFLUSH(1)     TYPE C, "反冲
        INHSEPRODT(3)    TYPE C, "自制生产时间
        PLND_DELRY(3)    TYPE C, "计划交货时间
        SM_KEY(3)        TYPE C, "计划边际码
        SAFETY_STK(18)   TYPE C, "安全库存
*DETERM_GRP(4) TYPE C, "库存确定组
*BULK_MAT TYPE C, "散装物料标识

* SUPPLY_AREA(10) TYPE C, "缺省供应区域 add 2010-12-3
*&---------------------------------------------------------------------*
*& MRP 3
*&---------------------------------------------------------------------*
        AVAILCHECK       TYPE STRING, " 可用性检查(MARC)
***NEW（BAPI_MPGD）
        PLNG_MATL(18)    TYPE C, "计划物料
        PLNG_PLANT(4)    TYPE C, "计划工厂
        CONVFACTOR(10)   TYPE C, "计划物料转换因子
*PLAN_STRGP(2) TYPE C, "计划策略组
*CONSUMMODE(1) TYPE C, "消耗模式
*FWD_CONS(3) TYPE C, "向前消耗期间
*BWD_CONS(3) TYPE C, "消耗期间:逆向
****************************************************
****************************************************
*&---------------------------------------------------------------------*
*& MRP 4 BAPI_MARC
*&---------------------------------------------------------------------*
****************************************************
        DEP_REQ_ID(1)    TYPE C, "独立/集中
*NEW
        COMP_SCRAP(5)    TYPE C, "部件废品
*ALT_BOM_ID TYPE STRING, " 选择方法
****************************************************

*&---------------------------------------------------------------------*
*& 工作计划
*&---------------------------------------------------------------------*
*NEW
        ISSUE_UNIT(3)    TYPE C, "发货单位
*SERNO_PROF TYPE SERAIL, "序列号参数文件
*PRODUCTION_SCHEDULER(3) TYPE C, "生产管理员
******************************************************
*PRODPROF TYPE STRING,   " 生产计划参数文件(MARC)
*PROC_TIME TYPE STRING,  " 处理时间(MARC)
*BASE_QTY TYPE STRING,   " 基准数量(MARC)
*OVER_TOL TYPE STRING,   " 过度交货允差（百分比）(MARC)
*UNLIMITED TYPE STRING,  " 无限制交货(MARC)
*****************************************************
*&---------------------------------------------------------------------*
*& 工厂数据/存储 NEW
*&---------------------------------------------------------------------*
        STGE_LOC(4)      TYPE C, "库存地点(MARD)
**NEW
        STGE_BIN(10)     TYPE C, "仓位
        MINREMLIFE(4)    TYPE C, "最小剩余货架寿命（MARA）
        STGE_PD_UN(3)    TYPE C, "货架寿命期间标识(MARC)
*&---------------------------------------------------------------------*
*& 会计、成本
*&---------------------------------------------------------------------*

        VAL_CLASS(4)     TYPE C, "评估类
        PRICE_CTRL(1)    TYPE C, "价格控制指示符
        STD_PRICE(28)    TYPE C, "标准价格
        MOVING_PR(28)    TYPE C, "移动平均价
        PRICE_UNIT(5)    TYPE C, "价格单位


        PROFIT_CTR       TYPE CHAR10, "利润中心
        LOT_SIZE         TYPE STRING,
**NEW
        NO_COSTING(1)    TYPE C, "无成本核算（MARC）
****
        QTY_STRUCT(1)    TYPE C, "用QS的成本估算(MBEW)
        ORIG_MAT(1)      TYPE C, "物料相关的源
**NEW
        SPECPROCTY(2)    TYPE C, "特殊采购成本核算（MARC）
        PUR_STATUS(2)    TYPE C, "工厂特定的物料状态（MARC）
****
*ORIG_GROUP(4) TYPE C, "允甲#
*VARIANCE_KEY(6) TYPE C, "差异码
*********************************************************
*********************************************************
*LOT_SIZE(18) TYPE C, "批量产品成本核算

        OVERHEAD_GRP     TYPE STRING,
        PLNDPRICE1       TYPE STRING, "计划价格1
        PLNDPRDATE1(10)  TYPE C, "计划价格日期1
*PLNDPRICE2(28) TYPE C, "计划价格2
*PLNDPRDATE2(10) TYPE C, "计划价格日期2

        MESSAGE(40)      TYPE C,

      END OF I_DATA.

DATA T_DATA LIKE I_DATA OCCURS 0 WITH HEADER LINE. "用于ALV显示

*{ INSERT QASK901809 5

DATA TNAME(70) TYPE C.

DATA:BEGIN OF IT_T001L OCCURS 0,"扩展库存地点
       WERKS LIKE T001L-WERKS,
       LGORT LIKE T001L-LGORT,
     END OF IT_T001L.

DATA:WA_LGORT LIKE LINE OF IT_T001L.


*DATA: W_THEAD LIKE THEAD,
*T_LINES LIKE TLINE OCCURS 0 WITH HEADER LINE.
DATA SK(6) TYPE C .
*DATA: L TYPE I,
*I(2) TYPE N.
*DATA STR(1) TYPE C VALUE ' '.
*DATA: L_STR(18) TYPE C,
*OFF TYPE I,
*LEN TYPE I.
*DATA: L_T TYPE F VALUE '0.5',
*L_A TYPE C.

DATA: HEADDATA             TYPE BAPIMATHEAD,
      CLIENTDATA           TYPE BAPI_MARA,
      CLIENTDATAX          TYPE BAPI_MARAX,
      PLANTDATA            TYPE BAPI_MARC,
      PLANTDATAX           TYPE BAPI_MARCX,
      FORECASTPARAMETERS   TYPE BAPI_MPOP,
      FORECASTPARAMETERSX  TYPE BAPI_MPOPX,
      PLANNINGDATA         TYPE BAPI_MPGD,
      PLANNINGDATAX        TYPE BAPI_MPGDX,
      STORAGELOCATIONDATA  TYPE BAPI_MARD,
      STORAGELOCATIONDATAX TYPE BAPI_MARDX,
      WAREHOUSENUMBERDATA  TYPE BAPI_MLGN,
      WAREHOUSENUMBERDATAX TYPE BAPI_MLGNX,
      SALESDATA            TYPE BAPI_MVKE,
      SALESDATAX           TYPE BAPI_MVKEX,
      STORAGETYPEDATA      TYPE BAPI_MLGT,
      STORAGETYPEDATAX     TYPE BAPI_MLGTX,
      VALUATIONDATA        TYPE BAPI_MBEW,
      VALUATIONDATAX       TYPE BAPI_MBEWX,
      RETURN               TYPE BAPIRET2,
      MATERIALDESCRIPTION  TYPE TABLE OF BAPI_MAKT WITH HEADER LINE,
      TAXCLASSIFICATIONS   TYPE TABLE OF BAPI_MLAN WITH HEADER LINE,
      MATERIALLONGTEXT     TYPE TABLE OF BAPI_MLTX WITH HEADER LINE,
      RETURNMESSAGES       TYPE TABLE OF BAPI_MATRETURN2,
      UNITSOFMEASURE       TYPE TABLE OF BAPI_MARM WITH HEADER LINE,
      UNITSOFMEASUREX      TYPE TABLE OF BAPI_MARMX WITH HEADER LINE.

DATA: BEGIN OF LOG_RETURN OCCURS 0,
        MATNR   LIKE MARA-MATNR,
        MESSAGE TYPE BAPI_MSG,
      END OF LOG_RETURN.
*DATA: BEGIN OF TEMP OCCURS 0,
*PLANT TYPE WERKS_D,
*STGE_LOC_1 TYPE LGORT_D,
*STGE_LOC_2 TYPE LGORT_D,
*END OF TEMP.
DATA: SEC TYPE F VALUE '0.1'.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS: MATERIAL LIKE RLGRAP-FILENAME OBLIGATORY. "物料导入摸版

PARAMETERS: A AS CHECKBOX MODIF ID 1.
PARAMETERS: B AS CHECKBOX MODIF ID 2.
PARAMETERS: C AS CHECKBOX MODIF ID 2.
PARAMETERS: D AS CHECKBOX MODIF ID 2.
PARAMETERS: E AS CHECKBOX MODIF ID 2.
PARAMETERS: F AS CHECKBOX MODIF ID 2.
PARAMETERS: H AS CHECKBOX MODIF ID 2.
PARAMETERS: I AS CHECKBOX MODIF ID 2.
PARAMETERS: G AS CHECKBOX MODIF ID 2.

SELECTION-SCREEN END OF BLOCK BLK1.


*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'G'.
      SCREEN-INVISIBLE = 1.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR MATERIAL.
  PERFORM SELECT_PATH."选择路径


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.
  IF NOT MATERIAL IS INITIAL.
    PERFORM UPLOAD_MATERIAL_DATA."数据导入

    PERFORM SAVE_MATERIAL_DATA. "保存数据

    PERFORM BUILD_FIELDCATALOG.

    PERFORM BUILD_LAYOUT.

    PERFORM DISPLAY_ALV_REPORT.

  ENDIF.

*&---------------------------------------------------------------------*
*& Form UPLOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*

FORM UPLOAD_MATERIAL_DATA.
  DATA: L_RAW TYPE TRUXS_T_TEXT_DATA.

* 从EXCEL上传到内表
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*     I_LINE_HEADER        =
      I_TAB_RAW_DATA       = L_RAW
      I_FILENAME           = MATERIAL
    TABLES
      I_TAB_CONVERTED_DATA = I_DATA
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  DELETE I_DATA INDEX 1.
  DELETE I_DATA INDEX 1.

  LOOP AT I_DATA.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = I_DATA-PR_REF_MAT
      IMPORTING
        OUTPUT = I_DATA-PR_REF_MAT
* EXCEPTIONS
*       LENGTH_ERROR       = 1
*       OTHERS = 2
      .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA-MATERIAL
      IMPORTING
        OUTPUT = I_DATA-MATERIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_DATA-PLNG_MATL
      IMPORTING
        OUTPUT = I_DATA-PLNG_MATL.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT          = I_DATA-STGE_PD_UN
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = I_DATA-STGE_PD_UN
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT          = I_DATA-BASE_UOM
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = I_DATA-BASE_UOM
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT          = I_DATA-ALT_UNIT
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = I_DATA-ALT_UNIT
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT          = I_DATA-PO_UNIT
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = I_DATA-PO_UNIT
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT          = I_DATA-PROFIT_CTR
      IMPORTING
        OUTPUT         = I_DATA-PROFIT_CTR
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.



    MODIFY I_DATA.
  ENDLOOP.

ENDFORM. " UPLOAD_MATERIAL_DATA

*&---------------------------------------------------------------------*
*& Form SAVE_MATERIAL_DATA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*

FORM SAVE_MATERIAL_DATA.
  DATA TNAME TYPE STRING.
  DATA TMARA TYPE TABLE OF MARA WITH HEADER LINE.

  IF I_DATA[] IS NOT INITIAL.
    SELECT *
      INTO TABLE TMARA
      FROM MARA
      FOR ALL ENTRIES IN I_DATA
      WHERE MATNR = I_DATA-MATERIAL"已经存在的基本视图
        AND LVORM = ''.
  ENDIF.

  LOOP AT I_DATA.

************************************************************************
* HEADDATA
************************************************************************
    PERFORM INPUT_HEADDATA.
************************************************************************
* CLIENTDATA
************************************************************************
    IF A = 'X'.

      PERFORM INPUT USING I_DATA-BASE_UOM CHANGING CLIENTDATA-BASE_UOM.     "基本计量单位
      PERFORM INPUT USING I_DATA-MATL_GROUP CHANGING CLIENTDATA-MATL_GROUP. "物料组
      PERFORM INPUT USING I_DATA-OLD_MAT_NO CHANGING CLIENTDATA-OLD_MAT_NO. "旧物料号
      PERFORM INPUT USING I_DATA-DIVISION CHANGING CLIENTDATA-DIVISION.     "产品组
*   PERFORM INPUT USING I_DATA-PUR_STATUS CHANGING CLIENTDATA-PUR_STATUS.
      PERFORM INPUT USING I_DATA-TRANS_GRP   CHANGING CLIENTDATA-TRANS_GRP."运输组
      PERFORM INPUT USING I_DATA-PO_UNIT      CHANGING CLIENTDATA-PO_UNIT. "订单单位，采购
      PERFORM INPUT USING I_DATA-NET_WEIGHT CHANGING CLIENTDATA-NET_WEIGHT.
      PERFORM INPUT USING I_DATA-UNIT_OF_WT CHANGING CLIENTDATA-UNIT_OF_WT.
      PERFORM INPUT USING I_DATA-EXTMATLGRP CHANGING CLIENTDATA-EXTMATLGRP.
      PERFORM INPUT USING I_DATA-DOCUMENT   CHANGING CLIENTDATA-DOCUMENT.
      PERFORM INPUT USING I_DATA-DOCUMENT   CHANGING CLIENTDATA-DOCUMENT.
      PERFORM INPUT USING I_DATA-PROD_MEMO  CHANGING CLIENTDATA-PROD_MEMO.
      PERFORM INPUT USING I_DATA-STD_DESCR   CHANGING CLIENTDATA-STD_DESCR.
*   PERFORM INPUT USING I_DATA-XCHPF      CHANGING CLIENTDATA-BATCH_MGMT.

      PERFORM INPUT1 USING I_DATA-BASE_UOM CHANGING CLIENTDATAX-BASE_UOM.
      PERFORM INPUT1 USING I_DATA-MATL_GROUP CHANGING CLIENTDATAX-MATL_GROUP.
      PERFORM INPUT1 USING I_DATA-OLD_MAT_NO CHANGING CLIENTDATAX-OLD_MAT_NO.
      PERFORM INPUT1 USING I_DATA-DIVISION CHANGING CLIENTDATAX-DIVISION.
*   PERFORM INPUT1 USING I_DATA-PUR_STATUS CHANGING CLIENTDATAX-PUR_STATUS.
      PERFORM INPUT1 USING I_DATA-TRANS_GRP   CHANGING CLIENTDATAX-TRANS_GRP.
      PERFORM INPUT1 USING I_DATA-PO_UNIT      CHANGING CLIENTDATAX-PO_UNIT.
      PERFORM INPUT1 USING I_DATA-NET_WEIGHT CHANGING CLIENTDATAX-NET_WEIGHT.
      PERFORM INPUT1 USING I_DATA-UNIT_OF_WT CHANGING CLIENTDATAX-UNIT_OF_WT.
      PERFORM INPUT1 USING I_DATA-EXTMATLGRP CHANGING CLIENTDATAX-EXTMATLGRP."
      PERFORM INPUT1 USING I_DATA-DOCUMENT   CHANGING CLIENTDATAX-DOCUMENT."图纸号
      PERFORM INPUT1 USING I_DATA-PROD_MEMO  CHANGING CLIENTDATAX-PROD_MEMO.
      PERFORM INPUT1 USING I_DATA-STD_DESCR   CHANGING CLIENTDATAX-STD_DESCR.
*   PERFORM INPUT1 USING I_DATA-XCHPF      CHANGING CLIENTDATAX-BATCH_MGMT.
*******************************************************
    ENDIF.

************************************************************************
* CLIENTDATAX
************************************************************************
*
*    IF A = 'X'.
*      IF I_DATA-BASE_UOM <> ' '. "基本计量单位
*        CLIENTDATAX-BASE_UOM = 'X'.
*      ENDIF.
*      IF I_DATA-MATL_GROUP <> ' '. "物料组
*        CLIENTDATAX-MATL_GROUP = 'X'.
*      ENDIF.
*      IF I_DATA-OLD_MAT_NO <> ' '. "助记码(旧物料号)
*        CLIENTDATAX-OLD_MAT_NO = 'X'.
*      ENDIF.
*      IF I_DATA-DIVISION <> ' '. "产品组
*        CLIENTDATAX-DIVISION = 'X'.
*      ENDIF.
*      CLIENTDATAX-DSN_OFFICE = 'X'.
*    ENDIF.

*    IF B = 'X'.
*      CLIENTDATAX-TRANS_GRP = 'X'.
*    ENDIF.

************************************************************************
* 附加数据(基本视图)
************************************************************************

    IF A = 'X'.
* 添加物料描述
      CLEAR MATERIALDESCRIPTION.
      REFRESH MATERIALDESCRIPTION.
      IF I_DATA-MATL_DESC_ZH <> ' '. "物料描述
        IF I_DATA-MATL_DESC_ZH = '#'.
          MATERIALDESCRIPTION-LANGU = '1'.
          MATERIALDESCRIPTION-MATL_DESC = ' '.
          APPEND MATERIALDESCRIPTION.
        ELSE.
          MATERIALDESCRIPTION-LANGU = '1'.
          MATERIALDESCRIPTION-MATL_DESC = I_DATA-MATL_DESC_ZH.
          APPEND MATERIALDESCRIPTION.
        ENDIF.
      ENDIF.

*添加物料长描述
      MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
      MATERIALLONGTEXT-TEXT_NAME = I_DATA-MATERIAL.
      MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
      MATERIALLONGTEXT-LANGU = '1'.
      MATERIALLONGTEXT-FORMAT_COL = '*'.
      MATERIALLONGTEXT-TEXT_LINE = I_DATA-LONGTEXT.

      APPEND MATERIALLONGTEXT.

      PERFORM INPUT USING I_DATA-ALT_UNIT CHANGING UNITSOFMEASURE-ALT_UNIT.
      PERFORM INPUT USING I_DATA-NUMERATOR CHANGING UNITSOFMEASURE-NUMERATOR.
      PERFORM INPUT USING I_DATA-DENOMINATR CHANGING UNITSOFMEASURE-DENOMINATR.

      PERFORM INPUT  USING I_DATA-ALT_UNIT CHANGING UNITSOFMEASUREX-ALT_UNIT.
      PERFORM INPUT1 USING I_DATA-NUMERATOR CHANGING UNITSOFMEASUREX-NUMERATOR.
      PERFORM INPUT1 USING I_DATA-DENOMINATR CHANGING UNITSOFMEASUREX-DENOMINATR.

      IF UNITSOFMEASURE-ALT_UNIT IS NOT INITIAL.
        APPEND UNITSOFMEASURE.
        APPEND UNITSOFMEASUREX.
      ENDIF.

    ENDIF.

************************************************************************
* PLANTDATA
************************************************************************

    IF B = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT.
      PLANTDATAX-PLANT = I_DATA-PLANT.


      CONCATENATE I_DATA-MATERIAL I_DATA-SALES_ORG I_DATA-DISTR_CHAN INTO TNAME.

      MATERIALLONGTEXT-APPLOBJECT = 'MVKE'.
      MATERIALLONGTEXT-TEXT_NAME = TNAME.
      MATERIALLONGTEXT-TEXT_ID = '0001'.
      MATERIALLONGTEXT-LANGU = '1'.
      MATERIALLONGTEXT-FORMAT_COL = '*'.
      MATERIALLONGTEXT-TEXT_LINE = I_DATA-YZDW.

      APPEND MATERIALLONGTEXT.

*   PERFORM INPUT USING I_DATA-ALT_UNIT CHANGING UNITSOFMEASURE-ALT_UNIT.
*   PERFORM INPUT USING I_DATA-NUMERATOR CHANGING UNITSOFMEASURE-NUMERATOR.
*   PERFORM INPUT USING I_DATA-DENOMINATR CHANGING UNITSOFMEASURE-DENOMINATR.
*
*   PERFORM INPUT  USING I_DATA-ALT_UNIT CHANGING UNITSOFMEASUREX-ALT_UNIT.
*   PERFORM INPUT1 USING I_DATA-NUMERATOR CHANGING UNITSOFMEASUREX-NUMERATOR.
*   PERFORM INPUT1 USING I_DATA-DENOMINATR CHANGING UNITSOFMEASUREX-DENOMINATR.
*
*   IF UNITSOFMEASURE-ALT_UNIT IS NOT INITIAL.
*   APPEND UNITSOFMEASURE.
*   APPEND UNITSOFMEASUREX.
*   ENDIF.
***************************************************
** Add By Linyuan 20120826
*      IF I_DATA-LOADINGGRP <> ' '. "序列号参数文件
*        IF I_DATA-LOADINGGRP  = '#'.
*          PLANTDATA-LOADINGGRP  = ' '.
*        ELSE.
*          PLANTDATA-LOADINGGRP  = I_DATA-LOADINGGRP .
*        ENDIF.
*      ENDIF.
*
*      IF I_DATA-PROFIT_CTR <> ' '.
*        IF I_DATA-PROFIT_CTR = '#'.
*          PLANTDATA-PROFIT_CTR = ' '.
*        ELSE.
*          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
*        ENDIF.
*      ENDIF.
***************************************************
      PERFORM INPUT USING I_DATA-LOADINGGRP CHANGING PLANTDATA-LOADINGGRP.
      PERFORM INPUT USING I_DATA-PROFIT_CTR CHANGING PLANTDATA-PROFIT_CTR.
      PERFORM INPUT USING I_DATA-LOT_SIZE CHANGING PLANTDATA-LOT_SIZE.
      PERFORM INPUT USING I_DATA-PLANT CHANGING PLANTDATA-PLANT.
      PERFORM INPUT USING I_DATA-AVAILCHECK CHANGING PLANTDATA-AVAILCHECK.
      PERFORM INPUT USING I_DATA-MRP_GROUP CHANGING PLANTDATA-MRP_GROUP.
      PERFORM INPUT USING I_DATA-TRANS_GRP CHANGING CLIENTDATA-TRANS_GRP.
      PERFORM INPUT USING I_DATA-MATL_STATS CHANGING SALESDATA-MATL_STATS.
      PERFORM INPUT USING I_DATA-ACCT_ASSGT CHANGING SALESDATA-ACCT_ASSGT.
      PERFORM INPUT USING I_DATA-PR_REF_MAT CHANGING SALESDATA-PR_REF_MAT.
      PERFORM INPUT USING I_DATA-ITEM_CAT1 CHANGING CLIENTDATA-ITEM_CAT.
      PERFORM INPUT USING I_DATA-DIVISION CHANGING CLIENTDATA-DIVISION.     "产品组
*   PERFORM INPUT USING I_DATA-PUR_STATUS CHANGING CLIENTDATA-PUR_STATUS.
      PERFORM INPUT USING I_DATA-TRANS_GRP   CHANGING CLIENTDATA-TRANS_GRP."运输组

      PERFORM INPUT1 USING I_DATA-LOADINGGRP CHANGING PLANTDATAX-LOADINGGRP.
      PERFORM INPUT1 USING I_DATA-PROFIT_CTR CHANGING PLANTDATAX-PROFIT_CTR.
      PERFORM INPUT1 USING I_DATA-LOT_SIZE CHANGING PLANTDATAX-LOT_SIZE.
      PERFORM INPUT1 USING I_DATA-PLANT CHANGING PLANTDATAX-PLANT.
      PERFORM INPUT1 USING I_DATA-AVAILCHECK CHANGING PLANTDATAX-AVAILCHECK.
      PERFORM INPUT1 USING I_DATA-MRP_GROUP CHANGING PLANTDATAX-MRP_GROUP.
      PERFORM INPUT1 USING I_DATA-TRANS_GRP CHANGING CLIENTDATAX-TRANS_GRP.
      PERFORM INPUT1 USING I_DATA-MATL_STATS CHANGING SALESDATAX-MATL_STATS.
      PERFORM INPUT1 USING I_DATA-ACCT_ASSGT CHANGING SALESDATAX-ACCT_ASSGT.
      PERFORM INPUT1 USING I_DATA-PR_REF_MAT CHANGING SALESDATAX-PR_REF_MAT.
      PERFORM INPUT1 USING I_DATA-ITEM_CAT1 CHANGING CLIENTDATAX-ITEM_CAT.
      PERFORM INPUT1 USING I_DATA-DIVISION CHANGING CLIENTDATAX-DIVISION.     "产品组
*   PERFORM INPUT1 USING I_DATA-PUR_STATUS CHANGING CLIENTDATA-PUR_STATUS.
      PERFORM INPUT1 USING I_DATA-TRANS_GRP   CHANGING CLIENTDATAX-TRANS_GRP."运输组

    ENDIF.

    IF C = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT. "工厂
      PLANTDATAX-PLANT = I_DATA-PLANT.
*
*      IF I_DATA-PUR_GROUP <> ' '. "采购组
*        IF I_DATA-PUR_GROUP = '#'.
*          PLANTDATA-PUR_GROUP = ' '.
*        ELSE.
*          PLANTDATA-PUR_GROUP = I_DATA-PUR_GROUP.
*        ENDIF.
*      ENDIF.
*
*      IF I_DATA-MRP_GROUP <> ' '. "MRP组
*        IF I_DATA-MRP_GROUP = '#'.
*          PLANTDATA-MRP_GROUP = ' '.
*        ELSE.
*          PLANTDATA-MRP_GROUP = I_DATA-MRP_GROUP.
*        ENDIF.
*      ENDIF.
*
*      IF I_DATA-GR_PR_TIME <> ' '. "收货处理时间
*        IF I_DATA-GR_PR_TIME = '#'.
*          PLANTDATA-GR_PR_TIME = ' '.
*        ELSE.
*          PLANTDATA-GR_PR_TIME = I_DATA-GR_PR_TIME.
*        ENDIF.
*      ENDIF.
*
********************************************************
*      " Add by Linyuan 20120826
*
*      IF I_DATA-SOURCELIST <> ' '. "源清单
*        IF I_DATA-SOURCELIST = '#'.
*          PLANTDATA-SOURCELIST = ' '.
*        ELSE.
*          PLANTDATA-SOURCELIST = I_DATA-SOURCELIST.
*        ENDIF.
*      ENDIF.
*
*      IF I_DATA-PROFIT_CTR <> ' '.
*        IF I_DATA-PROFIT_CTR = '#'.
*          PLANTDATA-PROFIT_CTR = ' '.
*        ELSE.
*          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
*        ENDIF.
*      ENDIF.

      PERFORM INPUT USING I_DATA-PUR_GROUP CHANGING PLANTDATA-PUR_GROUP.
      PERFORM INPUT USING I_DATA-MRP_GROUP CHANGING PLANTDATA-MRP_GROUP.
      PERFORM INPUT USING I_DATA-GR_PR_TIME CHANGING PLANTDATA-GR_PR_TIME.
      PERFORM INPUT USING I_DATA-SOURCELIST CHANGING PLANTDATA-SOURCELIST.
      PERFORM INPUT USING I_DATA-PROFIT_CTR CHANGING PLANTDATA-PROFIT_CTR.
      PERFORM INPUT USING I_DATA-CRIT_PART  CHANGING PLANTDATA-CRIT_PART.
      PERFORM INPUT USING I_DATA-MATFRGTGRP CHANGING PLANTDATA-MATFRGTGRP.
*     PERFORM INPUT USING I_DATA-XCHPF      CHANGING CLIENTDATA-BATCH_MGMT.
      PERFORM INPUT USING I_DATA-XCHPF      CHANGING PLANTDATA-BATCH_MGMT.
      PERFORM INPUT USING I_DATA-PO_UNIT      CHANGING CLIENTDATA-PO_UNIT. "订单单位，采购
      PERFORM INPUT USING I_DATA-MANU_MAT   CHANGING CLIENTDATA-MANU_MAT.

      PERFORM INPUT1 USING I_DATA-PUR_GROUP CHANGING PLANTDATAX-PUR_GROUP.
      PERFORM INPUT1 USING I_DATA-MRP_GROUP CHANGING PLANTDATAX-MRP_GROUP.
      PERFORM INPUT1 USING I_DATA-GR_PR_TIME CHANGING PLANTDATAX-GR_PR_TIME.
      PERFORM INPUT1 USING I_DATA-SOURCELIST CHANGING PLANTDATAX-SOURCELIST.
      PERFORM INPUT1 USING I_DATA-PROFIT_CTR CHANGING PLANTDATAX-PROFIT_CTR.
      PERFORM INPUT1 USING I_DATA-CRIT_PART CHANGING PLANTDATAX-CRIT_PART.
      PERFORM INPUT1 USING I_DATA-MATFRGTGRP CHANGING PLANTDATAX-MATFRGTGRP.
*     PERFORM INPUT1 USING I_DATA-XCHPF      CHANGING CLIENTDATAX-BATCH_MGMT.
      PERFORM INPUT1 USING I_DATA-XCHPF      CHANGING PLANTDATAX-BATCH_MGMT.
      PERFORM INPUT1 USING  I_DATA-PO_UNIT     CHANGING CLIENTDATAX-PO_UNIT. "订单单位，采购
      PERFORM INPUT1 USING I_DATA-MANU_MAT   CHANGING CLIENTDATAX-MANU_MAT.

*******************************************************
    ENDIF.

    IF D = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT. "工厂
      PLANTDATAX-PLANT = I_DATA-PLANT.

      IF I_DATA-MRP_GROUP <> ' '. "MRP组
        IF I_DATA-MRP_GROUP = '#'.
          PLANTDATA-MRP_GROUP = ' '.
        ELSE.
          PLANTDATA-MRP_GROUP = I_DATA-MRP_GROUP.
        ENDIF.
      ENDIF.

      IF I_DATA-MRP_TYPE <> ' '. "物料需求计划类型
        IF I_DATA-MRP_TYPE = '#'.
          PLANTDATA-MRP_TYPE = ' '.
        ELSE.
          PLANTDATA-MRP_TYPE = I_DATA-MRP_TYPE.
        ENDIF.
      ENDIF.

      IF I_DATA-MRP_CTRLER <> ' '. "MRP 控制者
        IF I_DATA-MRP_CTRLER = '#'.
          PLANTDATA-MRP_CTRLER = ' '.
        ELSE.
          PLANTDATA-MRP_CTRLER = I_DATA-MRP_CTRLER.
        ENDIF.
      ENDIF.
      IF I_DATA-LOTSIZEKEY <> ' '. "批量大小
        IF I_DATA-LOTSIZEKEY = '#'.
          PLANTDATA-LOTSIZEKEY = ' '.
        ELSE.
          PLANTDATA-LOTSIZEKEY = I_DATA-LOTSIZEKEY.
        ENDIF.
      ENDIF.
      IF I_DATA-MINLOTSIZE <> ' '. "最小批量大小
        IF I_DATA-MINLOTSIZE = '#'.
          PLANTDATA-MINLOTSIZE = ' '.
        ELSE.
          PLANTDATA-MINLOTSIZE = I_DATA-MINLOTSIZE.
        ENDIF.
      ENDIF.
      IF I_DATA-MAXLOTSIZE <> ' '. "最大批量大小
        IF I_DATA-MAXLOTSIZE = '#'.
          PLANTDATA-MAXLOTSIZE = ' '.
        ELSE.
          PLANTDATA-MAXLOTSIZE = I_DATA-MAXLOTSIZE.
        ENDIF.
      ENDIF.
      IF I_DATA-PROC_TYPE <> ' '. "获取类型
        IF I_DATA-PROC_TYPE = '#'.
          PLANTDATA-PROC_TYPE = ' '.
        ELSE.
          PLANTDATA-PROC_TYPE = I_DATA-PROC_TYPE.
        ENDIF.
      ENDIF.
      IF I_DATA-SPPROCTYPE <> ' '. "特殊采购类型
        IF I_DATA-SPPROCTYPE = '#'.
          PLANTDATA-SPPROCTYPE = ' '.
        ELSE.
          PLANTDATA-SPPROCTYPE = I_DATA-SPPROCTYPE.
        ENDIF.
      ENDIF.
      IF I_DATA-BACKFLUSH <> ' '. "反冲
        IF I_DATA-BACKFLUSH = '#'.
          PLANTDATA-BACKFLUSH = ' '.
        ELSE.
          PLANTDATA-BACKFLUSH = I_DATA-BACKFLUSH.
        ENDIF.
      ENDIF.
      IF I_DATA-ISS_ST_LOC <> ' '. "发货库存地点
        IF I_DATA-ISS_ST_LOC = '#'.
          PLANTDATA-ISS_ST_LOC = ' '.
        ELSE.
          PLANTDATA-ISS_ST_LOC = I_DATA-ISS_ST_LOC.
        ENDIF.
      ENDIF.
      IF I_DATA-SLOC_EXPRC <> ' '. "外部采购的缺省仓储位置
        IF I_DATA-SLOC_EXPRC = '#'.
          PLANTDATA-SLOC_EXPRC = ' '.
        ELSE.
          PLANTDATA-SLOC_EXPRC = I_DATA-SLOC_EXPRC.
        ENDIF.
      ENDIF.

* PLANTDATA-BULK_MAT = I_DATA-BULK_MAT. "标识：散装物料

      IF I_DATA-INHSEPRODT <> ' '. "厂内生产时间
        IF I_DATA-INHSEPRODT = '#'.
          PLANTDATA-INHSEPRODT = ' '.
        ELSE.
          PLANTDATA-INHSEPRODT = I_DATA-INHSEPRODT.
        ENDIF.
      ENDIF.
      IF I_DATA-PLND_DELRY <> ' '. "计划的天数内交货
        IF I_DATA-PLND_DELRY = '#'.
          PLANTDATA-PLND_DELRY = ' '.
        ELSE.
          PLANTDATA-PLND_DELRY = I_DATA-PLND_DELRY.
        ENDIF.
      ENDIF.
      IF I_DATA-SM_KEY <> ' '. "浮动的计划边际码
        IF I_DATA-SM_KEY = '#'.
          PLANTDATA-SM_KEY = ' '.
        ELSE.
          PLANTDATA-SM_KEY = I_DATA-SM_KEY.
        ENDIF.
      ENDIF.
      IF I_DATA-SAFETY_STK <> ' '. "安全库存
        IF I_DATA-SAFETY_STK = '#'.
          PLANTDATA-SAFETY_STK = ' '.
        ELSE.
          PLANTDATA-SAFETY_STK = I_DATA-SAFETY_STK.
        ENDIF.
      ENDIF.

      IF I_DATA-DEP_REQ_ID <> ' '. "对于独立和集中需求的相关需求标识\
        IF I_DATA-DEP_REQ_ID = '#'.
          PLANTDATA-DEP_REQ_ID = ' '.
        ELSE.
          PLANTDATA-DEP_REQ_ID = I_DATA-DEP_REQ_ID.
        ENDIF.
      ENDIF.
      " Add by Linyuan 20120826
      IF I_DATA-ABC_ID <> ' '. " ABC标识
        IF I_DATA-ABC_ID = '#'.
          PLANTDATA-ABC_ID = ' '.
        ELSE.
          PLANTDATA-ABC_ID = I_DATA-ABC_ID.
        ENDIF.
      ENDIF.

      IF I_DATA-ASSY_SCRAP <> ' '. " 装配报废
        IF I_DATA-ASSY_SCRAP = '#'.
          PLANTDATA-ASSY_SCRAP = ' '.
        ELSE.
          PLANTDATA-ASSY_SCRAP = I_DATA-ASSY_SCRAP.
        ENDIF.
      ENDIF.

      IF I_DATA-ROUND_VAL <> ' '. " 舍入值
        IF I_DATA-ROUND_VAL = '#'.
          PLANTDATA-ROUND_VAL = ' '.
        ELSE.
          PLANTDATA-ROUND_VAL = I_DATA-ROUND_VAL.
        ENDIF.
      ENDIF.

      IF I_DATA-ROUND_VAL <> ' '. " 舍入值
        IF I_DATA-ROUND_VAL = '#'.
          PLANTDATAX-ROUND_VAL = ' '.
        ELSE.
          PLANTDATAX-ROUND_VAL = I_DATA-ROUND_VAL.
        ENDIF.
      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATA-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
        ENDIF.
      ENDIF.

      IF I_DATA-PUR_GROUP <> ' '. "MRP组
        IF I_DATA-PUR_GROUP = '#'.
          PLANTDATA-PUR_GROUP = ' '.
        ELSE.
          PLANTDATA-PUR_GROUP = I_DATA-PUR_GROUP.
        ENDIF.
      ENDIF.

      PLANNINGDATA-PLANT = I_DATA-PLANT.
      PLANNINGDATAX-PLANT = I_DATA-PLANT.

      PERFORM INPUT USING I_DATA-AVAILCHECK CHANGING PLANTDATA-AVAILCHECK.
      PERFORM INPUT USING I_DATA-PLNG_MATL CHANGING PLANNINGDATA-PLNG_MATL.
      PERFORM INPUT USING I_DATA-PLNG_PLANT CHANGING PLANNINGDATA-PLNG_PLANT.
      PERFORM INPUT USING I_DATA-CONVFACTOR CHANGING PLANNINGDATA-CONVFACTOR.

      PERFORM INPUT1 USING I_DATA-AVAILCHECK CHANGING PLANTDATAX-AVAILCHECK.
      PERFORM INPUT1 USING I_DATA-PLNG_MATL CHANGING PLANNINGDATAX-PLNG_MATL.
      PERFORM INPUT1 USING I_DATA-PLNG_PLANT CHANGING PLANNINGDATAX-PLNG_PLANT.
      PERFORM INPUT1 USING I_DATA-CONVFACTOR CHANGING PLANNINGDATAX-CONVFACTOR.
*******************************************************
    ENDIF.

    IF E = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT.
      IF I_DATA-MRP_GROUP <> ' '. "MRP组
        IF I_DATA-MRP_GROUP = '#'.
          PLANTDATA-MRP_GROUP = ' '.
        ELSE.
          PLANTDATA-MRP_GROUP = I_DATA-MRP_GROUP.
        ENDIF.
      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATA-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
        ENDIF.
      ENDIF.
**************************************************************
    ENDIF.
    IF F = 'X'.

      STORAGELOCATIONDATA-PLANT = I_DATA-PLANT.

      IF I_DATA-MRP_GROUP <> ' '. "MRP组
        IF I_DATA-MRP_GROUP = '#'.
          PLANTDATA-MRP_GROUP = ' '.
        ELSE.
          PLANTDATA-MRP_GROUP = I_DATA-MRP_GROUP.
        ENDIF.
      ENDIF.
*      IF I_DATA-DETERM_GRP <> ' '. "库存确定组
*        IF I_DATA-DETERM_GRP = '#'.
*          PLANTDATA-DETERM_GRP = ' '.
*        ELSE.
*          PLANTDATA-DETERM_GRP = I_DATA-DETERM_GRP.
*        ENDIF.
*      ENDIF.
*      IF I_DATA-BULK_MAT <> ' '. "散装物料标识
*        IF I_DATA-BULK_MAT = '#'.
*          PLANTDATA-BULK_MAT = ' '.
*        ELSE.
*          PLANTDATA-BULK_MAT = I_DATA-BULK_MAT.
*        ENDIF.
*      ENDIF.
      IF I_DATA-STGE_LOC <> ' '.
        IF I_DATA-STGE_LOC = '#'.
          STORAGELOCATIONDATA-STGE_LOC = ' '.
        ELSE.
          STORAGELOCATIONDATA-STGE_LOC = I_DATA-STGE_LOC.
        ENDIF.
      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATA-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
        ENDIF.
      ENDIF.
    ENDIF.
    IF G = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATA-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
        ENDIF.
      ENDIF.

    ENDIF.
    IF H = 'X'.
      PLANTDATA-PLANT = I_DATA-PLANT.
*      IF I_DATA-LOT_SIZE <> ' '.
*        IF I_DATA-LOT_SIZE = '#'.
*          PLANTDATA-LOT_SIZE = ' '.
*        ELSE.
*          PLANTDATA-LOT_SIZE = I_DATA-LOT_SIZE.
*        ENDIF.
*      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATA-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATA-PROFIT_CTR = I_DATA-PROFIT_CTR.
        ENDIF.
      ENDIF.

      PERFORM INPUT USING I_DATA-NO_COSTING CHANGING PLANTDATA-NO_COSTING.
      PERFORM INPUT USING I_DATA-SPECPROCTY CHANGING PLANTDATA-SPECPROCTY.
      PERFORM INPUT USING I_DATA-PUR_STATUS CHANGING PLANTDATA-PUR_STATUS.

      PERFORM INPUT1 USING I_DATA-NO_COSTING CHANGING PLANTDATAX-NO_COSTING.
      PERFORM INPUT1 USING I_DATA-SPECPROCTY CHANGING PLANTDATAX-SPECPROCTY.
      PERFORM INPUT1 USING I_DATA-PUR_STATUS CHANGING PLANTDATAX-PUR_STATUS.

    ENDIF.

**********************************************************************
* PLANTDATAX
**********************************************************************

    IF B = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.

* PLANTDATAX-LOADINGGRP = 'X'.
* PLANTDATAX-SHIP_PROC_TIME = 'X'.

      PLANTDATAX-BASE_QTY_PLAN = 'X'.
      PLANTDATAX-AVAILCHECK = 'X'.
      PLANTDATAX-BULK_MAT = 'X'.
      PLANTDATAX-DETERM_GRP = 'X'.
*      IF I_DATA-SERNO_PROF <> ' '. "序列号参数文件
*        PLANTDATAX-SERNO_PROF = 'X'.
*      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.

* IF I_DATA-STOR_CONDS <> ' '.
* CLIENTDATAX-STOR_CONDS = 'X'.
* ENDIF.
    ENDIF.
    IF C = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.

* IF I_DATA-PO_UNIT <> ' '. "订单单位
* CLIENTDATAX-PO_UNIT = 'X'.
* ENDIF.

      PLANTDATAX-DETERM_GRP = 'X'.
      PLANTDATAX-BULK_MAT = 'X'.

      IF I_DATA-PUR_GROUP <> ' '. "采购组
        PLANTDATAX-PUR_GROUP = 'X'.
      ENDIF.
      IF I_DATA-GR_PR_TIME <> ' '. "收货处理时间
        PLANTDATAX-GR_PR_TIME = 'X'.
      ENDIF.

      IF I_DATA-SOURCELIST <> ' '. "源清单
        PLANTDATAX-SOURCELIST = 'X'.
      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.

      IF I_DATA-SOURCELIST <> ' '. "源清单
        IF I_DATA-SOURCELIST = '#'.
          PLANTDATAX-SOURCELIST = ' '.
        ELSE.
          PLANTDATAX-SOURCELIST = 'X'.
        ENDIF.
      ENDIF.

*      IF I_DATA-AUTO_P_ORD <> ' '. "自动采购订单
*        IF I_DATA-AUTO_P_ORD = '#'.
*          PLANTDATAX-AUTO_P_ORD = ' '.
*        ELSE.
*          PLANTDATAX-AUTO_P_ORD = 'X'.
*        ENDIF.
*      ENDIF.

    ENDIF.
    IF D = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.

      IF I_DATA-ABC_ID <> ' '. " ABC标识
        IF I_DATA-ABC_ID = '#'.
          PLANTDATAX-ABC_ID = ' '.
        ELSE.
          PLANTDATAX-ABC_ID = 'X'.
        ENDIF.
      ENDIF.

      IF I_DATA-MRP_GROUP <> ' '. "物料需求计划类型
        PLANTDATAX-MRP_GROUP = 'X'.
      ENDIF.
      PLANTDATAX-DETERM_GRP = 'X'.
      PLANTDATAX-BULK_MAT = 'X'.
      IF I_DATA-MRP_TYPE <> ' '. "物料需求计划类型
        PLANTDATAX-MRP_TYPE = 'X'.
      ENDIF.
*      IF I_DATA-REORDER_PT <> ' '. "再订货点
*        PLANTDATAX-REORDER_PT = 'X'.
*      ENDIF.
      IF I_DATA-MRP_CTRLER <> ' '. "MRP 控制者
        PLANTDATAX-MRP_CTRLER = 'X'.
      ENDIF.
      IF I_DATA-LOTSIZEKEY <> ' '. "批量大小
        PLANTDATAX-LOTSIZEKEY = 'X'.
      ENDIF.
      IF I_DATA-MINLOTSIZE <> ' '. "最小批量大小
        PLANTDATAX-MINLOTSIZE = 'X'.
      ENDIF.
      IF I_DATA-PROC_TYPE <> ' '. "获取类型
        PLANTDATAX-PROC_TYPE = 'X'.
      ENDIF.
      IF I_DATA-SPPROCTYPE <> ' '. "特殊采购类型
        PLANTDATAX-SPPROCTYPE = 'X'.
      ENDIF.
      IF I_DATA-ISS_ST_LOC <> ' '. "发货库存地点
        PLANTDATAX-ISS_ST_LOC = 'X'.
      ENDIF.
      IF I_DATA-SLOC_EXPRC <> ' '. "外部采购的缺省仓储位置
        PLANTDATAX-SLOC_EXPRC = 'X'.
      ENDIF.
      IF I_DATA-INHSEPRODT <> ' '. "厂内生产时间
        PLANTDATAX-INHSEPRODT = 'X'.
      ENDIF.
      PLANTDATAX-DETERM_GRP = 'X'.
      PLANTDATAX-BULK_MAT = 'X'.
      IF I_DATA-PLND_DELRY <> ' '. "计划的天数内交货
        PLANTDATAX-PLND_DELRY = 'X'.
      ENDIF.
      IF I_DATA-SM_KEY <> ' '. "浮动的计划边际码
        PLANTDATAX-SM_KEY = 'X'.
      ENDIF.
      IF I_DATA-SAFETY_STK <> ' '. "安全库存
        PLANTDATAX-SAFETY_STK = 'X'.
      ENDIF.

* IF I_DATA-SUPPLY_AREA <> ' '. "缺省供应区域
* PLANTDATAX-SUPPLY_AREA = 'X'.
* ENDIF.

*      IF I_DATA-PLAN_STRGP <> ' '. "计划策略组
*        PLANTDATAX-PLAN_STRGP = 'X'.
*      ENDIF.
*      IF I_DATA-CONSUMMODE <> ' '. "消耗模式
*        PLANTDATAX-CONSUMMODE = 'X'.
*      ENDIF.
*      IF I_DATA-FWD_CONS <> ' '. "向前消耗期间
*        PLANTDATAX-FWD_CONS = 'X'.
*      ENDIF.
*      IF I_DATA-BWD_CONS <> ' '. "消耗期间:逆向
*        PLANTDATAX-BWD_CONS = 'X'.
*      ENDIF.
      PLANTDATAX-AVAILCHECK = 'X'.
      IF I_DATA-DEP_REQ_ID <> ' '. ""对于独立和集中需求的相关需求标识\
        PLANTDATAX-DEP_REQ_ID = 'X'.
      ENDIF.
      IF I_DATA-MAXLOTSIZE <> ' '. "最大批量大小
        PLANTDATAX-MAXLOTSIZE = 'X'.
      ENDIF.
      IF I_DATA-BACKFLUSH <> ' '. "反冲
        PLANTDATAX-BACKFLUSH = 'X'.
      ENDIF.
*      IF I_DATA-FIXED_LOT <> ' '.
*        PLANTDATAX-FIXED_LOT = 'X'.
*      ENDIF.

*} INSERT
      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.

      IF I_DATA-PUR_GROUP <> ' '. "MRP组
        IF I_DATA-PUR_GROUP = '#'.
          PLANTDATAX-PUR_GROUP = ' '.
        ELSE.
          PLANTDATAX-PUR_GROUP = 'X'.
        ENDIF.
      ENDIF.

    ENDIF.

    IF E = 'X'.
      PLANTDATAX-DETERM_GRP = 'X'.
      PLANTDATAX-BULK_MAT = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.
*      IF I_DATA-PRODUCTION_SCHEDULER <> ' '.
*        PLANTDATAX-PRODUCTION_SCHEDULER = 'X'.
*      ENDIF.
*      IF I_DATA-SERNO_PROF <> ' '.
*        PLANTDATAX-SERNO_PROF = 'X'.
*      ENDIF.


      "1010工厂默认生产计划参数文件
*      IF I_DATA-PLANT = '1010' AND ( I_DATA-PROC_TYPE = 'E' OR I_DATA-PROC_TYPE = 'X'
*      ).
*        PLANTDATAX-PRODPROF = 'X'.
*      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.

*      IF I_DATA-PRODPROF <> ' '.
*        IF I_DATA-PRODPROF = '#'.
*          PLANTDATAX-PRODPROF = ' '.
*        ELSE.
*          PLANTDATAX-PRODPROF = 'X'.
*        ENDIF.
*      ENDIF.

    ENDIF.

    IF F = 'X'.
      STORAGELOCATIONDATAX-PLANT = I_DATA-PLANT.
      IF I_DATA-STGE_LOC <> ' '.
        STORAGELOCATIONDATAX-STGE_LOC = I_DATA-STGE_LOC.
      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF G = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF H = 'X'.
      PLANTDATAX-PLANT = I_DATA-PLANT.

* IF I_DATA-NO_COSTING <> ' '.
* PLANTDATAX-NO_COSTING = 'X'.
* ENDIF.

*      IF I_DATA-LOT_SIZE <> ' '.
*        PLANTDATAX-LOT_SIZE = 'X'.
*      ENDIF.

      IF I_DATA-PROFIT_CTR <> ' '.
        IF I_DATA-PROFIT_CTR = '#'.
          PLANTDATAX-PROFIT_CTR = ' '.
        ELSE.
          PLANTDATAX-PROFIT_CTR = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

************************************************************************
* SALESDATA
************************************************************************

    IF B = 'X'.
      SALESDATA-SALES_ORG = I_DATA-SALES_ORG. "销售组织
      SALESDATA-DISTR_CHAN = I_DATA-DISTR_CHAN. "分销渠道
      SALESDATAX-SALES_ORG = I_DATA-SALES_ORG.
      SALESDATAX-DISTR_CHAN = I_DATA-DISTR_CHAN.

      IF I_DATA-ITEM_CAT <> ' '. "项目类别组
        IF I_DATA-ITEM_CAT = '#'.
          SALESDATA-ITEM_CAT = ' '.
        ELSE.
          SALESDATA-ITEM_CAT = I_DATA-ITEM_CAT.
        ENDIF.
      ENDIF.
      IF I_DATA-ACCT_ASSGT <> ' '. "科目设置组
        IF I_DATA-ACCT_ASSGT = '#'.
          SALESDATA-ACCT_ASSGT = ' '.
        ELSE.
          SALESDATA-ACCT_ASSGT = I_DATA-ACCT_ASSGT.
        ENDIF.
      ENDIF.

      IF I_DATA-DELYG_PLNT <> ' '. "交货工厂
        IF I_DATA-DELYG_PLNT = '#'.
          SALESDATA-DELYG_PLNT = ' '.
        ELSE.
          SALESDATA-DELYG_PLNT = I_DATA-DELYG_PLNT.
        ENDIF.
      ENDIF.

      IF I_DATA-MATL_GRP_1  <> ' '. "物料组1
        IF I_DATA-MATL_GRP_1 = '#'.
          SALESDATA-MATL_GRP_1 = ' '.
        ELSE.
          SALESDATA-MATL_GRP_1 = I_DATA-MATL_GRP_1.
        ENDIF.
      ENDIF.

      IF I_DATA-MATL_GRP_2  <> ' '. "物料组2
        IF I_DATA-MATL_GRP_2 = '#'.
          SALESDATA-MATL_GRP_2 = ' '.
        ELSE.
          SALESDATA-MATL_GRP_2 = I_DATA-MATL_GRP_2.
        ENDIF.
      ENDIF.

      IF I_DATA-MATL_GRP_3  <> ' '. "物料组3
        IF I_DATA-MATL_GRP_3 = '#'.
          SALESDATA-MATL_GRP_3 = ' '.
        ELSE.
          SALESDATA-MATL_GRP_3 = I_DATA-MATL_GRP_3.
        ENDIF.
      ENDIF.

      IF I_DATA-MATL_GRP_4  <> ' '. "物料组4
        IF I_DATA-MATL_GRP_4 = '#'.
          SALESDATA-MATL_GRP_4 = ' '.
        ELSE.
          SALESDATA-MATL_GRP_4 = I_DATA-MATL_GRP_4.
        ENDIF.
      ENDIF.

      IF I_DATA-MATL_GRP_5  <> ' '. "物料组5
        IF I_DATA-MATL_GRP_5 = '#'.
          SALESDATA-MATL_GRP_5 = ' '.
        ELSE.
          SALESDATA-MATL_GRP_5 = I_DATA-MATL_GRP_5.
        ENDIF.
      ENDIF.

      IF I_DATA-DELYG_PLNT <> ' '. "交货工厂
        SALESDATAX-DELYG_PLNT = 'X'. "交货工厂
      ENDIF.
      IF I_DATA-ITEM_CAT <> ' '. "项目类别组
        SALESDATAX-ITEM_CAT = 'X'.
      ENDIF.

      IF I_DATA-MATL_GRP_1 <> ' '. "物料组1
        SALESDATAX-MATL_GRP_1 = 'X'.
      ENDIF.
      IF I_DATA-MATL_GRP_2 <> ' '. "物料组2
        SALESDATAX-MATL_GRP_2 = 'X'.
      ENDIF.
      IF I_DATA-MATL_GRP_3 <> ' '. "物料组3
        SALESDATAX-MATL_GRP_3 = 'X'.
      ENDIF.
      IF I_DATA-MATL_GRP_4 <> ' '. "物料组4
        SALESDATAX-MATL_GRP_4 = 'X'.
      ENDIF.


      IF I_DATA-MATL_GRP_5 <> ' '. "物料组5
        SALESDATAX-MATL_GRP_5 = 'X'.
      ENDIF.

      IF I_DATA-ACCT_ASSGT <> ' '. "科目设置组
        SALESDATAX-ACCT_ASSGT = 'X'.
      ENDIF.

************************************************************************
* TAXCLASSIFICATIONS
************************************************************************
      CLEAR TAXCLASSIFICATIONS.
      REFRESH TAXCLASSIFICATIONS.
      TAXCLASSIFICATIONS-DEPCOUNTRY = 'CN'.
      TAXCLASSIFICATIONS-TAX_TYPE_2 = 'MWST'.
      TAXCLASSIFICATIONS-TAXCLASS_2 = I_DATA-TAXCLASS_1.
      APPEND TAXCLASSIFICATIONS.

    ENDIF.

************************************************************************
* VALUATIONDATA
************************************************************************

    IF H = 'X'.
      VALUATIONDATA-VAL_AREA = I_DATA-PLANT. "工厂固定写
      VALUATIONDATAX-VAL_AREA = I_DATA-PLANT."工厂固定写

      PERFORM INPUT USING I_DATA-VAL_CLASS CHANGING VALUATIONDATA-VAL_CLASS.
      PERFORM INPUT USING I_DATA-PRICE_CTRL CHANGING VALUATIONDATA-PRICE_CTRL.
      PERFORM INPUT USING I_DATA-MOVING_PR CHANGING VALUATIONDATA-MOVING_PR.
      PERFORM INPUT USING I_DATA-STD_PRICE CHANGING VALUATIONDATA-STD_PRICE.
      PERFORM INPUT USING I_DATA-PRICE_UNIT CHANGING VALUATIONDATA-PRICE_UNIT.
      PERFORM INPUT USING I_DATA-QTY_STRUCT CHANGING VALUATIONDATA-QTY_STRUCT.
      PERFORM INPUT USING I_DATA-ORIG_MAT CHANGING VALUATIONDATA-ORIG_MAT.
      PERFORM INPUT USING I_DATA-OVERHEAD_GRP CHANGING VALUATIONDATA-OVERHEAD_GRP.
      PERFORM INPUT USING I_DATA-PLNDPRICE1 CHANGING VALUATIONDATA-PLNDPRICE1.
      PERFORM INPUT USING I_DATA-PLNDPRDATE1 CHANGING VALUATIONDATA-PLNDPRDATE1.

      PERFORM INPUT1 USING I_DATA-VAL_CLASS CHANGING VALUATIONDATAX-VAL_CLASS.
      PERFORM INPUT1 USING I_DATA-PRICE_CTRL CHANGING VALUATIONDATAX-PRICE_CTRL.
      PERFORM INPUT1 USING I_DATA-MOVING_PR CHANGING VALUATIONDATAX-MOVING_PR.
      PERFORM INPUT1 USING I_DATA-STD_PRICE CHANGING VALUATIONDATAX-STD_PRICE.
      PERFORM INPUT1 USING I_DATA-PRICE_UNIT CHANGING VALUATIONDATAX-PRICE_UNIT.
      PERFORM INPUT1 USING I_DATA-QTY_STRUCT CHANGING VALUATIONDATAX-QTY_STRUCT.
      PERFORM INPUT1 USING I_DATA-ORIG_MAT CHANGING VALUATIONDATAX-ORIG_MAT.
      PERFORM INPUT1 USING I_DATA-OVERHEAD_GRP CHANGING VALUATIONDATAX-OVERHEAD_GRP.
      PERFORM INPUT1 USING I_DATA-PLNDPRICE1 CHANGING VALUATIONDATAX-PLNDPRICE1.
      PERFORM INPUT1 USING I_DATA-PLNDPRDATE1 CHANGING VALUATIONDATAX-PLNDPRDATE1.

    ENDIF.



    IF I = 'X'.

      STORAGELOCATIONDATA-PLANT = I_DATA-PLANT.
      STORAGELOCATIONDATAX-PLANT = I_DATA-PLANT.

      PERFORM INPUT USING I_DATA-STGE_LOC CHANGING STORAGELOCATIONDATA-STGE_LOC.
      PERFORM INPUT USING I_DATA-STGE_BIN CHANGING STORAGELOCATIONDATA-STGE_BIN.
      PERFORM INPUT USING I_DATA-MINREMLIFE CHANGING CLIENTDATA-MINREMLIFE.
      PERFORM INPUT USING I_DATA-STGE_PD_UN CHANGING PLANTDATA-STGE_PD_UN.

      PERFORM INPUT  USING I_DATA-STGE_LOC CHANGING STORAGELOCATIONDATAX-STGE_LOC.
      PERFORM INPUT1 USING I_DATA-STGE_BIN CHANGING STORAGELOCATIONDATAX-STGE_BIN.
      PERFORM INPUT1 USING I_DATA-MINREMLIFE CHANGING CLIENTDATAX-MINREMLIFE.
      PERFORM INPUT1 USING I_DATA-STGE_PD_UN CHANGING PLANTDATAX-STGE_PD_UN.

    ENDIF.

    DATA:RETURNMESSAGES LIKE TABLE OF BAPI_MATRETURN2.
    DATA FLAG2 TYPE CHAR1.
    CLEAR FLAG2.

    IF SY-UNAME <> 'HANDLY'
      AND SY-UNAME <> 'HANDYMR'.

      READ TABLE TMARA WITH KEY MATNR = I_DATA-MATERIAL.
      IF SY-SUBRC = 0.

        IF A = 'X'.
          I_DATA-MESSAGE = RETURN-MESSAGE.
          MODIFY I_DATA.
          I_DATA-MESSAGE = '该物料已经导入基本视图，不允许再修改'.
          APPEND I_DATA TO T_DATA.
          LOG_RETURN-MATNR = I_DATA-MATERIAL."日志增加对应得物料号
          LOG_RETURN-MESSAGE = I_DATA-MESSAGE."日志增加消息文本
          APPEND LOG_RETURN.
          FLAG2 = 'X'.
        ENDIF.

      ENDIF.

    ENDIF.

    IF FLAG2 = ''.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA             = HEADDATA
          CLIENTDATA           = CLIENTDATA
          CLIENTDATAX          = CLIENTDATAX
          PLANTDATA            = PLANTDATA
          PLANTDATAX           = PLANTDATAX
          PLANNINGDATA         = PLANNINGDATA
          PLANNINGDATAX        = PLANNINGDATAX
          SALESDATA            = SALESDATA
          SALESDATAX           = SALESDATAX
          WAREHOUSENUMBERDATA  = WAREHOUSENUMBERDATA
          WAREHOUSENUMBERDATAX = WAREHOUSENUMBERDATAX
          STORAGELOCATIONDATA  = STORAGELOCATIONDATA
          STORAGELOCATIONDATAX = STORAGELOCATIONDATAX
          VALUATIONDATA        = VALUATIONDATA
          VALUATIONDATAX       = VALUATIONDATAX
        IMPORTING
          RETURN               = RETURN
        TABLES
          MATERIALDESCRIPTION  = MATERIALDESCRIPTION
          UNITSOFMEASURE       = UNITSOFMEASURE
          UNITSOFMEASUREX      = UNITSOFMEASUREX
**         INTERNATIONALARTNOS   =
          MATERIALLONGTEXT     = MATERIALLONGTEXT
          TAXCLASSIFICATIONS   = TAXCLASSIFICATIONS
          RETURNMESSAGES       = RETURNMESSAGES
*         PRTDATA              =
*         PRTDATAX             =
*         EXTENSIONIN          =
*         EXTENSIONINX         =
*         NFMCHARGEWEIGHTS     =
*         NFMCHARGEWEIGHTSX    =
*         NFMSTRUCTURALWEIGHTS =
*         NFMSTRUCTURALWEIGHTSX =
        .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*       EXPORTING
*         WAIT          =
*       IMPORTING
*         RETURN        =
        .

      WAIT UP TO SEC SECONDS.

**********************************************************************

      IF RETURN-TYPE = 'E'.
        I_DATA-MESSAGE = RETURN-MESSAGE.
        MODIFY I_DATA.
        APPEND I_DATA TO T_DATA.
        T_DATA-MESSAGE = RETURN-MESSAGE.

      ENDIF.

**********************************************************************

      LOG_RETURN-MATNR = I_DATA-MATERIAL."日志增加对应得物料号
      LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
      APPEND LOG_RETURN.

    ENDIF.

    CLEAR HEADDATA.
    CLEAR CLIENTDATA.
    CLEAR CLIENTDATAX.
    CLEAR PLANTDATA.
    CLEAR PLANTDATAX.
    CLEAR SALESDATA.
    CLEAR SALESDATAX.
    CLEAR WAREHOUSENUMBERDATA.
    CLEAR WAREHOUSENUMBERDATAX.
    CLEAR STORAGELOCATIONDATA.
    CLEAR STORAGELOCATIONDATAX.
    CLEAR VALUATIONDATA.
    CLEAR VALUATIONDATAX.


    CLEAR MATERIALDESCRIPTION.
    REFRESH MATERIALDESCRIPTION.
    CLEAR TAXCLASSIFICATIONS.
    REFRESH TAXCLASSIFICATIONS.
    CLEAR MATERIALLONGTEXT.
    REFRESH MATERIALLONGTEXT.
    CLEAR UNITSOFMEASURE.
    REFRESH UNITSOFMEASURE.


    CLEAR I_DATA.
    CLEAR LOG_RETURN.
    CLEAR RETURN.
  ENDLOOP.
*  REFRESH T_MARA.

  LOOP AT LOG_RETURN.
    WRITE:/ LOG_RETURN-MATNR,LOG_RETURN-MESSAGE.
  ENDLOOP.
ENDFORM. " SAVE_MATERIAL_DATA

*&---------------------------------------------------------------------*
*& Form SELECT_PATH
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*

FORM SELECT_PATH .
  DATA V_MATERIAL LIKE RLGRAP-FILENAME.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.* ,*.*.'
      MODE             = '0'
      TITLE            = '请选择要上传的信息文件'
    IMPORTING
      FILENAME         = V_MATERIAL
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
  MATERIAL = V_MATERIAL.
ENDFORM. " SELECT_PATH

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*

FORM BUILD_FIELDCATALOG.

  PERFORM FIELD_CAT USING 'MESSAGE' '错误消息' ''.
  PERFORM FIELD_CAT USING 'IND_SECTOR' '行业领域' ''.
  PERFORM FIELD_CAT USING 'MATL_TYPE' '物料类型' ''.
  PERFORM FIELD_CAT USING 'MATERIAL' '物料号' ''.
  PERFORM FIELD_CAT USING 'MATL_DESC_ZH' '物料描述' ''.
  PERFORM FIELD_CAT USING 'OLD_MAT_NO' '旧物料号' ''.
  PERFORM FIELD_CAT USING 'MATL_GROUP' '物料组' ''.
  PERFORM FIELD_CAT USING 'BASE_UOM' '基本计量单位' ''.
  PERFORM FIELD_CAT USING 'NET_WEIGHT' '净重' ''.
  PERFORM FIELD_CAT USING 'UNIT_OF_WT' '重量单位' ''.
  PERFORM FIELD_CAT USING 'EXTMATLGRP' '外部物料组' ''.
  PERFORM FIELD_CAT USING 'DOCUMENT' '图纸号' ''.
  PERFORM FIELD_CAT USING 'PROD_MEMO' '备忘录' ''.
  PERFORM FIELD_CAT USING 'STD_DESCR' '行业标准' ''.
  PERFORM FIELD_CAT USING 'LONGTEXT' '物料长描述' ''.
  PERFORM FIELD_CAT USING 'ALT_UNIT' '替换单位' ''.
  PERFORM FIELD_CAT USING 'NUMERATOR' '分子' ''.
  PERFORM FIELD_CAT USING 'DENOMINATR' '分母' ''.
  PERFORM FIELD_CAT USING 'PLANT' '工厂' ''.
  PERFORM FIELD_CAT USING 'SALES_ORG' '销售组织' ''.
  PERFORM FIELD_CAT USING 'DISTR_CHAN' '分销渠道' ''.
  PERFORM FIELD_CAT USING 'DIVISION' '产品组' ''.
  PERFORM FIELD_CAT USING 'DELYG_PLNT' '交货工厂' ''.
  PERFORM FIELD_CAT USING 'TAXCLASS_1' '税分类' ''.
  PERFORM FIELD_CAT USING 'MATL_STATS' '物料统计组' ''.
  PERFORM FIELD_CAT USING 'ACCT_ASSGT' '科目分配类别' ''.
  PERFORM FIELD_CAT USING 'PR_REF_MAT' '定价参考物料' ''.
  PERFORM FIELD_CAT USING 'ITEM_CAT1' '普通项目类别（MARA）' ''.
  PERFORM FIELD_CAT USING 'ITEM_CAT' '项目类别组(MVKE)' ''.
  PERFORM FIELD_CAT USING 'MATL_GRP_1' '产品系列' ''.
  PERFORM FIELD_CAT USING 'MATL_GRP_2' '研制单位' ''.
  PERFORM FIELD_CAT USING 'MATL_GRP_3' '经营责任收入主体' ''.
  PERFORM FIELD_CAT USING 'MATL_GRP_4' '经营责任新签主体' ''.
  PERFORM FIELD_CAT USING 'MATL_GRP_5' '生产履约主体' ''.
  PERFORM FIELD_CAT USING 'TRANS_GRP' '运输组（MARA）' ''.
  PERFORM FIELD_CAT USING 'LOADINGGRP' '装载组(MARC)' ''.
  PERFORM FIELD_CAT USING 'YZDW' '研制单位' ''.
  PERFORM FIELD_CAT USING 'PUR_GROUP' '采购组' ''.
  PERFORM FIELD_CAT USING 'GR_PR_TIME' '收货处理时间' ''.
  PERFORM FIELD_CAT USING 'PO_UNIT' ' 订单单位（MARA）' ''.
  PERFORM FIELD_CAT USING 'SOURCELIST' ' 源清单（MARC）' ''.
  PERFORM FIELD_CAT USING 'XCHPF' '批次管理' ''.
  PERFORM FIELD_CAT USING 'ABC_ID' ' ABC标识（MARC）' ''.
  PERFORM FIELD_CAT USING 'MRP_GROUP' 'MRP组' ''.
  PERFORM FIELD_CAT USING 'MRP_TYPE' 'MRP类型' ''.
  PERFORM FIELD_CAT USING 'MRP_CTRLER' 'MRP 控制者' ''.
  PERFORM FIELD_CAT USING 'LOTSIZEKEY' '批量大小' ''.
  PERFORM FIELD_CAT USING 'MINLOTSIZE' '最小批量大小(生产)' ''.
  PERFORM FIELD_CAT USING 'MAXLOTSIZE' '最大批量大小（生产）' ''.
  PERFORM FIELD_CAT USING 'ASSY_SCRAP' ' 装配报废(MARC)' ''.
  PERFORM FIELD_CAT USING 'ROUND_VAL' ' 舍入值(MARC)' ''.
  PERFORM FIELD_CAT USING 'PROC_TYPE' '采购类型' ''.
  PERFORM FIELD_CAT USING 'SPPROCTYPE' '特殊采购类型' ''.
  PERFORM FIELD_CAT USING 'ISS_ST_LOC' '生产仓储地点' ''.
  PERFORM FIELD_CAT USING 'SLOC_EXPRC' '外部采购仓储地点' ''.
  PERFORM FIELD_CAT USING 'BACKFLUSH' '反冲' ''.
  PERFORM FIELD_CAT USING 'INHSEPRODT' '自制生产时间' ''.
  PERFORM FIELD_CAT USING 'PLND_DELRY' '计划交货时间' ''.
  PERFORM FIELD_CAT USING 'SM_KEY' '计划边际码' ''.
  PERFORM FIELD_CAT USING 'SAFETY_STK' '安全库存' ''.
  PERFORM FIELD_CAT USING 'AVAILCHECK' ' 可用性检查(MARC)' ''.
  PERFORM FIELD_CAT USING 'PLNG_MATL' '计划物料' ''.
  PERFORM FIELD_CAT USING 'PLNG_PLANT' '计划工厂' ''.
  PERFORM FIELD_CAT USING 'CONVFACTOR' '计划物料转换因子' ''.

  PERFORM FIELD_CAT USING 'DEP_REQ_ID' '独立/集中' ''.
  PERFORM FIELD_CAT USING 'COMP_SCRAP' '部件废品' ''.
  PERFORM FIELD_CAT USING 'ISSUE_UNIT' '发货单位' ''.

  PERFORM FIELD_CAT USING 'STGE_LOC' '库存地点(MARD)' ''.
  PERFORM FIELD_CAT USING 'STGE_BIN' '仓位' ''.
  PERFORM FIELD_CAT USING 'MINREMLIFE' '最小剩余货架寿命（MARA）' ''.
  PERFORM FIELD_CAT USING 'STGE_PD_UN' '货架寿命期间标识(MARC)' ''.
  PERFORM FIELD_CAT USING 'HANDLG_GRP' '采购统计维度(MARC)' ''.
  PERFORM FIELD_CAT USING 'VAL_CLASS' '评估类' ''.
  PERFORM FIELD_CAT USING 'PRICE_CTRL' '价格控制指示符' ''.
  PERFORM FIELD_CAT USING 'STD_PRICE' '标准价格' ''.
  PERFORM FIELD_CAT USING 'MOVING_PR' '移动平均价' ''.
  PERFORM FIELD_CAT USING 'PRICE_UNIT' '价格单位' ''.
  PERFORM FIELD_CAT USING 'PROFIT_CTR' '利润中心' ''.
  PERFORM FIELD_CAT USING 'LOT_SIZE' '批量产品成本核算' ''.
  PERFORM FIELD_CAT USING 'NO_COSTING' '无成本核算（MARC）' ''.
  PERFORM FIELD_CAT USING 'QTY_STRUCT' '用QS的成本估算(MBEW)' ''.
  PERFORM FIELD_CAT USING 'ORIG_MAT' '物料相关的源' ''.
  PERFORM FIELD_CAT USING 'SPECPROCTY' '特殊采购成本核算（MARC）' ''.
  PERFORM FIELD_CAT USING 'PUR_STATUS' '工厂特定的物料状态（MARC）' ''.
  PERFORM FIELD_CAT USING 'OVERHEAD_GRP' '间接费用组' ''.
  PERFORM FIELD_CAT USING 'PLNDPRICE1' '计划价格1' ''.
  PERFORM FIELD_CAT USING 'PLNDPRDATE1' '计划价格1日期' ''.


ENDFORM. "BUILD_FIELDCATALOG

*&---------------------------------------------------------------------*
*& Form BUILD_LAYOUT
*&---------------------------------------------------------------------*
* Build layout for ALV grid report
*----------------------------------------------------------------------*

FORM BUILD_LAYOUT.

* GD_LAYOUT-NO_INPUT = ''.<

  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GD_LAYOUT-ZEBRA = 'X'.

* GD_LAYOUT-GROUP_CHANGE_EDIT = 'X'.

  GD_LAYOUT-HEADER_TEXT = '导入结果查询'.
ENDFORM. " BUILD_LAYOUT


*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
* Display report using ALV grid
*----------------------------------------------------------------------*

FORM DISPLAY_ALV_REPORT.
  GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = GD_REPID
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_grid_title            = outtext
      IS_LAYOUT               = GD_LAYOUT
      IT_FIELDCAT             = FIELDCATALOG[]
*     it_special_groups       = gd_tabgroup
*     IT_EVENTS               = GT_XEVENTS
      I_SAVE                  = 'X'
*     is_variant              = z_template
    TABLES
      T_OUTTAB                = T_DATA
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM. " DISPLAY_ALV_REPORT

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* -->UCOMM text
* -->SELFIELD text
*----------------------------------------------------------------------*

FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM
SELFIELD TYPE SLIS_SELFIELD.

  "ELFIELD包含了你所双击单元格的内容/字段名等信息
  SELFIELD-REFRESH = 'X'.
  REFRESH I_DATA.
  CLEAR I_DATA.
  READ TABLE T_DATA INTO I_DATA INDEX SELFIELD-TABINDEX.
  APPEND I_DATA.
  IF SELFIELD-TABINDEX = 0."全部选中删除所有行
    REFRESH T_DATA.
    CLEAR T_DATA.
  ELSE.
    DELETE T_DATA INDEX SELFIELD-TABINDEX. "权删除选中行
  ENDIF.
  PERFORM SAVE_MATERIAL_DATA.
ENDFORM. "USER_COMMAND

FORM INPUT USING PIN CHANGING POUT.

  IF PIN <> ' '.
    IF PIN = '#'.
      POUT = ' '.
    ELSE.
      POUT = PIN.
    ENDIF.
  ENDIF.

ENDFORM.

FORM INPUT1 USING PIN CHANGING POUT.
* 针对更新标识的勾选
  IF PIN IS NOT INITIAL.
    POUT = 'X'.
  ELSE.
    CLEAR POUT.
  ENDIF.

ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FRM_KZ_LGORT
**&---------------------------------------------------------------------*
** text
**----------------------------------------------------------------------*
** --> p1 text
** <-- p2 text
**----------------------------------------------------------------------*
*
*FORM FRM_KZ_LGORT .
*  IF S_LGORT[] IS NOT INITIAL.
*    DATA:LT_DATA LIKE I_DATA OCCURS 0.
*
*    SELECT WERKS
*    LGORT
*    FROM T001L
*    INTO CORRESPONDING FIELDS OF TABLE IT_T001L
*    WHERE LGORT IN S_LGORT.
*
*    IF IT_T001L[] IS NOT INITIAL.
*      LOOP AT I_DATA.
*        LOOP AT IT_T001L WHERE WERKS = I_DATA-PLANT.
*          I_DATA-STGE_LOC = IT_T001L-LGORT.
*          APPEND I_DATA TO LT_DATA.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*
*    APPEND LINES OF LT_DATA TO I_DATA.
*
*    DELETE ADJACENT DUPLICATES FROM I_DATA COMPARING MATERIAL STGE_LOC.
*  ENDIF.
*ENDFORM. " FRM_KZ_LGORT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3775   text
*      -->P_3776   text
*      -->P_3777   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    PNAME
                         PTEXT
                         PKEY.

  CLEAR FIELDCATALOG.
  FIELDCATALOG-FIELDNAME = PNAME.
  FIELDCATALOG-SELTEXT_M = PTEXT.
  FIELDCATALOG-KEY       = PKEY.
  APPEND FIELDCATALOG TO FIELDCATALOG.

ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  INPUT_HEADDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INPUT_HEADDATA .
  HEADDATA-MATERIAL = I_DATA-MATERIAL.      "物料
  HEADDATA-IND_SECTOR = I_DATA-IND_SECTOR.  "行业领域
  HEADDATA-MATL_TYPE = I_DATA-MATL_TYPE.    "物料类型

  IF A = 'X'.
    HEADDATA-BASIC_VIEW = 'X'.
  ENDIF.
  IF B = 'X'.
    HEADDATA-SALES_VIEW = 'X'.
  ENDIF.
  IF C = 'X'.
    HEADDATA-PURCHASE_VIEW = 'X'.
  ENDIF.
  IF D = 'X'.
    HEADDATA-MRP_VIEW = 'X'.
  ENDIF.
  IF E = 'X'.
    HEADDATA-WORK_SCHED_VIEW = 'X'.
  ENDIF.
  IF F = 'X'.
    HEADDATA-STORAGE_VIEW = 'X'.
  ENDIF.
  IF G = 'X'.
    HEADDATA-QUALITY_VIEW = 'X'.
  ENDIF.
  IF H = 'X'.
    HEADDATA-COST_VIEW = 'X'.
    HEADDATA-ACCOUNT_VIEW = 'X'.
  ENDIF.
ENDFORM.                    " INPUT_HEADDATA
