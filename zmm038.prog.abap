REPORT ZMM038.

*& DATE CREATED : 2015/2/6                                             *
*& CREATED BY   : 汉得-远程                                         *
*& DESCRIPTION  :励丰采购订单打印                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&DATE            DEVELOPER           REQNO       DESCRIPTIONS            *
*& ==========    ==================  ==========  ========================*
*& 2017-0815-22  IT02&魏云           ED1K906857  励丰采购合同打印模板调整
*& 2017-0825-22  IT02&魏云           ED1K906969  励丰采购合同打印模板优化计划交货日期


*---------------------------------------------------------------------*
* TYPE-POOL                                                           *
*                                                                     *
*---------------------------------------------------------------------*
TYPE-POOLS:
SLIS,VRM.                       " GLOBALE TYPE

*---------------------------------------------------------------------*
* DECLARE TABLES                                                      *
*                                                                     *
*---------------------------------------------------------------------*
TABLES: EKKO,EKPO,LFA1,KONV,MARA,EKKN,PRPS,T001,T052,KNVK,BNKA,LFBK,PROJ,EKET.

*---------------------------------------------------------------------*
* TYPES                                                               *
*                                                                     *
*---------------------------------------------------------------------*
TYPES:BEGIN OF TY_TAB,
        BOX        TYPE C,              "box
        EBELN      TYPE EKKO-EBELN,     "采购订单
        EBELP(6)   TYPE N,              "行项目号
        KNTTP      TYPE EKPO-KNTTP,     "科目分配类别
        PSTYP      TYPE EKPO-PSTYP,     "行项目类别
        EKGRP      TYPE EKKO-EKGRP,     "采购组
        LIFNR      TYPE EKKO-LIFNR,     "供应商
        NAME1      TYPE LFA1-NAME1,     "供应商名称
        MATNR      TYPE EKPO-MATNR,     "物料号
        TXZ01      TYPE EKPO-TXZ01,     "物料描述
        EXTWG      TYPE MARA-EXTWG,     "品牌
        MEINS      TYPE EKPO-MEINS,     "单位
        MEINS_1    TYPE C LENGTH 10,    "单位转换
        MENGE      TYPE EKPO-MENGE,     "数量
*----------------add by handyhl---2017.07.27--------------------------------------------
        KBETR      TYPE KONV-KBETR,     "含税价
        ZNETPR     TYPE  ZNETPR,  "含税价
*        BPRME      TYPE EKPO-BPRME,     "货币
        WAERS      TYPE KONV-WAERS,  "货币
*----------------add by handyhl---2017.07.27--------------------------------------------
        MWSKZ      TYPE EKPO-MWSKZ,     "税码
        EINDT      TYPE EKET-EINDT,    "计划交货日期
        WERKS      TYPE EKPO-WERKS,     "工厂
        LGORT      TYPE EKPO-LGORT,     "库存地点
        PS_PSP_PNR TYPE EKKN-PS_PSP_PNR, "项目编码
        POST1      TYPE PRPS-POST1,     "项目名称
        KOSTL      TYPE EKKN-KOSTL,     "成本中心
        AUFNR      TYPE EKKN-AUFNR,     "内部订单
        ANLN1      TYPE EKKN-ANLN1,     "固定的资产号
        BEDNR      TYPE EKPO-BEDNR,     "申请部门
        AFNAM      TYPE EKPO-AFNAM,     "申请人
        ERNAM      TYPE EKKO-ERNAM,     "创建者
        AEDAT      TYPE EKKO-AEDAT,     "创建日期
        BUTXT      TYPE T001-BUTXT,     "甲方
        STRAS      TYPE LFA1-STRAS,     "住宅号
        NAME2      TYPE KNVK-NAME1,     "名称
        TELF1      TYPE KNVK-TELF1,     "电话
        KOINH      TYPE LFBK-KOINH,     "银行帐户号码
        BANKA      TYPE BNKA-BANKA,     "银行名称
        KNUMV      TYPE KONV-KNUMV,     "单据条件数
        PSPHI      TYPE PRPS-PSPHI,     "适的项目的当前编号
        STCEG      TYPE LFA1-STCEG,     "增值税登记号
        ZBNX       TYPE EKKO-ZBNX,      "乙方免费质保年限
        TEXT1      TYPE T052U-TEXT1,    "收付条款的自解释
        YFCDF      TYPE EKKO-YFCDF,
        TELF2      TYPE LFA1-TELF1,     "乙方传真
        TELFX      TYPE LFA1-TELFX,     "乙方传真
        EKNAM      TYPE T024-EKNAM,
        MENGE_1    TYPE EKPO-MENGE,     "数量
     "   WAERS      TYPE KONV-WAERS,     "货币码
      END OF TY_TAB.

TYPES:BEGIN OF TY_ZTXT,
        ZTXT TYPE ZMM038-ZTXT,        "地址（弹出窗口页面的地址）
      END OF TY_ZTXT.

TYPES:BEGIN OF TY_EKKO_EKPO,
        ZTERM           TYPE EKKO-ZTERM,     "付款条件
        BUKRS           TYPE EKKO-BUKRS,     "公司代码
        KNUMV           TYPE EKKO-KNUMV,     "单据条件数
        EBELN           TYPE EKKO-EBELN,     "采购订单
        EKGRP           TYPE EKKO-EKGRP,     "采购组
        LIFNR           TYPE EKKO-LIFNR,     "供应商
        ERNAM           TYPE EKKO-ERNAM,     "创建者
        AEDAT           TYPE EKKO-AEDAT,     "创建日期
        EBELP(6)        TYPE N,              "行项目号
        KNTTP           TYPE EKPO-KNTTP,     "科目分配类别
        PSTYP           TYPE EKPO-PSTYP,     "行项目类别
        MATNR           TYPE EKPO-MATNR,     "物料号
        TXZ01           TYPE EKPO-TXZ01,     "物料描述
        MEINS           TYPE EKPO-MEINS,     "单位
        MENGE           TYPE EKPO-MENGE,     "数量
*----------------add by handyhl---2017.07.27-------begin-------------------------------------
*        PEINH          TYPE ekpo-PEINH,  "数量
*        netpr          TYPE ekpo-netpr,
*----------------add by handyhl---2017.07.27-------end-------------------------------------
*----------------add by handyhl---2017.07.28-------begin-------------------------------------
*       ebelp           TYPE ekko-ebelp,
        loekz          TYPE ekpo-loekz, "删除标识
*----------------add by handyhl---2017.07.28-------end-------------------------------------

*        BPRME           TYPE EKPO-BPRME,     "货币
        MWSKZ           TYPE EKPO-MWSKZ,     "税码
        WERKS           TYPE EKPO-WERKS,     "工厂
        LGORT           TYPE EKPO-LGORT,     "库存地点
        BEDNR           TYPE EKPO-BEDNR,     "申请部门
        AFNAM           TYPE EKPO-AFNAM,     "申请人
        YFCDF           TYPE EKKO-YFCDF,
        ZBNX            TYPE EKKO-ZBNX,      "乙方免费质保年限
*--------------EKKN内表-------------
        ZEKKN           TYPE EKKN-ZEKKN,      "帐户分配的顺序编号
        PS_PSP_PNR      TYPE EKKN-PS_PSP_PNR, "项目编码
        KOSTL           TYPE EKKN-KOSTL,      "成本中心
        AUFNR           TYPE EKKN-AUFNR,      "内部订单
        ANLN1           TYPE EKKN-ANLN1,      "固定的资产号
        PS_PSP_PNR1(24) TYPE C,
      END OF TY_EKKO_EKPO.

TYPES:BEGIN OF TY_EKKO_EKPO1,
        EBELN TYPE EKKO-EBELN,     "采购订单
        EBELP TYPE EKPO-EBELP,     "行项目号
      END OF TY_EKKO_EKPO1.

TYPES:BEGIN OF TY_LFA1,
        LIFNR TYPE LFA1-LIFNR,     "供应商或债权人的帐号
        STRAS TYPE LFA1-STRAS,     "住宅号
        NAME1 TYPE LFA1-NAME1,     "供应商名称
        STCEG TYPE LFA1-STCEG,     "增值税登记号
        TELF1 TYPE LFA1-TELF1,     "乙方电话

        TELFX TYPE LFA1-TELFX,     "乙方传真
      END OF TY_LFA1.

TYPES:BEGIN OF TY_KONV,
        KNUMV TYPE KONV-KNUMV,     "单据条件数
        kposn TYPE konv-kposn,
*        KBETR TYPE KONV-KBETR,     "含税价
        KSCHL TYPE KONV-KSCHL,     "条件类型
*----------------add by handyhl---2017.07.28--------------------------------------------
*        KPOSN TYPE N LENGTH 6,     "行项目号
       waers TYPE konv-waers,
       KBETR TYPE KONV-KBETR,     "含税价
       KPEIN TYPE KONV-KPEIN,
*----------------add by handyhl---2017.07.28--------------------------------------------
      END OF TY_KONV.

TYPES:BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,     "物料号
        EXTWG TYPE MARA-EXTWG,     "品牌
      END OF TY_MARA.

TYPES:BEGIN OF TY_EKKN,
        EBELN           TYPE EKKN-EBELN,
        EBELP           TYPE N LENGTH 6,      "EKKN-EBELP,
        ZEKKN           TYPE EKKN-ZEKKN,      "帐户分配的顺序编号
        PS_PSP_PNR      TYPE EKKN-PS_PSP_PNR, "项目编码
        KOSTL           TYPE EKKN-KOSTL,      "成本中心
        AUFNR           TYPE EKKN-AUFNR,      "内部订单
        ANLN1           TYPE EKKN-ANLN1,      "固定的资产号
        PS_PSP_PNR1(24) TYPE C,
      END OF TY_EKKN.

TYPES:BEGIN OF TY_PRPS,
        PSPNR TYPE PRPS-PSPNR,      "WBS要素
        PSPHI TYPE PRPS-PSPHI,      "合适的项目的当前编号
*        posid TYPE prps-posid,
        PSPID TYPE PROJ-PSPID,      "项目定义
      END OF TY_PRPS.

TYPES:BEGIN OF TY_T001,
        BUKRS TYPE T001-BUKRS,      "公司代码
        BUTXT TYPE T001-BUTXT,      "甲方
      END OF TY_T001.

TYPES:BEGIN OF TY_KNVK,
        LIFNR TYPE KNVK-LIFNR,      "供应商
        NAME1 TYPE KNVK-NAME1,      "名称
        TELF1 TYPE KNVK-TELF1,      "电话
      END OF TY_KNVK.

TYPES:BEGIN OF TY_LFBK,
        LIFNR TYPE LFBK-LIFNR,     "供应商
        KOINH TYPE LFBK-KOINH,     "银行账户号码
        BANKL TYPE LFBK-BANKL,     "银行代码
        BANKS TYPE LFBK-BANKS,     "银行国家代码
      END OF TY_LFBK.

TYPES:BEGIN OF TY_BNKA,
        BANKS TYPE BNKA-BANKS,     "银行国家代码
        BANKL TYPE BNKA-BANKL,     "银行代码
        BANKA TYPE BNKA-BANKA,     "银行名称
      END OF TY_BNKA.

TYPES:BEGIN OF TY_EKPO,
        EBELN    TYPE EKPO-EBELN,   "采购订单
        EBELP(6) TYPE N,            "行项目号
      END OF TY_EKPO.

TYPES:BEGIN OF TY_PROJ,
        PSPNR TYPE PROJ-PSPNR,      "项目定义 (内部)
        PSPID TYPE PROJ-PSPID,      "项目定义
        POST1 TYPE PROJ-POST1,      "项目名称
      END OF TY_PROJ.

TYPES:BEGIN OF TY_T052,
        ZTERM TYPE T052-ZTERM,      "付款条件
        TEXT1 TYPE T052U-TEXT1,     "收付条款的自解释
      END OF TY_T052.

TYPES:BEGIN OF TY_T052U,
        TEXT1 TYPE T052U-TEXT1,     "收付条款的自解释
        ZTERM TYPE T052-ZTERM,      "付款条件
      END OF TY_T052U.

TYPES:BEGIN OF TY_EKET,
        EBELN TYPE EKET-EBELN,
        EBELP TYPE EKET-EBELP,
        EINDT TYPE EKET-EINDT,      "计划交货日期
      END OF TY_EKET.
TYPES:BEGIN OF TY_T024,
        EKNAM TYPE T024-EKNAM,
        EKGRP TYPE T024-EKGRP,
      END OF  TY_T024.

TYPES:BEGIN OF ty_cgdd,
        ebeln type ekko-ebeln,
      end of ty_cgdd.


DATA:begin of ty_wbs,
        posid type prps-posid,
        post1 type prps-post1,
      end of ty_wbs.

**----------------add by handyhl---2017.07.27--------------------------------------------
*data : BEGIN OF ty_mep,
*       PEINH  TYPE MEPO1211-PEINH,  "数量
*       NETPR  TYPE MEPO1211-NETPR,
*   END OF TY_MEP.
*   DATA : GT_MEP LIKE TABLE OF TY_MEP,
*          GS_MEP LIKE LINE OF GT_MEP.
*
**----------------add by handyhl---2017.07.27--------------------------------------------



*---------------------------------------------------------------------*
* DEFINING A STRUCTURED                                               *
*                                                                     *
*---------------------------------------------------------------------*
DATA:GT_TAB   TYPE TABLE OF TY_TAB,  "ALV输出
     GS_TAB   LIKE LINE OF GT_TAB,

     GT_TAB_Z TYPE TABLE OF TY_TAB,  "中间变量
     GS_TAB_Z LIKE LINE OF GT_TAB_Z,

     GT_TAB1  TYPE TABLE OF TY_TAB,  "SMARTFORMS
     GS_TAB1  LIKE LINE OF GT_TAB1,

     GT_ZTXT  TYPE TABLE OF TY_ZTXT,
     GS_ZTXT  LIKE LINE OF GT_ZTXT.

DATA:GT_EKKO_EKPO  TYPE TABLE OF TY_EKKO_EKPO,
     GS_EKKO_EKPO  LIKE LINE OF GT_EKKO_EKPO,

     GT_EKKO_EKPO1 TYPE TABLE OF TY_EKKO_EKPO1,
     GS_EKKO_EKPO1 LIKE LINE OF GT_EKKO_EKPO1,

     GT_LFA1       TYPE TABLE OF TY_LFA1,
     GS_LFA1       LIKE LINE OF GT_LFA1,

     GT_KONV       TYPE TABLE OF TY_KONV,
     GS_KONV       LIKE LINE OF GT_KONV,

     GT_MARA       TYPE TABLE OF TY_MARA,
     GS_MARA       LIKE LINE OF GT_MARA,

     GT_EKKN       TYPE TABLE OF TY_EKKN,
     GS_EKKN       LIKE LINE OF GT_EKKN,

     GT_PRPS       TYPE TABLE OF TY_PRPS,
     GS_PRPS       LIKE LINE OF GT_PRPS,

     GT_T001       TYPE TABLE OF TY_T001,
     GS_T001       LIKE LINE OF GT_T001,

     GT_KNVK       TYPE TABLE OF TY_KNVK,
     GS_KNVK       LIKE LINE OF GT_KNVK,

     GT_LFBK       TYPE TABLE OF TY_LFBK,
     GS_LFBK       LIKE LINE OF GT_LFBK,

     GT_BNKA       TYPE TABLE OF TY_BNKA,
     GS_BNKA       LIKE LINE OF GT_BNKA,

     GT_PROJ       TYPE TABLE OF TY_PROJ,
     GS_PROJ       LIKE LINE OF GT_PROJ,

     GT_T052U      TYPE TABLE OF TY_T052U,
     GS_T052U      LIKE LINE OF GT_T052U,

     GT_T052       TYPE TABLE OF TY_T052,
     GS_T052       LIKE LINE OF GT_T052,

     GT_EKET       TYPE TABLE OF TY_EKET,
     GS_EKET       LIKE LINE OF GT_EKET,

     GT_T024       TYPE TABLE OF TY_T024,
     GS_T024       LIKE LINE OF GT_T024.

DATA:gt_cgdd type table of ty_cgdd,
     gs_cgdd type ty_cgdd.

DATA:OK_CODE TYPE SY-UCOMM,
     SAVE_OK TYPE SY-UCOMM.

DATA:gt_wbs like table of ty_wbs,
     gs_wbs like ty_wbs.

*-----------  smartforms数据存储结构  ------------
DATA :ZMM038_TAB TYPE ZSMM038_02.       "表头
DATA :GT_SF TYPE TABLE OF ZSMM038_01,   "表体
      GS_SF TYPE ZSMM038_01.
DATA:
      ZTXZ01 TYPE STRING.
*--------------------  分页  ---------------------
DATA:PAGE_NUM TYPE I VALUE 4,    "每页条数
     DATA_NUM TYPE I VALUE 0,    "打印的数目
     GV_SUM   TYPE ZNETPR,   "合计
     ZNUM     TYPE I VALUE 0,  "序号
     GV_SUMDX   TYPE c LENGTH 150.     "申请付款金额大写

*--------------  打印相关声明  --------------------
DATA:FM_NAME            TYPE RS38L_FNAM,
     OUTPUT             TYPE SSFCOMPOP,
     CONTROL_PARAMETERS TYPE SSFCTRLOP,
     LW_SSFCRESCL       TYPE SSFCRESCL,
     OPTION             TYPE SSFCRESCL,
     GT_PDFTAB          TYPE TABLE OF TLINE,
     G_BINFILESIZE      TYPE I.

*----------  用户选择保存路径  -------------
DATA:L_FULLPATH TYPE STRING,
     L_PATH     TYPE STRING,
     L_NAME     TYPE STRING.
*-----------屏幕下拉框-------------





DATA:LISTBOX1 TYPE C LENGTH 100,
     VVA1     TYPE VRM_VALUES,
     LVVA1    LIKE LINE OF VVA1,
     LVVA2    LIKE LINE OF VVA1,
     LVVA3    LIKE LINE OF VVA1,
     LVVA5    LIKE LINE OF VVA1,
     LVVA6    LIKE LINE OF VVA1.
DATA:
      LVVA4 TYPE C LENGTH 200.

*---------------------------------------------------------------------*
* ALV                                                                 *
*                                                                     *
*---------------------------------------------------------------------*
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV, " WITH HEADER LINE,"全局内表
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
*---------------------------------------------------------------------*
* MACROS                                                              *
*                                                                     *
*---------------------------------------------------------------------*
DEFINE FILL_FIELDCAT.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = &1.      "字段名称
  GS_FIELDCAT-SELTEXT_L = &2.      "文本
  GS_FIELDCAT-KEY = &3.            "是否为主键
  GS_FIELDCAT-DECIMALS_OUT = &4.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

*---------------------------------------------------------------------*
* SELECTION-SCREEN                                                    *
*                                                                     *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:S_WBS   FOR EKKN-PS_PSP_PNR,  "项目WBS
               S_EBELN FOR EKKN-EBELN,     "采购订单
               S_EKORG FOR EKKO-EKORG,     "采购组织
               S_EKGRP FOR EKKO-EKGRP,     "采购组
               S_LIFNR FOR EKKO-LIFNR,     "供应商
               S_ERNAM FOR EKKO-ERNAM,     "创建人
               S_MATNR FOR EKPO-MATNR,     "物料号
               S_MATKL FOR EKPO-MATKL,     "物料组
               S_EXTWG FOR MARA-EXTWG,     "品牌
               S_WERKS FOR EKPO-WERKS,     "工厂
               S_AUFNR FOR EKKN-AUFNR,     "内部订单号
               S_KOSTL FOR EKKN-KOSTL,     "成本中心
               S_ANLN1 FOR EKKN-ANLN1,     "资产号码
               S_AEDAT FOR EKPO-AEDAT,     "凭证日期
               S_EINDT FOR EKET-EINDT.     "计划交货日期

SELECTION-SCREEN END OF BLOCK BLK1.

*---------------------------------------------------------------------*
* INITIALIZATION                                                      *
*                                                                     *
*---------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_WBS-low.
*  PERFORM getwbs.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_WBS-high.
*  PERFORM getwbs.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                 *
*                                                                     *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.


*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*                                                                     *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM FRM_SELDATA.
  IF GT_EKKO_EKPO IS INITIAL.
    MESSAGE '查询无数据' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*---------------------------------------------------------------------*
*   END-OF-SELECTION                                                  *
*                                                                     *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM FRM_LAYOUT.    "alv输出属性
  PERFORM FRM_FIELDCAT.  "alv输出样式
  PERFORM FRM_ALV.       "alv展示

*&---------------------------------------------------------------------*
*&      FORM  FRM_SELDATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_SELDATA .

*----------------  定义局部变量  --------------------------
  DATA: LT_EKPO TYPE TABLE OF TY_EKKO_EKPO,
        LT_EKKN TYPE TABLE OF TY_EKKN,
*        lt_ekkn TYPE TABLE OF ty_ekko_ekpo,
        LT_LFA1 TYPE TABLE OF TY_LFA1,
        LT_LFBK TYPE TABLE OF TY_LFBK.

*--------------先查出关于WBS相关的EBELN。
  SELECT
    EBELN
    EBELP
    ZEKKN
    PS_PSP_PNR     "项目编码
    KOSTL          "成本中心
    AUFNR          "内部订单
    ANLN1          "固定的资产号
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
    FROM EKKN
*      FOR ALL ENTRIES IN gt_ekko_ekpo1
*      WHERE ebeln = gt_ekko_ekpo1-ebeln
    WHERE
    EBELN IN S_EBELN
*      AND ebelp = gt_ekko_ekpo1-ebelp
    AND
    PS_PSP_PNR IN S_WBS.


*&--代码注释 BY HANDYBY 26.07.2017 20:01:56  BEGIN
*    IF s_wbs IS INITIAL AND s_ebeln IS  INITIAL.
*&--代码注释 BY HANDYBY 26.07.2017 20:01:56  END
*&--代码添加 BY HANDYBY 26.07.2017 20:02:07  BEGIN
  IF S_WBS IS INITIAL .
*&--代码添加 BY HANDYBY 26.07.2017 20:02:07  END

*-----------------  主表  -----------------
    SELECT
      EKKO~EBELN     "采购订单
      EKKO~EKGRP     "采购组
      EKKO~LIFNR     "供应商
      EKKO~ERNAM     "创建者
      EKKO~AEDAT     "创建日期
      EKPO~EBELP     "行项目号
*------add by handyhl 2017.07.28-begin--------------------------------------------
*      EKkO~EBELP     "行项目号
*------add by handyhl 2017.07.28-end--------------------------------------------
      EKPO~KNTTP     "科目分配类别
      EKPO~PSTYP     "行项目类别
      EKPO~MATNR     "物料号
      EKPO~TXZ01     "物料描述
      EKPO~MEINS     "单位
      EKPO~MENGE     "数量
*      EKPO~BPRME     "货币
      EKPO~MWSKZ     "税码
      EKPO~WERKS     "工厂
      EKPO~LGORT     "库存地点
      EKPO~BEDNR     "申请部门
      EKPO~AFNAM     "申请人
      EKKO~KNUMV
      EKKO~BUKRS     "公司代码
      EKKO~ZTERM      "付款条件
      EKKO~ZBNX
      EKKO~YFCDF
*----------------add by handyhl---2017.07.27--------------------------------------------
*      ekpo~netpr
*      ekpo~PEINH
      ekpo~loekz
*----------------add by handyhl---2017.07.27--------------------------------------------

*
*    EKKN~zekkn
*    EKKN~ps_psp_pnr     "项目编码
*    EKKN~kostl          "成本中心
*    EKKN~aufnr          "内部订单
*    EKKN~anln1          "固定的资产号
      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO_EKPO
      FROM EKKO
*    INNER JOIN EKKN ON EKKO~EBELN = EKKN~EBELN
      INNER JOIN EKPO ON EKKO~EBELN = EKPO~EBELN
*FOR ALL ENTRIES IN GT_EKKN

      WHERE
      EKKO~EBELN IN S_EBELN
      AND EKKO~EKORG IN S_EKORG
      AND EKKO~EKGRP IN S_EKGRP
      AND EKKO~LIFNR IN S_LIFNR
      AND EKKO~ERNAM IN S_ERNAM
      AND EKPO~MATNR IN S_MATNR
      AND EKPO~MATKL IN S_MATKL
      AND EKPO~WERKS IN S_WERKS
      AND EKPO~AEDAT IN S_AEDAT
       and ekpo~loekz ne 'L'
*    AND EKKN~ps_psp_pnr IN s_wbs
   .


  ELSEIF GT_EKKN IS NOT INITIAL.

*-----------------  主表  -----------------

    SELECT
      EKKO~EBELN     "采购订单
      EKKO~EKGRP     "采购组
      EKKO~LIFNR     "供应商
      EKKO~ERNAM     "创建者
      EKKO~AEDAT     "创建日期
      EKPO~EBELP     "行项目号
      EKPO~KNTTP     "科目分配类别
      EKPO~PSTYP     "行项目类别
      EKPO~MATNR     "物料号
      EKPO~TXZ01     "物料描述
      EKPO~MEINS     "单位
      EKPO~MENGE     "数量
*      EKPO~BPRME     "货币
      EKPO~MWSKZ     "税码
      EKPO~WERKS     "工厂
      EKPO~LGORT     "库存地点
      EKPO~BEDNR     "申请部门
      EKPO~AFNAM     "申请人
      EKKO~KNUMV
      EKKO~BUKRS     "公司代码
      EKKO~ZTERM      "付款条件
      EKKO~ZBNX
      EKKO~YFCDF
*----------------add by handyhl---2017.07.27--------------------------------------------
*      ekpo~netpr
*      ekpo~PEINH
      ekpo~loekz
*----------------add by handyhl---2017.07.27--------------------------------------------
*
*    EKKN~zekkn
*    EKKN~ps_psp_pnr     "项目编码
*    EKKN~kostl          "成本中心
*    EKKN~aufnr          "内部订单
*    EKKN~anln1          "固定的资产号

      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO_EKPO
      FROM EKKO
*    INNER JOIN EKKN ON EKKO~EBELN = EKKN~EBELN
      INNER JOIN EKPO ON EKKO~EBELN = EKPO~EBELN
  FOR ALL ENTRIES IN GT_EKKN

      WHERE EKKO~EBELN = GT_EKKN-EBELN
      AND EKKO~EKORG IN S_EKORG
      AND EKKO~EKGRP IN S_EKGRP
      AND EKKO~LIFNR IN S_LIFNR
      AND EKKO~ERNAM IN S_ERNAM
      AND EKPO~MATNR IN S_MATNR
      AND EKPO~MATKL IN S_MATKL
      AND EKPO~WERKS IN S_WERKS
      AND EKPO~AEDAT IN S_AEDAT
      and ekpo~loekz ne 'L'
*    AND EKKN~ps_psp_pnr IN s_wbs
      .

  ENDIF.

  "查询采购订单
 if gt_ekko_ekpo is not initial.
      move-corresponding GT_EKKO_EKPO to gt_cgdd.
      sort gt_cgdd by ebeln.
      delete adjacent duplicates from gt_cgdd comparing ebeln .
 endif.



**------------------   EKET表  ----------------------------
*  SELECT
*    EBELN
*    EBELP
*    EINDT
*    INTO TABLE GT_EKET
*    FROM EKET
**      FOR ALL ENTRIES IN gt_ekko_ekpo1
**      WHERE ebeln = gt_ekko_ekpo1-ebeln
*    WHERE EBELN IN S_EBELN
***      AND ebelp = gt_ekko_ekpo1-ebelp
*    AND EINDT IN S_EINDT.
*
**------------------  删除小于系统当前日期的数据  ----------------------
*  DELETE GT_EKET WHERE EINDT < SY-DATUM.
*  SORT GT_EKET BY EBELN EBELP EINDT.
*  DELETE ADJACENT DUPLICATES FROM GT_EKET COMPARING EBELN EBELP.


"add it02&魏云 20170825  begin
if gt_cgdd is not initial.
    select
    EBELN
    EBELP
    EINDT
    INTO TABLE GT_EKET
    FROM EKET
    for all entries in gt_cgdd
    where ebeln = gt_cgdd-ebeln
    .
 sort gt_eket by ebeln ebelp.

endif.
"add it02&魏云 20170825  end
*-------------------  EKKN表  ----------------------
*  SELECT
*    ebeln
*    ebelp
*    zekkn
*    ps_psp_pnr     "项目编码
*    kostl          "成本中心
*    aufnr          "内部订单
*    anln1          "固定的资产号
*    INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
*    FROM ekkn
**      FOR ALL ENTRIES IN gt_ekko_ekpo1
**      WHERE ebeln = gt_ekko_ekpo1-ebeln
*    WHERE ebeln IN s_ebeln
**      AND ebelp = gt_ekko_ekpo1-ebelp
*    AND ps_psp_pnr IN s_wbs
*    .

  IF GT_EKKN IS NOT INITIAL.
*&---------------  先取合适的项目的当前编号  -----------
    APPEND LINES OF GT_EKKN TO LT_EKKN.
    SORT LT_EKKN BY PS_PSP_PNR.
    DELETE ADJACENT DUPLICATES FROM LT_EKKN COMPARING PS_PSP_PNR.

    SELECT
      PSPNR
      PSPHI

      INTO TABLE GT_PRPS
      FROM PRPS
      FOR ALL ENTRIES IN LT_EKKN
      WHERE PSPNR = LT_EKKN-PS_PSP_PNR
            .
    FREE: LT_EKKN.
*---------------------  将八位的内标编号转换为12位 ---------------
    LOOP AT GT_PRPS INTO GS_PRPS.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          INPUT  = GS_PRPS-PSPHI
        IMPORTING
          OUTPUT = GS_PRPS-PSPID
*         PSELT  =
        .
      MODIFY GT_PRPS FROM GS_PRPS.
    ENDLOOP.

*&-----------------  项目名称  ---------------------------
    IF GT_PRPS IS NOT INITIAL.
      SELECT
        PSPNR
        PSPID
        POST1
        INTO TABLE GT_PROJ
        FROM PROJ
        FOR ALL ENTRIES IN GT_PRPS
        WHERE PSPID = GT_PRPS-PSPID
        .
    ENDIF.
  ENDIF.

*----------------------  LFA1表  --------------
  IF GT_EKKO_EKPO IS NOT INITIAL.


    APPEND LINES OF GT_EKKO_EKPO TO LT_EKPO.
    SORT LT_EKPO BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING LIFNR.

    SELECT
      LIFNR
      STRAS     "住宅号
      NAME1
      STCEG
      TELF1
      TELFX
      INTO TABLE GT_LFA1
      FROM LFA1
      FOR ALL ENTRIES IN LT_EKPO
      WHERE LIFNR = LT_EKPO-LIFNR
      .
    FREE: LT_EKPO.

*-----------------------  KNVK表 -----------------------
    IF GT_LFA1 IS NOT INITIAL.
      APPEND LINES OF GT_LFA1 TO LT_LFA1.
      SORT LT_LFA1 BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_LFA1 COMPARING LIFNR.

      SELECT
        LIFNR
        NAME1
        TELF1
        INTO TABLE GT_KNVK
        FROM KNVK
        FOR ALL ENTRIES IN LT_LFA1
        WHERE LIFNR = LT_LFA1-LIFNR
        .

*----------------------  取LFBK表  ----------------------------
      SELECT
        LIFNR
        KOINH
        BANKL
        BANKS
        INTO TABLE GT_LFBK
        FROM LFBK
        FOR ALL ENTRIES IN LT_LFA1
        WHERE LIFNR = LT_LFA1-LIFNR
        .
      FREE:LT_LFA1.

*----------------------  BNKA表  ---------------------------
      IF GT_LFBK IS NOT INITIAL.
        APPEND LINES OF GT_LFBK TO LT_LFBK.
        SORT LT_LFBK BY BANKL.
        DELETE ADJACENT DUPLICATES FROM LT_LFBK COMPARING BANKL.
        SELECT
          BANKS
          BANKL
          BANKA
          INTO TABLE GT_BNKA
          FROM BNKA
          FOR ALL ENTRIES IN LT_LFBK
          WHERE BANKL = LT_LFBK-BANKL
          .
        FREE:LT_LFBK.
      ENDIF.
    ENDIF.

*-------------------------  KNOV表  ----------------------
    APPEND LINES OF GT_EKKO_EKPO TO LT_EKPO.
    SORT LT_EKPO BY KNUMV EBELP.
    DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING KNUMV EBELP.
    SELECT
      KNUMV
      KPOSN "AS EBELP
*      KBETR
      KSCHL
*----------------add by handyhl---2017.07.28-------------------------------------------
     waers
     KBETR
     KPEIN
*----------------add by handyhl---2017.07.28--------------------------------------------
      INTO TABLE GT_KONV
      FROM KONV
      FOR ALL ENTRIES IN LT_EKPO
      WHERE KNUMV = LT_EKPO-KNUMV
      AND KPOSN = LT_EKPO-EBELP
*----------------add by handyhl---2017.07.28-------------------------------------------
   "  AND  KSCHL = 'PBXX'.
*----------------add by handyhl---2017.07.28--------------------------------------------
     AND ( KSCHL = 'PBXX' OR KSCHL = 'PB00' ) .


*-------------------------  MARA表  ------------------------
    SELECT
      MATNR
      EXTWG
      INTO TABLE GT_MARA
      FROM MARA
      FOR ALL ENTRIES IN LT_EKPO
      WHERE MATNR = LT_EKPO-MATNR
      .
    FREE: LT_EKPO.

*--------------------  T001表  ----------------------------
    APPEND LINES OF GT_EKKO_EKPO TO LT_EKPO.
    SORT LT_EKPO BY BUKRS.
    DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING BUKRS.
    SELECT
      BUKRS
      BUTXT        "甲方
      INTO TABLE GT_T001
      FROM T001
      FOR ALL ENTRIES IN LT_EKPO
      WHERE BUKRS = LT_EKPO-BUKRS
      .
    FREE: LT_EKPO.
*---------------T024表-------------
    SELECT
      EKGRP
      EKNAM
     FROM T024 INTO CORRESPONDING FIELDS OF TABLE GT_T024
      FOR ALL ENTRIES IN GT_EKKO_EKPO
      WHERE EKGRP = GT_EKKO_EKPO-EKGRP.
  ENDIF.

*---------------  T052表取付款条件  --------------------
  SELECT
    T052~ZTERM
    T052U~TEXT1
    INTO TABLE GT_T052
    FROM T052
    INNER JOIN T052U
    ON T052~ZTERM = T052U~ZTERM
    .

*-------------   排序  --------------
  SORT GT_EKKN BY EBELN EBELP.
  SORT GT_PRPS BY PSPNR.
  SORT GT_PROJ BY PSPID.
  SORT GT_LFA1 BY LIFNR.
  SORT GT_KONV BY KNUMV KPOSN.
  SORT GT_MARA BY MATNR.
  SORT GT_T001 BY BUKRS.
  SORT GT_KNVK BY LIFNR.
  SORT GT_BNKA BY BANKL.
  SORT GT_LFBK BY LIFNR.
  SORT GT_T052 BY ZTERM.
  SORT GT_T024 BY EKGRP.

*-----------------------  将值传给ALV显示的内表  -----------------------------
  LOOP AT GT_EKKO_EKPO INTO GS_EKKO_EKPO.
    GS_TAB-EBELN = GS_EKKO_EKPO-EBELN.     "采购订单
    GS_TAB-EKGRP = GS_EKKO_EKPO-EKGRP.     "采购组
    GS_TAB-LIFNR = GS_EKKO_EKPO-LIFNR.     "供应商
    GS_TAB-ERNAM = GS_EKKO_EKPO-ERNAM .    "创建者
    GS_TAB-AEDAT = GS_EKKO_EKPO-AEDAT .    "创建日期
    GS_TAB-EBELP = GS_EKKO_EKPO-EBELP .    "行项目号
    GS_TAB-KNTTP = GS_EKKO_EKPO-KNTTP .    "科目分配类别
    GS_TAB-PSTYP = GS_EKKO_EKPO-PSTYP .    "行项目类别
    GS_TAB-MATNR = GS_EKKO_EKPO-MATNR .    "物料号
    GS_TAB-TXZ01 = GS_EKKO_EKPO-TXZ01 .    "物料描述
    GS_TAB-MEINS = GS_EKKO_EKPO-MEINS .    "单位、、

    GS_TAB-MEINS_1 = GS_TAB-MEINS.

    GS_TAB-MENGE = GS_EKKO_EKPO-MENGE .    "数量
*----------------add by handyhl---2017.07.27--------------------------------------------
*    GS_TAB-peinh = GS_EKKO_EKPO-peinh .    "数量
*    GS_TAB-ZNETPR = GS_EKKO_EKPO-NETPR / GS_EKKO_EKPO-PEINH .
*----------------add by handyhl---2017.07.27--------------------------------------------
*
*----------------add by handyhl---2017.07.28--------------------------------------------
*    GS_TAB-BPRME = GS_EKKO_EKPO-BPRME .    "货币
*----------------add by handyhl---2017.07.28--------------------------------------------
    GS_TAB-MWSKZ = GS_EKKO_EKPO-MWSKZ .    "税码
    GS_TAB-WERKS = GS_EKKO_EKPO-WERKS .    "工厂
    GS_TAB-LGORT = GS_EKKO_EKPO-LGORT .    "库存地点
    GS_TAB-BEDNR = GS_EKKO_EKPO-BEDNR .    "申请部门
    GS_TAB-AFNAM = GS_EKKO_EKPO-AFNAM .    "申请人
    GS_TAB-ZBNX = GS_EKKO_EKPO-ZBNX.
    GS_TAB-YFCDF = GS_EKKO_EKPO-YFCDF.
*&-------------------------------  取项目名称  -------------------------------------
    READ TABLE GT_EKKN INTO GS_EKKN WITH KEY EBELN = GS_EKKO_EKPO-EBELN
                                            EBELP = GS_EKKO_EKPO-EBELP+1(5)
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-PS_PSP_PNR = GS_EKKN-PS_PSP_PNR.   "项目编码
      GS_TAB-KOSTL = GS_EKKN-KOSTL.          "成本中心
      GS_TAB-AUFNR = GS_EKKN-AUFNR.          "内部订单
      GS_TAB-ANLN1 = GS_EKKN-ANLN1.          "固定的资产号
      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY PSPNR = GS_EKKN-PS_PSP_PNR BINARY SEARCH.
      IF SY-SUBRC = 0.
*----------------------------------   项目名称  -----------------------------------------
        READ TABLE GT_PROJ INTO GS_PROJ WITH KEY PSPID = GS_PRPS-PSPID BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_TAB-POST1 = GS_PROJ-POST1.
        ENDIF.
      ENDIF.
    ENDIF.

*---------------------------  计划交货日期  ---------------------------------
    READ TABLE GT_EKET INTO GS_EKET WITH KEY EBELN = GS_EKKO_EKPO-EBELN
                                                 EBELP = GS_EKKO_EKPO-EBELP+1(5)
                                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-EINDT = GS_EKET-EINDT.
    ENDIF.

*----------------------------------   取银行名称   --------------------------------------
    READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_EKKO_EKPO-LIFNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-NAME1 = GS_LFA1-NAME1.
      GS_TAB-STRAS = GS_LFA1-STRAS.
      GS_TAB-STCEG = GS_LFA1-STCEG.
      GS_TAB-TELF2 = GS_LFA1-TELF1.
      GS_TAB-TELFX = GS_LFA1-TELFX.
      READ TABLE GT_KNVK INTO GS_KNVK WITH KEY LIFNR = GS_LFA1-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_TAB-NAME2 = GS_KNVK-NAME1.
        GS_TAB-TELF1 = GS_KNVK-TELF1.
      ENDIF.
      READ TABLE GT_LFBK INTO GS_LFBK WITH KEY LIFNR = GS_LFA1-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_TAB-KOINH = GS_LFBK-KOINH.
        READ TABLE GT_BNKA INTO GS_BNKA WITH KEY BANKL = GS_LFBK-BANKL
                                                 BANKS = GS_LFBK-BANKS
                                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_TAB-BANKA = GS_BNKA-BANKA.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE GT_KONV INTO GS_KONV WITH KEY KNUMV = GS_EKKO_EKPO-KNUMV
                                             KPOSN = GS_EKKO_EKPO-EBELP
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
*--------add by handyhl----2017.07.27----begin------------------------------------------------------------------
       GS_TAB-KBETR = GS_KONV-KBETR.
*--------add by handyhl----2017.07.27----end---------------------------------

*--------add by handyhl----2017.07.28----begin------------------------------------------------------------------
      GS_TAB-waers = GS_KONV-waers.
      GS_TAB-ZNETPR = GS_KONV-KBETR / GS_KONV-KPEIN.

      GS_TAB-WAERS = GS_KONV-WAERS.
   "   GS_TAB-ZNETPR  = GS_KONV-KBETR .


*--------add by handyhl----2017.07.28----end---------------------------------
      GS_TAB-KNUMV = GS_KONV-KNUMV.

      "打印数量
      GS_TAB-MENGE_1 = GS_TAB-MENGE / GS_KONV-KPEIN .

      IF GS_KONV-KPEIN EQ '1000'.
         IF GS_TAB-MEINS EQ 'PC'.
           GS_TAB-MEINS_1 = '千只'.
           ELSEIF GS_TAB-MEINS EQ 'M'..
               GS_TAB-MEINS_1 = '千米'.
            ENDIF.
      ENDIF.
      ELSE.
         GS_TAB-MENGE_1 = GS_TAB-MENGE .
    ENDIF.

    READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_EKKO_EKPO-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-EXTWG = GS_MARA-EXTWG.
    ENDIF.

    READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = GS_EKKO_EKPO-BUKRS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-BUTXT = GS_T001-BUTXT.
    ENDIF.

    READ TABLE GT_T052 INTO GS_T052 WITH KEY ZTERM = GS_EKKO_EKPO-ZTERM BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-TEXT1 = GS_T052-TEXT1.
    ENDIF.
    READ TABLE GT_T024 INTO GS_T024 WITH  KEY EKGRP = GS_EKKO_EKPO-EKGRP BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_TAB-EKNAM = GS_T024-EKNAM+5(10).
    ENDIF.
*---------------------------  去前导零  ------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_EKKO_EKPO-LIFNR
      IMPORTING
        OUTPUT = GS_TAB-LIFNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_EKKN-AUFNR
      IMPORTING
        OUTPUT = GS_TAB-AUFNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_EKKN-KOSTL
      IMPORTING
        OUTPUT = GS_TAB-KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_EKKN-ANLN1
      IMPORTING
        OUTPUT = GS_TAB-ANLN1.
    APPEND GS_TAB TO GT_TAB.
    CLEAR GS_TAB.
    CLEAR GS_EKKO_EKPO.
    CLEAR GS_LFA1.
    CLEAR GS_KONV.
    CLEAR GS_MARA.
    CLEAR GS_EKKN.
    CLEAR GS_PRPS.

  ENDLOOP.
*--------add by handyhl----2017.07.27----begin------------------------------------------------------------------
*  delete gt_tab WHERE lgort is NOT INITIAL.
*--------add by handyhl----2017.07.27----end------------------------------------------------------------------

*------------------------  送货地址的默认值  -------------------------
  GS_ZTXT-ZTXT = '广州市萝岗区科学城科学大道8号 程建彬020-66829500-2219'.

  SORT GT_TAB BY EBELN EBELP.
*  MOVE GT_TAB TO GT_TAB1.
  gt_tab1 = gt_tab.
ENDFORM.                    " FRM_SELDATA
*&---------------------------------------------------------------------*
*&      FORM  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_LAYOUT .

  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA = 'X'.             "颜色间隔
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'. "自适应宽度
  GS_LAYOUT-BOX_FIELDNAME = 'BOX'.

ENDFORM.                    " FRM_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT .

  FILL_FIELDCAT:'EBELN' TEXT-002 'X' '',
                'EBELP' TEXT-003 'X' '',
                'KNTTP' TEXT-004 '' '',
                'PSTYP' TEXT-005 '' '',
                'EKGRP' TEXT-006 '' '',
                'LIFNR' TEXT-007 '' '',
                'NAME1' TEXT-008 '' '',
                'MATNR' TEXT-009 '' '',
                'TXZ01' TEXT-010 '' '',
                'EXTWG' TEXT-011 '' '',
                'MEINS' TEXT-012 '' '',
                'MENGE' TEXT-013 '' '',
*                 'KBETR' TEXT-014 '',
*--------add by handyhl----2017.07.27----begin------------------------------------------------------------------
*                 'PEINH' TEXT-013 '',
                 'ZNETPR' TEXT-014 '' '',
*--------add by handyhl----2017.07.27----end------------------------------------------------------------------

                'WAERS' TEXT-015 '' '',
                'MWSKZ' TEXT-016 '' '',
                'EINDT' TEXT-017 '' '',
                'WERKS' TEXT-018 '' '',
                'LGORT' TEXT-019 '' '',
                'PS_PSP_PNR' TEXT-020 '' '',
                'POST1' TEXT-021 '' '',
                'KOSTL' TEXT-022 '' '',
                'AUFNR' TEXT-023 '' '',
                'ANLN1' TEXT-024 '' '',
                'BEDNR' TEXT-025 '' '',
                'AFNAM' TEXT-026 '' '',
                'ERNAM' TEXT-027 '' '',
                'AEDAT' TEXT-028 '' ''.

ENDFORM.                    " FRM_FIELDCAT
*&---------------------------------------------------------------------*
*&      FORM  FRM_ALV
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_ALV .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = 'ZMM038'            "当前程序名、
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IS_LAYOUT                = GS_LAYOUT       "输出样式
      IT_FIELDCAT              = GT_FIELDCAT[]   "字段定义描述表
*     I_CALLBACK_TOP_OF_PAGE   = 'F_TOP_OF_PAGE'
      I_SAVE                   = 'A'
*     IT_SORT                  = LT_SORT
    TABLES
      T_OUTTAB                 = GT_TAB        "内表输出名称
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_ALV

*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA: BEGIN OF EX_TAB OCCURS 0,
          FCODE LIKE SY-UCOMM,
        END   OF EX_TAB.
  EX_TAB-FCODE = '&ILT'.
  APPEND EX_TAB.
  SET PF-STATUS '9002' EXCLUDING EX_TAB.

ENDFORM. "SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN 'SAVE'.
      CALL SCREEN 9000 STARTING AT 35 3.

  ENDCASE.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS '9000'.
  LVVA1-KEY = '1、在质保期内，除未按产品说明使用和不可抗力的原因外，对产品产品由于设计、工艺'.
  LVVA2-KEY = '、材料、制造、安装的缺陷而发生的任何质量故障，甲方有权要求乙方在24小时内派专'.
  LVVA3-KEY = '业人员到故障现场维修,免收维修工资和配件材料费。如质量问题严重需发回工厂维修'.
  LVVA5-KEY = '而产生的国内运费由乙方承担，乙方应保证提供替代产品给甲方。如乙方未按甲方'.
  LVVA6-KEY = '要求及时修复，甲方可委托他人维修，所需必要合理的维修费用由乙方承担。'.
  CONCATENATE LVVA1-KEY LVVA2-KEY LVVA3-KEY LVVA5-KEY LVVA6-KEY INTO LVVA4.
  APPEND LVVA4 TO VVA1.
  LVVA1-KEY = '2、在质保期内，除未按产品说明使用和不可抗力的原因外，乙方按甲方要求的时间提供'.
  LVVA2-KEY = '无偿售后服务。如乙方未按甲方要求及时修复，甲方可委托他人维修，所需必要合理的'.
  LVVA3-KEY = '维修费用由乙方承担。在质保期内，甲方工程项目有重大活动期间，乙方提供免费上门'.
  LVVA5-KEY = '保驾护航服务，免费向甲方人员提供合同产品及配套产品的安装、操作、维护及保'.
  LVVA6-KEY = '养方面必要的咨询服务直到甲方相关人员完全掌握相关技术。'.
  CONCATENATE LVVA1-KEY LVVA2-KEY LVVA3-KEY LVVA5-KEY LVVA6-KEY INTO LVVA4.
  APPEND LVVA4 TO VVA1.
  LVVA1-KEY = '3、略'.
  APPEND LVVA1 TO VVA1.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'LISTBOX1'
      VALUES          = VVA1
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CLEAR:VVA1.

*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'EXIT' OR '&F03' OR '&F12' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN 'PRINT'.

      PERFORM FRM_PRINT.
    WHEN 'PRINTPDF'.

* -------------------  用户选择保存路径  --------------------------
      PERFORM FRM_GET_FULLPATH CHANGING L_FULLPATH L_PATH L_NAME.
*-----------------  路径为空则退出  --------------------
      IF L_FULLPATH IS INITIAL.
        MESSAGE TEXT-029 TYPE 'E'.
        RETURN.
      ENDIF.

      PERFORM FRM_PRINTPDF.   "打印PDF
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      FORM  FRM_PRINT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_PRINT .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'    "根据FORM名取的函数名
    EXPORTING
      FORMNAME = 'ZMM038' "'ZMM038'                     "FORM名
*     VARIANT  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME  = FM_NAME                      "返回函数名
*       EXCEPTIONS
*     NO_FORM  = 1
*     NO_FUNCTION_MODULE       = 2
*     OTHERS   = 3
    .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*--------------------  打印设置  ----------------------
  CONTROL_PARAMETERS-NO_OPEN   = 'X'.
  CONTROL_PARAMETERS-NO_CLOSE  = 'X'.
  CONTROL_PARAMETERS-NO_DIALOG  = 'X'.   "跳过打印预览
  CONTROL_PARAMETERS-PREVIEW  = 'X'.     "跳过打印预览

  OUTPUT-TDDEST = 'LP01'.
  OUTPUT-RQPOSNAME = ''.
  OUTPUT-TDDATASET = ''.
  OUTPUT-TDSUFFIX1 = ''.
  OUTPUT-TDSUFFIX2 = ''.
  OUTPUT-TDIMMED   = 'X'.
  OUTPUT-TDDELETE  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = OUTPUT
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.


  LOOP AT GT_TAB INTO GS_TAB WHERE BOX = 'X'.
    gt_tab1 = gt_tab.
    GS_TAB_Z-BUTXT = GS_TAB-BUTXT.
    GS_TAB_Z-EBELN = GS_TAB-EBELN.
    GS_TAB_Z-NAME1 = GS_TAB-NAME1.
    GS_TAB_Z-AEDAT = GS_TAB-AEDAT.
    GS_TAB_Z-STRAS = GS_TAB-STRAS.
    GS_TAB_Z-NAME2 = GS_TAB-NAME2.
    GS_TAB_Z-TELF1 = GS_TAB-TELF1.
    GS_TAB_Z-BANKA = GS_TAB-BANKA.
    GS_TAB_Z-KOINH = GS_TAB-KOINH.
    GS_TAB_Z-STRAS = GS_TAB-STRAS.
    GS_TAB_Z-STCEG = GS_TAB-STCEG.
    GS_TAB_Z-ZBNX = GS_TAB-ZBNX.
    GS_TAB_Z-TEXT1 = GS_TAB-TEXT1.
    GS_TAB_Z-YFCDF = GS_TAB-YFCDF.
    GS_TAB_Z-STCEG = GS_TAB-STCEG.
    GS_TAB_Z-TELF2 = GS_TAB-TELF2.
    GS_TAB_Z-TELFX = GS_TAB-TELFX.
    GS_TAB_Z-MWSKZ = GS_TAB-MWSKZ.
    GS_TAB_Z-EKNAM = GS_TAB-EKNAM.
    AT NEW EBELN.
*------------------  表头显示的数据  ----------------------
      ZMM038_TAB-BUTXT = GS_TAB_Z-BUTXT.
      ZMM038_TAB-EBELN = GS_TAB_Z-EBELN.
      ZMM038_TAB-NAME1 = GS_TAB_Z-NAME1.
      ZMM038_TAB-AEDAT = GS_TAB_Z-AEDAT.
      ZMM038_TAB-STRAS = GS_TAB_Z-STRAS.
      ZMM038_TAB-NAME2 = GS_TAB_Z-NAME2.
      ZMM038_TAB-TELF1 = GS_TAB_Z-TELF1.
      ZMM038_TAB-BANKA = GS_TAB_Z-BANKA.
      ZMM038_TAB-KOINH = GS_TAB_Z-KOINH.
      ZMM038_TAB-STRAS = GS_TAB_Z-STRAS.
      ZMM038_TAB-STCEG = GS_TAB_Z-STCEG.
      ZMM038_TAB-ZTXT2 = GS_ZTXT-ZTXT.
      ZMM038_TAB-ZBNX = GS_TAB_Z-ZBNX+11(2).
      ZMM038_TAB-TEXT1 = GS_TAB_Z-TEXT1.
      ZMM038_TAB-YFCDF = GS_TAB_Z-YFCDF.
      ZMM038_TAB-TELF2 = GS_TAB_Z-TELF2.   "乙方电话
      ZMM038_TAB-TELFX = GS_TAB_Z-TELFX.   "乙方传真
      ZMM038_TAB-EKNAM = GS_TAB_Z-EKNAM.

      IF LISTBOX1 = '1、在质保期内，除未按产品说明使用和不可抗力的原因外，对产品产品由于设计、工艺、'.
        ZMM038_TAB-LISTBOX = '在质保期内，除未按产品说明使用和不可抗力的原因外，对产品产品由于设计、工艺、材料、制造、安装的缺陷而发生的任何质量故障，甲方有权要求乙方在24小时内派专业人员到故障现场维修，免收维修工资和配件材料费。如质量问题严重需发回工厂维修而产生的国内运费由乙方承担，乙方应保证提供替代产品给甲方。如乙方未按甲方要求及时修复，甲方可委托他人维修，所需必要合理的维修费用由乙方承担。'.
      ELSEIF LISTBOX1 = '2、在质保期内，除未按产品说明使用和不可抗力的原因外，乙方按甲方要求的时间提供无'.
        ZMM038_TAB-LISTBOX = '在质保期内，除未按产品说明使用和不可抗力的原因外，乙方按甲方要求的时间提供无偿售后服务。如乙方未按甲方要求及时修复，甲方可委托他人维修，所需必要合理的维修费用由乙方承担。在质保期内，甲方工程项目有重大活动期间，乙方提供免费上门保驾护航服务，免费向甲方人员提供合同产品及配套产品的安装、操作、维护及保养方面必要的咨询服务直到甲方相关人员完全掌握相关技术。'.
      ELSEIF LISTBOX1 = '3、略'.
        ZMM038_TAB-LISTBOX = '略'.
      ELSE.
        ZMM038_TAB-LISTBOX = '略'.

      ENDIF.

*--------------------------  判断税号  ----------------------------------
      CASE GS_TAB_Z-MWSKZ.
        WHEN 'J1'.
          ZMM038_TAB-ZTXT1 = '17%税增值税专用发票'.
        WHEN 'J6'.
          ZMM038_TAB-ZTXT1 = '3%税增值税发票'.
        WHEN 'J3'.
          ZMM038_TAB-ZTXT1 = '11%税增值税专用发票'.
        WHEN 'J5'.
          ZMM038_TAB-ZTXT1 = '6%税增值税发票'.
      ENDCASE.

    ENDAT.
    CLEAR GS_TAB_Z.
*    zmm038_tab-LISTBOX = LISTBOX1.

    AT END OF EBELN.
      GV_SUM = 0.
        DELETE GT_TAB1 WHERE BOX NE 'X'.
*------------------------   表体循环显示的数据  ----------------------
      LOOP AT GT_TAB1 INTO GS_TAB1 WHERE EBELN = GS_TAB-EBELN
                                     and  box  = 'X'.

     "   DATA_NUM = DATA_NUM + 1.
        ZNUM = ZNUM + 1.
        GS_SF-ZNUM = ZNUM.                "序号
        GS_SF-EXTWG = GS_TAB1-EXTWG.      " 品牌
        GS_SF-TXZ01  = GS_TAB1-TXZ01.
        CONDENSE GS_SF-TXZ01 NO-GAPS.
        GS_SF-MEINS = GS_TAB1-MEINS.

        GS_SF-MEINS_1 = GS_TAB1-MEINS_1. "单位 add by it02
*        GS_SF-MENGE = GS_TAB1-MENGE.
*--------add by handyhl----2017.07.27----begin----------------------
*        READ TABLE GT_EKKO_EKPO INTO GS_EKKO_EKPO WITH KEY EBELN = GS_TAB-EBELN.
*        IF SY-SUBRC = 0.
**            GS_SF-PEINH = GS_EKKO_EKPO-PEINH.
**            GS_SF-NETPR = GS_EKKO_EKPO-NETPR.
*        ENDIF.
     "    GS_SF-MENGE = GS_TAB1-MENGE.

         GS_SF-MENGE =  GS_TAB1-MENGE_1.
*--------add by handyhl----2017.07.27----end------------------------
        GS_SF-MATNR = GS_TAB1-MATNR.

*        GS_SF-KBETR = GS_TAB1-KBETR.
*        znet = gs_tab1-znetpr.
*         SHIFT znet RIGHT DELETING TRAILING '0'.
*         GS_SF-NETPR = znet.
*             znet = gs_tab1-znetpr.
*        CONDENSE znet NO-GAPS.
*        SPLIT znet at '.' INTO znet_a znet_b.
*        SHIFT znet_b RIGHT DELETING TRAILING '0'.
*        CONDENSE znet_b.
*        CONCATENATE znet_a '.' znet_b INTO znet.
*        SHIFT znet RIGHT DELETING TRAILING '.'.
*        CONDENSE znet.
       " GS_SF-NETPR = gs_tab1-znetpr.
         GS_SF-NETPR = gs_tab1-KBETR.  "add by it02 20170811
*         GS_SF-NETPR = gs_tab1-znetpr.
*        GS_SF-TOTAL = GS_SF-MENGE * GS_SF-KBETR.
*--------add by handyhl----2017.07.27----begin----------------------
        GS_SF-TOTAL = GS_SF-MENGE * GS_SF-NETPR.
*--------add by handyhl----2017.07.27----end------------------------
        GV_SUM = GV_SUM + GS_SF-TOTAL.

        GS_SF-EINDT = GS_TAB1-EINDT.
        GS_SF-WAERS = GS_TAB1-WAERS.  "货币码
        APPEND GS_SF TO GT_SF.
        CLEAR GS_SF.

      ENDLOOP.

      ZMM038_TAB-ZSUM = GV_SUM.  "合计（单价的合计）
     "合计金额大写
       PERFORM conv_amount USING ZMM038_TAB-ZSUM
                   CHANGING ZMM038_TAB-ZSUMDX.
     CONCATENATE '大写：人民币' ZMM038_TAB-ZSUMDX INTO ZMM038_TAB-ZSUMDX.


**---------------------  补空行  ------------------------------
*      DATA_NUM = DATA_NUM MOD PAGE_NUM."最后页面行数
*      IF DATA_NUM <> 0.
*        DATA_NUM = PAGE_NUM - DATA_NUM."空行数
*        CLEAR GS_SF.
*        DO DATA_NUM TIMES.
*          ZNUM = ZNUM + 1.
*          GS_SF-ZNUM = ZNUM.
*          APPEND GS_SF TO GT_SF..
*        ENDDO.
*      ENDIF.

      CALL FUNCTION FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = OUTPUT
          ZMM038             = ZMM038_TAB
*          PAGE_NUM           = PAGE_NUM
        TABLES
          IT_TAB             = GT_SF
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.

      IF SY-SUBRC = 0.

      ENDIF.
ENDAT.
*---------  清空全局变量  ----------
    CLEAR GT_SF.
    CLEAR GV_SUM.
    CLEAR DATA_NUM.
    CLEAR ZNUM.

  ENDLOOP.

*---------------   关闭打印机设置  --------------------
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
  ENDIF.

*----------  清空打印相关声明  -----------
  CLEAR FM_NAME.
  CLEAR OUTPUT.
  CLEAR CONTROL_PARAMETERS.
  CLEAR LW_SSFCRESCL.
  CLEAR OPTION.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_PRINTPDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINTPDF .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'     "根据FORM名取的函数名
    EXPORTING
      FORMNAME = 'ZMM038'                      "FORM名
*     VARIANT  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME  = FM_NAME                      "返回函数名
*       EXCEPTIONS
*     NO_FORM  = 1
*     NO_FUNCTION_MODULE       = 2
*     OTHERS   = 3
    .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*--------------------  打印设置  ----------------------
  CONTROL_PARAMETERS-GETOTF   = 'X'.
  CONTROL_PARAMETERS-NO_DIALOG  = 'X'.   "跳过打印预览
  CONTROL_PARAMETERS-PREVIEW  = 'X'.     "跳过打印预览

  LOOP AT GT_TAB INTO GS_TAB WHERE BOX = 'X'.
    gt_tab1 = gt_tab.
    GS_TAB_Z-BUTXT = GS_TAB-BUTXT.
    GS_TAB_Z-EBELN = GS_TAB-EBELN.
    GS_TAB_Z-NAME1 = GS_TAB-NAME1.
    GS_TAB_Z-AEDAT = GS_TAB-AEDAT.
    GS_TAB_Z-STRAS = GS_TAB-STRAS.
    GS_TAB_Z-NAME2 = GS_TAB-NAME2.
    GS_TAB_Z-TELF1 = GS_TAB-TELF1.
    GS_TAB_Z-BANKA = GS_TAB-BANKA.
    GS_TAB_Z-KOINH = GS_TAB-KOINH.
    GS_TAB_Z-STRAS = GS_TAB-STRAS.
    GS_TAB_Z-STCEG = GS_TAB-STCEG.
    GS_TAB_Z-ZBNX = GS_TAB-ZBNX.
    GS_TAB_Z-TEXT1 = GS_TAB-TEXT1.
    GS_TAB_Z-YFCDF = GS_TAB-YFCDF.
    GS_TAB_Z-STCEG = GS_TAB-STCEG.
    GS_TAB_Z-TELF2 = GS_TAB-TELF2.
    GS_TAB_Z-TELFX = GS_TAB-TELFX.
    GS_TAB_Z-MWSKZ = GS_TAB-MWSKZ.
    GS_TAB_Z-EKNAM = GS_TAB-EKNAM.
    AT NEW EBELN.
*------------------  表头显示的数据  ----------------------
      ZMM038_TAB-BUTXT = GS_TAB_Z-BUTXT.
      ZMM038_TAB-EBELN = GS_TAB_Z-EBELN.
      ZMM038_TAB-NAME1 = GS_TAB_Z-NAME1.
      ZMM038_TAB-AEDAT = GS_TAB_Z-AEDAT.
      ZMM038_TAB-STRAS = GS_TAB_Z-STRAS.
      ZMM038_TAB-NAME2 = GS_TAB_Z-NAME2.
      ZMM038_TAB-TELF1 = GS_TAB_Z-TELF1.
      ZMM038_TAB-BANKA = GS_TAB_Z-BANKA.
      ZMM038_TAB-KOINH = GS_TAB_Z-KOINH.
      ZMM038_TAB-STRAS = GS_TAB_Z-STRAS.
      ZMM038_TAB-STCEG = GS_TAB_Z-STCEG.
      ZMM038_TAB-ZTXT2 = GS_ZTXT-ZTXT.
      ZMM038_TAB-ZBNX = GS_TAB_Z-ZBNX+11(2).
      ZMM038_TAB-TEXT1 = GS_TAB_Z-TEXT1.
      ZMM038_TAB-YFCDF = GS_TAB_Z-YFCDF.

      ZMM038_TAB-TELF2 = GS_TAB_Z-TELF2.   "乙方电话
      ZMM038_TAB-TELFX = GS_TAB_Z-TELFX.   "乙方传真

      ZMM038_TAB-EKNAM = GS_TAB_Z-EKNAM.

      IF LISTBOX1 = '1、在质保期内，除未按产品说明使用和不可抗力的原因外，对产品产品由于设计、工艺、'.
        ZMM038_TAB-LISTBOX = '在质保期内，除未按产品说明使用和不可抗力的原因外，对产品产品由于设计、工艺、材料、制造、安装的缺陷而发生的任何质量故障，甲方有权要求乙方在24小时内派专业人员到故障现场维修，免收维修工资和配件材料费。如质量问题严重需发回工厂维修而产生的国内运费由乙方承担，乙方应保证提供替代产品给甲方。如乙方未按甲方要求及时修复，甲方可委托他人维修，所需必要合理的维修费用由乙方承担。'.
      ELSEIF LISTBOX1 = '2、在质保期内，除未按产品说明使用和不可抗力的原因外，乙方按甲方要求的时间提供无'.
        ZMM038_TAB-LISTBOX = '在质保期内，除未按产品说明使用和不可抗力的原因外，乙方按甲方要求的时间提供无偿售后服务。如乙方未按甲方要求及时修复，甲方可委托他人维修，所需必要合理的维修费用由乙方承担。在质保期内，甲方工程项目有重大活动期间，乙方提供免费上门保驾护航服务，免费向甲方人员提供合同产品及配套产品的安装、操作、维护及保养方面必要的咨询服务直到甲方相关人员完全掌握相关技术。'.
      ELSEIF LISTBOX1 = '3、略'.
        ZMM038_TAB-LISTBOX = '略'.
      ELSE.
        ZMM038_TAB-LISTBOX = '略'.

      ENDIF.

*--------------------------  判断税号  ----------------------------------
      CASE GS_TAB_Z-MWSKZ.
        WHEN 'J1'.
          ZMM038_TAB-ZTXT1 = '17%税增值税专用发票'.
        WHEN 'J6'.
          ZMM038_TAB-ZTXT1 = '3%税增值税发票'.
        WHEN 'J3'.
          ZMM038_TAB-ZTXT1 = '11%税增值税专用发票'.
        WHEN 'J5'.
          ZMM038_TAB-ZTXT1 = '6%税增值税发票'.
      ENDCASE.

    ENDAT.
    CLEAR GS_TAB_Z.
*    zmm038_tab-LISTBOX = LISTBOX1.

    AT END OF EBELN.
      GV_SUM = 0.
     DELETE GT_TAB1 WHERE BOX NE 'X'.
*------------------------   表体循环显示的数据  ----------------------
      LOOP AT GT_TAB1 INTO GS_TAB1 WHERE EBELN = GS_TAB-EBELN
                                     AND BOX = 'X'.


        DATA_NUM = DATA_NUM + 1.
        ZNUM = ZNUM + 1.
        GS_SF-ZNUM = ZNUM.                "序号
        GS_SF-EXTWG = GS_TAB1-EXTWG.      " 品牌
        GS_SF-TXZ01  = GS_TAB1-TXZ01.
        GS_SF-MEINS = GS_TAB1-MEINS.
        GS_SF-MEINS_1 = GS_TAB1-MEINS_1. "单位 add by it02
*        GS_SF-MENGE = GS_TAB1-MENGE.
*       READ TABLE GT_EKKO_EKPO INTO GS_EKKO_EKPO WITH KEY EBELN = GS_TAB-EBELN.
*        IF SY-SUBRC = 0.
**            GS_SF-PEINH = GS_EKKO_EKPO-PEINH.
**            GS_SF-NETPR = GS_EKKO_EKPO-NETPR.
*        ENDIF.
*--------add by handyhl----2017.07.27----begin----------------------

     "   GS_SF-MENGE = GS_TAB1-MENGE.
      GS_SF-MENGE =  GS_TAB1-MENGE_1.
*--------add by handyhl----2017.07.27----end------------------------
*           znet = gs_tab1-znetpr.
*         SHIFT znet RIGHT DELETING TRAILING '0'.
*         GS_SF-NETPR = znet.
*        znet = gs_tab1-znetpr.
*        CONDENSE znet NO-GAPS.
*        SPLIT znet at '.' INTO znet_a znet_b.
*        SHIFT znet_b RIGHT DELETING TRAILING '0'.
*        CONDENSE znet_b.
*        CONCATENATE znet_a '.' znet_b INTO znet.
*        SHIFT znet RIGHT DELETING TRAILING '.'.
*        CONDENSE znet.
   "     GS_SF-NETPR = gs_tab1-znetpr.
        GS_SF-MATNR = GS_TAB1-MATNR.
        GS_SF-NETPR = gs_tab1-KBETR.  "add by it02 20170811
*        GS_SF-KBETR = GS_TAB1-KBETR.

*         GS_SF-NETPR = gs_tab1-znetpr.
*        GS_SF-TOTAL = GS_SF-MENGE * GS_SF-KBETR.
*--------add by handyhl----2017.07.27----begin----------------------
        GS_SF-TOTAL = GS_SF-MENGE * GS_SF-NETPR.
*--------add by handyhl----2017.07.27----end------------------------
        GV_SUM = GV_SUM + GS_SF-TOTAL.

        GS_SF-EINDT = GS_TAB1-EINDT.
          GS_SF-WAERS = GS_TAB1-WAERS.  "货币码
        APPEND GS_SF TO GT_SF.
        CLEAR GS_SF.


      ENDLOOP.

      ZMM038_TAB-ZSUM = GV_SUM.  "合计（单价的合计）
     "合计金额大写
       PERFORM conv_amount USING ZMM038_TAB-ZSUM
                   CHANGING ZMM038_TAB-ZSUMDX.
     CONCATENATE '大写：人民币' ZMM038_TAB-ZSUMDX INTO ZMM038_TAB-ZSUMDX.


**---------------------  补空行  ------------------------------
*      DATA_NUM = DATA_NUM MOD PAGE_NUM."最后页面行数
*      IF DATA_NUM <> 0.
*        DATA_NUM = PAGE_NUM - DATA_NUM."空行数
*        CLEAR GS_SF.
*        DO DATA_NUM TIMES.
*          ZNUM = ZNUM + 1.
*          GS_SF-ZNUM = ZNUM.
*          APPEND GS_SF TO GT_SF..
*        ENDDO.
*      ENDIF.

      CALL FUNCTION FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = OUTPUT
          ZMM038             = ZMM038_TAB
          PAGE_NUM           = PAGE_NUM
        IMPORTING
          JOB_OUTPUT_INFO    = OPTION
        TABLES
          IT_TAB             = GT_SF
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.

      IF SY-SUBRC = 0.

      ENDIF.
  ENDAT.
*--------  清空全局变量  ---------
    CLEAR GT_SF.
    CLEAR GV_SUM.
    CLEAR DATA_NUM.
    CLEAR ZNUM.

  ENDLOOP.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT        = 'PDF'
      MAX_LINEWIDTH = 132
*     ARCHIVE_INDEX = ' '
*     COPYNUMBER    = 0
*     ASCII_BIDI_VIS2LOG          = ' '
*     PDF_DELETE_OTFTAB           = ' '
*     PDF_USERNAME  = ' '
*     PDF_PREVIEW   = ' '
*     USE_CASCADING = ' '
    IMPORTING
      BIN_FILESIZE  = G_BINFILESIZE
*     BIN_FILE      =
    TABLES
      OTF           = OPTION-OTFDATA
      LINES         = GT_PDFTAB
* EXCEPTIONS
*     ERR_MAX_LINEWIDTH           = 1
*     ERR_FORMAT    = 2
*     ERR_CONV_NOT_POSSIBLE       = 3
*     ERR_BAD_OTF   = 4
*     OTHERS        = 5
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

*-------------------   将数据保存到本地  ------------------------
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      BIN_FILESIZE = G_BINFILESIZE
      FILENAME     = L_FULLPATH
      FILETYPE     = 'BIN'
*     APPEND       = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER       = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF     = 'X'
*     COL_SELECT   = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE     = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE     = ' '
*     IGNORE_CERR  = ABAP_TRUE
*     REPLACEMENT  = '#'
*     WRITE_BOM    = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT = ' '
*     WK1_N_SIZE   = ' '
*     WK1_T_FORMAT = ' '
*     WK1_T_SIZE   = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*     VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*     FILELENGTH   =
    TABLES
      DATA_TAB     = GT_PDFTAB
*     FIELDNAMES   =
* EXCEPTIONS
*     FILE_WRITE_ERROR                = 1
*     NO_BATCH     = 2
*     GUI_REFUSE_FILETRANSFER         = 3
*     INVALID_TYPE = 4
*     NO_AUTHORITY = 5
*     UNKNOWN_ERROR                   = 6
*     HEADER_NOT_ALLOWED              = 7
*     SEPARATOR_NOT_ALLOWED           = 8
*     FILESIZE_NOT_ALLOWED            = 9
*     HEADER_TOO_LONG                 = 10
*     DP_ERROR_CREATE                 = 11
*     DP_ERROR_SEND                   = 12
*     DP_ERROR_WRITE                  = 13
*     UNKNOWN_DP_ERROR                = 14
*     ACCESS_DENIED                   = 15
*     DP_OUT_OF_MEMORY                = 16
*     DISK_FULL    = 17
*     DP_TIMEOUT   = 18
*     FILE_NOT_FOUND                  = 19
*     DATAPROVIDER_EXCEPTION          = 20
*     CONTROL_FLUSH_ERROR             = 21
*     OTHERS       = 22
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

*----------  清空打印相关声明  -----------
  CLEAR FM_NAME.
  CLEAR OUTPUT.
  CLEAR CONTROL_PARAMETERS.
  CLEAR LW_SSFCRESCL.
  CLEAR OPTION.
  CLEAR GT_PDFTAB.
  CLEAR G_BINFILESIZE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FULLPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FULLPATH  text
*      <--P_L_PATH  text
*      <--P_L_NAME  text
*----------------------------------------------------------------------*
FORM FRM_GET_FULLPATH  CHANGING P_FULLPATH TYPE STRING
                                P_PATH TYPE STRING
                                P_NAME TYPE STRING.

  DATA: L_INIT_PATH  TYPE STRING,
        L_INIT_FNAME TYPE STRING,
        L_PATH       TYPE STRING,
        L_FILENAME   TYPE STRING,
        L_FULLPATH   TYPE STRING.

* ------  初始名称(输出的文件名称)  ------------
  L_INIT_FNAME = 'TEST.PDF'.

* ---------------------------  获取桌面路径  -------------------------------
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
    CHANGING
      DESKTOP_DIRECTORY    = L_INIT_PATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

* --------------------------   用户选择名称、路径  ----------------------------
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
*     window_title         = '指定保存文件名'
      DEFAULT_EXTENSION    = 'PDF'
      DEFAULT_FILE_NAME    = L_INIT_FNAME
      FILE_FILTER          = 'PDF文件（*.PDF）' "cl_gui_frontend_services=>filetype_all
      INITIAL_DIRECTORY    = L_INIT_PATH
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = L_FILENAME
      PATH                 = L_PATH
      FULLPATH             = L_FULLPATH
*     USER_ACTION          =
*     FILE_ENCODING        =
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC = 0.
    P_FULLPATH = L_FULLPATH.
    P_PATH     = L_PATH.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONV_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZMM038_TAB_ZSUM  text
*      <--P_ZMM038_TAB_ZSUMDX  text
*----------------------------------------------------------------------*
FORM conv_amount USING VALUE(f_source)
                 CHANGING VALUE(f_result).
  DATA: scr(30) TYPE c, res(60) TYPE c,fen(2) TYPE c .
  DATA: len TYPE i, c1 TYPE i, c2 TYPE i, c3 TYPE i, c4 TYPE i.
  DATA: d1(1) TYPE c, d2(1) TYPE c, d3 TYPE i.
  DATA: digit(2)  TYPE c, weight(2) TYPE c.
  DATA: rule1(20) TYPE c VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: rule2(30) TYPE c VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  scr = f_source * 100.
  CONDENSE scr NO-GAPS.
  IF scr = '0'.
    res = '零元'.
  ELSE.
    len = strlen( scr ).
    c1 = 0.
    d1 = '0'.
    CLEAR res.
    DO len TIMES.
      c1 = c1 + 1.
      c2 = len - c1.
      d2 = scr+c2(1) .
      IF d2 = '0'.
        d3 = 0.
      ELSE.
        d3 = d2.
      ENDIF.
      digit = rule1+d3(1) .
      c3 = ( c1 - 1 ) .
      weight = rule2+c3(1) .
      IF d2 = '0'.
        IF c1 = 3.
          digit = ''.
        ELSEIF c1 = 7.
          digit = ''.
          IF len > 10 .
            c4 = len - 10.
            IF scr+c4(4) = '0000'.
              weight = ''.
            ENDIF.
          ENDIF.
        ELSEIF c1 = 11.
          digit = ''.
        ELSEIF d1 = '0'.
          digit = ''.
          weight = ''.
        ELSE.
          weight = ''.
        ENDIF.
      ENDIF.
      CONCATENATE digit weight res INTO res .
      d1 = d2.
    ENDDO.
  ENDIF.
  len = strlen( res ) - 1.
  fen = res+len(1).
  IF fen <> '分' .
    CONCATENATE res '整' INTO f_result.
  ELSE.
    f_result = res.
  ENDIF.
ENDFORM.                    "conv_amount
*&---------------------------------------------------------------------*
*&      Form  GETWBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM getwbs .
*select posid post1 into table gt_wbs
*  from prps
*  where werks in ('1700','1710','1720').
*sort gt_wbs by posid.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'POSID' "大写,可选值内表的字段名
*     " value_org       = 'S' "就写'S'
*      dynpprog        = sy-repid "返回的输入框所在的main program
*      dynpnr          = sy-dynnr "返回的输入框所在屏幕
*      dynprofield     = 'S_WBS' "返回的输入框名
*      window_title    = 'WBS元素'
*    TABLES
*      value_tab       = gt_wbs "可选值的内表
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*ENDFORM.
