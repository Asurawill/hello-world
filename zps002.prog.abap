*----------------------------------------------------------------------*
*  程序名称         : ZPS002
*  创建者           : 吴丽娟
*  创建日期         : 2015-10-28
*----------------------------------------------------------------------*
*  概要说明
* ECP批导
*----------------------------------------------------------------------*
* modify  check 只允许一个项目导入   it02 20160229 add
"原中间删除0和100版本数据现更改为：先删除成功后，再重新创建0版本和100版本数据

REPORT ZPS002.

TYPES:ICON,SLIS.

TYPES:BEGIN OF TY_CH_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        HPEINH     TYPE MENGE_POS, "ckis-peinh,              "合同工程量
        HGPREIS    TYPE CKIS-GPREIS,             "合同单价
        JPEINH     TYPE MENGE_POS,              "计划工程量
        CGPREIS    TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS     TYPE CKIS-FPREIS,             "人工单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_CH_SOURCE.



TYPES:BEGIN OF TY_H_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        HPEINH     TYPE MENGE_POS,              "合同工程量
        HGPREIS    TYPE CKIS-GPREIS,             "合同单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_H_SOURCE.

TYPES:BEGIN OF TY_C_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        JPEINH     TYPE MENGE_POS, "CKIS-PEINH,    "计划工程量
        CGPREIS    TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS     TYPE CKIS-FPREIS,             "人工单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_C_SOURCE.

TYPES:BEGIN OF TY_CH_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        HPEINH       TYPE MENGE_POS,              "合同工程量
        HGPREIS      TYPE CKIS-GPREIS,             "合同单价
        JPEINH       TYPE MENGE_POS,              "计划工程量
        CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS       TYPE CKIS-FPREIS,             "人工单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_CH_OUTPUT.

TYPES:BEGIN OF TY_H_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        HPEINH       TYPE MENGE_POS,              "合同工程量
        HGPREIS      TYPE CKIS-GPREIS,             "合同单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_H_OUTPUT.

TYPES:BEGIN OF TY_C_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        JPEINH       TYPE MENGE_POS,              "计划工程量
        CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS       TYPE CKIS-FPREIS,             "人工单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_C_OUTPUT.
TYPES:BEGIN OF TY_CHZ_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        HPEINH     TYPE MENGE_POS,              "合同工程量
        HGPREIS    TYPE CKIS-GPREIS,             "合同单价
        JPEINH     TYPE MENGE_POS,              "计划工程量
        CGPREIS    TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS     TYPE CKIS-FPREIS,             "人工单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_CHZ_SOURCE.

TYPES:BEGIN OF TY_HZ_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        HPEINH     TYPE MENGE_POS,              "合同工程量
        HGPREIS    TYPE CKIS-GPREIS,             "合同单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_HZ_SOURCE.

TYPES:BEGIN OF TY_CZ_SOURCE,
        XH         TYPE INT4,                    "序号
        POSID      TYPE PRPS-POSID,              "WBS
        XMLB(1)    TYPE C,                       "项目类别
        WERKS      TYPE T001W-WERKS,             "工厂
        MATNR      TYPE CKIS-MATNR,              "物料编码
        MAKTX      TYPE MAKT-MAKTX,              "物料描述
        JPEINH     TYPE MENGE_POS,              "计划工程量
        CGPREIS    TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS     TYPE CKIS-FPREIS,             "人工单价
        CBYS       TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40) TYPE C,                       "成本要素名称
      END OF TY_CZ_SOURCE.

TYPES:BEGIN OF TY_CHZ_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        HPEINH       TYPE MENGE_POS,              "合同工程量
        HGPREIS      TYPE CKIS-GPREIS,             "合同单价
        JPEINH       TYPE MENGE_POS,              "计划工程量
        CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS       TYPE CKIS-FPREIS,             "人工单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_CHZ_OUTPUT.

TYPES:BEGIN OF TY_HZ_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        HPEINH       TYPE MENGE_POS,              "合同工程量
        HGPREIS      TYPE CKIS-GPREIS,             "合同单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_HZ_OUTPUT.

TYPES:BEGIN OF TY_CZ_OUTPUT,
        XH           TYPE INT4,                    "序号
        POSID        TYPE PRPS-POSID,              "WBS
        XMLB(1)      TYPE C,                       "项目类别
        WERKS        TYPE T001W-WERKS,             "工厂
        MATNR        TYPE CKIS-MATNR,              "物料编码
        MAKTX        TYPE MAKT-MAKTX,              "物料描述
        JPEINH       TYPE MENGE_POS,              "计划工程量
        CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
        FPREIS       TYPE CKIS-FPREIS,             "人工单价
        CBYS         TYPE CKIS-KSTAR,              "成本要素
        CBYSMC(40)   TYPE C,                       "成本要素名称
        POST1        TYPE PROJ-POST1,
        VERNR        TYPE PROJ-VERNR,
        VBUKR        TYPE PROJ-VBUKR,
        KALID        TYPE PROJ-KALID,
        ZTEHT        TYPE PROJ-ZTEHT,
        TYPE(10)     TYPE C,                       "状态
        MESSAGE(400) TYPE C,                       "消息
      END OF TY_CZ_OUTPUT.
TYPES:BEGIN OF TY_VERSN,
        VERSN TYPE PRECP1-VERSN,
      END OF TY_VERSN.
TYPES:BEGIN OF TY_OBJID,
        TOPNR TYPE PRECP1-TOPNR,
        VERSN TYPE PRECP1-VERSN,
      END OF TY_OBJID.
TYPES:BEGIN OF TY_MEINS,
*&--代码添加 BY HANDYBY 28.04.2017 10:47:35  BEGIN

        MATNR TYPE MARA-MATNR,
*
*&--代码添加 BY HANDYBY 28.04.2017 10:47:35  END
        MEINS TYPE MARA-MEINS,
      END OF TY_MEINS.
TYPES:BEGIN OF TY_PSPHI,
        PSPHI TYPE PRPS-PSPHI,
      END OF TY_PSPHI.
TYPES:BEGIN OF TY_OBJNR,
        OBJNR TYPE PROJ-OBJNR,
      END OF TY_OBJNR.
TYPES:BEGIN OF TY_KALNR,
        SUBNR TYPE J_OBJNR,
        VERSN TYPE VERSN,
        KALNR TYPE PRECP2-KALNR,
        TOPNR TYPE J_OBJNR,
      END OF TY_KALNR.
TYPES:BEGIN OF TY_HTGCL,
        MATNR  TYPE CKIS-MATNR,              "物料编码
        HPEINH TYPE MENGE_POS,              "合同工程量
      END OF TY_HTGCL.
TYPES:BEGIN OF TY_JHGCL,
        MATNR  TYPE CKIS-MATNR,              "物料编码
        JPEINH TYPE MENGE_POS,              "计划工程量
      END OF TY_JHGCL.
TYPES:BEGIN OF TY_HB,
        MATNR  TYPE CKIS-MATNR,              "物料编码
        HPEINH TYPE MENGE_POS,              "合同工程量
        JPEINH TYPE MENGE_POS,              "计划工程量
      END OF TY_HB.
TYPES:BEGIN OF TY_JG,
        MATNR TYPE CKIS-MATNR,
        HTDJ  TYPE CKIS-GPREIS,
        CLDJ  TYPE P LENGTH 16 DECIMALS 4,
        RGDJ  TYPE CKIS-FPREIS,
      END OF TY_JG.
TYPES:BEGIN OF TY_JGHJ,
        MATNR   TYPE CKIS-MATNR,
        HGPREIS TYPE CKIS-GPREIS,
        CGPREIS TYPE P LENGTH 16 DECIMALS 4,
        FPREIS  TYPE CKIS-FPREIS,
      END OF TY_JGHJ.
TYPES:BEGIN OF TY_JG1,
        MATNR TYPE CKIS-MATNR,
        CLDJ  TYPE P LENGTH 16 DECIMALS 4,
        RGDJ  TYPE CKIS-FPREIS,
      END OF TY_JG1.
TYPES:BEGIN OF TY_JGHJ1,
        MATNR   TYPE CKIS-MATNR,
        CGPREIS TYPE P LENGTH 16 DECIMALS 4,
        FPREIS  TYPE CKIS-FPREIS,
      END OF TY_JGHJ1.
TYPES:BEGIN OF TY_JG2,
        MATNR TYPE CKIS-MATNR,
        HTDJ  TYPE CKIS-GPREIS,
        RGDJ  TYPE CKIS-FPREIS,
      END OF TY_JG2.
TYPES:BEGIN OF TY_JGHJ2,
        MATNR   TYPE CKIS-MATNR,
        HGPREIS TYPE CKIS-GPREIS,
        FPREIS  TYPE CKIS-FPREIS,
      END OF TY_JGHJ2.
TYPES:BEGIN OF TY_CHECK,
        POSID   TYPE PRPS-POSID,              "WBS
        XMLB(1) TYPE C,                       "项目类别
        CBYS    TYPE CKIS-KSTAR,              "成本要素
*&--代码添加 BY HANDYBY 22.05.2017 23:31:26  BEGIN
        WERKS   TYPE PRPS-WERKS,              "工厂
*&--代码添加 BY HANDYBY 22.05.2017 23:31:26  END
      END OF TY_CHECK.

TYPES:BEGIN OF TY_CHECK_MATNR  ,
        MATNR TYPE MATNR,
        MEINS TYPE MEINS,
      END OF TY_CHECK_MATNR .

DATA:IT_JG TYPE TABLE OF TY_JG,
     WA_JG TYPE TY_JG.
DATA:IT_JG1 TYPE TABLE OF TY_JG1,
     WA_JG1 TYPE TY_JG1.
DATA:IT_JG2 TYPE TABLE OF TY_JG2,
     WA_JG2 TYPE TY_JG2.

DATA:IT_JGHJ TYPE TABLE OF TY_JGHJ,
     WA_JGHJ TYPE TY_JGHJ.
DATA:IT_JGHJ1 TYPE TABLE OF TY_JGHJ1,
     WA_JGHJ1 TYPE TY_JGHJ1.
DATA:IT_JGHJ2 TYPE TABLE OF TY_JGHJ2,
     WA_JGHJ2 TYPE TY_JGHJ2.

DATA:GT_HTGCL TYPE TABLE OF TY_HTGCL,
     GS_HTGCL TYPE TY_HTGCL.

DATA:GT_JHGCL TYPE TABLE OF TY_JHGCL,
     GS_JHGCL TYPE TY_JHGCL.

DATA:GT_HB TYPE TABLE OF TY_HB,
     GS_HB TYPE TY_HB.

DATA:GT_CHECK_MATNR TYPE TABLE OF TY_CHECK_MATNR,
     GS_CHECK_MATNR TYPE TY_CHECK_MATNR.

DATA:g(50)  TYPE C,
     w(50)  TYPE C,
     LL(50) TYPE C,
     JJ(50) TYPE C.
DATA:MESSAGE1(200) TYPE C,
     MESSAGE2(200) TYPE C,
     MESSAGE3(200) TYPE C.
DATA:  GT_CH_SOURCE      TYPE TABLE OF TY_CH_SOURCE WITH HEADER LINE.
DATA:GT_H_SOURCE       TYPE TABLE OF TY_H_SOURCE WITH HEADER LINE.
DATA:GT_C_SOURCE       TYPE TABLE OF TY_C_SOURCE WITH HEADER LINE.
DATA:GT_CH_OUTPUT      TYPE TABLE OF TY_CH_OUTPUT WITH HEADER LINE.
DATA:GT_CH_OUTPUT1     TYPE TABLE OF TY_CH_OUTPUT WITH HEADER LINE.
DATA:GT_H_OUTPUT       TYPE TABLE OF TY_H_OUTPUT WITH HEADER LINE.
DATA:GT_H_OUTPUT1      TYPE TABLE OF TY_H_OUTPUT WITH HEADER LINE.
DATA:GT_C_OUTPUT       TYPE TABLE OF TY_C_OUTPUT WITH HEADER LINE.
DATA:GT_C_OUTPUT1      TYPE TABLE OF TY_C_OUTPUT WITH HEADER LINE.
DATA:GT_CHZ_SOURCE     TYPE TABLE OF TY_CH_SOURCE WITH HEADER LINE.
DATA:GT_HZ_SOURCE      TYPE TABLE OF TY_H_SOURCE WITH HEADER LINE.
DATA:GT_CZ_SOURCE      TYPE TABLE OF TY_C_SOURCE WITH HEADER LINE.
DATA:GT_CHZ_OUTPUT     TYPE TABLE OF TY_CH_OUTPUT WITH HEADER LINE.
DATA:GS_CHZ_OUTPUT     TYPE TY_CH_OUTPUT .
DATA:GT_CHZ_OUTPUT1    TYPE TABLE OF TY_CH_OUTPUT WITH HEADER LINE.
DATA:GT_HZ_OUTPUT      TYPE TABLE OF TY_H_OUTPUT WITH HEADER LINE.
DATA:GS_HZ_OUTPUT      TYPE TY_H_OUTPUT.
DATA:GT_HZ_OUTPUT1     TYPE TABLE OF TY_H_OUTPUT WITH HEADER LINE.
DATA:GT_CZ_OUTPUT      TYPE TABLE OF TY_C_OUTPUT WITH HEADER LINE.
DATA:GT_CZ_OUTPUT1     TYPE TABLE OF TY_C_OUTPUT WITH HEADER LINE.
DATA:GT_RETURN         TYPE TABLE OF BAPIRET2.                            "声明返回消息的内表和工作区
DATA:GT_RETURN1        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
DATA:GT_RETURN2        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
DATA:GT_RETURN3        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
DATA:GT_RETURN4        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
DATA:GT_RETURN5        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
DATA:GS_RETURN         TYPE BAPIRET2.
DATA:GS_RETURN1        TYPE BAPIRET2.
DATA:GS_RETURN2        TYPE BAPIRET2.
DATA:GS_RETURN3        TYPE BAPIRET2.
DATA:GS_RETURN4        TYPE BAPIRET2.
DATA:GS_RETURN5        TYPE BAPIRET2.
DATA:P_OBJID(20)       TYPE C.
DATA:LS_ITEM           TYPE BAPICKECP_ITEM.                               "定义BAPI中用到的结构和内表
DATA:LS_ITEM1          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_ITEM2          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_ITEM3          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_ITEM4          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_ITEM5          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_ITEM6          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
DATA:LS_WBS_COSTLINES  TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES1 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES2 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES3 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES4 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES5 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LS_WBS_COSTLINES6 TYPE PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES  TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES1 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES2 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES3 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES4 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES5 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:LT_WBS_COSTLINES6 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
DATA:GT_VERSN1         TYPE TABLE OF TY_VERSN WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:GT_VERSN2         TYPE TABLE OF TY_VERSN WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:GT_VERSN3         TYPE TABLE OF TY_VERSN WITH HEADER LINE.           "定义放置版本号的内表和工作区
DATA:GT_OBJID          TYPE TABLE OF TY_OBJID WITH HEADER LINE.
DATA:GT_OBJID1         TYPE TABLE OF TY_OBJID WITH HEADER LINE.
DATA:GT_OBJID2         TYPE TABLE OF TY_OBJID WITH HEADER LINE.
DATA:GT_MEINS          TYPE TABLE OF TY_MEINS WITH HEADER LINE.
DATA:GT_PSPHI          TYPE TABLE OF TY_PSPHI WITH HEADER LINE.
DATA:IT_BDCDATA TYPE STANDARD TABLE OF BDCDATA.
DATA:WA_BDCDATA TYPE BDCDATA.
DATA:IT_MESSAGE TYPE STANDARD TABLE OF BDCMSGCOLL.
DATA:WA_MESSAGE TYPE BDCMSGCOLL.
DATA:IT_PROJ TYPE TABLE OF PROJ.
DATA:WA_PROJ TYPE PROJ.
DATA:IT_KALNR TYPE TABLE OF TY_KALNR.
DATA:IT_KALNR1 TYPE TABLE OF TY_KALNR.
DATA:WA_KALNR TYPE TY_KALNR.
DATA:WA_KALNR2 TYPE TY_KALNR.
DATA:IT_OBJNR TYPE TABLE OF TY_OBJNR.
DATA:WA_OBJNR TYPE TY_OBJNR.
DATA:IT_CKIS TYPE TABLE OF CKIS.
DATA:IT_CKIS1 TYPE TABLE OF CKIS.
DATA:WA_CKIS TYPE CKIS.
DATA:WA_CKIS1 TYPE CKIS.
DATA:JS TYPE INT4 VALUE '0'.
DATA:l(50) TYPE C.
DATA:j(50) TYPE C.

DATA:SC_LEN TYPE I.      "成功导入预算行数
DATA:SC_IMG TYPE STRING ."成功执行预算消息
DATA:L1 TYPE I,       "0版本导入数量
     L2 TYPE I,       "100版本导入数量
     L3 TYPE I,       "0版本导入最后数量
     L4 TYPE I.       "100版本导入最后数量
"DATA:gt_c_output_check       TYPE TABLE OF ty_c_output WITH HEADER LINE.
DATA:PROJ_OBJNR TYPE J_OBJNR , "项目对象号
     PRPS_OBJNR TYPE J_OBJNR . "WBS对象号

DATA:MSG000(50) TYPE C ,   "000版本消息通知
     MSG001(50) TYPE  C,    "000后最新版本消息通知
     MSG100(50) TYPE  C,    "100版本消息通知
     MSG101(50) TYPE  C,   "100后最新版本消息通知
     ERMSG      TYPE STRING.  "错误消息

*&--代码添加 BY HANDYBY 14.08.2017 23:15:22  BEGIN
DATA ZSTR(1000) TYPE C .
DATA: ZTEXT(100)  TYPE C,
      ZTEXT2(100) TYPE C,
      ZTEXT3(100) TYPE C,
      ZTEXT4(100) TYPE C.
*&--代码添加 BY HANDYBY 14.08.2017 23:15:22  END

DATA:E1_MSG TYPE STRING,
     E2_MSG TYPE STRING.

DATA:GT_C_OUTPUT_CHECK TYPE TABLE OF TY_CHECK WITH HEADER LINE.
"    gs_c_output_check TYPE TY_CHECK.



DATA: OK_CODE TYPE SY-UCOMM,
      SAVE_OK TYPE SY-UCOMM.

DATA:GT_FIELDCAT       TYPE LVC_T_FCAT.                                   "ALV定义
DATA:GS_FIELDCAT       TYPE LVC_S_FCAT.
DATA:GS_LAYOUT         TYPE LVC_S_LAYO.

SELECTION-SCREEN PUSHBUTTON 2(13) BUT1 USER-COMMAND CMD2 MODIF ID 11.    "模板下载
PARAMETERS:P_UPC   TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X',               "上传成本预算
           P_UPH   TYPE C RADIOBUTTON GROUP G1,                           "上传合同报价
           P_UPCH  TYPE C RADIOBUTTON GROUP G1,                           "上传成本预算和合同报价
           P_UPCZ  TYPE C RADIOBUTTON GROUP G1,                           "上传成本预算
           P_UPHZ  TYPE C RADIOBUTTON GROUP G1,                           "上传合同报价
           P_UPCHZ TYPE C RADIOBUTTON GROUP G1,                           "上传成本预算和合同报价
           P_PATH  LIKE RLGRAP-FILENAME MEMORY ID FIL MODIF ID 11.        "上传文件路径

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM FRM_GET_FILENAME.                                              "选择文件，获取文件名，返回文件名

INITIALIZATION.
  CALL FUNCTION 'ICON_CREATE'                                            "调用模板下载按钮的图片
    EXPORTING
      NAME   = ICON_EXPORT
      TEXT   = '模板下载'
    IMPORTING
      RESULT = BUT1
    EXCEPTIONS
      OTHERS = 0.

AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CMD2'.                                                         "模板下载
      PERFORM DOWNLOAD_TPT USING TEXT-001
                                 P_OBJID
                           CHANGING P_PATH.
    WHEN OTHERS.
  ENDCASE.

START-OF-SELECTION.
  IF P_PATH IS NOT INITIAL.
    PERFORM FRM_UPLOAD.     "导入文件
    " add it02 20160229-有预算值导入成功消息提示 begin
*    IF SC_LEN >= 1 .
*      READ TABLE gt_c_output_check   index 1.
*      IF SY-SUBRC EQ 0 .
*        CLEAR:SC_IMG.
*        SC_IMG = SC_LEN.
*        CONCATENATE '项目:' gt_c_output_check-posid '已成功导入' SC_IMG '条预算行数' INTO SC_IMG .
*        MESSAGE SC_IMG  TYPE 'I'.
*       ENDIF.
*     ENDIF.
    " add it02 20160229-有预算值导入成功消息提示 end
    PERFORM FRM_FIELDCAT.                                                  "字段名
    PERFORM FRM_LAYOUT.                                                    "布局
    IF P_UPC = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_C_OUTPUT.                           "成本预算的ALV报表显示
    ELSEIF P_UPH = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_H_OUTPUT.                          "合同报价的ALV报表显示
    ELSEIF P_UPCH = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_CH_OUTPUT.                         "成本预算和合同报价的ALV报表显示
    ELSEIF P_UPCZ = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_CZ_OUTPUT.                           "成本预算的ALV报表显示
    ELSEIF P_UPHZ = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_HZ_OUTPUT.                          "合同报价的ALV报表显示
    ELSEIF P_UPCHZ = 'X'.
      PERFORM FRM_ALV_REPORT TABLES GT_CHZ_OUTPUT.                         "成本预算和合同报价的ALV报表显示
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_FILENAME .                                                  "获取上传文件的路径
  DATA: LW_RC    TYPE I,
        LW_USER  TYPE I,
        LIT_FILE TYPE FILETABLE,
        LIW_FILE TYPE FILE_TABLE.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = '选择文件'
      FILE_FILTER             = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
    CHANGING
      FILE_TABLE              = LIT_FILE
      RC                      = LW_RC
      USER_ACTION             = LW_USER
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    CHECK LW_RC = 1
      AND LW_USER <> 9.
    READ TABLE LIT_FILE INTO LIW_FILE INDEX 1.
    P_PATH = LIW_FILE-FILENAME.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_001  text
*      -->P_P_OBJID  text
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM DOWNLOAD_TPT   USING    P_TEXT
                   VALUE(P_OBJID)
                   CHANGING PV_FILE.
  IF P_UPC = 'X'.                                                        "下载成本预算批导模板
    P_OBJID = 'ZUPC'.
  ELSEIF P_UPH = 'X'.                                                    "下载合同报价金额批导模板
    P_OBJID = 'ZUPH'.
  ELSEIF P_UPCH = 'X'.                                                   "下载成本预算和合同报价批导模板
    P_OBJID = 'ZUPCH'.
  ELSEIF P_UPCZ = 'X'.                                                        "下载成本预算批导模板
    P_OBJID = 'ZUPCZ'.
  ELSEIF P_UPHZ = 'X'.                                                    "下载合同报价金额批导模板
    P_OBJID = 'ZUPHZ'.
  ELSEIF P_UPCHZ = 'X'.                                                   "下载成本预算和合同报价批导模板
    P_OBJID = 'ZUPCHZ'.
  ENDIF.

  DATA: LV_FNAME TYPE STRING,
        LV_TITLE TYPE STRING,
        LV_PATH  TYPE STRING VALUE 'D:/',
        LV_FPATH TYPE STRING VALUE 'D:/'.

  DATA: LS_WDATB   LIKE WWWDATATAB.
  DATA: LV_SUBRC   TYPE SY-SUBRC.
  DATA: GV_MSG TYPE STRING .

  LV_FNAME = P_TEXT.

  CONCATENATE P_TEXT '下载' INTO LV_TITLE.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = LV_TITLE
      DEFAULT_EXTENSION    = 'XLS'
      DEFAULT_FILE_NAME    = LV_FNAME
      INITIAL_DIRECTORY    = 'D:\'
      FILE_FILTER          = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = LV_FNAME
      PATH                 = LV_PATH
      FULLPATH             = LV_FPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    SELECT SINGLE RELID
                  OBJID
      FROM WWWDATA
      INTO CORRESPONDING FIELDS OF LS_WDATB
      WHERE SRTF2 = 0
      AND RELID = 'MI'
      AND OBJID = P_OBJID.                        "p_objid就是传入模板的参数
    IF LS_WDATB IS INITIAL.
      MESSAGE '模板文件不存在！' TYPE 'E'.
    ELSE.
      PV_FILE = LV_FPATH.
      IF PV_FILE IS NOT INITIAL.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            KEY         = LS_WDATB
            DESTINATION = PV_FILE
          IMPORTING
            RC          = LV_SUBRC.
        IF LV_SUBRC NE 0.
          MESSAGE '模板下载失败！' TYPE 'E'.
        ELSE.
          CLEAR GV_MSG.
          CONCATENATE '模板下载到本地文件' PV_FILE INTO GV_MSG.
          MESSAGE GV_MSG TYPE 'S' .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_UPLOAD .
  "取出相应的1800 工厂下的物料新

*&--代码添加 BY HANDYBY 22.05.2017 22:23:59  BEGIN
*  SELECT A~MATNR B~MEINS
*    INTO CORRESPONDING FIELDS OF TABLE GT_CHECK_MATNR
*    FROM MARC AS A
*    INNER JOIN MARA AS B
*    ON A~MATNR = B~MATNR
*    WHERE A~WERKS = '1800'.
*&--代码添加 BY HANDYBY 22.05.2017 22:23:59  END
*&--代码添加 BY HANDYBY 22.05.2017 22:24:23  BEGIN
  SELECT A~MATNR B~MEINS
  INTO CORRESPONDING FIELDS OF TABLE GT_CHECK_MATNR
  FROM MARC AS A
  INNER JOIN MARA AS B
  ON A~MATNR = B~MATNR .
*&--代码添加 BY HANDYBY 22.05.2017 22:24:23  END

  SORT GT_CHECK_MATNR BY MATNR .
  IF P_UPC = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_C_SOURCE.                           "成本预算的导入
    PERFORM FRM_BAPI_GT_C_OUTPUT.                                        "成本预算的BAPI
  ELSEIF P_UPH = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_H_SOURCE.                          "合同报价的导入
    PERFORM FRM_BAPI_GT_H_OUTPUT.                                       "合同报价的BAPI
  ELSEIF P_UPCH = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_CH_SOURCE.                         "成本预算和合同报价的导入
    PERFORM FRM_BAPI_GT_CH_OUTPUT.                                      "成本预算和合同报价的BAPI
  ELSEIF P_UPCZ = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_CZ_SOURCE.                           "成本预算的导入
    PERFORM FRM_BAPI_GT_CZ_OUTPUT.                                        "成本预算的BAPI
  ELSEIF P_UPHZ = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_HZ_SOURCE.                          "合同报价的导入
    PERFORM FRM_BAPI_GT_HZ_OUTPUT.                                       "合同报价的BAPI
  ELSEIF P_UPCHZ = 'X'.
    PERFORM FRM_XLS_TO_SAP TABLES GT_CHZ_SOURCE.                         "成本预算和合同报价的导入
    PERFORM FRM_BAPI_GT_CHZ_OUTPUT.                                      "成本预算和合同报价的BAPI
  ENDIF.
  "统计最后版本导入数量

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT .
  IF P_UPC = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF P_UPH = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF P_UPCH = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF P_UPCZ = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF P_UPHZ = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ELSEIF P_UPCHZ = 'X'.
    PERFORM FRM_FIELD_CAT USING:
            'XH' '' '' '序号',
            'TYPE' '' '' '消息类型',
            'MESSAGE' '' '' '消息内容',
            'POSID' 'PRPS' 'POSID' 'WBS',
            'XMLB' '' '' '项目类别',
            'WERKS' 'T001W' 'WERKS' '工厂',
            'MATNR' 'CKIS' 'MATNR' '物料编码',
            'MAKTX' 'MAKT' 'MAKTX' '物料描述',
            'HPEINH' 'CKIS' 'PEINH' '合同工程量',
            'HGPREIS' 'CKIS' 'GPREIS' '合同单价',
            'JPEINH' 'CKIS' 'PEINH' '计划工程量',
            'CGPREIS' 'CKIS' 'GPREIS' '材料单价',
            'FPREIS' 'CKIS' 'FPREIS' '人工单价',
            'CBYS' 'CKIS' 'CBYS' '成本要素',
            'CBYSMC' '' '' '成本要素名称'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_LAYOUT .
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-ZEBRA = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_C_SOURCE  text
*----------------------------------------------------------------------*
FORM FRM_ALV_REPORT TABLES T_GT_OUTPUT.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'STANDARD_FULLSCREEN '
*     I_CALLBACK_USER_COMMAND  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
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
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = T_GT_OUTPUT
* EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_C_SOURCE  text
*----------------------------------------------------------------------*
FORM FRM_XLS_TO_SAP TABLES P_GS_SOURCE.

  DATA:DR_LEN TYPE I VALUE IS INITIAL .
  REFRESH GT_C_OUTPUT_CHECK[].
  REFRESH P_GS_SOURCE[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_PATH
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = P_GS_SOURCE[]
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN  '1'.
        MESSAGE '存在错误的传入参数' TYPE 'E'.
      WHEN  '2'.
        MESSAGE 'OLE控件错误，请检查EXCEL插件及系统' TYPE 'E'.
      WHEN  '3'.
        MESSAGE '数据上载出错' TYPE 'E'.
      WHEN OTHERS.
    ENDCASE.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH:GT_C_OUTPUT,GT_H_OUTPUT,GT_CH_OUTPUT,
          GT_CZ_OUTPUT,GT_HZ_OUTPUT,GT_CHZ_OUTPUT.
  "add it02 begin check 导入模板几个项目

*&--代码添加 BY HANDYBY 23.05.2017 00:15:42  BEGIN
********************************************************************** 数据校验
*&--代码添加 BY HANDYBY 23.05.2017 00:15:42  END

*&--代码添加 BY HANDYBY 23.05.2017 09:15:48  BEGIN
  IF P_GS_SOURCE[] IS INITIAL .
    MESSAGE 'EXCEL没数据！' TYPE 'E' .
    EXIT .
  ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 09:15:48  END

*&--代码添加 BY HANDYBY 05.06.2017 22:00:40  BEGIN
  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
  LOOP AT GT_C_OUTPUT_CHECK.
    IF GT_C_OUTPUT_CHECK-POSID IS INITIAL .
      MESSAGE 'WBS元素不能为空！' TYPE 'E' DISPLAY LIKE 'S' .
    ENDIF.
  ENDLOOP.
  REFRESH GT_C_OUTPUT_CHECK[] .
  CLEAR GT_C_OUTPUT_CHECK .
*&--代码添加 BY HANDYBY 05.06.2017 22:00:40  END

*&--代码添加 BY HANDYBY 23.05.2017 00:16:56  BEGIN
  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
  SORT GT_C_OUTPUT_CHECK BY POSID .
  DELETE ADJACENT DUPLICATES FROM GT_C_OUTPUT_CHECK COMPARING POSID .
  DESCRIBE TABLE GT_C_OUTPUT_CHECK LINES DR_LEN .
  DATA:BEGIN OF LS_PRJ,
         PSPNR1 TYPE PRPS-PSPNR,
         POSID  TYPE PRPS-POSID,
         PSPHI  TYPE PRPS-PSPHI,
         PSPNR2 TYPE PROJ-PSPNR,
         PSPID  TYPE PROJ-PSPID,
       END OF LS_PRJ.
  DATA LT_PRJ LIKE TABLE OF LS_PRJ .

  IF DR_LEN <> 1 .
    SELECT R~PSPNR AS PSPNR1
           R~POSID
           R~PSPHI
           J~PSPNR AS PSPNR2
           J~PSPID
      INTO CORRESPONDING FIELDS OF TABLE LT_PRJ
      FROM PRPS AS R
     INNER JOIN PROJ AS J
        ON R~PSPHI = J~PSPNR
       FOR ALL ENTRIES IN GT_C_OUTPUT_CHECK
     WHERE R~POSID = GT_C_OUTPUT_CHECK-POSID .
    IF LT_PRJ IS INITIAL .
      MESSAGE '导入的数据没有对应的项目定义！' TYPE 'E'.
    ENDIF.
    SORT LT_PRJ BY PSPNR2 .
    DELETE ADJACENT DUPLICATES FROM LT_PRJ COMPARING PSPNR2 .
    CLEAR DR_LEN .
    DESCRIBE TABLE LT_PRJ LINES DR_LEN .
    IF DR_LEN > 1 .
      MESSAGE '一次只可以导入一个项目的预算！' TYPE 'E'.
      EXIT .
    ENDIF.
  ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 00:16:56  END

*&--代码添加 BY HANDYBY 22.05.2017 23:07:29  BEGIN
*  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
*  SORT GT_C_OUTPUT_CHECK BY POSID .
*  DELETE ADJACENT DUPLICATES FROM GT_C_OUTPUT_CHECK  COMPARING POSID.
*  DESCRIBE TABLE GT_C_OUTPUT_CHECK LINES DR_LEN .
*  IF DR_LEN > 1 .
*    MESSAGE '只允许导入一个项目的预算值，请检查！' TYPE 'E' .
*  ENDIF.
*&--代码添加 BY HANDYBY 22.05.2017 23:07:29  END
*&--代码添加 BY HANDYBY 22.05.2017 23:11:08  BEGIN
  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
  DELETE GT_C_OUTPUT_CHECK WHERE XMLB = 'V'.
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_C_OUTPUT_CHECK[]
   WHERE POSID = GT_C_OUTPUT_CHECK-POSID .
  SORT LT_PRPS BY POSID .
  LOOP AT GT_C_OUTPUT_CHECK .
    READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = GT_C_OUTPUT_CHECK-WERKS BINARY SEARCH .
    IF SY-SUBRC = 0 .
      IF LS_PRPS-WERKS <> GT_C_OUTPUT_CHECK-WERKS .
        MESSAGE '物料和项目不为同一公司，请检查！' TYPE 'E' .
        EXIT.
      ENDIF.
      CLEAR LS_PRPS .
    ENDIF.
  ENDLOOP.
  REFRESH GT_C_OUTPUT_CHECK[] .
  CLEAR GT_C_OUTPUT_CHECK .
  REFRESH LT_PRPS .
*&--代码添加 BY HANDYBY 22.05.2017 23:11:08  END

*&--代码注释 BY HANDYBY 22.05.2017 23:58:56  BEGIN
*  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
*  DELETE GT_C_OUTPUT_CHECK WHERE XMLB = 'M'.
*  SORT GT_C_OUTPUT_CHECK BY CBYS .
*  CLEAR :DR_LEN .
*  LOOP AT GT_C_OUTPUT_CHECK WHERE XMLB = 'V'.
*    CLEAR:DR_LEN .
*    LOOP AT GT_C_OUTPUT_CHECK WHERE CBYS = GT_C_OUTPUT_CHECK-CBYS .
*      DR_LEN = DR_LEN + 1 .
*
*    ENDLOOP.
*    IF DR_LEN > 1.
*      MESSAGE 'V类型只允许导入一个成本要素的预算值或模板错误，请检查！' TYPE 'E' .
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*&--代码注释 BY HANDYBY 22.05.2017 23:58:56  END
*&--代码添加 BY HANDYBY 22.05.2017 23:55:37  BEGIN
  MOVE-CORRESPONDING P_GS_SOURCE[] TO GT_C_OUTPUT_CHECK[] .
  DELETE GT_C_OUTPUT_CHECK WHERE XMLB = 'M'.
  SORT GT_C_OUTPUT_CHECK BY CBYS .
  DATA LS_C_OUTPUT_CHECK TYPE TY_CHECK .

  LOOP AT GT_C_OUTPUT_CHECK INTO LS_C_OUTPUT_CHECK WHERE XMLB = 'V'.
    CLEAR:DR_LEN .
    LOOP AT GT_C_OUTPUT_CHECK WHERE CBYS = GT_C_OUTPUT_CHECK-CBYS .
      IF DR_LEN = 0.
        DR_LEN = DR_LEN + 1 .
      ELSE.
        IF GT_C_OUTPUT_CHECK-POSID = LS_C_OUTPUT_CHECK-POSID .
          MESSAGE '同一个WBS元素下只能出现一个成本要素！' TYPE 'E' .
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR LS_C_OUTPUT_CHECK .
  ENDLOOP.
*&--代码添加 BY HANDYBY 22.05.2017 23:55:37  END

*&--代码添加 BY HANDYBY 23.05.2017 00:16:01  BEGIN
********************************************************************** 数据校验
*&--代码添加 BY HANDYBY 23.05.2017 00:16:01  END

  DATA: BEGIN OF LS_PRPS2,
          PSPNR TYPE PRPS-PSPNR,
          POSID TYPE PRPS-POSID,
          PSPHI TYPE PRPS-PSPHI,
        END OF LS_PRPS2.
  DATA LT_PRPS2 LIKE TABLE OF LS_PRPS2 .

  " add it02 20160229 end
  IF P_UPC = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_C_OUTPUT.
      GT_C_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_C_OUTPUT.
    ENDLOOP.

*&--代码注释 BY HANDYBY 02.06.2017 14:59:13  BEGIN
*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_C_OUTPUT
*    WHERE PSPID = GT_C_OUTPUT-POSID.
*&--代码注释 BY HANDYBY 02.06.2017 14:59:13  END

*&--代码添加 BY HANDYBY 02.06.2017 14:17:07  BEGIN

    SELECT PSPNR
           POSID
           PSPHI
      INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
      FROM PRPS
       FOR ALL ENTRIES IN GT_C_OUTPUT[]
     WHERE POSID = GT_C_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.
*&--代码添加 BY HANDYBY 02.06.2017 14:17:07  END

    LOOP AT GT_C_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_C_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_C_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_C_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_C_OUTPUT-KALID = WA_PROJ-KALID.
      GT_C_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_C_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_C_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF P_UPH = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_H_OUTPUT.
      GT_H_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_H_OUTPUT.
    ENDLOOP.

*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_H_OUTPUT
*    WHERE PSPID = GT_H_OUTPUT-POSID.

    SELECT PSPNR
               POSID
               PSPHI
          INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
          FROM PRPS
           FOR ALL ENTRIES IN GT_H_OUTPUT[]
         WHERE POSID = GT_H_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.


    LOOP AT GT_H_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_H_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_H_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_H_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_H_OUTPUT-KALID = WA_PROJ-KALID.
      GT_H_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_H_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_H_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF P_UPCH = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_CH_OUTPUT.
      GT_CH_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_CH_OUTPUT.
    ENDLOOP.

*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_CH_OUTPUT
*    WHERE PSPID = GT_CH_OUTPUT-POSID.

    SELECT PSPNR
               POSID
               PSPHI
          INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
          FROM PRPS
           FOR ALL ENTRIES IN GT_CH_OUTPUT[]
         WHERE POSID = GT_CH_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.

    LOOP AT GT_CH_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_CH_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_CH_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_CH_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_CH_OUTPUT-KALID = WA_PROJ-KALID.
      GT_CH_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_CH_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_CH_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF P_UPCZ = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_CZ_OUTPUT.
      GT_CZ_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_CZ_OUTPUT.
    ENDLOOP.

*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_CZ_OUTPUT
*    WHERE PSPID = GT_CZ_OUTPUT-POSID.

    SELECT PSPNR
               POSID
               PSPHI
          INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
          FROM PRPS
           FOR ALL ENTRIES IN GT_CZ_OUTPUT[]
         WHERE POSID = GT_CZ_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.


    LOOP AT GT_CZ_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_CZ_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_CZ_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_CZ_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_CZ_OUTPUT-KALID = WA_PROJ-KALID.
      GT_CZ_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_CZ_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_CZ_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF P_UPHZ = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_HZ_OUTPUT.
      GT_HZ_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_HZ_OUTPUT.
    ENDLOOP.

*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_HZ_OUTPUT
*    WHERE PSPID = GT_HZ_OUTPUT-POSID.

    SELECT PSPNR
               POSID
               PSPHI
          INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
          FROM PRPS
           FOR ALL ENTRIES IN GT_HZ_OUTPUT[]
         WHERE POSID = GT_HZ_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.

    LOOP AT GT_HZ_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_HZ_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_HZ_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_HZ_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_HZ_OUTPUT-KALID = WA_PROJ-KALID.
      GT_HZ_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_HZ_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_HZ_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ELSEIF P_UPCHZ = 'X'.
    LOOP AT P_GS_SOURCE.
      MOVE-CORRESPONDING P_GS_SOURCE TO GT_CHZ_OUTPUT.
      GT_CHZ_OUTPUT-TYPE = ICON_LIGHT_OUT.
      APPEND GT_CHZ_OUTPUT.
    ENDLOOP.

*    SELECT *
*    FROM PROJ
*    INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
*    FOR ALL ENTRIES IN GT_CHZ_OUTPUT
*    WHERE PSPID = GT_CHZ_OUTPUT-POSID.

    SELECT PSPNR
               POSID
               PSPHI
          INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
          FROM PRPS
           FOR ALL ENTRIES IN GT_CHZ_OUTPUT[]
         WHERE POSID = GT_CHZ_OUTPUT-POSID .
    IF LT_PRPS2 IS NOT INITIAL .
      SELECT *
        FROM PROJ
        INTO CORRESPONDING FIELDS OF TABLE IT_PROJ
         FOR ALL ENTRIES IN LT_PRPS2
       WHERE PSPNR = LT_PRPS2-PSPHI.
    ENDIF.

    LOOP AT GT_CHZ_OUTPUT.
      READ TABLE IT_PROJ INTO WA_PROJ INDEX 1.
      GT_CHZ_OUTPUT-POST1 = WA_PROJ-POST1.
      GT_CHZ_OUTPUT-VERNR = WA_PROJ-VERNR.
      GT_CHZ_OUTPUT-VBUKR = WA_PROJ-VBUKR.
      GT_CHZ_OUTPUT-KALID = WA_PROJ-KALID.
      GT_CHZ_OUTPUT-ZTEHT = WA_PROJ-ZTEHT.
      MODIFY GT_CHZ_OUTPUT.
      CLEAR WA_PROJ.
    ENDLOOP.
    IF GT_CHZ_OUTPUT[] IS INITIAL.
      MESSAGE '文件为空！' TYPE 'I'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0852   text
*      -->P_0853   text
*      -->P_0854   text
*      -->P_0855   text
*----------------------------------------------------------------------*
FORM FRM_FIELD_CAT  USING  PNAME
                           PTABLE
                           PFIELD
                           PTEXT.
  CLEAR GS_FIELDCAT.
  GS_FIELDCAT-FIELDNAME = PNAME.
  GS_FIELDCAT-REF_TABLE = PTABLE.
  GS_FIELDCAT-REF_FIELD = PFIELD.
  GS_FIELDCAT-COLTEXT = PTEXT.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_C_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_C_OUTPUT.                                            "成本预算
  CLEAR :SC_LEN .
  SELECT PSPHI                                                        "取出了WBS元素对应的项目定义
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_C_OUTPUT
    WHERE POSID = GT_C_OUTPUT-POSID.

  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 08:35:21  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 08:35:21  END
    SELECT TOPNR VERSN                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码注释 BY HANDYBY 23.05.2017 09:03:18  BEGIN
*    FOR ALL ENTRIES IN GT_PSPHI
*&--代码注释 BY HANDYBY 23.05.2017 09:03:18  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN1                        "取一个对象编号下对应的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID
        WHERE TOPNR = GT_OBJID-TOPNR
        AND VERSN BETWEEN '000' AND '099'.
    ENDIF.

  ELSE.
    MESSAGE 'WBS元素没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH:GT_OBJID,GT_PSPHI.

  DATA:P_BB1 TYPE PRECP1-VERSN.

  SORT GT_VERSN1 BY VERSN.

  IF GT_VERSN1[] IS NOT INITIAL.
    LOOP AT GT_VERSN1 WHERE VERSN >= '000' AND VERSN <= '099'.
*    if gt_versn1-versn is not initial.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN1-VERSN = GT_VERSN1-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN1-VERSN
          IMPORTING
            OUTPUT = GT_VERSN1-VERSN.

        P_BB1 = GT_VERSN1-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB1 = '001'.
  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 09:39:10  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 09:39:10  END
  SORT GT_C_OUTPUT BY MATNR.

  REFRESH IT_JG1.
  LOOP AT GT_C_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_C_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG1-MATNR = GT_C_OUTPUT-MATNR.
        WA_JG1-CLDJ = GT_C_OUTPUT-CGPREIS.
        WA_JG1-RGDJ = GT_C_OUTPUT-FPREIS.
        APPEND WA_JG1 TO IT_JG1.
      ENDIF.
    ENDAT.
    CLEAR GT_C_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ1.
  LOOP AT GT_C_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ.
    WA_JGHJ1-MATNR = GT_C_OUTPUT-MATNR.
    WA_JGHJ1-CGPREIS = GT_C_OUTPUT-CGPREIS.
    WA_JGHJ1-FPREIS = GT_C_OUTPUT-FPREIS.
    COLLECT WA_JGHJ1 INTO IT_JGHJ1.
  ENDLOOP.

  LOOP AT IT_JG1 INTO WA_JG1.
    READ TABLE IT_JGHJ1 INTO WA_JGHJ1 WITH KEY MATNR = WA_JG1-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_C_OUTPUT WHERE MATNR = WA_JGHJ1-MATNR.
        G = G + 1.
      ENDLOOP.
      LL = WA_JGHJ1-CGPREIS / G.
      JJ = WA_JGHJ1-FPREIS / G.


      IF LL <> WA_JG1-CLDJ.
        CONCATENATE '物料' WA_JGHJ1-MATNR '的材料单价不一致！' INTO MESSAGE2.
        MESSAGE MESSAGE2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF JJ <> WA_JG1-RGDJ.
        CONCATENATE '物料' WA_JGHJ1-MATNR '的人工单价不一致！' INTO MESSAGE3.
        MESSAGE MESSAGE3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG1,W,LL,JJ,MESSAGE2,MESSAGE3.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 09:38:57  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 09:38:57  END
  DELETE GT_C_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_C_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 09:46:06  BEGIN
* 这里GT_JHGCL取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 09:46:06  END
  REFRESH GT_JHGCL.
  LOOP AT GT_C_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_JHGCL.
    GS_JHGCL-MATNR = GT_C_OUTPUT-MATNR.
    GS_JHGCL-JPEINH = GT_C_OUTPUT-JPEINH.
    COLLECT GS_JHGCL INTO GT_JHGCL.
  ENDLOOP.
*&--代码添加 BY HANDYBY 27.04.2017 12:52:33  BEGIN
  SORT GT_JHGCL BY MATNR .
*&--代码添加 BY HANDYBY 27.04.2017 12:52:33  END

  LOOP AT GT_C_OUTPUT WHERE MATNR = ''.
    APPEND GT_C_OUTPUT TO GT_C_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_C_OUTPUT COMPARING MATNR.

  DELETE GT_C_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_C_OUTPUT1.
    APPEND GT_C_OUTPUT1 TO GT_C_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_C_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的将被导入一次，且将合计相同物料的计划工程量！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.



*&--代码添加 BY HANDYBY 28.04.2017 11:23:47  BEGIN
  "先检查一下可控的行错误信息.

  LOOP AT GT_C_OUTPUT .

    IF GT_C_OUTPUT-POSID IS INITIAL.

      GT_C_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_C_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GT_C_OUTPUT-XMLB = 'M'.
      IF GT_C_OUTPUT-MATNR IS  INITIAL. .
        GT_C_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GT_C_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 11:56:09  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GT_C_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GT_C_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 11:56:09  END

*&--代码添加 BY HANDYBY 25.05.2017 09:47:34  BEGIN
* 物料号变大写
      TRANSLATE GT_C_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:47:34  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GT_C_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GT_C_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GT_C_OUTPUT-WERKS '工厂' '不存在:' GT_C_OUTPUT-MATNR '物料号' INTO    GT_C_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.

    IF GT_C_OUTPUT-XMLB = 'V' AND GT_C_OUTPUT-CBYS IS  INITIAL..

      GT_C_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_C_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_C_OUTPUT .
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 11:23:47  END



*  ........................add IT02 20160307begin ...................... *
  READ TABLE GT_C_OUTPUT WITH KEY TYPE = ICON_RED_LIGHT .
  IF SY-SUBRC NE 0 .

    CLEAR:PROJ_OBJNR,PRPS_OBJNR.

    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_C_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_C_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_C_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '000'.

          IF LS_KALNR IS NOT INITIAL .

            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    REFRESH: LT_WBS_COSTLINES3,GT_RETURN2,LS_WBS_COSTLINES3-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN2.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.


      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除000版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.

      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除0版本失败：' GS_RETURN2-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        WAIT UP TO 0 SECONDS .
      ENDIF.
    ENDIF.

    "再重新读取000版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
       FROM CKIS
       INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
       WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.


  ELSE .
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.

*  ........................add IT02 20160307end ...................... *



*&--代码注释 BY HANDYBY 28.04.2017 10:45:20  BEGIN
*
*  IF IT_CKIS IS INITIAL.
*    CLEAR:L1,L2,L3,L4.   "初始导入版本数量
*    LOOP AT GT_C_OUTPUT.
*
*      IF GT_C_OUTPUT-POSID IS NOT INITIAL.
*        LS_WBS_COSTLINES-WBS_NAME = GT_C_OUTPUT-POSID.                  "WBS
*      ELSE.
*        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
*        CONTINUE.
*      ENDIF.
*
*      GT_C_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态
*
*      LS_ITEM-RESOURCE-TYPPS = GT_C_OUTPUT-XMLB.                        "项目类别
*
*      IF GT_C_OUTPUT-WERKS IS INITIAL.
*        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
*      ELSE.
*        LS_ITEM-RESOURCE-WERKS = GT_C_OUTPUT-WERKS.
*      ENDIF.
*
*      IF GT_C_OUTPUT-XMLB = 'M'.
*        IF GT_C_OUTPUT-MATNR IS NOT INITIAL.
*          LS_ITEM-RESOURCE-MATNR = GT_C_OUTPUT-MATNR.                  "物料号
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-RESOURCE-MATNR = GT_C_OUTPUT-MATNR.                    "物料编码
*      ENDIF.
*
*      REFRESH GT_MEINS[].
*      CLEAR GT_MEINS.
*      IF GT_C_OUTPUT-MATNR IS NOT INITIAL.
*        SELECT MEINS
*          FROM MARA
*          INTO TABLE GT_MEINS
*          WHERE MATNR = GT_C_OUTPUT-MATNR.
*      ENDIF.
*
*      READ TABLE GT_MEINS.
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.
*      LS_ITEM-DESCRIPT       = GT_C_OUTPUT-MAKTX.                       "物料描述
*
**&--代码注释 BY HANDYBY 27.04.2017 15:22:39  BEGIN
**    READ TABLE gt_jhgcl INTO gs_jhgcl WITH KEY matnr = gt_c_output-matnr .
**&--代码注释 BY HANDYBY 27.04.2017 15:22:39  END
**&--代码添加 BY HANDYBY 27.04.2017 15:22:28  BEGIN
*      READ TABLE GT_JHGCL INTO GS_JHGCL WITH KEY MATNR = GT_C_OUTPUT-MATNR BINARY SEARCH .
**&--代码添加 BY HANDYBY 27.04.2017 15:22:28  END
*      IF SY-SUBRC = 0.
*        GT_C_OUTPUT-JPEINH = GS_JHGCL-JPEINH.
*      ENDIF.
*
*      LS_ITEM-QUANTITY-QUANTITY = GT_C_OUTPUT-JPEINH.                   "计划工程量
*      LS_ITEM-PRICE-TOTAL    = GT_C_OUTPUT-CGPREIS.                     "材料单价
*      LS_ITEM-PRICE-FIXED    = GT_C_OUTPUT-FPREIS.                      "人工单价
*
*      IF GT_C_OUTPUT-XMLB = 'V'.
*        IF GT_C_OUTPUT-CBYS IS NOT INITIAL.
*          LS_ITEM-COST_ELEM = GT_C_OUTPUT-CBYS.                          "成本要素
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-COST_ELEM = GT_C_OUTPUT-CBYS.                             "成本要素
*      ENDIF.
*      LS_ITEM-CURRENCY = 'CNY'.
*      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*
*      CALL FUNCTION 'CNECP_MAINTAIN'
*        EXPORTING
*          I_PROJ_ID       = GT_C_OUTPUT-POSID                           "项目定义
*          I_VERSION       = P_BB1                                       "版本
*          I_COST_VARIANT  = 'PS06'                                      "核算变式
*          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*        IMPORTING
*          MESSAGES        = GT_RETURN.
*
*      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC NE 0.
*        L3 = L3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
*        "  gt_c_output-type = icon_green_light.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
*        CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
*          EXPORTING
*            I_PROJ_ID       = GT_C_OUTPUT-POSID                           "项目定义
*            I_VERSION       = '000'                                         "版本
*            I_COST_VARIANT  = 'PS06'                                      "核算变式
*            I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*          IMPORTING
*            MESSAGES        = GT_RETURN1.
*        READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
*        IF SY-SUBRC NE 0.
*          L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              WAIT = 'X'.
*        ELSE.
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_C_OUTPUT-MESSAGE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*
*
*      ELSE.
*        "    gt_c_output-type = icon_red_light.
*        " gt_c_output-message = gs_return-message.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      ENDIF.
*      MODIFY GT_C_OUTPUT TRANSPORTING JPEINH TYPE MESSAGE.
*
*      CLEAR GT_C_OUTPUT.
*
*      CLEAR:LS_ITEM,LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES.
*      REFRESH LT_WBS_COSTLINES.
*      CLEAR:GS_RETURN.
*      REFRESH:GT_RETURN.
*
*      " js = js + 1.
*
*    ENDLOOP.
*  ELSE.
*    MESSAGE '该项目原0版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 10:45:20  END

*&--代码添加 BY HANDYBY 28.04.2017 10:45:40  BEGIN

*  IF GT_C_OUTPUT[] IS NOT INITIAL .
*    SELECT  MATNR
*            MEINS
*          FROM MARA
*          INTO CORRESPONDING FIELDS OF TABLE GT_MEINS
*      FOR ALL ENTRIES IN GT_C_OUTPUT
*          WHERE MATNR = GT_C_OUTPUT-MATNR.
*    SORT GT_MEINS BY MATNR .
*  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 11:14:23  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_C_OUTPUT[]
   WHERE POSID = GT_C_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 11:14:23  END

  IF IT_CKIS IS INITIAL.
    CLEAR:L1,L2,L3,L4.   "初始导入版本数量
    CLEAR GT_C_OUTPUT.
    REFRESH LT_WBS_COSTLINES.

    DATA:L_INDEX TYPE SY-TABIX.
    TYPES:BEGIN OF TY_C_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            JPEINH       TYPE MENGE_POS,              "计划工程量
            CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
            FPREIS       TYPE CKIS-FPREIS,             "人工单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_C_OUTPUT2.
    DATA:LT_C_OUTPUT TYPE TABLE OF TY_C_OUTPUT2,
         LS_C_OUTPUT TYPE TY_C_OUTPUT2.
    DATA LS_C_OUTPUT2 TYPE TY_C_OUTPUT2 .

    MOVE-CORRESPONDING GT_C_OUTPUT[] TO LT_C_OUTPUT .
    SORT LT_C_OUTPUT BY POSID  .
    LOOP AT LT_C_OUTPUT INTO LS_C_OUTPUT2 .

      CLEAR:LS_ITEM.
      L_INDEX = SY-TABIX .

      MOVE-CORRESPONDING LS_C_OUTPUT2 TO LS_C_OUTPUT .
*      CLEAR :LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES.

*      AT FIRST .
*        READ TABLE GT_C_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_C_OUTPUT-POSID.                   "WBS
*        ENDIF.
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_C_OUTPUT-POSID.                   "WBS
      ENDAT .

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-MATNR = LS_C_OUTPUT-MATNR.                       "物料号

      LS_C_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

      IF  LS_ITEM-RESOURCE-MATNR IS NOT INITIAL.
        CLEAR GS_CHECK_MATNR.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_C_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
        ENDIF.
      ENDIF.

      IF LS_C_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_C_OUTPUT-CBYS.                          "成本要素
      ENDIF.

      LS_ITEM-RESOURCE-TYPPS = LS_C_OUTPUT-XMLB.                        "项目类别

      IF LS_C_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 11:16:38  BEGIN
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_C_OUTPUT-POSID BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_ITEM-RESOURCE-WERKS = LS_PRPS-WERKS .
          CLEAR LS_PRPS .                              "工厂
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 11:16:38  END
      ELSE.
        LS_ITEM-RESOURCE-WERKS = LS_C_OUTPUT-WERKS.
      ENDIF.

      LS_ITEM-DESCRIPT       = LS_C_OUTPUT-MAKTX.                       "物料描述

      LS_ITEM-PRICE-FIXED    = LS_C_OUTPUT-FPREIS.                      "人工单价

*      READ TABLE GT_MEINS WITH KEY MATNR = GT_C_OUTPUT-MATNR BINARY SEARCH .
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.

      READ TABLE GT_JHGCL INTO GS_JHGCL WITH KEY MATNR = LS_C_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0.
        GT_C_OUTPUT-JPEINH = GS_JHGCL-JPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_C_OUTPUT-JPEINH.                   "计划工程量
      LS_ITEM-PRICE-TOTAL    = LS_C_OUTPUT-CGPREIS.                     "材料单价

      LS_ITEM-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*      ENDAT.

      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        CLEAR LS_WBS_COSTLINES .
      ENDAT .

      CLEAR LS_ITEM .
      CLEAR LS_C_OUTPUT2.
      CLEAR LS_C_OUTPUT .

    ENDLOOP.


    REFRESH:GT_RETURN.
    REFRESH:GT_RETURN1.
    READ TABLE GT_C_OUTPUT INDEX 1 .
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS4 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS4
      FROM PRPS
     WHERE POSID = GT_C_OUTPUT-POSID .
    IF LS_PRPS4 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS4-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 10:07:04  BEGIN
    DATA L_FLAG TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 10:07:04  END
    CALL FUNCTION 'CNECP_MAINTAIN'
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
        I_VERSION       = P_BB1                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.

    CLEAR:GS_RETURN.
    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC NE 0.
*      L3 = L3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
      "  gt_c_output-type = icon_green_light.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN1.
      READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
*        L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  BEGIN
        L_FLAG = 0 .
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  END

      ELSE.
        CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_C_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  BEGIN
        L_FLAG = 1 .
        LOOP AT GT_RETURN1 INTO GS_RETURN1 .
          CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN1 .
        ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  END
      ENDIF.


    ELSE.
      "    gt_c_output-type = icon_red_light.
      " gt_c_output-message = gs_return-message.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  BEGIN
      L_FLAG = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 10:08:04  END
    ENDIF.



*    MODIFY GT_C_OUTPUT TRANSPORTING JPEINH TYPE MESSAGE.

  ELSE.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*&--代码添加 BY HANDYBY 28.04.2017 10:45:40  END


  "增加消息提示

*&--代码添加 BY HANDYBY 23.05.2017 10:10:50  BEGIN
  MSG000 = L_INDEX .
  CONDENSE MSG000 NO-GAPS.
  MSG001 = L_INDEX .
  CONDENSE MSG001 NO-GAPS.

  IF L_FLAG = 0.
    "000版本
    CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
    "000后版本
    CONCATENATE P_BB1 '版本已成功导入' MSG001 '条数量' INTO  MSG001.
  ELSEIF L_FLAG = 1 .
    CONCATENATE  P_BB1 '版本已成功导入:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本导入失败:'  MSG000 '条数量' INTO MSG000 .
  ELSEIF L_FLAG = 2 .
    CONCATENATE  P_BB1 '版本导入失败:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本没有执行导入动作！' '' INTO MSG000 .
  ENDIF.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .
*&--代码添加 BY HANDYBY 23.05.2017 10:10:50  END

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_H_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_H_OUTPUT.                                            "合同报价
  CLEAR :SC_LEN .
  SELECT PSPHI
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_H_OUTPUT
    WHERE POSID = GT_H_OUTPUT-POSID.

  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 14:59:06  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 14:59:06  END
    SELECT TOPNR VERSN                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码注释 BY HANDYBY 23.05.2017 14:59:31  BEGIN
*    FOR ALL ENTRIES IN GT_PSPHI
*&--代码注释 BY HANDYBY 23.05.2017 14:59:31  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN2                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID
        WHERE TOPNR = GT_OBJID-TOPNR
        AND VERSN BETWEEN '100' AND '199'.
    ENDIF.
  ELSE.
    MESSAGE 'WBS没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH GT_OBJID.

  DATA:P_BB2 TYPE PRECP1-VERSN.

  SORT GT_VERSN2 BY VERSN.

  IF GT_VERSN2[] IS NOT INITIAL.
    LOOP AT GT_VERSN2 WHERE VERSN >= '100' AND VERSN <= '199'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN2-VERSN = GT_VERSN2-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN2-VERSN
          IMPORTING
            OUTPUT = GT_VERSN2-VERSN.

        P_BB2 = GT_VERSN2-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB2 = 101.
  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:00:09  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:00:09  END
  SORT GT_H_OUTPUT BY MATNR.

  REFRESH IT_JG2.
  LOOP AT GT_H_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_H_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG2-MATNR = GT_H_OUTPUT-MATNR.
        WA_JG2-HTDJ = GT_H_OUTPUT-HGPREIS.
        APPEND WA_JG2 TO IT_JG2.
      ENDIF.
    ENDAT.
    CLEAR GT_H_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ2.
  LOOP AT GT_H_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ2.
    WA_JGHJ2-MATNR = GT_H_OUTPUT-MATNR.
    WA_JGHJ2-HGPREIS = GT_H_OUTPUT-HGPREIS.
    COLLECT WA_JGHJ2 INTO IT_JGHJ2.
  ENDLOOP.

  LOOP AT IT_JG2 INTO WA_JG2.
    READ TABLE IT_JGHJ2 INTO WA_JGHJ2 WITH KEY MATNR = WA_JG2-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_H_OUTPUT WHERE MATNR = WA_JGHJ2-MATNR.
        G = G + 1.
      ENDLOOP.
      W = WA_JGHJ2-HGPREIS / G.


      IF W <> WA_JG2-HTDJ.
        CONCATENATE '物料' WA_JGHJ2-MATNR '的合同单价不一致！' INTO MESSAGE1.
        MESSAGE MESSAGE1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG2,W,MESSAGE1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:01:35  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:01:35  END
  DELETE GT_H_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_H_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:02:19  BEGIN
* 这里GT_HTGCL取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 15:02:19  END
  REFRESH GT_HTGCL.
  LOOP AT GT_H_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_HTGCL.
    GS_HTGCL-MATNR = GT_H_OUTPUT-MATNR.
    GS_HTGCL-HPEINH = GT_H_OUTPUT-HPEINH.
    COLLECT GS_HTGCL INTO GT_HTGCL.
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 12:20:49  BEGIN
  SORT GT_HTGCL BY MATNR .
*&--代码添加 BY HANDYBY 28.04.2017 12:20:49  END

  LOOP AT GT_H_OUTPUT WHERE MATNR = ''.
    APPEND GT_H_OUTPUT TO GT_H_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_H_OUTPUT COMPARING MATNR.

  DELETE GT_H_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_H_OUTPUT1.
    APPEND GT_H_OUTPUT1 TO GT_H_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_H_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的只被导入一次，且相同物料的合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.


*&--代码添加 BY HANDYBY 28.04.2017 11:54:56  BEGIN
  "先检查一下可控的行错误信息.

  LOOP AT GT_H_OUTPUT .

    IF GT_H_OUTPUT-POSID IS INITIAL.

      GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_H_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GT_H_OUTPUT-XMLB = 'M'.
      IF GT_H_OUTPUT-MATNR IS  INITIAL. .
        GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GT_H_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:03:42  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GT_H_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GT_H_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:03:42  END

*&--代码添加 BY HANDYBY 25.05.2017 09:47:34  BEGIN
* 物料号变大写
      TRANSLATE GT_H_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:47:34  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GT_H_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GT_H_OUTPUT-WERKS '工厂' '不存在:' GT_H_OUTPUT-MATNR '物料号' INTO    GT_H_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.

    IF GT_H_OUTPUT-XMLB = 'V' AND GT_H_OUTPUT-CBYS IS  INITIAL..

      GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_H_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_H_OUTPUT .
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 11:54:56  END



*  -------------------------------先删除100版本数据 it02 20160308 begin----------------------
  CLEAR:PROJ_OBJNR,PRPS_OBJNR.
  READ TABLE GT_H_OUTPUT INDEX 1.
  IF SY-SUBRC EQ 0 .


    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_H_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_H_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_H_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '100'.

          IF LS_KALNR IS NOT INITIAL .

            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.



    REFRESH: LT_WBS_COSTLINES3,GT_RETURN2,LS_WBS_COSTLINES2-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN2.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.
      ENDLOOP.



      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.
      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除100版本失败：' GS_RETURN2-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.

    ENDIF.
    "再重新读取100版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
             FROM CKIS
             INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
             WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.

  ELSE .
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.



*&--代码注释 BY HANDYBY 28.04.2017 11:56:56  BEGIN
*

*  IF IT_CKIS IS  INITIAL.
*    LOOP AT GT_H_OUTPUT.
*
*      IF GT_H_OUTPUT-POSID IS NOT INITIAL.
*        LS_WBS_COSTLINES-WBS_NAME = GT_H_OUTPUT-POSID.                   "WBS
*      ELSE.
*        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
*        CONTINUE.
*      ENDIF.
*
*      GT_H_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态
*
*      LS_ITEM-RESOURCE-TYPPS = GT_H_OUTPUT-XMLB.                        "项目类别
*
*      IF GT_H_OUTPUT-WERKS IS INITIAL.
*        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
*      ELSE.
*        LS_ITEM-RESOURCE-WERKS = GT_H_OUTPUT-WERKS.
*      ENDIF.
*
*      IF GT_H_OUTPUT-XMLB = 'M'.
*        IF GT_H_OUTPUT-MATNR IS NOT INITIAL.
*          LS_ITEM-RESOURCE-MATNR = GT_H_OUTPUT-MATNR.                       "物料号
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-RESOURCE-MATNR = GT_H_OUTPUT-MATNR.                       "物料号
*      ENDIF.
*
*      REFRESH GT_MEINS[].
*      CLEAR GT_MEINS.
*      IF GT_H_OUTPUT-MATNR IS NOT INITIAL.
*        SELECT MEINS
*              FROM MARA
*              INTO TABLE GT_MEINS
*              WHERE MATNR = GT_H_OUTPUT-MATNR.
*      ENDIF.
*
*      READ TABLE GT_MEINS.
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.
*      LS_ITEM-DESCRIPT       = GT_H_OUTPUT-MAKTX.                       "物料描述
*
*      READ TABLE GT_HTGCL INTO GS_HTGCL WITH TABLE KEY MATNR = GT_H_OUTPUT-MATNR.
*      IF SY-SUBRC = 0.
*        GT_H_OUTPUT-HPEINH = GS_HTGCL-HPEINH.
*      ENDIF.
*
*      LS_ITEM-QUANTITY-QUANTITY = GT_H_OUTPUT-HPEINH.                  "合同工程量
*      LS_ITEM-PRICE-TOTAL   = GT_H_OUTPUT-HGPREIS.                     "合同单价
*
*      IF GT_H_OUTPUT-XMLB = 'V'.
*        IF GT_H_OUTPUT-CBYS IS NOT INITIAL.
*          LS_ITEM-COST_ELEM = GT_H_OUTPUT-CBYS.                          "成本要素
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-COST_ELEM = GT_H_OUTPUT-CBYS.                             "成本要素
*      ENDIF.
*      LS_ITEM-CURRENCY = 'CNY'.
*      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*
*      CALL FUNCTION 'CNECP_MAINTAIN'
*        EXPORTING
*          I_PROJ_ID       = GT_H_OUTPUT-POSID                           "项目编号
*          I_VERSION       = P_BB2                                       "版本
*          I_COST_VARIANT  = 'PS06'                                      "核算变式
*          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*        IMPORTING
*          MESSAGES        = GT_RETURN.
*
*      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC NE 0.
*        L4 = L4 + 1. "统计导入100后最新版本的数据  IT02 20160307
*        GT_H_OUTPUT-TYPE = ICON_GREEN_LIGHT.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
*        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
*          EXPORTING
*            I_PROJ_ID       = GT_H_OUTPUT-POSID                           "项目定义
*            I_VERSION       = '100'                                         "版本
*            I_COST_VARIANT  = 'PS06'                                      "核算变式
*            I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*          IMPORTING
*            MESSAGES        = GT_RETURN1.
*        READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
*        IF SY-SUBRC NE 0.
*          L2 = L2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
*        ELSE.
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_H_OUTPUT-MESSAGE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*
*      ELSE.
*        GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.
*        GT_H_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      ENDIF.
*
*      MODIFY GT_H_OUTPUT TRANSPORTING HPEINH TYPE MESSAGE.
*
*      CLEAR GT_H_OUTPUT.
*
*      CLEAR:LS_ITEM,LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES.
*      REFRESH LT_WBS_COSTLINES.
*      CLEAR:GS_RETURN.
*      REFRESH:GT_RETURN.
*
*      " js = js + 1.
*
*    ENDLOOP.
*  ELSE.
*    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 11:56:56  END

*&--代码添加 BY HANDYBY 28.04.2017 11:57:14  BEGIN
*  IF GT_H_OUTPUT[] IS NOT INITIAL .
*    SELECT  MATNR
*            MEINS
*          FROM MARA
*          INTO CORRESPONDING FIELDS OF TABLE GT_MEINS
*      FOR ALL ENTRIES IN GT_H_OUTPUT
*          WHERE MATNR = GT_H_OUTPUT-MATNR.
*    SORT GT_MEINS BY MATNR .
*  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:07:47  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_H_OUTPUT[]
   WHERE POSID = GT_H_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 15:07:47  END

  IF IT_CKIS IS  INITIAL.
    CLEAR GT_H_OUTPUT.
    REFRESH LT_WBS_COSTLINES.

    DATA:L_INDEX TYPE SY-TABIX.
    TYPES:BEGIN OF TY_H_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            HPEINH       TYPE MENGE_POS,              "合同工程量
            HGPREIS      TYPE CKIS-GPREIS,             "合同单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_H_OUTPUT2.
    DATA:LT_H_OUTPUT TYPE TABLE OF TY_H_OUTPUT2,
         LS_H_OUTPUT TYPE TY_H_OUTPUT2.
    DATA LS_H_OUTPUT2 TYPE TY_H_OUTPUT2 .

    MOVE-CORRESPONDING GT_H_OUTPUT[] TO LT_H_OUTPUT .
    SORT LT_H_OUTPUT BY POSID .
    LOOP AT LT_H_OUTPUT INTO LS_H_OUTPUT2 .

      CLEAR:LS_ITEM.
      L_INDEX = SY-TABIX .

      MOVE-CORRESPONDING LS_H_OUTPUT2 TO LS_H_OUTPUT .
*      AT FIRST .
*        READ TABLE GT_H_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_H_OUTPUT-POSID.                   "WBS
*        ENDIF.
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_H_OUTPUT-POSID.                   "WBS
      ENDAT .

      GT_H_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-TYPPS = LS_H_OUTPUT-XMLB.                        "项目类别

      IF LS_H_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 15:09:03  BEGIN
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_H_OUTPUT-POSID BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_ITEM-RESOURCE-WERKS = LS_PRPS-WERKS .                                  "工厂
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:09:03  END
      ELSE.
        LS_ITEM-RESOURCE-WERKS = LS_H_OUTPUT-WERKS.
      ENDIF.

      LS_ITEM-RESOURCE-MATNR = LS_H_OUTPUT-MATNR.                       "物料号

      IF  LS_ITEM-RESOURCE-MATNR IS NOT INITIAL.
        CLEAR GS_CHECK_MATNR.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_H_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
        ENDIF.
      ENDIF.

      LS_ITEM-DESCRIPT       = LS_H_OUTPUT-MAKTX.                       "物料描述

      READ TABLE GT_HTGCL INTO GS_HTGCL WITH TABLE KEY MATNR = LS_H_OUTPUT-MATNR .
      IF SY-SUBRC = 0.
        LS_H_OUTPUT-HPEINH = GS_HTGCL-HPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_H_OUTPUT-HPEINH.                  "合同工程量
      LS_ITEM-PRICE-TOTAL    = LS_H_OUTPUT-HGPREIS.                     "合同单价

      IF LS_H_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_H_OUTPUT-CBYS.                          "成本要素
      ENDIF.

      LS_ITEM-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*      ENDAT.

      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        CLEAR LS_WBS_COSTLINES .
      ENDAT .

      CLEAR LS_ITEM .
      CLEAR LS_H_OUTPUT2.
      CLEAR LS_H_OUTPUT .

    ENDLOOP.

    REFRESH:GT_RETURN.
    REFRESH:GT_RETURN1.
    READ TABLE GT_H_OUTPUT INDEX 1 .
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS4 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS4
      FROM PRPS
     WHERE POSID = GT_H_OUTPUT-POSID .
    IF LS_PRPS4 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS4-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:11:00  BEGIN
    DATA L_FLAG TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 15:11:00  END
    CALL FUNCTION 'CNECP_MAINTAIN'
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                           "项目编号
        I_VERSION       = P_BB2                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.

    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC NE 0.
*      L4 = L4 + 1. "统计导入100后最新版本的数据  IT02 20160307
*      GT_H_OUTPUT-TYPE = ICON_GREEN_LIGHT.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN1.
      READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
*        L2 = L2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
*&--代码添加 BY HANDYBY 23.05.2017 15:11:52  BEGIN
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
        L_FLAG = 0 .
*&--代码添加 BY HANDYBY 23.05.2017 15:11:52  END
      ELSE.
        CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_H_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*&--代码添加 BY HANDYBY 23.05.2017 15:12:13  BEGIN
        L_FLAG = 1 .
        LOOP AT GT_RETURN1 INTO GS_RETURN1 .
          CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN1 .
        ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 15:12:13  END
      ENDIF.

    ELSE.
*      GT_H_OUTPUT-TYPE = ICON_RED_LIGHT.
*      GT_H_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*&--代码添加 BY HANDYBY 23.05.2017 15:12:30  BEGIN
      L_FLAG = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 15:12:30  END

    ENDIF.

  ELSE.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*&--代码添加 BY HANDYBY 28.04.2017 11:57:14  END



  "增加消息提示

*&--代码添加 BY HANDYBY 23.05.2017 15:13:07  BEGIN
  MSG100 = L_INDEX .
  CONDENSE MSG100 NO-GAPS.
  MSG101 = L_INDEX .
  CONDENSE MSG101 NO-GAPS.

  IF L_FLAG = 0.
    "100版本
    CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
    "100后版本
    CONCATENATE P_BB2 '版本已成功导入' MSG101 '条数量' INTO  MSG101.
  ELSEIF L_FLAG = 1 .
    CONCATENATE  P_BB2 '版本已成功导入:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本导入失败:'  MSG100 '条数量' INTO MSG100 .
  ELSEIF L_FLAG = 2 .
    CONCATENATE  P_BB2 '版本导入失败:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本没有执行导入动作！' '' INTO MSG100 .
  ENDIF.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .

*&--代码添加 BY HANDYBY 23.05.2017 15:13:07  END


  "100版本
*  MSG100 = L2 .
*  CONDENSE MSG100 NO-GAPS.
*  CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
*  "100后版本
*  MSG101 = L4 .
*  CONDENSE MSG101 NO-GAPS.
*  CONCATENATE P_BB2  '版本已成功导入'  MSG101 '条数量' INTO MSG101.
*  CALL SCREEN 0101 STARTING AT 25 10.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CH_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_CH_OUTPUT.                                            "成本预算和合同报价
  CLEAR :SC_LEN .
  SELECT PSPHI
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_CH_OUTPUT
    WHERE POSID = GT_CH_OUTPUT-POSID.
  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 15:28:51  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 15:28:51  END
    SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID1
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 15:29:07  BEGIN
* FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 15:29:07  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID1[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN1                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID1
        WHERE TOPNR = GT_OBJID1-TOPNR
*&--代码添加 BY HANDYBY 23.05.2017 15:32:32  BEGIN
          AND VERSN BETWEEN '000' AND '099'.
*&--代码添加 BY HANDYBY 23.05.2017 15:32:32  END
    ENDIF.

    SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID2
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 15:30:06  BEGIN
*   FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 15:30:06  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID2[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN2                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID2
        WHERE TOPNR = GT_OBJID2-TOPNR
*&--代码添加 BY HANDYBY 23.05.2017 15:33:00  BEGIN
          AND VERSN BETWEEN '100' AND '199'.
*&--代码添加 BY HANDYBY 23.05.2017 15:33:00  END
    ENDIF.
  ENDIF.

  REFRESH:GT_OBJID1,GT_OBJID2.

  DATA:P_BB1 TYPE PRECP1-VERSN.

  SORT GT_VERSN1 BY VERSN.
  SORT GT_VERSN2 BY VERSN.

  IF GT_VERSN1[] IS NOT INITIAL.
    LOOP AT GT_VERSN1 WHERE VERSN >= '000' AND VERSN <= '099'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN1-VERSN = GT_VERSN1-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN1-VERSN
          IMPORTING
            OUTPUT = GT_VERSN1-VERSN.

        P_BB1 = GT_VERSN1-VERSN.
      ENDAT.
    ENDLOOP.
  ELSEIF GT_VERSN1[] IS INITIAL.
    P_BB1 = '001'.
  ENDIF.

  DATA:P_BB2 TYPE PRECP1-VERSN.

  IF GT_VERSN2[] IS NOT INITIAL.
    LOOP AT GT_VERSN2 WHERE VERSN >= '100' AND VERSN <= '199'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN2-VERSN = GT_VERSN2-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN2-VERSN
          IMPORTING
            OUTPUT = GT_VERSN2-VERSN.

        P_BB2 = GT_VERSN2-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB2 = 101.
  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:34:01  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:34:01  END
  SORT GT_CH_OUTPUT BY MATNR.

  REFRESH IT_JG.
  LOOP AT GT_CH_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_CH_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG-MATNR = GT_CH_OUTPUT-MATNR.
        WA_JG-HTDJ = GT_CH_OUTPUT-HGPREIS.
        WA_JG-CLDJ = GT_CH_OUTPUT-CGPREIS.
        WA_JG-RGDJ = GT_CH_OUTPUT-FPREIS.
        APPEND WA_JG TO IT_JG.
      ENDIF.
    ENDAT.
    CLEAR GT_CH_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ.
  LOOP AT GT_CH_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ.
    WA_JGHJ-MATNR = GT_CH_OUTPUT-MATNR.
    WA_JGHJ-HGPREIS = GT_CH_OUTPUT-HGPREIS.
    WA_JGHJ-CGPREIS = GT_CH_OUTPUT-CGPREIS.
    WA_JGHJ-FPREIS = GT_CH_OUTPUT-FPREIS.
    COLLECT WA_JGHJ INTO IT_JGHJ.
  ENDLOOP.

  LOOP AT IT_JG INTO WA_JG.
    READ TABLE IT_JGHJ INTO WA_JGHJ WITH KEY MATNR = WA_JG-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_CH_OUTPUT WHERE MATNR = WA_JGHJ-MATNR.
        G = G + 1.
      ENDLOOP.
      W = WA_JGHJ-HGPREIS / G.
      LL = WA_JGHJ-CGPREIS / G.
      JJ = WA_JGHJ-FPREIS / G.


      IF W <> WA_JG-HTDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的合同单价不一致！' INTO MESSAGE1.
        MESSAGE MESSAGE1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF LL <> WA_JG-CLDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的材料单价不一致！' INTO MESSAGE2.
        MESSAGE MESSAGE2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF JJ <> WA_JG-RGDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的人工单价不一致！' INTO MESSAGE3.
        MESSAGE MESSAGE3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG,W,LL,JJ,MESSAGE1,MESSAGE2,MESSAGE3.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:35:33  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:35:33  END
  DELETE GT_CH_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_CH_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:36:12  BEGIN
* 这里GT_HB取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 15:36:12  END
  REFRESH GT_HB.
  LOOP AT GT_CH_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_HB.
    GS_HB-MATNR = GT_CH_OUTPUT-MATNR.
    GS_HB-HPEINH = GT_CH_OUTPUT-HPEINH.
    GS_HB-JPEINH = GT_CH_OUTPUT-JPEINH.
    COLLECT GS_HB INTO GT_HB.
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 13:25:10  BEGIN
  SORT GT_HB BY MATNR .
*&--代码添加 BY HANDYBY 28.04.2017 13:25:10  END

  LOOP AT GT_CH_OUTPUT WHERE MATNR = ''.
    APPEND GT_CH_OUTPUT TO GT_CH_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_CH_OUTPUT COMPARING MATNR.

  DELETE GT_CH_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_CH_OUTPUT1.
    APPEND GT_CH_OUTPUT1 TO GT_CH_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_CH_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的只被导入一次，且相同物料的计划工程量和合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.


*&--代码添加 BY HANDYBY 28.04.2017 12:41:00  BEGIN
  "先检查一下可控的行错误信息.

  LOOP AT GT_CH_OUTPUT .

    IF GT_CH_OUTPUT-POSID IS INITIAL.

      GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_CH_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GT_CH_OUTPUT-XMLB = 'M'.
      IF GT_CH_OUTPUT-MATNR IS  INITIAL. .
        GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GT_CH_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:36:47  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GT_CH_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GT_CH_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:36:47  END

*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  BEGIN
* 物料号变大写
      TRANSLATE GT_CH_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GT_CH_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GT_CH_OUTPUT-WERKS '工厂' '不存在:' GT_CH_OUTPUT-MATNR '物料号' INTO    GT_CH_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.

    IF GT_CH_OUTPUT-XMLB = 'V' AND GT_CH_OUTPUT-CBYS IS  INITIAL..

      GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_CH_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_CH_OUTPUT .
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 12:41:00  END



  READ TABLE GT_CH_OUTPUT WITH KEY TYPE = ICON_RED_LIGHT .
  IF SY-SUBRC NE 0 .

    CLEAR:PROJ_OBJNR,PRPS_OBJNR.

    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS4.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
    DATA: BEGIN OF LS_KALNR1,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR1.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_CH_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_CH_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_CH_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '100'.
          IF LS_KALNR IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .
          ENDIF.


          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR1
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '000'.
          IF LS_KALNR1 IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
              WHERE KALNR = LS_KALNR1-KALNR.

            CLEAR LS_PRPS4 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS4
              FROM PRPS
             WHERE OBJNR = LS_KALNR1-SUBNR .
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.


    REFRESH: LT_WBS_COSTLINES3,GT_RETURN5,LT_WBS_COSTLINES4,GT_RETURN5,LS_WBS_COSTLINES3-COST_LINES,LS_WBS_COSTLINES4-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN5,LS_WBS_COSTLINES4,GS_RETURN5.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.
      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN5.

      READ TABLE GT_RETURN5 INTO GS_RETURN5 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除100版本失败：' GS_RETURN5-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取100版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
           FROM CKIS
           INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
           WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.


    IF IT_CKIS1 IS NOT INITIAL.
      LOOP AT IT_CKIS1 INTO WA_CKIS1.
        CLEAR:LS_ITEM4.
        AT FIRST .
          LS_WBS_COSTLINES4-WBS_NAME = LS_PRPS4-POSID.
        ENDAT.
        LS_ITEM4-ITEM_NUMBER = WA_CKIS1-POSNR.
        LS_ITEM4-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM4 TO LS_WBS_COSTLINES4-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES4 TO LT_WBS_COSTLINES4.
        ENDAT.

      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES4                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN4.

      READ TABLE GT_RETURN4 INTO GS_RETURN4 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除000版本失败：' GS_RETURN4-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取000版本的预算项目信息
    IF LS_KALNR1 IS NOT INITIAL.
      SELECT *
           FROM CKIS
           INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
           WHERE KALNR = LS_KALNR1-KALNR.
    ENDIF.

  ELSE .
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 12:43:33  BEGIN
*

*  IF IT_CKIS1 IS INITIAL AND IT_CKIS IS INITIAL.
*    CLEAR:L1,L2,L3,L4.   "初始导入版本数量
*    LOOP AT GT_CH_OUTPUT.
*
*      IF GT_CH_OUTPUT-POSID IS NOT INITIAL.
*        LS_WBS_COSTLINES-WBS_NAME = GT_CH_OUTPUT-POSID.                   "WBS
*        LS_WBS_COSTLINES1-WBS_NAME = GT_CH_OUTPUT-POSID.                  "WBS
*      ELSE.
*        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
*        CONTINUE.
*      ENDIF.
*
*      GT_CH_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态
*
*      LS_ITEM-RESOURCE-TYPPS = GT_CH_OUTPUT-XMLB.                        "项目类别
*      LS_ITEM1-RESOURCE-TYPPS = GT_CH_OUTPUT-XMLB.                       "项目类别
*
*      IF GT_CH_OUTPUT-WERKS IS INITIAL.
*        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
*        LS_ITEM1-RESOURCE-WERKS = 1800.                                  "工厂
*      ELSE.
*        LS_ITEM-RESOURCE-WERKS = GT_CH_OUTPUT-WERKS.
*        LS_ITEM1-RESOURCE-WERKS = GT_CH_OUTPUT-WERKS.
*      ENDIF.
*
*      IF GT_CH_OUTPUT-XMLB = 'M'.
*        IF GT_CH_OUTPUT-MATNR IS NOT INITIAL.
*          LS_ITEM-RESOURCE-MATNR = GT_CH_OUTPUT-MATNR.                       "物料号
*          LS_ITEM1-RESOURCE-MATNR = GT_CH_OUTPUT-MATNR.                       "物料号
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-RESOURCE-MATNR = GT_CH_OUTPUT-MATNR.                       "物料号
*        LS_ITEM1-RESOURCE-MATNR = GT_CH_OUTPUT-MATNR.                       "物料号
*      ENDIF.
*
*      REFRESH GT_MEINS[].
*      CLEAR GT_MEINS.
*      IF GT_CH_OUTPUT-MATNR IS NOT INITIAL.
*        SELECT MEINS
*              FROM MARA
*              INTO TABLE GT_MEINS
*              WHERE MATNR = GT_CH_OUTPUT-MATNR.
*      ENDIF.
*
*      READ TABLE GT_MEINS.
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                 "单位
*        LS_ITEM1-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.
*      LS_ITEM-DESCRIPT       = GT_CH_OUTPUT-MAKTX.                       "物料描述
*      LS_ITEM1-DESCRIPT       = GT_CH_OUTPUT-MAKTX.                       "物料描述
*
*      LS_ITEM1-PRICE-FIXED    = GT_CH_OUTPUT-FPREIS.                      "人工单价
*
*      IF GT_CH_OUTPUT-XMLB = 'V'.
*        IF GT_CH_OUTPUT-CBYS IS NOT INITIAL.
*          LS_ITEM-COST_ELEM = GT_CH_OUTPUT-CBYS.                          "成本要素
*          LS_ITEM1-COST_ELEM = GT_CH_OUTPUT-CBYS.                          "成本要素
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-COST_ELEM = GT_CH_OUTPUT-CBYS.                             "成本要素
*        LS_ITEM1-COST_ELEM = GT_CH_OUTPUT-CBYS.                             "成本要素
*      ENDIF.
*
*      READ TABLE GT_HB INTO GS_HB WITH KEY MATNR = GT_CH_OUTPUT-MATNR.
*      IF SY-SUBRC = 0.
*        GT_CH_OUTPUT-HPEINH = GS_HB-HPEINH.
*        GT_CH_OUTPUT-JPEINH = GS_HB-JPEINH.
*      ENDIF.
*
*      LS_ITEM-QUANTITY-QUANTITY = GT_CH_OUTPUT-HPEINH.                   "合同工程量
*      LS_ITEM-PRICE-TOTAL = GT_CH_OUTPUT-HGPREIS.                        "合同单价
*      LS_ITEM1-QUANTITY-QUANTITY = GT_CH_OUTPUT-JPEINH.                   "计划工程量
*      LS_ITEM1-PRICE-TOTAL = GT_CH_OUTPUT-CGPREIS.                     "材料单价
*      LS_ITEM-CURRENCY = 'CNY'.
*      LS_ITEM1-CURRENCY = 'CNY'.
*      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*
*      APPEND LS_ITEM1 TO LS_WBS_COSTLINES1-COST_LINES.
*      APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
*
*      "合同
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
*
*      APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
*
*
*      CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
*        EXPORTING
*          I_PROJ_ID       = GT_CH_OUTPUT-POSID                          "项目编号
*          I_VERSION       = P_BB2                                       "版本
*          I_COST_VARIANT  = 'PS06'                                      "核算变式
*          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*        IMPORTING
*          MESSAGES        = GT_RETURN.
*
*      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC <> 0.
*        L4 = L4 + 1. "统计导入100版本后最新版本导入数量          ADD IT02 20160307
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
*        CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
*          EXPORTING
*            I_PROJ_ID       = GT_CH_OUTPUT-POSID                          "项目编号
*            I_VERSION       = P_BB1                                       "版本
*            I_COST_VARIANT  = 'PS06'                                      "核算变式
*            I_WBS_COSTLINES = LT_WBS_COSTLINES1                           "内表
*          IMPORTING
*            MESSAGES        = GT_RETURN1.
*
*        READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
*        IF SY-SUBRC <> 0 .
*          L3 = L3 + 1. "统计导入000版本后最新版本导入数量          ADD IT02 20160307
*          GT_CH_OUTPUT-TYPE = ICON_GREEN_LIGHT.      "如果两个都更新成功，那么绿灯，而且0版本和100版本也会更新成功，只是消息会报目前两个是否成功
*
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              WAIT = 'X'.
*
*          CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
*            EXPORTING
*              I_PROJ_ID       = GT_CH_OUTPUT-POSID                           "项目定义
*              I_VERSION       = '100'                                         "版本
*              I_COST_VARIANT  = 'PS06'                                      "核算变式
*              I_WBS_COSTLINES = LT_WBS_COSTLINES5                            "内表
*            IMPORTING
*              MESSAGES        = GT_RETURN2.
*
*          READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
*          IF SY-SUBRC <> 0 .
*            L2 = L2 + 1. "统计导入100版本导入数量          ADD IT02 20160307
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                WAIT = 'X'.
*          ELSE.
*            GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，
*            CONCATENATE '100版本' GS_RETURN2-MESSAGE INTO GT_CH_OUTPUT-MESSAGE.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          ENDIF.
*
*          CALL FUNCTION 'CNECP_MAINTAIN'          "创建0版本
*            EXPORTING
*              I_PROJ_ID       = GT_CH_OUTPUT-POSID                           "项目定义
*              I_VERSION       = '000'                                         "版本
*              I_COST_VARIANT  = 'PS06'                                      "核算变式
*              I_WBS_COSTLINES = LT_WBS_COSTLINES6                            "内表
*            IMPORTING
*              MESSAGES        = GT_RETURN3.
*
*          READ TABLE GT_RETURN3 INTO GS_RETURN3 WITH KEY TYPE = 'E'.
*          IF SY-SUBRC <> 0 .
*            L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                WAIT = 'X'.
*          ELSE.
*            GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，
*            CONCATENATE '000版本' GT_CH_OUTPUT-MESSAGE  GS_RETURN3-MESSAGE  INTO GT_CH_OUTPUT-MESSAGE.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          ENDIF.
*
*
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_CH_OUTPUT-MESSAGE.
*
*        ELSE.
*
*          GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_CH_OUTPUT-MESSAGE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，同时不更新0版本和100版本
*        GT_CH_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
*
*      ENDIF.
*
*      MODIFY GT_CH_OUTPUT TRANSPORTING HPEINH JPEINH TYPE MESSAGE.
*
*      CLEAR:LS_ITEM,LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES,GT_CH_OUTPUT.
*      REFRESH:LT_WBS_COSTLINES,LT_WBS_COSTLINES5,LT_WBS_COSTLINES6.
*      CLEAR:LS_ITEM1,LS_WBS_COSTLINES1-COST_LINES,LS_WBS_COSTLINES1.
*      REFRESH LT_WBS_COSTLINES1.
*      CLEAR:GS_RETURN,GS_RETURN1.
*      REFRESH:GT_RETURN,GT_RETURN1,GT_RETURN2,GT_RETURN3.
*
*    ENDLOOP.
*  ELSEIF IT_CKIS IS NOT INITIAL.
*    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*  ELSEIF IT_CKIS1 IS NOT INITIAL.
*    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 12:43:33  END


*&--代码添加 BY HANDYBY 23.05.2017 15:40:38  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_CH_OUTPUT[]
   WHERE POSID = GT_CH_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 15:40:38  END


*&--代码添加 BY HANDYBY 28.04.2017 12:44:05  BEGIN
  IF IT_CKIS1 IS INITIAL AND IT_CKIS IS INITIAL.
    CLEAR:L1,L2,L3,L4.   "初始导入版本数量

    DATA:L_INDEX TYPE SY-TABIX.
    REFRESH LT_WBS_COSTLINES.
    REFRESH LT_WBS_COSTLINES1.
    REFRESH LT_WBS_COSTLINES5.
    REFRESH LT_WBS_COSTLINES6.

    TYPES:BEGIN OF TY_CH_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            HPEINH       TYPE MENGE_POS,              "合同工程量
            HGPREIS      TYPE CKIS-GPREIS,             "合同单价
            JPEINH       TYPE MENGE_POS,              "计划工程量
            CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
            FPREIS       TYPE CKIS-FPREIS,             "人工单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_CH_OUTPUT2.
    DATA:LT_CH_OUTPUT TYPE TABLE OF TY_CH_OUTPUT2,
         LS_CH_OUTPUT TYPE TY_CH_OUTPUT2.
    DATA LS_CH_OUTPUT2 TYPE TY_CH_OUTPUT2 .

    MOVE-CORRESPONDING GT_CH_OUTPUT[] TO LT_CH_OUTPUT .
    SORT LT_CH_OUTPUT BY POSID  .
    LOOP AT LT_CH_OUTPUT INTO LS_CH_OUTPUT2 .
      CLEAR:LS_ITEM,LS_ITEM1.
      L_INDEX = SY-TABIX.

      MOVE-CORRESPONDING LS_CH_OUTPUT2 TO LS_CH_OUTPUT .

*      AT FIRST .
*        READ TABLE GT_CH_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_CH_OUTPUT-POSID.                   "WBS
*      LS_WBS_COSTLINES1-WBS_NAME = GT_CH_OUTPUT-POSID.                  "WBS
*        ENDIF.
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_CH_OUTPUT-POSID.                   "WBS
        LS_WBS_COSTLINES1-WBS_NAME = LS_CH_OUTPUT-POSID.                  "WBS
      ENDAT .

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号
      LS_ITEM1-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-MATNR = LS_CH_OUTPUT-MATNR.                       "物料号
      LS_ITEM1-RESOURCE-MATNR = LS_CH_OUTPUT-MATNR.                       "物料号

      LS_CH_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

      IF  LS_ITEM1-RESOURCE-MATNR IS NOT INITIAL.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_CH_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
          LS_ITEM1-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                "单位
        ENDIF.
      ENDIF.

      IF LS_CH_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_CH_OUTPUT-CBYS.                          "成本要素
        LS_ITEM1-COST_ELEM = LS_CH_OUTPUT-CBYS.                          "成本要素
      ENDIF.

      LS_ITEM-RESOURCE-TYPPS = LS_CH_OUTPUT-XMLB.                        "项目类别
      LS_ITEM1-RESOURCE-TYPPS = LS_CH_OUTPUT-XMLB.                       "项目类别

      IF LS_CH_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 15:42:09  BEGIN
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_CH_OUTPUT-POSID BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_ITEM-RESOURCE-WERKS = LS_PRPS-WERKS .
          LS_ITEM1-RESOURCE-WERKS = LS_PRPS-WERKS.
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:42:09  END
      ELSE.
        LS_ITEM-RESOURCE-WERKS = LS_CH_OUTPUT-WERKS.
        LS_ITEM1-RESOURCE-WERKS = LS_CH_OUTPUT-WERKS.
      ENDIF.

      LS_ITEM-DESCRIPT       = LS_CH_OUTPUT-MAKTX.                       "物料描述
      LS_ITEM1-DESCRIPT       = LS_CH_OUTPUT-MAKTX.                       "物料描述

      LS_ITEM1-PRICE-FIXED    = LS_CH_OUTPUT-FPREIS.                      "人工单价

      READ TABLE GT_HB INTO GS_HB WITH KEY MATNR = LS_CH_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0.
        LS_CH_OUTPUT-HPEINH = GS_HB-HPEINH.
        LS_CH_OUTPUT-JPEINH = GS_HB-JPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_CH_OUTPUT-HPEINH.                   "合同工程量
      LS_ITEM-PRICE-TOTAL = LS_CH_OUTPUT-HGPREIS.                        "合同单价
      LS_ITEM1-QUANTITY-QUANTITY = LS_CH_OUTPUT-JPEINH.                   "计划工程量
      LS_ITEM1-PRICE-TOTAL = LS_CH_OUTPUT-CGPREIS.                     "材料单价
      LS_ITEM-CURRENCY = 'CNY'.
      LS_ITEM1-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
      APPEND LS_ITEM1 TO LS_WBS_COSTLINES1-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
*      ENDAT .

      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
        CLEAR LS_WBS_COSTLINES .
        CLEAR LS_WBS_COSTLINES1 .
      ENDAT .

      CLEAR LS_ITEM .
      CLEAR LS_ITEM1 .
      CLEAR LS_CH_OUTPUT .
      CLEAR LS_CH_OUTPUT2 .


    ENDLOOP.

    REFRESH GT_RETURN.
    REFRESH GT_RETURN1.
    REFRESH GT_RETURN2.
    REFRESH GT_RETURN3.
    CLEAR GS_RETURN .
    CLEAR GS_RETURN1 .
    CLEAR GS_RETURN2 .
    CLEAR GS_RETURN3 .
    READ TABLE GT_CH_OUTPUT INDEX 1 .

    DATA: BEGIN OF LS_PRPS5,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS5 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS5
      FROM PRPS
     WHERE POSID = GT_CH_OUTPUT-POSID .
    IF LS_PRPS5 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS5-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:43:44  BEGIN
    DATA L_FLAG1 TYPE I VALUE IS INITIAL  .
    DATA L_FLAG2 TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 15:43:44  END

*&--代码添加 BY HANDYBY 23.05.2017 15:56:01  BEGIN
********************************************************************** 000-099
*&--代码添加 BY HANDYBY 23.05.2017 15:56:01  END

    CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
        I_VERSION       = P_BB2                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.

    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
*      L4 = L4 + 1. "统计导入100版本后最新版本导入数量          ADD IT02 20160307
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES5                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.

      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0 .
*          L2 = L2 + 1. "统计导入100版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        L_FLAG1 = 0 .

      ELSE.
*          GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，
*          CONCATENATE '100版本' GS_RETURN2-MESSAGE INTO GT_CH_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        L_FLAG1 = 1 .
        LOOP AT GT_RETURN2 INTO GS_RETURN2 .
          CONCATENATE ZSTR GS_RETURN2-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN2 .
        ENDLOOP.

      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，同时不更新0版本和100版本
*      GT_CH_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.

      L_FLAG1 = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.

    ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:55:45  BEGIN
******************************************************************** 100-199
*&--代码添加 BY HANDYBY 23.05.2017 15:55:45  END
    CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
        I_VERSION       = P_BB1                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES1                           "内表
      IMPORTING
        MESSAGES        = GT_RETURN1.

    READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0 .
*        L3 = L3 + 1. "统计导入000版本后最新版本导入数量          ADD IT02 20160307
*        GT_CH_OUTPUT-TYPE = ICON_GREEN_LIGHT.      "如果两个都更新成功，那么绿灯，而且0版本和100版本也会更新成功，只是消息会报目前两个是否成功

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'          "创建0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES6                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN3.

      READ TABLE GT_RETURN3 INTO GS_RETURN3 WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0 .
*          L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        L_FLAG2 = 0 .

      ELSE.
*          GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT. "报出相应的错误，
*          CONCATENATE '000版本' GT_CH_OUTPUT-MESSAGE  GS_RETURN3-MESSAGE  INTO GT_CH_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        L_FLAG2 = 1 .
        LOOP AT GT_RETURN3 INTO GS_RETURN3 .
          CONCATENATE ZSTR GS_RETURN3-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN3 .
        ENDLOOP.

      ENDIF.

    ELSE.

*        GT_CH_OUTPUT-TYPE = ICON_RED_LIGHT.
*        CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_CH_OUTPUT-MESSAGE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      L_FLAG2 = 2 .
      LOOP AT GT_RETURN1 INTO GS_RETURN1 .
        CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN1 .
      ENDLOOP.

    ENDIF.

  ELSEIF IT_CKIS IS NOT INITIAL.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ELSEIF IT_CKIS1 IS NOT INITIAL.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*&--代码添加 BY HANDYBY 28.04.2017 12:44:05  END



  "增加消息提示
  MSG000 = L_INDEX .
  CONDENSE MSG000 NO-GAPS.
  MSG001 = L_INDEX .
  CONDENSE MSG001 NO-GAPS.

  MSG100 = L_INDEX .
  CONDENSE MSG100 NO-GAPS.
  MSG101 = L_INDEX .
  CONDENSE MSG101 NO-GAPS.

  IF L_FLAG1 = 0.
    "100版本
    CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
    "100后版本
    CONCATENATE P_BB2 '版本已成功导入' MSG101 '条数量' INTO  MSG101.
  ELSEIF L_FLAG1 = 1 .
    CONCATENATE  P_BB2 '版本已成功导入:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本导入失败:'  MSG100 '条数量' INTO MSG100 .
  ELSEIF L_FLAG1 = 2 .
    CONCATENATE  P_BB2 '版本导入失败:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本没有执行导入动作！' '' INTO MSG100 .
  ENDIF.

  IF L_FLAG2 = 0.
    "000版本
    CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
    "000后版本
    CONCATENATE P_BB1 '版本已成功导入' MSG001 '条数量' INTO  MSG001.
  ELSEIF L_FLAG2 = 1 .
    CONCATENATE  P_BB1 '版本已成功导入:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本导入失败:'  MSG000 '条数量' INTO MSG000 .
  ELSEIF L_FLAG2 = 2 .
    CONCATENATE  P_BB1 '版本导入失败:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本没有执行导入动作！' '' INTO MSG000 .
  ENDIF.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .

  "000版本
*  MSG000 = L1 .
*  CONDENSE MSG000 NO-GAPS.
*  CONCATENATE '0版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
*  "100版本
*  MSG100 = L2 .
*  CONDENSE MSG100 NO-GAPS.
*  CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
*  "000后版本
*  MSG001 = L3 .
*  CONDENSE MSG001 NO-GAPS.
*  CONCATENATE P_BB1   '版本已成功导入' MSG001 '条数量' INTO  MSG001.
*  "100后版本
*  MSG101 = L4 .
*  CONDENSE MSG101 NO-GAPS.
*  CONCATENATE P_BB2  '版本已成功导入'  MSG101 '条数量' INTO MSG101.
*  CALL SCREEN 0101 STARTING AT 25 10.


  CLEAR:P_BB1,P_BB2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_CZ_OUTPUT .
  CLEAR :SC_LEN .
  SELECT PSPHI                                                        "取出了WBS元素对应的项目定义
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_CZ_OUTPUT
    WHERE POSID = GT_CZ_OUTPUT-POSID.

  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 16:07:38  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 16:07:38  END
    SELECT TOPNR VERSN                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 16:07:54  BEGIN
      FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 16:07:54  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN1                        "取一个对象编号下对应的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID
        WHERE TOPNR = GT_OBJID-TOPNR
        AND VERSN BETWEEN '000' AND '099'.
    ENDIF.

  ELSE.
    MESSAGE 'WBS元素没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH:GT_OBJID,GT_PSPHI.

  DATA:P_BB1 TYPE PRECP1-VERSN.

  SORT GT_VERSN1 BY VERSN.

  IF GT_VERSN1[] IS NOT INITIAL.
    LOOP AT GT_VERSN1 WHERE VERSN >= '000' AND VERSN <= '099'.
*    if gt_versn1-versn is not initial.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN1-VERSN = GT_VERSN1-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN1-VERSN
          IMPORTING
            OUTPUT = GT_VERSN1-VERSN.

        P_BB1 = GT_VERSN1-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB1 = '001'.
  ENDIF.


*&--代码添加 BY HANDYBY 23.05.2017 16:08:21  BEGIN
**********************************************************************  开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:08:21  END

  SORT GT_CZ_OUTPUT BY MATNR.

  REFRESH IT_JG1.
  LOOP AT GT_CZ_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_CZ_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG1-MATNR = GT_CZ_OUTPUT-MATNR.
        WA_JG1-CLDJ = GT_CZ_OUTPUT-CGPREIS.
        WA_JG1-RGDJ = GT_CZ_OUTPUT-FPREIS.
        APPEND WA_JG1 TO IT_JG1.
      ENDIF.
    ENDAT.
    CLEAR GT_CZ_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ1.
  LOOP AT GT_CZ_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ.
    WA_JGHJ1-MATNR = GT_CZ_OUTPUT-MATNR.
    WA_JGHJ1-CGPREIS = GT_CZ_OUTPUT-CGPREIS.
    WA_JGHJ1-FPREIS = GT_CZ_OUTPUT-FPREIS.
    COLLECT WA_JGHJ1 INTO IT_JGHJ1.
  ENDLOOP.

  LOOP AT IT_JG1 INTO WA_JG1.
    READ TABLE IT_JGHJ1 INTO WA_JGHJ1 WITH KEY MATNR = WA_JG1-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_CZ_OUTPUT WHERE MATNR = WA_JGHJ1-MATNR.
        G = G + 1.
      ENDLOOP.
      LL = WA_JGHJ1-CGPREIS / G.
      JJ = WA_JGHJ1-FPREIS / G.


      IF LL <> WA_JG1-CLDJ.
        CONCATENATE '物料' WA_JGHJ1-MATNR '的材料单价不一致！' INTO MESSAGE2.
        MESSAGE MESSAGE2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF JJ <> WA_JG1-RGDJ.
        CONCATENATE '物料' WA_JGHJ1-MATNR '的人工单价不一致！' INTO MESSAGE3.
        MESSAGE MESSAGE3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG1,W,LL,JJ,MESSAGE2,MESSAGE3.
  ENDLOOP.


*&--代码添加 BY HANDYBY 23.05.2017 16:08:40  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:08:40  END

  DELETE GT_CZ_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_CZ_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:08:55  BEGIN
*  这里GT_JHGCL取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 16:08:55  END
  REFRESH GT_JHGCL.
  LOOP AT GT_CZ_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_JHGCL.
    GS_JHGCL-MATNR = GT_CZ_OUTPUT-MATNR.
    GS_JHGCL-JPEINH = GT_CZ_OUTPUT-JPEINH.
    COLLECT GS_JHGCL INTO GT_JHGCL.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:09:46  BEGIN
  SORT GT_JHGCL BY MATNR .
*&--代码添加 BY HANDYBY 23.05.2017 16:09:46  END

  LOOP AT GT_CZ_OUTPUT WHERE MATNR = ''.
    APPEND GT_CZ_OUTPUT TO GT_CZ_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_CZ_OUTPUT COMPARING MATNR.

  DELETE GT_CZ_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_CZ_OUTPUT1.
    APPEND GT_CZ_OUTPUT1 TO GT_CZ_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_CZ_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的将被导入一次，且将合计相同物料的计划工程量！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.


*&--代码添加 BY HANDYBY 28.04.2017 13:47:24  BEGIN
  "先检查一下可控的行错误信息.

  LOOP AT GT_CZ_OUTPUT .

    IF GT_CZ_OUTPUT-POSID IS INITIAL.

      GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_CZ_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GT_CZ_OUTPUT-XMLB = 'M'.
      IF GT_CZ_OUTPUT-MATNR IS  INITIAL. .
        GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GT_CZ_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 16:11:04  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GT_CZ_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GT_CZ_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:11:04  END

*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  BEGIN
* 物料号变大写
      TRANSLATE GT_CZ_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GT_CZ_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GT_CZ_OUTPUT-WERKS '工厂' '不存在:'  GT_CZ_OUTPUT-MATNR '物料号' INTO    GT_CZ_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.

    IF GT_CZ_OUTPUT-XMLB = 'V' AND GT_CZ_OUTPUT-CBYS IS  INITIAL..

      GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_CZ_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_CZ_OUTPUT .
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 13:47:24  END


*  ........................add IT02 20160307begin ...................... *
  READ TABLE GT_CZ_OUTPUT WITH KEY TYPE = ICON_RED_LIGHT .
  IF SY-SUBRC NE 0 .
    CLEAR:PROJ_OBJNR,PRPS_OBJNR.

    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_CZ_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_CZ_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_CZ_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '000'.

          IF LS_KALNR IS NOT INITIAL .

            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.


*&--代码注释 BY HANDYBY 02.06.2017 16:22:54  BEGIN
*    SELECT SINGLE  OBJNR
*        INTO PROJ_OBJNR
*        FROM PROJ
*        WHERE PSPID = GT_CZ_OUTPUT-POSID.
*    SELECT SINGLE OBJNR
*        INTO PRPS_OBJNR
*        FROM  PRPS
*        WHERE POSID = GT_CZ_OUTPUT-POSID.
*&--代码注释 BY HANDYBY 02.06.2017 16:22:54  END
*&--代码添加 BY HANDYBY 02.06.2017 16:23:05  BEGIN


*&--代码添加 BY HANDYBY 02.06.2017 16:23:05  END

    REFRESH: LT_WBS_COSTLINES3,GT_RETURN2,LS_WBS_COSTLINES3-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN2.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.
      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除000版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.

      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除0版本失败：' GS_RETURN2-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
*  ENDIF.
    "再重新读取000版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
           FROM CKIS
           INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
           WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.

  ELSE .
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 13:54:01  BEGIN
*

*  IF IT_CKIS IS  INITIAL.
*    CLEAR:L1,L2,L3,L4.   "初始导入版本数量
*    LOOP AT GT_CZ_OUTPUT.
*
*      IF GT_CZ_OUTPUT-POSID IS NOT INITIAL.
*        LS_WBS_COSTLINES-WBS_NAME = GT_CZ_OUTPUT-POSID.                  "WBS
*      ELSE.
*        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
*        CONTINUE.
*      ENDIF.
*
*      GT_CZ_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态
*
*      LS_ITEM-RESOURCE-TYPPS = GT_CZ_OUTPUT-XMLB.                        "项目类别
*
*      IF GT_CZ_OUTPUT-WERKS IS INITIAL.
*        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
*      ELSE.
*        LS_ITEM-RESOURCE-WERKS = GT_CZ_OUTPUT-WERKS.
*      ENDIF.
*
*      IF GT_CZ_OUTPUT-XMLB = 'M'.
*        IF GT_CZ_OUTPUT-MATNR IS NOT INITIAL.
*          LS_ITEM-RESOURCE-MATNR = GT_CZ_OUTPUT-MATNR.                  "物料号
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-RESOURCE-MATNR = GT_CZ_OUTPUT-MATNR.                    "物料编码
*      ENDIF.
*
*      REFRESH GT_MEINS[].
*      CLEAR GT_MEINS.
*      IF GT_CZ_OUTPUT-MATNR IS NOT INITIAL.
*        SELECT MEINS
*          FROM MARA
*          INTO TABLE GT_MEINS
*          WHERE MATNR = GT_CZ_OUTPUT-MATNR.
*      ENDIF.
*
*      READ TABLE GT_MEINS.
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.
*      LS_ITEM-DESCRIPT       = GT_CZ_OUTPUT-MAKTX.                       "物料描述
*
*      READ TABLE GT_JHGCL INTO GS_JHGCL WITH KEY MATNR = GT_CZ_OUTPUT-MATNR.
*      IF SY-SUBRC = 0.
*        GT_CZ_OUTPUT-JPEINH = GS_JHGCL-JPEINH.
*      ENDIF.
*
*      LS_ITEM-QUANTITY-QUANTITY = GT_CZ_OUTPUT-JPEINH.                   "计划工程量
*      LS_ITEM-PRICE-TOTAL    = GT_CZ_OUTPUT-CGPREIS.                     "材料单价
*      LS_ITEM-PRICE-FIXED    = GT_CZ_OUTPUT-FPREIS.                      "人工单价
*
*      IF GT_CZ_OUTPUT-XMLB = 'V'.
*        IF GT_CZ_OUTPUT-CBYS IS NOT INITIAL.
*          LS_ITEM-COST_ELEM = GT_CZ_OUTPUT-CBYS.                          "成本要素
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-COST_ELEM = GT_CZ_OUTPUT-CBYS.                             "成本要素
*      ENDIF.
*      LS_ITEM-CURRENCY = 'CNY'.
*      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*
*      CALL FUNCTION 'CNECP_MAINTAIN'
*        EXPORTING
*          I_PROJ_ID       = GT_CZ_OUTPUT-POSID                           "项目定义
*          I_VERSION       = P_BB1                                       "版本
*          I_COST_VARIANT  = 'PS06'                                      "核算变式
*          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*        IMPORTING
*          MESSAGES        = GT_RETURN.
*
*      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC NE 0.
*        L3 = L3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
*        GT_C_OUTPUT-TYPE = ICON_GREEN_LIGHT.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
*        CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
*          EXPORTING
*            I_PROJ_ID       = GT_CZ_OUTPUT-POSID                           "项目定义
*            I_VERSION       = '000'                                         "版本
*            I_COST_VARIANT  = 'PS06'                                      "核算变式
*            I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*          IMPORTING
*            MESSAGES        = GT_RETURN1.
*        READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
*        IF SY-SUBRC NE 0.
*          L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              WAIT = 'X'.
*        ELSE.
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_CZ_OUTPUT-MESSAGE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*
*
*      ELSE.
*        GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.
*        GT_CZ_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      ENDIF.
*
*      MODIFY GT_CZ_OUTPUT TRANSPORTING JPEINH TYPE MESSAGE.
*
*      CLEAR GT_CZ_OUTPUT.
*
*      CLEAR:LS_ITEM,LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES.
*      REFRESH LT_WBS_COSTLINES.
*      CLEAR:GS_RETURN.
*      REFRESH:GT_RETURN.
*
*    ENDLOOP.
*  ELSE.
**    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*    "增加消息提示
*    "000版本
**    MSG000 = L1 .
**    CONDENSE MSG000 NO-GAPS.
**    CONCATENATE '0版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
**    "000后版本
**    MSG001 = L3 .
**    CONDENSE MSG001 NO-GAPS.
**    CONCATENATE P_BB1   '版本已成功导入' MSG001 '条数量' INTO  MSG001.
**    CALL SCREEN 0101 STARTING AT 25 10.
*  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 13:54:01  END

*&--代码添加 BY HANDYBY 23.05.2017 16:12:44  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_CZ_OUTPUT[]
   WHERE POSID = GT_CZ_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 16:12:44  END

*&--代码添加 BY HANDYBY 28.04.2017 13:54:36  BEGIN

  IF IT_CKIS IS  INITIAL.
    CLEAR:L1,L2,L3,L4.   "初始导入版本数量
    CLEAR GT_CZ_OUTPUT.
    REFRESH LT_WBS_COSTLINES.

    DATA:L_INDEX TYPE SY-TABIX.
    TYPES:BEGIN OF TY_C_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            JPEINH       TYPE MENGE_POS,              "计划工程量
            CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
            FPREIS       TYPE CKIS-FPREIS,             "人工单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_C_OUTPUT2.
    DATA:LT_CZ_OUTPUT TYPE TABLE OF TY_C_OUTPUT2,
         LS_CZ_OUTPUT TYPE TY_C_OUTPUT2.
    DATA LS_CZ_OUTPUT2 TYPE TY_C_OUTPUT2 .

    MOVE-CORRESPONDING GT_CZ_OUTPUT[] TO LT_CZ_OUTPUT .
    SORT LT_CZ_OUTPUT BY POSID  .
    LOOP AT LT_CZ_OUTPUT INTO LS_CZ_OUTPUT2 .
      CLEAR:LS_ITEM.
      L_INDEX = SY-TABIX .

      MOVE-CORRESPONDING LS_CZ_OUTPUT2 TO LS_CZ_OUTPUT .
*      AT FIRST .
*        READ TABLE GT_CZ_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_CZ_OUTPUT-POSID.                   "WBS
*        ENDIF.
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_CZ_OUTPUT-POSID.                   "WBS
      ENDAT .

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-MATNR = LS_CZ_OUTPUT-MATNR.                       "物料号

      LS_CZ_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

      LS_ITEM-RESOURCE-TYPPS = LS_CZ_OUTPUT-XMLB.                        "项目类别

      IF  LS_ITEM-RESOURCE-MATNR IS NOT INITIAL.
        CLEAR GS_CHECK_MATNR.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_CZ_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
        ENDIF.
      ENDIF.

      IF LS_CZ_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_CZ_OUTPUT-CBYS.                          "成本要素
      ENDIF.

      IF LS_CZ_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 16:14:35  BEGIN
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_CZ_OUTPUT-POSID BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_ITEM-RESOURCE-WERKS = LS_PRPS-WERKS .                                  "工厂
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:14:35  END                           "工厂
      ELSE.
        LS_ITEM-RESOURCE-WERKS = LS_CZ_OUTPUT-WERKS.
      ENDIF.

      LS_ITEM-DESCRIPT       = LS_CZ_OUTPUT-MAKTX.                       "物料描述

      LS_ITEM-PRICE-FIXED    = LS_CZ_OUTPUT-FPREIS.                      "人工单价

      READ TABLE GT_JHGCL INTO GS_JHGCL WITH KEY MATNR = LS_CZ_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0.
        LS_CZ_OUTPUT-JPEINH = GS_JHGCL-JPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_CZ_OUTPUT-JPEINH.                   "计划工程量
      LS_ITEM-PRICE-TOTAL    = LS_CZ_OUTPUT-CGPREIS.                     "材料单价

      LS_ITEM-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*      ENDAT .

      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        CLEAR LS_WBS_COSTLINES .
      ENDAT .

      CLEAR LS_ITEM .
      CLEAR LS_CZ_OUTPUT2.
      CLEAR LS_CZ_OUTPUT .

    ENDLOOP.

    REFRESH:GT_RETURN.
    REFRESH:GT_RETURN1.
    READ TABLE GT_CZ_OUTPUT INDEX 1 .
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS4 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS4
      FROM PRPS
     WHERE POSID = GT_CZ_OUTPUT-POSID .
    IF LS_PRPS4 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS4-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:15:13  BEGIN
    DATA L_FLAG TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 16:15:13  END
    CALL FUNCTION 'CNECP_MAINTAIN'
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
        I_VERSION       = P_BB1                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.

    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC NE 0.
*      L3 = L3 + 1. "统计导入0版本后成功执行条数  add it02 20160307
*      GT_C_OUTPUT-TYPE = ICON_GREEN_LIGHT.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'            "创建0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN1.
      READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
*        L1 = L1 + 1. "统计导入000版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

*&--代码添加 BY HANDYBY 23.05.2017 16:15:32  BEGIN
        L_FLAG = 0 .
*&--代码添加 BY HANDYBY 23.05.2017 16:15:32  END
      ELSE.
*        CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_CZ_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*&--代码添加 BY HANDYBY 23.05.2017 16:15:44  BEGIN
        L_FLAG = 1 .
        LOOP AT GT_RETURN1 INTO GS_RETURN1 .
          CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN1 .
        ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 16:15:44  END
      ENDIF.

    ELSE.
*      GT_CZ_OUTPUT-TYPE = ICON_RED_LIGHT.
*      GT_CZ_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*&--代码添加 BY HANDYBY 23.05.2017 16:15:54  BEGIN
      L_FLAG = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 16:15:54  END
    ENDIF.

  ELSE.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*&--代码添加 BY HANDYBY 28.04.2017 13:54:36  END



*&--代码添加 BY HANDYBY 23.05.2017 16:16:38  BEGIN
  "增加消息提示

  MSG000 = L_INDEX .
  CONDENSE MSG000 NO-GAPS.
  MSG001 = L_INDEX .
  CONDENSE MSG001 NO-GAPS.

  IF L_FLAG = 0.
    "000版本
    CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
    "000后版本
    CONCATENATE P_BB1 '版本已成功导入' MSG001 '条数量' INTO  MSG001.
  ELSEIF L_FLAG = 1 .
    CONCATENATE  P_BB1 '版本已成功导入:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本导入失败:'  MSG000 '条数量' INTO MSG000 .
  ELSEIF L_FLAG = 2 .
    CONCATENATE  P_BB1 '版本导入失败:'   MSG001 '条数量' INTO MSG001 .
    CONCATENATE '000版本没有执行导入动作！' '' INTO MSG000 .
  ENDIF.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .
*&--代码添加 BY HANDYBY 23.05.2017 16:16:38  END

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_HZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_HZ_OUTPUT .
  CLEAR :SC_LEN .
  SELECT PSPHI
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_HZ_OUTPUT
    WHERE POSID = GT_HZ_OUTPUT-POSID.

  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 16:25:28  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 16:25:28  END
    SELECT TOPNR VERSN                                            "取出了项目定义相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 16:25:38  BEGIN
* FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 16:25:38  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.
    IF GT_OBJID[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN2                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID
        WHERE TOPNR = GT_OBJID-TOPNR
        AND VERSN BETWEEN '100' AND '199'.
    ENDIF.
  ELSE.
    MESSAGE 'WBS没有对应的项目定义' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  REFRESH GT_OBJID.

  DATA:P_BB2 TYPE PRECP1-VERSN.

  SORT GT_VERSN2 BY VERSN.

  IF GT_VERSN2[] IS NOT INITIAL.
    LOOP AT GT_VERSN2 WHERE VERSN >= '100' AND VERSN <= '199'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN2-VERSN = GT_VERSN2-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN2-VERSN
          IMPORTING
            OUTPUT = GT_VERSN2-VERSN.

        P_BB2 = GT_VERSN2-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB2 = 101.
  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 16:26:01  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:26:01  END
  SORT GT_HZ_OUTPUT BY MATNR.

  REFRESH IT_JG2.
  LOOP AT GT_HZ_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_HZ_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG2-MATNR = GT_HZ_OUTPUT-MATNR.
        WA_JG2-HTDJ = GT_HZ_OUTPUT-HGPREIS.
        APPEND WA_JG2 TO IT_JG2.
      ENDIF.
    ENDAT.
    CLEAR GT_HZ_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ2.
  LOOP AT GT_HZ_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ2.
    WA_JGHJ2-MATNR = GT_HZ_OUTPUT-MATNR.
    WA_JGHJ2-HGPREIS = GT_HZ_OUTPUT-HGPREIS.
    COLLECT WA_JGHJ2 INTO IT_JGHJ2.
  ENDLOOP.

  LOOP AT IT_JG2 INTO WA_JG2.
    READ TABLE IT_JGHJ2 INTO WA_JGHJ2 WITH KEY MATNR = WA_JG2-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_HZ_OUTPUT WHERE MATNR = WA_JGHJ2-MATNR.
        G = G + 1.
      ENDLOOP.
      W = WA_JGHJ2-HGPREIS / G.


      IF W <> WA_JG2-HTDJ.
        CONCATENATE '物料' WA_JGHJ2-MATNR '的合同单价不一致！' INTO MESSAGE1.
        MESSAGE MESSAGE1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG2,W,MESSAGE1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:26:14  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:26:14  END
  DELETE GT_HZ_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_HZ_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:26:26  BEGIN
* 这里GT_HTGCL取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 16:26:26  END
  REFRESH GT_HTGCL.
  LOOP AT GT_HZ_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_HTGCL.
    GS_HTGCL-MATNR = GT_HZ_OUTPUT-MATNR.
    GS_HTGCL-HPEINH = GT_HZ_OUTPUT-HPEINH.
    COLLECT GS_HTGCL INTO GT_HTGCL.
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 14:05:03  BEGIN
  SORT  GT_HTGCL BY MATNR .
*&--代码添加 BY HANDYBY 28.04.2017 14:05:03  END

  LOOP AT GT_HZ_OUTPUT WHERE MATNR = ''.
    APPEND GT_HZ_OUTPUT TO GT_HZ_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_HZ_OUTPUT COMPARING MATNR.

  DELETE GT_HZ_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_HZ_OUTPUT1.
    APPEND GT_HZ_OUTPUT1 TO GT_HZ_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_HZ_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的只被导入一次，且相同物料的合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.


*&--代码添加 BY HANDYBY 28.04.2017 14:00:43  BEGIN
  "先检查一下可控的行错误信息.

  LOOP AT GT_HZ_OUTPUT .

    IF GT_HZ_OUTPUT-POSID IS INITIAL.

      GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_HZ_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GT_HZ_OUTPUT-XMLB = 'M'.
      IF GT_HZ_OUTPUT-MATNR IS  INITIAL. .
        GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GT_HZ_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:26:53  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GT_HZ_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GT_HZ_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:26:53  END

*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  BEGIN
* 物料号变大写
      TRANSLATE GT_HZ_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GT_HZ_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GT_HZ_OUTPUT-WERKS '工厂' '不存在:' GT_HZ_OUTPUT-MATNR '物料号' INTO    GT_HZ_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.

    IF GT_HZ_OUTPUT-XMLB = 'V' AND GT_HZ_OUTPUT-CBYS IS  INITIAL..

      GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GT_HZ_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_HZ_OUTPUT .
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 14:00:43  END




* --------------------先删除000增值税版本数据 it02 20160308 begin---------------
  CLEAR:PROJ_OBJNR,PRPS_OBJNR.
  READ TABLE GT_HZ_OUTPUT INDEX 1.
  IF SY-SUBRC EQ 0 .

    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_HZ_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_HZ_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_HZ_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '100'.

          IF LS_KALNR IS NOT INITIAL .

            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.


    REFRESH: LT_WBS_COSTLINES3,GT_RETURN2,LS_WBS_COSTLINES3-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN2.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.
      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.
      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除100版本失败：' GS_RETURN2-MESSAGE  INTO ERMSG .
        "  MESSAGE '该项目删除0版本数据失败，请往CJ9ECP核对删除' TYPE 'E'.
        MESSAGE ERMSG  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.

    ENDIF.
    " * --------------------先删除000增值税版本数据 it02 20160308  end---------------

    "再重新读取100版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
             FROM CKIS
             INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
             WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.

  ELSE .
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 14:01:53  BEGIN
*

*  IF IT_CKIS IS  INITIAL.
*
*    LOOP AT GT_HZ_OUTPUT.
*
*      IF GT_HZ_OUTPUT-POSID IS NOT INITIAL.
*        LS_WBS_COSTLINES-WBS_NAME = GT_HZ_OUTPUT-POSID.                   "WBS
*      ELSE.
*        MESSAGE:'WBS元素为空的数据将不会被导入！' TYPE 'I' DISPLAY LIKE 'W'.
*        CONTINUE.
*      ENDIF.
*
*      GT_HZ_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态
*
*      LS_ITEM-RESOURCE-TYPPS = GT_HZ_OUTPUT-XMLB.                        "项目类别
*
*      IF GT_HZ_OUTPUT-WERKS IS INITIAL.
*        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
*      ELSE.
*        LS_ITEM-RESOURCE-WERKS = GT_HZ_OUTPUT-WERKS.
*      ENDIF.
*
*      IF GT_HZ_OUTPUT-XMLB = 'M'.
*        IF GT_HZ_OUTPUT-MATNR IS NOT INITIAL.
*          LS_ITEM-RESOURCE-MATNR = GT_HZ_OUTPUT-MATNR.                       "物料号
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为M时，物料编号为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-RESOURCE-MATNR = GT_HZ_OUTPUT-MATNR.                       "物料号
*      ENDIF.
*
*      REFRESH GT_MEINS[].
*      CLEAR GT_MEINS.
*      IF GT_HZ_OUTPUT-MATNR IS NOT INITIAL.
*        SELECT MEINS
*              FROM MARA
*              INTO TABLE GT_MEINS
*              WHERE MATNR = GT_HZ_OUTPUT-MATNR.
*      ENDIF.
*
*      READ TABLE GT_MEINS.
*      IF SY-SUBRC = 0.
*        LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GT_MEINS-MEINS.                "单位
*      ENDIF.
*      LS_ITEM-DESCRIPT       = GT_HZ_OUTPUT-MAKTX.                       "物料描述
*
*      READ TABLE GT_HTGCL INTO GS_HTGCL WITH TABLE KEY MATNR = GT_HZ_OUTPUT-MATNR.
*      IF SY-SUBRC = 0.
*        GT_HZ_OUTPUT-HPEINH = GS_HTGCL-HPEINH.
*      ENDIF.
*
*      LS_ITEM-QUANTITY-QUANTITY = GT_HZ_OUTPUT-HPEINH.                  "合同工程量
*      LS_ITEM-PRICE-TOTAL   = GT_HZ_OUTPUT-HGPREIS.                     "合同单价
*
*      IF GT_HZ_OUTPUT-XMLB = 'V'.
*        IF GT_HZ_OUTPUT-CBYS IS NOT INITIAL.
*          LS_ITEM-COST_ELEM = GT_HZ_OUTPUT-CBYS.                          "成本要素
*        ELSE.
*          MESSAGE:'请检查模板，项目类别为V时，成本要素为空的不会被导入' TYPE 'I' DISPLAY LIKE 'W'.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        LS_ITEM-COST_ELEM = GT_HZ_OUTPUT-CBYS.                             "成本要素
*      ENDIF.
*      LS_ITEM-CURRENCY = 'CNY'.
*      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
*      APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*
*      CALL FUNCTION 'CNECP_MAINTAIN'
*        EXPORTING
*          I_PROJ_ID       = GT_HZ_OUTPUT-POSID                           "项目编号
*          I_VERSION       = P_BB2                                       "版本
*          I_COST_VARIANT  = 'PS06'                                      "核算变式
*          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*        IMPORTING
*          MESSAGES        = GT_RETURN.
*
*      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC NE 0.
*        L4 = L4 + 1. "统计导入100后最新版本的数据  IT02 20160307
*        GT_H_OUTPUT-TYPE = ICON_GREEN_LIGHT.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
*        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
*          EXPORTING
*            I_PROJ_ID       = GT_HZ_OUTPUT-POSID                           "项目定义
*            I_VERSION       = '100'                                         "版本
*            I_COST_VARIANT  = 'PS06'                                      "核算变式
*            I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
*          IMPORTING
*            MESSAGES        = GT_RETURN1.
*        READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
*        IF SY-SUBRC NE 0.
*          L2 = L2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
*        ELSE.
*          CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_HZ_OUTPUT-MESSAGE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*
*      ELSE.
*        GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.
*        GT_HZ_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      ENDIF.
*
*
*      MODIFY GT_HZ_OUTPUT TRANSPORTING HPEINH TYPE MESSAGE.
*
*      CLEAR GT_HZ_OUTPUT.
*
*      CLEAR:LS_ITEM,LS_WBS_COSTLINES-COST_LINES,LS_WBS_COSTLINES.
*      REFRESH LT_WBS_COSTLINES.
*      CLEAR:GS_RETURN.
*      REFRESH:GT_RETURN.
*
*    ENDLOOP.
*  ELSE.
*    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
*  ENDIF.

*&--代码注释 BY HANDYBY 28.04.2017 14:01:53  END

*&--代码添加 BY HANDYBY 23.05.2017 16:27:44  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_HZ_OUTPUT[]
   WHERE POSID = GT_HZ_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 16:27:44  END

*&--代码添加 BY HANDYBY 28.04.2017 14:02:21  BEGIN
  IF IT_CKIS IS  INITIAL.
    CLEAR GT_HZ_OUTPUT.
    REFRESH LT_WBS_COSTLINES.

    DATA:L_INDEX TYPE SY-TABIX.
    TYPES:BEGIN OF TY_H_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            HPEINH       TYPE MENGE_POS,              "合同工程量
            HGPREIS      TYPE CKIS-GPREIS,             "合同单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_H_OUTPUT2.
    DATA:LT_HZ_OUTPUT TYPE TABLE OF TY_H_OUTPUT2,
         LS_HZ_OUTPUT TYPE TY_H_OUTPUT2.
    DATA LS_HZ_OUTPUT2 TYPE TY_H_OUTPUT2 .

    MOVE-CORRESPONDING GT_HZ_OUTPUT[] TO LT_HZ_OUTPUT .
    SORT LT_HZ_OUTPUT BY POSID  .
    LOOP AT LT_HZ_OUTPUT INTO LS_HZ_OUTPUT2 .

      CLEAR:LS_ITEM.
      L_INDEX = SY-TABIX .

      MOVE-CORRESPONDING LS_HZ_OUTPUT2 TO LS_HZ_OUTPUT .
*      AT FIRST .
*        READ TABLE GT_HZ_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_HZ_OUTPUT-POSID.                   "WBS
*        ENDIF.
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_HZ_OUTPUT-POSID.                   "WBS
      ENDAT .

      LS_HZ_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-TYPPS = LS_HZ_OUTPUT-XMLB.                        "项目类别

      IF LS_HZ_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 16:28:20  BEGIN
        READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = LS_HZ_OUTPUT-POSID BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_ITEM-RESOURCE-WERKS = LS_PRPS-WERKS .                                  "工厂
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:28:20  END
        LS_ITEM-RESOURCE-WERKS = 1800.                                  "工厂
      ELSE.
        LS_ITEM-RESOURCE-WERKS = LS_HZ_OUTPUT-WERKS.
      ENDIF.

      LS_ITEM-RESOURCE-MATNR = LS_HZ_OUTPUT-MATNR.                       "物料号

      IF  LS_ITEM-RESOURCE-MATNR IS NOT INITIAL.
        CLEAR GS_CHECK_MATNR.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_HZ_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
        ENDIF.
      ENDIF.

      LS_ITEM-DESCRIPT       = LS_HZ_OUTPUT-MAKTX.                       "物料描述

      READ TABLE GT_HTGCL INTO GS_HTGCL WITH TABLE KEY MATNR = LS_HZ_OUTPUT-MATNR .
      IF SY-SUBRC = 0.
        LS_HZ_OUTPUT-HPEINH = GS_HTGCL-HPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_HZ_OUTPUT-HPEINH.                  "合同工程量
      LS_ITEM-PRICE-TOTAL   = LS_HZ_OUTPUT-HGPREIS.                     "合同单价

      IF LS_HZ_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_HZ_OUTPUT-CBYS.                          "成本要素
      ENDIF.

      LS_ITEM-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*      ENDAT .

      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        CLEAR LS_WBS_COSTLINES .
      ENDAT .

      CLEAR LS_HZ_OUTPUT2 .
      CLEAR LS_ITEM .
      CLEAR LS_HZ_OUTPUT .

    ENDLOOP .

    REFRESH:GT_RETURN.
    REFRESH:GT_RETURN1.
    READ TABLE GT_HZ_OUTPUT INDEX 1 .
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS4 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS4
      FROM PRPS
     WHERE POSID = GT_HZ_OUTPUT-POSID .
    IF LS_PRPS4 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS4-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:28:55  BEGIN
    DATA L_FLAG TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 16:28:55  END

    CALL FUNCTION 'CNECP_MAINTAIN'
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                           "项目编号
        I_VERSION       = P_BB2                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.

    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC NE 0.
*        L4 = L4 + 1. "统计导入100后最新版本的数据  IT02 20160307
*        GT_H_OUTPUT-TYPE = ICON_GREEN_LIGHT.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN1.
      READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
*          L2 = L2 + 1 .   "统计导入100版本后最新版本导入数量          ADD IT02 20160307
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

*&--代码添加 BY HANDYBY 23.05.2017 16:29:19  BEGIN
        L_FLAG = 0 .
*&--代码添加 BY HANDYBY 23.05.2017 16:29:19  END

      ELSE.
        CONCATENATE GS_RETURN-MESSAGE GS_RETURN1-MESSAGE INTO GT_HZ_OUTPUT-MESSAGE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*&--代码添加 BY HANDYBY 23.05.2017 16:29:51  BEGIN
        L_FLAG = 1 .
        LOOP AT GT_RETURN1 INTO GS_RETURN1 .
          CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN1 .
        ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 16:29:51  END
      ENDIF.

    ELSE.
*        GT_HZ_OUTPUT-TYPE = ICON_RED_LIGHT.
*        GT_HZ_OUTPUT-MESSAGE = GS_RETURN-MESSAGE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*&--代码添加 BY HANDYBY 23.05.2017 16:30:06  BEGIN
      L_FLAG = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.
*&--代码添加 BY HANDYBY 23.05.2017 16:30:06  END

    ENDIF.

  ELSE.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.
*&--代码添加 BY HANDYBY 28.04.2017 14:02:21  END


  "增加消息提示
  MSG100 = L_INDEX .
  CONDENSE MSG100 NO-GAPS.
  MSG101 = L_INDEX .
  CONDENSE MSG101 NO-GAPS.

  IF L_FLAG = 0.
    "100版本
    CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
    "100后版本
    CONCATENATE P_BB2 '版本已成功导入' MSG101 '条数量' INTO  MSG101.
  ELSEIF L_FLAG = 1 .
    CONCATENATE  P_BB2 '版本已成功导入:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本导入失败:'  MSG100 '条数量' INTO MSG100 .
  ELSEIF L_FLAG = 2 .
    CONCATENATE  P_BB2 '版本导入失败:'   MSG101 '条数量' INTO MSG101 .
    CONCATENATE '100版本没有执行导入动作！' '' INTO MSG100 .
  ENDIF.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_GT_CHZ_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_GT_CHZ_OUTPUT .
  CLEAR :SC_LEN .
  SELECT PSPHI
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
    FOR ALL ENTRIES IN GT_CHZ_OUTPUT
    WHERE POSID = GT_CHZ_OUTPUT-POSID.
  IF GT_PSPHI[] IS NOT INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 16:35:52  BEGIN
    DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
    READ TABLE GT_PSPHI INDEX 1 .
*&--代码添加 BY HANDYBY 23.05.2017 16:35:52  END
    SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID1
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 16:36:02  BEGIN
* FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 16:36:02  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID1[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN1                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID1
        WHERE TOPNR = GT_OBJID1-TOPNR
        AND VERSN BETWEEN '000' AND '099'.
    ENDIF.

    SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
      INTO CORRESPONDING FIELDS OF TABLE GT_OBJID2
      FROM PRECP1
      JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
*&--代码添加 BY HANDYBY 23.05.2017 16:36:18  BEGIN
* FOR ALL ENTRIES IN GT_PSPHI
*&--代码添加 BY HANDYBY 23.05.2017 16:36:18  END
      WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

    IF GT_OBJID2[] IS NOT INITIAL.
      SELECT VERSN "max( versn ) as versn
        INTO CORRESPONDING FIELDS OF TABLE GT_VERSN2                        "取一个对象编号下对应的当前最大的版本号
        FROM PRECP1
        FOR ALL ENTRIES IN GT_OBJID2
        WHERE TOPNR = GT_OBJID2-TOPNR
        AND VERSN BETWEEN '100' AND '199'.
    ENDIF.

  ENDIF.

  REFRESH:GT_OBJID1,GT_OBJID2.

  DATA:P_BB1 TYPE PRECP1-VERSN.

  SORT GT_VERSN1 BY VERSN.
  SORT GT_VERSN2 BY VERSN.

  IF GT_VERSN1[] IS NOT INITIAL.
    LOOP AT GT_VERSN1 WHERE VERSN >= '000' AND VERSN <= '099'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN1-VERSN = GT_VERSN1-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN1-VERSN
          IMPORTING
            OUTPUT = GT_VERSN1-VERSN.

        P_BB1 = GT_VERSN1-VERSN.
      ENDAT.
    ENDLOOP.
  ELSEIF GT_VERSN1[] IS INITIAL.
    P_BB1 = '001'.
  ENDIF.

  DATA:P_BB2 TYPE PRECP1-VERSN.

  IF GT_VERSN2[] IS NOT INITIAL.
    LOOP AT GT_VERSN2 WHERE VERSN >= '100' AND VERSN <= '199'.
      AT END OF VERSN.                                                  "取出了最大的版本号
        GT_VERSN2-VERSN = GT_VERSN2-VERSN + 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GT_VERSN2-VERSN
          IMPORTING
            OUTPUT = GT_VERSN2-VERSN.

        P_BB2 = GT_VERSN2-VERSN.
      ENDAT.
    ENDLOOP.
  ELSE.
    P_BB2 = 101.
  ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 16:36:35  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:36:35  END
  SORT GT_CHZ_OUTPUT BY MATNR.

  REFRESH IT_JG.
  LOOP AT GT_CHZ_OUTPUT.
    AT NEW MATNR.
      READ TABLE GT_CHZ_OUTPUT INDEX SY-TABIX.
      IF SY-SUBRC = 0.
        WA_JG-MATNR = GT_CHZ_OUTPUT-MATNR.
        WA_JG-HTDJ = GT_CHZ_OUTPUT-HGPREIS.
        WA_JG-CLDJ = GT_CHZ_OUTPUT-CGPREIS.
        WA_JG-RGDJ = GT_CHZ_OUTPUT-FPREIS.
        APPEND WA_JG TO IT_JG.
      ENDIF.
    ENDAT.
    CLEAR GT_CHZ_OUTPUT.
  ENDLOOP.

  REFRESH IT_JGHJ.
  LOOP AT GT_CHZ_OUTPUT WHERE MATNR <> ''.
    CLEAR WA_JGHJ.
    WA_JGHJ-MATNR = GT_CHZ_OUTPUT-MATNR.
    WA_JGHJ-HGPREIS = GT_CHZ_OUTPUT-HGPREIS.
    WA_JGHJ-CGPREIS = GT_CHZ_OUTPUT-CGPREIS.
    WA_JGHJ-FPREIS = GT_CHZ_OUTPUT-FPREIS.
    COLLECT WA_JGHJ INTO IT_JGHJ.
  ENDLOOP.

  LOOP AT IT_JG INTO WA_JG.
    READ TABLE IT_JGHJ INTO WA_JGHJ WITH KEY MATNR = WA_JG-MATNR.
    IF SY-SUBRC = 0.
      CLEAR G.
      LOOP AT GT_CHZ_OUTPUT WHERE MATNR = WA_JGHJ-MATNR.
        G = G + 1.
      ENDLOOP.
      W = WA_JGHJ-HGPREIS / G.
      LL = WA_JGHJ-CGPREIS / G.
      JJ = WA_JGHJ-FPREIS / G.


      IF W <> WA_JG-HTDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的合同单价不一致！' INTO MESSAGE1.
        MESSAGE MESSAGE1  TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF LL <> WA_JG-CLDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的材料单价不一致！' INTO MESSAGE2.
        MESSAGE MESSAGE2 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      IF JJ <> WA_JG-RGDJ.
        CONCATENATE '物料' WA_JGHJ-MATNR '的人工单价不一致！' INTO MESSAGE3.
        MESSAGE MESSAGE3 TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
    CLEAR:WA_JG,W,LL,JJ,MESSAGE1,MESSAGE2,MESSAGE3.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:36:49  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 16:36:49  END
  DELETE GT_CHZ_OUTPUT WHERE POSID = ''.

  CLEAR L.
  LOOP AT GT_CHZ_OUTPUT.
    L = L + 1.
  ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 16:37:31  BEGIN
* 这里GT_HB取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 16:37:31  END
  REFRESH GT_HB.
  LOOP AT GT_CHZ_OUTPUT WHERE MATNR <> ''.
    CLEAR GS_HB.
    GS_HB-MATNR = GT_CHZ_OUTPUT-MATNR.
    GS_HB-HPEINH = GT_CHZ_OUTPUT-HPEINH.
    GS_HB-JPEINH = GT_CHZ_OUTPUT-JPEINH.
    COLLECT GS_HB INTO GT_HB.
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 13:30:39  BEGIN
  SORT GT_HB BY MATNR .
*&--代码添加 BY HANDYBY 28.04.2017 13:30:39  END

  LOOP AT GT_CHZ_OUTPUT WHERE MATNR = ''.
    APPEND GT_CHZ_OUTPUT TO GT_CHZ_OUTPUT1.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM GT_CHZ_OUTPUT COMPARING MATNR.

  DELETE GT_CHZ_OUTPUT WHERE MATNR = ''.

  LOOP AT GT_CHZ_OUTPUT1.
    APPEND GT_CHZ_OUTPUT1 TO GT_CHZ_OUTPUT.
  ENDLOOP.

  CLEAR J.
  LOOP AT GT_CHZ_OUTPUT.
    J = J + 1.
  ENDLOOP.

  IF J < L.
    MESSAGE '物料号相同的只被导入一次，且相同物料的计划工程量和合同工程量将被合计！' TYPE 'I' DISPLAY LIKE 'W'.
  ENDIF.

  "先检查一下可控的行错误信息.

  LOOP AT GT_CHZ_OUTPUT INTO GS_CHZ_OUTPUT.

    IF GS_CHZ_OUTPUT-POSID IS INITIAL.

      GS_CHZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GS_CHZ_OUTPUT-MESSAGE = 'WBS元素不能为空'.              "消息

    ENDIF.

    IF GS_CHZ_OUTPUT-XMLB = 'M'.
      IF GS_CHZ_OUTPUT-MATNR IS  INITIAL. .
        GS_CHZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
        GS_CHZ_OUTPUT-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:37:53  BEGIN
* 物料加前导零，否则查不出
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = GS_CHZ_OUTPUT-MATNR
        IMPORTING
          OUTPUT       = GS_CHZ_OUTPUT-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
        MESSAGE '物料添加前导零失败！' TYPE 'E' .
        EXIT .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:37:53  END

*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  BEGIN
* 物料号变大写
      TRANSLATE GS_CHZ_OUTPUT-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  END

      READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
             MATNR = GS_CHZ_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC NE 0 .
        GS_CHZ_OUTPUT-TYPE = ICON_RED_LIGHT.     "状态
        CONCATENATE GS_CHZ_OUTPUT-WERKS '工厂' '不存在:'  GS_CHZ_OUTPUT-MATNR '物料号' INTO    GS_CHZ_OUTPUT-MESSAGE ."消息
      ENDIF.
    ENDIF.


    IF GS_CHZ_OUTPUT-XMLB = 'V' AND GS_CHZ_OUTPUT-CBYS IS  INITIAL..

      GS_CHZ_OUTPUT-TYPE = ICON_RED_LIGHT.                             "状态
      GS_CHZ_OUTPUT-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息

    ENDIF.
    MODIFY GT_CHZ_OUTPUT FROM GS_CHZ_OUTPUT.
  ENDLOOP.




*-----------------先删除100 、100版本增值税 it02 20160308
  READ TABLE  GT_CHZ_OUTPUT WITH KEY TYPE = ICON_RED_LIGHT.
  IF SY-SUBRC NE 0 .
    CLEAR:PROJ_OBJNR,PRPS_OBJNR.




    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS4.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
    DATA: BEGIN OF LS_KALNR1,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR1.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    IF GT_CHZ_OUTPUT[] IS NOT INITIAL .
      READ TABLE GT_CHZ_OUTPUT INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = GT_CHZ_OUTPUT-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '100'.
          IF LS_KALNR IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .
          ENDIF.


          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR1
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '000'.
          IF LS_KALNR1 IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
              WHERE KALNR = LS_KALNR1-KALNR.

            CLEAR LS_PRPS4 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS4
              FROM PRPS
             WHERE OBJNR = LS_KALNR1-SUBNR .
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.



    REFRESH: LT_WBS_COSTLINES3,GT_RETURN5,LT_WBS_COSTLINES4,GT_RETURN5,LS_WBS_COSTLINES3-COST_LINES,LS_WBS_COSTLINES4-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN5,LS_WBS_COSTLINES4,GS_RETURN5.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.


      ENDLOOP.
      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN5.

      READ TABLE GT_RETURN5 INTO GS_RETURN5 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除100版本失败：' GS_RETURN5-MESSAGE  INTO ERMSG .
        MESSAGE ERMSG  TYPE 'E'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        WAIT UP TO 0 SECONDS .
      ENDIF.
    ENDIF.
    "再重新读取100版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
         FROM CKIS
         INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
         WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.


    IF IT_CKIS1 IS NOT INITIAL.
      LOOP AT IT_CKIS1 INTO WA_CKIS1.
        CLEAR:LS_ITEM4.
        AT FIRST .
          LS_WBS_COSTLINES4-WBS_NAME = LS_PRPS4-POSID.
        ENDAT.
        LS_ITEM4-ITEM_NUMBER = WA_CKIS1-POSNR.
        LS_ITEM4-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM4 TO LS_WBS_COSTLINES4-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES4 TO LT_WBS_COSTLINES4.
        ENDAT.

      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES4                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN4.

      READ TABLE GT_RETURN4 INTO GS_RETURN4 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除0版本失败：' GS_RETURN4-MESSAGE  INTO ERMSG .
        MESSAGE ERMSG  TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        WAIT UP TO 0 SECONDS .
      ENDIF.
    ENDIF.
    "再重新读取000版本的预算项目信息
    IF LS_KALNR1 IS NOT INITIAL.
      SELECT *
         FROM CKIS
         INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
         WHERE KALNR = LS_KALNR1-KALNR.
    ENDIF.

  ELSE.
    MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
  ENDIF.



*&--代码添加 BY HANDYBY 23.05.2017 16:39:04  BEGIN
  DATA:BEGIN OF LS_PRPS,
         PSPNR TYPE PRPS-PSPNR,
         POSID TYPE PRPS-POSID,
         WERKS TYPE PRPS-WERKS,
       END OF LS_PRPS.
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .

  SELECT PSPNR
         POSID
         WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
     FOR ALL ENTRIES IN GT_CHZ_OUTPUT[]
   WHERE POSID = GT_CHZ_OUTPUT-POSID .
  SORT LT_PRPS BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 16:39:04  END


  " *-----------------先删除100 、100版本增值税 it02 20160308 end-------------------

  IF IT_CKIS1 IS INITIAL AND IT_CKIS IS INITIAL.
    CLEAR:L1,L2,L3,L4.   "初始导入版本数量

    REFRESH LT_WBS_COSTLINES.
    REFRESH LT_WBS_COSTLINES1.
    REFRESH LT_WBS_COSTLINES5.
    REFRESH LT_WBS_COSTLINES6.

    DATA:L_INDEX TYPE SY-TABIX.

    TYPES:BEGIN OF TY_CH_OUTPUT2,
            POSID        TYPE PRPS-POSID,              "WBS
            XH           TYPE INT4,                    "序号
            XMLB(1)      TYPE C,                       "项目类别
            WERKS        TYPE T001W-WERKS,             "工厂
            MATNR        TYPE CKIS-MATNR,              "物料编码
            MAKTX        TYPE MAKT-MAKTX,              "物料描述
            HPEINH       TYPE MENGE_POS,              "合同工程量
            HGPREIS      TYPE CKIS-GPREIS,             "合同单价
            JPEINH       TYPE MENGE_POS,              "计划工程量
            CGPREIS      TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
            FPREIS       TYPE CKIS-FPREIS,             "人工单价
            CBYS         TYPE CKIS-KSTAR,              "成本要素
            CBYSMC(40)   TYPE C,                       "成本要素名称
            POST1        TYPE PROJ-POST1,
            VERNR        TYPE PROJ-VERNR,
            VBUKR        TYPE PROJ-VBUKR,
            KALID        TYPE PROJ-KALID,
            ZTEHT        TYPE PROJ-ZTEHT,
            TYPE(10)     TYPE C,                       "状态
            MESSAGE(400) TYPE C,                       "消息
          END OF TY_CH_OUTPUT2.
    DATA:LT_CHZ_OUTPUT TYPE TABLE OF TY_CH_OUTPUT2,
         LS_CHZ_OUTPUT TYPE TY_CH_OUTPUT2.
    DATA LS_CHZ_OUTPUT2 TYPE TY_CH_OUTPUT2 .

    MOVE-CORRESPONDING GT_CHZ_OUTPUT[] TO LT_CHZ_OUTPUT .
    SORT LT_CHZ_OUTPUT BY POSID  .
    LOOP AT LT_CHZ_OUTPUT INTO LS_CHZ_OUTPUT2 .
      CLEAR:LS_ITEM,LS_ITEM1.
      L_INDEX = SY-TABIX.

      MOVE-CORRESPONDING LS_CHZ_OUTPUT2 TO LS_CHZ_OUTPUT .
*      AT FIRST .
*        READ TABLE GT_CHZ_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_CHZ_OUTPUT-POSID.                   "WBS
*      LS_WBS_COSTLINES1-WBS_NAME = GT_CHZ_OUTPUT-POSID.                  "WBS
*        ENDIF.
*
*      ENDAT.

      AT NEW POSID .
        LS_WBS_COSTLINES-WBS_NAME = LS_CHZ_OUTPUT-POSID.                   "WBS
        LS_WBS_COSTLINES1-WBS_NAME = LS_CHZ_OUTPUT-POSID.                  "WBS
      ENDAT .

      "序号
      LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号
      LS_ITEM1-ITEM_NUMBER = L_INDEX .                                      "序号

      LS_ITEM-RESOURCE-MATNR = LS_CHZ_OUTPUT-MATNR.                       "物料号
      LS_ITEM1-RESOURCE-MATNR = LS_CHZ_OUTPUT-MATNR.                       "物料号


      IF  LS_ITEM1-RESOURCE-MATNR IS NOT INITIAL.
        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                  MATNR = LS_CHZ_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
          LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
          LS_ITEM1-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                "单位

        ENDIF.

      ENDIF.

      IF LS_CHZ_OUTPUT-CBYS IS NOT INITIAL.
        LS_ITEM-COST_ELEM = LS_CHZ_OUTPUT-CBYS.                          "成本要素
        LS_ITEM1-COST_ELEM = LS_CHZ_OUTPUT-CBYS.                          "成本要素

      ENDIF.

      " gt_chz_output-type = icon_yellow_light.                             "状态

      LS_ITEM-RESOURCE-TYPPS = LS_CHZ_OUTPUT-XMLB.                        "项目类别
      LS_ITEM1-RESOURCE-TYPPS = LS_CHZ_OUTPUT-XMLB.                       "项目类别

      LS_ITEM-RESOURCE-WERKS = LS_CHZ_OUTPUT-WERKS.                     "工厂
      LS_ITEM1-RESOURCE-WERKS = LS_CHZ_OUTPUT-WERKS.                    "工厂


      LS_ITEM-DESCRIPT       = LS_CHZ_OUTPUT-MAKTX.                       "物料描述
      LS_ITEM1-DESCRIPT       = LS_CHZ_OUTPUT-MAKTX.                       "物料描述

      LS_ITEM1-PRICE-FIXED    = LS_CHZ_OUTPUT-FPREIS.                      "人工单价

      READ TABLE GT_HB INTO GS_HB WITH KEY MATNR = LS_CHZ_OUTPUT-MATNR BINARY SEARCH .
      IF SY-SUBRC = 0.
        LS_CHZ_OUTPUT-HPEINH = GS_HB-HPEINH.
        LS_CHZ_OUTPUT-JPEINH = GS_HB-JPEINH.
      ENDIF.

      LS_ITEM-QUANTITY-QUANTITY = LS_CHZ_OUTPUT-HPEINH.                   "合同工程量
      LS_ITEM-PRICE-TOTAL = LS_CHZ_OUTPUT-HGPREIS.                        "合同单价
      LS_ITEM1-QUANTITY-QUANTITY = LS_CHZ_OUTPUT-JPEINH.                   "计划工程量
      LS_ITEM1-PRICE-TOTAL = LS_CHZ_OUTPUT-CGPREIS.                        "材料单价
      LS_ITEM-CURRENCY = 'CNY'.
      LS_ITEM1-CURRENCY = 'CNY'.
      APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
      APPEND LS_ITEM1 TO LS_WBS_COSTLINES1-COST_LINES.
*      AT LAST .
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
*
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
*
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.
*
*      ENDAT.


      AT END OF POSID .
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
        CLEAR LS_WBS_COSTLINES .
        CLEAR LS_WBS_COSTLINES1 .
      ENDAT .

      CLEAR LS_ITEM .
      CLEAR LS_ITEM1 .
      CLEAR LS_CHZ_OUTPUT .
      CLEAR LS_CHZ_OUTPUT2 .

    ENDLOOP.


    REFRESH GT_RETURN.
    REFRESH GT_RETURN1.
    REFRESH GT_RETURN2.
    REFRESH GT_RETURN3.
    CLEAR GS_RETURN .
    CLEAR GS_RETURN1 .
    CLEAR GS_RETURN2 .
    CLEAR GS_RETURN3 .

    READ TABLE GT_CHZ_OUTPUT INDEX 1 .

    DATA: BEGIN OF LS_PRPS5,
            PSPNR TYPE PRPS-PSPNR,
            PSPHI TYPE PRPS-PSPHI,
          END OF LS_PRPS5 .
    DATA: BEGIN OF LS_PROJ3 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
          END OF LS_PROJ3 .
    SELECT SINGLE PSPNR
           PSPHI
      INTO CORRESPONDING FIELDS OF LS_PRPS5
      FROM PRPS
     WHERE POSID = GT_CHZ_OUTPUT-POSID .
    IF LS_PRPS5 IS NOT INITIAL .
      SELECT SINGLE PSPNR
             PSPID
        INTO CORRESPONDING FIELDS OF LS_PROJ3
        FROM PROJ
       WHERE PSPNR = LS_PRPS5-PSPHI .
    ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 16:40:25  BEGIN
    DATA L_FLAG1 TYPE I VALUE IS INITIAL  .
    DATA L_FLAG2 TYPE I VALUE IS INITIAL  .
*&--代码添加 BY HANDYBY 23.05.2017 16:40:25  END
*    READ TABLE  GT_CHZ_OUTPUT WITH KEY TYPE = ICON_RED_LIGHT.
*    IF SY-SUBRC NE 0 .
    "创建 1000版本往下的数据
    CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
        I_VERSION       = P_BB2                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
      IMPORTING
        MESSAGES        = GT_RETURN.
    READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
      " DESCRIBE TABLE ls_wbs_costlines-cost_lines LINES l4  .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES5                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN2.
      READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0.
*          L2 = L4 .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        L_FLAG1 = 0 .
*          MESSAGE '上传合同报价（增值税）成功！'  TYPE 'I'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        L_FLAG1 = 1 .
        LOOP AT GT_RETURN2 INTO GS_RETURN2 .
          CONCATENATE ZSTR GS_RETURN2-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN2 .
        ENDLOOP.
      ENDIF.

    ELSE.
*        LOOP AT GT_RETURN INTO GS_RETURN  WHERE  TYPE = 'E'.
*          IF E1_MSG IS INITIAL .
*            E1_MSG =    '上传合同报价（增值税）错误:' .
*          ELSE.
*            CONCATENATE  E1_MSG GS_RETURN-MESSAGE INTO E1_MSG .
*          ENDIF.
*        ENDLOOP.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      L_FLAG1 = 2 .
      LOOP AT GT_RETURN INTO GS_RETURN .
        CONCATENATE ZSTR GS_RETURN-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN .
      ENDLOOP.
*        MESSAGE E1_MSG TYPE 'I'.    "消息提示
    ENDIF.


    "创建000版本以下数据
    CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
      EXPORTING
        I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
        I_VERSION       = P_BB1                                       "版本
        I_COST_VARIANT  = 'PS06'                                      "核算变式
        I_WBS_COSTLINES = LT_WBS_COSTLINES1                           "内表
      IMPORTING
        MESSAGES        = GT_RETURN1.

    READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0 .
      " DESCRIBE TABLE ls_wbs_costlines-cost_lines LINES l3  .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      "  创建0版本
      CALL FUNCTION 'CNECP_MAINTAIN'
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES6                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN3.

      READ TABLE GT_RETURN3 INTO GS_RETURN3 WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0 .
*          L2 = L3.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        L_FLAG2 =  0 .
*          MESSAGE '上传成本预算（增值税）成功！'   TYPE 'I'.
      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        L_FLAG2 =  1 .

        LOOP AT GT_RETURN3 INTO GS_RETURN3 .
          CONCATENATE ZSTR GS_RETURN3-MESSAGE ';' INTO ZSTR .
          CLEAR GS_RETURN3 .
        ENDLOOP.

      ENDIF.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      L_FLAG2 =  2 .

      LOOP AT GT_RETURN1 INTO GS_RETURN1 .
        CONCATENATE ZSTR GS_RETURN1-MESSAGE ';' INTO ZSTR .
        CLEAR GS_RETURN1 .
      ENDLOOP.
*        LOOP AT GT_RETURN1 INTO GS_RETURN1  WHERE  TYPE = 'E'.
*          IF E1_MSG IS INITIAL .
*            E2_MSG =    '上传成本预算（增值税）错误:' .
*          ELSE.
*            CONCATENATE  E1_MSG GS_RETURN1-MESSAGE INTO E2_MSG .
*          ENDIF.
*        ENDLOOP.

*        MESSAGE E2_MSG TYPE 'I'.    "消息提示
    ENDIF.
*    ELSE.
*      MESSAGE '检查到红灯状态类型记录，无法导入！'  TYPE 'I'.
*    ENDIF.

  ELSEIF IT_CKIS IS NOT INITIAL.
    MESSAGE '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ELSEIF IT_CKIS1 IS NOT INITIAL.
    MESSAGE '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' TYPE 'E'.
  ENDIF.


  "增加消息提示
  "000版本
  MSG000 = L_INDEX .
  CONDENSE MSG000 NO-GAPS.
  CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
  "100版本
  MSG100 = L_INDEX .
  CONDENSE MSG100 NO-GAPS.
  CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
  "000后版本
  MSG001 = L_INDEX .
  CONDENSE MSG001 NO-GAPS.
  CONCATENATE P_BB1   '版本已成功导入' MSG001 '条数量' INTO  MSG001.
  "100后版本
  MSG101 = L_INDEX .
  CONDENSE MSG101 NO-GAPS.
  CONCATENATE P_BB2  '版本已成功导入'  MSG101 '条数量' INTO MSG101.

  ZTEXT = ZSTR+0(100) .
  ZTEXT2 = ZSTR+100(100) .
  ZTEXT3 = ZSTR+200(100) .
  ZTEXT4 = ZSTR+300(100) .
  CALL SCREEN 0101 STARTING AT 25 10.

  CLEAR: ZSTR,ZTEXT,ZTEXT2,ZTEXT3,ZTEXT4 .

  CLEAR:P_BB1,P_BB2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'STA0101'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.


FORM STANDARD_FULLSCREEN USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
