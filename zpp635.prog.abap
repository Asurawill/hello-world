REPORT ZPP635.
*--------------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Report  ZPP635
*&---------------------------------------------------------------------*
*& NAME:订单可用性检查平台
*& CREAT BY :黄鑫鹏
*& modify by :于慧
*& 2009/3/3
*&  MODI:1.2009.9.9 不考虑是散装类型的组件物料
*&
*& mod   by   : 冯飞剑（汉得）
*& mod   date : 2010-8-10
*& modfication: 1、对于非限制的在途数量排除按单采购的订单,通过科目分配类别(ekpo-knttp <> 'M')
*&              2、取MRP控制者的描述添加工厂限制
*&              3、取生产调度的描述添加工厂限制
*&---------------------------------------------------------------------*

TABLES :RESB,MARD,MAKT,STPOX,MAST,MARA ,AFPO,MARC,AUFK,AFKO,TJ02T,PLAF.

************************************************************************
*ALV层级关系定义
************************************************************************
TYPE-POOLS: SLIS,
            ICON.
DATA: WT_FIELDCAT     TYPE SLIS_T_FIELDCAT_ALV,
      WT_LAYOUT       TYPE SLIS_LAYOUT_ALV,
      WT_EVENTS       TYPE SLIS_T_EVENT,
      I_LIST_COMMENTS TYPE SLIS_T_LISTHEADER,
      W_LIST_COMMENTS LIKE LINE OF I_LIST_COMMENTS.
DATA: WS_EVENTS LIKE LINE OF WT_EVENTS                .
************************************************************************
*DATA
**************************************************************
DATA: BEGIN OF T_ITEMS OCCURS 0,
*  status   like icon-id,
        STATUS(4)     TYPE C,
        AUFNR         LIKE AFPO-AUFNR,
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
        BDTER         LIKE RESB-BDTER,
        ZMNGKO        LIKE STPOX-MNGKO, "组件数量  ？？？
        ZSYXQS        LIKE STPOX-MNGKO,  "剩余需求数
        MEINS         LIKE STPOX-MEINS, "子件计量单位
        DUMPS         LIKE STPOX-DUMPS,  "虚拟项
        KZAUS         LIKE STPOX-KZAUS,  "中止指示符
        NFMAT         LIKE STPOX-NFMAT,  "后续物料
        KZNFP         LIKE STPOX-KZNFP,  "后继项目
        RGEKZ         LIKE STPOX-RGEKZ,  "反冲
        ALPGR         LIKE STPOX-ALPGR,  "组
        ALPRF         LIKE STPOX-ALPRF,  "评比订单
        LABST         LIKE MARD-LABST, "子件库存量
        ZMZDAT        TYPE D,          "如果剩余可用量为负数 求满足日期
        VMENG         LIKE RESB-VMENG,  "已分配量
        ZMENGE_S      LIKE RESB-VMENG,  "剩余可用量
        DATUV         LIKE STPOX-DATUV,  "起止
        DATUB         LIKE STPOX-DATUB,  "有效至
        MTART         LIKE STPOX-MTART,  "类型
        AUSCH         LIKE STPOX-AUSCH,  "
        SOBSL         LIKE STPOX-SOBSL,
        LGPRO         LIKE STPOX-LGPRO,
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
        EKGRP         TYPE STPOX-EKGRP,  "采购组
        EKNAM         TYPE T024-EKNAM,   "采购组名称
        ZT_MENGE      TYPE RESB-VMENG,   "在途数量
        ZZ_MENGE      TYPE RESB-VMENG,   "在制数量
        ZJH_MENGE     TYPE RESB-VMENG,   "计划分配数量
        BDART         LIKE RESB-BDART,
        STTXT         TYPE BSVX-STTXT,    "生产订单状态
        GAMNG         LIKE AFKO-GAMNG,   "订单数量
        RSNUM         LIKE RESB-RSNUM,
        RSPOS         LIKE RESB-RSPOS,
        ALPGR_C       TYPE C,   "可替代标识
        ZQL_MENGE     TYPE RESB-VMENG,  "计划分配后可用量
        ZSY_MENGE     TYPE RESB-VMENG, "总剩余可用量
        ZKY_MENGE     TYPE RESB-VMENG, "总剩余可用量(+W.P)
        ZDATE         TYPE D,
        ZYW_STATUS(4) TYPE C,  "延误状态
        ZTIME         TYPE SY-UZEIT,
        ZDATE_F       TYPE D,
        ZTIME_F       TYPE SY-UZEIT,
***  add by 冯飞剑（汉得） 国变需求 2010-6-17 （S）
        DDKALAB       LIKE MSKA-KALAB,     "订单库存
        QTKALAB       LIKE MSKA-KALAB,     "其它订单库存
        DDMENGE       LIKE MSKA-KALAB,     "本单在途数量
        QTMENGE       LIKE MSKA-KALAB,     "其他在途数量
        ZZGAMNG       LIKE AFKO-GAMNG,     "本单在制数量
        QTGAMNG       LIKE AFKO-GAMNG,     "其他单在制数量
        KDAUF         LIKE AFPO-KDAUF,     "销售订单
        KDPOS         LIKE AFPO-KDPOS,     "销售订单行项目
***  add by 冯飞剑（汉得） 2010-6-17 （E）

***  add by 冯飞剑（汉得） 2010-8-19 （S）
        YQMNG         LIKE AFKO-GAMNG,     "逾期在途数量
***  add by 冯飞剑（汉得） 2010-8-19 （E）
      END OF T_ITEMS.

"非限制库存
DATA:BEGIN OF T_MARD OCCURS 0,
       MATNR LIKE MARD-MATNR,
       WERKS LIKE MARD-WERKS,
       LABST LIKE MARD-LABST,
       LGORT LIKE MARD-LGORT,
     END OF T_MARD.

"已分配量
DATA:BEGIN OF T_RESB OCCURS 0,
       RSNUM LIKE RESB-RSNUM,
       RSPOS LIKE RESB-RSPOS,
       AUFNR LIKE RESB-AUFNR,
       BDMNG LIKE RESB-BDMNG,  "需求数量
       ENMNG LIKE RESB-ENMNG,   "提货数
       MATNR LIKE RESB-MATNR,
       WERKS LIKE RESB-WERKS,
       VMENG LIKE RESB-VMENG,  "已分配量
       BDART LIKE RESB-BDART,
       BDTER LIKE RESB-BDTER,
       ALPGR LIKE RESB-ALPGR,
       PLNUM LIKE RESB-PLNUM,
     END OF T_RESB.

*-------用于取生产订单组件的需求日期
DATA:BEGIN OF T_AFNR_RESB OCCURS 0,
       RSNUM LIKE RESB-RSNUM,
       RSPOS LIKE RESB-RSPOS,
       AUFNR LIKE RESB-AUFNR,
       BDTER LIKE RESB-BDTER,
       MATNR LIKE RESB-MATNR,
       WERKS LIKE RESB-WERKS,
       BDART LIKE RESB-BDART,
     END OF T_AFNR_RESB.

DATA:BEGIN OF T_AFPO OCCURS 0 ,
       AUFNR LIKE AFPO-AUFNR,
       GLTRP LIKE AFKO-GLTRP,
       MATNR LIKE AFPO-MATNR,
       MEINS LIKE AFPO-MEINS,
       PSMNG LIKE AFPO-PSMNG, "订单数量
       WEMNG LIKE AFPO-WEMNG, "交货数量
       PSAMG LIKE AFPO-PSAMG,  "报废数量
       IAMNG LIKE AFPO-IAMNG,  "交货过量或不足
       LGORT LIKE AFPO-LGORT,
       WERKS LIKE AFPO-DWERK,
     END OF T_AFPO.

DATA:BEGIN OF T_AFKO OCCURS 0 ,  "生产订单数据
       AUFNR LIKE AFPO-AUFNR,
       PSMNG LIKE AFPO-PSMNG,
       MATNR LIKE AFPO-MATNR,
       DWERK LIKE AFPO-DWERK,
       GSTRP LIKE AFKO-GSTRP,
       RSNUM LIKE AFKO-RSNUM,
       STTXT TYPE BSVX-STTXT,
       GAMNG LIKE AFKO-GAMNG,   "订单数量
       KDAUF LIKE AFPO-KDAUF,   "销售订单
       KDPOS LIKE AFPO-KDPOS,   "销售订单行项目
     END OF T_AFKO.

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
     END OF T_EKPO.

*-------计算满足需求最近的日期用
DATA:BEGIN OF T_MZTAB OCCURS 0 ,
       MZLNR(12) TYPE C,
       MATNR     TYPE MARA-MATNR,
       MENGE     TYPE EKPO-MENGE,
       ZMZDAT    TYPE EKET-EINDT,
     END OF T_MZTAB.

*-----------取物料描述和物料类型
DATA:BEGIN OF T_MATNR_INFO OCCURS 0,
       MATNR LIKE MARA-MATNR,
       MAKTX LIKE MAKT-MAKTX,
       MTART LIKE MARA-MTART,
     END OF T_MATNR_INFO.

*-----------公司间采购订单类型表
DATA:BEGIN OF T_161V OCCURS 0,
       BSART LIKE T161V-BSART,
       RESWK LIKE T161V-RESWK,
     END OF T_161V.


DATA L_AUFNR LIKE JEST-OBJNR.
DATA MAKTX_F TYPE MAKT-MAKTX.
DATA MEINS_F TYPE MARA-MEINS.
DATA STLAL  TYPE MAST-STLAL.
DATA STLAN  TYPE MAST-STLAN.
DATA BMENG  TYPE STKO-BMENG.
*ranges s_werks for mast-werks.
DATA:L_FLAG(10) TYPE C.

DATA: L_WERKS   TYPE AFPO-DWERK,
      L_MATNR   TYPE AFPO-MATNR,
      L_EMENG   TYPE AFPO-PSMNG,
      L_MENGE_S TYPE AFPO-PSMNG.

DATA: QL_MENGE TYPE AFPO-PSMNG.
*----------选择屏幕取值
DATA:BEGIN OF LTAB_VALUES OCCURS 0,
       FELD(40) TYPE C,
     END OF LTAB_VALUES.
DATA:LTAB_FIELDS LIKE HELP_VALUE OCCURS 0 WITH HEADER LINE.

"用于统计显示
DATA:BEGIN OF T_MATNR OCCURS 0,
       IDNRK         TYPE MARA-MATNR,
       AUFNR         LIKE AFPO-AUFNR,
       OJTXP         TYPE MAKT-MAKTX,
       WERKS         LIKE MARC-WERKS,
       EKGRP         TYPE STPOX-EKGRP,  "采购组
       EKNAM         TYPE T024-EKNAM,   "采购组名称
*     matnr    LIKE mara-matnr,
       ZYW_STATUS(4) TYPE C,
       WHT_NUM       TYPE I,
       GQ_NUM        TYPE I,
       CQ_NUM        TYPE I,
       HJ            TYPE I,
     END OF T_MATNR.
DATA:T_MATNR_R LIKE STANDARD TABLE OF T_MATNR WITH HEADER LINE.

DATA:BEGIN OF T_EKGRP OCCURS 0,
       EKGRP         TYPE STPOX-EKGRP,  "采购组
       AUFNR         LIKE AFPO-AUFNR,
       IDNRK         LIKE MARA-MATNR,
       WERKS         LIKE MARC-WERKS,
       MATNR         LIKE MARA-MATNR,
       EKNAM         TYPE T024-EKNAM,   "采购组名称
       ZYW_STATUS(4) TYPE C,
       WHT_NUM       TYPE I,
       GQ_NUM        TYPE I,
       CQ_NUM        TYPE I,
       TOTAL_NUM     TYPE I,
       YWL           TYPE P DECIMALS 2,
     END OF T_EKGRP.
DATA:T_EKGRP_R LIKE STANDARD TABLE OF T_EKGRP WITH HEADER LINE.

DATA:WHT_N TYPE I,  "无合同
     GQ_N  TYPE I,   "过期
     CQ_N  TYPE I.    "超期
*
DATA:TOTAL       TYPE I,  "总的物料行数
     WL_WHT      TYPE I,
     WL_GQ       TYPE I,
     WL_CQ       TYPE I,
     WL_TOTAL    TYPE I,

     WX_WHT      TYPE I,
     WX_GQ       TYPE I,
     WX_CQ       TYPE I,
     WX_TOTAL    TYPE I,

     TOTAL_WHT   TYPE I,
     TOTAL_GQ    TYPE I,
     TOTAL_CQ    TYPE I,
     TOTAL_TOTAL TYPE I.

DATA:L_DATE(30)   TYPE C,
     L_TIME       TYPE SY-UZEIT,
     S_DATE       TYPE D,
     S_TIME       TYPE SY-UZEIT,
     L_DATE_F(30) TYPE C.

DATA: L_USER TYPE SY-UNAME.
L_USER = 'HANDDEV'.
***  ADD BY 冯飞剑（汉得） 增加工厂权限检查内表 2010-6-17 （S）
DATA: BEGIN OF T_WERKS OCCURS 0,
        WERKS LIKE T001W-WERKS,
      END   OF T_WERKS.

***  定义销售订单库存表
DATA: BEGIN OF T_MSKA OCCURS 0,
        SALAB LIKE MSKA-KALAB,
        MATNR LIKE MSKA-MATNR,
        WERKS LIKE MSKA-WERKS,
        VBELN LIKE MSKA-VBELN,
        POSNR LIKE MSKA-POSNR,
      END   OF T_MSKA.

***  定义销售订单全制程委外的内表\
DATA: BEGIN OF T_EKPO_1 OCCURS 0,
        VBELN LIKE EKKN-VBELN,
        VBELP LIKE EKKN-VBELP,
        MENGE LIKE EKKN-MENGE,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        MATNR LIKE EKPO-MATNR,
        WERKS LIKE EKPO-WERKS,
        EINDT LIKE EKET-EINDT,    "请求交货日期
      END   OF T_EKPO_1.

DATA: W_EKPO_1 LIKE T_EKPO_1.
***  定义采购订单收货数量内表
DATA: BEGIN OF T_EKBE OCCURS 0,
        MATNR LIKE EKBE-MATNR,
        WERKS LIKE EKBE-WERKS,
        MENGE LIKE EKBE-MENGE,
        EBELN LIKE EKKN-EBELN,
        EBELP LIKE EKKN-EBELP,
        SHKZG LIKE EKBE-SHKZG,    "借贷标识
      END   OF T_EKBE.

***  ADD BY 冯飞剑（汉得） 2010-6-17 （E）
*&---------------------------------------------------------------------*
*&      SELECTION-SCREEN
*&---------------------------------------------------------------------*

PARAMETER :P_ORDBOX TYPE C AS CHECKBOX DEFAULT 'X',
           P_JHOBOX TYPE C AS CHECKBOX.

SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: S_KDAUF FOR AFPO-KDAUF MODIF ID M1,
                S_KDPOS FOR AFPO-KDPOS MODIF ID M1.
SELECTION-SCREEN END   OF BLOCK BLK3.

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:S_AUFNR FOR AUFK-AUFNR,
               S_MATNR FOR AFPO-MATNR,
               S_WERKS FOR AFPO-DWERK OBLIGATORY, "DEFAULT 1010,
               S_AUART FOR AUFK-AUART,
               S_DISPO FOR AFKO-DISPO,
               S_FEVOR FOR AFKO-FEVOR.

SELECT-OPTIONS:S_GSTRP FOR AFKO-GSTRP,
               S_GLTRP FOR AFKO-GLTRP,
               S_ERDAT FOR AUFK-ERDAT.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS:S_PLNUM  FOR PLAF-PLNUM,
               S_PMATNR FOR PLAF-MATNR,
               S_PLWRK  FOR PLAF-PLWRK OBLIGATORY, "DEFAULT 1010,
               S_PDISPO FOR PLAF-DISPO,
               S_PLGRP  FOR PLAF-PLGRP.

SELECT-OPTIONS:S_PSTTR FOR PLAF-PSTTR,
               S_PEDTR FOR PLAF-PSTTR,
               S_PALTR FOR PLAF-PALTR
               .

SELECTION-SCREEN END OF BLOCK BLK2.

PARAMETER :P_FRONT RADIOBUTTON GROUP A1 ,
           P_BACK RADIOBUTTON GROUP A1 DEFAULT 'X'.

*at selection-screen on value-request for p_syst.
*  perform get_tj02 using p_syst text-006.


*

***  add by 冯飞剑（汉得） 2010-6-17（S） 国变需求
**
AT SELECTION-SCREEN OUTPUT.                 "选择屏幕PBO逻辑流
  PERFORM FRM_HIDE_ELEMENTS.                "设置国变的前、后台执行模式为不可见，设置为前台执行

***  add by 冯飞剑（汉得） 2010-6-17（E）
AT SELECTION-SCREEN.
  IF P_FRONT = 'X'.

    PERFORM CHECK_DATE.

    PERFORM GET_ORDER.

    PERFORM GET_RESB_BOM.

  ENDIF.

INITIALIZATION.

  PERFORM FRM_SET_INITIAL.

START-OF-SELECTION.
  PERFORM FRM_CHECK_AUTHORITY.

END-OF-SELECTION.

  IF P_FRONT = 'X'.
    PERFORM GET_DATA.
  ELSE.
    PERFORM GET_ZATP.   "后台作业的执行结果
  ENDIF.

  PERFORM PRINT_ALV_DATA.



***  add by 冯飞剑（汉得） 国变需求 2010-6-17 （S）
*----------------------------------------------------------------------*
*  frm_set_initial
*  根据执行人员的订单工厂维护的权限值来设置选择屏幕工厂、计划工厂的初始值
*
*----------------------------------------------------------------------*
FORM FRM_SET_INITIAL.
*  AUTHORITY-CHECK OBJECT 'C_AFKO_AWK' ID 'WERKS' FIELD '3000'.
*  IF SY-SUBRC EQ 0.
*    S_WERKS-LOW = '3000'.
*    S_WERKS-SIGN = 'I'.
*    S_WERKS-OPTION = 'EQ'.
*    APPEND S_WERKS.
*    S_PLWRK-LOW = '3000'.
*    S_PLWRK-SIGN = 'I'.
*    S_PLWRK-OPTION = 'EQ'.
*    APPEND S_PLWRK.
*  ELSE.
*    S_WERKS-LOW = '3000'.
*    S_WERKS-SIGN = 'I'.
*    S_WERKS-OPTION = 'EQ'.
*    APPEND S_WERKS.
*    S_PLWRK-LOW = '3000'.
*    S_PLWRK-SIGN = 'I'.
*    S_PLWRK-OPTION = 'EQ'.
*    APPEND S_PLWRK.
*  ENDIF.

ENDFORM.                    "FRM_SET_INITIAL
*--------------------------------------------------------------------*
*  frm_check_authority
*  权限检查
*
*--------------------------------------------------------------------*
FORM FRM_CHECK_AUTHORITY.
  DATA: S_MSG TYPE STRING.
  SELECT WERKS
    INTO CORRESPONDING FIELDS OF TABLE T_WERKS
    FROM T001W
    WHERE WERKS IN S_WERKS
      AND WERKS IN S_PLWRK.

  IF T_WERKS[] IS INITIAL.
  ENDIF.
  LOOP AT T_WERKS.
    AUTHORITY-CHECK OBJECT 'C_AFKO_AWK' ID 'WERKS' FIELD T_WERKS-WERKS.
    IF SY-SUBRC NE 0.
      CONCATENATE '你没有工厂' T_WERKS-WERKS '的权限' INTO S_MSG.
      MESSAGE S_MSG TYPE 'E'.
    ENDIF.
    CLEAR T_WERKS.
  ENDLOOP.

ENDFORM.                    "FRM_CHECK_AUTHORITY

*----------------------------------------------------------------------*
*   form frm_hide_elements
*   隐藏部分屏幕元素、同时设置默认值
*
*----------------------------------------------------------------------*
FORM FRM_HIDE_ELEMENTS.
*  AUTHORITY-CHECK OBJECT 'C_AFKO_AWK' ID 'WERKS' FIELD '3000'.
  IF SY-SUBRC EQ 0.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'P_FRONT' OR SCREEN-NAME = 'P_BACK'.
        SCREEN-ACTIVE = 0.
        P_FRONT = 'X'.
        P_BACK  = SPACE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'M1'.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "frm_undisplay_elements


***   add by 冯飞剑（汉得） 2010-6-17 （E）
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
**********************************************
*计算各种数量
*******************************************
  "计算子件库存量、 已分配量
  SELECT  LABST
          MATNR
          WERKS
          LGORT
          INTO CORRESPONDING FIELDS OF TABLE T_MARD
          FROM MARD
          FOR ALL ENTRIES IN T_ITEMS
          WHERE MATNR = T_ITEMS-IDNRK
          AND WERKS = T_ITEMS-WERKS
          AND DISKZ <> '1'.

*** add by 冯飞剑（汉得） 2010-6-17 国变需求 （S）
*  取BOM物料的订单库存
  SELECT MATNR
         WERKS
         SALAB
         VBELN
         POSNR
    INTO CORRESPONDING FIELDS OF TABLE T_MSKA
    FROM MSSA
    FOR ALL ENTRIES IN T_ITEMS
    WHERE WERKS = T_ITEMS-WERKS
      AND MATNR = T_ITEMS-IDNRK.

*  取全制程委外销售订单相关内表
  SELECT MATNR
         WERKS
         EKPO~MENGE
         EKPO~EBELN
         EKPO~EBELP
         VBELN
         VBELP
         EINDT
    INTO CORRESPONDING FIELDS OF TABLE T_EKPO_1
    FROM EKPO INNER JOIN EKKN
    ON  EKPO~EBELN = EKKN~EBELN
    AND EKPO~EBELP = EKKN~EBELP
    INNER JOIN EKET
    ON  EKPO~EBELN = EKET~EBELN
    AND EKPO~EBELP = EKET~EBELP
    FOR ALL ENTRIES IN T_ITEMS
    WHERE WERKS = T_ITEMS-WERKS
      AND MATNR = T_ITEMS-IDNRK
      AND ELIKZ = SPACE
      AND EKPO~LOEKZ <> 'L'.

  SORT T_EKPO_1 BY EBELN EBELP EINDT DESCENDING.
  DELETE ADJACENT DUPLICATES FROM T_EKPO_1 COMPARING EBELN EBELP.

*  取采购订单交货数量内表
  IF T_EKPO_1[] IS NOT INITIAL.
    SELECT MATNR
           WERKS
           MENGE
           SHKZG
           EBELN
           EBELP
      INTO CORRESPONDING FIELDS OF TABLE T_EKBE
      FROM EKBE
      FOR ALL ENTRIES IN T_EKPO_1
      WHERE EBELN = T_EKPO_1-EBELN
        AND EBELP = T_EKPO_1-EBELP
        AND BEWTP = 'E'.
  ENDIF.
*** add by 冯飞剑（汉得） 2010-6-17 （E）
  SELECT RSNUM
         RSPOS
         AUFNR
         BDMNG    "需求数量
         ENMNG    "提货数
         MATNR
         WERKS
         BDART
         BDTER
         PLNUM   "计划订单数量
         INTO CORRESPONDING FIELDS OF TABLE T_RESB
         FROM RESB
         FOR ALL ENTRIES IN T_ITEMS
         WHERE MATNR = T_ITEMS-IDNRK
         AND BDTER <= T_ITEMS-BDTER
         AND WERKS = T_ITEMS-WERKS
         AND KZEAR <> 'X'  "最后发货
         AND XLOEK <> 'X'   "删除标识
         AND DUMPS <> 'X'   "虚拟项
         AND SCHGT <> 'X'
         AND ( BDART = 'AR' OR  BDART = 'SB' ) "生产订单预留
         .

  LOOP AT T_RESB WHERE AUFNR = ''.
    T_RESB-AUFNR = T_RESB-PLNUM.
    MODIFY T_RESB.
  ENDLOOP.

  "按需求日期升序排列 add 09.6.18
  SORT T_RESB BY BDTER ASCENDING.

*    sort t_resb by matnr.
*     取得在制数量
  SELECT AFPO~AUFNR
         AFPO~MATNR
         AFPO~MEINS
         AFPO~PSMNG "订单数量
         AFPO~WEMNG "交货数量
         AFPO~PSAMG "报废数量
         AFPO~IAMNG
         AFKO~GLTRP
         AFPO~LGORT
         AFPO~DWERK AS WERKS
         INTO CORRESPONDING FIELDS OF TABLE T_AFPO
         FROM AFPO
         JOIN AFKO ON AFPO~AUFNR = AFKO~AUFNR
         JOIN T001L ON T001L~LGORT = AFPO~LGORT
         FOR ALL ENTRIES IN T_ITEMS
         WHERE ELIKZ <> 'X'  "交货完成
         AND WEPOS = 'X' "是否收货
         AND MATNR = T_ITEMS-IDNRK
         AND DWERK = T_ITEMS-WERKS
         AND DISKZ <> '1'
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

*------------取物料信息  (物料描述,物料类型)
  SELECT MARA~MATNR
         MTART
         MAKTX
         INTO CORRESPONDING FIELDS OF TABLE T_MATNR_INFO
         FROM MARA
         JOIN MAKT ON MARA~MATNR = MAKT~MATNR
         FOR ALL ENTRIES IN T_ITEMS
         WHERE SPRAS = 1
           AND ( MARA~MATNR = T_ITEMS-MATNR
           OR    MARA~MATNR = T_ITEMS-IDNRK ).

  PERFORM GET_PO_QUANTITY."取得采购在途数量

  PERFORM GET_T161V."*取的公司间的采购订单类型

  LOOP AT T_ITEMS.
*   库存数量
    LOOP AT T_MARD WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS.
      T_ITEMS-LABST = T_ITEMS-LABST + T_MARD-LABST.
    ENDLOOP.

    LOOP AT T_RESB WHERE MATNR = T_ITEMS-IDNRK  AND WERKS = T_ITEMS-WERKS AND BDTER <= T_ITEMS-BDTER.
      IF  T_RESB-BDART = 'AR'  .     "已分配量(排除该订单本身的需求数)
        IF  T_RESB-BDTER = T_ITEMS-BDTER.
          IF  T_RESB-RSNUM < T_ITEMS-RSNUM OR ( T_RESB-RSNUM = T_ITEMS-RSNUM AND T_RESB-RSPOS < T_ITEMS-RSPOS ).
            T_ITEMS-VMENG = T_ITEMS-VMENG + ( T_RESB-BDMNG - T_RESB-ENMNG ).
          ENDIF.
        ELSE.
          T_ITEMS-VMENG = T_ITEMS-VMENG + ( T_RESB-BDMNG - T_RESB-ENMNG ).
        ENDIF.
      ELSEIF T_RESB-BDART = 'SB'. "   计划分配量(排除 该订单本身的需求数)
        IF  T_RESB-BDTER = T_ITEMS-BDTER.
          IF  T_RESB-RSNUM < T_ITEMS-RSNUM OR ( T_RESB-RSNUM = T_ITEMS-RSNUM AND T_RESB-RSPOS < T_ITEMS-RSPOS ).
            T_ITEMS-ZJH_MENGE = T_ITEMS-ZJH_MENGE + ( T_RESB-BDMNG - T_RESB-ENMNG ).
          ENDIF.
        ELSE.
          T_ITEMS-ZJH_MENGE = T_ITEMS-ZJH_MENGE + ( T_RESB-BDMNG - T_RESB-ENMNG ).
        ENDIF.
      ENDIF.
    ENDLOOP.

*  在制数量   = 订单数量-交货数量-报废数量
    LOOP AT T_AFPO WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS AND GLTRP <= T_ITEMS-BDTER.
      T_ITEMS-ZZ_MENGE =  T_ITEMS-ZZ_MENGE + ( T_AFPO-PSMNG -  T_AFPO-WEMNG - T_AFPO-IAMNG ).
    ENDLOOP.

* 计算在途量
    LOOP AT T_EKPO WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS.
*-----------如果采购订单是公司间采购 将数量添加到确认分配  else 数量添加到在途数量
      READ TABLE T_161V WITH KEY BSART = T_EKPO-BSART.
      IF SY-SUBRC = 0 AND T_ITEMS-BDTER => T_EKPO-EINDT.
        T_ITEMS-VMENG = T_ITEMS-VMENG + ( T_EKPO-MENGE - T_EKPO-WEMNG ).
      ELSEIF SY-SUBRC <> 0 AND T_ITEMS-BDTER => T_EKPO-EINDT.
        T_ITEMS-ZT_MENGE = T_ITEMS-ZT_MENGE + ( T_EKPO-MENGE - T_EKPO-WEMNG ).
      ENDIF.

***  ADD BY 冯飞剑（汉得） 2010-8-19 （S） 增加计算逾期数量
      IF T_ITEMS-BDTER <= T_EKPO-EINDT.
        T_ITEMS-YQMNG = T_ITEMS-YQMNG + ( T_EKPO-MENGE - T_EKPO-WEMNG ).
      ENDIF.
***  ADD BY 冯飞剑（汉得） 2010-8-19（E）
    ENDLOOP.

    "  剩余可用量  =
    T_ITEMS-ZMENGE_S = T_ITEMS-LABST - T_ITEMS-VMENG - T_ITEMS-ZSYXQS.

***   add by 冯飞剑（汉得） 2010-6-17 国变需求量（S）
*  计算订单的库存
    READ TABLE T_MSKA WITH KEY VBELN = T_ITEMS-KDAUF POSNR = T_ITEMS-KDPOS MATNR = T_ITEMS-IDNRK.
    IF SY-SUBRC EQ 0.
      T_ITEMS-DDKALAB = T_MSKA-SALAB.
    ENDIF.

*  计算当前物料的其它订单库存
    LOOP AT T_MSKA WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS AND VBELN <> T_ITEMS-KDAUF.
      T_ITEMS-QTKALAB = T_ITEMS-QTKALAB + T_MSKA-SALAB.
      CLEAR T_MSKA.
    ENDLOOP.

    IF T_ITEMS-KDAUF IS NOT INITIAL.
      READ TABLE T_EKPO_1 WITH KEY VBELN = T_ITEMS-KDAUF VBELP = T_ITEMS-KDPOS MATNR = T_ITEMS-IDNRK.
      IF SY-SUBRC NE 0.
        T_ITEMS-ZYW_STATUS = '无合同'.
      ENDIF.
    ENDIF.

*  计算订单在途数量
    LOOP AT T_EKPO_1 WHERE VBELN = T_ITEMS-KDAUF AND VBELP = T_ITEMS-KDPOS AND MATNR = T_ITEMS-IDNRK.
      T_ITEMS-DDMENGE = T_ITEMS-DDMENGE + T_EKPO_1-MENGE.

**  处理按单的延误状态
      IF T_EKPO_1-EINDT < SY-DATUM.           "如果采购订单的交货日期是过去的日期，则对应的是合同过期状态
        T_ITEMS-ZYW_STATUS = '合同过期'.
      ELSEIF T_EKPO_1-EINDT > T_ITEMS-BDTER.        "如果当前采购订单的最后一个交货日期大于需求日期，则对应的是合同延误状态
        T_ITEMS-ZYW_STATUS = '合同超期'.
      ENDIF.

***  计算销售订单对应的采购订单的已收货/冲销的数量
      LOOP AT T_EKBE WHERE EBELN = T_EKPO_1-EBELN AND EBELP = T_EKPO_1-EBELP.
        IF T_EKBE-SHKZG = 'S'.
          T_ITEMS-DDMENGE = T_ITEMS-DDMENGE - T_EKBE-MENGE.
        ENDIF.

        IF T_EKBE-SHKZG = 'H'.
          T_ITEMS-DDMENGE = T_ITEMS-DDMENGE + T_EKBE-MENGE.
        ENDIF.
        CLEAR T_EKBE.
      ENDLOOP.
      CLEAR T_EKPO_1.
    ENDLOOP.


*** 计算当前物料的其它订单在途库存
    LOOP AT T_EKPO_1 WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS AND VBELN <> T_ITEMS-KDAUF.
      T_ITEMS-QTMENGE = T_ITEMS-QTMENGE + T_EKPO_1-MENGE.
***   计算已收货数量、冲销数量
      LOOP AT T_EKBE WHERE  EBELN = T_EKPO_1-EBELN AND EBELP = T_EKPO_1-EBELP.
        IF T_EKBE-SHKZG = 'S'.                "对应的是冲销的操作
          T_ITEMS-QTMENGE = T_ITEMS-QTMENGE - T_EKBE-MENGE.
        ENDIF.

        IF T_EKBE-SHKZG = 'H'.               "对应的是收货的操作
          T_ITEMS-QTMENGE = T_ITEMS-QTMENGE + T_EKBE-MENGE.
        ENDIF.

        CLEAR T_EKBE.
      ENDLOOP.
      CLEAR T_EKPO_1.
    ENDLOOP.


    CLEAR: T_EKPO_1, W_EKPO_1.
***  ADD BY 冯飞剑（汉得） 2010-6-17（E）
    MODIFY T_ITEMS.
    CLEAR T_ITEMS.
  ENDLOOP.

  REFRESH T_MARD.
  REFRESH T_RESB.
*  REFRESH T_AFPO.
  DATA:TMP_MENGE TYPE AFPO-PSMNG.

  LOOP AT T_ITEMS.
    CLEAR QL_MENGE.
    CLEAR TMP_MENGE.
    "订单缺料量    (=库存量- 在途量- 计划分配量)---只计划订单分析有使用价值
    QL_MENGE = T_ITEMS-LABST - T_ITEMS-VMENG - T_ITEMS-ZJH_MENGE.
    IF QL_MENGE > 0 .
      TMP_MENGE = QL_MENGE - T_ITEMS-ZSYXQS.

      IF TMP_MENGE >= 0.
        T_ITEMS-ZQL_MENGE = 0.
      ELSE.
        T_ITEMS-ZQL_MENGE = QL_MENGE - T_ITEMS-ZSYXQS.
      ENDIF.

    ELSE.
      T_ITEMS-ZQL_MENGE  = - T_ITEMS-ZSYXQS .
    ENDIF.


    "总剩余可用量         (=库存量-剩余需求量-确认分配量-计划分配量)----确认订单当前可执行性,负表示眼下库存缺料;
    T_ITEMS-ZSY_MENGE = T_ITEMS-LABST - T_ITEMS-ZSYXQS - T_ITEMS-VMENG - T_ITEMS-ZJH_MENGE.

    "总剩余可用量(+W.P)   (=库存量+在制量+在途量-剩余需求量-确认分配量-计划分配量)---跟踪整体计划需求的合理性Etc,负表示理论缺料料;
    T_ITEMS-ZKY_MENGE = T_ITEMS-LABST + T_ITEMS-ZZ_MENGE + T_ITEMS-ZT_MENGE - T_ITEMS-ZSYXQS - T_ITEMS-VMENG - T_ITEMS-ZJH_MENGE.

    IF T_ITEMS-ZSY_MENGE < 0 AND T_ITEMS-KDAUF IS INITIAL.
      PERFORM MZ_DAT.  " 剩余数量负数时计算满足日期
    ENDIF.
    MODIFY T_ITEMS.
  ENDLOOP.

  PERFORM GET_DESCRIBE.

*--------------判断需求状态  图标显示 满足日期+总剩余可用量
  LOOP AT T_ITEMS.
    IF T_ITEMS-KDAUF IS INITIAL.        "对应的是按库生产的处理
      IF T_ITEMS-ZSY_MENGE < 0.
        T_ITEMS-STATUS = '缺料'.                              "'@0A@'.
      ELSEIF T_ITEMS-ZSY_MENGE >= 0.
        T_ITEMS-STATUS = '料齐'.                              "'@08@'.
      ENDIF.

      IF T_ITEMS-ZMZDAT IS NOT INITIAL AND T_ITEMS-ZSY_MENGE < 0 AND T_ITEMS-ZMZDAT <  SY-DATUM.  "答复的到合同到货期已经过期
        T_ITEMS-ZYW_STATUS = '合同过期'.
      ELSEIF  T_ITEMS-ZMZDAT IS NOT INITIAL AND T_ITEMS-ZSY_MENGE < 0 AND T_ITEMS-ZMZDAT >  T_ITEMS-BDTER. "签订了合同到货日期，但是到货时间无法满足生产订单的需求时间
        T_ITEMS-ZYW_STATUS = '合同超期'.
      ELSEIF  T_ITEMS-ZMZDAT IS INITIAL AND T_ITEMS-ZSY_MENGE < 0.
        T_ITEMS-ZYW_STATUS = '无合同'.   "在系统中仍然是采购申请，采购计划员没有处理签订采购合同的情况
      ENDIF.
    ENDIF.
***  add by 冯飞剑（汉得） 2010-6-22 增加按单生产的剩余可用量 （S)
    "  按单的剩余可用量 = 订单库存 - 剩余需求数量
    "  缺料数量        =  剩余需求数量 - 订单库存
    IF T_ITEMS-KDAUF IS NOT INITIAL.   "如果对应的销售订单行项目不为空，说明是按单生产的
      T_ITEMS-ZMENGE_S = T_ITEMS-DDKALAB - T_ITEMS-ZSYXQS.          "剩余可用量
      T_ITEMS-ZQL_MENGE = T_ITEMS-ZSYXQS - T_ITEMS-DDKALAB.         "缺料数量
      IF T_ITEMS-ZMENGE_S >= 0.
        T_ITEMS-STATUS = '料齐'.
      ELSE.
        T_ITEMS-STATUS = '缺料'.
      ENDIF.
    ENDIF.
***  add by 冯飞剑（汉得） 2010-6-22 （E）
    MODIFY T_ITEMS.
  ENDLOOP.


ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_ZATP
*&---------------------------------------------------------------------*
FORM  GET_ZATP.
*  IF P_ORDBOX = 'X'.
*    SELECT *
*      APPENDING CORRESPONDING FIELDS OF TABLE T_ITEMS
*      FROM ZATP
*      JOIN AFKO ON ZATP~AUFNR = AFKO~AUFNR
*      JOIN AUFK ON ZATP~AUFNR = AUFK~AUFNR
*             WHERE ZATP~AUFNR IN S_AUFNR
*               AND ZATP~MATNR IN S_MATNR
*               AND ZATP~WERKS IN S_WERKS
*               AND AUFK~AUART IN S_AUART
*               AND ZATP~DISPO IN S_DISPO
*               AND AFKO~FEVOR IN S_FEVOR
*               AND AFKO~GSTRP IN S_GSTRP
*               AND AFKO~GLTRP IN S_GLTRP
*               AND AUFK~ERDAT IN S_ERDAT
*               AND ZFLAG = 'S'.
*  ENDIF.
*
*  IF P_JHOBOX = 'X'.
*    SELECT A~WERKS
*           A~ZYW_STATUS
*           A~AUFNR
*           A~MATNR A~IDNRK A~ZFLAG A~GAMNG A~STTXT A~BDTER A~ZMNGKO A~ZSYXQS A~LABST A~VMENG A~ZMENGE_S A~ZMZDAT A~ZT_MENGE A~ZZ_MENGE
*           A~ZJH_MENGE A~ALPGR  A~DISPO A~BESKZ A~EKGRP A~RGEKZ A~STATUS A~ZQL_MENGE A~ZSY_MENGE A~ZKY_MENGE A~ZDATE A~ZTIME A~ZDATE_F A~ZTIME_F
*    APPENDING CORRESPONDING FIELDS OF TABLE T_ITEMS
*    FROM ZATP AS A
*    JOIN PLAF ON A~AUFNR = PLAF~PLNUM
*           WHERE A~AUFNR IN S_PLNUM
*             AND A~MATNR IN S_PMATNR
*             AND A~WERKS IN S_PLWRK
*             AND A~DISPO IN S_PDISPO
*             AND PLGRP IN S_PLGRP
*             AND PSTTR IN S_PSTTR
*             AND PEDTR IN S_PEDTR
*             AND PALTR IN S_PALTR
*             AND ZFLAG = 'J'.
*  ENDIF.



  PERFORM GET_DESCRIBE.

ENDFORM.                    "GET_ZATP

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

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'STATUS'.
  WLT_FIELDCAT-SELTEXT_L    = '状态'.
*  WLT_FIELDCAT-ICON    = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZYW_STATUS'.
  WLT_FIELDCAT-SELTEXT_L    = '延误状态'.
*  WLT_FIELDCAT-ICON    = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.



  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'WERKS'.
  WLT_FIELDCAT-SELTEXT_L    = '工厂'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'AUFNR'.
  WLT_FIELDCAT-SELTEXT_L    = '订单号'.
  WLT_FIELDCAT-NO_ZERO    = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'MATNR'.
  WLT_FIELDCAT-SELTEXT_L    = '产品编号'.
  WLT_FIELDCAT-OUTPUTLEN = 18.
  WLT_FIELDCAT-NO_ZERO    = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'MAKTX'.
  WLT_FIELDCAT-SELTEXT_L    = '产品描述'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'GAMNG'.
  WLT_FIELDCAT-SELTEXT_L    = '订单数量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'STTXT'.
  WLT_FIELDCAT-SELTEXT_L    = '订单状态'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'IDNRK'.
  WLT_FIELDCAT-SELTEXT_L    = '组件编号'.
  WLT_FIELDCAT-OUTPUTLEN   =  18.
  WLT_FIELDCAT-NO_ZERO    = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'OJTXP'.
  WLT_FIELDCAT-SELTEXT_L    = '组件描述'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


*    CLEAR wlt_fieldcat.
*    wlt_fieldcat-fieldname    = 'MEINS'.
*    wlt_fieldcat-seltext_l    = '组件单位'.
**   WLT_FIELDCAT-SELTEXT_M    = 'Un'.
*    APPEND wlt_fieldcat TO wt_fieldcat.

*      CLEAR WLT_FIELDCAT.
*      WLT_FIELDCAT-FIELDNAME    = 'MEINS'.
*      WLT_FIELDCAT-SELTEXT_L    = '单位'.
**   WLT_FIELDCAT-SELTEXT_M    = 'Un'.
*      APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'BDTER'.
  WLT_FIELDCAT-SELTEXT_L    = '需求日期'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZMNGKO'.
  WLT_FIELDCAT-SELTEXT_L    = '原需求数量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZSYXQS'.
  WLT_FIELDCAT-SELTEXT_L    = '剩余需求数'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'LABST'.
  WLT_FIELDCAT-SELTEXT_L    = '库存量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'VMENG'.
  WLT_FIELDCAT-SELTEXT_L    = '确认分配量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZMENGE_S'.
  WLT_FIELDCAT-SELTEXT_L    = '剩余可用量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZMZDAT'.
  WLT_FIELDCAT-SELTEXT_L    = '满足日期'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZT_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '在途量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZZ_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '在制量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZJH_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '计划分配量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZQL_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '本订单缺料量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZSY_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '总剩余可用量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ZKY_MENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '总剩余可用量(+W.P)'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'MTART'.
  WLT_FIELDCAT-SELTEXT_L    = '物料类型'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'DISPO'.
  WLT_FIELDCAT-SELTEXT_L    = 'M号'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'DSNAM'.
  WLT_FIELDCAT-SELTEXT_L    = 'MRP控制者'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.


*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'FEVOR'.
*  wlt_fieldcat-seltext_l    = '生产调度员编号'.
*  append wlt_fieldcat to wt_fieldcat.
*
*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'TXT'.
*  wlt_fieldcat-seltext_l    = '生产调度员'.
*  append wlt_fieldcat to wt_fieldcat.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'BESKZ'.
  WLT_FIELDCAT-SELTEXT_L    = '采购类型'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'BESKZ_C'.
*  wlt_fieldcat-seltext_l    = '采购类型'.
*  append wlt_fieldcat to wt_fieldcat.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'EKGRP'.
  WLT_FIELDCAT-SELTEXT_L    = '采购组'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'EKNAM'.
  WLT_FIELDCAT-SELTEXT_L    = '采购组描述'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'DZEIT'.
*  wlt_fieldcat-seltext_l    = '自制提前期'.
*  append wlt_fieldcat to wt_fieldcat.
*
*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'PLIFZ'.
*  wlt_fieldcat-seltext_l    = '采购提前期'.
*  append wlt_fieldcat to wt_fieldcat.
*
*  clear wlt_fieldcat.
*  wlt_fieldcat-fieldname    = 'WEBAZ'.
*  wlt_fieldcat-seltext_l    = '收货处理时间'.
*  append wlt_fieldcat to wt_fieldcat.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'RGEKZ'.
  WLT_FIELDCAT-SELTEXT_L    = '反冲'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'ALPGR_C'.
  WLT_FIELDCAT-SELTEXT_L    = '可替代标识'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'DDKALAB'.
  WLT_FIELDCAT-SELTEXT_L    = '本单库存量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'QTKALAB'.
  WLT_FIELDCAT-SELTEXT_L    = '其他单库存总量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'DDMENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '本单在途数量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'QTMENGE'.
  WLT_FIELDCAT-SELTEXT_L    = '其他单在途总量'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'KDAUF'.
  WLT_FIELDCAT-SELTEXT_L    = '销售订单'.
  WLT_FIELDCAT-NO_ZERO      = 'X'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'KDPOS'.
  WLT_FIELDCAT-SELTEXT_L    = '销售订单行项目'.
  WLT_FIELDCAT-JUST         = 'C'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

  CLEAR WLT_FIELDCAT.
  WLT_FIELDCAT-FIELDNAME    = 'YQMNG'.
  WLT_FIELDCAT-SELTEXT_L    = '逾期在途数量'.
  WLT_FIELDCAT-NO_ZERO      = 'X'.
  WLT_FIELDCAT-JUST         = 'C'.
  APPEND WLT_FIELDCAT TO WT_FIELDCAT.

*  CLEAR P_MATNR1.
  SET PARAMETER ID 'ZMAT1' FIELD ''.
  SET PARAMETER ID 'ZPMAT' FIELD ''.
  SET PARAMETER ID 'ZMAT' FIELD ''.
  SET PARAMETER ID 'ZSTLAL' FIELD ''.
  WT_LAYOUT-ZEBRA = 'X'.
*  wt_layout-f2code = '&ETA'.
  WT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  PERFORM EVENT_BUILD.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      IS_LAYOUT                = WT_LAYOUT
*     i_callback_html_top_of_page = 'ALV_TOP_OF_PAGE'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      I_CALLBACK_USER_COMMAND  = 'EXECUTE_COMMAND'
      IT_FIELDCAT              = WT_FIELDCAT[]
      IT_EVENTS                = WT_EVENTS
    TABLES
      T_OUTTAB                 = T_ITEMS.


ENDFORM.                    " PRINT_ALV_DATA

*&---------------------------------------------------------------------*
*&      SET PF-STATUS
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'PF_STATUS'.        "ALV_PF_STATUS为GUI名字
ENDFORM. "Set_pf_status

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

    WHEN '&MATNR'.   "按物料统计
      PERFORM SET_BY_MATNR.

    WHEN '&EKGRP'.   "按采购组统计
      PERFORM SET_BY_EKGRP.

      "在set pf-status.的屏幕状态里面定义的
  ENDCASE.
ENDFORM.                    "EXECUTE_COMMAND

*---------------------------------------------------------------------*
*       FORM ALV_TOP_OF_PAGE                                          *
*---------------------------------------------------------------------*
FORM ALV_TOP_OF_PAGE.

  CLEAR: I_LIST_COMMENTS[].


  CLEAR L_DATE.
  CLEAR L_DATE_F.
  IF P_FRONT = 'X'.
    CONCATENATE S_DATE+0(4) '年' S_DATE+4(2) '月' S_DATE+6(2) '日' INTO L_DATE.
    CONCATENATE L_DATE '(' S_TIME+0(2) ':' S_TIME+2(2) ':' S_TIME+4(2) ')' INTO L_DATE.

    GET TIME.
    CONCATENATE SY-DATUM+0(4) '年' SY-DATUM+4(2) '月' SY-DATUM+6(2) '日' INTO L_DATE_F.
    CONCATENATE L_DATE_F '(' SY-UZEIT+0(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) ')' INTO L_DATE_F.
  ENDIF.

  IF P_BACK = 'X'.
    READ TABLE T_ITEMS INDEX 1.
    CONCATENATE T_ITEMS-ZDATE+0(4) '年' T_ITEMS-ZDATE+4(2) '月' T_ITEMS-ZDATE+6(2) '日' INTO L_DATE.
    CONCATENATE L_DATE '(' T_ITEMS-ZTIME+0(2) ':' T_ITEMS-ZTIME+2(2) ':' T_ITEMS-ZTIME+4(2) ')' INTO L_DATE.

    CONCATENATE T_ITEMS-ZDATE_F+0(4) '年' T_ITEMS-ZDATE_F+4(2) '月' T_ITEMS-ZDATE_F+6(2) '日' INTO L_DATE_F.
    CONCATENATE L_DATE_F '(' T_ITEMS-ZTIME_F+0(2) ':' T_ITEMS-ZTIME_F+2(2) ':' T_ITEMS-ZTIME_F+4(2) ')' INTO L_DATE_F.
  ENDIF.

  W_LIST_COMMENTS-TYP  = 'S'. " H = Header, S = Selection, A = Action
  W_LIST_COMMENTS-KEY  = ''.
  CONCATENATE '开始时间(日/时/分秒):' L_DATE INTO W_LIST_COMMENTS-INFO SEPARATED BY ''.
  APPEND W_LIST_COMMENTS TO I_LIST_COMMENTS.

  W_LIST_COMMENTS-TYP  = 'S'. " H = Header, S = Selection, A = Action
  W_LIST_COMMENTS-KEY  = ''.
  CONCATENATE '结束时间(日/时/分秒):' L_DATE_F INTO W_LIST_COMMENTS-INFO SEPARATED BY ''.
  APPEND W_LIST_COMMENTS TO I_LIST_COMMENTS.



  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     I_LOGO             = 'ENJOYSAP_LOGO'
      IT_LIST_COMMENTARY = I_LIST_COMMENTS.

ENDFORM.                    "alv_top_of_page

*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
FORM EVENT_BUILD.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = WT_EVENTS.

  READ TABLE WT_EVENTS
       WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
       INTO WS_EVENTS.
  IF SY-SUBRC = 0.
    MOVE 'ALV_TOP_OF_PAGE' TO WS_EVENTS-FORM.
    MODIFY WT_EVENTS FROM WS_EVENTS INDEX SY-TABIX.
  ENDIF.
ENDFORM.                    "EVENT_BUILD

*&---------------------------------------------------------------------*
*&      Form  取得订单
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ORDER .
  CLEAR S_DATE.
  CLEAR S_TIME.
  S_DATE = SY-DATUM.
  S_TIME = SY-UZEIT.
  REFRESH: T_AFKO.
*--------------取生产订单信息
  IF P_ORDBOX = 'X'.
    SELECT AFPO~AUFNR
           PSMNG
           MATNR
           DWERK
           GSTRP  "开始日期
           RSNUM
           GAMNG  "订单需求数量
           INTO CORRESPONDING FIELDS OF TABLE T_AFKO
           FROM AFPO
           JOIN AFKO ON AFPO~AUFNR = AFKO~AUFNR
           JOIN AUFK ON AFPO~AUFNR = AUFK~AUFNR
           WHERE AFPO~AUFNR IN S_AUFNR
             AND AFPO~MATNR IN S_MATNR
             AND AFPO~DWERK IN S_WERKS
             AND AUFK~AUART IN S_AUART
             AND AFKO~DISPO IN S_DISPO
             AND AFKO~FEVOR IN S_FEVOR
             AND AFKO~GSTRP IN S_GSTRP
             AND AFKO~GLTRP IN S_GLTRP
             AND AUFK~ERDAT IN S_ERDAT
      .
    DATA:   L_LINE    LIKE BSVX-STTXT,
            L_STAT(4) TYPE C.
    LOOP AT T_AFKO.
      CONCATENATE 'OR' T_AFKO-AUFNR INTO L_AUFNR.
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
          DELETE T_AFKO.
        ENDIF.
      ELSE.
        DELETE T_AFKO.
      ENDIF.

      T_AFKO-STTXT = L_LINE.
      MODIFY T_AFKO.
    ENDLOOP.
  ENDIF.
  IF P_JHOBOX = 'X'.
*---------取计划订单-----------------
    DATA:T_AFKO_JH LIKE T_AFKO OCCURS 0 WITH HEADER LINE.
    SELECT
    PLNUM AS AUFNR
    BDMNG AS PSMNG
    MATNR
    PLWRK AS DWERK
    PSTTR AS GSTRP
    RSNUM
    GSMNG AS GAMNG "订单数量
    INTO CORRESPONDING FIELDS OF TABLE T_AFKO_JH
    FROM PLAF
    WHERE PLNUM IN S_PLNUM
    AND MATNR IN S_PMATNR
    AND PLWRK IN S_PLWRK
    AND DISPO IN S_PDISPO
    AND PLGRP IN S_PLGRP
    AND PSTTR IN S_PSTTR
    AND PEDTR IN S_PEDTR
    AND PALTR IN S_PALTR.
    LOOP AT T_AFKO_JH.
      APPEND T_AFKO_JH TO T_AFKO.
    ENDLOOP.
  ENDIF.


  IF T_AFKO[] IS INITIAL.
    MESSAGE E001(ZPP635).
  ENDIF.


ENDFORM.                    " get_order

*&---------------------------------------------------------------------*
*&      Form  从RESB 取订单的组件\数量\需求日期等
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RESB_BOM .

  RANGES:S_ORDBOX FOR RESB-BDART.
  IF P_ORDBOX = 'X'.
    S_ORDBOX-LOW = 'AR'.
    S_ORDBOX-OPTION = 'EQ'.
    S_ORDBOX-SIGN = 'I'.
    APPEND S_ORDBOX.
  ENDIF.
  IF P_JHOBOX = 'X'.
    S_ORDBOX-LOW = 'SB'.
    S_ORDBOX-OPTION = 'EQ'.
    S_ORDBOX-SIGN = 'I'.
    APPEND S_ORDBOX.
  ENDIF.

  DATA:BEGIN OF T_BOM OCCURS 0 ,
         RSNUM LIKE RESB-RSNUM,
         RSPOS LIKE RESB-RSPOS,
         AUFNR LIKE RESB-AUFNR,
         PLNUM LIKE RESB-PLNUM,
         MATNR LIKE RESB-MATNR,
         BDMNG LIKE RESB-BDMNG,
         ENMNG LIKE RESB-ENMNG,
         MEINS LIKE RESB-MEINS,
         RGEKZ LIKE RESB-RGEKZ,
         BDTER LIKE RESB-BDTER,
         BDART LIKE RESB-BDART,
         WERKS LIKE RESB-WERKS,
         ALPGR LIKE RESB-ALPGR,
         KDAUF LIKE RESB-KDAUF,
         KDPOS LIKE RESB-KDPOS,
       END OF T_BOM.
*----------------取生产订单预留BOM-----------------
  SELECT RSNUM
         RSPOS
         AUFNR
         PLNUM
         MATNR
         BDMNG  "需求数
         ENMNG  "已交货数
         MEINS
         RGEKZ
         BDTER
         BDART
         WERKS
         ALPGR
         KDAUF  "销售订单
         KDPOS  "销售订单行项目
         INTO CORRESPONDING FIELDS OF TABLE T_BOM
         FROM RESB
         FOR ALL ENTRIES IN T_AFKO
         WHERE RSNUM = T_AFKO-RSNUM
*           AND WERKS = T_AFKO-DWERK
           AND BDART IN S_ORDBOX
           AND KDAUF IN S_KDAUF
           AND KDPOS IN S_KDPOS
           AND KZEAR <> 'X'  "最后发货
           AND XLOEK <> 'X'   "删除标识
           AND DUMPS <> 'X'   "虚拟项.
           AND SCHGT <> 'X'.   "散装

  LOOP AT T_BOM.
    IF T_BOM-AUFNR IS NOT INITIAL.
      T_ITEMS-AUFNR  = T_BOM-AUFNR.
    ELSEIF T_BOM-PLNUM IS NOT INITIAL.
      T_ITEMS-AUFNR  = T_BOM-PLNUM.
    ENDIF.
    T_ITEMS-WERKS  = T_BOM-WERKS .
    T_ITEMS-IDNRK  = T_BOM-MATNR . "组件料号
    T_ITEMS-ZMNGKO = T_BOM-BDMNG . "数量
    T_ITEMS-ZSYXQS = T_BOM-BDMNG - T_BOM-ENMNG.   "需求数量减去已发货数量   =   剩余需求数量
    T_ITEMS-MEINS  = T_BOM-MEINS . "单位
    T_ITEMS-BDTER  = T_BOM-BDTER.  "需求日期
    T_ITEMS-RGEKZ  = T_BOM-RGEKZ . "倒冲
    T_ITEMS-BDART  = T_BOM-BDART . "
    T_ITEMS-RSNUM  = T_BOM-RSNUM .
    T_ITEMS-RSPOS  = T_BOM-RSPOS .   "modify by YUHUI 2009.5.12
    T_ITEMS-ALPGR = T_BOM-ALPGR.
    T_ITEMS-KDAUF = T_BOM-KDAUF.
    T_ITEMS-KDPOS = T_BOM-KDPOS.
    COLLECT T_ITEMS.
  ENDLOOP.


  IF T_ITEMS[] IS INITIAL.
    MESSAGE E004(ZPP635).
  ENDIF.

  LOOP AT T_ITEMS.
    READ TABLE T_AFKO WITH KEY AUFNR = T_ITEMS-AUFNR.
    IF SY-SUBRC = 0.
      T_ITEMS-STTXT = T_AFKO-STTXT.
      T_ITEMS-GAMNG = T_AFKO-GAMNG.
      T_ITEMS-MATNR = T_AFKO-MATNR.
      MODIFY T_ITEMS.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " GET_AUFNR
*&---------------------------------------------------------------------*
*&      Form  取得订单的数量和交货数量
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
       AND WERKS = T_ITEMS-WERKS
       AND KNTTP <> 'M'  "科目分配类别 = 'M' 表示按单采购
       AND LOEKZ <> 'L'  "标记删除
       AND ELIKZ <> 'X'. "交货已完成

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
*&      Form  计算可以满足需求的日期
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MZ_DAT .

  REFRESH :T_MZTAB.

*-------------------在途量添加
  LOOP AT T_EKPO WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS.
    T_MZTAB-MZLNR = T_EKPO-EBELN.
    T_MZTAB-MATNR = T_EKPO-MATNR.
    T_MZTAB-MENGE = T_EKPO-MENGE - T_EKPO-WEMNG.
    T_MZTAB-ZMZDAT = T_EKPO-EINDT.
    APPEND T_MZTAB.
  ENDLOOP.
*--------------------在制量添加
  LOOP AT T_AFPO WHERE MATNR = T_ITEMS-IDNRK AND WERKS = T_ITEMS-WERKS.
    T_MZTAB-MZLNR = T_AFPO-AUFNR.
    T_MZTAB-MATNR = T_AFPO-MATNR.
    T_MZTAB-MENGE = T_AFPO-PSMNG -  T_AFPO-WEMNG - T_AFPO-IAMNG .
    T_MZTAB-ZMZDAT = T_AFPO-GLTRP.
    APPEND T_MZTAB.
  ENDLOOP.

  SORT T_MZTAB BY ZMZDAT MENGE.

  L_MENGE_S = T_ITEMS-ZSY_MENGE.
  LOOP AT T_MZTAB.

    L_MENGE_S = T_MZTAB-MENGE + L_MENGE_S.
    IF L_MENGE_S >= 0.
      T_ITEMS-ZMZDAT = T_MZTAB-ZMZDAT.
      EXIT.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " mz_dat
*&---------------------------------------------------------------------*
*&      Form  订单状态搜索帮助
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SYST   text
*      -->P_TEXT_006 text
*----------------------------------------------------------------------*
FORM GET_TJ02  USING    P_P_SYST
      P_TEXT_006.

*-- Set up fields to retrieve data
  LTAB_FIELDS-TABNAME    = 'TJ02T'.
  LTAB_FIELDS-FIELDNAME  = 'TXT04'.
  LTAB_FIELDS-SELECTFLAG = 'X'.
  APPEND LTAB_FIELDS.
  LTAB_FIELDS-TABNAME    = 'TJ02T'.
  LTAB_FIELDS-FIELDNAME  = 'TXT30'.
  LTAB_FIELDS-SELECTFLAG = SPACE.
  APPEND LTAB_FIELDS.
  REFRESH LTAB_VALUES.
*-- Fill values
  SELECT * FROM TJ02T WHERE SPRAS = 1.
    LTAB_VALUES-FELD = TJ02T-TXT04.
    APPEND LTAB_VALUES.
    LTAB_VALUES-FELD = TJ02T-TXT30.
    APPEND LTAB_VALUES.
  ENDSELECT.


  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      FIELDNAME                 = 'TXT04'
      TABNAME                   = 'TJ02T'
      TITLE_IN_VALUES_LIST      = 'Select a value'
    IMPORTING
      SELECT_VALUE              = P_P_SYST
    TABLES
      FIELDS                    = LTAB_FIELDS
      VALUETAB                  = LTAB_VALUES
    EXCEPTIONS
      FIELD_NOT_IN_DDIC         = 01
      MORE_THEN_ONE_SELECTFIELD = 02
      NO_SELECTFIELD            = 03.

ENDFORM.                                                    " get_tj02
*&---------------------------------------------------------------------*
*&      Form  get_t161v
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_T161V .
*-----------取得所有公司间采购订单类型
  SELECT
    BSART
    RESWK
  INTO CORRESPONDING FIELDS OF TABLE T_161V
    FROM T161V.
ENDFORM.                                                    " get_t161v


*&---------------------------------------------------------------------*
*&      Form  check_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATE .

  IF P_ORDBOX IS NOT INITIAL.
    IF S_GSTRP IS INITIAL AND  S_GLTRP IS INITIAL AND  S_ERDAT IS INITIAL.
      MESSAGE W002(ZPP635).
    ENDIF.
  ENDIF.
  IF  P_JHOBOX IS NOT INITIAL.
    IF S_PSTTR  IS INITIAL AND  S_PEDTR  IS INITIAL AND  S_PALTR  IS INITIAL .
      MESSAGE W003(ZPP635).
    ENDIF.
  ENDIF.

ENDFORM.                    " check_date


*&---------------------------------------------------------------------*
*&      Form  get_describe
*&---------------------------------------------------------------------*
FORM GET_DESCRIBE.
*------------取物料信息  (物料描述,物料类型)
  SELECT MARA~MATNR
         MTART
         MAKTX
         INTO CORRESPONDING FIELDS OF TABLE T_MATNR_INFO
         FROM MARA
         JOIN MAKT ON MARA~MATNR = MAKT~MATNR
         FOR ALL ENTRIES IN T_ITEMS
         WHERE SPRAS = 1
           AND ( MARA~MATNR = T_ITEMS-MATNR
           OR    MARA~MATNR = T_ITEMS-IDNRK ).


  DATA:BEGIN OF T_MARC OCCURS 0,
         MATNR TYPE MARC-MATNR,
         WERKS TYPE MARC-WERKS,
         DISPO TYPE MARC-DISPO,
         FEVOR TYPE MARC-FEVOR,
         BESKZ TYPE MARC-BESKZ,
         DZEIT TYPE MARC-DZEIT,
         PLIFZ TYPE MARC-PLIFZ,
         WEBAZ TYPE MARC-WEBAZ,
         EKGRP TYPE MARC-EKGRP,
       END OF T_MARC.

  "MRP控制者（MARC-DISPO）及描述（V_T024D-DSNAM），生产调度员（MARC-FEVOR）字段及描述（V_T024F-FEVORTXXT）,采购类型 BESKZ
  SELECT    MATNR
            WERKS
            DISPO
            FEVOR
            BESKZ
            DZEIT
            PLIFZ
            WEBAZ
            EKGRP
            INTO CORRESPONDING FIELDS OF TABLE T_MARC
            FROM MARC
            FOR ALL ENTRIES IN T_ITEMS
            WHERE MATNR = T_ITEMS-IDNRK
            AND WERKS = T_ITEMS-WERKS.

  LOOP AT T_ITEMS.
    READ TABLE T_MARC WITH KEY MATNR = T_ITEMS-IDNRK WERKS = T_ITEMS-WERKS.
    IF SY-SUBRC = 0.
      T_ITEMS-DISPO = T_MARC-DISPO.
      T_ITEMS-FEVOR = T_MARC-FEVOR.
      T_ITEMS-BESKZ = T_MARC-BESKZ.
      T_ITEMS-DZEIT = T_MARC-DZEIT.
      T_ITEMS-PLIFZ = T_MARC-PLIFZ.
      T_ITEMS-WEBAZ = T_MARC-WEBAZ.
      T_ITEMS-EKGRP = T_MARC-EKGRP.
    ENDIF.

*-------------------读入物料信息
    READ TABLE T_MATNR_INFO WITH KEY MATNR = T_ITEMS-IDNRK.
    IF SY-SUBRC = 0.
      T_ITEMS-OJTXP = T_MATNR_INFO-MAKTX.
      T_ITEMS-MTART = T_MATNR_INFO-MTART.
    ENDIF.

*-------------产品物料描述
    READ TABLE T_MATNR_INFO WITH KEY MATNR = T_ITEMS-MATNR.
    IF SY-SUBRC = 0.
      T_ITEMS-MAKTX = T_MATNR_INFO-MAKTX.
    ENDIF.

    IF T_ITEMS-ALPGR <> ''.
      T_ITEMS-ALPGR_C =  'X'.  "可代用标志
    ENDIF.

    MODIFY T_ITEMS.
  ENDLOOP.
  REFRESH T_MARC.
  REFRESH T_MATNR_INFO.


*-------------MRP控制者名称
  DATA: BEGIN OF T_T024D OCCURS 0,
          DISPO TYPE T024D-DISPO,
          DSNAM TYPE T024D-DSNAM,
          WERKS TYPE T024D-WERKS,
        END OF T_T024D.
  SELECT  DSNAM
          DISPO
          WERKS
    INTO CORRESPONDING FIELDS OF TABLE T_T024D
    FROM T024D
    FOR ALL ENTRIES IN T_ITEMS
    WHERE DISPO = T_ITEMS-DISPO
      AND WERKS = T_ITEMS-WERKS.          "ADD BY 冯飞剑（汉得） 2010-8-11

*-------------调度描述
  DATA:BEGIN OF T_T024F OCCURS 0,
         FEVOR TYPE T024F-FEVOR,
         TXT   TYPE T024F-TXT,
         WERKS TYPE T024F-WERKS,
       END OF T_T024F.
  SELECT TXT
         FEVOR
         WERKS
   INTO CORRESPONDING FIELDS OF TABLE T_T024F
   FROM T024F
   FOR ALL ENTRIES IN T_ITEMS
   WHERE FEVOR = T_ITEMS-FEVOR
     AND WERKS = T_ITEMS-WERKS.           "ADD BY 冯飞剑（汉得） 2010-8-11

*-------------采购组描述
  DATA:BEGIN OF T_EKGRP OCCURS 0,
         EKGRP TYPE T024-EKGRP,
         EKNAM TYPE T024-EKNAM,
       END OF T_EKGRP.
  SELECT EKNAM
         EKGRP
    INTO CORRESPONDING FIELDS OF TABLE T_EKGRP
    FROM T024
    FOR ALL ENTRIES IN T_ITEMS
    WHERE EKGRP = T_ITEMS-EKGRP.

  LOOP AT T_ITEMS.
    READ TABLE T_T024D WITH KEY DISPO = T_ITEMS-DISPO WERKS = T_ITEMS-WERKS.      "MOD BY 冯飞剑（汉得） 2010-8-11 添加工厂限制
    IF SY-SUBRC = 0.
      T_ITEMS-DSNAM = T_T024D-DSNAM.
    ENDIF.

    READ TABLE T_T024F WITH KEY FEVOR = T_ITEMS-FEVOR WERKS = T_ITEMS-WERKS.      "MOD BY 冯飞剑（汉得） 2010-8-11 添加工厂限制
    IF SY-SUBRC = 0.
      T_ITEMS-TXT = T_T024F-TXT.
    ENDIF.

    IF T_ITEMS-EKGRP <> ''.
      READ TABLE T_EKGRP WITH KEY EKGRP = T_ITEMS-EKGRP.
      IF SY-SUBRC = 0.
        T_ITEMS-EKNAM =  T_EKGRP-EKNAM.
      ENDIF.
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
  REFRESH T_EKGRP.
  REFRESH T_T024D.
  REFRESH T_T024F.

ENDFORM.                    "GET_DESCRIBE


*&---------------------------------------------------------------------*
*&      Form  set_by_matnr
*&---------------------------------------------------------------------*
FORM SET_BY_MATNR.
  REFRESH T_MATNR_R.
  LOOP AT T_ITEMS WHERE IDNRK <> ''.
    MOVE-CORRESPONDING T_ITEMS TO T_MATNR.
    APPEND T_MATNR.
  ENDLOOP.

  SORT T_MATNR BY IDNRK .

  DELETE ADJACENT DUPLICATES FROM T_MATNR COMPARING ALL FIELDS.


  LOOP AT T_MATNR.
    AT NEW IDNRK.
      READ TABLE T_MATNR INDEX SY-TABIX.
      T_MATNR_R-IDNRK  = T_MATNR-IDNRK.
      T_MATNR_R-OJTXP  = T_MATNR-OJTXP.
      T_MATNR_R-EKGRP  = T_MATNR-EKGRP.
      T_MATNR_R-EKNAM  = T_MATNR-EKNAM.
    ENDAT.

    IF T_MATNR-ZYW_STATUS = '无合同'.
      WHT_N  = WHT_N + 1.
    ELSEIF   T_MATNR-ZYW_STATUS = '合同过期'.
      GQ_N = GQ_N + 1.
    ELSEIF   T_MATNR-ZYW_STATUS = '合同超期'.
      CQ_N = CQ_N + 1.
    ENDIF.


    AT END OF IDNRK.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = T_MATNR_R-IDNRK
        IMPORTING
          OUTPUT = T_MATNR_R-IDNRK.
      T_MATNR_R-WHT_NUM = WHT_N.
      T_MATNR_R-GQ_NUM = GQ_N.
      T_MATNR_R-CQ_NUM = CQ_N.
      T_MATNR_R-HJ = WHT_N + GQ_N + CQ_N.
      APPEND T_MATNR_R.
      CLEAR WHT_N.
      CLEAR GQ_N.
      CLEAR CQ_N.
    ENDAT.

  ENDLOOP.
  REFRESH T_MATNR.

  PERFORM ALV_MATNR.

ENDFORM.                    "SET_BY_MATNR


*&---------------------------------------------------------------------*
*&      Form  set_by_ekgrp
*&---------------------------------------------------------------------*
FORM SET_BY_EKGRP.
  DATA:P_YWL TYPE P DECIMALS 4.
*  CLEAR TOTAL.
*  CLEAR TOTAL.
  REFRESH T_EKGRP_R.
  LOOP AT T_ITEMS WHERE EKGRP <> '' AND EKGRP <> 'Z03'.
    CLEAR T_EKGRP.
    MOVE-CORRESPONDING T_ITEMS TO T_EKGRP.
    APPEND T_EKGRP.
  ENDLOOP.

*  DESCRIBE TABLE T_ITEMS LINES TOTAL.
*  TOTAL_C  = TOTAL.

  SORT T_EKGRP BY EKGRP.
  LOOP AT T_EKGRP.
    AT NEW EKGRP.
      READ TABLE T_EKGRP INDEX SY-TABIX.
      T_EKGRP_R-EKGRP  = T_EKGRP-EKGRP.
      T_EKGRP_R-EKNAM  = T_EKGRP-EKNAM.
    ENDAT.

    IF T_EKGRP-ZYW_STATUS = '无合同'.
      WHT_N  = WHT_N + 1.
    ELSEIF   T_EKGRP-ZYW_STATUS = '合同过期'.
      GQ_N = GQ_N + 1.
    ELSEIF   T_EKGRP-ZYW_STATUS = '合同超期'.
      CQ_N = CQ_N + 1.
    ENDIF.
    TOTAL = TOTAL + 1.

    "按物流、外协。总计统计延误率
    IF T_EKGRP-EKGRP+0(1) = 'Z' AND T_EKGRP-EKGRP <> 'Z03'.  "外协
      IF T_EKGRP-ZYW_STATUS = '无合同'.
        WX_WHT = WX_WHT + 1.
      ELSEIF T_EKGRP-ZYW_STATUS = '合同过期'.
        WX_GQ  = WX_GQ + 1.
      ELSEIF T_EKGRP-ZYW_STATUS = '合同超期'.
        WX_CQ  = WX_CQ + 1.
      ENDIF.
      WX_TOTAL =  WX_TOTAL + 1.

    ELSEIF T_EKGRP-EKGRP+0(1) = 'W' OR T_EKGRP-EKGRP+0(1) = 'H'.  "物流
      IF T_EKGRP-ZYW_STATUS = '无合同'.
        WL_WHT = WL_WHT + 1.
      ELSEIF T_EKGRP-ZYW_STATUS = '合同过期'.
        WL_GQ  = WL_GQ + 1.
      ELSEIF T_EKGRP-ZYW_STATUS = '合同超期'.
        WL_CQ  = WL_CQ + 1.
      ENDIF.
      WL_TOTAL =  WL_TOTAL + 1.
    ENDIF.

    IF T_EKGRP-ZYW_STATUS = '无合同'.
      TOTAL_WHT =   TOTAL_WHT + 1.
    ELSEIF T_EKGRP-ZYW_STATUS = '合同过期'.
      TOTAL_GQ = TOTAL_GQ + 1.
    ELSEIF T_EKGRP-ZYW_STATUS = '合同超期'.
      TOTAL_CQ = TOTAL_CQ + 1.
    ENDIF.
    TOTAL_TOTAL = TOTAL_TOTAL + 1.

    AT END OF EKGRP.
      T_EKGRP_R-WHT_NUM = WHT_N.
      T_EKGRP_R-GQ_NUM = GQ_N.
      T_EKGRP_R-CQ_NUM = CQ_N.
      T_EKGRP_R-TOTAL_NUM = TOTAL.
      P_YWL = ( WHT_N + GQ_N + CQ_N ) / TOTAL.
      T_EKGRP_R-YWL = P_YWL * 100.
      APPEND T_EKGRP_R.
      CLEAR WHT_N.
      CLEAR GQ_N.
      CLEAR CQ_N.
      CLEAR TOTAL.
      CLEAR P_YWL.
    ENDAT.

  ENDLOOP.
  REFRESH T_EKGRP.


  CLEAR T_EKGRP_R.
  T_EKGRP_R-EKNAM = '外协控制部物料延误率'.
  T_EKGRP_R-WHT_NUM = WX_WHT.
  T_EKGRP_R-GQ_NUM = WX_GQ.
  T_EKGRP_R-CQ_NUM = WX_CQ.
  T_EKGRP_R-TOTAL_NUM = WX_TOTAL.
  P_YWL = ( WX_WHT + WX_GQ + WX_CQ ) / WX_TOTAL.
  T_EKGRP_R-YWL =  P_YWL * 100.
  APPEND T_EKGRP_R.

  T_EKGRP_R-EKNAM = '物流中心物料延误率'.
  T_EKGRP_R-WHT_NUM = WL_WHT.
  T_EKGRP_R-GQ_NUM = WL_GQ.
  T_EKGRP_R-CQ_NUM = WL_CQ.
  T_EKGRP_R-TOTAL_NUM = WL_TOTAL.
  CLEAR P_YWL.
  P_YWL = ( WL_WHT + WL_GQ + WL_CQ ) / WL_TOTAL.
  T_EKGRP_R-YWL =  P_YWL * 100.
  APPEND T_EKGRP_R.

  T_EKGRP_R-EKNAM = 'SAP系统物料延误率'.
  T_EKGRP_R-WHT_NUM = TOTAL_WHT.
  T_EKGRP_R-GQ_NUM = TOTAL_GQ.
  T_EKGRP_R-CQ_NUM = TOTAL_CQ.
  T_EKGRP_R-TOTAL_NUM = TOTAL_TOTAL.
  CLEAR P_YWL.
  P_YWL = ( TOTAL_WHT + TOTAL_GQ + TOTAL_CQ ) / TOTAL_TOTAL.
  T_EKGRP_R-YWL =  P_YWL * 100.
  APPEND T_EKGRP_R.


  CLEAR WX_WHT.  CLEAR WX_GQ. CLEAR WX_CQ. CLEAR WX_TOTAL.
  CLEAR WL_WHT.  CLEAR WL_GQ. CLEAR WL_CQ. CLEAR WL_TOTAL.
  CLEAR TOTAL_WHT.  CLEAR TOTAL_GQ. CLEAR TOTAL_CQ. CLEAR TOTAL_TOTAL.
  PERFORM ALV_EKGRP.
ENDFORM.                    "SET_BY_EKGRP

*&---------------------------------------------------------------------*
*&      Form  alv_matnr
*&---------------------------------------------------------------------*
FORM  ALV_MATNR.
  REFRESH WT_FIELDCAT.

  DATA:  MATNR_FIELDCAT LIKE LINE OF WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'IDNRK'.
  MATNR_FIELDCAT-SELTEXT_L    = '物料编码'.
  MATNR_FIELDCAT-OUTPUTLEN = 18.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'OJTXP'.
  MATNR_FIELDCAT-SELTEXT_L    = '物料描述'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'EKGRP'.
  MATNR_FIELDCAT-SELTEXT_L    = '采购组'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'EKNAM'.
  MATNR_FIELDCAT-SELTEXT_L    = '采购组描述'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'WHT_NUM'.
  MATNR_FIELDCAT-SELTEXT_L    = '无合同订单数量'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'GQ_NUM'.
  MATNR_FIELDCAT-SELTEXT_L    = '合同过期订单数量'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'CQ_NUM'.
  MATNR_FIELDCAT-SELTEXT_L    = '合同超期订单数量'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.


  CLEAR MATNR_FIELDCAT.
  MATNR_FIELDCAT-FIELDNAME    = 'HJ'.
  MATNR_FIELDCAT-SELTEXT_L    = '（无合同+过期+超期）数量'.
  APPEND MATNR_FIELDCAT TO WT_FIELDCAT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT          = WT_LAYOUT
      I_SAVE             = 'A'
      IT_FIELDCAT        = WT_FIELDCAT[]
*     it_events          = wt_events
    TABLES
      T_OUTTAB           = T_MATNR_R.
ENDFORM.                    "ALV_MATNR


*&---------------------------------------------------------------------*
*&      Form  alv_ekgrp
*&---------------------------------------------------------------------*
FORM  ALV_EKGRP.
  REFRESH WT_FIELDCAT.

  DATA:  EKGRP_FIELDCAT LIKE LINE OF WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'EKGRP'.
  EKGRP_FIELDCAT-SELTEXT_L    = '采购组'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'EKNAM'.
  EKGRP_FIELDCAT-SELTEXT_L    = '采购组描述'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'WHT_NUM'.
  EKGRP_FIELDCAT-SELTEXT_L    = '无合同订单数量'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'GQ_NUM'.
  EKGRP_FIELDCAT-SELTEXT_L    = '合同过期订单数量'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'CQ_NUM'.
  EKGRP_FIELDCAT-SELTEXT_L    = '合同超期订单数量'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'TOTAL_NUM'.
  EKGRP_FIELDCAT-SELTEXT_L    = '所属物料数的订单数量'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

  CLEAR EKGRP_FIELDCAT.
  EKGRP_FIELDCAT-FIELDNAME    = 'YWL'.
  EKGRP_FIELDCAT-SELTEXT_L    = '物料延误率'.
  APPEND EKGRP_FIELDCAT TO WT_FIELDCAT.

*  PERFORM EVENT_EKGRP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT          = WT_LAYOUT
      I_SAVE             = 'A'
      IT_FIELDCAT        = WT_FIELDCAT[]
*     IT_EVENTS          = WT_EVENTS
    TABLES
      T_OUTTAB           = T_EKGRP_R.
ENDFORM.                    "ALV_EKGRP

**---------------------------------------------------------------------*
**       FORM ALV_TOP_OF_ekgrp                                          *
**---------------------------------------------------------------------*
*FORM ALV_TOP_OF_EKGRP.
*
*  CLEAR: I_LIST_COMMENTS[].
*
*  W_LIST_COMMENTS-TYP  = 'S'. " H = Header, S = Selection, A = Action
*  W_LIST_COMMENTS-KEY  = ''.
*  CONCATENATE '总的物料行数：' TOTAL_C '行' INTO W_LIST_COMMENTS-INFO SEPARATED BY ''.
*  APPEND W_LIST_COMMENTS TO I_LIST_COMMENTS.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
**      I_LOGO             = 'ENJOYSAP_LOGO'
*      IT_LIST_COMMENTARY = I_LIST_COMMENTS.
*
*ENDFORM.                    "alv_top_of_page
*
**&---------------------------------------------------------------------*
**&      Form  event_build
**&---------------------------------------------------------------------*
*FORM EVENT_EKGRP.
*
*  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*    EXPORTING
*      I_LIST_TYPE = 0
*    IMPORTING
*      ET_EVENTS   = WT_EVENTS.
*
*  READ TABLE WT_EVENTS
*       WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
*       INTO WS_EVENTS.
*  IF SY-SUBRC = 0.
*    MOVE 'ALV_TOP_OF_EKGRP' TO WS_EVENTS-FORM.
*    MODIFY WT_EVENTS FROM WS_EVENTS INDEX SY-TABIX.
*  ENDIF.
*ENDFORM.

*Text elements
*----------------------------------------------------------
* 001 生产订单
* 002 计划订单
* 003 销售订单
* EXC 排斥
* SYS 系统状态


*Selection texts
*----------------------------------------------------------
* P_BACK         后台作业
* P_FRONT         前台执行
* P_JHOBOX         计划订单
* P_ORDBOX         生产订单
* S_AUART         订单类型
* S_AUFNR         生产订单
* S_DISPO         MRP控制者
* S_ERDAT         创建日期
* S_FEVOR         生产调度员
* S_GLTRP         基本完成日期
* S_GSTRP         基本开始日期
* S_KDAUF         销售订单
* S_KDPOS         行项目
* S_MATNR         物料
* S_PALTR         创建日期
* S_PDISPO         MRP控制者
* S_PEDTR         基本完成日期
* S_PLGRP         生产调度员
* S_PLNUM         计划订单
* S_PLWRK         计划工厂
* S_PMATNR         物料
* S_PSTTR         基本开始日期
* S_WERKS         生产工厂


*Messages
*----------------------------------------------------------
*YPE
*
* Message class: ZPP635
*001   没有找到符合条件的生产订单!
*002   生产订单没有输入任何日期限制,数据庞大,可能运行超时.
*003   计划订单没有输入任何日期限制,数据庞大,可能运行超时.
*004   没有找到相关数据，请重新选择.


*--------------------------------------------------------------------------------
