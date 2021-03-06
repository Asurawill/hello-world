
*&--------------------------------------------------------------------*
*& Report  ZDM002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2016-12-30  it02                ED1K905147   增加 “是否需要上传受控文件”特征值
*&---------------------------------------------------------------------*

REPORT ZMM002.


TYPE-POOLS: SLIS,VRM.
TABLES:MARA,MARC,MAKT,MARD,MLAN,MVKE,MBEW,MVGD,QMAT.

TYPES: BEGIN OF T1_OUT,
         BISMT       LIKE MARA-BISMT,
         MATNR       LIKE MARA-MATNR,
         LVORM       LIKE MARA-LVORM,
         MAKTX       LIKE MAKT-MAKTX,
         MAKTX_EN    LIKE MAKT-MAKTX,
         MAKTX_Z1    LIKE MAKT-MAKTX,
         MBRSH       LIKE MARA-MBRSH,
         MTART       LIKE MARA-MTART,
         WERKS       LIKE MARC-WERKS,
         LGORT       LIKE MARD-LGORT,
         MEINS       LIKE MARA-MEINS,
         MATKL       LIKE MARA-MATKL,
         SPART       LIKE MARA-SPART,
         FERTH       LIKE MARA-FERTH,
         NORMT       LIKE MARA-NORMT,
         ZEINR       LIKE MARA-ZEINR,
         AESZN       LIKE MARA-AESZN,
         ERSDA       LIKE MARA-ERSDA,
         MSTAE       LIKE MARA-MSTAE,
         MHDRZ       LIKE MARA-MHDRZ,
         MHDHB       LIKE MARA-MHDHB,
         IPRKZ       LIKE MARA-IPRKZ,
         EPRKZ       LIKE PRDKZT-EPRKZ, " 期间标识
         BSTME       LIKE MARA-BSTME,
         LABOR       LIKE MARA-LABOR,
         LAEDA       LIKE MARA-LAEDA,
         EXTWG       LIKE MARA-EXTWG,
         LONG_TEXT   TYPE STRING,
         LAENG       LIKE MARA-LAENG,
         BREIT       LIKE MARA-BREIT,
         HOEHE       LIKE MARA-HOEHE,
         MEABM       LIKE MARA-MEABM,
         MAGRV       LIKE MARA-MAGRV,
         BESKZ       LIKE MARC-BESKZ,
         GGXH        TYPE CHAR30,
         XSJJ        TYPE CHAR30,
         ROHS        TYPE CHAR30,
         SFYGD       TYPE CHAR30,
         CHANGJIA    TYPE CHAR30,
         DHH         TYPE STRING , " CHAR30,
         CPLX        TYPE CHAR30,
         TEZZ        TYPE STRING, " CHAR30,
         SHENQINGREN TYPE CHAR30,
         SKWJ        TYPE CHAR30,  "是否需要上传受控文件
         CGWB(500)   TYPE C,
       END OF T1_OUT.

TYPES: BEGIN OF T2_OUT,
         BISMT LIKE MARA-BISMT,
         MATNR LIKE MARA-MATNR,
         LVORM LIKE MARC-LVORM,
         MTART LIKE MARA-MTART,
         MBRSH LIKE MARA-MBRSH,
         WERKS LIKE MARC-WERKS,
         MAKTX LIKE MAKT-MAKTX,
         MEINS LIKE MARA-MEINS,
         MATKL LIKE MARA-MATKL,
         SPART LIKE MARA-SPART,
         MAABC LIKE MARC-MAABC,
         DISMM LIKE MARC-DISMM,
         DISPO LIKE MARC-DISPO,
         DISLS LIKE MARC-DISLS,
         MINBE LIKE MARC-MINBE,
         BSTFE LIKE MARC-BSTFE,
         BSTMI LIKE MARC-BSTMI,
         BSTRF LIKE MARC-BSTRF,
         BSTMA LIKE MARC-BSTMA,
         BESKZ LIKE MARC-BESKZ,
         SOBSL LIKE MARC-SOBSL,
         LGPRO LIKE MARC-LGPRO,
         LGFSB LIKE MARC-LGFSB,
         SCHGT LIKE MARC-SCHGT,
         DZEIT LIKE MARC-DZEIT,
         PLIFZ LIKE MARC-PLIFZ,
         FHORI LIKE MARC-FHORI,
         EISBE LIKE MARC-EISBE,
         STRGR LIKE MARC-STRGR,
         MTVFP LIKE MARC-MTVFP,
         SBDKZ LIKE MARC-SBDKZ,
         KZAUS LIKE MARC-KZAUS,
         AUSDT LIKE MARC-AUSDT,
         NFMAT LIKE MARC-NFMAT,
         FEVOR LIKE MARC-FEVOR,
         ERSDA LIKE MARA-ERSDA,
         DISGR LIKE MARC-DISGR,
         RGEKZ LIKE MARC-RGEKZ,
         KAUSF LIKE MARC-KAUSF,
         BEARZ LIKE MARC-BEARZ,
         BASMG LIKE MARC-BASMG,
         SFCPF LIKE MARC-SFCPF,
         WEBAZ LIKE MARC-WEBAZ,
         AUSSS LIKE MARC-AUSSS,
*ADD BY HANDWY 2015,3,5.
         MABST LIKE MARC-MABST, "最大库存水平
         VRMOD LIKE MARC-VRMOD, "消耗模式
         VINT1 LIKE MARC-VINT1, "逆向消耗期间
         VINT2 LIKE MARC-VINT2, "向前消耗期间
         RUEZT LIKE MARC-RUEZT, "准备时间
*&--代码添加 BY HANDYBY 27.07.2017 09:29:45  BEGIN
         EXTWG LIKE MARA-EXTWG ,  " 外部物料组
*&--代码添加 BY HANDYBY 27.07.2017 09:29:45  END
       END OF T2_OUT.

TYPES: BEGIN OF T3_OUT,
         BISMT LIKE MARA-BISMT,
         MATNR LIKE MARA-MATNR,
         LVORM LIKE MVKE-LVORM,
         MTART LIKE MARA-MTART,
         MBRSH LIKE MARA-MBRSH,
         VKORG LIKE MVKE-VKORG,
         VTWEG LIKE MVKE-VTWEG,
         MAKTX LIKE MAKT-MAKTX,
         DWERK LIKE MVKE-DWERK,
         TAXM1 LIKE MLAN-TAXM1,
         KTGRM LIKE MVKE-KTGRM,
         MTPOS LIKE MVKE-MTPOS,
         MTVFP LIKE MARC-MTVFP,
         TRAGR LIKE MARA-TRAGR,
         LADGR LIKE MARC-LADGR,
         ERSDA LIKE MARA-ERSDA,
*ADD BY HANDWY 2015,3,5.
         MVGR1 LIKE MVKE-MVGR1,
         MVGR2 LIKE MVKE-MVGR2,
         MVGR3 LIKE MVKE-MVGR3,
         MVGR4 LIKE MVKE-MVGR4,
         MVGR5 LIKE MVKE-MVGR5,
       END OF T3_OUT.

TYPES: BEGIN OF T4_OUT,
         BISMT     LIKE MARA-BISMT,
         MATNR     LIKE MARA-MATNR,
         LVORM     LIKE MARC-LVORM,
         MAKTX     LIKE MAKT-MAKTX,
         MBRSH     LIKE MARA-MBRSH,
         MTART     LIKE MARA-MTART,
         EKGRP     LIKE MARC-EKGRP,
         XCHPF     LIKE MARC-XCHPF,
         WERKS     LIKE MARC-WERKS,
         KORDB     LIKE MARC-KORDB,
         ERSDA     LIKE MARA-ERSDA,
         CGWB(500) TYPE C,
       END OF T4_OUT.

TYPES: BEGIN OF T5_OUT,
         BISMT LIKE MARA-BISMT,
         MATNR LIKE MARA-MATNR,
         LVORM LIKE MARC-LVORM,
         MAKTX LIKE MAKT-MAKTX,
         MBRSH LIKE MARA-MBRSH,
         MTART LIKE MARA-MTART,
         WERKS LIKE MARC-WERKS,
         LGORT LIKE MARD-LGORT,
         MHDRZ LIKE MARA-MHDRZ,
         MHDHB LIKE MARA-MHDHB,
         "LVORM LIKE MARA-LVORM,
         ERSDA LIKE MARA-ERSDA,
         LOGGR LIKE MARC-LOGGR, " 后勤处理组
       END OF T5_OUT.

TYPES: BEGIN OF T6_OUT,
         BISMT LIKE MARA-BISMT,
         MATNR LIKE MARA-MATNR,
         LVORM LIKE MARC-LVORM,
         MAKTX LIKE MAKT-MAKTX,
         MBRSH LIKE MARA-MBRSH,
         MTART LIKE MARA-MTART,
         WERKS LIKE MARC-WERKS,
         BKLAS LIKE MBEW-BKLAS,
         VPRSV LIKE MBEW-VPRSV,
         VERPR LIKE MBEW-VERPR,
         STPRS LIKE MBEW-STPRS,
         PEINH LIKE MBEW-PEINH,
         HKMAT LIKE MBEW-HKMAT,
         MMSTA LIKE MARC-MMSTA,
         LOSGR LIKE MARC-LOSGR,
         ZPLP1 LIKE MBEW-ZPLP1,
         ZPLD1 LIKE MBEW-ZPLD1,
         ERSDA LIKE MARA-ERSDA,
         MLAST LIKE MBEW-MLAST, " 价格确定
         BWKEY LIKE MBEW-BWKEY,
         HRKFT LIKE MBEW-HRKFT, " 原始组
         ZPLP2 LIKE MBEW-ZPLP2, " 计划价格2
         ZPLD2 LIKE MBEW-ZPLD2, " 计划价格2日期
       END OF T6_OUT.

TYPES: BEGIN OF T7_OUT,
         BISMT     LIKE MARA-BISMT,
         MATNR     LIKE MARA-MATNR,
         MAKTX     LIKE MAKT-MAKTX,
         MBRSH     LIKE MARA-MBRSH,
         MTART     LIKE MARA-MTART,
         WERKS     LIKE MARC-WERKS,
         LGORT     LIKE MARD-LGORT,
         MEINS     LIKE MARA-MEINS,
         MATKL     LIKE MARA-MATKL,
         SPART     LIKE MARA-SPART,
         FERTH     LIKE MARA-FERTH,
         NORMT     LIKE MARA-NORMT,
         ZEINR     LIKE MARA-ZEINR,
         AESZN     LIKE MARA-AESZN,   "后面差 类别种类，类别，标准栏"
         ERSDA     LIKE MARA-ERSDA,
         MSTAE     LIKE MARA-MSTAE,
         VKORG     LIKE MVKE-VKORG,
         VTWEG     LIKE MVKE-VTWEG,
         DWERK     LIKE MVKE-DWERK,
         TAXM1     LIKE MLAN-TAXM1,
         MTPOS     LIKE MVKE-MTPOS,
         TRAGR     LIKE MARA-TRAGR,
         LADGR     LIKE MARC-LADGR,
         EKGRP     LIKE MARC-EKGRP,
         XCHPF     LIKE MARC-XCHPF,
         KORDB     LIKE MARC-KORDB,
         MAABC     LIKE MARC-MAABC,
         DISMM     LIKE MARC-DISMM,
         DISPO     LIKE MARC-DISPO,
         DISLS     LIKE MARC-DISLS,
         MINBE     LIKE MARC-MINBE,
         BSTFE     LIKE MARC-BSTFE,
         BSTMI     LIKE MARC-BSTMI,
         BSTRF     LIKE MARC-BSTRF,
         BESKZ     LIKE MARC-BESKZ,
         SOBSL     LIKE MARC-SOBSL,
         LGPRO     LIKE MARC-LGPRO,
         LGFSB     LIKE MARC-LGFSB,
         DZEIT     LIKE MARC-DZEIT,
         PLIFZ     LIKE MARC-PLIFZ,
         FHORI     LIKE MARC-FHORI,
         EISBE     LIKE MARC-EISBE,
         STRGR     LIKE MARC-STRGR,
         MTVFP     LIKE MARC-MTVFP,
         PRGRP     LIKE MVGD-PRGRP,  "计划物料"
         PRWRK     LIKE MVGD-PRWRK,  "计划工厂"
         SBDKZ     LIKE MARC-SBDKZ,
         KZAUS     LIKE MARC-KZAUS,
         AUSDT     LIKE MARC-AUSDT,
         NFMAT     LIKE MARC-NFMAT,
         FEVOR     LIKE MARC-FEVOR,
         SFCPF     LIKE MARC-SFCPF,
         MHDRZ     LIKE MARA-MHDRZ,
         MHDHB     LIKE MARA-MHDHB,
         BKLAS     LIKE MBEW-BKLAS,
         VPRSV     LIKE MBEW-VPRSV,
         PEINH     LIKE MBEW-PEINH,
         NCOST     LIKE MARC-NCOST,
         EKALR     LIKE MBEW-EKALR,
         HKMAT     LIKE MBEW-HKMAT,
         MMSTA     LIKE MARC-MMSTA,
         LOSGR     LIKE MARC-LOSGR,
         ZPLP1     LIKE MBEW-ZPLP1,
         ZPLD1     LIKE MBEW-ZPLD1,
         MLAST     LIKE MBEW-MLAST, " 价格确定
         CGWB(500) TYPE C,
         DISGR     LIKE MARC-DISGR,
         RGEKZ     LIKE MARC-RGEKZ,
         KAUSF     LIKE MARC-KAUSF,
         BEARZ     LIKE MARC-BEARZ,
         BASMG     LIKE MARC-BASMG,
         WEBAZ     LIKE MARC-WEBAZ,
         AUSSS     LIKE MARC-AUSSS,
*ADD BY HANDWY 2015,3,5.
         MABST     LIKE MARC-MABST, "最大库存水平
         VRMOD     LIKE MARC-VRMOD, "消耗模式
         VINT1     LIKE MARC-VINT1, "逆向消耗期间
         VINT2     LIKE MARC-VINT2, "向前消耗期间
         RUEZT     LIKE MARC-RUEZT, "准备时间
         MVGR1     LIKE MVKE-MVGR1,
         MVGR2     LIKE MVKE-MVGR2,
         MVGR3     LIKE MVKE-MVGR3,
         MVGR4     LIKE MVKE-MVGR4,
         MVGR5     LIKE MVKE-MVGR5,
       END OF T7_OUT.

" 质检视图
TYPES: BEGIN OF T8_OUT,
         WERKS LIKE MARC-WERKS,
         MATNR LIKE MARA-MATNR,
         LVORM LIKE MARC-LVORM,
         EXTWG LIKE MARA-MTART,
         ART   LIKE QMAT-ART,
         APA   LIKE QMAT-APA,
         AKTIV LIKE QMAT-AKTIV,
         MTART LIKE MARA-MTART,
       END OF T8_OUT.

DATA: IT_JBST TYPE T1_OUT OCCURS 0 WITH HEADER LINE,
      IT_GCST TYPE T2_OUT OCCURS 0 WITH HEADER LINE,
      IT_XSST TYPE T3_OUT OCCURS 0 WITH HEADER LINE,
      IT_CGST TYPE T4_OUT OCCURS 0 WITH HEADER LINE,
      IT_KCST TYPE T5_OUT OCCURS 0 WITH HEADER LINE,
      IT_CWST TYPE T6_OUT OCCURS 0 WITH HEADER LINE,
      IT_ZJST TYPE T8_OUT OCCURS 0 WITH HEADER LINE,
      IT_HZST TYPE T7_OUT OCCURS 0 WITH HEADER LINE.

DATA: IT_MARC TYPE STANDARD TABLE OF MARC WITH HEADER LINE,
      IT_MARA TYPE STANDARD TABLE OF MARA WITH HEADER LINE,
      IT_MARD TYPE STANDARD TABLE OF MARD WITH HEADER LINE,
      IT_MAKT TYPE STANDARD TABLE OF MAKT WITH HEADER LINE,
      IT_MLAN TYPE STANDARD TABLE OF MLAN WITH HEADER LINE,
      IT_MVKE TYPE STANDARD TABLE OF MVKE WITH HEADER LINE,
      IT_MBEW TYPE STANDARD TABLE OF MBEW WITH HEADER LINE,
      IT_MVGD TYPE STANDARD TABLE OF MVGD WITH HEADER LINE.

DATA: LS_FIELDCAT    TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GS_LAYOUT      TYPE SLIS_LAYOUT_ALV,
      GIT_LISTHEADER TYPE SLIS_T_LISTHEADER,        "ALV 表头
      GIT_EVENTS     TYPE SLIS_T_EVENT,             "ALV 事件
      V_STRU_DISVAR  TYPE DISVARIANT,
      G_REPID        LIKE SY-REPID.

DATA GT_T001W  TYPE TABLE OF T001W WITH HEADER LINE.

*分类视图
DATA:
  IT_NUM  LIKE BAPI1003_ALLOC_VALUES_NUM  OCCURS 0 WITH HEADER LINE,
  IT_CHAR LIKE BAPI1003_ALLOC_VALUES_CHAR OCCURS 0 WITH HEADER LINE,
  IT_CURR LIKE BAPI1003_ALLOC_VALUES_CURR OCCURS 0 WITH HEADER LINE,
  IT_RET  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:P1 TYPE C RADIOBUTTON GROUP G1 USER-COMMAND UC DEFAULT 'X',
           P2 TYPE C RADIOBUTTON GROUP G1,
           P3 TYPE C RADIOBUTTON GROUP G1,
           P4 TYPE C RADIOBUTTON GROUP G1,
           P5 TYPE C RADIOBUTTON GROUP G1,
           P6 TYPE C RADIOBUTTON GROUP G1,
           P8 TYPE C RADIOBUTTON GROUP G1,
           P7 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:S_MATNR FOR MARA-MATNR MODIF ID MI1.    "物料号"
SELECT-OPTIONS:S_BISMT FOR MARA-BISMT MODIF ID MI1.    "旧物料号
SELECT-OPTIONS:S_MTART FOR MARA-MTART MODIF ID MI1.    "物料类型"
SELECT-OPTIONS:S_MATKL FOR MARA-MATKL MODIF ID MI1.    "物料组"
SELECT-OPTIONS:S_EXTWG FOR MARA-EXTWG MODIF ID MI1.    "外部物料组
*SELECT-OPTIONS:S_WERKS FOR MARC-WERKS MODIF ID MI1.    "工厂"
*SELECT-OPTIONS:S_BESKZ FOR MARC-BESKZ MODIF ID MI1.    "采购类型"
SELECT-OPTIONS:S_ERSDA FOR MARA-ERSDA MODIF ID MI1.    "创建日期"
SELECT-OPTIONS:S_LAEDA FOR MARA-LAEDA MODIF ID MI1.    "上次更改
SELECT-OPTIONS:S_MSTAE FOR MARA-MSTAE MODIF ID MI1.    "跨工厂物料状态"


SELECT-OPTIONS:S_MATNR2 FOR MARA-MATNR MODIF ID MI2.
SELECT-OPTIONS:S_BISMT2 FOR MARA-BISMT MODIF ID MI2.    "旧物料号
SELECT-OPTIONS:S_MTART2 FOR MARA-MTART MODIF ID MI2.
SELECT-OPTIONS:S_MATKL2 FOR MARA-MATKL MODIF ID MI2.   "物料组"
SELECT-OPTIONS:S_EXTWG2 FOR MARA-EXTWG MODIF ID MI2.   "外部物料组
SELECT-OPTIONS:S_WERKS2 FOR MARC-WERKS MODIF ID MI2.
SELECT-OPTIONS:S_DISPO  FOR MARC-DISPO MODIF ID MI2.   "MRP控制者"
SELECT-OPTIONS:S_BESKZ2 FOR MARC-BESKZ MODIF ID MI2.
SELECT-OPTIONS:S_ERSDA2 FOR MARA-ERSDA MODIF ID MI2.
SELECT-OPTIONS:S_LAEDA2 FOR MARA-LAEDA MODIF ID MI2.    "上次更改


SELECT-OPTIONS:S_MATNR3 FOR MARA-MATNR MODIF ID MI3.
SELECT-OPTIONS:S_BISMT3 FOR MARA-BISMT MODIF ID MI3.    "旧物料号
SELECT-OPTIONS:S_MTART3 FOR MARA-MTART MODIF ID MI3.
SELECT-OPTIONS:S_MATKL3 FOR MARA-MATKL MODIF ID MI3.    "物料组"
SELECT-OPTIONS:S_EXTWG3 FOR MARA-EXTWG MODIF ID MI3.    "外部物料组
SELECT-OPTIONS:S_VKORG  FOR MVKE-VKORG MODIF ID MI3.    "销售组织"
SELECT-OPTIONS:S_VTWGE  FOR MVKE-VTWEG MODIF ID MI3.    "分销渠道"
SELECT-OPTIONS:S_ERSDA3 FOR MARA-ERSDA MODIF ID MI3.
SELECT-OPTIONS:S_LAEDA3 FOR MARA-LAEDA MODIF ID MI3.     "上次更改


SELECT-OPTIONS:S_MATNR4 FOR MARA-MATNR MODIF ID MI4.
SELECT-OPTIONS:S_BISMT4 FOR MARA-BISMT MODIF ID MI4.    "旧物料号
SELECT-OPTIONS:S_MTART4 FOR MARA-MTART MODIF ID MI4.
SELECT-OPTIONS:S_MATKL4 FOR MARA-MATKL MODIF ID MI4.     "物料组"
SELECT-OPTIONS:S_EXTWG4 FOR MARA-EXTWG MODIF ID MI4.     "外部物料组
SELECT-OPTIONS:S_EKGRP  FOR MARC-EKGRP MODIF ID MI4.    "采购组"
SELECT-OPTIONS:S_WERKS4 FOR MARC-WERKS MODIF ID MI4.
SELECT-OPTIONS:S_ERSDA4 FOR MARA-ERSDA MODIF ID MI4.
SELECT-OPTIONS:S_LAEDA4 FOR MARA-LAEDA MODIF ID MI4.     "上次更改


SELECT-OPTIONS:S_MATNR5 FOR MARA-MATNR MODIF ID MI5.
SELECT-OPTIONS:S_BISMT5 FOR MARA-BISMT MODIF ID MI5.    "旧物料号
SELECT-OPTIONS:S_MTART5 FOR MARA-MTART MODIF ID MI5.
SELECT-OPTIONS:S_MATKL5 FOR MARA-MATKL MODIF ID MI5.     "物料组"
SELECT-OPTIONS:S_EXTWG5 FOR MARA-EXTWG MODIF ID MI5.     "外部物料组
SELECT-OPTIONS:S_WERKS5 FOR MARC-WERKS MODIF ID MI5.
SELECT-OPTIONS:S_LGORT  FOR MARD-LGORT MODIF ID MI5.   "库存地点"
SELECT-OPTIONS:S_LVORM  FOR MARA-LVORM MODIF ID MI5.     "物料删除标识"
SELECT-OPTIONS:S_ERSDA5 FOR MARA-ERSDA MODIF ID MI5.
SELECT-OPTIONS:S_LAEDA5 FOR MARA-LAEDA MODIF ID MI5.    "上次更改

SELECT-OPTIONS:S_MATNR6 FOR MARA-MATNR MODIF ID MI6.
SELECT-OPTIONS:S_BISMT6 FOR MARA-BISMT MODIF ID MI6.    "旧物料号
SELECT-OPTIONS:S_MTART6 FOR MARA-MTART MODIF ID MI6.
SELECT-OPTIONS:S_MATKL6 FOR MARA-MATKL MODIF ID MI6.    "物料组"
SELECT-OPTIONS:S_EXTWG6 FOR MARA-EXTWG MODIF ID MI6.    "外部物料组
SELECT-OPTIONS:S_WERKS6 FOR MARC-WERKS MODIF ID MI6.
SELECT-OPTIONS:S_VPRSV  FOR MBEW-VPRSV MODIF ID MI6.     "价格控制"
SELECT-OPTIONS:S_BESKZ6 FOR MARC-BESKZ MODIF ID MI6.
SELECT-OPTIONS:S_ERSDA6 FOR MARA-ERSDA MODIF ID MI6.
SELECT-OPTIONS:S_LAEDA6 FOR MARA-LAEDA MODIF ID MI6.    "上次更改

SELECT-OPTIONS:S_MATNR7 FOR MARA-MATNR MODIF ID MI7.
SELECT-OPTIONS:S_BISMT7 FOR MARA-BISMT MODIF ID MI7.    "旧物料号
SELECT-OPTIONS:S_MTART7 FOR MARA-MTART MODIF ID MI7.
SELECT-OPTIONS:S_MATKL7 FOR MARA-MATKL MODIF ID MI7.   "物料组"
SELECT-OPTIONS:S_EXTWG7 FOR MARA-EXTWG MODIF ID MI7.   "外部物料组
SELECT-OPTIONS:S_WERKS7 FOR MARC-WERKS MODIF ID MI7.
SELECT-OPTIONS:S_ERSDA7 FOR MARA-ERSDA MODIF ID MI7.
SELECT-OPTIONS:S_LAEDA7 FOR MARA-LAEDA MODIF ID MI7.  "上次更改


SELECT-OPTIONS:S_MATNR8 FOR MARA-MATNR MODIF ID MI8.
SELECT-OPTIONS:S_BISMT8 FOR MARA-BISMT MODIF ID MI8.    "旧物料号
SELECT-OPTIONS:S_MTART8 FOR MARA-MTART MODIF ID MI8.
SELECT-OPTIONS:S_WERKS8 FOR MARC-WERKS MODIF ID MI8.
SELECT-OPTIONS:S_ART FOR QMAT-ART MODIF ID MI8.

SELECTION-SCREEN END OF BLOCK B2 .

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    DEFINE SET_SCREEN.
      if screen-group1 = &1.
        screen-active = '1'.
      elseif screen-group1 cp 'MI*'.
        screen-active = '0'.
      endif.
    END-OF-DEFINITION.

    CASE 'X'.
      WHEN P1.
        SET_SCREEN 'MI1'.
      WHEN P2.
        SET_SCREEN 'MI2'.
      WHEN P3.
        SET_SCREEN 'MI3'.
      WHEN P4.
        SET_SCREEN 'MI4'.
      WHEN P5.
        SET_SCREEN 'MI5'.
      WHEN P6.
        SET_SCREEN 'MI6'.
      WHEN P7.
        SET_SCREEN 'MI7'.
      WHEN P8.
        SET_SCREEN 'MI8'.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

*查询工厂
  SELECT * FROM T001W
    INTO CORRESPONDING FIELDS OF TABLE GT_T001W.

  PERFORM FRM_AUTH_CHECK.

  PERFORM FRM_DATA.

END-OF-SELECTION.

  PERFORM INIT_LAYOUT.
  PERFORM INIT_FIELDCAT.
  CASE 'X'.
    WHEN P1 .
      PERFORM FRM_OUTPUT TABLES IT_JBST.
    WHEN P2.
      PERFORM FRM_OUTPUT TABLES IT_GCST.
    WHEN P3.
      PERFORM FRM_OUTPUT TABLES IT_XSST.
    WHEN P4.
      PERFORM FRM_OUTPUT TABLES IT_CGST.
    WHEN P5.
      PERFORM FRM_OUTPUT TABLES IT_KCST.
    WHEN P6.
      PERFORM FRM_OUTPUT TABLES IT_CWST.
    WHEN P7.
      PERFORM FRM_OUTPUT TABLES IT_HZST.
    WHEN P8.
      PERFORM FRM_OUTPUT TABLES IT_ZJST.
  ENDCASE.


FORM INIT_LAYOUT.
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-DETAIL_POPUP = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.

FORM INIT_FIELDCAT.

  DATA: LS_FCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR: LS_FIELDCAT[].

  DEFINE BUILD_FIELDCAT.
    ls_fcat-fieldname = &1.
    ls_fcat-seltext_s = &2.
    ls_fcat-seltext_m = &2.
    ls_fcat-seltext_l = &2.
    ls_fcat-outputlen = &3.
    if ls_fcat-fieldname = 'MATNR'.
      ls_fcat-no_zero = 'X'.
    endif.
*    ls_fcat-convexit = &4.
    append ls_fcat to ls_fieldcat.
    clear ls_fcat.
  END-OF-DEFINITION.

  CASE 'X'.
    WHEN P1.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''  .
      BUILD_FIELDCAT 'LVORM' '集团级的DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''  .
      BUILD_FIELDCAT 'MAKTX_EN' '英文物料描述' ''.
      BUILD_FIELDCAT 'MAKTX_Z1'  'Z1物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''  .
      BUILD_FIELDCAT 'MTART' '物料类型' ''  .
*      BUILD_FIELDCAT 'WERKS' '工厂' ''  .
      BUILD_FIELDCAT 'LGORT' '库存地点' ''  .
      BUILD_FIELDCAT 'MEINS' '基本单位' ''  .
      BUILD_FIELDCAT 'MATKL' '物料组' ''  .
*      BUILD_FIELDCAT 'EXTWG' '外部物料组' ''  .
      BUILD_FIELDCAT 'MHDRZ' '最小剩余货架寿命' ''  .
      BUILD_FIELDCAT 'MHDHB' '总货架寿命' ''  .
      BUILD_FIELDCAT 'EPRKZ' 'SLED 的期间标识' ''  .
      BUILD_FIELDCAT 'BSTME' '订单单位' ''  .
*      BUILD_FIELDCAT 'LABOR' '实验室/办公室' ''  .
      BUILD_FIELDCAT 'SPART' '产品组' ''.
*      BUILD_FIELDCAT 'FERTH' '项目编号' ''  .
*      BUILD_FIELDCAT 'NORMT' '物料系列' ''  .
*      BUILD_FIELDCAT 'ZEINR' '装入整件图号' ''  .
*      BUILD_FIELDCAT 'AESZN' '成品密级' ''  .
      BUILD_FIELDCAT 'LAEDA' '上次更改日期' ''.
      BUILD_FIELDCAT 'ERSDA' '创建日期' ''.
      BUILD_FIELDCAT 'LONG_TEXT' '长文本' ''.
      BUILD_FIELDCAT 'LAENG' '长度' ''.
      BUILD_FIELDCAT 'BREIT' '宽度' ''.
      BUILD_FIELDCAT 'HOEHE' '高度' ''.
      BUILD_FIELDCAT 'MEABM' '长度/宽度/高度的尺寸单位' ''.
      BUILD_FIELDCAT 'CHANGJIA' '厂家' ''.
      BUILD_FIELDCAT 'CPLX' '产品类型' ''.
      BUILD_FIELDCAT 'DHH' '订货号' ''.
      BUILD_FIELDCAT 'GGXH' '规格型号' ''.
      BUILD_FIELDCAT 'XSJJ' '相素间距' ''.
      BUILD_FIELDCAT 'ROHS' 'ROHS认证' ''.
      BUILD_FIELDCAT 'SFYGD' '是否亿光灯' ''.
      BUILD_FIELDCAT 'SKWJ' '是否需要上传受控文件' ''.
      BUILD_FIELDCAT 'TEZZ' '特征值' ''.
      BUILD_FIELDCAT 'SHENQINGREN' '申请人' ''.
      BUILD_FIELDCAT 'CGWB' '采购文本' ''.

*      BUILD_FIELDCAT 'MAGRV' '物料组: 包装物料' ''.

    WHEN P2.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '工厂级DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'WERKS' '工厂' ''.
      BUILD_FIELDCAT 'MEINS' '基本单位' ''.
      BUILD_FIELDCAT 'MATKL' '物料组' ''.
      BUILD_FIELDCAT 'SPART' '产品组' ''.
*      BUILD_FIELDCAT 'MAABC' 'ABC标识' ''.
      BUILD_FIELDCAT 'DISMM' 'MRP类型' ''.
      BUILD_FIELDCAT 'DISPO' 'MRP控制者' ''.
      BUILD_FIELDCAT 'DISLS' '批量大小' ''.
      BUILD_FIELDCAT 'MINBE' '再订货点' ''.
      BUILD_FIELDCAT 'BSTFE' '固定批量大小' ''.
      BUILD_FIELDCAT 'BSTMI' '最小批量大小' ''.
      BUILD_FIELDCAT 'BSTRF' '舍入值' ''.
      BUILD_FIELDCAT 'BSTMA' '最大批量大小' ''.
      BUILD_FIELDCAT 'BESKZ' '采购类型' ''.
      BUILD_FIELDCAT 'SOBSL' '特殊采购类' ''.
      BUILD_FIELDCAT 'LGPRO' '生产仓储地点' ''.
      BUILD_FIELDCAT 'LGFSB' '外部采购仓储地点' ''.
*      BUILD_FIELDCAT 'SCHGT' '散装物料' ''.
      BUILD_FIELDCAT 'DZEIT' '自制生产时间' ''.
      BUILD_FIELDCAT 'PLIFZ' '计划交货时间' ''.
      BUILD_FIELDCAT 'FHORI' '计划边际码' ''.
      BUILD_FIELDCAT 'EISBE' '安全库存' ''.
      BUILD_FIELDCAT 'STRGR' '策略组' ''.
      BUILD_FIELDCAT 'MTVFP' '可用性检查' ''.
      BUILD_FIELDCAT 'SBDKZ' '独立/集中' ''.
*      BUILD_FIELDCAT 'KZAUS' '非连续标识' ''.
*      BUILD_FIELDCAT 'AUSDT' '生效期' ''.
*      BUILD_FIELDCAT 'NFMAT' '后续的物料' ''.
      BUILD_FIELDCAT 'FEVOR' '生产调度员' ''.
      BUILD_FIELDCAT 'DISGR' 'MRP组' ''.
      BUILD_FIELDCAT 'RGEKZ' '标识：反冲' ''.
      BUILD_FIELDCAT 'KAUSF' '部件废品' ''.
      BUILD_FIELDCAT 'BEARZ' '处理时间' ''.
      BUILD_FIELDCAT 'BASMG' '基准数量' ''.
      BUILD_FIELDCAT 'SFCPF' '生产计划参数文件' ''.
      BUILD_FIELDCAT 'WEBAZ' '收货处理时间' ''.
      BUILD_FIELDCAT 'AUSSS' '装配件报废率' ''.
      BUILD_FIELDCAT 'MABST' '最大库存水平' ''.
      BUILD_FIELDCAT 'VRMOD' '消耗模式' ''.
      BUILD_FIELDCAT 'VINT1' '逆向消耗期间' ''.
      BUILD_FIELDCAT 'VINT2' '向前消耗期间' ''.
      BUILD_FIELDCAT 'RUEZT' '准备时间' ''.
*&--代码添加 BY HANDYBY 27.07.2017 09:23:42  BEGIN
      BUILD_FIELDCAT 'EXTWG' '外部物料组' ''.
*&--代码添加 BY HANDYBY 27.07.2017 09:23:42  END


    WHEN P3.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '发布链级别的DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'VKORG' '销售组织' ''.
      BUILD_FIELDCAT 'VTWEG' '分销渠道' ''.
      BUILD_FIELDCAT 'DWERK' '交货工厂' ''.
      BUILD_FIELDCAT 'TAXM1' '税分类' ''.
      BUILD_FIELDCAT 'KTGRM' '科目设置组' ''.
      BUILD_FIELDCAT 'MTPOS' '项目类别组' ''.
      BUILD_FIELDCAT 'MTVFP' '可用性检查' ''.
      BUILD_FIELDCAT 'TRAGR' '运输组' ''.
      BUILD_FIELDCAT 'LADGR' '装载组' ''.
      BUILD_FIELDCAT 'MVGR1' '物料组1' ''.
      BUILD_FIELDCAT 'MVGR2' '物料组2' ''.
      BUILD_FIELDCAT 'MVGR3' '物料组3' ''.
      BUILD_FIELDCAT 'MVGR4' '物料组4' ''.
      BUILD_FIELDCAT 'MVGR5' '物料组5' ''.


    WHEN P4.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '工厂级DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'WERKS' '工厂' ''.
      BUILD_FIELDCAT 'EKGRP' '采购组' ''.
      BUILD_FIELDCAT 'XCHPF' '批次管理' ''.
      BUILD_FIELDCAT 'KORDB' '源清单' ''.
      BUILD_FIELDCAT 'CGWB' '采购订单文本' ''.

    WHEN P5.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '工厂级DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'WERKS' '工厂' ''.
      BUILD_FIELDCAT 'MHDRZ' '最小货架寿命' ''.
      BUILD_FIELDCAT 'MHDHB' '总货架寿命' ''.
      BUILD_FIELDCAT 'LVORM' '物料删除标识' ''.
      BUILD_FIELDCAT 'LOGGR' '后勤处理组' ''.

    WHEN P6.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '工厂级DF' ''  .
      BUILD_FIELDCAT 'MAKTX' '物料描述' ''.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'BKLAS' '评估类' ''.
      BUILD_FIELDCAT 'VPRSV' '价格控制' ''.
*      BUILD_FIELDCAT 'VERPR' '移动平均价' ''.
      BUILD_FIELDCAT 'PEINH' '价格单位' ''.
      BUILD_FIELDCAT 'HKMAT' '物料来源' ''.
      BUILD_FIELDCAT 'MMSTA' '特定工厂的物料状态' ''.
      BUILD_FIELDCAT 'LOSGR' '成本核算批量' ''.
*价格权限控制
      AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
      IF SY-SUBRC = 0.
        BUILD_FIELDCAT 'ZPLP1' '计划价格1' ''.
        BUILD_FIELDCAT 'ZPLD1' '计划价格日期1' ''.
      ENDIF.
      BUILD_FIELDCAT 'MLAST' '价格确定' ''.
      BUILD_FIELDCAT 'BWKEY' '评估范围' ''.
      BUILD_FIELDCAT 'HRKFT' '原始组' ''.
*价格权限控制
      AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
      IF SY-SUBRC = 0.
        BUILD_FIELDCAT 'ZPLP2' '计划价格2' ''.
        BUILD_FIELDCAT 'ZPLD2' '计划价格2日期' ''.
        BUILD_FIELDCAT 'VERPR' '周期价格' ''.
        BUILD_FIELDCAT 'STPRS' '标准价格' ''.
      ENDIF.

    WHEN P7.
      BUILD_FIELDCAT 'BISMT' '旧物料号' ''.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'MAKTX' '物料描述' '80'.
      BUILD_FIELDCAT 'MBRSH' '行业类别' ''.
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'WERKS' '工厂' ''.
      BUILD_FIELDCAT 'LGORT' '库存地点' ''.
      BUILD_FIELDCAT 'MEINS' '基本单位' ''.
      BUILD_FIELDCAT 'MATKL' '物料组' ''.
      BUILD_FIELDCAT 'SPART' '产品组' ''.
*      BUILD_FIELDCAT 'FERTH' '项目编号' ''.
*      BUILD_FIELDCAT 'NORMT' '物料系列' ''.
*      BUILD_FIELDCAT 'ZEINR' '装入整件图号' ''.
*      BUILD_FIELDCAT 'AESZN' '成品密级' ''.
      BUILD_FIELDCAT 'VKORG' '销售组织' ''.
      BUILD_FIELDCAT 'VTWEG' '分销渠道' ''.
      BUILD_FIELDCAT 'DWERK' '交货工厂' ''.
      BUILD_FIELDCAT 'TAXM1' '税分类' ''.
      BUILD_FIELDCAT 'MTPOS' '项目类别组' ''.
      BUILD_FIELDCAT 'MTVFP' '可用性检查' ''.
      BUILD_FIELDCAT 'TRAGR' '运输组' ''.
      BUILD_FIELDCAT 'LADGR' '装载组' ''.
      BUILD_FIELDCAT 'EKGRP' '采购组' ''.
      BUILD_FIELDCAT 'XCHPF' '批次管理' ''.
      BUILD_FIELDCAT 'KORDB' '源清单' ''.
*      BUILD_FIELDCAT 'MAABC' 'ABC标识' ''.
      BUILD_FIELDCAT 'DISMM' 'MRP类型' ''.
      BUILD_FIELDCAT 'DISPO' 'MRP控制者' ''.
      BUILD_FIELDCAT 'DISLS' '批量大小' ''.
      BUILD_FIELDCAT 'MINBE' '再订货点' ''.
      BUILD_FIELDCAT 'BSTFE' '固定批量大小' ''.
      BUILD_FIELDCAT 'BSTMI' '最小批量大小' ''.
      BUILD_FIELDCAT 'BSTRF' '舍入值' ''.
      BUILD_FIELDCAT 'BESKZ' '采购类型' ''.
      BUILD_FIELDCAT 'SOBSL' '特殊采购类' ''.
      BUILD_FIELDCAT 'LGPRO' '生产仓储地点' ''.
      BUILD_FIELDCAT 'LGFSB' '外部采购仓储地点' ''.
      BUILD_FIELDCAT 'DZEIT' '自制生产时间' ''.
      BUILD_FIELDCAT 'PLIFZ' '计划交货时间' ''.
      BUILD_FIELDCAT 'FHORI' '计划边际码' ''.
      BUILD_FIELDCAT 'EISBE' '安全库存' ''.
      BUILD_FIELDCAT 'STRGR' '策略组' ''.
      BUILD_FIELDCAT 'MTVFP' '可用性检查' ''.
      BUILD_FIELDCAT 'SBDKZ' '独立/集中' ''.
*      BUILD_FIELDCAT 'KZAUS' '非连续标识' ''.
*      BUILD_FIELDCAT 'AUSDT' '生效期' ''.
*      BUILD_FIELDCAT 'NFMAT' '后续的物料' ''.
      BUILD_FIELDCAT 'FEVOR' '生产调度员' ''.
      BUILD_FIELDCAT 'PRGRP' '计划物料' ''.
      BUILD_FIELDCAT 'PRWRK' '计划工厂' ''.
      BUILD_FIELDCAT 'SFCPF' '生产计划参数文件' ''.
      BUILD_FIELDCAT 'MHDRZ' '最小货架寿命' ''.
      BUILD_FIELDCAT 'MHDHB' '总货架寿命' ''.
      BUILD_FIELDCAT 'BKLAS' '评估类' ''.
      BUILD_FIELDCAT 'VPRSV' '价格控制' ''.
      BUILD_FIELDCAT 'PEINH' '价格单位' ''.
      BUILD_FIELDCAT 'NCOST' '无成本核算' ''.
      BUILD_FIELDCAT 'EKALR' '用QS的成本估算' ''.
      BUILD_FIELDCAT 'HKMAT' '物料来源' ''.
      BUILD_FIELDCAT 'MMSTA' '特定工厂的物料状态' ''.
      BUILD_FIELDCAT 'LOSGR' '成本核算批量' ''.
*价格权限控制
      AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
      IF SY-SUBRC = 0.
        BUILD_FIELDCAT 'ZPLP1' '计划价格1' ''.
        BUILD_FIELDCAT 'ZPLD1' '计划价格日期1' ''.
      ENDIF.
      BUILD_FIELDCAT 'CGWB'  '采购订单文本' ''.
      BUILD_FIELDCAT 'MLAST' '价格确定' ''.
      BUILD_FIELDCAT 'DISGR' 'MRP组' ''.
      BUILD_FIELDCAT 'RGEKZ' '标识：反冲' ''.
      BUILD_FIELDCAT 'KAUSF' '部件废品' ''.
      BUILD_FIELDCAT 'BEARZ' '处理时间' ''.
      BUILD_FIELDCAT 'BASMG' '基准数量' ''.
      BUILD_FIELDCAT 'SFCPF' '生产计划参数文件' ''.
      BUILD_FIELDCAT 'WEBAZ' '收货处理时间' ''.
      BUILD_FIELDCAT 'AUSSS' '装配件报废率' ''.
      BUILD_FIELDCAT 'MABST' '最大库存水平' ''.
      BUILD_FIELDCAT 'VRMOD' '消耗模式' ''.
      BUILD_FIELDCAT 'VINT1' '逆向消耗期间' ''.
      BUILD_FIELDCAT 'VINT2' '向前消耗期间' ''.
      BUILD_FIELDCAT 'RUEZT' '准备时间' ''.
      BUILD_FIELDCAT 'MVGR1' '物料组1' ''.
      BUILD_FIELDCAT 'MVGR2' '物料组2' ''.
      BUILD_FIELDCAT 'MVGR3' '物料组3' ''.
      BUILD_FIELDCAT 'MVGR4' '物料组4' ''.
      BUILD_FIELDCAT 'MVGR5' '物料组5' ''.
*价格权限控制
      AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
      IF SY-SUBRC = 0.
        BUILD_FIELDCAT 'VERPR' '周期价格' ''.
        BUILD_FIELDCAT 'STPRS' '标准价格' ''.
      ENDIF..

    WHEN P8.
      BUILD_FIELDCAT 'MATNR' '物料号' ''.
      BUILD_FIELDCAT 'LVORM' '工厂级DF' ''  .
      BUILD_FIELDCAT 'MTART' '物料类型' ''.
      BUILD_FIELDCAT 'WERKS' '工厂' ''.
      BUILD_FIELDCAT 'ART' '检验类型' ''.
      BUILD_FIELDCAT 'APA' '首选检验类型' ''.
      BUILD_FIELDCAT 'AKTIV' '活动' ''.

  ENDCASE.
ENDFORM.

FORM FRM_DATA.
  CASE 'X'.
    WHEN P1.
      PERFORM FRM_DATA01.
    WHEN P2.
      PERFORM FRM_DATA02.
    WHEN P3.
      PERFORM FRM_DATA03.
    WHEN P4.
      PERFORM FRM_DATA04.
    WHEN P5.
      PERFORM FRM_DATA05.
    WHEN P6.
      PERFORM FRM_DATA06.
    WHEN P7.
      PERFORM FRM_DATA07.
    WHEN P8.
      PERFORM FRM_DATA08.
  ENDCASE.


ENDFORM.

FORM FRM_OUTPUT TABLES T_OUTTAB.        "输出ALV\

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'MENU_SET'
      I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = LS_FIELDCAT[]
      IT_EVENTS                = GIT_EVENTS[]
    TABLES
      T_OUTTAB                 = T_OUTTAB
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.

FORM MENU_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD' .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_USER_COMMAND USING L_UCOMM LIKE SY-UCOMM
                            L_SELFIELD TYPE SLIS_SELFIELD.

  DATA: L_GRID TYPE REF TO CL_GUI_ALV_GRID.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.

  CALL METHOD L_GRID->CHECK_CHANGED_DATA.
  L_SELFIELD-REFRESH = 'X'.     "刷新ALV的数据
ENDFORM.

MODULE L_UCOMM..
  CASE SY-UCOMM.
    WHEN 'BACK' .
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

INCLUDE ZMM002_FORM.
