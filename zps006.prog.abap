*&---------------------------------------------------------------------*
*& Report  ZPS006
*&
*&---------------------------------------------------------------------*
*& Create by     : 吴丽娟 （hand)
*& Create date   : 2015/09/10
*& Request       : ED1K902719
*& Descriptions  : 项目基础信息表创建
*&
*& Modify by     : 徐中生  (hand)
*& Modify date   : 2015/09/14
*& Request       : ED1K902719
*& Descriptions  : 项目基础信息表修改
*&
*&---------------------------------------------------------------------*
REPORT ZPS006.

TYPE-POOLS:SLIS, ICON, ABAP.
*----------------------------------------------------------------------*
* 类型定义
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_TAB,
         PSPNR      TYPE PROJ-PSPNR,      "2-项目定义 (内部)
         PSPID      TYPE PROJ-PSPID,      "4-项目定义
         POST1      TYPE PROJ-POST1,      "6-项目名称
         OBJNR      TYPE PROJ-OBJNR,      "7-对象号
         VERNR      TYPE PROJ-VERNR,      "16-项目经理
         VBUKR      TYPE PROJ-VBUKR,      "20-项目的公司代码
         PLFAZ      TYPE PROJ-PLFAZ,      "27-实际开工日期
         PLSEZ      TYPE PROJ-PLSEZ,      "28-实际完工日期
         WERKS      TYPE PROJ-WERKS,      "29-工厂
         ZXMDZ      TYPE PROJ-ZXMDZ,      "88-项目地址
         ZFGLD      TYPE PROJ-ZFGLD,      "89-分管领导
         ZGCXZ      TYPE PROJ-ZGCXZ,      "90-工程性质
         ZLG        TYPE PROJ-ZLG,        "91-楼高
         ZJZMJ      TYPE PROJ-ZJZMJ,      "92-建筑面积
         ZXMJLCZMB  TYPE PROJ-ZXMJLCZMB,  "93-项目经理产值目标
         ZZXJL      TYPE PROJ-ZZXJL,      "94-执行经理
         ZSJS       TYPE PROJ-ZSJS,       "95-设计师
         ZJSFZR     TYPE PROJ-ZJSFZR,     "96-技术负责人
         ZCBYSZY    TYPE PROJ-ZCBYSZY,    "97-成本预算专员
         ZZLY       TYPE PROJ-ZZLY,       "98-资料员
         ZAQY       TYPE PROJ-ZAQY,       "99-安全员
         ZSGY       TYPE PROJ-ZSGY,       "100-施工员
         ZCGY       TYPE PROJ-ZCGY,       "101-仓管员
         ZCGZY      TYPE PROJ-ZCGZY,      "102-采购专员
         ZHTJE      TYPE PROJ-ZHTJE,      "103-合同金额
         ZHTGQ      TYPE PROJ-ZHTGQ,      "104-合同工期
         ZZBPHF     TYPE PROJ-ZZBPHF,     "105-总包配合费
         ZDLSYF     TYPE PROJ-ZDLSYF,     "106-吊篮使用费
         ZAQYJ      TYPE PROJ-ZAQYJ,      "107-安全押金
         ZJFMC      TYPE PROJ-ZJFMC,      "108-甲方名称
         ZJFLXFS    TYPE PROJ-ZJFLXFS,    "109-甲方联系方式
         ZSJDWMC    TYPE PROJ-ZSJDWMC,    "110-设计单位名称
         ZSJDWLXFS  TYPE PROJ-ZSJDWLXFS,  "111-设计单位联系方式
         ZMQDWMC    TYPE PROJ-ZMQDWMC,    "112-幕墙单位名称
         ZMQDWLXFS  TYPE PROJ-ZMQDWLXFS,  "113-幕墙单位联系方式
         ZZBDWMC    TYPE PROJ-ZZBDWMC,    "114-总包单位名称
         ZZBDWLXFS  TYPE PROJ-ZZBDWLXFS,  "115-总包单位联系方式
         ZJLDWMC    TYPE PROJ-ZJLDWMC,    "116-监理单位名称
         ZJLDWLXFS  TYPE PROJ-ZJLDWLXFS,  "117-监理单位联系方式
         ZJGYSRQ    TYPE PROJ-ZJGYSRQ,    "118-竣工验收日期
         ZJGJSRQ    TYPE PROJ-ZJGJSRQ,    "119-竣工决算日期
         ZLYBHJE    TYPE PROJ-ZLYBHJE,    "120-履约保函金额
         ZLYBHKSSJ  TYPE PROJ-ZLYBHKSSJ,  "121-履约保函开始日期
         ZLYBHJSSJ  TYPE PROJ-ZLYBHJSSJ,  "122-履约保函结束日期
         ZYFKBHJE   TYPE PROJ-ZYFKBHJE,   "124-预付款保函金额
         ZYFKBHKSSJ TYPE PROJ-ZYFKBHKSSJ, "125-预付款保函开始日期
         ZYFKBHJSSJ TYPE PROJ-ZYFKBHJSSJ, "126-预付款保函结束日期
         ZZBKSSJ    TYPE PROJ-ZZBKSSJ,    "128-质保开始日期
         ZZBJSSJ    TYPE PROJ-ZZBJSSJ,    "129-质保结束日期
         ZWZJKSSJ   TYPE PROJ-ZWZJKSSJ,   "131-外经证开始日期
         ZWZJJSSJ   TYPE PROJ-ZWZJJSSJ,   "133-外经证结束日期
         ZWZJSFBA   TYPE PROJ-ZWZJSFBA,   "135-外经证是否备案
         ZKJH       TYPE PROJ-ZKJH,       "136-开具银行
         ZKJYHZH    TYPE PROJ-ZKJYHZH,    "137-开具银行账号
         ZFKFS      TYPE PROJ-ZFKFS,      "138-付款方式
         ZTBBZJ     TYPE PROJ-ZTBBZJ,     "139-投标保证金
         ZLYBZJ     TYPE PROJ-ZLYBZJ,     "140-履约保证金
         ZJSJE      TYPE PROJ-ZJSJE,      "141-结算金额
         ZJSFS      TYPE PROJ-ZJSFS,      "142-结算方式
         ZSFYZBTZS  TYPE PROJ-ZSFYZBTZS,  "143-是否有中标通知书
         ZHTQDZT    TYPE PROJ-ZHTQDZT,    "144-合同签订状态
         ZLWHTQDZT  TYPE PROJ-ZLWHTQDZT,  "145-劳务合同签订状态
         ZSFYJGYSZM TYPE PROJ-ZSFYJGYSZM, "146-是否有竣工验收证明
         ZSFYJSS    TYPE PROJ-ZSFYJSS,    "147-是否有结算书
         ZQT        TYPE PROJ-ZQT,        "149-其他
         ZKHBM      TYPE PROJ-ZKHBM,      "150-客户编码
         ZJGJSJE    TYPE PROJ-ZJGJSJE,    "151-竣工决算金额
         ZDQ        TYPE PROJ-ZDQ,        "152-地区（省）
         SPROG      TYPE PROJ-SPROG,      "154-计划开工日期
         EPROG      TYPE PROJ-EPROG,      "155-计划完工日期

         VERNA      TYPE TCJ04-VERNA,     "负责人姓名（项目管理者）
         STAT       TYPE JCDS-STAT,       "项目状态
         TXT30      TYPE TJ30T-TXT30,     "项目状态文本
         XMCB       TYPE COSP-WTG001,     "项目成本
         LJKPS      TYPE COSP-WTG001,     "累计开票数
         LWRGFZC    TYPE COSP-WTG001,     "劳务人工费支出
         LJSKJE     TYPE COSP-WTG001,     "累计收款金额
         YKPWSKJE   TYPE COSP-WTG001,     "已开票未收款金额
         LJCZ       TYPE TSLVT12 ,         "cosp-wtg001,     "累计产值

         LWHT1      TYPE KONV-KWERT,      "劳务合同1
         LWHT2      TYPE KONV-KWERT,      "劳务合同2
         LWHT3      TYPE KONV-KWERT,      "劳务合同3
         LWHT4      TYPE KONV-KWERT,      "劳务合同4
         LWHT5      TYPE KONV-KWERT,      "劳务合同5
         LWHT6      TYPE KONV-KWERT,      "劳务合同6
         LWHT7      TYPE KONV-KWERT,      "劳务合同7
         LWHT8      TYPE KONV-KWERT,      "劳务合同8
         LWHT9      TYPE KONV-KWERT,      "劳务合同9
         LWHT10     TYPE KONV-KWERT,      "劳务合同10
         LWHT11     TYPE KONV-KWERT,      "劳务合同11
         LWHT12     TYPE KONV-KWERT,      "劳务合同12
         LWHT13     TYPE KONV-KWERT,      "劳务合同13
         LWHT14     TYPE KONV-KWERT,      "劳务合同14
         LWHT15     TYPE KONV-KWERT,      "劳务合同15
         LWHT16     TYPE KONV-KWERT,      "劳务合同16
         LWHT17     TYPE KONV-KWERT,      "劳务合同17
         LWHT18     TYPE KONV-KWERT,      "劳务合同18
         LWHT19     TYPE KONV-KWERT,      "劳务合同19
         LWHT20     TYPE KONV-KWERT,      "劳务合同20
         LWHT21     TYPE KONV-KWERT,      "劳务合同21
         LWHT22     TYPE KONV-KWERT,      "劳务合同22
         LWHT23     TYPE KONV-KWERT,      "劳务合同23
         LWHT24     TYPE KONV-KWERT,      "劳务合同24
         LWHT25     TYPE KONV-KWERT,      "劳务合同25
         LWHT26     TYPE KONV-KWERT,      "劳务合同26
         LWHT27     TYPE KONV-KWERT,      "劳务合同27
         LWHT28     TYPE KONV-KWERT,      "劳务合同28
         LWHT29     TYPE KONV-KWERT,      "劳务合同29
         LWHT30     TYPE KONV-KWERT,      "劳务合同30

         LWFB1      TYPE LFA1-NAME1,      "劳务分包1
         LWFB2      TYPE LFA1-NAME1,      "劳务分包2
         LWFB3      TYPE LFA1-NAME1,      "劳务分包3
         LWFB4      TYPE LFA1-NAME1,      "劳务分包4
         LWFB5      TYPE LFA1-NAME1,      "劳务分包5
         LWFB6      TYPE LFA1-NAME1,      "劳务分包6
         LWFB7      TYPE LFA1-NAME1,      "劳务分包7
         LWFB8      TYPE LFA1-NAME1,      "劳务分包8
         LWFB9      TYPE LFA1-NAME1,      "劳务分包9
         LWFB10     TYPE LFA1-NAME1,      "劳务分包10
         LWFB11     TYPE LFA1-NAME1,      "劳务分包11
         LWFB12     TYPE LFA1-NAME1,      "劳务分包12
         LWFB13     TYPE LFA1-NAME1,      "劳务分包13
         LWFB14     TYPE LFA1-NAME1,      "劳务分包14
         LWFB15     TYPE LFA1-NAME1,      "劳务分包15
         LWFB16     TYPE LFA1-NAME1,      "劳务分包16
         LWFB17     TYPE LFA1-NAME1,      "劳务分包17
         LWFB18     TYPE LFA1-NAME1,      "劳务分包18
         LWFB19     TYPE LFA1-NAME1,      "劳务分包19
         LWFB20     TYPE LFA1-NAME1,      "劳务分包20
         LWFB21     TYPE LFA1-NAME1,      "劳务分包21
         LWFB22     TYPE LFA1-NAME1,      "劳务分包22
         LWFB23     TYPE LFA1-NAME1,      "劳务分包23
         LWFB24     TYPE LFA1-NAME1,      "劳务分包24
         LWFB25     TYPE LFA1-NAME1,      "劳务分包25
         LWFB26     TYPE LFA1-NAME1,      "劳务分包26
         LWFB27     TYPE LFA1-NAME1,      "劳务分包27
         LWFB28     TYPE LFA1-NAME1,      "劳务分包28
         LWFB29     TYPE LFA1-NAME1,      "劳务分包29
         LWFB30     TYPE LFA1-NAME1,      "劳务分包30
       END OF TY_TAB,
       BEGIN OF TY_JCDS,
         OBJNR TYPE JCDS-OBJNR,           "对象号
         STAT  TYPE JCDS-STAT,            "对象状态
         UDATE TYPE JCDS-UDATE,           "创建修改文档的数据
         UTIME TYPE JCDS-UTIME,           "时间已更改
         TXT30 TYPE TJ30T-TXT30,          "对象状态
       END OF TY_JCDS,
       BEGIN OF TY_PRPS,
         PSPNR TYPE PRPS-PSPNR,          "2-WBS 要素
         POSID TYPE PRPS-POSID,          "4-工作分解结构元素 (WBS 元素)
         OBJNR TYPE PRPS-OBJNR,          "6-对象号
         PSPHI TYPE PRPS-PSPHI,          "7-合适的项目的当前编号
       END OF TY_PRPS,
       BEGIN OF TY_COSP,
         PSPEL  TYPE AUFK-PSPEL,          "工作分解结构要素 (WBS 要素)
         OBJNR  TYPE COSP-OBJNR,          "对象号
         KSTAR  TYPE COSP-KSTAR,          "成本要素
         WTG001 TYPE COSP-WTG001,         "交易货币中的总值
         WTG002 TYPE COSP-WTG002,         "交易货币中的总值
         WTG003 TYPE COSP-WTG003,         "交易货币中的总值
         WTG004 TYPE COSP-WTG004,         "交易货币中的总值
         WTG005 TYPE COSP-WTG005,         "交易货币中的总值
         WTG006 TYPE COSP-WTG006,         "交易货币中的总值
         WTG007 TYPE COSP-WTG007,         "交易货币中的总值
         WTG008 TYPE COSP-WTG008,         "交易货币中的总值
         WTG009 TYPE COSP-WTG009,         "交易货币中的总值
         WTG010 TYPE COSP-WTG010,         "交易货币中的总值
         WTG011 TYPE COSP-WTG011,         "交易货币中的总值
         WTG012 TYPE COSP-WTG012,         "交易货币中的总值
         WTG013 TYPE COSP-WTG013,         "交易货币中的总值
         WTG014 TYPE COSP-WTG014,         "交易货币中的总值
         WTG015 TYPE COSP-WTG015,         "交易货币中的总值
         WTG016 TYPE COSP-WTG016,         "交易货币中的总值
       END OF TY_COSP,
       BEGIN OF TY_ZFI005,
         BUKRS TYPE ZFI005-BUKRS,         "3-公司代码
         HSL   TYPE ZFI005-HSL,           "18-用本币计算的结转余额
         XMBH  TYPE ZFI005-XMBH,          "29-项目编号
         BUDAT TYPE ZFI005-BUDAT,         "32-凭证中的过帐日期
       END OF TY_ZFI005,
       BEGIN OF TY_MSEG,
         BWART  TYPE MSEG-BWART,          "10-移动类型（库存管理）
         MATNR  TYPE MSEG-MATNR,          "12-物料号
         WERKS  TYPE MSEG-WERKS,          "13-工厂
         MENGE  TYPE MSEG-MENGE,          "34-数量
         PS_PNR TYPE MSEG-PS_PSP_PNR,     "114-工作分解结构元素 (WBS 元素)
         PSPNR  TYPE MSEG-MAT_PSPNR,      "167-已估价的销售定单库存 WBS 元素
       END OF TY_MSEG,
       BEGIN OF TY_EKKN,
         EBELN      TYPE EKKN-EBELN,      "采购凭证号
         EBELP      TYPE EKPO-EBELP,      "采购凭证的项目编号
         ZEKKN      TYPE EKKN-ZEKKN,      "帐户分配的顺序编号
         PS_PSP_PNR TYPE EKKN-PS_PSP_PNR, "工作分解结构元素 (WBS 元素)
         LIFNR      TYPE EKKO-LIFNR,      "供应商编号
         KNUMV      TYPE EKKO-KNUMV,      "单据条件数
         LOEKZ      TYPE EKPO-LOEKZ,      "采购凭证中的删除标识
         MATNR      TYPE EKPO-MATNR,      "物料号
         KNTTP      TYPE EKPO-KNTTP,      "科目分配类别
       END OF TY_EKKN,
       TY_T_EKKN TYPE STANDARD TABLE OF TY_EKKN,
       BEGIN OF TY_KONV,
         KNUMV TYPE KONV-KNUMV,           "2-单据条件数
         KPOSN TYPE KONV-KPOSN,           "3-条件项目号
         KSCHL TYPE KONV-KSCHL,           "条件类型
         WAERS TYPE KONV-WAERS,           "12-货币码
         KKURS TYPE KONV-KKURS,           "13-换算成本位币的条件汇率
         KWERT TYPE KONV-KWERT,           "38-定价值
       END OF TY_KONV,
       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,           "供应商编号
         NAME1 TYPE LFA1-NAME1,           "供应商名称
       END OF TY_LFA1,
       BEGIN OF TY_ZFI017,
         BUKRS   TYPE ZFI017-BUKRS,       "公司代码
         GJAHR   TYPE ZFI017-GJAHR,       "会计年度
         EBELN   TYPE ZFI017-EBELN,       "采购凭证号
         ZSQFKJE TYPE ZFI017-ZSQFKJE,     "申请付款金额
         ZCLJD   TYPE ZFI017-ZCLJD,       "处理进度
         BELNR_F TYPE ZFI017-BELNR_F,     "会计凭证编号
         POSID   TYPE ZFI017-POSID,       "工作分解结构元素 (WBS 元素)
       END OF TY_ZFI017,
       BEGIN OF TY_EKPO,
         EBELN TYPE EKPO-EBELN,
       END OF TY_EKPO,
       BEGIN OF TY_MATNR,
         WERKS TYPE MSEG-WERKS,
         MATNR TYPE MSEG-MATNR,
         MENGE TYPE P LENGTH 16 DECIMALS 2,
         PRICE TYPE P LENGTH 16 DECIMALS 2,
       END OF TY_MATNR.

*----------------------------------------------------------------------*
* 工作区定义
*----------------------------------------------------------------------*
DATA: W_TAB     TYPE TY_TAB,
      W_JCDS    TYPE TY_JCDS,
      W_PRPS    TYPE TY_PRPS,
      W_COSP    TYPE TY_COSP,
      W_ZFI005  TYPE TY_ZFI005,
      W_MSEG    TYPE TY_MSEG,
      W_EKKN    TYPE TY_EKKN,
      W_KONV    TYPE TY_KONV,
      W_LFA1    TYPE TY_LFA1,
      W_TCJ04   TYPE TCJ04,
      W_ZFI017  TYPE TY_ZFI017,
*&--代码添加 BY HANDYBY 26.04.2017 20:44:25  BEGIN
      W_ZFI017T TYPE TY_ZFI017,
*&--代码添加 BY HANDYBY 26.04.2017 20:44:25  END
      W_EKPO    TYPE TY_EKPO,
      W_FCAT    TYPE LVC_S_FCAT,
      W_LAYO    TYPE LVC_S_LAYO.

*----------------------------------------------------------------------*
* 内表定义
*----------------------------------------------------------------------*
DATA: T_TAB     TYPE STANDARD TABLE OF TY_TAB,
      T_JCDS    TYPE STANDARD TABLE OF TY_JCDS,
      T_PRPS    TYPE SORTED TABLE OF TY_PRPS WITH NON-UNIQUE KEY PSPHI,
      T_COSP    TYPE SORTED TABLE OF TY_COSP WITH NON-UNIQUE KEY PSPEL OBJNR,
      T_ZFI005  TYPE STANDARD TABLE OF TY_ZFI005,
      T_MSEG    TYPE STANDARD TABLE OF TY_MSEG,
      T_EKKN    TYPE STANDARD TABLE OF TY_EKKN,
      T_KONV    TYPE STANDARD TABLE OF TY_KONV,
      T_LFA1    TYPE HASHED TABLE OF TY_LFA1 WITH UNIQUE KEY LIFNR,
      T_TCJ04   TYPE HASHED TABLE OF TCJ04 WITH UNIQUE KEY VERNR,
      T_ZFI017  TYPE SORTED TABLE OF TY_ZFI017 WITH NON-UNIQUE KEY POSID,
      T_ZFI017T TYPE SORTED TABLE OF TY_ZFI017 WITH NON-UNIQUE KEY EBELN,
      T_EKPO    TYPE SORTED TABLE OF TY_EKPO WITH NON-UNIQUE KEY EBELN,
      T_FCAT    TYPE LVC_T_FCAT.

*----------------------------------------------------------------------*
* 宏定义
*----------------------------------------------------------------------*
DEFINE INIT_FIELDCAT.
  CLEAR w_fcat.
  w_fcat-fieldname = &1.
  w_fcat-coltext = &2.
  w_fcat-ref_table = &3.
  w_fcat-ref_field = &4.
  APPEND w_fcat TO t_fcat.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* 选择屏幕
*----------------------------------------------------------------------*
TABLES:PROJ.
SELECTION-SCREEN BEGIN OF BLOCK TEXT WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_PSPID FOR PROJ-PSPID,
                S_VERNR FOR PROJ-VERNR.
SELECTION-SCREEN END OF BLOCK TEXT.
*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM FRM_FETCH_DATA.
  PERFORM FRM_PROCESS_DATA.

END-OF-SELECTION.
  PERFORM FRM_LAYOUT.
  PERFORM FRM_FIELDCAT.
  PERFORM FRM_OUTPUT.



*&---------------------------------------------------------------------*
*&      Form  FRM_FETCH_DATA
*&---------------------------------------------------------------------*
*       获取数据
*----------------------------------------------------------------------*
FORM FRM_FETCH_DATA .
  SELECT PSPNR
         PSPID            "4-项目定义
         POST1            "6-项目名称
         OBJNR            "7-对象号
         VERNR            "16-项目经理
         VBUKR            "20-项目的公司代码
         PLFAZ            "27-实际开工日期
         PLSEZ            "28-实际完工日期
         WERKS            "29-工厂
         ZXMDZ            "88-项目地址
         ZFGLD            "89-分管领导
         ZGCXZ            "90-工程性质
         ZLG              "91-楼高
         ZJZMJ            "92-建筑面积
         ZXMJLCZMB        "93-项目经理产值目标
         ZZXJL            "94-执行经理
         ZSJS             "95-设计师
         ZJSFZR           "96-技术负责人
         ZCBYSZY          "97-成本预算专员
         ZZLY             "98-资料员
         ZAQY             "99-安全员
         ZSGY             "100-施工员
         ZCGY             "101-仓管员
         ZCGZY            "102-采购专员
         ZHTJE            "103-合同金额
         ZHTGQ            "104-合同工期
         ZZBPHF           "105-总包配合费
         ZDLSYF           "106-吊篮使用费
         ZAQYJ            "107-安全押金
         ZJFMC            "108-甲方名称
         ZJFLXFS          "109-甲方联系方式
         ZSJDWMC          "110-设计单位名称
         ZSJDWLXFS        "111-设计单位联系方式
         ZMQDWMC          "112-幕墙单位名称
         ZMQDWLXFS        "113-幕墙单位联系方式
         ZZBDWMC          "114-总包单位名称
         ZZBDWLXFS        "115-总包单位联系方式
         ZJLDWMC          "116-监理单位名称
         ZJLDWLXFS        "117-监理单位联系方式
         ZJGYSRQ          "118-竣工验收日期
         ZJGJSRQ          "119-竣工决算日期
         ZLYBHJE          "120-履约保函金额
         ZLYBHKSSJ        "121-履约保函开始日期
         ZLYBHJSSJ        "122-履约保函结束日期
         ZYFKBHJE         "124-预付款保函金额
         ZYFKBHKSSJ       "125-预付款保函开始日期
         ZYFKBHJSSJ       "126-预付款保函结束日期
         ZZBKSSJ          "128-质保开始日期
         ZZBJSSJ          "129-质保结束日期
         ZWZJKSSJ         "131-外经证开始日期
         ZWZJJSSJ         "133-外经证结束日期
         ZWZJSFBA         "135-外经证是否备案
         ZKJH             "136-开具银行
         ZKJYHZH          "137-开具银行账号
         ZFKFS            "138-付款方式
         ZTBBZJ           "139-投标保证金
         ZLYBZJ           "140-履约保证金
         ZJSJE            "141-结算金额
         ZJSFS            "142-结算方式
         ZSFYZBTZS        "143-是否有中标通知书
         ZHTQDZT          "144-合同签订状态
         ZLWHTQDZT        "145-劳务合同签订状态
         ZSFYJGYSZM       "146-是否有竣工验收证明
         ZSFYJSS          "147-是否有结算书
         ZQT              "149-其他
         ZKHBM            "150-客户编码
         ZJGJSJE          "151-竣工决算金额
         ZDQ              "152-地区（省）
         SPROG            "154-计划开工日期
         EPROG            "155-计划完工日期
    INTO TABLE T_TAB
    FROM PROJ
   WHERE PSPID IN S_PSPID
     AND VERNR IN S_VERNR.

  IF T_TAB[] IS NOT INITIAL.
    SELECT JCDS~OBJNR           "对象号
           JCDS~STAT            "对象状态
           JCDS~UDATE           "创建修改文档的数据
           JCDS~UTIME           "时间已更改
           TJ30T~TXT30          "对象状态
      INTO TABLE T_JCDS
      FROM JCDS
     INNER JOIN TJ30T
        ON JCDS~STAT EQ TJ30T~ESTAT
       FOR ALL ENTRIES IN T_TAB
     WHERE JCDS~OBJNR EQ T_TAB-OBJNR
       AND JCDS~STAT LIKE 'E%'
       AND TJ30T~STSMA EQ 'ZPS00020'
       AND TJ30T~SPRAS EQ SY-LANGU.

    SELECT BUKRS          "3-公司代码
           HSL            "18-用本币计算的结转余额
           XMBH           "29-项目编号
           BUDAT          "32-凭证中的过帐日期
      INTO TABLE T_ZFI005
      FROM ZFI005
       FOR ALL ENTRIES IN T_TAB
     WHERE BUKRS EQ T_TAB-VBUKR
       AND XMBH IN S_PSPID
       AND STEP EQ ICON_GREEN_LIGHT.
    SORT T_ZFI005 BY BUKRS XMBH .

  ENDIF.

  IF T_TAB[] IS NOT INITIAL.
    SELECT PSPNR          "2-WBS 要素
           POSID          "4-工作分解结构元素 (WBS 元素)
           OBJNR          "6-对象号
           PSPHI          "7-合适的项目的当前编号
      INTO TABLE T_PRPS
      FROM PRPS
       FOR ALL ENTRIES IN T_TAB
     WHERE PSPHI EQ T_TAB-PSPNR.
  ENDIF.

  IF T_PRPS[] IS NOT INITIAL.
    SELECT AUFK~PSPEL           "工作分解结构要素 (WBS 要素)
           COSP~OBJNR           "对象号
           COSP~KSTAR           "成本要素
           COSP~WTG001          "交易货币中的总值
           COSP~WTG002          "交易货币中的总值
           COSP~WTG003          "交易货币中的总值
           COSP~WTG004          "交易货币中的总值
           COSP~WTG005          "交易货币中的总值
           COSP~WTG006          "交易货币中的总值
           COSP~WTG007          "交易货币中的总值
           COSP~WTG008          "交易货币中的总值
           COSP~WTG009          "交易货币中的总值
           COSP~WTG010          "交易货币中的总值
           COSP~WTG011          "交易货币中的总值
           COSP~WTG012          "交易货币中的总值
           COSP~WTG013          "交易货币中的总值
           COSP~WTG014          "交易货币中的总值
           COSP~WTG015          "交易货币中的总值
           COSP~WTG016          "交易货币中的总值
      INTO TABLE T_COSP
      FROM AUFK
     INNER JOIN COSP
        ON AUFK~OBJNR EQ COSP~OBJNR
       FOR ALL ENTRIES IN T_PRPS
     WHERE AUFK~PSPEL EQ T_PRPS-PSPNR
       AND AUFK~BUKRS EQ '1800'
       AND COSP~WRTTP EQ '04'
       AND ( COSP~KSTAR LIKE '5%'
        OR   COSP~KSTAR LIKE '8%'
        OR   COSP~KSTAR EQ '6603060101' ).

    SELECT COSP~OBJNR           "对象号
           COSP~KSTAR           "成本要素
           COSP~WTG001          "交易货币中的总值
           COSP~WTG002          "交易货币中的总值
           COSP~WTG003          "交易货币中的总值
           COSP~WTG004          "交易货币中的总值
           COSP~WTG005          "交易货币中的总值
           COSP~WTG006          "交易货币中的总值
           COSP~WTG007          "交易货币中的总值
           COSP~WTG008          "交易货币中的总值
           COSP~WTG009          "交易货币中的总值
           COSP~WTG010          "交易货币中的总值
           COSP~WTG011          "交易货币中的总值
           COSP~WTG012          "交易货币中的总值
           COSP~WTG013          "交易货币中的总值
           COSP~WTG014          "交易货币中的总值
           COSP~WTG015          "交易货币中的总值
           COSP~WTG016          "交易货币中的总值
 APPENDING CORRESPONDING FIELDS OF TABLE T_COSP
      FROM COSP
       FOR ALL ENTRIES IN T_PRPS
     WHERE COSP~OBJNR EQ T_PRPS-OBJNR
       AND COSP~WRTTP EQ '04'
       AND ( COSP~KSTAR LIKE '5%'
        OR   COSP~KSTAR LIKE '8%'
        OR   COSP~KSTAR EQ '6603060101' ).

    SELECT BWART                "10-移动类型（库存管理）
           MATNR                "12-物料号
           WERKS                "13-工厂
           MENGE                "34-数量
           PS_PSP_PNR AS PS_PNR "114-工作分解结构元素 (WBS 元素)
           MAT_PSPNR AS PSPNR   "167-已估价的销售定单库存 WBS 元素
      INTO TABLE T_MSEG
      FROM MSEG
       FOR ALL ENTRIES IN T_PRPS
     WHERE BWART IN ( '281', '282', '221', '222', 'Z19',
                      'Z20', 'Z21', 'Z22', 'Z23', 'Z24' )
       AND ( MAT_PSPNR EQ T_PRPS-PSPNR
        OR MAT_PSPNR EQ SPACE
       AND PS_PSP_PNR EQ T_PRPS-PSPNR ).

    SELECT EKKN~EBELN           "采购凭证号
           EKKN~EBELP           "采购凭证的项目编号
           EKKN~ZEKKN           "帐户分配的顺序编号
           EKKN~PS_PSP_PNR      "工作分解结构元素 (WBS 元素)
           EKKO~LIFNR           "供应商编号
           EKKO~KNUMV           "单据条件数
           EKPO~LOEKZ           "采购凭证中的删除标识
           EKPO~MATNR           "物料号
           EKPO~KNTTP           "科目分配类别
      INTO TABLE T_EKKN
      FROM EKKN
     INNER JOIN EKKO
        ON EKKN~EBELN EQ EKKO~EBELN
     INNER JOIN EKPO
        ON EKKO~EBELN EQ EKPO~EBELN
       FOR ALL ENTRIES IN T_PRPS
     WHERE EKKN~PS_PSP_PNR EQ T_PRPS-PSPNR.

    SELECT KNUMV            "2-单据条件数
           KPOSN            "3-条件项目号
           KSCHL
           WAERS            "12-货币码
           KKURS            "13-换算成本位币的条件汇率
           KWERT            "38-定价值
      INTO TABLE T_KONV
      FROM KONV
       FOR ALL ENTRIES IN T_EKKN
     WHERE KNUMV EQ T_EKKN-KNUMV
       AND STUNR EQ '001'.
    SORT T_KONV BY KNUMV KPOSN .
  ENDIF.

  SELECT LIFNR
         NAME1
    INTO TABLE T_LFA1
    FROM LFA1.

  SELECT *
    INTO TABLE T_TCJ04
    FROM TCJ04.
  sort T_TCJ04 by VERNR.

  SELECT BUKRS         "公司代码
         GJAHR         "会计年度
         EBELN         "采购凭证号
         ZSQFKJE       "申请付款金额
         ZCLJD         "处理进度
         BELNR_F       "会计凭证编号
         POSID         "工作分解结构元素 (WBS 元素)
    INTO TABLE T_ZFI017
    FROM ZFI017
   WHERE EBELN LIKE '42%'
     AND ZCLJD EQ '3'
     AND POSID IN S_PSPID.

  IF T_ZFI017[] IS NOT INITIAL.
    SELECT EBELN
      INTO TABLE T_EKPO
      FROM EKPO
       FOR ALL ENTRIES IN T_ZFI017
     WHERE EBELN EQ T_ZFI017-EBELN
       AND KNTTP IN ('Y', 'H', 'J').
  ENDIF.

  IF T_EKPO[] IS NOT INITIAL.
*&--代码注释 BY HANDYBY 26.04.2017 20:41:38  BEGIN
*    SELECT BUKRS         "公司代码
*           GJAHR         "会计年度
*           EBELN         "采购凭证号
*           ZSQFKJE       "申请付款金额
*           ZCLJD         "处理进度
*           BELNR_F       "会计凭证编号
*           POSID         "工作分解结构元素 (WBS 元素)
*      INTO TABLE T_ZFI017
*      FROM ZFI017
*       FOR ALL ENTRIES IN T_EKPO
*     WHERE EBELN EQ T_EKPO-EBELN
*       AND ZCLJD EQ '3'.
*&--代码注释 BY HANDYBY 26.04.2017 20:41:38  END

*&--代码添加 BY HANDYBY 26.04.2017 20:41:50  BEGIN
    SELECT BUKRS         "公司代码
         GJAHR         "会计年度
         EBELN         "采购凭证号
         ZSQFKJE       "申请付款金额
         ZCLJD         "处理进度
         BELNR_F       "会计凭证编号
         POSID         "工作分解结构元素 (WBS 元素)
    INTO TABLE T_ZFI017T
    FROM ZFI017
     FOR ALL ENTRIES IN T_EKPO
   WHERE EBELN EQ T_EKPO-EBELN
     AND ZCLJD EQ '3'.
*&--代码添加 BY HANDYBY 26.04.2017 20:41:50  END

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       处理数据
*----------------------------------------------------------------------*
FORM FRM_PROCESS_DATA .
  DATA: LV_XMCB   TYPE COSP-WTG001,
        LV_KWERT  TYPE P LENGTH 16 DECIMALS 2,

        W_MATNR   TYPE TY_MATNR,
        W_EBELN   TYPE TY_EKKN,

        T_MATNR   TYPE STANDARD TABLE OF TY_MATNR,
        T_EBELN   TYPE STANDARD TABLE OF TY_EKKN,
        T_ORDERNO TYPE STANDARD TABLE OF EKKO-EBELN.
  FIELD-SYMBOLS: <FS_TAB> TYPE TY_TAB.

*&--代码添加 BY HANDYBY 26.04.2017 18:59:50  BEGIN
*  DATA LT_COSP  TYPE SORTED TABLE OF TY_COSP WITH NON-UNIQUE KEY PSPEL OBJNR.
*  DATA LS_COSP LIKE LINE OF LT_COSP .
*&--代码添加 BY HANDYBY 26.04.2017 18:59:50  END

  SORT T_JCDS BY OBJNR ASCENDING UDATE UTIME DESCENDING.

*&--代码添加 BY HANDYBY 26.04.2017 18:52:44  BEGIN
*  SORT T_PRPS BY PSPHI .
*  SORT T_COSP BY OBJNR .
*  APPEND LINES OF T_COSP TO LT_COSP .
*  SORT LT_COSP BY PSPEL .
  SORT T_MSEG BY WERKS .
  SORT T_EKKN BY PS_PSP_PNR LOEKZ MATNR .
*&--代码添加 BY HANDYBY 26.04.2017 18:52:44  END

  LOOP AT T_TAB ASSIGNING <FS_TAB>.
    READ TABLE T_TCJ04 INTO W_TCJ04 WITH TABLE KEY VERNR = <FS_TAB>-VERNR .
    IF SY-SUBRC EQ 0.
      <FS_TAB>-VERNA = W_TCJ04-VERNA.
    ENDIF.
    "状态和状态文本
    READ TABLE T_JCDS INTO W_JCDS WITH KEY OBJNR = <FS_TAB>-OBJNR BINARY SEARCH .
    IF SY-SUBRC EQ 0.
      <FS_TAB>-STAT = W_JCDS-STAT.
      <FS_TAB>-TXT30 = W_JCDS-TXT30.
    ENDIF.

    "项目成本 & 累计开票数
*&--代码注释 BY HANDYBY 26.04.2017 19:02:00  BEGIN

*    LOOP AT T_PRPS INTO W_PRPS WHERE PSPHI EQ <FS_TAB>-PSPNR.
*      LOOP AT T_COSP INTO W_COSP WHERE OBJNR EQ W_PRPS-OBJNR
*                                    OR PSPEL EQ W_PRPS-PSPNR.
*        LV_XMCB = W_COSP-WTG001 + W_COSP-WTG002 + W_COSP-WTG003 +
*                  W_COSP-WTG004 + W_COSP-WTG005 + W_COSP-WTG006 +
*                  W_COSP-WTG007 + W_COSP-WTG008 + W_COSP-WTG009 +
*                  W_COSP-WTG010 + W_COSP-WTG011 + W_COSP-WTG012 +
*                  W_COSP-WTG013 + W_COSP-WTG014 + W_COSP-WTG015 +
*                  W_COSP-WTG016.
*        IF W_COSP-KSTAR+0(1) EQ '5' OR W_COSP-KSTAR+0(1) EQ '8'.
*          ADD LV_XMCB TO <FS_TAB>-XMCB.
*        ENDIF.
*
*        IF W_COSP-KSTAR+0(1) EQ '6'.
*          IF W_COSP-KSTAR+5(2) EQ '22' OR W_COSP-KSTAR+5(2) EQ '23'.
*            LV_XMCB = LV_XMCB * '1.17'.
*          ENDIF.
*          ADD LV_XMCB TO <FS_TAB>-LJKPS.
*        ENDIF.
*      ENDLOOP.
*      "累计产值
*      CLEAR: T_MATNR[].
*      LOOP AT T_MSEG INTO W_MSEG WHERE WERKS EQ <FS_TAB>-WERKS.
*        IF W_MSEG-PSPNR EQ W_PRPS-PSPNR OR W_PRPS-PSPNR IS INITIAL AND
*           W_MSEG-PS_PNR EQ W_PRPS-PSPNR.
*          CLEAR: W_MATNR.
*          W_MATNR-WERKS = W_MSEG-WERKS.
*          W_MATNR-MATNR = W_MSEG-MATNR.
*          W_MATNR-MENGE = W_MSEG-MENGE.
*          READ TABLE T_EKKN INTO W_EKKN WITH KEY PS_PSP_PNR = W_PRPS-PSPNR
*                                                 LOEKZ = SPACE
*                                                 MATNR = W_MSEG-MATNR.
*          IF SY-SUBRC EQ 0.
*            READ TABLE T_KONV INTO W_KONV WITH KEY KNUMV = W_EKKN-KNUMV
*                                                   KPOSN = W_EKKN-EBELP.
*            IF SY-SUBRC EQ 0.
*              IF W_KONV-WAERS EQ 'CNY'.
*                W_MATNR-PRICE = W_KONV-KWERT.
*              ELSE.
*                W_MATNR-PRICE = W_KONV-KWERT * W_KONV-KKURS.
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*          COLLECT W_MATNR INTO T_MATNR.
*        ENDIF.
*      ENDLOOP.
*      LOOP AT T_MATNR INTO W_MATNR.
*        LV_KWERT = W_MATNR-MENGE * W_MATNR-PRICE.
*        ADD LV_KWERT TO <FS_TAB>-LJCZ.
*      ENDLOOP.
*
*      "劳务合同、劳务分包
*      CLEAR: T_EBELN, T_ORDERNO.
*      LOOP AT T_EKKN INTO W_EKKN WHERE PS_PSP_PNR EQ W_PRPS-PSPNR.
*        APPEND W_EKKN TO T_EBELN.
*        READ TABLE T_ORDERNO WITH KEY TABLE_LINE = W_EKKN-EBELN
*                             TRANSPORTING NO FIELDS.
*        IF SY-SUBRC NE 0.
*          APPEND W_EKKN-EBELN TO T_ORDERNO.
*        ENDIF.
*      ENDLOOP.
*      SORT T_ORDERNO BY TABLE_LINE ASCENDING.
*
*      DO 30 TIMES.
*        PERFORM FRM_GET_LW TABLES T_EBELN T_ORDERNO
*                            USING SY-INDEX
*                           CHANGING <FS_TAB>.
*      ENDDO.
*    ENDLOOP.

*&--代码注释 BY HANDYBY 26.04.2017 19:02:00  END

*&--代码添加 BY HANDYBY 26.04.2017 19:02:33  BEGIN
    READ TABLE T_PRPS WITH KEY PSPHI = <FS_TAB>-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT T_PRPS  INTO W_PRPS FROM SY-TABIX .
        IF W_PRPS-PSPHI = <FS_TAB>-PSPNR .
          LOOP AT T_COSP INTO W_COSP WHERE OBJNR EQ W_PRPS-OBJNR
                                         OR PSPEL EQ W_PRPS-PSPNR.
            LV_XMCB = W_COSP-WTG001 + W_COSP-WTG002 + W_COSP-WTG003 +
                      W_COSP-WTG004 + W_COSP-WTG005 + W_COSP-WTG006 +
                      W_COSP-WTG007 + W_COSP-WTG008 + W_COSP-WTG009 +
                      W_COSP-WTG010 + W_COSP-WTG011 + W_COSP-WTG012 +
                      W_COSP-WTG013 + W_COSP-WTG014 + W_COSP-WTG015 +
                      W_COSP-WTG016.
            IF W_COSP-KSTAR+0(1) EQ '5' OR W_COSP-KSTAR+0(1) EQ '8'.
              ADD LV_XMCB TO <FS_TAB>-XMCB.
            ENDIF.

            IF W_COSP-KSTAR+0(1) EQ '6'.
              IF W_COSP-KSTAR+5(2) EQ '22' OR W_COSP-KSTAR+5(2) EQ '23'.
                LV_XMCB = LV_XMCB * '1.17'.
              ENDIF.
              ADD LV_XMCB TO <FS_TAB>-LJKPS.
            ENDIF.
          ENDLOOP.
          "累计产值
          CLEAR: T_MATNR[].
          READ TABLE T_MSEG WITH KEY WERKS = <FS_TAB>-WERKS BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT T_MSEG INTO W_MSEG FROM SY-TABIX .
              IF W_MSEG-WERKS = <FS_TAB>-WERKS .
                IF W_MSEG-PSPNR EQ W_PRPS-PSPNR OR W_PRPS-PSPNR IS INITIAL AND
                    W_MSEG-PS_PNR EQ W_PRPS-PSPNR.
                  CLEAR: W_MATNR.
                  W_MATNR-WERKS = W_MSEG-WERKS.
                  W_MATNR-MATNR = W_MSEG-MATNR.
                  W_MATNR-MENGE = W_MSEG-MENGE.

                  READ TABLE T_EKKN INTO W_EKKN WITH KEY PS_PSP_PNR = W_PRPS-PSPNR
                                                         LOEKZ = SPACE
                                                         MATNR = W_MSEG-MATNR BINARY SEARCH .
                  IF SY-SUBRC EQ 0.
                    READ TABLE T_KONV INTO W_KONV WITH KEY KNUMV = W_EKKN-KNUMV
                                                           KPOSN = W_EKKN-EBELP BINARY SEARCH .
                    IF SY-SUBRC EQ 0.
                      IF W_KONV-WAERS EQ 'CNY'.
                        W_MATNR-PRICE = W_KONV-KWERT.
                      ELSE.
                        W_MATNR-PRICE = W_KONV-KWERT * W_KONV-KKURS.
                      ENDIF.
                    ENDIF.

                  ENDIF.
                  COLLECT W_MATNR INTO T_MATNR.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
              CLEAR W_MSEG .
            ENDLOOP.
          ENDIF.
          LOOP AT T_MATNR INTO W_MATNR.
            LV_KWERT = W_MATNR-MENGE * W_MATNR-PRICE.
            ADD LV_KWERT TO <FS_TAB>-LJCZ.
          ENDLOOP.

          "劳务合同、劳务分包
          CLEAR: T_EBELN, T_ORDERNO.
          READ TABLE T_EKKN WITH KEY PS_PSP_PNR = W_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT T_EKKN INTO W_EKKN FROM SY-TABIX .
              IF W_EKKN-PS_PSP_PNR = W_PRPS-PSPNR .
                APPEND W_EKKN TO T_EBELN.
                READ TABLE T_ORDERNO WITH KEY TABLE_LINE = W_EKKN-EBELN
                                     TRANSPORTING NO FIELDS.
                IF SY-SUBRC NE 0.
                  APPEND W_EKKN-EBELN TO T_ORDERNO.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
              CLEAR W_EKKN .
            ENDLOOP.
          ENDIF.
          SORT T_ORDERNO BY TABLE_LINE ASCENDING.
          SORT T_EBELN BY EBELN KNTTP .

          DO 30 TIMES.
            PERFORM FRM_GET_LW TABLES T_EBELN T_ORDERNO
                                USING SY-INDEX
                               CHANGING <FS_TAB>.
          ENDDO.
        ELSE.
          EXIT .
        ENDIF.
        CLEAR W_PRPS.
      ENDLOOP.
    ENDIF.

*&--代码添加 BY HANDYBY 26.04.2017 19:02:33  END



    "劳务人工费支出
*&--代码注释 BY HANDYBY 26.04.2017 20:34:56  BEGIN
*    LOOP AT T_ZFI017 INTO W_ZFI017 WHERE POSID EQ <FS_TAB>-PSPID.
*      READ TABLE T_EKPO INTO W_EKPO WITH TABLE KEY EBELN = W_ZFI017-EBELN.
*      CHECK SY-SUBRC EQ 0.
*      LOOP AT T_ZFI017T INTO W_ZFI017 WHERE EBELN EQ W_EKPO-EBELN.
*        ADD W_ZFI017-ZSQFKJE TO <FS_TAB>-LWRGFZC.
*      ENDLOOP.
*    ENDLOOP.
*&--代码注释 BY HANDYBY 26.04.2017 20:34:56  END

*&--代码添加 BY HANDYBY 26.04.2017 20:35:06  BEGIN
    READ TABLE T_ZFI017 WITH KEY POSID = <FS_TAB>-PSPID BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT T_ZFI017 INTO W_ZFI017 FROM SY-TABIX .
        IF W_ZFI017-POSID = <FS_TAB>-PSPID .
          READ TABLE T_EKPO INTO W_EKPO WITH TABLE KEY EBELN = W_ZFI017-EBELN.
          CHECK SY-SUBRC EQ 0.
          READ TABLE T_ZFI017T WITH KEY EBELN = W_EKPO-EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT T_ZFI017T INTO W_ZFI017T FROM SY-TABIX .
              IF W_ZFI017T-EBELN = W_EKPO-EBELN .
                ADD W_ZFI017-ZSQFKJE TO <FS_TAB>-LWRGFZC.
              ELSE.
                EXIT .
              ENDIF.
              CLEAR W_ZFI017T .
            ENDLOOP.
          ENDIF.

        ELSE.
          EXIT .
        ENDIF.
        CLEAR W_ZFI017 .
      ENDLOOP.
    ENDIF.

*&--代码添加 BY HANDYBY 26.04.2017 20:35:06  END


    "累计收款金额
*&--代码注释 BY HANDYBY 26.04.2017 21:13:23  BEGIN
*     LOOP AT T_ZFI005 INTO W_ZFI005 WHERE BUKRS EQ <FS_TAB>-VBUKR
*                                     AND XMBH  EQ <FS_TAB>-PSPID.
*      ADD W_ZFI005-HSL TO <FS_TAB>-LJSKJE.
*    ENDLOOP.
*&--代码注释 BY HANDYBY 26.04.2017 21:13:23  END

*&--代码添加 BY HANDYBY 26.04.2017 21:13:33  BEGIN
    READ TABLE T_ZFI005 WITH KEY BUKRS = <FS_TAB>-VBUKR
                                 XMBH = <FS_TAB>-PSPID
                                 BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT T_ZFI005 INTO W_ZFI005 FROM SY-TABIX .
        IF W_ZFI005-BUKRS = <FS_TAB>-VBUKR AND
            W_ZFI005-XMBH = <FS_TAB>-PSPID .
          ADD W_ZFI005-HSL TO <FS_TAB>-LJSKJE.
        ELSE.
          EXIT.
        ENDIF.

        CLEAR W_ZFI005 .
      ENDLOOP.
    ENDIF.

*&--代码添加 BY HANDYBY 26.04.2017 21:13:33  END

    "已开票未收款金额
    <FS_TAB>-YKPWSKJE = <FS_TAB>-LJKPS - <FS_TAB>-LJSKJE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_LW
*&---------------------------------------------------------------------*
*       计算劳务合同和分包
*----------------------------------------------------------------------*
*      -->FT_EBELN    数据内表
*      -->FT_ORDERNO  对应采购订单编号内表
*      <--FC_LWHT     劳务合同
*      <--FC_LWFB     劳务分包
*----------------------------------------------------------------------*
FORM FRM_GET_LW TABLES FT_EBELN TYPE TY_T_EKKN
                       FT_ORDERNO
                 USING FU_INDEX
                 CHANGING FC_TAB TYPE TY_TAB.
  DATA: LV_KWERT TYPE KONV-KWERT,
        LV_CHAR  TYPE C LENGTH 2,
        LV_EBELN TYPE EKKO-EBELN,
        LV_FIELD TYPE FIELDNAME,
        LV_LWFB  TYPE FIELDNAME.
  CONSTANTS: LC_LWHT TYPE FIELDNAME VALUE 'LWHT',
             LC_LWFB TYPE FIELDNAME VALUE 'LWFB'.
  FIELD-SYMBOLS: <FS_LWHT> TYPE ANY,
                 <FS_LWFB> TYPE ANY.

  LV_CHAR = FU_INDEX.
  CONCATENATE LC_LWHT LV_CHAR INTO LV_FIELD.
  ASSIGN COMPONENT LV_FIELD OF STRUCTURE FC_TAB TO <FS_LWHT>.

  CONCATENATE LC_LWFB LV_CHAR INTO LV_FIELD.
  ASSIGN COMPONENT LV_FIELD OF STRUCTURE FC_TAB TO <FS_LWFB>.

  READ TABLE FT_ORDERNO INTO LV_EBELN INDEX FU_INDEX.
  CHECK SY-SUBRC EQ 0.

*&--代码注释 BY HANDYBY 26.04.2017 20:25:56  BEGIN
*  LOOP AT FT_EBELN WHERE EBELN = LV_EBELN
*                     AND ( KNTTP EQ 'Y' OR KNTTP EQ 'H' OR KNTTP EQ 'J' ).
*    IF <FS_LWHT> IS ASSIGNED.
*      LOOP AT T_KONV INTO W_KONV WHERE KNUMV EQ FT_EBELN-KNUMV
*                                   AND KPOSN EQ FT_EBELN-EBELP.
*        IF W_KONV-WAERS EQ 'CNY'.
*          LV_KWERT = W_KONV-KWERT.
*        ELSE.
*          LV_KWERT = W_KONV-KWERT * W_KONV-KKURS.
*        ENDIF.
*        ADD LV_KWERT TO <FS_LWHT>.
*      ENDLOOP.
*    ENDIF.
*    IF <FS_LWFB> IS ASSIGNED.
*      READ TABLE T_LFA1 INTO W_LFA1 WITH TABLE KEY LIFNR = FT_EBELN-LIFNR.
*      IF SY-SUBRC EQ 0.
*        <FS_LWFB> = W_LFA1-NAME1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*&--代码注释 BY HANDYBY 26.04.2017 20:25:56  END

*&--代码添加 BY HANDYBY 26.04.2017 20:26:09  BEGIN
  READ TABLE FT_EBELN WITH KEY EBELN = LV_EBELN BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .
    LOOP AT FT_EBELN FROM SY-TABIX .
      IF FT_EBELN-EBELN = LV_EBELN .
        IF FT_EBELN-KNTTP EQ 'Y' OR FT_EBELN-KNTTP EQ 'H' OR FT_EBELN-KNTTP EQ 'J' .
          IF <FS_LWHT> IS ASSIGNED.
            READ TABLE T_KONV WITH KEY KNUMV = FT_EBELN-KNUMV
                                       KPOSN = FT_EBELN-EBELP
                                       BINARY SEARCH TRANSPORTING NO FIELDS .
            IF SY-SUBRC = 0 .
              LOOP AT T_KONV INTO W_KONV FROM SY-TABIX .
                IF W_KONV-KNUMV = FT_EBELN-KNUMV AND
                    W_KONV-KPOSN = FT_EBELN-EBELP .
                  IF W_KONV-WAERS EQ 'CNY'.
                    LV_KWERT = W_KONV-KWERT.
                  ELSE.
                    LV_KWERT = W_KONV-KWERT * W_KONV-KKURS.
                  ENDIF.
                  ADD LV_KWERT TO <FS_LWHT>.
                ELSE.
                  EXIT.
                ENDIF.
                CLEAR W_KONV.
              ENDLOOP.
            ENDIF.
          ENDIF.
          IF <FS_LWFB> IS ASSIGNED.
            READ TABLE T_LFA1 INTO W_LFA1 WITH TABLE KEY LIFNR = FT_EBELN-LIFNR.
            IF SY-SUBRC EQ 0.
              <FS_LWFB> = W_LFA1-NAME1.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT .
      ENDIF.
    ENDLOOP.
  ENDIF.

*&--代码添加 BY HANDYBY 26.04.2017 20:26:09  END

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       设置布局
*----------------------------------------------------------------------*
FORM FRM_LAYOUT .
  W_LAYO-CWIDTH_OPT = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       设置显示字段
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT .

  INIT_FIELDCAT:
    'PSPID'      TEXT-T01 'PROJ' 'PSPID',       "项目定义
    'POST1'      TEXT-T02 'PROJ' 'POST1',       "项目名称
    'VERNA'      TEXT-T03 'TCJ04' 'VERNA',      "项目经理
    'ZDQ'        TEXT-T04 'PROJ' 'ZDQ',         "地区（省）
    'ZXMDZ'      TEXT-T05 'PROJ' 'ZXMDZ',       "项目地址
    'ZFGLD'      TEXT-T06 'PROJ' 'ZFGLD',       "分管领导
    'ZGCXZ'      TEXT-T07 'PROJ' 'ZGCXZ',       "工程性质
    'ZLG'        TEXT-T08 'PROJ' 'ZLG',         "楼高
    'ZJZMJ'      TEXT-T09 'PROJ' 'ZJZMJ',       "建筑面积
    'ZXMJLCZMB'  TEXT-T10 'PROJ' 'ZXMJLCZMB',   "项目经理产值目标
    'ZZXJL'      TEXT-T11  'PROJ' 'ZZXJL',      "执行经理
    'ZSJS'       TEXT-T12 'PROJ' 'ZSJS',        "设计师
    'ZJSFZR'     TEXT-T13 'PROJ' 'ZJSFZR',      "技术负责人
    'ZCBYSZY'    TEXT-T14 'PROJ' 'ZCBYSZY',     "成本预算专员
    'ZZLY'       TEXT-T15 'PROJ' 'ZZLY',        "资料员
    'ZAQY'       TEXT-T16 'PROJ' 'ZAQY',        "安全员
    'ZSGY'       TEXT-T17 'PROJ' 'ZSGY',        "施工员
    'ZCGY'       TEXT-T18 'PROJ' 'ZCGY',        "仓管员
    'ZCGZY'      TEXT-T19 'PROJ' 'ZCGZY',       "采购专员
    'ZHTJE'      TEXT-T20 'PROJ' 'ZHTJE',       "合同金额
    'ZHTGQ'      TEXT-T21 'PROJ' 'ZHTGQ',       "合同工期
    'ZZBPHF'     TEXT-T22 'PROJ' 'ZZBPHF',      "总包配合费
    'ZDLSYF'     TEXT-T23 'PROJ' 'ZDLSYF',      "吊篮使用费
    'ZAQYJ'      TEXT-T24 'PROJ' 'ZAQYJ',       "安全押金
    'ZJFMC'      TEXT-T25 'PROJ' 'ZJFMC',       "甲方名称
    'ZJFLXFS'    TEXT-T26 'PROJ' 'ZJFLXFS',     "甲方联系方式
    'ZSJDWMC'    TEXT-T27 'PROJ' 'ZSJDWMC',     "设计单位名称
    'ZSJDWLXFS'  TEXT-T28 'PROJ' 'ZSJDWLXFS',   "设计单位联系方式
    'ZMQDWMC'    TEXT-T29 'PROJ' 'ZMQDWMC',     "幕墙单位名称
    'ZMQDWLXFS'  TEXT-T30 'PROJ' 'ZMQDWLXFS',   "幕墙单位联系方式
    'ZZBDWMC'    TEXT-T31 'PROJ' 'ZZBDWMC',     "总包单位名称
    'ZZBDWLXFS'  TEXT-T32 'PROJ' 'ZZBDWLXFS',   "总包单位联系方式
    'ZJLDWMC'    TEXT-T33 'PROJ' 'ZJLDWMC',     "监理单位名称
    'ZJLDWLXFS'  TEXT-T34 'PROJ' 'ZJLDWLXFS',   "监理单位联系方式
    'ZJGYSRQ'    TEXT-T35 'PROJ' 'ZJGYSRQ',     "竣工验收日期
    'ZJGJSRQ'    TEXT-T36 'PROJ' 'ZJGJSRQ',     "竣工决算日期
    'ZLYBHJE'    TEXT-T37 'PROJ' 'ZLYBHJE',     "履约保函金额
    'ZLYBHKSSJ'  TEXT-T38 'PROJ' 'ZLYBHKSSJ',   "履约保函开始日期
    'ZLYBHJSSJ'  TEXT-T39 'PROJ' 'ZLYBHJSSJ',   "履约保函结束日期
    'ZYFKBHJE'   TEXT-T40 'PROJ' 'ZYFKBHJE',    "预付款保函金额
    'ZYFKBHKSSJ' TEXT-T41 'PROJ' 'ZYFKBHKSSJ',  "预付款保函开始日期
    'ZYFKBHJSSJ' TEXT-T42 'PROJ' 'ZYFKBHJSSJ',  "预付款保函结束日期
    'ZZBKSSJ'    TEXT-T43 'PROJ' 'ZZBKSSJ',     "质保开始日期
    'ZZBJSSJ'    TEXT-T44 'PROJ' 'ZZBJSSJ',     "质保结束日期
    'ZWZJKSSJ'   TEXT-T45 'PROJ' 'ZWZJKSSJ',    "外经证开始日期
    'ZWZJJSSJ'   TEXT-T46 'PROJ' 'ZWZJJSSJ',    "外经证结束日期
    'ZWZJSFBA'   TEXT-T47 'PROJ' 'ZWZJSFBA',    "外经证是否备案
    'ZKJH'       TEXT-T48 'PROJ' 'ZKJH',        "开具银行
    'ZKJYHZH'    TEXT-T49 'PROJ' 'ZKJYHZH',     "开具银行账号
    'ZFKFS'      TEXT-T50 'PROJ' 'ZFKFS',       "付款方式
    'ZTBBZJ'     TEXT-T51 'PROJ' 'ZTBBZJ',      "投标保证金
    'ZLYBZJ'     TEXT-T52 'PROJ' 'ZLYBZJ',      "履约保证金
    'ZJSJE'      TEXT-T53 'PROJ' 'ZJSJE',       "结算金额
    'ZJSFS'      TEXT-T54 'PROJ' 'ZJSFS',       "结算方式
    'ZSFYZBTZS'  TEXT-T55 'PROJ' 'ZSFYZBTZS',   "是否有中标通知书
    'ZHTQDZT'    TEXT-T56 'PROJ' 'ZHTQDZT',     "合同签订状态
    'ZLWHTQDZT'  TEXT-T57 'PROJ' 'ZLWHTQDZT',   "劳务合同签订状态
    'ZSFYJGYSZM' TEXT-T58 'PROJ' 'ZSFYJGYSZM',  "是否有竣工验收证明
    'ZSFYJSS'    TEXT-T59 'PROJ' 'ZSFYJSS',     "是否有结算书
    'ZQT'        TEXT-T60 'PROJ' 'ZQT',         "其他
    'ZKHBM'      TEXT-T61 'PROJ' 'ZKHBM',       "客户编码
    'TXT30'      TEXT-T62 'TJ30T' 'TXT30',      "项目状态
    'SPROG'      TEXT-T63 'PROJ' 'SPROG',       "计划开工日期
    'EPROG'      TEXT-T64 'PROJ' 'EPROG',       "计划完工日期
    'PLFAZ'      TEXT-T65 'PROJ' 'PLFAZ',       "实际开工日期
    'PLSEZ'      TEXT-T66 'PROJ' 'PLSEZ',       "实际完工日期
    'XMCB'       TEXT-T67 'COSP' 'WTG001',      "项目成本
    'LJKPS'      TEXT-T68 'COSP' 'WTG001',      "累计开票数
    'LWRGFZC'    TEXT-T69 'COSP' 'WTG001',      "劳务人工费支出
    'LJSKJE'     TEXT-T70 'COSP' 'WTG001',      "累计收款金额
    'YKPWSKJE'   TEXT-T71 'COSP' 'WTG001',      "已开票未收款金额
    'ZJGJSJE'    TEXT-T72 'PROJ' 'ZJGJSJE',     "竣工决算金额
    'LJCZ'       TEXT-T73 'COSP' 'WTG001',      "累计产值
    'LWHT1'      TEXT-T74 'KONV' 'KWERT',       "劳务合同1
    'LWHT2'      TEXT-T75 'KONV' 'KWERT',       "劳务合同2
    'LWHT3'      TEXT-T76 'KONV' 'KWERT',       "劳务合同3
    'LWHT4'      TEXT-T77 'KONV' 'KWERT',       "劳务合同4
    'LWHT5'      TEXT-T78 'KONV' 'KWERT',       "劳务合同5
    'LWHT6'      TEXT-T79 'KONV' 'KWERT',       "劳务合同6
    'LWHT7'      TEXT-T80 'KONV' 'KWERT',       "劳务合同7
    'LWHT8'      TEXT-T81 'KONV' 'KWERT',       "劳务合同8
    'LWHT9'      TEXT-T82 'KONV' 'KWERT',       "劳务合同9
    'LWHT10'     TEXT-T83 'KONV' 'KWERT',       "劳务合同10
    'LWHT11'     TEXT-T84 'KONV' 'KWERT',       "劳务合同11
    'LWHT12'     TEXT-T85 'KONV' 'KWERT',       "劳务合同12
    'LWHT13'     TEXT-T86 'KONV' 'KWERT',       "劳务合同13
    'LWHT14'     TEXT-T87 'KONV' 'KWERT',       "劳务合同14
    'LWHT15'     TEXT-T88 'KONV' 'KWERT',       "劳务合同15
    'LWHT16'     TEXT-T89 'KONV' 'KWERT',       "劳务合同16
    'LWHT17'     TEXT-T90 'KONV' 'KWERT',       "劳务合同17
    'LWHT18'     TEXT-T91 'KONV' 'KWERT',       "劳务合同18
    'LWHT19'     TEXT-T92 'KONV' 'KWERT',       "劳务合同19
    'LWHT20'     TEXT-T93 'KONV' 'KWERT',       "劳务合同20
    'LWHT21'     TEXT-T94 'KONV' 'KWERT',       "劳务合同21
    'LWHT22'     TEXT-T95 'KONV' 'KWERT',       "劳务合同22
    'LWHT23'     TEXT-T96 'KONV' 'KWERT',       "劳务合同23
    'LWHT24'     TEXT-T97 'KONV' 'KWERT',       "劳务合同24
    'LWHT25'     TEXT-T98 'KONV' 'KWERT',       "劳务合同25
    'LWHT26'     TEXT-T99 'KONV' 'KWERT',       "劳务合同26
    'LWHT27'     TEXT-100 'KONV' 'KWERT',       "劳务合同27
    'LWHT28'     TEXT-101 'KONV' 'KWERT',       "劳务合同28
    'LWHT29'     TEXT-102 'KONV' 'KWERT',       "劳务合同29
    'LWHT30'     TEXT-103 'KONV' 'KWERT',       "劳务合同30
    'LWFB1'      TEXT-104 'LFA1' 'NAME1',       "劳务分包1
    'LWFB2'      TEXT-105 'LFA1' 'NAME1',       "劳务分包2
    'LWFB3'      TEXT-105 'LFA1' 'NAME1',       "劳务分包3
    'LWFB4'      TEXT-106 'LFA1' 'NAME1',       "劳务分包4
    'LWFB5'      TEXT-107 'LFA1' 'NAME1',       "劳务分包5
    'LWFB6'      TEXT-108 'LFA1' 'NAME1',       "劳务分包6
    'LWFB7'      TEXT-109 'LFA1' 'NAME1',       "劳务分包7
    'LWFB8'      TEXT-110 'LFA1' 'NAME1',       "劳务分包8
    'LWFB9'      TEXT-111 'LFA1' 'NAME1',       "劳务分包9
    'LWFB10'     TEXT-112 'LFA1' 'NAME1',       "劳务分包10
    'LWFB11'     TEXT-113 'LFA1' 'NAME1',       "劳务分包11
    'LWFB12'     TEXT-114 'LFA1' 'NAME1',       "劳务分包12
    'LWFB13'     TEXT-116 'LFA1' 'NAME1',       "劳务分包13
    'LWFB14'     TEXT-117 'LFA1' 'NAME1',       "劳务分包14
    'LWFB15'     TEXT-118 'LFA1' 'NAME1',       "劳务分包15
    'LWFB16'     TEXT-119 'LFA1' 'NAME1',       "劳务分包16
    'LWFB17'     TEXT-120 'LFA1' 'NAME1',       "劳务分包17
    'LWFB18'     TEXT-121 'LFA1' 'NAME1',       "劳务分包18
    'LWFB19'     TEXT-122 'LFA1' 'NAME1',       "劳务分包19
    'LWFB20'     TEXT-123 'LFA1' 'NAME1',       "劳务分包20
    'LWFB21'     TEXT-124 'LFA1' 'NAME1',       "劳务分包21
    'LWFB22'     TEXT-125 'LFA1' 'NAME1',       "劳务分包22
    'LWFB23'     TEXT-126 'LFA1' 'NAME1',       "劳务分包23
    'LWFB24'     TEXT-127 'LFA1' 'NAME1',       "劳务分包24
    'LWFB25'     TEXT-128 'LFA1' 'NAME1',       "劳务分包25
    'LWFB26'     TEXT-129 'LFA1' 'NAME1',       "劳务分包26
    'LWFB27'     TEXT-130 'LFA1' 'NAME1',       "劳务分包27
    'LWFB28'     TEXT-131 'LFA1' 'NAME1',       "劳务分包28
    'LWFB29'     TEXT-132 'LFA1' 'NAME1',       "劳务分包29
    'LWFB30'     TEXT-133 'LFA1' 'NAME1'.       "劳务分包30

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT_LVC      = W_LAYO
      IT_FIELDCAT_LVC    = T_FCAT
      I_SAVE             = 'A'
    TABLES
      T_OUTTAB           = T_TAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
