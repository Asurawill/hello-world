*&---------------------------------------------------------------------*
*& 程序名称:ZMM001
*& 作者    :汪昱
*& 开发日期:
*& 请求号  :
*& 描述    :物料主数据导入程序
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2016-07-15 it02                              工厂2100 价格控制默认为：V
*& 2016-10-09 it02                              增加检验类型Z3的默认值
*& 2016-12-30  it02                ED1K905147   增加 “是否需要上传受控文件”特征值
*&---------------------------------------------------------------------*

REPORT ZDM001.

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

*基本视图
DATA: BEGIN OF I_DATA1 OCCURS 0,
        SHENQINGREN(30) TYPE C, "申请人
        BISMT(18)       TYPE C, "旧物料号
        MATNR(18)       TYPE C, "物料号MARA-MATNR
        MAKTX(40)       TYPE C, "物料描述
        EMAKTX(40)      TYPE C, "英文物料描述
        MBRSH(1)        TYPE C, "行业领域
        MTART(4)        TYPE C, "物类型MARA-MTART
        MATKL(9)        TYPE C, "物料组MARA-MATKL
        MEINS(3)        TYPE C, "基本计量单位MARA-MEINS



        MHDRZ(4)        TYPE C, "最短剩余货架寿命 MARA-MHDRZ
        MHDHB(4)        TYPE C, "总货架寿命  MARA-MHDHB
        IPRKZ(1)        TYPE C, "SLED的期间标识
        BSTME(3)        TYPE C, "采购订单的计量单位
        TEXT2(250)      TYPE C, "采购订单文本
        SPART(2)        TYPE C, "产品组 MARA-SPART
        LAENG(17)       TYPE C, "长
        BREIT(17)       TYPE C, "宽
        HOEHE(17)       TYPE C, "高
        MEABM(3)        TYPE C, "长宽高单位
        GGXH(30)        TYPE C, "规格型号
        XSJJ(30)        TYPE C, "相素间距
        ROHS(30)        TYPE C, "ROHS认证
        SFYGD(30)       TYPE C, "是否亿光灯
        CHANGJIA(30)    TYPE C, "产家
        DHH1(30)        TYPE C, "订货单
        TZZ(30)         TYPE C, "特征值
        CPLX(30)        TYPE C, "产品类型

        SKWJ(30)        TYPE C, "是否需要上传受控文件

*&--代码添加 BY HANDYBY 22.05.2017 08:19:24  BEGIN
        EXTWG(18)       TYPE C, "外部物料组
*&--代码添加 BY HANDYBY 22.05.2017 08:19:24  END

        TYPE(10)        TYPE C,
        MESSAGE(300)    TYPE C,
      END OF I_DATA1.

*工厂数据
DATA: BEGIN OF I_DATA2 OCCURS 0,
        MATNR(18)    TYPE C, "物料号MARA-MATNR
        WERKS(4)     TYPE C, "MARC-WERKS
        EKGRP(3)     TYPE C, " MARC-EKGRP 采购组
        KORDB        TYPE C, " MARC-KORDB 源清单
        BSTMI(13)    TYPE C, "最小批量大小
        BSTRF(13)    TYPE C, "舍入值
        LGFSB(4)     TYPE C, " MARC-LGFSB 外部采购仓储地点
        PLIFZ(3)     TYPE C, " MARC-PLIFZ 计划交货时间(天数)
        DISMM(2)     TYPE C, " MARC-DISMM, MRP 类型
        MINBE(13)    TYPE C, "再订货点
        MABST(1)     TYPE C, "最大库存水平
        DISPO(3)     TYPE C, " MARC-DISPO MRP 控制者
        DISLS(17)    TYPE C, "MARC-BSTMI 最小批量大小(BTCI)
        BESKZ        TYPE C, "采购类型
        SOBSL(2)     TYPE C, " MARC-SOBSL 特殊采购类型
        LGPRO(4)     TYPE C , " MARC-LGPRO 生产仓储地点
        RGEKZ        TYPE C, " MARC-RGEKZ 标识：反冲
        DZEIT(3)     TYPE C, "自制生产时间
        EISBE(17)    TYPE C, " MARC-EISBE 安全库存
        FHORI(3)     TYPE C , " MARC-FHORI 计划边际码
        STRGR(2)     TYPE C , "MARC-STRGR 策略组
        VRMOD        TYPE C, " MARC-VRMOD 消耗模式
        VINT2(3)     TYPE C, " MARC-VINT2 向前消耗期间
        VINT1(3)     TYPE C, " MARC-VINT1 逆向消耗期间
        MTVFP(2)     TYPE C, " MARC-MTVFP 可用性检查
        SBDKZ        TYPE C, "独立/集中
        FEVOR(3)     TYPE C, " MARC-FEVOR 生产调度员
        KZAUS        TYPE C, "MARC-KZAUS 非连续标识
        AUSDT(9)     TYPE C, " MARC-AUSDT 生效期
        NFMAT(18)    TYPE C , "MARC-NFMAT 后继的物料
        RUEZT(5)     TYPE C,  "准备时间
        BEARZ(8)     TYPE C, " MARC-BEARZ 处理时间(BTCI)
        BASMG(17)    TYPE C, " MARC-BASMG 基准数量(BTCI)
        XCHPF(1)     TYPE C, "批次管理需求
        ART(8)       TYPE C, "QMAT-ART 检验类型
        APA          TYPE C, " QMAT-APA  首选检验类型
        AKTIV        TYPE C, " QMAT-AKTIV 活动
        BKLAS(4)     TYPE C, "评估类
        VPRSV        TYPE C, "MBEW-VPRSV 价格控制指示符
        MLAST        TYPE C, " MBEW-MLAST 价格确定
        PEINH(5)     TYPE C, " MBEW-PEINH 价格单位
        EKALR        TYPE C, "用QS的成本核算
        HKMAT        TYPE C, " MBEW-HKMAT 物料相关的源
        MMSTA(2)     TYPE C, "MARC-MMSTA 工厂特定的物料状态
        AWSLS(6)     TYPE C, "差异码
        LOSGR(17)    TYPE C, "批量产品成本核算 MARC-LOSGR
        ZPLP1(14)    TYPE C, "MBEW-ZPLP1计划价格 1
        ZPLD1(8)     TYPE C, "MBEW-ZPLD1计划价格日期1

*&--代码添加 BY HANDYBY 22.05.2017 08:45:10  BEGIN
        ZPLP2(14)    TYPE C, "MBEW-ZPLP2计划价格 2
        ZPLD2(8)     TYPE C, "MBEW-ZPLD2计划价格日期2
*&--代码添加 BY HANDYBY 22.05.2017 08:45:10  END

        TYPE(10)     TYPE C,
        MESSAGE(400) TYPE C,
      END OF I_DATA2.

*销售数据
DATA: BEGIN OF I_DATA3 OCCURS 0,
        MATNR(18)    TYPE C, "物料号MARA-MATNR
        VKORG(4)     TYPE C, "销售组织 MVKE-VKORG
        VTWEG(2)     TYPE C, "分销渠道 MVKE-VTWEG
        DWERK(4)     TYPE C, "交货工厂 (自有或外部) MVKE-DWERK
        TAXM1        TYPE C, "物料的税分类  MLAN-TAXM1
        KTGRM(2)     TYPE C, "该物料的科目设置组 MVKE-KTGRM
        MTPOS(4)     TYPE C, "来自物料主文件的项目类别组 MVKE-MTPOS
        MVGR1(3)     TYPE C, "物料组1 MVKE-MVGR1
        MVGR2(3)     TYPE C, "物料组2 MVKE-MVGR2
        MVGR3(3)     TYPE C, "物料组3 MVKE-MVGR3
        MVGR4(3)     TYPE C, "物料组4 MVKE-MVGR4
        MVGR5(3)     TYPE C, "物料组5 MVKE-MVGR5
        TRAGR(4)     TYPE C, "运输组 MARA-TRAGR
        LADGR(4)     TYPE C, "装载组 MARC-LADGR
        MTVFP(2)     TYPE C, "可用性检查组
        TYPE(10)     TYPE C,
        MESSAGE(300) TYPE C,
      END OF I_DATA3.

DATA T_DATA1 LIKE I_DATA1 OCCURS 0 WITH HEADER LINE. "用于ALV显示
DATA T_DATA2 LIKE I_DATA2 OCCURS 0 WITH HEADER LINE. "用于ALV显示
DATA T_DATA3 LIKE I_DATA3 OCCURS 0 WITH HEADER LINE. "用于ALV显示

DATA: TZTCDM003 LIKE TABLE OF ZTCDM003 WITH HEADER LINE,
      TZTCDM004 LIKE TABLE OF ZTCDM004 WITH HEADER LINE.
FIELD-SYMBOLS: <F>,<F2>.

*{ INSERT QASK901809 5

DATA TNAME(70) TYPE C.
DATA TMESSAGE(40) TYPE C.
DATA TTYPE(40) TYPE C.
DATA:BEGIN OF IT_T001L OCCURS 0,"扩展库存地点
       WERKS LIKE T001L-WERKS,
       LGORT LIKE T001L-LGORT,
     END OF IT_T001L.

DATA:WA_LGORT LIKE LINE OF IT_T001L.

DATA:
  AUSSS TYPE AUSSS,
  KAUSF TYPE KAUSF,
  VERPR TYPE VERPR,
  STPRS TYPE STPRS,
  LOSGR TYPE LOSGR,
  ZPLP1 TYPE DZPLP1,
  ZPLP2 TYPE DZPLP2,
  LAENG TYPE LAENG,
  BREIT TYPE BREIT,
  HOEHE TYPE HOEHE.

DATA SK(6) TYPE C .
DATA: TDLINE LIKE TLINE OCCURS 0 WITH HEADER LINE.
DATA  HEADER LIKE THEAD."SAVE_TEXT函数
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
      IRETURN              TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      INSPECTIONCTRL       TYPE TABLE OF BAPI1001004_QMAT WITH HEADER LINE,
      MATERIALDESCRIPTION  TYPE TABLE OF BAPI_MAKT WITH HEADER LINE,
      TAXCLASSIFICATIONS   TYPE TABLE OF BAPI_MLAN WITH HEADER LINE,
      MATERIALLONGTEXT     TYPE TABLE OF BAPI_MLTX WITH HEADER LINE,
      RETURNMESSAGES       TYPE TABLE OF BAPI_MATRETURN2,
      UNITSOFMEASURE       TYPE TABLE OF BAPI_MARM WITH HEADER LINE,
      UNITSOFMEASUREX      TYPE TABLE OF BAPI_MARMX WITH HEADER LINE,
      EXTENSIONIN          TYPE TABLE OF BAPIPAREX WITH HEADER LINE,
      EXTENSIONINX         TYPE TABLE OF BAPIPAREXX WITH HEADER LINE.
DATA: G_TE_MARA      TYPE TABLE OF BAPI_TE_MARA WITH HEADER LINE,
      G_TE_MARAX     TYPE TABLE OF BAPI_TE_MARAX WITH HEADER LINE,
      G_TE_MARC      TYPE TABLE OF BAPI_TE_MARC WITH HEADER LINE,
      G_TE_MARCX     TYPE TABLE OF BAPI_TE_MARCX WITH HEADER LINE,
      PARTVALUE(960).

*分类视图
DATA:
  IT_NUM  LIKE BAPI1003_ALLOC_VALUES_NUM  OCCURS 0 WITH HEADER LINE,
  IT_CHAR LIKE BAPI1003_ALLOC_VALUES_CHAR OCCURS 0 WITH HEADER LINE,
  IT_CURR LIKE BAPI1003_ALLOC_VALUES_CURR OCCURS 0 WITH HEADER LINE,
  IT_RET  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF LOG_RETURN OCCURS 0,
        MATNR   LIKE MARA-MATNR,
        MESSAGE TYPE BAPI_MSG,
      END OF LOG_RETURN.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS: MATERIAL LIKE RLGRAP-FILENAME OBLIGATORY MEMORY ID ZMM001. "物料导入摸版
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P1  RADIOBUTTON GROUP G1 ."基本视图
PARAMETERS: P2  RADIOBUTTON GROUP G1 ."工厂视图


SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
PARAMETERS: B AS CHECKBOX MODIF ID 2 DEFAULT 'X'. "采购视图
PARAMETERS: C AS CHECKBOX MODIF ID 2 DEFAULT 'X'. "MRP视图
PARAMETERS: D AS CHECKBOX MODIF ID 2 DEFAULT 'X'. "质量管理视图
PARAMETERS: E AS CHECKBOX MODIF ID 2 DEFAULT 'X'. "工厂存储视图
PARAMETERS: F AS CHECKBOX MODIF ID 2 DEFAULT 'X'. "会计成本视图
SELECTION-SCREEN END OF BLOCK BLK3.

PARAMETERS: P3 RADIOBUTTON GROUP G1 ."销售组织视图

SELECTION-SCREEN END OF BLOCK BLK2.

DATA SEC TYPE F VALUE '0'.
*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
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

    PERFORM BUILD_FIELDCATALOG.

    PERFORM BUILD_LAYOUT.

*基本数据
    IF P1 = 'X'.
      PERFORM DISPLAY_ALV_REPORT TABLES T_DATA1.
*工厂数据
    ELSEIF P2 = 'X'.
      PERFORM DISPLAY_ALV_REPORT TABLES T_DATA2.
    ELSE.
*销售数据
      PERFORM DISPLAY_ALV_REPORT TABLES T_DATA3.
    ENDIF.

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
*基本数据
  IF P1 = 'X'.
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  BEGIN
    PERFORM FRM_CHECK_AUTH USING '01' .
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  END
    PERFORM FRM_XLS_TO_SAP TABLES I_DATA1.
*&--代码添加 BY HANDYBY 07.07.2017 10:44:47  BEGIN
    PERFORM FRM_CHECK_EXIST TABLES I_DATA1 .
*&--代码添加 BY HANDYBY 07.07.2017 10:44:47  END
    PERFORM FRM_FILL_DATA1.
*工厂数据
  ELSEIF P2 = 'X'.
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  BEGIN
    PERFORM FRM_CHECK_AUTH USING '02' .
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  END
    PERFORM FRM_XLS_TO_SAP TABLES I_DATA2.
    PERFORM FRM_FILL_DATA2.
*销售数据
  ELSEIF P3 = 'X'.
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  BEGIN
    PERFORM FRM_CHECK_AUTH USING '03' .
*&--代码添加 BY HANDYBY 10.07.2017 14:43:12  END
    PERFORM FRM_XLS_TO_SAP TABLES I_DATA3.
    PERFORM FRM_FILL_DATA3.
  ENDIF.

ENDFORM. " UPLOAD_MATERIAL_DATA

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
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MATERIAL  =    V_MATERIAL.

ENDFORM. " SELECT_PATH

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*

FORM BUILD_FIELDCATALOG.
  IF P1 = 'X'.
    PERFORM FIELD_CAT USING :
      'TYPE'    '消息类型' '',
      'MESSAGE' '消息内容' '',
      'SHENQINGREN'    '申请人' '',
      'BISMT'   '旧物料号' '',
      'MATNR'   '物料号'   '',
      'MAKTX'   '物料描述'  '',
      'EMAKTX'  '英文物料描述'  '',
      'MBRSH'   '行业领域'  '',
      'MTART'   '物类型'  '',
      'MATKL'   '物料组'  '',
      'MEINS'   '基本计量单位'  '',
      'MHDRZ'   '最短剩余货架寿命'  '',
      'MHDHB'   '总货架寿命'  '',
      'IPRKZ'   '货架寿命到期日的期间标识'  '',
      'BSTME'   '采购订单的计量单位'   '',
      'TEXT2'   '采购订单文本'  '',
      'SPART'   '产品组'   '',
      'TRAGR'   '运输组'  '',
      'LAENG'   '长' '',
      'BREIT'   '宽' '',
      'HOEHE'   '高' '',
      'MEABM'   '长宽高单位' '',
      'GGXH'    '规格型号' '',
      'XSJJ'    '相素间距' '',
      'ROHS'     'ROHS认证' '',
      'SFYGD'    '是否亿光灯' '',
      'CHANGJIA' '产家' '',
      'DHH1'     '订货单' '',
      'TZZ'      '特征值' '',
      'CPLX'     '产品类型' '',
*&--代码添加 BY HANDYBY 26.05.2017 12:17:20  BEGIN
      'EXTWG'   '品牌' '' ,
*&--代码添加 BY HANDYBY 26.05.2017 12:17:20  END
      'SKWJ'     '是否需要上传受控文件'  ''.

  ELSEIF P2 = 'X'.
    PERFORM FIELD_CAT USING :
        'TYPE'    '消息类型' '',
        'MESSAGE' '消息内容' '',
        'MATNR' '物料号'   '',
        'WERKS' '工厂'   '',
        'ART' '检验类型'   '',
        'APA' '首选检验类型'   '',
        'AKTIV' '活动'   '',
        'WEBAZ' '用天数表示的收货处理时间'   '',
        'EKGRP' '采购组'   '',
        'XCHPF' '标识：批次管理需求'   '',
        'KORDB' '源清单'   '',
        'DISLS' '批量大小'   '',
        'BSTMI' '最小批量大小(BTCI)'   '',
        'MAABC' 'ABC标识'   '',
        'SOBSL' '特殊采购类型'   '',
        'LGFSB' '外部采购仓储地点'   '',
        'PLIFZ' '计划交货时间(天数)'   '',
        'EISBE' '安全库存'   '',
        'LOGGR' '后勤处理组'   '',
        'DISGR' 'MRP组'   '',
        'DISMM' 'MRP类型'   '',
        'DISPO' 'MRP控制者'   '',
        'AUSSS' '装配件报废率'   '',
        'BESKZ'  '采购类型'   '',
        'LGPRO' '生产仓储地点'   '',
        'FHORI' '计划边际码'   '',
        'RGEKZ' '标识：反冲'   '',
        'SCHGT' '标识：散装物料'   '',
        'STRGR' '策略组'   '',
        'VRMOD'  '消耗模式'   '',
        'VINT1' '逆向消耗期间'   '',
        'VINT2' '向前消耗期间'   '',
        'MISKZ' '综合MRP'   '',
        'MTVFP' '可用性检查'   '',
        'SBDKZ' '独立/集中'   '',
        'KZAUS' '非连续标识'   '',
        'AUSDT' '生效期'   '',
        'RUEZT' '准备时间' '',
        'NFMAT' '后继的物料'   '',
        'KAUSF' '部件废品(%)'   '',
        'FEVOR' '生产调度员'   '',
        'SFCPF' '生产计划参数文件'   '',
        'BEARZ' '处理时间(BTCI)'   '',
        'BASMG' '基准数量(BTCI)'   '',
        'BKLAS' '评估类'   '',
        'MLAST'  '价格确定'   '',
        'VPRSV' '价格控制指示符'   '',
        'VERPR' '移动平均价格(BTCI)'   '',
        'STPRS' '标准价格'   '',
        'PEINH' '价格单位'   '',
        'NCOST' '无成本核算'   '',
        'EKALR' '用QS的成本估算'   '',
        'HKMAT' '物料相关的源'   '',
        'MMSTA' '工厂特定的物料状态'   '',
        'AWSLS' '差异码'   '',
        'LOSGR' '批量产品成本核算'   '',
        'HRKFT' '原始组'   '',
        'ZPLP1' '计划价格1'   '',
        'ZPLD1' '计划价格日期1'   '',
*&--代码添加 BY HANDYBY 26.05.2017 12:18:31  BEGIN
        'ZPLP2' '计划价格1'   '',
        'ZPLD2' '计划价格日期1'   ''.
*&--代码添加 BY HANDYBY 26.05.2017 12:18:31  END

  ELSE.
    PERFORM FIELD_CAT USING :
        'TYPE' '消息类型' '',
        'MESSAGE' '消息内容' '',
        'MATNR' '物料号'   '',
        'VKORG' '销售组织'   '',
        'VTWEG' '分销渠道'     '',
        'DWERK' '交货工厂(自有或外部)'   '',
        'TAXM1' '物料的税分类'    '',
        'KTGRM' '该物料的科目设置组'    '',
        'MTPOS' '来自物料主文件的项目类别组'   '',
        'MVGR1' '物料组1'   '',
        'MVGR2' '物料组2'   '',
        'MVGR3' '物料组3'   '',
        'MVGR4' '物料组4'    '',
        'MVGR5' '物料组5'    '',
        'TRAGR'   '运输组'  '',
        'LADGR' '装载组'   '',
        'MTVFP' '可用性检查的检查组' '',
        'ALAND' '发货国' '',
        'TAX_TYPE_1' '税种' '',
        'MTVFP' '可用性检查的检查组' ''.
  ENDIF.
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

FORM DISPLAY_ALV_REPORT TABLES TDATA.
  GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GD_REPID
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_grid_title       = outtext
      IS_LAYOUT          = GD_LAYOUT
      IT_FIELDCAT        = FIELDCATALOG[]
*     it_special_groups  = gd_tabgroup
*     IT_EVENTS          = GT_XEVENTS
      I_SAVE             = 'X'
*     is_variant         = z_template
    TABLES
      T_OUTTAB           = TDATA
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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

FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM.


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
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_DATA1  text
*----------------------------------------------------------------------*
FORM FRM_XLS_TO_SAP  TABLES P_I_DATA .

  REFRESH  P_I_DATA[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = MATERIAL
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 3
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = P_I_DATA[]
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

  IF P_I_DATA[] IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.
ENDFORM.                    " FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA1 .
  DATA L_NUM   TYPE I.
  DATA L_PER   TYPE I.
  DATA L_LINE  TYPE I.
  CLEAR L_NUM.
  CLEAR L_PER.
  CLEAR L_LINE.

  DESCRIBE TABLE I_DATA1 LINES L_LINE.

*&--代码添加 BY HANDYBY 07.07.2017 11:03:56  BEGIN
  READ TABLE I_DATA1 WITH KEY TYPE = 'E' .
  IF SY-SUBRC EQ 0 .
    MOVE-CORRESPONDING I_DATA1[] TO T_DATA1[] .
  ELSE.
*&--代码添加 BY HANDYBY 07.07.2017 11:03:56  END

    LOOP AT I_DATA1.
      L_NUM = L_NUM + 1.
      L_PER = L_NUM DIV L_LINE.
      IF L_PER = 0.
        L_PER = 1.
      ENDIF.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = L_PER
          TEXT       = '数据上载中'.

*BAPIMATHEAD .
      CLEAR HEADDATA.
      PERFORM FRM_ALPHA_INPUT CHANGING I_DATA1-MATNR.

*&--代码添加 BY HANDYBY 16.05.2017 17:22:52  BEGIN
      TRANSLATE I_DATA1-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 16.05.2017 17:22:52  END

      HEADDATA-MATERIAL   = I_DATA1-MATNR."物料号
      HEADDATA-IND_SECTOR = I_DATA1-MBRSH."行业领域 MARA-MBRSH
      HEADDATA-MATL_TYPE  = I_DATA1-MTART."物料类型 MARA-MTART
      HEADDATA-BASIC_VIEW = 'X'.
      HEADDATA-PURCHASE_VIEW = 'X'.

*    HEADDATA-WORK_SCHED_VIEW = 'X'.
*    HEADDATA-QUALITY_VIEW    = 'X'.
*    HEADDATA-PURCHASE_VIEW   = 'X'.
*    HEADDATA-STORAGE_VIEW    = 'X'.

*      BAPI_MARA.
      CLEAR CLIENTDATA.
      CLIENTDATA-MATL_GROUP = I_DATA1-MATKL."物料组MARA-MATKL

      PERFORM FRM_UNIT_CHANGE CHANGING I_DATA1-MEINS.
      CLIENTDATA-BASE_UOM = I_DATA1-MEINS. " 基本计量单位MARA-MEINS
      PERFORM FRM_UNIT_CHANGE_OUTPUT CHANGING I_DATA1-MEINS.

*&--代码添加 BY HANDYBY 22.05.2017 08:23:32  BEGIN
      CLIENTDATA-EXTMATLGRP = I_DATA1-EXTWG. "外部物料组
*&--代码添加 BY HANDYBY 22.05.2017 08:23:32  END

      CLIENTDATA-MINREMLIFE = I_DATA1-MHDRZ."最短剩余货架寿命 MARA-MHDRZ
      CLIENTDATA-SHELF_LIFE = I_DATA1-MHDHB."总货架寿命

      PERFORM FRM_UNIT_CHANGE2 CHANGING I_DATA1-IPRKZ.
      CLIENTDATA-PERIOD_IND_EXPIRATION_DATE = I_DATA1-IPRKZ."货架寿命到期日的期间标识 MARA-IPRKZ
      CLIENTDATA-PO_UNIT  = I_DATA1-BSTME.                  "采购订单的计量单位 MARA-BSTME
      CLIENTDATA-DIVISION = I_DATA1-SPART.                  "产品组 MARA-SPART
      CLIENTDATA-OLD_MAT_NO   = I_DATA1-BISMT.              "旧物料号
      CLEAR CLIENTDATAX.

      CLIENTDATAX-MATL_GROUP = 'X'."物料组MARA-MATKL
      CLIENTDATAX-MINREMLIFE = 'X'."最短剩余货架寿命 MARA-MHDRZ
      CLIENTDATAX-SHELF_LIFE = 'X'."总货架寿命
      CLIENTDATAX-PERIOD_IND_EXPIRATION_DATE = 'X'."货架寿命到期日的期间标识 MARA-IPRKZ
      CLIENTDATAX-BASE_UOM   = 'X'. "基本计量单位MARA-MEINS
      CLIENTDATAX-PO_UNIT    = 'X'.  "采购订单的计量单位 MARA-BSTME

*&--代码添加 BY HANDYBY 22.05.2017 08:24:27  BEGIN
      CLIENTDATAX-EXTMATLGRP    = 'X'.
*&--代码添加 BY HANDYBY 22.05.2017 08:24:27  END

      CLIENTDATAX-OLD_MAT_NO = 'X'.  "旧物料号

      CLEAR UNITSOFMEASURE.
      REFRESH UNITSOFMEASURE.
*    UNITSOFMEASURE-DENOMINATR        = '1'.
      UNITSOFMEASURE-ALT_UNIT     = I_DATA1-MEINS.
      UNITSOFMEASURE-ALT_UNIT_ISO = I_DATA1-MEINS.
      UNITSOFMEASURE-LENGTH       = I_DATA1-LAENG.
      UNITSOFMEASURE-WIDTH        = I_DATA1-BREIT.
      UNITSOFMEASURE-HEIGHT       = I_DATA1-HOEHE.
      UNITSOFMEASURE-UNIT_DIM     = I_DATA1-MEABM.
      UNITSOFMEASURE-UNIT_DIM_ISO = I_DATA1-MEABM.
      APPEND UNITSOFMEASURE.

      CLEAR UNITSOFMEASUREX.
      REFRESH UNITSOFMEASUREX.
*    UNITSOFMEASUREX-DENOMINATR   = 'X'.
      UNITSOFMEASUREX-ALT_UNIT     = I_DATA1-MEINS.
      UNITSOFMEASUREX-ALT_UNIT_ISO = I_DATA1-MEINS.
      UNITSOFMEASUREX-LENGTH       = 'X'.
      UNITSOFMEASUREX-WIDTH        = 'X'.
      UNITSOFMEASUREX-HEIGHT       = 'X'.
      UNITSOFMEASUREX-UNIT_DIM     = 'X'.
      UNITSOFMEASUREX-UNIT_DIM_ISO = 'X'.
      APPEND UNITSOFMEASUREX.

*BAPI_MAKT
      CLEAR MATERIALDESCRIPTION.
      REFRESH MATERIALDESCRIPTION.
      MATERIALDESCRIPTION-MATL_DESC = I_DATA1-MAKTX. "物料描述MAKT-MAKTX
      MATERIALDESCRIPTION-LANGU     = 1.
      APPEND MATERIALDESCRIPTION.

      IF I_DATA1-EMAKTX IS NOT INITIAL.
        MATERIALDESCRIPTION-MATL_DESC = I_DATA1-EMAKTX."英文物料描述MAKT-MAKTX
        MATERIALDESCRIPTION-LANGU     = 'EN'.
        MATERIALDESCRIPTION-LANGU_ISO = 'EN'.
        APPEND MATERIALDESCRIPTION.
      ENDIF.

*当物料类型是ZKDT，复制ZH描述到Z1描述
      IF HEADDATA-MATL_TYPE  = 'ZKDT'AND I_DATA1-MAKTX IS NOT INITIAL.
        MATERIALDESCRIPTION-MATL_DESC = I_DATA1-MAKTX. "物料描述MAKT-MAKTX
        MATERIALDESCRIPTION-LANGU     = 'Z1'.
        MATERIALDESCRIPTION-LANGU_ISO = 'Z1'.
        APPEND MATERIALDESCRIPTION.
      ENDIF.

**添加物料长描述
*    AUTHORITY-CHECK OBJECT 'ZDM001'   ID 'ZDM001_01' FIELD 'TEXT1'.
*    IF SY-SUBRC = 0.
*      CLEAR MATERIALLONGTEXT.
*      REFRESH MATERIALLONGTEXT.
*      MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
*      MATERIALLONGTEXT-TEXT_NAME = I_DATA1-MATNR.
*      MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
*      MATERIALLONGTEXT-LANGU = '1'.
*      MATERIALLONGTEXT-FORMAT_COL = '*'.
*      MATERIALLONGTEXT-TEXT_LINE = I_DATA1-TEXT1.
*      APPEND MATERIALLONGTEXT.
*    ENDIF.

*    存储采购文本
*    AUTHORITY-CHECK OBJECT 'ZDM001'   ID 'ZDM001_02' FIELD 'TEXT2'.
*    IF SY-SUBRC = 0.
      CLEAR MATERIALLONGTEXT.
      MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
      MATERIALLONGTEXT-TEXT_NAME  = I_DATA1-MATNR.
      MATERIALLONGTEXT-TEXT_ID    = 'BEST'.
      MATERIALLONGTEXT-LANGU      = '1'.
      MATERIALLONGTEXT-FORMAT_COL = '*'.
      MATERIALLONGTEXT-TEXT_LINE  = I_DATA1-TEXT2.
      APPEND MATERIALLONGTEXT.
*    ENDIF.

*    CLEAR G_TE_MARA.
*    REFRESH G_TE_MARA.
*    G_TE_MARA-MATERIAL = I_DATA1-MATNR.
*    APPEND G_TE_MARA.

*    REFRESH EXTENSIONIN.
*    CLEAR EXTENSIONIN.
*    PARTVALUE = G_TE_MARA.
*    EXTENSIONIN-STRUCTURE = 'BAPI_TE_MARA'.
*    EXTENSIONIN-VALUEPART1 = PARTVALUE(240) .
*    EXTENSIONIN-VALUEPART2 = PARTVALUE+240(240) .
*    EXTENSIONIN-VALUEPART3 = PARTVALUE+480(240) .
*    EXTENSIONIN-VALUEPART4 = PARTVALUE+720(240) .
*    APPEND EXTENSIONIN.
*
*    PARTVALUE = G_TE_MARAX.
*    REFRESH EXTENSIONINX.
*    CLEAR EXTENSIONINX.
*    EXTENSIONINX-STRUCTURE = 'BAPI_TE_MARAX'.
*    EXTENSIONINX-VALUEPART1 = PARTVALUE(240) .
*    EXTENSIONINX-VALUEPART2 = PARTVALUE+240(240) .
*    EXTENSIONINX-VALUEPART3 = PARTVALUE+480(240) .
*    EXTENSIONINX-VALUEPART4 = PARTVALUE+720(240) .
*    APPEND EXTENSIONINX.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA            = HEADDATA
          CLIENTDATA          = CLIENTDATA
          CLIENTDATAX         = CLIENTDATAX
*         PLANTDATA           = PLANTDATA
*         PLANTDATAX          = PLANTDATAX
        IMPORTING
          RETURN              = RETURN
        TABLES
          MATERIALDESCRIPTION = MATERIALDESCRIPTION
          UNITSOFMEASURE      = UNITSOFMEASURE
          UNITSOFMEASUREX     = UNITSOFMEASUREX
          MATERIALLONGTEXT    = MATERIALLONGTEXT
          RETURNMESSAGES      = RETURNMESSAGES.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      WAIT UP TO SEC SECONDS.

**********************************************************************
*当创建成功的时候，扩建分类视图
      DATA   L_OBJECT LIKE BAPI1003_KEY-OBJECT.
      L_OBJECT = I_DATA1-MATNR.

      IF RETURN-TYPE = 'S'.
*读取该物料有没扩充分类视图
        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            OBJECTKEY       = L_OBJECT
            OBJECTTABLE     = 'MARA'
            CLASSNUM        = 'LYD001'
            CLASSTYPE       = '001'
          TABLES
            ALLOCVALUESNUM  = IT_NUM
            ALLOCVALUESCHAR = IT_CHAR
            ALLOCVALUESCURR = IT_CURR
            RETURN          = IT_RET.

        REFRESH:IT_NUM[],
                IT_CHAR[],
                IT_CURR[].
        CLEAR:IT_NUM,
              IT_CHAR,
              IT_CURR.
*分类特性
        IT_CHAR-CHARACT = 'GGXH'.
        IT_CHAR-VALUE_CHAR = I_DATA1-GGXH.
        APPEND IT_CHAR.

        IT_CHAR-CHARACT    = 'XSJJ'.
        IT_CHAR-VALUE_CHAR = I_DATA1-XSJJ.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT    = 'ROHS'.
        IT_CHAR-VALUE_CHAR = I_DATA1-ROHS.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT    = 'SFYGD'.
        IT_CHAR-VALUE_CHAR = I_DATA1-SFYGD.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT    = 'CHANGJIA'.
        IT_CHAR-VALUE_CHAR = I_DATA1-CHANGJIA.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT    = 'DHH'.
        IT_CHAR-VALUE_CHAR = I_DATA1-DHH1.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT    = 'TEZZ'.
        IT_CHAR-VALUE_CHAR = I_DATA1-TZZ.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT = 'CPLX'.
        IT_CHAR-VALUE_CHAR = I_DATA1-CPLX.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT = 'SHENQINGREN'.
        IT_CHAR-VALUE_CHAR = I_DATA1-SHENQINGREN.
        APPEND IT_CHAR .

        IT_CHAR-CHARACT = 'SKWJ'.
        IT_CHAR-VALUE_CHAR = I_DATA1-SKWJ.
        APPEND IT_CHAR .



*      IT_CHAR-CHARACT = 'SQR'.
*      IT_CHAR-VALUE_CHAR = I_DATA1-CPLX.
*      APPEND IT_CHAR .

        READ TABLE IT_RET WITH KEY TYPE = 'S'
                                   ID = 'CL'
                                   NUMBER = 731.
*当没有创建的，进入修改模式。
        IF SY-SUBRC <> 0.
          REFRESH IT_RET[].
          CALL FUNCTION 'BAPI_OBJCL_CREATE'
            EXPORTING
              OBJECTKEYNEW      = L_OBJECT
              OBJECTTABLENEW    = 'MARA'
              CLASSNUMNEW       = 'LYD001'
              CLASSTYPENEW      = '001'
              STATUS            = '1'
*             STANDARDCLASS     =
*             CHANGENUMBER      = 'LYD001'
*             KEYDATE           = SY-DATUM
              NO_DEFAULT_VALUES = 'X'
*             CLASSIF_STATUS    =
            TABLES
              ALLOCVALUESNUM    = IT_NUM
              ALLOCVALUESCHAR   = IT_CHAR
              ALLOCVALUESCURR   = IT_CURR
              RETURN            = IT_RET.

          READ TABLE IT_RET WITH KEY TYPE = 'E'.
          IF SY-SUBRC  <> 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

*执行成功表示物料创建成功
            I_DATA1-TYPE    = RETURN-TYPE.
            I_DATA1-MESSAGE = RETURN-MESSAGE.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            I_DATA1-TYPE    = IT_RET-TYPE.
            I_DATA1-MESSAGE = IT_RET-MESSAGE.
          ENDIF.
        ELSE.

*修改模式
          REFRESH IT_RET[].
          CALL FUNCTION 'BAPI_OBJCL_CHANGE'
            EXPORTING
              OBJECTKEY          = L_OBJECT
              OBJECTTABLE        = 'MARA'
              CLASSNUM           = 'LYD001'
              CLASSTYPE          = '001'
*             STATUS             = '1'
*             STANDARDCLASS      =
*             CHANGENUMBER       =
*             KEYDATE            = SY-DATUM
              NO_DEFAULT_VALUES  = 'X'
*             KEEP_SAME_DEFAULTS = ' '
*                IMPORTING
*             CLASSIF_STATUS     =
            TABLES
              ALLOCVALUESNUMNEW  = IT_NUM
              ALLOCVALUESCHARNEW = IT_CHAR
              ALLOCVALUESCURRNEW = IT_CURR
              RETURN             = IT_RET.

          READ TABLE IT_RET WITH KEY TYPE = 'E'.
          IF SY-SUBRC  <> 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

*执行成功表示物料创建成功
            I_DATA1-TYPE    = RETURN-TYPE.
            I_DATA1-MESSAGE = RETURN-MESSAGE.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            I_DATA1-TYPE    = IT_RET-TYPE.
            I_DATA1-MESSAGE = IT_RET-MESSAGE.
          ENDIF.
        ENDIF.

      ELSE.
        I_DATA1-TYPE    = RETURN-TYPE.
        I_DATA1-MESSAGE = RETURN-MESSAGE.
      ENDIF.

      LAENG = I_DATA1-LAENG.
      BREIT = I_DATA1-BREIT.
      HOEHE = I_DATA1-HOEHE.

      I_DATA1-LAENG = LAENG.
      I_DATA1-BREIT = BREIT.
      I_DATA1-HOEHE = HOEHE.
      MODIFY I_DATA1.
      APPEND I_DATA1 TO T_DATA1.

*      ENDIF.
      REFRESH:MATERIALLONGTEXT[],
              MATERIALDESCRIPTION[],
              UNITSOFMEASURE[],
              UNITSOFMEASUREX[].

**********************************************************************
      LOG_RETURN-MATNR   = I_DATA1-MATNR."日志增加对应得物料号
      LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
      APPEND LOG_RETURN.

*  REFRESH T_MARA.
    ENDLOOP.

    LOOP AT LOG_RETURN.
      WRITE:/ LOG_RETURN-MATNR,LOG_RETURN-MESSAGE.
    ENDLOOP.

*&--代码添加 BY HANDYBY 07.07.2017 11:07:11  BEGIN
  ENDIF .
*&--代码添加 BY HANDYBY 07.07.2017 11:07:11  END

ENDFORM.                    " FRM_FILL_DATA1
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTO_APPEND_BASIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTO_APPEND_BASIC USING WERKS TYPE WERKS_D.
  LOOP AT T_DATA1.
    CLEAR HEADDATA.
    HEADDATA-MATERIAL = T_DATA1-MATNR."物料号
    HEADDATA-WORK_SCHED_VIEW = 'X'.
    HEADDATA-QUALITY_VIEW    = 'X'.

    "*****************************1100
    CLEAR PLANTDATA.
    PLANTDATA-PLANT = WERKS."、1200、1201、1210、1211

    CLEAR PLANTDATAX.
    PLANTDATAX-PLANT = WERKS."、1200、1201、1210、1211

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA   = HEADDATA
        PLANTDATA  = PLANTDATA
        PLANTDATAX = PLANTDATAX
      IMPORTING
        RETURN     = RETURN.
    .
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    LOG_RETURN-MATNR   = T_DATA1-MATNR.   "日志增加对应得物料号
    LOG_RETURN-MESSAGE = RETURN-MESSAGE.  "日志增加消息文本
    APPEND LOG_RETURN.

    CONCATENATE T_DATA1-TYPE ';' RETURN-TYPE INTO T_DATA1-TYPE.
    CONCATENATE T_DATA1-MESSAGE ';' RETURN-MESSAGE INTO T_DATA1-MESSAGE.

    MODIFY T_DATA1.
  ENDLOOP.

  LOOP AT LOG_RETURN.
    WRITE:/ LOG_RETURN-MATNR,LOG_RETURN-MESSAGE.
  ENDLOOP.
ENDFORM.                    " FRM_AUTO_APPEND_BASIC
*&---------------------------------------------------------------------*
*&      Form  FRM_UNIT_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_UNIT  text
*----------------------------------------------------------------------*
FORM FRM_UNIT_CHANGE  CHANGING P_UNIT.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      INPUT          = P_UNIT
      LANGUAGE       = SY-LANGU
    IMPORTING
      OUTPUT         = P_UNIT
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.
ENDFORM.                    " FRM_UNIT_CHANGE

FORM FRM_UNIT_CHANGE_OUTPUT  CHANGING P_UNIT.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      INPUT          = P_UNIT
      LANGUAGE       = SY-LANGU
    IMPORTING
      OUTPUT         = P_UNIT
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.
ENDFORM.                    " FRM_UNIT_CHANGE
*&---------------------------------------------------------------------*
*&      Form  FRM_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_DATA1_MATNR  text
*----------------------------------------------------------------------*
FORM FRM_ALPHA_INPUT  CHANGING DATA.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = DATA
    IMPORTING
      OUTPUT = DATA.
ENDFORM.                    " FRM_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA3 .
  DATA L_NUM   TYPE I.
  DATA L_PER   TYPE I.
  DATA L_LINE  TYPE I.
  CLEAR L_NUM.
  CLEAR L_PER.
  CLEAR L_LINE.

  DESCRIBE TABLE I_DATA3 LINES L_LINE.

  LOOP AT I_DATA3.
    L_NUM = L_NUM + 1.
    L_PER = L_NUM DIV L_LINE.
    IF L_PER = 0.
      L_PER = 1.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = L_PER
        TEXT       = '数据上载中'.

    PERFORM FRM_ALPHA_INPUT CHANGING I_DATA3-MATNR.

*&--代码添加 BY HANDYBY 16.05.2017 21:51:03  BEGIN
    TRANSLATE I_DATA3-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 16.05.2017 21:51:03  END

    CLEAR HEADDATA.
    HEADDATA-MATERIAL   = I_DATA3-MATNR."物料号
    HEADDATA-SALES_VIEW = 'X'.
    HEADDATA-BASIC_VIEW = 'X'.
*    HEADDATA-

*BAPI_MVKE
    CLEAR SALESDATA.
    SALESDATA-SALES_ORG   = I_DATA3-VKORG."销售组织 MVKE-VKORG
    SALESDATA-DISTR_CHAN  = I_DATA3-VTWEG."分销渠道 MVKE-VTWEG
    SALESDATA-DELYG_PLNT  = I_DATA3-DWERK."交货工厂 (自有或外部) MVKE-DWERK
    SALESDATA-ACCT_ASSGT  = I_DATA3-KTGRM."该物料的科目设置组 MVKE-KTGRM
    SALESDATA-ITEM_CAT    = I_DATA3-MTPOS."来自物料主文件的项目类别组 MVKE-MTPOS
    SALESDATA-MATL_GRP_1  = I_DATA3-MVGR1."物料组1 MVKE-MVGR1
    SALESDATA-MATL_GRP_2  = I_DATA3-MVGR2."物料组2 MVKE-MVGR2
    SALESDATA-MATL_GRP_3  = I_DATA3-MVGR3."物料组3 MVKE-MVGR3
    SALESDATA-MATL_GRP_4  = I_DATA3-MVGR4."物料组4 MVKE-MVGR4
    SALESDATA-MATL_GRP_5  = I_DATA3-MVGR5."物料组5 MVKE-MVGR5


    CLEAR SALESDATAX.
    SALESDATAX-SALES_ORG  = I_DATA3-VKORG."销售组织 MVKE-VKORG
    SALESDATAX-DISTR_CHAN = I_DATA3-VTWEG."分销渠道 MVKE-VTWEG
    SALESDATAX-DELYG_PLNT = 'X'."交货工厂 (自有或外部) MVKE-DWERK
    SALESDATAX-ACCT_ASSGT = 'X'."该物料的科目设置组 MVKE-KTGRM
    SALESDATAX-ITEM_CAT   = 'X'."来自物料主文件的项目类别组 MVKE-MTPOS
    SALESDATAX-MATL_GRP_1 = 'X'."物料组1 MVKE-MVGR1
    SALESDATAX-MATL_GRP_2 = 'X'."物料组2 MVKE-MVGR2
    SALESDATAX-MATL_GRP_3 = 'X'."物料组3 MVKE-MVGR3
    SALESDATAX-MATL_GRP_4 = 'X'."物料组4 MVKE-MVGR4
    SALESDATAX-MATL_GRP_5 = 'X'."物料组5 MVKE-MVGR5

*装载组 MARC-LADGR
    CLEAR PLANTDATA.
    PLANTDATA-PLANT      = I_DATA3-DWERK.
    PLANTDATA-LOADINGGRP = I_DATA3-LADGR.
    CLEAR PLANTDATAX.
    PLANTDATAX-LOADINGGRP = 'X'.
    PLANTDATAX-PLANT = I_DATA3-DWERK.

*可用性检查的检查组
    PLANTDATA-AVAILCHECK  = I_DATA3-MTVFP.
    PLANTDATAX-AVAILCHECK = 'X'.

*物料的税分类  MLAN-TAXM1
    CLEAR TAXCLASSIFICATIONS.
    REFRESH TAXCLASSIFICATIONS.

*销售组织为1310为US，其余有CN
    IF SALESDATA-SALES_ORG <> '1310'.
      TAXCLASSIFICATIONS-DEPCOUNTRY = 'CN'.
    ELSE.
      TAXCLASSIFICATIONS-DEPCOUNTRY = 'US'.
    ENDIF.

    TAXCLASSIFICATIONS-TAXCLASS_1 = I_DATA3-TAXM1.
    TAXCLASSIFICATIONS-TAX_TYPE_1 = 'MWST'.
    APPEND TAXCLASSIFICATIONS.

*运输组 MARA-TRAGR
    CLEAR CLIENTDATA.
    CLIENTDATA-TRANS_GRP = I_DATA3-TRAGR.
    CLEAR CLIENTDATAX.
    CLIENTDATAX-TRANS_GRP = 'X'.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA           = HEADDATA
        CLIENTDATA         = CLIENTDATA
        CLIENTDATAX        = CLIENTDATAX
        SALESDATA          = SALESDATA
        SALESDATAX         = SALESDATAX
        PLANTDATA          = PLANTDATA
        PLANTDATAX         = PLANTDATAX
      IMPORTING
        RETURN             = RETURN
      TABLES
        TAXCLASSIFICATIONS = TAXCLASSIFICATIONS
        RETURNMESSAGES     = RETURNMESSAGES.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    WAIT UP TO SEC SECONDS.

    I_DATA3-TYPE    = RETURN-TYPE.
    I_DATA3-MESSAGE = RETURN-MESSAGE.
    MODIFY I_DATA3.
    APPEND I_DATA3 TO T_DATA3.

    LOG_RETURN-MATNR   = I_DATA3-MATNR.  "日志增加对应得物料号
    LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
    APPEND LOG_RETURN.
  ENDLOOP.

  LOOP AT LOG_RETURN.
    WRITE:/ LOG_RETURN-MATNR,LOG_RETURN-MESSAGE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA2 .
  DATA L_NUM   TYPE I.
  DATA L_PER   TYPE I.
  DATA L_LINE  TYPE I.
  CLEAR L_NUM.
  CLEAR L_PER.
  CLEAR L_LINE.

  DESCRIBE TABLE I_DATA2 LINES L_LINE.

  LOOP AT I_DATA2.
*      BAPIMATHEAD .
    L_NUM = L_NUM + 1.
    L_PER = L_NUM DIV L_LINE.
    IF L_PER = 0.
      L_PER = 1.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = L_PER
        TEXT       = '数据上载中!'.

    PERFORM FRM_ALPHA_INPUT CHANGING I_DATA2-MATNR.

*&--代码添加 BY HANDYBY 16.05.2017 21:50:26  BEGIN
    TRANSLATE I_DATA2-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 16.05.2017 21:50:26  END

    CLEAR HEADDATA.
    HEADDATA-MATERIAL = I_DATA2-MATNR.."物料号
    HEADDATA-WORK_SCHED_VIEW = 'X'.

    IF B = 'X'."采购视图
      HEADDATA-PURCHASE_VIEW = 'X'.
    ENDIF.
    IF C = 'X'.
      HEADDATA-MRP_VIEW     = 'X'.
    ENDIF.

    IF D = 'X'."质量管理视图
      HEADDATA-QUALITY_VIEW = 'X'.
    ENDIF.
    IF E = 'X'."工厂存储视图
      HEADDATA-STORAGE_VIEW = 'X'.
    ENDIF.
    IF F = 'X'."会计成本视图
      HEADDATA-ACCOUNT_VIEW = 'X'.
      HEADDATA-COST_VIEW = 'X'.
    ENDIF.

*财务数据
    CLEAR VALUATIONDATA .
    VALUATIONDATA-VAL_CLASS    = I_DATA2-BKLAS. "MBEW-BKLAS 评估类
    VALUATIONDATA-ML_SETTLE    = I_DATA2-MLAST. " MBEW-MLAST 价格确定
    VALUATIONDATA-PRICE_CTRL   = I_DATA2-VPRSV. "MBEW-VPRSV 价格控制指示符
    "ADD BY IT02 20160725 ：默认2100工厂的价格控制符为 "V" begin
    IF I_DATA2-WERKS EQ '2100' AND I_DATA2-VPRSV NE 'V'.
      VALUATIONDATA-PRICE_CTRL   = 'V'.
    ENDIF.
    "ADD BY IT02 20160725 ：默认2100工厂的价格控制符为 "V" end
    VALUATIONDATA-PRICE_UNIT   = I_DATA2-PEINH. " MBEW-PEINH 价格单位
    VALUATIONDATA-QTY_STRUCT   = I_DATA2-EKALR. "MBEW-EKALR 用QS的成本估算
    VALUATIONDATA-ORIG_MAT     = I_DATA2-HKMAT. "MBEW-HKMAT 物料相关的源
    VALUATIONDATA-PLNDPRICE1   = I_DATA2-ZPLP1. "MBEW-ZPLP1计划价格 1
    VALUATIONDATA-PLNDPRDATE1  = I_DATA2-ZPLD1. " MBEW-ZPLD1计划价格日期1

*&--代码添加 BY HANDYBY 22.05.2017 08:48:29  BEGIN
    VALUATIONDATA-PLNDPRICE2   = I_DATA2-ZPLP2. "MBEW-ZPLP2计划价格 2
    VALUATIONDATA-PLNDPRDATE2  = I_DATA2-ZPLD2. " MBEW-ZPLD2计划价格日期2
*&--代码添加 BY HANDYBY 22.05.2017 08:48:29  END

    VALUATIONDATA-VAL_AREA     = I_DATA2-WERKS. "评估范围

    CLEAR VALUATIONDATAX .
    VALUATIONDATAX-VAL_CLASS   =  'X'."MBEW-BKLAS 评估类
    VALUATIONDATAX-ML_SETTLE   =  'X'." MBEW-MLAST 价格确定
    VALUATIONDATAX-PRICE_CTRL  =  'X'."MBEW-VPRSV 价格控制指示符
    VALUATIONDATAX-PRICE_UNIT  =  'X'." MBEW-PEINH 价格单位
    VALUATIONDATAX-ORIG_MAT    =  'X'."MBEW-HKMAT 物料相关的源
    VALUATIONDATAX-PLNDPRICE1  =  'X'."MBEW-ZPLP1计划价格 1
    VALUATIONDATAX-PLNDPRDATE1 =  'X'." MBEW-ZPLD1计划价格日期1

*&--代码添加 BY HANDYBY 22.05.2017 08:49:10  BEGIN
    VALUATIONDATAX-PLNDPRICE2  =  'X'."MBEW-ZPLP2计划价格 2
    VALUATIONDATAX-PLNDPRDATE2 =  'X'." MBEW-ZPLD2计划价格日期2
*&--代码添加 BY HANDYBY 22.05.2017 08:49:10  END

    VALUATIONDATAX-VAL_AREA = I_DATA2-WERKS."评估范围

*工厂数据
    CLEAR PLANTDATA .
    PLANTDATA-PLANT        = I_DATA2-WERKS.
    PLANTDATA-PUR_STATUS   = I_DATA2-MMSTA ."MARC-MMSTA 工厂特定的物料状态
    PLANTDATA-VARIANCE_KEY = I_DATA2-AWSLS."差异码 MARC-AWSLS
    PLANTDATA-LOT_SIZE     = I_DATA2-LOSGR."批量产品成本核算 MARC-LOSGR
    PLANTDATA-PUR_GROUP    = I_DATA2-EKGRP." MARC-EKGRP 采购组
    PLANTDATA-BATCH_MGMT   = I_DATA2-XCHPF."  MARC-XCHPF 标识：批次管理需求
    PLANTDATA-SOURCELIST   = I_DATA2-KORDB." MARC-KORDB 源清单
    PLANTDATA-LOTSIZEKEY   = I_DATA2-DISLS." MARC-DISLS 批量大小
    PLANTDATA-MINLOTSIZE   = I_DATA2-BSTMI."  MARC-BSTMI 最小批量大小(BTCI)
    PLANTDATA-SPPROCTYPE   = I_DATA2-SOBSL." MARC-SOBSL 特殊采购类型
    PLANTDATA-SLOC_EXPRC   = I_DATA2-LGFSB." MARC-LGFSB 外部采购仓储地点
    PLANTDATA-PLND_DELRY   = I_DATA2-PLIFZ." MARC-PLIFZ 计划交货时间(天数)
    PLANTDATA-SAFETY_STK   = I_DATA2-EISBE." MARC-EISBE 安全库存
    PLANTDATA-MRP_TYPE     = I_DATA2-DISMM." MARC-DISMM, MRP 类型
    PLANTDATA-MRP_CTRLER   = I_DATA2-DISPO." MARC-DISPO MRP 控制者
    PLANTDATA-PROC_TYPE    = I_DATA2-BESKZ."MARC-BESKZ 采购类型
    PLANTDATA-ISS_ST_LOC   = I_DATA2-LGPRO." MARC-LGPRO 生产仓储地点
    PLANTDATA-SM_KEY       = I_DATA2-FHORI." MARC-FHORI 计划边际码
    PLANTDATA-BACKFLUSH    = I_DATA2-RGEKZ ." MARC-RGEKZ 标识：反冲
    PLANTDATA-PLAN_STRGP   = I_DATA2-STRGR."MARC-STRGR 策略组
    PLANTDATA-CONSUMMODE   = I_DATA2-VRMOD." MARC-VRMOD 消耗模式
    PLANTDATA-BWD_CONS     = I_DATA2-VINT1." MARC-VINT1 逆向消耗期间
    PLANTDATA-FWD_CONS     = I_DATA2-VINT2." MARC-VINT2 向前消耗期间
    PLANTDATA-AVAILCHECK   = I_DATA2-MTVFP." MARC-MTVFP 可用性检查
    PLANTDATA-DEP_REQ_ID   = I_DATA2-SBDKZ."MARC-SBDKZ 独立/集中
    PLANTDATA-DISCONTINU   = I_DATA2-KZAUS ."MARC-KZAUS 非连续标识
    PLANTDATA-EFF_O_DAY    = I_DATA2-AUSDT." MARC-AUSDT 生效期
    PLANTDATA-FOLLOW_UP    = I_DATA2-NFMAT."MARC-NFMAT 后继的物料
    PLANTDATA-PRODUCTION_SCHEDULER = I_DATA2-FEVOR." MARC-FEVOR 生产调度员
    PLANTDATA-PROC_TIME    = I_DATA2-BEARZ." MARC-BEARZ 处理时间(BTCI)
    PLANTDATA-BASE_QTY     = I_DATA2-BASMG." MARC-BASMG 基准数量(BTCI)
    PLANTDATA-SETUPTIME    = I_DATA2-RUEZT."准备时间
    PLANTDATA-ROUND_VAL    = I_DATA2-BSTRF."舍入值
    CLEAR PLANTDATAX.

    PLANTDATAX-PLANT         = I_DATA2-WERKS.
    PLANTDATAX-PUR_STATUS    =  'X' ."MARC-MMSTA 工厂特定的物料状态
    PLANTDATAX-VARIANCE_KEY  =  'X'."差异码 MARC-AWSLS
    PLANTDATAX-LOT_SIZE      =  'X'."批量产品成本核算 MARC-LOSGR
    PLANTDATAX-PUR_GROUP     =  'X'." MARC-EKGRP 采购组
    PLANTDATAX-BATCH_MGMT    =  'X'."  MARC-XCHPF 标识：批次管理需求
    PLANTDATAX-SOURCELIST    =  'X'." MARC-KORDB 源清单
    PLANTDATAX-LOTSIZEKEY    =  'X'." MARC-DISLS 批量大小
    PLANTDATAX-MINLOTSIZE    =  'X'."  MARC-BSTMI 最小批量大小(BTCI)
    PLANTDATAX-SPPROCTYPE    =  'X'." MARC-SOBSL 特殊采购类型
    PLANTDATAX-SLOC_EXPRC    =  'X'." MARC-LGFSB 外部采购仓储地点
    PLANTDATAX-PLND_DELRY    =  'X'." MARC-PLIFZ 计划交货时间(天数)
    PLANTDATAX-SAFETY_STK    =  'X'." MARC-EISBE 安全库存
    PLANTDATAX-MRP_TYPE      =  'X'." MARC-DISMM, MRP 类型
    PLANTDATAX-MRP_CTRLER    =  'X'." MARC-DISPO MRP 控制者
    PLANTDATAX-PROC_TYPE     =  'X'."MARC-BESKZ 采购类型
    PLANTDATAX-ISS_ST_LOC    =  'X'." MARC-LGPRO 生产仓储地点
    PLANTDATAX-SM_KEY        =  'X'." MARC-FHORI 计划边际码
    PLANTDATAX-BACKFLUSH     =  'X' ." MARC-RGEKZ 标识：反冲
    PLANTDATAX-CONSUMMODE    =  'X'." MARC-VRMOD 消耗模式
    PLANTDATAX-BWD_CONS      =  'X'." MARC-VINT1 逆向消耗期间
    PLANTDATAX-FWD_CONS      =  'X'." MARC-VINT2 向前消耗期间
    PLANTDATAX-AVAILCHECK    =  'X'." MARC-MTVFP 可用性检查
    PLANTDATAX-DEP_REQ_ID    =  'X'."MARC-SBDKZ 独立/集中
    PLANTDATAX-DISCONTINU    =  'X' ."MARC-KZAUS 非连续标识
    PLANTDATAX-EFF_O_DAY     =  'X'." MARC-AUSDT 生效期
    PLANTDATAX-FOLLOW_UP     =  'X'."MARC-NFMAT 后继的物料
    PLANTDATAX-PRODUCTION_SCHEDULER =  'X'." MARC-FEVOR 生产调度员
    PLANTDATAX-PROC_TIME     =  'X'." MARC-BEARZ 处理时间(BTCI)
    PLANTDATAX-BASE_QTY      =  'X'." MARC-BASMG 基准数量(BTCI)
    PLANTDATAX-PLAN_STRGP    =  'X'."MARC-STRGR 策略组
    PLANTDATAX-SETUPTIME     =  'X'."准备时间
    PLANTDATAX-ROUND_VAL     =  'X'."舍入值
    CLEAR G_TE_MARC.
    REFRESH G_TE_MARC.

    G_TE_MARC-PLANT = I_DATA2-WERKS."工厂
    APPEND G_TE_MARC.

    REFRESH RETURNMESSAGES.
    CLEAR RETURNMESSAGES.
    CLEAR RETURN.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA       = HEADDATA
        PLANTDATA      = PLANTDATA
        PLANTDATAX     = PLANTDATAX
        VALUATIONDATA  = VALUATIONDATA
        VALUATIONDATAX = VALUATIONDATAX
      IMPORTING
        RETURN         = RETURN
      TABLES
        RETURNMESSAGES = RETURNMESSAGES.
*        INTERNATIONALARTNOS = INSPECTIONCTRL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    WAIT UP TO SEC SECONDS.

*    CONCATENATE TTYPE RETURN-TYPE '_' INTO TTYPE.
    LOG_RETURN-MATNR   = I_DATA2-MATNR."日志增加对应得物料号
    LOG_RETURN-MESSAGE = RETURN-MESSAGE."日志增加消息文本
    APPEND LOG_RETURN.

    CLEAR TTYPE.
    CLEAR TMESSAGE.

*当基本视图没有创建错误消息，并且检验类型有值时，扩充质量视图
    IF RETURN-TYPE = 'S'
    AND I_DATA2-ART IS NOT INITIAL.

*质量视图
      REFRESH INSPECTIONCTRL.
      CLEAR INSPECTIONCTRL.
*      INSPECTIONCTRL-FUNCTION = '004'.
      INSPECTIONCTRL-INSPTYPE                = I_DATA2-ART.  "QMAT-ART 检验类型
      INSPECTIONCTRL-PREFERRED_INSPTYPE      = I_DATA2-APA.  "QMAT-APA  首选检验类型
      INSPECTIONCTRL-IND_INSPTYPE_MAT_ACTIVE = I_DATA2-AKTIV." QMAT-AKTIV 活动
      INSPECTIONCTRL-QUAL_SCORE_PROCEDURE    = '06'.
      INSPECTIONCTRL-MATERIAL = I_DATA2-MATNR.
      INSPECTIONCTRL-PLANT    = I_DATA2-WERKS.

*当检验类型为Z1
      "APPENDED Z3 的检验类型  By it02 20161009
      IF INSPECTIONCTRL-INSPTYPE = 'Z1' OR INSPECTIONCTRL-INSPTYPE = 'Z3'.
        INSPECTIONCTRL-IND_POST_TO_INSP_STOCK     = 'X'."过账到检验库存
        INSPECTIONCTRL-IND_INSP_WITH_TSK_LIST     = 'X'."有任务清单的检验
        INSPECTIONCTRL-IND_AUTO_ASSIGN            = 'X'."自动分配
        INSPECTIONCTRL-IND_INSP_BY_CHARAC         = 'X'."按特性检查
        INSPECTIONCTRL-IND_SKIPS_ALLOWED          = 'X'."略过允许
        INSPECTIONCTRL-IND_AUTOMATIC_UD           = 'X'."自动使用决策
        INSPECTIONCTRL-IND_SINGLE_UNITS_POSSIBLE  = 'X'."可能的序列号管理
        INSPECTIONCTRL-CONTR_INSP_LOT_CREATE      = ''. "控制检验批
      ENDIF.

*当检验类型为Z2
      IF INSPECTIONCTRL-INSPTYPE = 'Z2'.
        INSPECTIONCTRL-IND_INSP_WITH_TSK_LIST     = 'X'."有任务清单的检验
        INSPECTIONCTRL-IND_AUTO_ASSIGN            = 'X'."自动分配
        INSPECTIONCTRL-IND_INSP_BY_CHARAC         = 'X'."按特性检查
        INSPECTIONCTRL-IND_SKIPS_ALLOWED          = 'X'."略过允许
        INSPECTIONCTRL-IND_AUTOMATIC_UD           = 'X'."自动使用决策
        INSPECTIONCTRL-IND_SINGLE_UNITS_POSSIBLE  = 'X'."可能的序列号管理
      ENDIF.


      APPEND INSPECTIONCTRL.

      CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
        TABLES
          RETURN         = IRETURN
          INSPECTIONCTRL = INSPECTIONCTRL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      WAIT UP TO SEC SECONDS.

      READ TABLE IRETURN WITH  KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        CONCATENATE TTYPE IRETURN-TYPE ';' INTO TTYPE.
        CONCATENATE TMESSAGE IRETURN-MESSAGE ';' INTO TMESSAGE.
        LOG_RETURN-MATNR   = I_DATA2-MATNR.  "日志增加对应得物料号
        LOG_RETURN-MESSAGE = IRETURN-MESSAGE."日志增加消息文本
        APPEND LOG_RETURN.
        I_DATA2-MESSAGE = TMESSAGE.
        I_DATA2-TYPE    = TTYPE.

      ELSE.
        CONCATENATE TTYPE RETURN-TYPE ';' INTO TTYPE.
        CONCATENATE TMESSAGE RETURN-MESSAGE ';' INTO TMESSAGE.

        I_DATA2-MESSAGE = TMESSAGE.
        I_DATA2-TYPE = TTYPE.
      ENDIF.

    ELSE.
      CONCATENATE TTYPE RETURN-TYPE ';' INTO TTYPE.
      CONCATENATE TMESSAGE RETURN-MESSAGE ';' INTO TMESSAGE.

      I_DATA2-MESSAGE = TMESSAGE.
      I_DATA2-TYPE    = TTYPE.

    ENDIF.

*记录返回消息

    LOSGR = I_DATA2-LOSGR.
    I_DATA2-LOSGR = LOSGR.

    ZPLP1 = I_DATA2-ZPLP1.
    I_DATA2-ZPLP1 = ZPLP1.

    MODIFY I_DATA2.
    APPEND I_DATA2 TO T_DATA2.
  ENDLOOP.

*输出日志
  LOOP AT LOG_RETURN.
    WRITE:/ LOG_RETURN-MATNR,LOG_RETURN-MESSAGE.
  ENDLOOP.

ENDFORM.                    " FRM_FILL_DATA2
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTO_APPEND_PLANTVIEW_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WERKS  text
*----------------------------------------------------------------------*
FORM FRM_AUTO_APPEND_PLANTVIEW_F  USING    P_WERKS.
  MOVE-CORRESPONDING I_DATA2 TO T_DATA2.
  LOOP AT TZTCDM003."循环每个要改的字段，扩充的规则
    ASSIGN COMPONENT TZTCDM003-FIELD OF STRUCTURE T_DATA2 TO <F>.
    "找对应规则
    READ TABLE TZTCDM004 WITH KEY TNAME = TZTCDM003-TNAME
                                  FIELD = TZTCDM003-FIELD
                                 SWERKS = T_DATA2-WERKS
                                 SVALUE = <F>
                                 DWERKS = P_WERKS.
    IF SY-SUBRC = 0.
      <F> = TZTCDM004-DVALUE.
    ENDIF.

  ENDLOOP.

  T_DATA2-WERKS = P_WERKS."要扩充的工厂
  APPEND T_DATA2.
ENDFORM.                    " FRM_AUTO_APPEND_PLANTVIEW_F
*&---------------------------------------------------------------------*
*&      Form  FRM_UNIT_CHANGE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_DATA1_IPRKZ  text
*----------------------------------------------------------------------*
FORM FRM_UNIT_CHANGE2  CHANGING P_I_DATA1_IPRKZ.
  CALL FUNCTION 'CONVERSION_EXIT_PERKZ_INPUT'
    EXPORTING
      INPUT  = P_I_DATA1_IPRKZ
    IMPORTING
      OUTPUT = P_I_DATA1_IPRKZ.

ENDFORM.                    " FRM_UNIT_CHANGE2
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DATA21  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_EXIST  TABLES P1 STRUCTURE I_DATA1 .
  DATA: BEGIN OF LS_MARA ,
          MATNR TYPE MARA-MATNR,
        END OF LS_MARA .
  DATA LT_MARA LIKE TABLE OF LS_MARA .
  SELECT MATNR
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM MARA
     FOR ALL ENTRIES IN P1
   WHERE MATNR = P1-MATNR .
  SORT LT_MARA BY MATNR .
  IF LT_MARA IS NOT INITIAL .
    LOOP AT P1 .
      READ TABLE LT_MARA WITH KEY MATNR = P1-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC EQ 0 .
        P1-TYPE = 'E'.
        P1-MESSAGE = '物料已存在！'.
        MODIFY P1 .
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1041   text
*----------------------------------------------------------------------*
FORM FRM_CHECK_AUTH  USING    VALUE(P_1041).

  DATA L_STR(6) TYPE C .
  DATA L_MSG TYPE STRING .

  CASE P_1041 .
    WHEN '01'.
      L_STR = '基本属性导入' .
    WHEN '02' .
      L_STR = '工厂属性导入' .
    WHEN '03' .
      L_STR = '销售属性导入' .
  ENDCASE.

  AUTHORITY-CHECK OBJECT 'ZM_STA_001'
        ID 'ZM_STA_001' FIELD P_1041 .
  IF SY-SUBRC IS INITIAL .

  ELSE.
    CONCATENATE '你没有' L_STR '的权限' INTO L_MSG .
    MESSAGE L_MSG TYPE 'E' .
  ENDIF.

ENDFORM.
