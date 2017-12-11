*&---------------------------------------------------------------------*
*&程序名称/Program Name         :     ZMM040
*&程序描述/Program Des.         :     项目出货单打印
*&申请人/Applicant              :     14060
*&申请日期/Date of App          :     2017.5.19
*&作者/Author                   :    WANGYUJIE
*&完成日期/Completion Date      :
*&---------------------------------------------------------------------*
*&摘要：1).业务场景
*
*&      2).调拨方案实现：
*&
*&---------------------------------------------------------------------*
*&变更记录：
*&---------------------------------------------------------------------*                                                      *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2017.5.19    WANGYUJIE
*&---------------------------------------------------------------------*

REPORT ZMM040.
*---------------------------------------------------------------------*
* Type-pool                                                           *
*                                                                     *
*---------------------------------------------------------------------*
TYPE-POOLS:SLIS,VRM.      " Globale Type

*---------------------------------------------------------------------*
* Declare tables                                                      *
*                                                                     *
*---------------------------------------------------------------------*
TABLES: PRPS,PROJ,MARA,RESB,ZZT_CHUKU_A.

*---------------------------------------------------------------------*
* Types                                                               *
*                                                                     *
*---------------------------------------------------------------------*
*ALV展示
TYPES:BEGIN OF TY_ALV,
        PSPNR       TYPE PRPS-PSPNR,  "WBS元素
        POST1       TYPE PROJ-POST1,  "项目名称
        ZKHBM       TYPE PROJ-ZKHBM,  "客户
        NAME1       TYPE KNA1-NAME1,   "客户名称
        RSNUM       TYPE RESB-RSNUM,   "预留编号
        RSPOS       TYPE RESB-RSPOS,   "行项目
        MATNR       TYPE RESB-MATNR,   "物料编码
        MAKTX       TYPE MAKT-MAKTX,   "物料描述
        EXTWG       TYPE MARA-EXTWG,   "品牌
        MEINS       TYPE RESB-MEINS,    "单位
        GPREIS      TYPE CKIS-GPREIS,   "单价
        PWHIE       TYPE PROJ-PWHIE,   "货币
        BDMNG       TYPE RESB-BDMNG,   "需求数量
        ENMNG       TYPE RESB-ENMNG,   "已发数量
        NAME2       TYPE KNVK-NAME1,   "收货人
        TEL_NUMBER  TYPE ADR2-TEL_NUMBER, "联系电话
        MENGV13(13),             "发货数量
        WERKS       TYPE PRPS-WERKS,   "工厂
        ERNAM       TYPE PRPS-ERNAM,   "创建者
        ERDAT       TYPE PRPS-ERDAT,   "创建日期
        VERNA       TYPE PRPS-VERNA,   "负责人
        ZXMDZ       TYPE PROJ-ZXMDZ,   "项目地址
        BOX         TYPE C,
        ZDMBTR      TYPE BSAK-DMBTR,  "金额
        ZDY(20)     TYPE C, "打印日期
      END OF TY_ALV.

*PRPS&RESB TABLE
TYPES:BEGIN OF TY_PRPS_RESB,
        MATNR  TYPE RESB-MATNR,   "物料编码
        PSPNR  TYPE PRPS-PSPNR,  "WBS元素
        WERKS  TYPE PRPS-WERKS,   "工厂
        ERNAM  TYPE PRPS-ERNAM,   "创建者
        ZKHBM  TYPE PRPS-ZKHBM,
        ERDAT  TYPE PRPS-ERDAT,   "创建日期
        VERNA  TYPE PRPS-VERNA,   "负责人
        PSPHI  TYPE PRPS-PSPHI,   "合适的项目的当前编号
        PSPHI1 TYPE PRPS-POSID,  "编号内部转换存储
        PRART  TYPE PRPS-PRART,   "项目类型
        RSNUM  TYPE RESB-RSNUM,   "预留编号
        RSPOS  TYPE RESB-RSPOS,   "行项目
        MEINS  TYPE RESB-MEINS,    "单位
        BDMNG  TYPE RESB-BDMNG,   "需求数量
        ENMNG  TYPE RESB-ENMNG,   "已发数量
        BDTER  TYPE RESB-BDTER,   "需求日期
        PSPEL  TYPE RESB-PSPEL,   "WBS 要素
        AUFNR  TYPE RESB-AUFNR,
        BWART  TYPE RESB-BWART,
      END OF TY_PRPS_RESB.
*proj table
TYPES:BEGIN OF TY_PROJ,
        ZKHBM TYPE PROJ-ZKHBM,  "客户
        POST1 TYPE PROJ-POST1,  "项目名称
        PWHIE TYPE PROJ-PWHIE,   "货币
        ZXMDZ TYPE PROJ-ZXMDZ,   "项目地址
        PSPID TYPE PROJ-PSPID,   "项目定义
        PSPNR TYPE PROJ-PSPNR,
      END OF TY_PROJ.
*makt TABLE
TYPES:BEGIN OF TY_MAKT,
        MAKTX TYPE MAKT-MAKTX,   "物料描述
        MATNR TYPE MAKT-MATNR,   "物料编码
      END OF TY_MAKT.
*mara
TYPES:BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,   "物料编码
        EXTWG TYPE MARA-EXTWG,   "品牌
      END OF TY_MARA.
*kna1&knvk TBALE
TYPES:BEGIN OF TY_KNA1,
        NAME1 TYPE KNA1-NAME1,   "客户名称
        KUNNR TYPE KNA1-KUNNR,   "客户编号
      END OF TY_KNA1.

TYPES:BEGIN OF TY_KNVK,
        NAME2 TYPE KNVK-NAME1,   "收货人
        PRSNR TYPE KNVK-PRSNR,   "人员编号
        KUNNR TYPE KNVK-KUNNR,   "客户编号
      END OF TY_KNVK.

*adr2 TABLE
TYPES:BEGIN OF TY_ADR2,
        TEL_NUMBER TYPE ADR2-TEL_NUMBER, "联系电话
        R3_USER    TYPE ADR2-R3_USER,       "标识符：电话是移动电话
        PERSNUMBER TYPE ADR2-PERSNUMBER,   "人员编号
      END OF TY_ADR2.


DATA : GT_WA3 LIKE TABLE OF ZZT_CHUKU_A,
       GS_WA3 LIKE LINE OF GT_WA3.

*&--代码添加 BY HANDYBY 11.08.2017 17:16:47  BEGIN
DATA: GT_ZMMT040 TYPE TABLE OF ZMMT040,
      GS_ZMMT040 TYPE ZMMT040.
*&--代码添加 BY HANDYBY 11.08.2017 17:16:47  END

*---------------------------------------------------------------------*
* Defining a structured                                               *
*                                                                     *
*---------------------------------------------------------------------*
*&-----------------ALV展示--------------------------&*
DATA : GT_ALV TYPE TABLE OF TY_ALV,
       GS_ALV LIKE LINE OF GT_ALV.

DATA : GT_ALV1 TYPE TABLE OF TY_ALV,
       GS_ALV1 LIKE LINE OF GT_ALV1.

DATA : GT_ALV2 TYPE TABLE OF TY_ALV,
       GS_ALV2 LIKE LINE OF GT_ALV2.

DATA : GT_ALV3 TYPE TABLE OF TY_ALV,
       GS_ALV3 LIKE LINE OF GT_ALV3.


DATA : GT_ALV4 TYPE TABLE OF TY_ALV,
       GS_ALV4 LIKE LINE OF GT_ALV4.
*PPRPS&RESB TABLE
DATA : GT_PRPS_RESB TYPE TABLE OF TY_PRPS_RESB,
       GS_PRPS_RESB LIKE LINE OF GT_PRPS_RESB.
*PROJ TABLE
DATA : GT_PROJ TYPE TABLE OF TY_PROJ,
       GS_PROJ LIKE LINE OF GT_PROJ.

DATA : GT_PROJB TYPE TABLE OF TY_PROJ,
       GS_PROJB LIKE LINE OF GT_PROJB.
*makt TBALE
DATA : GT_MAKT TYPE TABLE OF TY_MAKT,
       GS_MAKT LIKE LINE OF  GT_MAKT.
*mara
DATA : GT_MARA TYPE TABLE OF TY_MARA,
       GS_MARA LIKE LINE OF  GT_MARA.
*kna1&knvk TBALE
DATA : GT_KNA1 TYPE TABLE OF TY_KNA1,
       GS_KNA1 LIKE LINE OF  GT_KNA1.
DATA : GT_KNVK TYPE TABLE OF TY_KNVK,
       GS_KNVK LIKE LINE OF  GT_KNVK.
*adr2 TABLE
DATA : GT_ADR2 TYPE TABLE OF TY_ADR2,
       GS_ADR2 LIKE LINE OF  GT_ADR2.

DATA : GT_GPREIS TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX,
       GS_GPREIS TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
DATA: GT_MESG TYPE BAPIRET2_T.

**---------------HANDHJD    修改   2017/7/14 ----------BEGIN----------
*FIELD-SYMBOLS:
*               <FS_DATA> TYPE CNCI_PRPS.
*ASSIGN ('(SAPLXCN1)CNCI_PRPS-ZKHBM') TO  <FS_DATA>.
*
**---------------HANDHJD    修改   2017/7/14 ----------END----------

*-----------------------------屏幕的触发事件--------------------------------*
DATA:OK_CODE TYPE SY-UCOMM,
     SAVE_OK TYPE SY-UCOMM.
*-----------------------------screen屏幕--------------------------------------*
DATA: FLD1(20) TYPE C.
*内表，记录下拉菜单screen字段变量定义
*运输方式
DATA:LISTBOX1 TYPE VRM_ID,
     VVA1     TYPE VRM_VALUES,
     LVVA1    LIKE LINE OF VVA1.
*运费承担
DATA:LISTBOX2 TYPE VRM_ID,
     VVA2     TYPE VRM_VALUES,
     LVVA2    LIKE LINE OF VVA2.
DATA: ZXMDZ_S(50),    "到货地址
      NAME2_S(35),      "收货人
      TEL(30),          "联系电话
      POST1_S(40).      "项目名称 使用单位


*&-----------------打印相关的声明---------------------&*
DATA:  FM_NAME            TYPE RS38L_FNAM,
       OUTPUT             TYPE SSFCOMPOP,
       CONTROL_PARAMETERS TYPE SSFCTRLOP,
       LW_SSFCRESCL       TYPE SSFCRESCL,
       OPTION             TYPE SSFCRESCL.

*为打印创建的结构
DATA: ZZS_TAB LIKE ZMM040_CK,     "导入到smartform表格接口里的导入
      GT_TAB  LIKE TABLE OF ZMM040_CK,
      GS_TAB  LIKE  ZMM040_CK.

*每页条数
DATA:ZPAGE_NUM TYPE I VALUE 12,
     SUM       TYPE MENGV13.
DATA:
      Z_SUM TYPE BSAK-DMBTR.
DATA : ZMENGE(13)   TYPE C,
       ZMENGE_A(13) TYPE C.

*pdf
DATA:
  GT_PDFTAB     TYPE TABLE OF TLINE,
  G_BINFILESIZE TYPE I.
*用户选择保存路径
DATA:L_FULLPATH TYPE STRING,
     L_PATH     TYPE STRING,
     L_NAME     TYPE STRING.

*---------------------------------------------------------------------*
* ALV                                                                 *
*                                                                     *
*---------------------------------------------------------------------*
DATA:
  GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,  "控制ALV 的显示信息
  GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
  GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,     "控制整体ALV的显示
  G_REPID     LIKE SY-REPID VALUE SY-REPID. "获取当前系统名
*---------------------------------------------------------------------*
* Selection-screen                                                    *
*                                                                     *
*---------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.  "选择画面（1000）的标准GUI的功能按钮1
SELECTION-SCREEN BEGIN OF BLOCK BK1 WITH FRAME TITLE TEXT-001.  "创建框架
*多选
SELECT-OPTIONS: S_PSPNR FOR PRPS-POSID OBLIGATORY,  "WBS元素
                S_PRART FOR PRPS-PRART,          "项目类型
                S_ZKHBM FOR PROJ-ZKHBM,         "客户
                S_MATNR FOR MARA-MATNR,         "物料号
                S_WERKS FOR PRPS-WERKS,          "工厂
                S_ERNAM FOR PRPS-ERNAM,         "创建者
                S_ERDAT FOR PRPS-ERDAT,         "创建日期
                S_VERNR FOR PRPS-VERNR,         "负责人
                S_BDTER FOR RESB-BDTER,         "需求日期
                S_RSNUM FOR RESB-RSNUM .        "预留编号
*&--代码添加 BY HANDYBY 16.08.2017 15:18:33  BEGIN
PARAMETERS P1 AS CHECKBOX .
PARAMETERS P2 AS CHECKBOX .
*&--代码添加 BY HANDYBY 16.08.2017 15:18:33  END
SELECTION-SCREEN END OF BLOCK BK1.

*---------------------------------------------------------------------*
* Initialization                                                      *
*    初始化                                                                 *
*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
* At selection-screen                                                 *
*                                                                     *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*---------------------------------------------------------------------*
* Start-of-selection                                                  *
*                                                                     *
*---------------------------------------------------------------------*
START-OF-SELECTION.
*&--代码添加 BY HANDYBY 16.08.2017 16:35:58  BEGIN
  PERFORM FRM_CHECK_VAR .
*&--代码添加 BY HANDYBY 16.08.2017 16:35:58  END
  PERFORM FRM_SELDATA.   "根据条件选择数据
*  IF GT_ALV IS INITIAL.
*    MESSAGE 'NO DATA!' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
  PERFORM FRM_WRITE.     "输出数据
*---------------------------------------------------------------------*
*   End-of-selection                                                  *
*                                                                     *
*---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_SELDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SELDATA .

  DATA:LT_ALV TYPE TABLE OF TY_ALV,
       LS_ALV TYPE TY_ALV.
  DATA: LT_PRPS TYPE TABLE OF TY_PRPS_RESB,
        LS_PRPS LIKE LINE OF LT_PRPS.
  DATA: LT_PROJ TYPE TABLE OF TY_PROJ.
  DATA: LT_KNVK TYPE TABLE OF TY_KNVK.
  DATA: LT_KNA1 TYPE TABLE OF TY_KNA1.
  DATA: LV_PSPNR TYPE PS_PSPID.
  DATA: LT_MATNR TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP-COST_LINES,
        LS_MATNR TYPE KIS1.
  DATA: LT_GPREIS TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.   "内表
*------------主表prps_resb-----------------------------*
  SELECT
    PRPS~PSPNR  "WBS元素
    PRPS~WERKS   "工厂
    PRPS~ZKHBM   "工厂
    PRPS~ERNAM   "创建者
    PRPS~ERDAT   "创建日期
    PRPS~VERNA   "负责人
    PRPS~PSPHI   "合适的项目的当前编号
    PRPS~PRART   "项目类型
    RESB~RSNUM   "预留编号
    RESB~RSPOS   "行项目
    RESB~MATNR   "物料
    RESB~MEINS   "单位
    RESB~BDMNG   "需求数量
    RESB~ENMNG   "已发数量
    RESB~BDTER   "需求日期
    RESB~PSPEL   "WBS 要素
    RESB~BWART
*    resb~aufnr
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS_RESB
    FROM PRPS
    INNER JOIN RESB ON  PRPS~PSPNR = RESB~PSPEL    "WBS 要素
    WHERE RESB~XLOEK <> 'X'

*&--代码注释 BY HANDYBY 27.07.2017 09:38:03  BEGIN
*    AND  resb~kzear <> 'X'
*&--代码注释 BY HANDYBY 27.07.2017 09:38:03  END

*    AND  resb~bwart eq '281'
*    AND RESB~AUFNR EQ ''
    AND  PRPS~POSID IN S_PSPNR  "WBS元素
    AND   PRPS~PRART IN S_PRART  "项目类型
    AND   PRPS~WERKS IN S_WERKS   "工厂
    AND   PRPS~ERNAM IN S_ERNAM   "创建者
    AND   PRPS~ERDAT IN S_ERDAT   "创建日期
    AND   PRPS~VERNA IN S_VERNR  "负责人
    AND   RESB~BDTER IN S_BDTER   "需求日期
    AND   RESB~RSNUM IN S_RSNUM   "单据编号
    AND   RESB~MATNR IN S_MATNR.  "物料

*-----------------------将八位的内标编号转换为12位---------------------*
  LOOP AT GT_PRPS_RESB INTO GS_PRPS_RESB.
    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        INPUT  = GS_PRPS_RESB-PSPHI   "合适的项目的当前编号
      IMPORTING
        OUTPUT = GS_PRPS_RESB-PSPHI1
*       PSELT  =
      .
    MODIFY GT_PRPS_RESB FROM GS_PRPS_RESB.
  ENDLOOP.
*---------------------proj------------------------------------------*
  IF GT_PRPS_RESB IS NOT INITIAL.
* --------打印日期----------------------------------
    SELECT
      PSPNR
      WERKS
      RSNUM
      RSPOS
      MATNR
      ZDY
      INTO CORRESPONDING FIELDS OF TABLE GT_WA3
      FROM ZZT_CHUKU_A
      FOR ALL ENTRIES IN GT_PRPS_RESB
      WHERE PSPNR = GT_PRPS_RESB-PSPNR
      AND   RSNUM = GT_PRPS_RESB-RSNUM
      AND   RSPOS = GT_PRPS_RESB-RSPOS
      AND   MATNR = GT_PRPS_RESB-MATNR.

    SORT GT_WA3 BY PSPNR RSNUM RSPOS MATNR.

*LOOP AT gt_prps_resb INTO gs_prps_resb.
*
*
*MODIFY gt_alv FROM gs_alv.
*
*ENDLOOP.









    APPEND LINES OF GT_PRPS_RESB TO LT_PRPS.
    SORT LT_PRPS BY PSPHI1.
    DELETE ADJACENT DUPLICATES FROM LT_PRPS COMPARING  PSPHI1.
    SELECT
        PROJ~ZKHBM  "客户
        PROJ~POST1  "项目名称
        PROJ~PWHIE   "货币
        PROJ~ZXMDZ   "项目地址
        PROJ~PSPID   "项目定义
        PROJ~PSPNR
        INTO  TABLE GT_PROJ
        FROM PROJ
        FOR ALL ENTRIES IN LT_PRPS
        WHERE PSPID = LT_PRPS-PSPHI1   "项目名称
        AND   PROJ~PSPNR  =  LT_PRPS-PSPHI
        AND   PROJ~ZKHBM IN S_ZKHBM .  "客户
    SORT GT_PROJ BY PSPID  PSPNR .

    FREE: LT_PRPS.
    APPEND LINES OF GT_PRPS_RESB TO LT_PRPS.
    SORT LT_PRPS BY PSPNR.
    DELETE ADJACENT DUPLICATES FROM LT_PRPS COMPARING  PSPNR.



    APPEND LINES OF GT_PROJ TO LT_PROJ.
    SORT LT_PROJ BY ZKHBM.
    DELETE ADJACENT DUPLICATES FROM LT_PROJ COMPARING  ZKHBM.
    SELECT
        KNA1~NAME1   "客户名称
        KNA1~KUNNR   "收货人
        INTO  TABLE GT_KNA1
        FROM KNA1
        FOR ALL ENTRIES IN GT_PRPS_RESB
        WHERE KNA1~KUNNR = GT_PRPS_RESB-ZKHBM
        .
    SORT GT_KNA1 BY KUNNR.
*&------通过函数读取WBS对应单价数据
    LOOP AT GT_PROJ INTO GS_PROJ.
      CLEAR: LV_PSPNR.
      FREE:LT_GPREIS.

*      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*        EXPORTING
*          input  = ls_prps-pspnr
*        IMPORTING
*          output = lv_pspnr.

      CALL FUNCTION 'CNECP_READ'
        EXPORTING
          I_PROJ_DEF = GS_PROJ-PSPID
          I_VERSION  = '100'
*         I_VSNMR    =
        IMPORTING
*         E_NETWORK_ECP       =
          E_WBS_ECP  = LT_GPREIS
          ET_MESG    = GT_MESG.
      APPEND LINES OF LT_GPREIS TO GT_GPREIS.
    ENDLOOP.
    SORT GT_GPREIS BY WBS_NAME.
  ENDIF.

*---------------------kna1&knvk------------------------------------------*
*  IF gt_proj IS NOT INITIAL.
*    APPEND LINES OF gt_proj TO lt_proj.
*    SORT lt_proj BY zkhbm.
*    DELETE ADJACENT DUPLICATES FROM lt_proj COMPARING  zkhbm.
*    SELECT
*        kna1~name1   "客户名称
*        kna1~kunnr   "收货人
*        INTO  TABLE gt_kna1
*        FROM kna1
*        FOR ALL ENTRIES IN gt_prps_resb
*        WHERE kna1~kunnr = gt_prps_resb-zkhbm
*        .
*    SORT gt_kna1 BY kunnr.

  IF GT_KNA1 IS NOT INITIAL.
    APPEND LINES OF GT_KNA1 TO LT_KNA1.
    SORT LT_KNA1 BY KUNNR.
    DELETE ADJACENT DUPLICATES FROM LT_KNA1 COMPARING  KUNNR.
    SELECT
      KNVK~NAME1   "收货人
      KNVK~PRSNR   "人员编号
      KNVK~KUNNR
      INTO  TABLE GT_KNVK
      FROM KNVK
      FOR ALL ENTRIES IN LT_KNA1
      WHERE KUNNR = LT_KNA1-KUNNR
      .
    SORT GT_KNVK BY NAME2.
  ENDIF.
*  ENDIF.
*---------------------adr2------------------------------------------*
  IF GT_KNVK IS NOT INITIAL.
    APPEND LINES OF GT_KNVK TO LT_KNVK.
    SORT LT_KNVK BY PRSNR.
    DELETE ADJACENT DUPLICATES FROM LT_KNVK COMPARING  PRSNR.
    SELECT
     ADR2~TEL_NUMBER    "联系电话
     ADR2~R3_USER      "标识符：电话是移动电话
    INTO TABLE GT_ADR2
    FROM ADR2
    FOR ALL ENTRIES IN  LT_KNVK
    WHERE ADR2~PERSNUMBER = LT_KNVK-PRSNR
      AND ADR2~R3_USER = 3
      .
    SORT GT_ADR2 BY PERSNUMBER .
  ENDIF.
*---------------------makt------------------------------------------*
  IF GT_PRPS_RESB IS NOT INITIAL.
    APPEND LINES OF GT_PRPS_RESB TO LT_PRPS.
    SORT LT_PRPS BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_PRPS COMPARING  MATNR.
    SELECT
      MAKT~MAKTX   "物料描述
      MAKT~MATNR   "物料号
      INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
      FROM MAKT
      FOR ALL ENTRIES IN LT_PRPS
      WHERE MATNR = LT_PRPS-MATNR   "物料号
      AND SPRAS = '1'                    "语言
      .
    FREE:LT_PRPS.
    SORT GT_MAKT BY MATNR.

  ENDIF.
*---------------------mara------------------------------------------*
  IF GT_PRPS_RESB IS NOT INITIAL.
    SELECT
    MARA~MATNR    "物料
    MARA~EXTWG   "品牌

    INTO CORRESPONDING FIELDS OF TABLE GT_MARA
    FROM MARA
    FOR ALL ENTRIES IN  GT_PRPS_RESB
    WHERE MATNR = GT_PRPS_RESB-MATNR.   "物料号.
    SORT GT_MARA BY MATNR.
  ENDIF.
  DATA : ZMEINS TYPE RESB-MEINS,
         ZBDMNG TYPE RESB-BDMNG,   "需求数量
         ZENMNG TYPE RESB-ENMNG.   "已发数量
  DATA : ZXQ_TOTAL TYPE RESB-BDMNG,
         ZYF_TOTAL TYPE RESB-ENMNG.
  ZMENGE = 0.
  ZXQ_TOTAL = 0.
  ZYF_TOTAL = 0.

  DATA : LS_RESB TYPE TY_PRPS_RESB.

  DELETE GT_PRPS_RESB WHERE BWART <> '281'.
  SORT GT_PRPS_RESB BY MATNR.
  LOOP AT GT_PRPS_RESB INTO GS_PRPS_RESB. "WHERE bwart = '281'.
*    zmeins = gs_prps_resb-meins.

    MOVE-CORRESPONDING GS_PRPS_RESB TO LS_RESB.
*    zbdmng = ls_resb-bdmng.
*    zenmng = ls_resb-enmng.
    GS_ALV-PSPNR = LS_RESB-PSPNR.  "WBS元素
    GS_ALV-WERKS = LS_RESB-WERKS.  "工厂
    GS_ALV-ERNAM = LS_RESB-ERNAM.  "创建者
    GS_ALV-ERDAT = LS_RESB-ERDAT.  "创建日期
    GS_ALV-VERNA = LS_RESB-VERNA.  "负责人
    GS_ALV-RSNUM = LS_RESB-RSNUM.  "预留编号
    GS_ALV-RSPOS = GS_PRPS_RESB-RSPOS.  "行项目

    GS_ALV-MATNR = LS_RESB-MATNR.  "物料号
    GS_ALV-MEINS = LS_RESB-MEINS.  "单位


*    gs_alv-enmng = ls_resb-enmng.  "已发数量
*
**发货数量=需求数量-已发数量
*    zmenge_a = gs_alv-bdmng - gs_alv-enmng. "发货数量
*    zmenge = zmenge + zmenge_a.
*    gs_alv-mengv13 = zmenge.


    READ TABLE GT_PROJ INTO GS_PROJ WITH KEY PSPID = GS_PRPS_RESB-PSPHI1 BINARY SEARCH.  "合适的项目的当前编号
    IF SY-SUBRC = 0 .
      GS_ALV-POST1 = GS_PROJ-POST1.  "项目名称
      GS_ALV-ZKHBM = GS_PROJ-ZKHBM.  "客户
      GS_ALV-PWHIE = GS_PROJ-PWHIE.  "货币
      GS_ALV-ZXMDZ = GS_PROJ-ZXMDZ.  "项目地址

    ENDIF.
    READ TABLE  GT_KNA1 INTO GS_KNA1 WITH KEY  KUNNR = GS_PRPS_RESB-ZKHBM BINARY SEARCH.   "客户
    IF SY-SUBRC = 0.
      GS_ALV-NAME1 = GS_KNA1-NAME1.  "客户名称
      READ TABLE  GT_KNVK INTO GS_KNVK WITH KEY  KUNNR = GS_KNA1-KUNNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GS_ALV-NAME2 = GS_KNVK-NAME2.  "收货人
      ENDIF.
    ENDIF.
    READ TABLE GT_MAKT INTO GS_MAKT WITH KEY MATNR = GS_PRPS_RESB-MATNR BINARY SEARCH.  "物料号
    IF SY-SUBRC = 0.
      GS_ALV-MAKTX = GS_MAKT-MAKTX.  "物料描述

    ENDIF.

    READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_PRPS_RESB-MATNR  BINARY SEARCH. "物料号
    IF SY-SUBRC = 0.
      GS_ALV-EXTWG = GS_MARA-EXTWG.  "品牌
    ENDIF.
    READ TABLE GT_ADR2 INTO GS_ADR2  WITH  KEY PERSNUMBER = GS_KNVK-PRSNR
                                               R3_USER = 3
                                               BINARY SEARCH.
    IF SY-SUBRC = 0 .
      GS_ALV-TEL_NUMBER = GS_ADR2-TEL_NUMBER.  "联系电话
    ENDIF.
*&------通过函数读取WBS对应单价数据
    CLEAR: LV_PSPNR.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = GS_ALV-PSPNR
      IMPORTING
        OUTPUT = LV_PSPNR.

    IF GT_GPREIS IS NOT INITIAL.
*      LOOP AT gt_gpreis INTO gs_gpreis where WBS_NAME = lv_pspnr. "
      READ TABLE GT_GPREIS INTO GS_GPREIS WITH KEY WBS_NAME = LV_PSPNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        FREE: LT_MATNR.
        LT_MATNR = GS_GPREIS-COST_LINES.
        SORT LT_MATNR BY MATNR.
        READ TABLE LT_MATNR INTO LS_MATNR WITH KEY MATNR = GS_PRPS_RESB-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_ALV-GPREIS = LS_MATNR-GPREIS.  "单价
        ENDIF.
      ENDIF.
*      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "去掉客户前面的0
      EXPORTING
        INPUT  = GS_ALV-ZKHBM  "展示客户的字段
      IMPORTING
        OUTPUT = GS_ALV-ZKHBM.

*金额=数量*单价
    GS_ALV-ZDMBTR = GS_ALV-MENGV13 * GS_ALV-GPREIS.
    READ TABLE GT_WA3 INTO GS_WA3 WITH KEY PSPNR = LS_RESB-PSPNR
                                           RSNUM = LS_RESB-RSNUM
                                           RSPOS = LS_RESB-RSPOS
                                           MATNR = LS_RESB-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_ALV-ZDY = GS_WA3-ZDY. "打印日期
    ENDIF.

*&--代码注释 BY HANDYBY 16.08.2017 21:19:09  BEGIN
*    AT NEW MATNR.
*      CLEAR : ZXQ_TOTAL,ZYF_TOTAL,ZMENGE.
*    ENDAT.

*    ZXQ_TOTAL = LS_RESB-BDMNG + ZXQ_TOTAL.
*    ZYF_TOTAL = LS_RESB-ENMNG + ZYF_TOTAL.

*    AT END OF MATNR.
*      GS_ALV-BDMNG = ZXQ_TOTAL.  "需求数量
*      GS_ALV-ENMNG = ZYF_TOTAL.
*      GS_ALV-MENGV13 = GS_ALV-BDMNG - GS_ALV-ENMNG.
*      APPEND GS_ALV TO GT_ALV.
*    ENDAT.
*&--代码注释 BY HANDYBY 16.08.2017 21:19:09  END
*&--代码添加 BY HANDYBY 16.08.2017 21:20:19  BEGIN
    CLEAR : ZXQ_TOTAL,ZYF_TOTAL,ZMENGE.

    ZXQ_TOTAL = LS_RESB-BDMNG .
    ZYF_TOTAL = LS_RESB-ENMNG .

    GS_ALV-BDMNG = ZXQ_TOTAL.  "需求数量
    GS_ALV-ENMNG = ZYF_TOTAL.
    GS_ALV-MENGV13 = GS_ALV-BDMNG - GS_ALV-ENMNG.
    APPEND GS_ALV TO GT_ALV.
*&--代码添加 BY HANDYBY 16.08.2017 21:20:19  END

*    zmenge_a = ZXQ_TOTAL - ZYF_TOTAL. "发货数量
*    zmenge = zmenge + zmenge_a.

*    CLEAR : ZXQ_TOTAL,zyf_total.

    CLEAR GS_ALV.


*    CLEAR gs_prps_resb.
*    CLEAR gs_kna1.
*    CLEAR gs_knvk.
*    CLEAR gs_makt.
*    CLEAR gs_mara.
*    CLEAR gs_adr2.

  ENDLOOP.

  SORT GT_ALV BY RSNUM MATNR.
*&-------选择屏幕物料筛选---------------------&*
  DELETE GT_ALV WHERE MATNR NOT IN S_MATNR.


*&--代码添加 BY HANDYBY 11.08.2017 16:21:47  BEGIN
  IF GT_ALV IS INITIAL.
    MESSAGE 'NO DATA!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
* 根据选择屏幕勾选的值判断是要取已经打印过的数据还是未打印的数据
  SELECT *
    INTO TABLE GT_ZMMT040
    FROM ZMMT040 .
  SORT GT_ZMMT040 BY PSPNR RSNUM RSPOS MATNR .

  IF P1 = 'X' AND P2 IS INITIAL .
    LOOP AT GT_ALV INTO GS_ALV .
      READ TABLE GT_ZMMT040 WITH KEY PSPNR = GS_ALV-PSPNR
                                     RSNUM = GS_ALV-RSNUM
                                     RSPOS = GS_ALV-RSPOS
                                     MATNR = GS_ALV-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC NE 0 .
        DELETE TABLE GT_ALV FROM GS_ALV .
      ENDIF.
    ENDLOOP.
  ELSEIF P1 IS INITIAL AND P2 = 'X' .
    LOOP AT GT_ALV INTO GS_ALV .
      READ TABLE GT_ZMMT040 WITH KEY PSPNR = GS_ALV-PSPNR
                                     RSNUM = GS_ALV-RSNUM
                                     RSPOS = GS_ALV-RSPOS
                                     MATNR = GS_ALV-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        DELETE TABLE GT_ALV FROM GS_ALV .
      ENDIF.
    ENDLOOP.
  ELSEIF P1 = 'X' AND P2 = 'X' .

  ENDIF.

  IF GT_ALV IS INITIAL .
    MESSAGE 'NO DATA!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
*&--代码添加 BY HANDYBY 11.08.2017 16:21:47  END


*  APPEND LINES OF GT_ALV TO LT_ALV.



*  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING MATNR.
ENDFORM.                    " FRM_SELDATA
*&---------------------------------------------------------------------*
*&      Form  FRM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_WRITE .
  PERFORM  LAYOUT.            "控制ALV 的显示信息
  PERFORM  FIELDCT_BUDDING.   "输出属性
  PERFORM  LISTADO.           "展示
ENDFORM.                    " FRM_WRITE
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LAYOUT .
  GS_LAYOUT-BOX_FIELDNAME = 'BOX'. "设置多行选择列，BOX必须为内表的一列,为一个字符长度
  GS_LAYOUT-ZEBRA = 'X'.              "设置Grid的行颜色变换显示
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.  "自适应宽度
  GS_LAYOUT-INFO_FIELDNAME = 'COLOR'."行颜色
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FIELDCT_BUDDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCT_BUDDING .
  DEFINE CATALOG.  "定义宏
    CLEAR : GS_FIELDCAT.   "清空
   &1 = &1 + 1 .    "行的序号的递加
    GS_FIELDCAT-COL_POS          = &1.  "字段定义
    GS_FIELDCAT-FIELDNAME        = &2. "字段名字
    GS_FIELDCAT-SELTEXT_M        = &3.  "字段描述
    GS_FIELDCAT-OUTPUTLEN              = &4.  "字段是否为关键字（x或空）关键的字名固定住
    GS_FIELDCAT-EDIT             = &5.   "表示字段内容是否可编辑
    GS_FIELDCAT-DECIMALS_OUT     = '0'.   "去掉小数点后边0
*    GS_FIELDCAT-OUTPUTLEN     = &7.
    APPEND GS_FIELDCAT TO  GT_FIELDCAT. "更新定义的
  END-OF-DEFINITION.
  DATA:L_COLPOS TYPE LVC_S_FCAT-COL_POS VALUE 0.            "初始值为0
  CATALOG:
  L_COLPOS  'PSPNR' 'WBS元素' ''  ''    ,
  L_COLPOS  'POST1' '项目名称' ''  ''  ,
  L_COLPOS  'ZKHBM' '客户' ''  ''  ,
  L_COLPOS  'NAME1' '客户名称' ''  ''  ,
  L_COLPOS  'RSNUM' '预留编号' ''  ''  ,
  L_COLPOS  'RSPOS' '行项目' ''  ''  ,
  L_COLPOS  'MATNR' '物料编码' '40' ''  ,
  L_COLPOS  'MAKTX' '物料描述' '' ''  ,
  L_COLPOS  'EXTWG' '品牌' ''  ''  ,
  L_COLPOS  'MEINS' '单位' '' ''  ,
  L_COLPOS  'GPREIS' '单价' '' '',
  L_COLPOS  'PWHIE' '货币' '' '' ,
  L_COLPOS  'BDMNG' '需求数量' '' ''  ,
  L_COLPOS  'ENMNG' '已发数量' '' ''  ,
  L_COLPOS  'NAME2' '收货人' ''  'X',
  L_COLPOS  'TEL_NUMBER' '联系电话' ''   'X',
  L_COLPOS  'MENGV13' '发货数量' ''   'X',
  L_COLPOS  'WERKS' '工厂' '' ''  ,
  L_COLPOS  'ERNAM' '创建者' ''   '',
  L_COLPOS  'ERDAT' '创建日期' ''   '',
  L_COLPOS  'VERNA' '负责人' ''   '',
  L_COLPOS  'ZXMDZ' '项目地址' ''  '',
  L_COLPOS  'ZDY' '打印日期' ''  ''.
ENDFORM.                    " FIELDCT_BUDDING
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       定义状态条，包括菜单，工具条按钮，系统按钮等
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS '9002' .
ENDFORM. "SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  LISTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LISTADO .
  G_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID            "当前程序名
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'   "设置ALV的自定义按钮
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND' "自定义按钮响应事件
      IS_LAYOUT                = GS_LAYOUT       "输出样式
      IT_FIELDCAT              = GT_FIELDCAT[]   "字段定义描述表
*     I_CALLBACK_TOP_OF_PAGE   = 'F_TOP_OF_PAGE'
      I_SAVE                   = 'A'
*     IT_SORT                  = LT_SORT   "排序小计
    TABLES
      T_OUTTAB                 = GT_ALV       "内表输出名称
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " LISTADO
*&--------------------------------------------------------------------*
*&      Form  user_command
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                    RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: GV_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA: L_NUM TYPE I.
  DATA: LT_ALV TYPE TABLE OF TY_ALV.
  "刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING     "导入
      E_GRID = GV_GRID.
  CALL METHOD GV_GRID->CHECK_CHANGED_DATA.
  RS_SELFIELD-REFRESH = 'X'.     "刷新ALV的数据
  CASE R_UCOMM.      "PAI发出的功能代码
    WHEN 'PRINT'.
      LOOP AT GT_ALV INTO GS_ALV WHERE BOX = 'X'.
        GS_ALV-ZDY = SY-DATUM.
        MODIFY GT_ALV FROM GS_ALV.
        GS_WA3-PSPNR = GS_ALV-PSPNR.
        GS_WA3-WERKS = GS_ALV-WERKS.
        GS_WA3-RSNUM = GS_ALV-RSNUM.
        GS_WA3-RSPOS = GS_ALV-RSPOS.
        GS_WA3-MATNR = GS_ALV-MATNR.
        GS_WA3-ZDY = GS_ALV-ZDY.
        APPEND GS_WA3 TO GT_WA3.
        MODIFY ZZT_CHUKU_A FROM GS_WA3.
      ENDLOOP.
      APPEND LINES OF GT_ALV TO LT_ALV.
      DELETE LT_ALV WHERE BOX NE 'X'.
      SORT LT_ALV BY RSNUM.
      GT_ALV3 = LT_ALV.
      DELETE ADJACENT DUPLICATES FROM LT_ALV COMPARING RSNUM.
      DESCRIBE TABLE LT_ALV LINES L_NUM.
      GT_ALV2 = LT_ALV.
*      LOOP AT  gt_alv INTO gs_alv WHERE box = 'X'.
*
*          l_num = l_num + 1.
*      ENDLOOP.


      IF  L_NUM < 1.
        MESSAGE '请选择一个项目打印' TYPE 'E'.
      ENDIF.

      LOOP AT GT_ALV2 INTO GS_ALV2 WHERE BOX = 'X'.
        ZXMDZ_S = GS_ALV2-ZXMDZ.    "到货地址
        POST1_S = GS_ALV2-POST1.    "项目名称 使用单位
        NAME2_S = GS_ALV2-NAME2.  "收货人
        TEL = GS_ALV2-TEL_NUMBER.  "联系电话
      ENDLOOP.
      CALL SCREEN 9000 STARTING AT 35 3.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  RS_SELFIELD-REFRESH = 'X'.
ENDFORM. "user_command
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STATUS'.
*加记录 运输方式
  LVVA1-KEY = '汽运'.
  APPEND LVVA1 TO VVA1.
  LVVA1-KEY = '航空'.
  APPEND LVVA1 TO VVA1.
  LVVA1-KEY = '火车'.
  APPEND LVVA1 TO VVA1.
  LVVA1-KEY = '快递'.
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

*加记录 运费承担
  LVVA2-KEY = '公司承担'.
  APPEND LVVA2 TO VVA2.
  LVVA2-KEY = '客户承担'.
  APPEND LVVA2 TO VVA2.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'LISTBOX2'
      VALUES          = VVA2
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CLEAR:VVA1, VVA2.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'PRINT'.
      PERFORM SETSMART.

*    WHEN 'SAVEPDF'.
** ---------------  用户选择保存路径  -------------
*      PERFORM FRM_GET_FULLPATH CHANGING L_FULLPATH L_PATH L_NAME.
** 路径为空则退出
*      IF L_FULLPATH IS INITIAL.
*        MESSAGE TEXT-001 TYPE 'S'.
*        RETURN.
*      ENDIF.
*      PERFORM SAVEPDF.
  ENDCASE.
ENDMODULE.
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

* 初始名称(输出的文件名称)
  L_INIT_FNAME = 'TEST.PDF'.

* 获取桌面路径
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

* 用户选择名称、路径
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
*     window_title         = '指定保存文件名'
      DEFAULT_EXTENSION    = '.PDF'
      DEFAULT_FILE_NAME    = L_INIT_FNAME
      FILE_FILTER          = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
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
*&      Form  SAVEPDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVEPDF .
* 总的数目
  DATA: DATA_NUM TYPE I VALUE 0.
  "获取打印名称
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZSF_ZMM040'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  "   打印设置
  CONTROL_PARAMETERS-GETOTF = 'X'.
*CONTROL_PARAMETERS-NO_OPEN   = 'X'.
*CONTROL_PARAMETERS-NO_CLOSE  = 'X'.
**  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
*
*
*OUTPUT-TDDEST = 'LP01'.
**  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
*  OUTPUT-RQPOSNAME = ''.
*  OUTPUT-TDDATASET = ''.
*  OUTPUT-TDSUFFIX1 = ''.
*  OUTPUT-TDSUFFIX2 = ''.
*  OUTPUT-TDIMMED   = 'X'.
*  OUTPUT-TDDELETE  = 'X'.
*
*  CALL FUNCTION 'SSF_OPEN'
*  EXPORTING
*    CONTROL_PARAMETERS = CONTROL_PARAMETERS
*    OUTPUT_OPTIONS     = OUTPUT
**    IMPORTING
**     JOB_OUTPUT_OPTIONS = OPTION
*  EXCEPTIONS
*    FORMATTING_ERROR   = 1
*    INTERNAL_ERROR     = 2
*    SEND_ERROR         = 3
*    USER_CANCELED      = 4
*    OTHERS             = 5.
* IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

*将数据导入像结构的内表里，循环到smartform
  GT_ALV1 = GT_ALV.
*  data : zmenge(13) TYPE c.
*  zmenge = 0.
* data : zmaktx TYPE makt-maktx,
*       zmeins TYPE resb-meins,
*       zmengv13_a(13),
*       zgpreis type ckis-gpreis.
  LOOP AT  GT_ALV3 INTO GS_ALV3 .  "alv2表示从ALV中选择的
    GS_ALV4-VERNA = GS_ALV3-VERNA.  "负责人
    GS_ALV4-NAME1 = GS_ALV3-NAME1.  "客户名称
    GS_ALV4-RSNUM = GS_ALV3-RSNUM.  "预留编号
    AT NEW RSNUM.

      DATA_NUM = 0 .
      ZZS_TAB-ZXMDZ_S = ZXMDZ_S.      "到货地址
      ZZS_TAB-NAME2_S = NAME2_S.      "收货人
      ZZS_TAB-TEL     = TEL .         "联系电话
      ZZS_TAB-POST1_S = POST1_S.      "项目名称 使用单位
      ZZS_TAB-LISTBOX1 = LISTBOX1.   "运输方式
      ZZS_TAB-LISTBOX2 = LISTBOX2.   "运费承担
      ZZS_TAB-VERNA = GS_ALV4-VERNA.  "负责人
      ZZS_TAB-NAME1 = GS_ALV4-NAME1.  "客户名称
      ZZS_TAB-RSNUM = GS_ALV4-RSNUM.  "预留编号
    ENDAT.

    CLEAR GS_ALV4.
    AT END OF RSNUM.

      Z_SUM = 0.
* DELETE ADJACENT DUPLICATES FROM gt_alv3 COMPARING ALL FIELDS.
      LOOP  AT GT_ALV3 INTO GS_ALV3.
        DATA_NUM = DATA_NUM + 1."数据条目  总数据条数
        GS_TAB-MATNR = GS_ALV3-MATNR.  "物料号
        GS_TAB-MAKTX = GS_ALV3-MAKTX.  "物料描述
        GS_TAB-MEINS = GS_ALV3-MEINS.  "单位
*      zmenge = zmenge + gs_alv3-mengv13.
        GS_TAB-ZMENGV13 = GS_ALV3-MENGV13.  "发货数量
        GS_TAB-GPREIS = GS_ALV3-GPREIS.      "单价
*金额=数量*单价
*    gs_alv-zdmbtr = gs_alv-mengv13 * gs_alv-gpreis.
        GS_TAB-ZDMBTR =  GS_TAB-ZMENGV13 * GS_TAB-GPREIS.     "金额
        Z_SUM = Z_SUM + GS_TAB-ZDMBTR.
        APPEND GS_TAB TO GT_TAB.
        CLEAR : GS_TAB.
      ENDLOOP.

*------------------------  空行 --------------------------
      DATA_NUM = DATA_NUM MOD ZPAGE_NUM."最后页面行数
      IF DATA_NUM <> 0.
        DATA_NUM = ZPAGE_NUM - DATA_NUM."空行数
        CLEAR GS_TAB.
        DO DATA_NUM TIMES.
          APPEND GS_TAB TO GT_TAB..
        ENDDO.
      ENDIF.

*调用Smartforms的Function Module打印
      CALL FUNCTION FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = OUTPUT
*         user_settings      = 'X'
          TAB                = ZZS_TAB    "结构导入到smartform表格接口里的导入type
          ZPAGE_NUM          = ZPAGE_NUM
          G_SUM_DMBTRZ       = Z_SUM
        IMPORTING
          JOB_OUTPUT_INFO    = OPTION
        TABLES
          GT_TAB             = GT_TAB       "结构导入到smartform表格接口里的表like
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CLEAR Z_SUM.
*    CLEAR gt_tab.   " 清除重复数据
    ENDAT.
  ENDLOOP.
  CLEAR GT_TAB.   " 清除重复数据

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
*     max_linewidth         = 132
*     ARCHIVE_INDEX         = ' '
*     COPYNUMBER            = 0
*     ASCII_BIDI_VIS2LOG    = ' '
    IMPORTING
      BIN_FILESIZE          = G_BINFILESIZE
    TABLES
      OTF                   = OPTION-OTFDATA
      LINES                 = GT_PDFTAB
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      ERR_BAD_OTF           = 4
      OTHERS                = 5.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      BIN_FILESIZE = G_BINFILESIZE
      FILENAME     = L_FULLPATH
      FILETYPE     = 'BIN'
*  IMPORTING
*     filelength   = file_size
    TABLES
      DATA_TAB     = GT_PDFTAB
*     FIELDNAMES   =
*  EXCEPTIONS
*     file_write_error        = 1
*     no_batch     = 2
*     gui_refuse_filetransfer = 3
*     invalid_type = 4
*     no_authority = 5
*     unknown_error           = 6
*     header_not_allowed      = 7
*     separator_not_allowed   = 8
*     filesize_not_allowed    = 9
*     header_too_long         = 10
*     dp_error_create         = 11
*     dp_error_send           = 12
*     dp_error_write          = 13
*     unknown_dp_error        = 14
*     access_denied           = 15
*     dp_out_of_memory        = 16
*     disk_full    = 17
*     dp_timeout   = 18
*     file_not_found          = 19
*     dataprovider_exception  = 20
*     control_flush_error     = 21
*     OTHERS       = 22.
    .
*  CLEAR GT_TAB.
* ENDLOOP.

  "#  关闭打印机设置
*  CALL FUNCTION 'SSF_CLOSE'
*    IMPORTING
*      JOB_OUTPUT_INFO  = LW_SSFCRESCL
*    EXCEPTIONS
*      FORMATTING_ERROR = 1
*      INTERNAL_ERROR   = 2
*      SEND_ERROR       = 3
*      OTHERS           = 4.
*  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  CLEAR FM_NAME.
  CLEAR OUTPUT.
  CLEAR CONTROL_PARAMETERS.
  CLEAR LW_SSFCRESCL.
  CLEAR OPTION.
  CLEAR GT_PDFTAB.
  CLEAR G_BINFILESIZE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SETSMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETSMART .

*&--代码添加 BY HANDYBY 16.08.2017 15:51:20  BEGIN
  DATA: BEGIN OF LS_PROJ ,
          POST1 TYPE PROJ-POST1,
          RSNUM TYPE RESB-RSNUM,
        END OF LS_PROJ .
  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  DATA: BEGIN OF LS_M ,
          POST1 TYPE PROJ-POST1,
          RSNUM TYPE RESB-RSNUM,
          MATNR TYPE RESB-MATNR,
        END OF LS_M .
  DATA LT_M LIKE TABLE OF LS_M .
*&--代码添加 BY HANDYBY 16.08.2017 15:51:20  END

* 总的数目
  DATA: DATA_NUM TYPE I VALUE 0.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZSF_ZMM040'   "SMARTFORMS NAME
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*
*
*打印设置
  CONTROL_PARAMETERS-NO_OPEN   = 'X'.
  CONTROL_PARAMETERS-NO_CLOSE  = 'X'.
*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
*  control_parameters-getotf  = 'X'.   "pdf相关
  OUTPUT-TDDEST = 'LP01'.           "打印设备名
*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
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
*    IMPORTING
*     JOB_OUTPUT_OPTIONS = OPTION
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

*&--代码添加 BY HANDYBY 16.08.2017 15:54:26  BEGIN
  LOOP AT GT_ALV3 INTO GS_ALV3 .
    LS_PROJ-POST1 = GS_ALV3-POST1 .
    LS_PROJ-RSNUM = GS_ALV3-RSNUM .
    APPEND LS_PROJ TO LT_PROJ .
    LS_M-POST1 = GS_ALV3-POST1 .
    LS_M-RSNUM = GS_ALV3-RSNUM .
    LS_M-MATNR = GS_ALV3-MATNR .
    APPEND LS_M TO LT_M .
    CLEAR LS_M .
    CLEAR LS_PROJ .
    CLEAR GS_ALV3 .
  ENDLOOP.
  SORT LT_PROJ BY POST1 RSNUM .
  DELETE ADJACENT DUPLICATES FROM LT_PROJ COMPARING POST1 RSNUM .
  SORT LT_M BY POST1 RSNUM MATNR .
  DELETE ADJACENT DUPLICATES FROM LT_M COMPARING POST1 RSNUM MATNR .
  SORT GT_ALV3 BY POST1 RSNUM MATNR .
*&--代码添加 BY HANDYBY 16.08.2017 15:54:26  END

*将数据导入像结构的内表里，循环到smartform
  GT_ALV1 = GT_ALV.
*  zmenge = 0.
*data : zmaktx TYPE makt-maktx,
*       zmeins TYPE resb-meins,
*       zmengv13_a(13),
*       zgpreis type ckis-gpreis.

*&--代码注释 BY HANDYBY 16.08.2017 16:06:26  BEGIN
*  LOOP AT  GT_ALV3 INTO GS_ALV3  .   "alv2表示从ALV中选择的
*
*    GS_ALV4-VERNA = GS_ALV3-VERNA.  "负责人
*    GS_ALV4-NAME1 = GS_ALV3-NAME1.  "客户名称
*    GS_ALV4-RSNUM = GS_ALV3-RSNUM.  "预留编号
*
*    AT NEW RSNUM.
*      DATA_NUM = 0 .
*      ZZS_TAB-ZXMDZ_S = ZXMDZ_S.      "到货地址
*      ZZS_TAB-NAME2_S = NAME2_S.      "收货人
*      ZZS_TAB-TEL     = TEL .         "联系电话
*      ZZS_TAB-POST1_S = POST1_S.      "项目名称 使用单位
*      ZZS_TAB-LISTBOX1 = LISTBOX1.   "运输方式
*      ZZS_TAB-LISTBOX2 = LISTBOX2.   "运费承担
*      ZZS_TAB-VERNA = GS_ALV4-VERNA.  "负责人
*      ZZS_TAB-NAME1 = GS_ALV4-NAME1.  "客户名称
*      ZZS_TAB-RSNUM = GS_ALV4-RSNUM.  "预留编号
*    ENDAT.
*    CLEAR GS_ALV4.
*
*    AT END OF RSNUM.
*      Z_SUM = 0.
*
*      LOOP  AT GT_ALV3 INTO GS_ALV3.
*        DATA_NUM = DATA_NUM + 1."数据条目  总数据条数
**      zmaktx = gs_alv3-maktx.
**      zmeins = gs_alv3-meins.
**      zmengv13_a = gs_alv3-mengv13.
**      zgpreis = gs_alv3-gpreis.
*        GS_TAB-MATNR = GS_ALV3-MATNR.  "物料号
*        GS_TAB-MAKTX = GS_ALV3-MAKTX.  "物料描述
*        GS_TAB-MEINS = GS_ALV3-MEINS.  "单位
**      zmenge = zmenge + gs_alv3-mengv13.
*        GS_TAB-ZMENGV13 = GS_ALV3-MENGV13.  "发货数量
*        GS_TAB-GPREIS = GS_ALV3-GPREIS.      "单价
*        GS_TAB-ZDMBTR =  GS_TAB-ZMENGV13 *  GS_TAB-GPREIS.   "金额
**      gs_tab-zdmbtr =  gs_alv3-zdmbtr.    "金额
*        Z_SUM = Z_SUM + GS_TAB-ZDMBTR.
*        APPEND GS_TAB TO GT_TAB.
*        CLEAR : GS_TAB.
*      ENDLOOP.
*
*
*      DELETE ADJACENT DUPLICATES FROM GT_TAB COMPARING MATNR.
**------------------------  空行 --------------------------
*      DATA_NUM = DATA_NUM MOD ZPAGE_NUM."最后页面行数
*      IF DATA_NUM <> 0.
*        DATA_NUM = ZPAGE_NUM - DATA_NUM."空行数
*        CLEAR GS_TAB.
*        DO DATA_NUM TIMES.
*          APPEND GS_TAB TO GT_TAB.
*        ENDDO.
*      ENDIF.
*
**调用Smartforms的Function Module打印
*      CALL FUNCTION FM_NAME
*        EXPORTING
*          CONTROL_PARAMETERS = CONTROL_PARAMETERS
*          OUTPUT_OPTIONS     = OUTPUT
*          USER_SETTINGS      = 'X'
*          TAB                = ZZS_TAB    "结构导入到smartform表格接口里的导入type
*          ZPAGE_NUM          = ZPAGE_NUM
*          G_SUM_DMBTRZ       = Z_SUM
*        TABLES
*          GT_TAB             = GT_TAB       "结构导入到smartform表格接口里的表like
*        EXCEPTIONS
*          FORMATTING_ERROR   = 1
*          INTERNAL_ERROR     = 2
*          SEND_ERROR         = 3
*          USER_CANCELED      = 4
*          OTHERS             = 5.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      CLEAR Z_SUM.
**    CLEAR gt_tab.   " 清除重复数据
*    ENDAT.
*  ENDLOOP.
*
*&--代码注释 BY HANDYBY 16.08.2017 16:06:26  END



*&--代码添加 BY HANDYBY 16.08.2017 16:06:45  BEGIN
  LOOP AT LT_PROJ INTO LS_PROJ .

*    GS_ALV4-VERNA = GS_ALV3-VERNA.  "负责人
*    GS_ALV4-NAME1 = GS_ALV3-NAME1.  "客户名称
*    GS_ALV4-RSNUM = GS_ALV3-RSNUM.  "预留编号

*    AT NEW RSNUM.
    DATA_NUM = 0 .
    ZZS_TAB-ZXMDZ_S = ZXMDZ_S.      "到货地址
    ZZS_TAB-NAME2_S = NAME2_S.      "收货人
    ZZS_TAB-TEL     = TEL .         "联系电话
    ZZS_TAB-POST1_S = LS_PROJ-POST1.      "项目名称 使用单位
    ZZS_TAB-LISTBOX1 = LISTBOX1.   "运输方式
    ZZS_TAB-LISTBOX2 = LISTBOX2.   "运费承担
*    ENDAT.
    CLEAR GS_ALV4.

    Z_SUM = 0.

    READ TABLE LT_M WITH KEY POST1 = LS_PROJ-POST1
                             RSNUM = LS_PROJ-RSNUM
*                            MATNR = LS_M-MATNR
                             BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP  AT LT_M INTO LS_M FROM SY-TABIX .
        IF LS_PROJ-POST1 = LS_M-POST1 AND
            LS_PROJ-RSNUM = LS_M-RSNUM .

          READ TABLE GT_ALV3 WITH KEY POST1 = LS_M-POST1
                                      RSNUM = LS_M-RSNUM
                                      MATNR = LS_M-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT GT_ALV3 INTO GS_ALV3 FROM SY-TABIX .
              IF GS_ALV3-POST1 = LS_M-POST1 AND
                    GS_ALV3-RSNUM = LS_M-RSNUM AND
                    GS_ALV3-MATNR = LS_M-MATNR .

                ZZS_TAB-VERNA = GS_ALV3-VERNA.  "负责人
                ZZS_TAB-NAME1 = GS_ALV3-NAME1.  "客户名称
                ZZS_TAB-RSNUM = GS_ALV3-RSNUM.  "预留编号
*      zmaktx = gs_alv3-maktx.
*      zmeins = gs_alv3-meins.
*      zmengv13_a = gs_alv3-mengv13.
*      zgpreis = gs_alv3-gpreis.
                GS_TAB-ZKHBM = GS_ALV3-ZKHBM .  " 客户编号
                GS_TAB-EXTWG = GS_ALV3-EXTWG .  " 品牌
*                GS_TAB-VERNA = GS_ALV3-VERNA .  " 负责人
*                GS_TAB-NAME1 = GS_ALV3-NAME1 .  " 客户名称
*                GS_TAB-LISTBOX1 = GS_ALV3-LISTBOX1 .  " 客户名称
*                GS_TAB-LISTBOX2 = GS_ALV3-LISTBOX2 .  " 客户名称
*                GS_TAB-ZXMDZ_S = GS_ALV3-ZXMDZ_S .  " 客户名称
*                GS_TAB-NAME2_S = GS_ALV3-NAME2_S .  " 客户名称
*                GS_TAB-TEL = GS_ALV3-TEL .  " 客户名称
*                GS_TAB-POST1_S = GS_ALV3-POST1_S .  " 客户名称

                GS_TAB-MATNR = GS_ALV3-MATNR.  "物料号
                GS_TAB-MAKTX = GS_ALV3-MAKTX.  "物料描述
                GS_TAB-MEINS = GS_ALV3-MEINS.  "单位
*      zmenge = zmenge + gs_alv3-mengv13.
                GS_TAB-ZMENGV13 = GS_TAB-ZMENGV13 + GS_ALV3-MENGV13.  "发货数量
                GS_TAB-GPREIS = GS_ALV3-GPREIS.      "单价
*      gs_tab-zdmbtr =  gs_alv3-zdmbtr.    "金额

                GS_ZMMT040-PSPNR = GS_ALV3-PSPNR .
                GS_ZMMT040-RSNUM = GS_ALV3-RSNUM .
                GS_ZMMT040-RSPOS = GS_ALV3-RSPOS .
                GS_ZMMT040-MATNR = GS_ALV3-MATNR .
                APPEND GS_ZMMT040 TO GT_ZMMT040 .
                CLEAR GS_ZMMT040 .

                CLEAR GS_ALV3 .
              ELSE .
                CLEAR GS_ALV3 .
                EXIT .
              ENDIF.
            ENDLOOP.
          ENDIF.

          " 打印行
          DATA_NUM = DATA_NUM + 1."数据条目  总数据条数
          GS_TAB-RSNUM = LS_M-RSNUM . " 预留号
          GS_TAB-ZDMBTR =  GS_TAB-ZMENGV13 *  GS_TAB-GPREIS.   "金额
          Z_SUM = Z_SUM + GS_TAB-ZDMBTR.

          APPEND GS_TAB TO GT_TAB.
          CLEAR : GS_TAB.

          CLEAR LS_M .
        ELSE .
          CLEAR LS_M .
          EXIT .
        ENDIF.
      ENDLOOP.
    ENDIF.

*    DELETE ADJACENT DUPLICATES FROM GT_TAB COMPARING MATNR.
*------------------------  空行 --------------------------
    DATA_NUM = DATA_NUM MOD ZPAGE_NUM."最后页面行数
    IF DATA_NUM <> 0.
      DATA_NUM = ZPAGE_NUM - DATA_NUM."空行数
      CLEAR GS_TAB.
      DO DATA_NUM TIMES.
        APPEND GS_TAB TO GT_TAB.
      ENDDO.
    ENDIF.

*调用Smartforms的Function Module打印
    CALL FUNCTION FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL_PARAMETERS
        OUTPUT_OPTIONS     = OUTPUT
        USER_SETTINGS      = 'X'
        TAB                = ZZS_TAB    "结构导入到smartform表格接口里的导入type
        ZPAGE_NUM          = ZPAGE_NUM
        G_SUM_DMBTRZ       = Z_SUM
      TABLES
        GT_TAB             = GT_TAB       "结构导入到smartform表格接口里的表like
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    REFRESH GT_TAB .
    CLEAR Z_SUM.
*    CLEAR gt_tab.   " 清除重复数据

  ENDLOOP.
*
*&--代码添加 BY HANDYBY 16.08.2017 16:06:45  END

  CLEAR GT_TAB.   " 清除重复数据
*  CLEAR gt_alv.
  "# 关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  BEGIN
* 塞数据进自建表
  MODIFY ZMMT040 FROM TABLE GT_ZMMT040 .
  IF SY-SUBRC = 0 .
    COMMIT WORK AND WAIT .
  ELSE .
    ROLLBACK WORK .
  ENDIF.

  REFRESH GT_ZMMT040 .
*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  END

  CLEAR FM_NAME.
  CLEAR OUTPUT.
  CLEAR CONTROL_PARAMETERS.
  CLEAR LW_SSFCRESCL.
  CLEAR OPTION.
ENDFORM. " SETSMART
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_VAR .
  IF P1 IS INITIAL AND P2 IS INITIAL .
    MESSAGE '复选框至少必须勾选一个!' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF.
ENDFORM.
