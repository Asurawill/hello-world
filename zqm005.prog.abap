REPORT ZQM005.
*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160815
*& Request       :
*& Descriptions  : 产成品检验报告
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
** 修改日期   开发人员  请求号        描述
" 20170316   IT02     ED1K905299    追加查询半成品生产订单前7位判断为销售订单
*&
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES:AUFK,AFPO,AFKO,MARA .
************************************************************************
* Type Declaration
************************************************************************
DATA :S_COUNT TYPE I. "行数
DATA: S_PAGE TYPE I VALUE 6. "每页记录个数
DATA:LS_ZZS_04 LIKE ZJLXCB_BD.
DATA: GT_TAB LIKE TABLE OF ZJLXCB_BD.
DATA:LS_ZZS_04_10 LIKE  ZJLXCB_H.
DATA: L_FM_NAME    TYPE RS38L_FNAM,
      OUTPUT       TYPE SSFCOMPOP,
      LW_SSFCRESCL TYPE SSFCRESCL.


TYPES:BEGIN OF TY_DATA,
        ZSEL,
        KDAUF  TYPE AFPO-KDAUF,   "销售订单号
        AUFNR  TYPE AFPO-AUFNR ,   "订单号
        OBJNR  TYPE AUFK-OBJNR,    "对象号
        AUART  TYPE  AUFK-AUART,     "订单类型
        MATNR  TYPE AFPO-MATNR,         "抬头物料号
        MAKTX  TYPE MAKT-MAKTX,         "物料描述
        KTEXT  TYPE AUFK-KTEXT,    "描述
        DDZT   TYPE STRING,     "订单状态
        MATKL  TYPE MARA-MATKL,      "物料组
        PSMNG  TYPE AFPO-PSMNG,      "订单数量
        BCRK   TYPE AFPO-PSMNG,      "本次入库数量
        WEMNG  TYPE AFPO-WEMNG,       "已入库数量
        "   ljrk   TYPE bdmng,       "累计入库数量
        CHARG  TYPE AFPO-CHARG,   "批次
        AMEIN  TYPE AFPO-AMEIN,   "单位
        LGORT  TYPE AFPO-LGORT,   "数量
*        kdauf  TYPE afpo-kdauf,   "销售订单号
        XMMC   TYPE STRING,       "项目名称
        ZJTLTS TYPE BDMNG,       "组件投料套数
        RSNUM  TYPE AFKO-RSNUM,   "预留相关需求
        IGMNG  TYPE AFKO-IGMNG,   "报工数量
        GAMNG  TYPE AFKO-GAMNG,    "订单数量
        XS     TYPE I,        "箱数
        BZ     TYPE STRING,       "备注
      END OF TY_DATA.

TYPES:BEGIN OF TY_DATA1,
        ZSEL,
        KDAUF  TYPE AFPO-KDAUF,   "销售订单号
        AUFNR  TYPE AFPO-AUFNR ,   "订单号
        OBJNR  TYPE AUFK-OBJNR,    "对象号
        AUART  TYPE  AUFK-AUART,     "订单类型
        MATNR  TYPE AFPO-MATNR,         "抬头物料号
        MAKTX  TYPE MAKT-MAKTX,         "物料描述
        KTEXT  TYPE AUFK-KTEXT,    "描述
        DDZT   TYPE STRING,     "订单状态
        MATKL  TYPE MARA-MATKL,      "物料组
        PSMNG  TYPE AFPO-PSMNG,      "订单数量
        BCRK   TYPE AFPO-PSMNG,      "本次入库数量
        WEMNG  TYPE AFPO-WEMNG,       "已入库数量
        "   ljrk   TYPE bdmng,       "累计入库数量
        CHARG  TYPE AFPO-CHARG,   "批次
        AMEIN  TYPE AFPO-AMEIN,   "单位
        LGORT  TYPE AFPO-LGORT,   "数量
*        kdauf  TYPE afpo-kdauf,   "销售订单号
        XMMC   TYPE STRING,       "项目名称
        ZJTLTS TYPE BDMNG,       "组件投料套数
        RSNUM  TYPE AFKO-RSNUM,   "预留相关需求
        IGMNG  TYPE AFKO-IGMNG,   "报工数量
        GAMNG  TYPE AFKO-GAMNG,    "订单数量
        XS     TYPE I,        "箱数
        BZ     TYPE STRING,       "备注

      END OF TY_DATA1.

TYPES:BEGIN OF TY_MATNR,
        WERKS TYPE WERKS_D,   "工厂
        MATNR TYPE MATNR,     "物料号
        MAKTX TYPE MAKTX,     "物料描述
        MATKL TYPE MATKL,     "物料组
      END OF TY_MATNR .


TYPES:BEGIN OF TY_PRINT,
        XH    TYPE  I ,           "序号
        AUFNR TYPE AFPO-AUFNR ,   "订单号
        MATNR TYPE AFPO-MATNR,         "抬头物料号
        MAKTX TYPE MAKT-MAKTX,         "物料描述
        KTEXT TYPE AUFK-KTEXT,    "描述
        PSMNG TYPE AFPO-PSMNG,      "订单数量
        BCRK  TYPE BDMNG,      "本次入库数量
        AMEIN TYPE AFPO-AMEIN,   "单位
        LGORT TYPE AFPO-LGORT,   "库存地点
        KDAUF TYPE AFPO-KDAUF,   "销售订单号
        XMMC  TYPE STRING,       "项目名称
        XS    TYPE I,        "箱数
        BZ    TYPE STRING,       "备注
      END OF TY_PRINT .

TYPES:BEGIN OF TY_RESB,
        RSNUM TYPE RESB-RSNUM,  "预留号
        RSPOS TYPE RESB-RSPOS,
        RSART TYPE RESB-RSART,
        XLOEK TYPE RESB-XLOEK,  "删除标识
        KZEAR TYPE RESB-KZEAR,  "最后发货
        BDMNG TYPE RESB-BDMNG,  "需求数量
        ENMNG TYPE RESB-ENMNG,  "提货数量
        TS    TYPE RESB-ENMNG,   "套数
      END OF TY_RESB .

TYPES:BEGIN OF TY_KDAUF,
        KDAUF TYPE KDAUF,
        XMMC  TYPE STRING,

      END OF TY_KDAUF .

DATA:GT_DATA TYPE TABLE OF TY_DATA,
     GS_DATA TYPE  TY_DATA.

DATA: GT_DATA_2 TYPE TABLE OF TY_DATA,
      GS_DATA_2 TYPE TY_DATA.
DATA: GT_DATA_3 TYPE TABLE OF TY_DATA,
      GS_DATA_3 TYPE TY_DATA.
DATA:GT_DATA_1 TYPE TABLE OF TY_DATA,
     GS_DATA_1 TYPE  TY_DATA.

DATA:GT_PRINT TYPE TABLE OF TY_PRINT,
     GS_PRINT TYPE TY_PRINT.

DATA:LT_PRT TYPE TABLE OF TY_PRINT,
     LS_PRT TYPE TY_PRINT.

DATA:GT_MATNR TYPE TABLE OF TY_MATNR,
     GS_MATNR TYPE TY_MATNR.

DATA:GT_JEST TYPE TABLE OF JEST,
     GS_JEST TYPE JEST.

DATA:GT_TJ02T TYPE TABLE OF TJ02T,
     GS_TJ02T TYPE TJ02T.

DATA:GT_ZQM005 TYPE TABLE OF ZQM005,
     GS_ZQM005 TYPE ZQM005.     "打印产成品检验报告表

DATA:GT_RESB TYPE TABLE OF TY_RESB,
     GS_RESB TYPE TY_RESB.

DATA:GT_KDAUF   TYPE TABLE OF TY_KDAUF,
     GS_KDAUF   TYPE TY_KDAUF,
     GS_KDAUF_1 TYPE TY_KDAUF.


FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA .

DATA:G_XH  TYPE I .     "全局序号


*获取销售长文本
DATA LT_LINE TYPE TABLE OF TLINE.
DATA LS_LINE TYPE TLINE.
DATA L_NAME TYPE THEAD-TDNAME.

*打印参数变量
DATA: CONTROL    TYPE SSFCTRLOP,
      NTOTALLINE TYPE I,
      NPAGELINE  TYPE I VALUE 9,
      P_INDEX    LIKE SY-TABIX.
DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
      NCURRLINE       TYPE I,      "中间变量
      JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA: G_NAME TYPE RS38L_FNAM.
DATA:L_FORMNAME TYPE TDSFNAME  VALUE 'ZSFQM004'.
DATA L_LINE TYPE I. "统计打印的行进行补行
DATA G_LINE TYPE I. "设定换页行数
DATA NAME   TYPE CHAR20. "打印人


************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.

  if gw_lvc-fieldname eq 'PSMNG' " OR gw_lvc-fieldname eq 'BCRK'
     OR gw_lvc-fieldname eq 'WEMNG' OR gw_lvc-fieldname eq 'LJRK'
     OR gw_lvc-fieldname eq 'IGMNG'.
      gw_lvc-tabname      = 'GT_DATA'.
      gw_lvc-qfieldname = 'AMEIN'.
  endif.

   if gw_lvc-fieldname eq 'BCRK' .
      gw_lvc-qfieldname = 'AMEIN'.
     gw_lvc-decimals = 3.
   endif.


*  IF gw_lvc-fieldname = 'LYTS'
*  OR gw_lvc-fieldname = 'BCLLS'.
*     gw_lvc-NO_ZERO = 'X'.
*  ENDIF.

  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,                      "SY-REPID 指 当前的主程序
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GW_EVENTS        LIKE LINE OF GT_EVENTS.
DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.

DATA: GW_ISTABLE TYPE LVC_S_STBL.

DATA G_EDIT TYPE C VALUE 'X'. "控制不可编辑

DATA LT_T001W TYPE T001W OCCURS 0 WITH HEADER LINE.

DATA:GS_VBAK TYPE VBAK,
     GT_VBAK TYPE TABLE OF VBAK.

************************************************************************
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-015 .
PARAMETERS    : P_WERKS TYPE AUFK-WERKS DEFAULT '1100' OBLIGATORY.
SELECT-OPTIONS:
                S_AUFNR FOR AUFK-AUFNR ,"OBLIGATORY ,
                S_MATNH FOR AFPO-MATNR,
                S_DISPO FOR AFKO-DISPO,
                S_FEVOR FOR AFKO-FEVOR,
                S_GSTRP FOR AFKO-GSTRP,
                S_GLTRP FOR AFKO-GLTRP.
SELECTION-SCREEN END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '__________'
            ID 'WERKS' FIELD  P_WERKS.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH P_WERKS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  SELECT A~AUFNR A~OBJNR A~AUART A~KTEXT
         B~MATNR B~PSMNG B~WEMNG B~CHARG B~AMEIN B~LGORT B~KDAUF
         C~RSNUM C~IGMNG C~GAMNG
    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM AUFK  AS A
    INNER JOIN  AFPO AS B
    ON A~AUFNR = B~AUFNR
    INNER JOIN AFKO AS C
    ON A~AUFNR = C~AUFNR
    WHERE A~WERKS EQ P_WERKS
       AND A~AUFNR IN S_AUFNR
       AND A~LOEKZ NE 'X'
       AND C~DISPO IN S_DISPO
       AND C~FEVOR IN S_FEVOR
       AND C~GSTRP IN S_GSTRP
       AND C~GLTRP IN S_GLTRP
       AND B~MATNR IN S_MATNH    .
  SORT GT_DATA BY AUFNR .


  CHECK GT_DATA IS NOT INITIAL .

  MOVE-CORRESPONDING  GT_DATA TO GT_KDAUF .

  DELETE GT_KDAUF  WHERE KDAUF IS INITIAL .

  SORT GT_KDAUF BY KDAUF .

  DELETE ADJACENT DUPLICATES FROM GT_KDAUF COMPARING KDAUF .




  "查找排除已锁定、技术性完成的对象
  SELECT * INTO TABLE GT_JEST
    FROM JEST
    FOR ALL ENTRIES IN GT_DATA
    WHERE OBJNR  = GT_DATA-OBJNR
    AND    STAT  NOT IN ('I0043','I0045')
    AND    INACT = ''.
  SORT  GT_JEST BY OBJNR .


  "查状态描述

  SELECT * INTO TABLE GT_TJ02T
    FROM TJ02T
    FOR ALL ENTRIES IN GT_JEST
    WHERE ISTAT = GT_JEST-STAT
    AND SPRAS = SY-LANGU.
  SORT GT_TJ02T BY ISTAT .

  "读取物料描述

  SELECT A~MATNR B~MAKTX C~MATKL
    INTO CORRESPONDING FIELDS OF TABLE GT_MATNR
    FROM MARC  AS A
    INNER JOIN MAKT AS B
    ON A~MATNR = B~MATNR
    INNER JOIN MARA AS C
    ON A~MATNR = C~MATNR
    WHERE A~WERKS EQ P_WERKS
     AND B~SPRAS = SY-LANGU.

  SORT GT_MATNR BY MATNR .

  SELECT RSNUM RSPOS RSART XLOEK KZEAR BDMNG ENMNG INTO TABLE GT_RESB
    FROM RESB
    FOR ALL ENTRIES  IN GT_DATA
    WHERE RSNUM = GT_DATA-RSNUM
    AND   BDMNG NE 0.
  DELETE GT_RESB WHERE XLOEK EQ 'X' OR KZEAR EQ'X' .


  SORT GT_RESB BY RSNUM .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
  DATA:T_M TYPE F .

  "先读取项目名称

  GT_DATA_1 = GT_DATA .

  DELETE GT_DATA_1 WHERE KDAUF IS NOT INITIAL .

  SORT GT_DATA_1 BY AUFNR .

  IF GT_DATA_1 IS NOT INITIAL.
    SELECT * INTO TABLE GT_VBAK
      FROM VBAK .
    SORT GT_VBAK BY VBELN .

    LOOP AT  GT_DATA_1  INTO GS_DATA_1.
      READ TABLE GT_VBAK INTO GS_VBAK WITH KEY VBELN = GS_DATA_1-AUFNR+0(7)
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        READ TABLE GT_KDAUF INTO GS_KDAUF WITH KEY KDAUF = GS_DATA_1-AUFNR+0(7)
                                            .
        IF SY-SUBRC NE 0 .
          CLEAR:GS_KDAUF_1 .
          GS_KDAUF_1-KDAUF = GS_DATA_1-AUFNR+0(7) .
          APPEND GS_KDAUF_1 TO GT_KDAUF .
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

  SORT GT_KDAUF BY KDAUF .
  LOOP AT GT_KDAUF INTO GS_KDAUF .

    PERFORM SELXMMC USING GS_KDAUF-KDAUF SY-LANGU  CHANGING GS_KDAUF-XMMC .
    MODIFY GT_KDAUF FROM GS_KDAUF.

  ENDLOOP.

  LOOP AT GT_RESB INTO GS_RESB .
    "   * 方法二
    IF GS_RESB-BDMNG NE 0 .
      READ TABLE GT_DATA INTO GS_DATA WITH KEY RSNUM = GS_RESB-RSNUM .
      IF SY-SUBRC EQ 0 .
        GS_RESB-TS = GS_RESB-ENMNG / GS_RESB-BDMNG *  GS_DATA-GAMNG .
      ENDIF.
    ENDIF.

    MODIFY GT_RESB FROM  GS_RESB .
  ENDLOOP.
  SORT GT_RESB BY RSNUM ASCENDING TS ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_RESB COMPARING RSNUM ."TS .
  LOOP AT GT_DATA ASSIGNING <FS_DATA> .
    P_INDEX = SY-TABIX .
    "读取状态
    READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = <FS_DATA>-OBJNR BINARY SEARCH .
    IF SY-SUBRC NE 0 .
      "排除TECO、LKD的订单
      DELETE GT_DATA INDEX P_INDEX .

    ELSE.
      LOOP AT GT_JEST INTO GS_JEST WHERE OBJNR = <FS_DATA>-OBJNR .
        IF <FS_DATA>-DDZT IS INITIAL .
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
            <FS_DATA>-DDZT = GS_TJ02T-TXT04.
          ENDIF.

        ELSE.
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
            CONCATENATE <FS_DATA>-DDZT   '_'  GS_TJ02T-TXT04  INTO <FS_DATA>-DDZT.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

    "物料号
    IF <FS_DATA>-MATNR IS NOT INITIAL .
      READ TABLE GT_MATNR INTO GS_MATNR WITH KEY MATNR = <FS_DATA>-MATNR BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        <FS_DATA>-MAKTX = GS_MATNR-MAKTX .
        <FS_DATA>-MATKL = GS_MATNR-MATKL.

      ENDIF.

    ENDIF.

    "累计入库
    "   <fs_data>-ljrk = <fs_data>-wemng.
    "默认本次入库数量
    <FS_DATA>-BCRK = <FS_DATA>-PSMNG - <FS_DATA>-WEMNG .

    "读取项目号
    IF <FS_DATA>-KDAUF  IS INITIAL.
      READ TABLE GT_KDAUF INTO GS_KDAUF WITH KEY KDAUF = <FS_DATA>-AUFNR+0(7)
                                        BINARY SEARCH .
      IF SY-SUBRC EQ 0.
        <FS_DATA>-KDAUF =  <FS_DATA>-AUFNR+0(7) .
      ENDIF.

    ENDIF.
    "项目名称
    IF <FS_DATA>-KDAUF IS NOT INITIAL .
      READ TABLE GT_KDAUF INTO GS_KDAUF WITH KEY KDAUF = <FS_DATA>-KDAUF
                                        BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        <FS_DATA>-XMMC = GS_KDAUF-XMMC .
      ENDIF.

      " PERFORM selxmmc USING <fs_data>-kdauf '1' CHANGING <fs_data>-xmmc .


    ENDIF.

    "组件投料套数

    READ TABLE GT_RESB INTO GS_RESB WITH KEY RSNUM = <FS_DATA>-RSNUM BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      CALL FUNCTION 'ROUND'
        EXPORTING
          DECIMALS      = 0       " 保留多少位小数
          INPUT         = GS_RESB-TS
          SIGN          = '+'     " + 向上取舍 - 向下取舍 （负数也一样）
        IMPORTING
          OUTPUT        = GS_RESB-TS     " 输出返回结果
        EXCEPTIONS
          INPUT_INVALID = 1
          OVERFLOW      = 2
          TYPE_INVALID  = 3
          OTHERS        = 4.
      IF GS_RESB-TS > <FS_DATA>-GAMNG .
        <FS_DATA>-ZJTLTS = <FS_DATA>-GAMNG .
      ELSE.
        <FS_DATA>-ZJTLTS  = GS_RESB-TS .
      ENDIF.
    ELSE.
      <FS_DATA>-ZJTLTS = <FS_DATA>-GAMNG .
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW .
  PERFORM INIT_LAYOUT.             "设置输出格式
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT .
  GW_LAYOUT-ZEBRA         = 'X'.
  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'ZSEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0433   text
*      -->P_0434   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS\

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表
  DATA L_SUBRC TYPE SY-SUBRC.
  CLEAR L_SUBRC.

  "CLEAR L_CHECK.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.


*打印
    WHEN '&PRNT'.
      IF P_WERKS = '1610'.
        PERFORM FRM_PRINT_1610.
      ELSE.
        PERFORM FRM_PRINT_DATA.
      ENDIF.




  ENDCASE.

  CALL METHOD G_REF_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DATA .
  DATA: CONTROL    TYPE SSFCTRLOP,
        NTOTALLINE TYPE I,
        NPAGELINE  TYPE I VALUE 6,
        P_INDEX    LIKE SY-TABIX.
  DATA: EMPTYCOUNT      TYPE I VALUE 0,  "空行数.
        NCURRLINE       TYPE I,      "中间变量
        JOB_OUTPUT_INFO TYPE SSFCRESCL.

  DATA:GT_SEL TYPE TABLE OF ZQM005,
       GS_SEL TYPE ZQM005.

  DATA:PRT_MIN TYPE AFKO-GAMNG .
  DATA:GT_MSG TYPE STRING .
  DATA: G_NAME TYPE RS38L_FNAM.
  DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFQM005'.
  CLEAR:GS_PRINT,GT_PRINT[],
        LS_PRT,LT_PRT[].
  REFRESH:GT_ZQM005.
  CLEAR:GS_ZQM005.
  FIELD-SYMBOLS <LW_PRT>  TYPE TY_PRINT.
  LOOP AT GT_DATA INTO GS_DATA  WHERE ZSEL EQ 'X'.
    CLEAR:PRT_MIN ,GT_MSG.
    IF GS_DATA-ZJTLTS > GS_DATA-IGMNG .
      PRT_MIN = GS_DATA-IGMNG .
    ELSE.
      PRT_MIN = GS_DATA-ZJTLTS .
    ENDIF.

    IF GS_DATA-BCRK + GS_DATA-WEMNG > PRT_MIN .
      CONCATENATE '生产订单:' GS_DATA-AUFNR  INTO GT_MSG .
      CONCATENATE GT_MSG '本次入库量+已入库累计数量超过领料套数或者报工数量，请先投料和报工！' INTO GT_MSG .
      MESSAGE GT_MSG TYPE 'E' .
      EXIT .

    ENDIF.

    CLEAR:GS_PRINT .
    MOVE-CORRESPONDING GS_DATA TO GS_PRINT .
    APPEND GS_PRINT TO GT_PRINT .
  ENDLOOP.

  SORT  GT_PRINT BY AUFNR .

  IF GT_PRINT IS INITIAL.
    MESSAGE S001(Z001) DISPLAY LIKE 'W'.
  ENDIF.



  CHECK GT_PRINT IS NOT INITIAL.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_FORMNAME         "SMARTFORMS的名字
    IMPORTING
      FM_NAME            = G_NAME                "对应的SMARTFORMS的函数
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  WAIT UP TO 1 SECONDS.
  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  CONTROL-NO_OPEN = 'X'.
  CONTROL-NO_CLOSE = 'X'.

* START PRINTING

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    EXIT.
  ENDIF.

  LOOP AT GT_PRINT ASSIGNING <LW_PRT>.
    G_XH =  G_XH + 1 .  "以生产订单号维度 统计序列数
    AT NEW AUFNR.
      CLEAR LS_PRT .
      CLEAR LT_PRT[] .
      "    LS_PRT = <lw_prt>.

    ENDAT.
    MOVE-CORRESPONDING <LW_PRT> TO LS_PRT .
    LS_PRT-XH =  G_XH  .  "序号
    APPEND  LS_PRT TO LT_PRT.
    AT END OF AUFNR.


      CALL FUNCTION G_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
   "      NPAGE_LINE         = NPAGELINE
*         W_HEAD             = LW_PRT
        TABLES
          T_ITEM             = LT_PRT[]
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        "保存打印成功的订单信息到日志表
        REFRESH:GT_SEL,GT_ZQM005 . .
        READ TABLE LT_PRT  INTO LS_PRT INDEX 1  .
        IF SY-SUBRC EQ 0 .
          CLEAR:GS_SEL .
          MOVE-CORRESPONDING LS_PRT TO  GS_ZQM005. .
          GS_ZQM005-WERKS =  P_WERKS.
          SELECT * INTO TABLE GT_SEL
          FROM ZQM005 WHERE AUFNR = LS_PRT-AUFNR .
          SORT GT_SEL BY AUFNR ASCENDING XH DESCENDING .

          READ TABLE GT_SEL INTO  GS_SEL  INDEX 1 .
          IF SY-SUBRC EQ 0 .
            GS_ZQM005-XH =  GS_SEL-XH + 1.
            GS_ZQM005-DY_DATE = SY-DATUM .
            GS_ZQM005-DY_TIME = SY-UZEIT .
            GS_ZQM005-DY_ZH = SY-UNAME .
            APPEND GS_ZQM005 TO GT_ZQM005 .
          ELSE.
            GS_ZQM005-XH =  1.
            GS_ZQM005-DY_DATE = SY-DATUM .
            GS_ZQM005-DY_TIME = SY-UZEIT .
            GS_ZQM005-DY_ZH = SY-UNAME .
            APPEND GS_ZQM005 TO GT_ZQM005 .
          ENDIF.
          IF GT_ZQM005 IS NOT INITIAL.
            "更新ZQM005 表
            MODIFY ZQM005 FROM TABLE GT_ZQM005 .
          ENDIF.



        ENDIF.
      ENDIF.
      CLEAR:G_XH .
    ENDAT.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
*   ERROR HANDLING
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF JOB_OUTPUT_INFO-OUTPUTDONE = 'X'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'AUFNR'            '生产订单号'        '' '' '' '' '' 'AFPO' 'AUFNR'.
  INIT_FIELDCAT 'AUART'            '订单类型'        '' '' '' '' '' 'AUFK' 'AUART'.
  INIT_FIELDCAT 'MATNR'            '抬头物料号'        '' '' '' '' '' 'AFPO' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'            '物料描述'        '' '' '' '' '' 'MAKT' 'MAKTX'.
  INIT_FIELDCAT 'KTEXT'            '描述'        '' '' '' '' '' 'AUFK' 'KTEXT'.
  INIT_FIELDCAT 'DDZT'            '状态'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'            '物料组'        '' '' '' '' '' 'MARA' 'MATKL'.
  INIT_FIELDCAT 'PSMNG'            '订单数量'        '' '' '' '' '' 'AFPO' 'PSMNG'.
  INIT_FIELDCAT 'BCRK'             '本次入库数量'        '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'WEMNG'            '已入库数量'        '' '' '' '' '' 'AFPO' 'WEMNG'.
  " init_fieldcat 'LJRK'            '累计入库数量'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CHARG'            '批次'        '' '' '' '' '' 'AFPO' 'CHARG'.
  INIT_FIELDCAT 'AMEIN'            '单位'        '' '' '' '' '' 'AFPO' 'AMEIN'.
  INIT_FIELDCAT 'LGORT'            '库存地点'        '' '' '' 'X' '' 'AFPO' 'LGORT'.
  INIT_FIELDCAT 'KDAUF'            '销售订单号'        '' '' '' '' '' 'AFPO' 'KDAUF'.
  INIT_FIELDCAT 'XMMC'            '项目名称'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJTLTS'            '组件投料套数'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'IGMNG'            '报工数量'        '' '' '' '' '' 'AFKO' 'IGMNG'.
  INIT_FIELDCAT 'XS'               '箱数'        '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'BZ'               '备注'        '' '' '' 'X' '' '' ''.

ENDFORM.


DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

FORM SELXMMC  USING    P_VBELN TYPE VBELN
                       P_YY     TYPE SPRAS
              CHANGING P_XMMC TYPE STRING.


  " 取项目名称 - 销售订单抬头文本
  G_OBJNAME = P_VBELN.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      ID                      = 'Z001'
      LANGUAGE                = P_YY
      NAME                    = G_OBJNAME
      OBJECT                  = 'VBBK'
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
* IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      LINES                   = IT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC = 0.
    READ TABLE IT_LINES INTO WA_LINES INDEX 1.
    IF SY-SUBRC = 0.
      P_XMMC = WA_LINES-TDLINE.
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: L_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL   TYPE LVC_S_STBL.
  DATA: LS_MOD_CELL TYPE LVC_S_MODI,
        LV_BCRK     TYPE LVC_VALUE,
        LV_YRK      TYPE LVC_VALUE,
        LV_ZJTLTS   TYPE LVC_VALUE,
        LV_BGSL     TYPE LVC_VALUE,
        L_BCRK      TYPE AFPO-WEMNG,
        L_YRK       TYPE AFPO-WEMNG,
        L_ZJTLTS    TYPE AFPO-WEMNG,
        L_BGSL      TYPE AFPO-WEMNG.

  DATA:MIN TYPE AFPO-WEMNG .




*  FIELD-SYMBOLS: <l_chang> TYPE any,<l_out> LIKE LINE OF it_out.
*  ASSIGN er_data_changed->mp_mod_rows->* TO <l_chang>.
*  lt_out[] = <l_chang>.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.
  "call METHOD l_grid->CHECK_CHANGED_DATA.

  READ TABLE  ER_DATA_CHANGED->MT_MOD_CELLS
                         INTO LS_MOD_CELL
                         WITH KEY FIELDNAME = 'BCRK'.
  IF SY-SUBRC = 0.
    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID      "读取已入库数量
        I_FIELDNAME = 'WEMNG'
      IMPORTING
        E_VALUE     = LV_YRK.

    CONDENSE LV_YRK NO-GAPS.
    L_YRK = LV_YRK.

    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = 'ZJTLTS'
      IMPORTING
        E_VALUE     = LV_ZJTLTS.

    CONDENSE LV_ZJTLTS NO-GAPS.

    L_ZJTLTS = LV_ZJTLTS .

    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = 'IGMNG'
      IMPORTING
        E_VALUE     = LV_BGSL.

    CONDENSE LV_BGSL NO-GAPS.
    L_BGSL = LV_BGSL .




    "本次入库数量
    L_BCRK = LS_MOD_CELL-VALUE .

    CLEAR:MIN .
    IF L_ZJTLTS > L_BGSL .
      MIN = L_BGSL .
    ELSE.
      MIN = L_ZJTLTS .
    ENDIF.

    IF L_BCRK  + L_YRK  >  MIN .


      "   lv_value = '234324'.
      CALL METHOD ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
        EXPORTING
          I_MSGID     = 'ZPP01'
          I_MSGNO     = '011'
          I_MSGTY     = 'E'
*         i_msgv1     = ls_mod_cell-value
*         i_msgv2     = l_bcrk
*         i_msgv3     = l_bcrk
          I_FIELDNAME = LS_MOD_CELL-FIELDNAME
          I_ROW_ID    = LS_MOD_CELL-ROW_ID.
*      CLEAR:ls_mod_cell-value .
      "    MESSAGE 'adfa'  TYPE 'E'.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_MOD_CELL-ROW_ID
          I_FIELDNAME = LS_MOD_CELL-FIELDNAME
          I_VALUE     = 0.
      "
    ELSE.
*      "累计入库数量
*      l_ljrk    = ls_mod_cell-value  +   l_ljrk .
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_mod_cell-row_id
*          i_fieldname = 'LJRK'
*          i_value     = l_ljrk.
    ENDIF.
  ENDIF.

*  CALL METHOD l_grid->refresh_table_display
*    EXPORTING
*      is_stable = stbl.
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_1610
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_1610 .
  DATA: ZXH TYPE INT4 VALUE 0.
  FIELD-SYMBOLS <LW_PRT1>  TYPE TY_DATA.
  "获取打印名称
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZJLX_005'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = L_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
  "   打印设置
  CONTROL-NO_OPEN   = 'X'.
  CONTROL-NO_CLOSE  = 'X'.
*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.


  OUTPUT-TDDEST = 'LP01'.
*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
  OUTPUT-RQPOSNAME = ''.
  OUTPUT-TDDATASET = ''.
  OUTPUT-TDSUFFIX1 = ''.
  OUTPUT-TDSUFFIX2 = ''.
  OUTPUT-TDIMMED   = 'X'.
  OUTPUT-TDDELETE  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL
      OUTPUT_OPTIONS     = OUTPUT
*    IMPORTING
*     JOB_OUTPUT_OPTIONS = OPTION
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.


  FIELD-SYMBOLS: <LW_DATA>   TYPE TY_DATA,
                 <LW_DATA_3> TYPE TY_DATA.
  LOOP AT GT_DATA ASSIGNING <LW_PRT1> WHERE ZSEL = 'X'.

    GT_DATA_2 = GT_DATA.
    DELETE GT_DATA_2 WHERE ZSEL NE 'X'.
    GT_DATA_3 = GT_DATA_2.
    GT_DATA_1 = GT_DATA_2.
  ENDLOOP.
  SORT GT_DATA_3 BY KDAUF.
  SORT GT_DATA_1 BY KDAUF.
  LOOP AT GT_DATA_3 ASSIGNING <LW_DATA_3>.
    AT NEW KDAUF.
      LS_ZZS_04_10-KDAUF = <LW_DATA_3>-KDAUF.
      LS_ZZS_04_10-XMMC = <LW_DATA_3>-XMMC.
    ENDAT.

    AT END OF KDAUF.
      CLEAR S_COUNT.
      LOOP AT GT_DATA_1  ASSIGNING <LW_DATA> WHERE KDAUF = <LW_DATA_3>-KDAUF .
        S_COUNT = S_COUNT + 1.
        LS_ZZS_04-AUFNR  = <LW_DATA>-AUFNR.
        LS_ZZS_04-MATNR  = <LW_DATA>-MATNR.
        LS_ZZS_04-MAKTX = <LW_DATA>-MAKTX.
        LS_ZZS_04-PSMNG = <LW_DATA>-PSMNG.
        LS_ZZS_04-BCRK = <LW_DATA>-BCRK.
        LS_ZZS_04-AMEIN = <LW_DATA>-AMEIN.
        LS_ZZS_04-XS = <LW_DATA>-XS.
        LS_ZZS_04-LGORT = <LW_DATA>-LGORT.
        ZXH = ZXH + 1.
        LS_ZZS_04-ZXH        =  ZXH.
        APPEND LS_ZZS_04 TO GT_TAB.
        CLEAR LS_ZZS_04.
      ENDLOOP.


      CLEAR:ZXH.
*  判断空行
      S_COUNT = S_COUNT MOD S_PAGE.
      IF S_COUNT NE 0.
        S_COUNT = S_PAGE - S_COUNT.
        CLEAR LS_ZZS_04.
        DO S_COUNT TIMES.
          APPEND LS_ZZS_04 TO GT_TAB.
        ENDDO.
      ENDIF.

      "#  调用Smartforms的Function Module打印
      CALL FUNCTION L_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
          OUTPUT_OPTIONS     = OUTPUT
          ZJLXCB_H           = LS_ZZS_04_10
          I_NUM              = S_PAGE
        TABLES
          GT_ZJLX            = GT_TAB
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.
      CLEAR GT_TAB.
    ENDAT.
  ENDLOOP.
  "#  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.
