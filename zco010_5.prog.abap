REPORT ZCO010_5.


*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160401
*& Request       :
*& Descriptions  : 生产类成本中心利润表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
TABLES:MKPF,MSEG,MAKT,CKMLHD,CKMLPRKEPH,CKMLCR,ZCO010_5_2.

"汇总表定义
TYPES:BEGIN OF TY_DATA,
   QSGJAHR TYPE BSEG-GJAHR,       "起算年度
   QSMONAT TYPE BKPF-MONAT,       "起算期间
   GJAHR   TYPE BKPF-GJAHR,       "会计年度
   MONAT   TYPE BKPF-MONAT,       "会计期间
   AUFNR   TYPE MSEG-AUFNR,        "订单号
   KALNR   TYPE CKMLHD-KALNR,      "成本估算号
   KOSTL   TYPE CRCO-KOSTL,          "成本中心
   LTEXT   TYPE CSKT-LTEXT,          "成本中心描述
   WERKS   TYPE MSEG-WERKS,          "工厂
   MATNR   TYPE MSEG-MATNR,          "物料号
   MAKTX   TYPE MAKT-MAKTX,          "物料描述
   MAT_KDAUF TYPE MSEG-KDAUF,       "销售订单
   MAT_KDPOS TYPE MSEG-MAT_KDPOS,   "销售订单行项目
   PEINH  TYPE  CKMLCR-PEINH,        "价格单位
   STPRS  TYPE CKMLCR-STPRS,         "当前标准价
   PVPRS  TYPE CKMLCR-PVPRS,         "当前实际价
   KST001 TYPE CKMLPRKEPH-KST001,   "直接材料成本
   KST003 TYPE CKMLPRKEPH-KST003,   "直接人工成本
   KST005 TYPE CKMLPRKEPH-KST005,   "直接机器
   KST007 TYPE CKMLPRKEPH-KST007,   "电成本
   KST009 TYPE CKMLPRKEPH-KST009,   "其制费成本
   KST011 TYPE CKMLPRKEPH-KST011,   "外协成本
   DQSH   TYPE MSEG-MENGE ,         " 当期收货
   DQTL   TYPE MSEG-MENGE,          "当期投料
   DQWX   TYPE MSEG-MENGE,          "当期外协
   QQSLJY TYPE MSEG-MENGE,          "前期数量结余
   QQCZJY TYPE TSLVT12,  "前期产值结余
   DQSLHJ TYPE MSEG-MENGE,         "当期数量合计
   DQYJSL TYPE MSEG-MENGE,         "当期应计数量
   DQCZ   TYPE TSLVT12,            "当期产值
   MEINS  TYPE MEINS,              "数量单位
   WAERS  TYPE WAERS,             "货币码
   SEL(1),
     END OF TY_DATA.

"明细表定义
TYPES:BEGIN OF TY_ITEM,
      MBLNR TYPE MSEG-MBLNR,               "物料凭证
      MJAHR TYPE MSEG-MJAHR,               "物料凭证年度
      BUDAT TYPE MKPF-BUDAT,               "过账日期
      ZEILE TYPE MSEG-ZEILE,               "物料凭证项目
      WERKS TYPE MSEG-WERKS,                "工厂
      AUFNR TYPE MSEG-AUFNR,                "生产订单
      MATNR TYPE MSEG-MATNR,                "物料号
      MAT_KDAUF TYPE MSEG-KDAUF,            "销售订单
      MAT_KDPOS TYPE MSEG-MAT_KDPOS,        "销售订单行项目
      SHKZG TYPE MSEG-SHKZG,                "借贷标识
      MENGE TYPE MSEG-MENGE,                "数量
      MEINS TYPE MSEG-MEINS,                "单位
      BWART TYPE MSEG-BWART,               "移动类型
      SOBKZ TYPE MSEG-SOBKZ,               "特殊库存
      END OF TY_ITEM .

"汇总表定义
TYPES:BEGIN OF TY_TOTAL,

      WERKS TYPE MSEG-WERKS,                "工厂
      MATNR TYPE MSEG-MATNR,                "物料号
      MAT_KDAUF TYPE MSEG-KDAUF,            "销售订单
      MAT_KDPOS TYPE C  LENGTH 6   ,        "销售订单行项目
    "  AUFNR TYPE MSEG-AUFNR,                "生产订单
      GJAHR TYPE C LENGTH 4,             "会计年度
      MONAT TYPE C LENGTH 2,             "会计期间
      KOSTL   TYPE CRCO-KOSTL,          "成本中心
      MEINS TYPE MSEG-MEINS,                "单位
      DQSH   TYPE MSEG-MENGE ,         " 当期收货
      DQTL   TYPE MSEG-MENGE,          "当期投料
      DQWX   TYPE MSEG-MENGE,          "当期外协

     END OF TY_TOTAL .

DATA:GS_DATA TYPE TY_DATA,
     GT_DATA TYPE TABLE OF TY_DATA .

 DATA:GS_ITEM TYPE TY_ITEM,
      GT_ITEM TYPE TABLE OF TY_ITEM.

  DATA:GS_ITEM_01 TYPE TY_ITEM,
      GT_ITEM_01 TYPE TABLE OF TY_ITEM.

  DATA:GS_TOTAL TYPE TY_TOTAL,
      GT_TOTAL TYPE TABLE OF TY_TOTAL.

   DATA:GS_TOTAL_01 TYPE TY_TOTAL,
      GT_TOTAL_01 TYPE TABLE OF TY_TOTAL.  "汇总外协

  DATA:GS_TOTAL_02 TYPE TY_TOTAL,
       GT_TOTAL_02 TYPE TABLE OF TY_TOTAL.

 DATA:GS_MAKT TYPE MAKT,
      GT_MAKT TYPE TABLE OF MAKT.

 DATA:GT_CKMLHD TYPE TABLE OF CKMLHD,
      GS_CKMLHD TYPE CKMLHD.

 DATA:GT_CKMLPRKEPH TYPE TABLE OF  CKMLPRKEPH,
      GS_CKMLPRKEPH TYPE  CKMLPRKEPH.

 DATA:GT_CKMLCR TYPE TABLE OF CKMLCR,
      GS_CKMLCR TYPE CKMLCR.

 DATA:GT_AFKO TYPE TABLE OF AFKO,
      GS_AFKO TYPE AFKO.

  DATA:GS_AFVC TYPE AFVC,
      GT_AFVC TYPE TABLE OF AFVC.

 DATA:GT_CRCO TYPE TABLE OF CRCO,
      GS_CRCO TYPE CRCO.

DATA:GT_CSKT TYPE TABLE OF CSKT,
      GS_CSKT TYPE CSKT.

DATA:D_F_DATE TYPE D ,
     D_E_DATE TYPE D,

     Q_F_DATE TYPE D ,
     Q_E_DATE TYPE D .

DATA:GT_ZCO010_5_2 TYPE TABLE OF ZCO010_5_2 ,
     GS_ZCO010_5_2 TYPE ZCO010_5_2.

DATA:GT_T001 TYPE TABLE OF T001,
     GS_T001 TYPE T001.

DATA:GT_SAVE TYPE TABLE OF ZCO010_5_2 ,
     GS_SAVE TYPE ZCO010_5_2.

DATA:QSGJAHR TYPE BKPF-GJAHR,
     QSMONAT TYPE BKPF-MONAT,
     Q_QSGJAHR TYPE BKPF-GJAHR,
     Q_QSMONAT TYPE BKPF-MONAT.

 FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA.

 DATA: IT_LINES TYPE TABLE OF TLINE,
       WA_LINES TYPE TLINE.

 DATA: G_OBJNAME TYPE THEAD-TDNAME.

  DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  IF &4 = 'X'.
    GW_LVC-KEY = 'X'.
  ENDIF.
*   IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
*   IF &1 = 'MENGE' .
*   GW_LVC-QFIELDNAME = 'MEINS'.
*  ENDIF.

  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_FIELD = &9.
  GW_LVC-REF_TABLE = &8.
  APPEND GW_LVC TO GT_LVC.
  CLEAR GW_LVC.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV DECLARATION
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "ALV的格式
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

************************************************************************
* GLOBAL VARIANT
************************************************************************


************************************************************************
* CONSTANT
************************************************************************

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY,                      "公司代码
P_GJAHR  TYPE BSEG-GJAHR OBLIGATORY,                      "会计代码
P_MONAT  TYPE BKPF-MONAT OBLIGATORY.                      "会计期间

*SELECT-OPTIONS:
*                S_KOSTL  FOR MSEG-KOSTL,                  "成本中心
*                S_AUFNR  FOR MSEG-AUFNR,                  "生产订单
*                S_MATNR  FOR MSEG-MATNR.                  "销售订单
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN:
  SKIP 2,
  BEGIN OF LINE,
    PUSHBUTTON 1(10) BTN1 USER-COMMAND COM_ZJB,
  END OF LINE.
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
   "读取维护起算日期视图
    IF SY-UCOMM EQ 'COM_ZJB'.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        ACTION                       = 'S'
*       CORR_NUMBER                  = '          '
*       GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*       SHOW_SELECTION_POPUP         = ' '
        VIEW_NAME                    = 'ZCO010_5_1'
*       NO_WARNING_FOR_CLIENTINDEP   = ' '
*       RFC_DESTINATION_FOR_UPGRADE  = ' '
*       CLIENT_FOR_UPGRADE           = ' '
*       VARIANT_FOR_SELECTION        = ' '
*       COMPLEX_SELCONDS_USED        = ' '
*       CHECK_DDIC_MAINFLAG          = ' '
*       SUPPRESS_WA_POPUP            = ' '
*     TABLES
*       DBA_SELLIST                  =
*       EXCL_CUA_FUNCT               =
      EXCEPTIONS
        CLIENT_REFERENCE             = 1
        FOREIGN_LOCK                 = 2
        INVALID_ACTION               = 3
        NO_CLIENTINDEPENDENT_AUTH    = 4
        NO_DATABASE_FUNCTION         = 5
        NO_EDITOR_FUNCTION           = 6
        NO_SHOW_AUTH                 = 7
        NO_TVDIR_ENTRY               = 8
        NO_UPD_AUTH                  = 9
        ONLY_SHOW_ALLOWED            = 10
        SYSTEM_FAILURE               = 11
        UNKNOWN_FIELD_IN_DBA_SELLIST = 12
        VIEW_NOT_FOUND               = 13
        MAINTENANCE_PROHIBITED       = 14
        OTHERS                       = 15.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF .

  INITIALIZATION.

  BTN1 = '设置起算日期'.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0289   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
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
 "查询年度、期间的第一天
CONCATENATE P_GJAHR P_MONAT '01' INTO D_F_DATE .

"查询年度、期间的最后一天
CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
  EXPORTING
    I_DATE        = D_F_DATE
 IMPORTING
   E_DATE        = D_E_DATE
          .
"查询上期间的最后一天
Q_E_DATE = D_F_DATE - 1.

"查询上期间的第一天
CONCATENATE Q_E_DATE+0(6) '01' INTO Q_F_DATE .

"查询公司代码维护的起算日期
SELECT SINGLE QSGJAHR QSMONAT INTO (QSGJAHR,QSMONAT)
  FROM ZCO010_5_1
  WHERE BUKRS = P_BUKRS.

 IF  QSGJAHR = P_GJAHR AND QSMONAT = P_MONAT .


 ELSE.

  "查询前一期间的已保存生产成本中心产值表
   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZCO010_5_2
     FROM ZCO010_5_2
     WHERE   " QSGJAHR = P_GJAHR AND QSMONAT = P_MONAT AND
         GJAHR = Q_E_DATE+0(4) AND MONAT = Q_E_DATE+4(2).

   IF  GT_ZCO010_5_2 IS INITIAL.
      MESSAGE '请先执行前一期间的计算' TYPE 'E'  DISPLAY LIKE 'S'.

    ELSE.

    ENDIF.
 ENDIF.

 "查询已过账的生产订单收货明细表
 SELECT MKPF~MBLNR MKPF~MJAHR MKPF~BUDAT
       MSEG~ZEILE MSEG~SHKZG MSEG~MENGE
       MSEG~WERKS MSEG~MATNR MSEG~MAT_KDAUF
       MSEG~MAT_KDPOS MSEG~MEINS MSEG~BWART
       MSEG~AUFNR
       INTO CORRESPONDING FIELDS OF TABLE GT_ITEM
    FROM MKPF
    INNER JOIN MSEG
    ON MKPF~MJAHR = MSEG~MJAHR
    AND MKPF~MBLNR = MSEG~MBLNR
    WHERE MKPF~BUDAT BETWEEN D_F_DATE AND D_E_DATE
    AND MSEG~BWART IN ('101','102','261','262','Z05','Z06')
    AND MSEG~AUFNR NE ''
    AND MSEG~WERKS EQ P_BUKRS .
  "  AND MSEG~MATNR IN S_MATNR
 "   AND MSEG~AUFNR IN S_AUFNR .

 SORT GT_ITEM BY WERKS MATNR MAT_KDAUF MAT_KDPOS AUFNR BUDAT MEINS .
"查询已过账的生产订单投料明细表
 SELECT MKPF~MBLNR MKPF~MJAHR MKPF~BUDAT
       MSEG~ZEILE MSEG~SHKZG MSEG~MENGE
       MSEG~WERKS MSEG~MATNR MSEG~MAT_KDAUF
       MSEG~MAT_KDPOS MSEG~MEINS MSEG~BWART
       "MSEG~AUFNR
     APPENDING CORRESPONDING FIELDS OF TABLE GT_ITEM_01
    FROM MKPF
    INNER JOIN MSEG
    ON MKPF~MJAHR = MSEG~MJAHR
    AND MKPF~MBLNR = MSEG~MBLNR
    WHERE MKPF~BUDAT BETWEEN D_F_DATE AND D_E_DATE
    AND MSEG~BWART IN ('543','544')
    AND MSEG~WERKS EQ P_BUKRS
   " AND MSEG~MATNR IN S_MATNR
     AND MSEG~AUFNR EQ ''.

   SORT GT_ITEM_01 BY WERKS MATNR MAT_KDAUF MAT_KDPOS BUDAT MEINS .

   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
     FROM T001
     WHERE BUKRS = P_BUKRS .
    SORT GT_T001 BY BUKRS .

    "查询生产订单
  IF GT_ITEM IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AFKO
     FROM AFKO
     FOR ALL ENTRIES IN GT_ITEM
     WHERE  AUFNR = GT_ITEM-AUFNR .

     " WHERE AUFNR IN S_AUFNR.
   SORT GT_AFKO BY AUFNR.
  ENDIF.
"查询生产订单对应工序
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AFVC
    FROM AFVC
    FOR ALL ENTRIES IN GT_AFKO
    WHERE AUFPL EQ GT_AFKO-AUFPL.
 SORT GT_AFVC BY AUFPL ascending APLZL descending.

"查询工作中心对应的成本中心
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CRCO
    FROM CRCO
     FOR ALL ENTRIES IN GT_AFVC
    WHERE OBJID = GT_AFVC-ARBID
     AND OBJTY = 'A'
     AND LASET = '    1'
     AND LANUM = '0001' .
 "    AND KOSTL IN S_KOSTL.
SORT  GT_CRCO BY OBJID.

"查询成本中心主数据信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CSKT
  FROM CSKT
"  FOR ALL ENTRIES IN GT_CRCO
  WHERE KOKRS = '1000'
  " AND KOSTL IN S_KOSTL
 " AND KOSTL = GT_CRCO-KOSTL
  AND SPRAS = '1'.
SORT GT_CSKT BY KOSTL.

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
   "以下明细按工厂 、物料号、销售订单号、销售订单行号、生产订单（成本中心）、会计年度、会计期间、单位统计
     "当期收货、当期投料、当期外协
LOOP AT GT_ITEM INTO GS_ITEM.
  CLEAR:GS_TOTAL.
    "成本中心
    READ TABLE GT_AFKO INTO GS_AFKO
      WITH KEY AUFNR = GS_ITEM-AUFNR BINARY SEARCH .
         IF SY-SUBRC EQ 0 .
           READ TABLE GT_AFVC INTO GS_AFVC
             WITH KEY AUFPL = GS_AFKO-AUFPL BINARY SEARCH .
             IF SY-SUBRC EQ 0 .
                "成本中心
                   READ TABLE GT_CRCO INTO GS_CRCO
                       WITH KEY OBJID = GS_AFVC-ARBID BINARY SEARCH.
                     IF SY-SUBRC EQ 0 .
                       GS_TOTAL-KOSTL = GS_CRCO-KOSTL.
                      ELSE.
                        CONTINUE.
                    ENDIF.
              ENDIF.
           ELSE.
             CONTINUE.
          ENDIF.
  GS_TOTAL-WERKS = GS_ITEM-WERKS.         "工厂
  GS_TOTAL-MATNR = GS_ITEM-MATNR.         "物料号
  GS_TOTAL-MAT_KDAUF = GS_ITEM-MAT_KDAUF. "销售订单号
  GS_TOTAL-MAT_KDPOS = GS_ITEM-MAT_KDPOS. "销售订单行号
 " GS_TOTAL-AUFNR = GS_ITEM-AUFNR.         "生产订单

  GS_TOTAL-GJAHR = GS_ITEM-BUDAT+0(4).    "会计年度
  GS_TOTAL-MONAT = GS_ITEM-BUDAT+4(2).    "会计期间
  GS_TOTAL-MEINS = GS_ITEM-MEINS .        "单位
  IF GS_ITEM-BWART = '101' OR GS_ITEM-BWART = '102'.
      IF GS_ITEM-SHKZG = 'H'.
         GS_TOTAL-DQSH = GS_ITEM-MENGE * -1.
        ELSE.                               "当期收货
         GS_TOTAL-DQSH = GS_ITEM-MENGE .
      ENDIF.
      COLLECT GS_TOTAL INTO GT_TOTAL .
      CONTINUE.
    ELSEIF GS_ITEM-BWART = '261' OR GS_ITEM-BWART = '262'.
          IF GS_ITEM-SHKZG = 'S'.
             GS_TOTAL-DQTL = GS_ITEM-MENGE * -1.
          ELSE.                              "当期投料
             GS_TOTAL-DQTL = GS_ITEM-MENGE .
         ENDIF.
         COLLECT GS_TOTAL INTO GT_TOTAL .
        CONTINUE.
        ELSEIF GS_ITEM-BWART = 'Z05' OR GS_ITEM-BWART = 'Z06'.
              IF GS_ITEM-SHKZG = 'S'.
                 GS_TOTAL-DQTL = GS_ITEM-MENGE * -1.
              ELSE.                         "当期外协
                 GS_TOTAL-DQTL = GS_ITEM-MENGE .
              ENDIF.
              COLLECT GS_TOTAL INTO GT_TOTAL .
            CONTINUE.
    ENDIF.
ENDLOOP.
SORT GT_TOTAL BY WERKS MATNR MAT_KDAUF MAT_KDPOS
                KOSTL GJAHR MONAT MEINS DQSH DQTL .
"以下明细按工厂 、物料号、销售订单号、销售订单行号、会计年度、会计期间、单位统计
     "、当期外协
  LOOP AT GT_ITEM_01 INTO GS_ITEM_01.
    READ TABLE GT_TOTAL INTO GS_TOTAL WITH KEY WERKS = GS_ITEM_01-WERKS
                                              MATNR = GS_ITEM_01-MATNR
                                              MAT_KDAUF = GS_ITEM_01-MAT_KDAUF
                                              MAT_KDPOS = GS_ITEM_01-MAT_KDPOS
                                              GJAHR = GS_ITEM_01-BUDAT+0(4)
                                              MONAT = GS_ITEM_01-BUDAT+4(2) .
   IF SY-SUBRC EQ 0 .
      CLEAR:GS_TOTAL_01.
     GS_TOTAL_01-WERKS = GS_ITEM_01-WERKS.         "工厂
     GS_TOTAL_01-MATNR = GS_ITEM_01-MATNR.         "物料号
     GS_TOTAL_01-MAT_KDAUF = GS_ITEM_01-MAT_KDAUF. "销售订单号
     GS_TOTAL_01-MAT_KDPOS = GS_ITEM_01-MAT_KDPOS. "销售订单行号
     GS_TOTAL_01-GJAHR = GS_ITEM_01-BUDAT+0(4).    "会计年度
     GS_TOTAL_01-MONAT = GS_ITEM_01-BUDAT+4(2).    "会计期间
     GS_TOTAL_01-MEINS = GS_ITEM_01-MEINS .        "单位
     IF GS_ITEM_01-SHKZG = 'S'.
           GS_TOTAL_01-DQWX = GS_ITEM_01-MENGE * -1.
       ELSE.                         "当期外协
            GS_TOTAL_01-DQWX = GS_ITEM_01-MENGE .
      ENDIF.
         COLLECT GS_TOTAL_01 INTO GT_TOTAL_01 .
     ELSE.
       CONTINUE.
   ENDIF.

  ENDLOOP.
SORT GT_TOTAL_01 BY WERKS MATNR MAT_KDAUF MAT_KDPOS
                    GJAHR MONAT MEINS DQSH DQTL DQWX.
CHECK GT_TOTAL IS NOT INITIAL.
APPEND LINES OF GT_TOTAL TO GT_TOTAL_02 .
SORT GT_TOTAL_02 BY WERKS MATNR MAT_KDAUF MAT_KDPOS .
DELETE ADJACENT DUPLICATES FROM GT_TOTAL_02
   COMPARING WERKS MATNR MAT_KDAUF MAT_KDPOS .
"查询成本估算号
SELECT KALNR  BWKEY MATNR VBELN POSNR
  INTO CORRESPONDING FIELDS OF TABLE GT_CKMLHD
  FROM CKMLHD
  FOR ALL ENTRIES IN GT_TOTAL_02
  WHERE BWKEY = GT_TOTAL_02-WERKS
    AND MATNR = GT_TOTAL_02-MATNR
    AND VBELN = GT_TOTAL_02-MAT_KDAUF .
 "   AND POSNR = GT_TOTAL_02-MAT_KDPOS.
SORT  GT_CKMLHD BY BWKEY MATNR VBELN POSNR.

"物料分类账成本价格字段
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CKMLPRKEPH
  FROM CKMLPRKEPH
  FOR ALL ENTRIES IN GT_CKMLHD
  WHERE KALNR = GT_CKMLHD-KALNR
  AND   BDATJ = P_GJAHR
  AND   POPER = P_MONAT
  AND   PRTYP = 'V'
  AND   KKZST = ''
  AND   CURTP = '10' .
SORT GT_CKMLPRKEPH BY KALNR  BDATJ POPER.

"物料分类账的期间价格信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CKMLCR
  FROM CKMLCR
  FOR ALL ENTRIES IN GT_CKMLHD
  WHERE KALNR = GT_CKMLHD-KALNR
  AND   BDATJ = P_GJAHR
  AND   POPER = P_MONAT
  AND   CURTP = '10'.
SORT GT_CKMLCR BY KALNR BDATJ  POPER .



"查询物料主数据信息
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FROM MAKT
    FOR ALL ENTRIES IN GT_TOTAL
    WHERE MATNR = GT_TOTAL-MATNR
    AND   SPRAS = '1'.
  SORT GT_MAKT BY MATNR.

 LOOP AT GT_TOTAL INTO GS_TOTAL.


    CLEAR:GS_DATA.
    MOVE-CORRESPONDING  GS_TOTAL TO GS_DATA.

   "、当期外协
     READ TABLE GT_TOTAL_01 INTO GS_TOTAL_01 WITH KEY WERKS = GS_TOTAL-WERKS
                                              MATNR = GS_TOTAL-MATNR
                                              MAT_KDAUF = GS_TOTAL-MAT_KDAUF
                                              MAT_KDPOS = GS_TOTAL-MAT_KDPOS
                                              GJAHR = GS_TOTAL-GJAHR
                                              MONAT = GS_TOTAL-MONAT .

    IF SY-SUBRC EQ 0 .
      GS_DATA-DQWX = GS_TOTAL_01-DQWX.

    ENDIF.

    "起算日期
    GS_DATA-QSGJAHR = QSGJAHR.
    GS_DATA-QSMONAT = QSMONAT.
    "成本估算号
    READ TABLE GT_CKMLHD INTO GS_CKMLHD WITH KEY BWKEY = GS_TOTAL-WERKS
                                                 MATNR = GS_TOTAL-MATNR
                                                 VBELN = GS_TOTAL-MAT_KDAUF
                                                 POSNR = GS_TOTAL-MAT_KDPOS
                                                 BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      GS_DATA-KALNR = GS_CKMLHD-KALNR.
    ENDIF.

   "成本中心描述
    READ TABLE GT_CSKT INTO GS_CSKT
         WITH KEY KOSTL = GS_DATA-KOSTL BINARY SEARCH.
       IF SY-SUBRC EQ 0 .
          GS_DATA-LTEXT = GS_CSKT-LTEXT.
      ENDIF.


     "物料描述
   READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = GS_DATA-MATNR BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
      GS_DATA-MAKTX =  GS_MAKT-MAKTX.
   ENDIF.

  READ TABLE GT_CKMLCR INTO GS_CKMLCR WITH KEY KALNR = GS_DATA-KALNR
                                               BDATJ = GS_DATA-GJAHR
                                               POPER = GS_DATA-MONAT BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
     "价格单位
     GS_DATA-PEINH = GS_CKMLCR-PEINH.
     "当期标准价
     GS_DATA-STPRS = GS_CKMLCR-STPRS.
     "当期实际价
     GS_DATA-PVPRS = GS_CKMLCR-PVPRS.

   ENDIF.

  READ TABLE GT_CKMLPRKEPH INTO GS_CKMLPRKEPH
             WITH KEY  KALNR = GS_DATA-KALNR
                       BDATJ = GS_DATA-GJAHR
                       POPER = GS_DATA-MONAT BINARY SEARCH .
   IF SY-SUBRC EQ 0.
      "直接材料成本
       GS_DATA-KST001 = GS_CKMLPRKEPH-KST001.
       "直接人工成本
       GS_DATA-KST003 = GS_CKMLPRKEPH-KST003.
       "直接机器
       GS_DATA-KST005 = GS_CKMLPRKEPH-KST005.
       "电成本
       GS_DATA-KST007 = GS_CKMLPRKEPH-KST007.
       "其制费成本
       GS_DATA-KST009 = GS_CKMLPRKEPH-KST009.
       "外协成本
       GS_DATA-KST011 = GS_CKMLPRKEPH-KST011.

     ENDIF.


   READ TABLE GT_ZCO010_5_2 INTO GS_ZCO010_5_2 WITH KEY GJAHR = Q_E_DATE+0(4)
                                                        MONAT = Q_E_DATE+4(2)
                                                        KALNR = GS_DATA-KALNR
                                                        KOSTL = GS_DATA-KOSTL.
       IF SY-SUBRC EQ 0.
         "前期数量结余
         GS_DATA-QQSLJY = GS_ZCO010_5_2-DQYJSL + GS_ZCO010_5_2-QQSLJY .
         "前期产值结余
         GS_DATA-QQCZJY = GS_ZCO010_5_2-DQCZ + GS_ZCO010_5_2-QQCZJY.
      ENDIF.



  "  当期数量合计: 1）计算 当期数量合计=当期收货-当期投料-当期外协
  GS_DATA-DQSLHJ = GS_DATA-DQSH - GS_DATA-DQTL - GS_DATA-DQWX .

   "当期投料 + 当期收货 + 前期结余 小于 0 。则 当期应计数量为0，当期产值为0
  IF GS_DATA-DQTL + GS_DATA-DQSH + GS_DATA-QQSLJY < 0 .
     GS_DATA-DQYJSL =  0 .
     GS_DATA-DQCZ   = 0 .

  ELSEIF GS_DATA-DQSLHJ < 0 AND (  abs( GS_DATA-DQSLHJ ) > GS_DATA-QQSLJY ).
        "当期应计数量： 当期应计数量= 前期数量结余× -1.
         GS_DATA-DQYJSL = GS_DATA-QQSLJY * -1.
         "当期产值 当期产值 = 前期产值结余
         GS_DATA-DQCZ   = GS_DATA-QQCZJY * -1.

    ELSE.
      "当期应计数量：
     "当期应计数量 = 当期数量合计
      GS_DATA-DQYJSL = GS_DATA-DQSLHJ  .
     "当期产值 = 当期数量合计*当期实际成本/价格单位*（1-外协成本/当期实际成本）
      GS_DATA-DQCZ   = GS_DATA-DQSLHJ  * GS_DATA-PVPRS / GS_DATA-PEINH
                        * ( 1 - GS_DATA-KST011 / GS_DATA-PVPRS ).


  ENDIF.
 "公司代码货币
READ TABLE GT_T001 INTO GS_T001
     WITH KEY BUKRS = P_BUKRS BINARY SEARCH .
  IF SY-SUBRC EQ 0 .
    GS_DATA-WAERS = GS_T001-WAERS .
  ENDIF.
APPEND GS_DATA  TO GT_DATA.

CLEAR:GS_SAVE.

MOVE-CORRESPONDING GS_DATA TO GS_SAVE .
APPEND GS_SAVE TO GT_SAVE.
ENDLOOP.
SORT GT_DATA BY QSGJAHR QSMONAT GJAHR  MONAT  KALNR KOSTL .

SORT GT_SAVE BY QSGJAHR QSMONAT GJAHR  MONAT  KALNR KOSTL .

CHECK GT_DATA IS NOT INITIAL.
  "保存查询到数据汇总报表到 数据表 ZCO010_5_2 .
 "先删除该查询屏幕的公司代码 + 会计年度 + 会计期间底层表的所有数据
 DELETE FROM ZCO010_5_2 WHERE WERKS = P_BUKRS AND GJAHR = P_GJAHR AND MONAT = P_MONAT .
 "然保存查询到的GＴ＿ＤＡＴＡ　数据到　ＺＣＯ０１０＿５＿２．

 MODIFY ZCO010_5_2 FROM TABLE GT_SAVE.
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
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_BUILD_EVENT.
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
  GW_LAYOUT-ZEBRA        = 'X'.
  GW_LAYOUT-CWIDTH_OPT   = 'X'.
  GW_LAYOUT-BOX_FNAME = 'SEL'.
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
*&      Form  FRM_INIT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LVC .
  INIT_FIELDCAT 'QSGJAHR'    '起算年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QSMONAT'    '起算期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'      '会计年度'         '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MONAT'      '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KALNR'      '成本估算号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'      '成本中心'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LTEXT'      '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WERKS'      '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'      '物料号'         '' '' '' '' '' 'MSEG' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'      '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAT_KDAUF'  '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAT_KDPOS'  '销售订单行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PEINH'      '价格单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STPRS'      '当前标准价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PVPRS'      '当前实际价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST001'     '直接材料成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST003'     '直接人工成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST005'     '直接机器'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST007'     '电成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST009'     '其制费成本'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KST011'     '外协成本'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQSH'       '当期收货'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQTL'       '当期投料'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQWX'       '当期外协'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QQSLJY'     '前期数量结余'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'QQCZJY'     '前期产值结余'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQYJSL'     '当期应计数量'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQCZ'       '当期产值'         '' '' '' '' '' '' ''.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0355   text
*      -->P_0356   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT  TABLES  PT_LVC TYPE LVC_T_FCAT
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
*      I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
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
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
*      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
*      CHECK SY-SUBRC = 0.
*      IF RS_SELFIELD-FIELDNAME = 'BELNR'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*      IF RS_SELFIELD-FIELDNAME = 'VBELN'
*        AND GS_DATA-VBELN IS NOT INITIAL.
*        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
