REPORT ZCO010_1.
"

*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 2016-04-06
*& Request       :
*& Descriptions  : 成本中心利润表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
************************************************************************
* TABLES
************************************************************************
TABLES:BKPF,BSEG,VBAK,T001,CSKT,ZCO010_6,TVKBT,isellist.

TYPES:BEGIN OF TY_DATA,
       BUKRS TYPE BUKRS,        "公司代码
       KOSTL TYPE KOSTL,       "成本中心
       KOSTL_TXT TYPE STRING, "成本中心描述
       KOSAR TYPE KOSAR,        "成本类型
       GJAHR TYPE GJAHR,        "会计年度
       MONAT TYPE MONAT,        "会计期间
       XSSR  TYPE TSLVT12,      "销售收入
       SCCZ  TYPE TSLVT12,      "生产产值
       BMFY  TYPE TSLVT12,      "部门费用
       WAERS TYPE WAERS,     "货币码
       SEL(1),
    END OF TY_DATA.

 TYPES:BEGIN OF TY_ITEM_XS ,
       BUKRS TYPE BUKRS,     "公司代码
       GJAHR TYPE GJAHR,     "会计年度
       MONAT TYPE MONAT,     "会计期间
       BELNR TYPE BELNR_D,      "会计凭证
       BUZEI TYPE BUZEI,      "行项目
       SHKZG TYPE SHKZG,      "借贷标识
       BUDAT TYPE BUDAT,      "过账日期
       DMBTR TYPE BSEG-DMBTR,  "本币金额
       VBEL2 TYPE BSEG-VBEL2,  "销售订单号
       XBLNR TYPE BKPF-XBLNR,  "参照
       VBELN TYPE VBELN,       "销售订单
       KOSTL TYPE KOSTL,       "成本中心

   END OF TY_ITEM_XS.

 TYPES:BEGIN OF TY_COLLECT ,
        KOSTL TYPE KOSTL,     "成本中心
       GJAHR TYPE C LENGTH 4,     "会计年度
       MONAT TYPE C LENGTH 2,     "会计期间

       XSSR  TYPE TSLVT12,   "销售收入
        SCCZ  TYPE TSLVT12,      "生产产值
       WAERS TYPE WAERS,     "货币码
       END OF TY_COLLECT.

TYPES:BEGIN OF TY_COLLECT_BMFY,
      OBJNR TYPE OBJNR ,
      GJAHR TYPE GJAHR,
      WOG001 TYPE TSLVT12,
      WOG002 TYPE TSLVT12,
      WOG003 TYPE TSLVT12,
      WOG004 TYPE TSLVT12,
      WOG005 TYPE TSLVT12,
      WOG006 TYPE TSLVT12,
      WOG007 TYPE TSLVT12,
      WOG008 TYPE TSLVT12,
      WOG009 TYPE TSLVT12,
      WOG010 TYPE TSLVT12,
      WOG011 TYPE TSLVT12,
      WOG012 TYPE TSLVT12,
     END OF TY_COLLECT_BMFY.

DATA:GT_COLLECT_BMFY TYPE TABLE OF TY_COLLECT_BMFY,
     GS_COLLECT_BMFY TYPE TY_COLLECT_BMFY .

DATA:GT_COLLECT_XS TYPE TABLE OF TY_COLLECT,
     GS_COLLECT_XS TYPE TY_COLLECT.

DATA:GT_COLLECT_CZ TYPE TABLE OF TY_COLLECT,
     GS_COLLECT_CZ TYPE TY_COLLECT.

DATA:GT_COLLECT TYPE TABLE OF TY_COLLECT,
     GS_COLLECT TYPE TY_COLLECT.

DATA:GS_DATA TYPE TY_DATA,
     GT_DATA TYPE TABLE OF TY_DATA.

DATA:GS_ITEM_XS TYPE TY_ITEM_XS,
     GT_ITEM_XS TYPE TABLE OF TY_ITEM_XS.

DATA:GS_VBAK_XM TYPE VBAK,
      GT_VBAK_XM TYPE TABLE OF VBAK.

DATA:GS_VBAK_CP TYPE VBAK,
       GT_VBAK_CP TYPE TABLE OF VBAK.

DATA:GT_T001 TYPE TABLE OF T001,
      GS_T001 TYPE T001.

DATA:GT_ZCO010_5_1 TYPE TABLE OF ZCO010_5_1,
     GS_ZCO010_5_1 TYPE  ZCO010_5_1 .

DATA:GT_ZCO010_5_2 TYPE TABLE OF ZCO010_5_2,
     GS_ZCO010_5_2 TYPE  ZCO010_5_2 .

DATA:GT_COSS TYPE TABLE  OF COSS,
     GS_COSS TYPE COSS.

DATA:GT_COSP TYPE TABLE OF COSP,
      GS_COSP TYPE COSP.

DATA:GT_ZCO010_6 TYPE TABLE OF ZCO010_6,
       GS_ZCO010_6 TYPE ZCO010_6.

DATA:GS_CSKT TYPE CSKT,
      GT_CSKT TYPE TABLE OF CSKT.

DATA:GS_CSKS TYPE CSKS,
     GT_CSKS TYPE TABLE OF CSKS.


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
   IF &1 = 'XSSR'  OR &1 = 'SCCZ' OR &1 = 'BMFY'.
   GW_LVC-CFIELDNAME = 'WAERS'.
  ENDIF.

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
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY,                   "公司代码
P_GJAHR  TYPE BKPF-GJAHR OBLIGATORY.                    "年度
SELECT-OPTIONS: "S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,   "过账日期
               " S_GZQJ  FOR isellist-month OBLIGATORY ,"过账期间
                S_MONAT  FOR BKPF-MONAT OBLIGATORY,   "期间
                S_KOSTL  FOR BSEG-KOSTL.   "成本中心

SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_GZQJ-low.
*  PERFORM getf4help_month.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_GZQJ-high.
*  PERFORM getf4help_month1.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.

 "&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
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
*      -->P_0309   text
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

"查询销售明细
 SELECT BKPF~BUKRS BKPF~GJAHR BKPF~BUDAT BKPF~MONAT  BKPF~XBLNR
        BSEG~BELNR BSEG~BUZEI  BSEG~DMBTR BSEG~SHKZG
        BSEG~VBEL2
  INTO CORRESPONDING FIELDS OF TABLE GT_ITEM_XS
   FROM BKPF
   INNER JOIN BSEG
    ON BKPF~GJAHR  = BSEG~GJAHR
   AND BKPF~BUKRS = BSEG~BUKRS
   AND BKPF~BELNR = BSEG~BELNR
   WHERE  BKPF~BUKRS EQ P_BUKRS
    AND   BKPF~GJAHR EQ P_GJAHR
    AND   BKPF~MONAT IN S_MONAT
*   AND   BKPF~GJAHR  EQ S_GZQJ+0(4)
*   AND   BKPF~MONAT EQ S_GZQJ+4(2)
   AND   BKPF~BLART EQ 'RV'
   AND   BSEG~HKONT BETWEEN '6001000000' AND '6001999999'.
 SORT GT_ITEM_XS BY BUKRS GJAHR MONAT  BELNR.

 "生产类成本中心产值表
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZCO010_5_2
   FROM ZCO010_5_2
   WHERE WERKS = P_BUKRS
   AND   GJAHR EQ P_GJAHR
   AND   MONAT IN S_MONAT
   AND   KOSTL IN S_KOSTL..

 SORT GT_ZCO010_5_2 BY WERKS  KOSTL GJAHR MONAT  .

 "销售副总对应成本中心维护
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ZCO010_6
  FROM ZCO010_6
  WHERE BUKRS EQ P_BUKRS
   AND   KOSTL IN S_KOSTL.
 SORT GT_ZCO010_6 BY BUKRS ASCENDING VKORG
                           ASCENDING VKBUR
                           ASCENDING YXQC DESCENDING.

* "保留销售副总对应的最新有效期成本中心
* DELETE ADJACENT DUPLICATES FROM GT_ZCO010_6 COMPARING BUKRS VKORG VKBUR .

 "查询项目类订单信息
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK_XM
   FROM VBAK
   FOR ALL ENTRIES IN GT_ZCO010_6
   WHERE VKORG = GT_ZCO010_6-VKORG
   AND AUART IN ('ZF1','ZPO','ZSO','ZWV','ZZG').

SORT GT_VBAK_XM BY VBELN.

 "查询产品类订单信息
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_VBAK_CP
   FROM VBAK
   FOR ALL ENTRIES IN GT_ZCO010_6
   WHERE VKORG = GT_ZCO010_6-VKORG
   AND AUART IN ('ZF2','ZOR','ZRE','ZSD').
SORT GT_VBAK_CP BY VBELN.


SORT GT_CSKT BY KOSTL.

"查询公司代码对应的基本信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
  FROM T001
  WHERE BUKRS = P_BUKRS.
SORT GT_T001 BY BUKRS.

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
  DATA:L_TABIX TYPE SY-TABIX.
  DATA:L_KS  TYPE C .
   "过滤符合查询条件产品订单和项目订单对应的成本中心
 LOOP AT GT_ITEM_XS INTO GS_ITEM_XS .
 "  clear:L_KS.
    L_TABIX = SY-TABIX.
         "先判断是不是项目订单
    READ TABLE GT_VBAK_XM INTO GS_VBAK_XM WITH KEY VBELN = GS_ITEM_XS-VBEL2 BINARY SEARCH.
     IF SY-SUBRC EQ 0 .
        GS_ITEM_XS-VBELN = GS_VBAK_XM-VBELN .
        "成本中心
            LOOP AT GT_ZCO010_6 INTO GS_ZCO010_6
              WHERE VKORG = GS_VBAK_XM-VKORG
              AND VKBUR = GS_VBAK_XM-VKBUR
              AND YXQC <= GS_ITEM_XS-BUDAT .
            GS_ITEM_XS-KOSTL = GS_ZCO010_6-KOSTL.
            MODIFY GT_ITEM_XS FROM GS_ITEM_XS.
         "   L_KS  = 1.
            EXIT.
          ENDLOOP.

            IF S_KOSTL IS NOT INITIAL AND GS_ITEM_XS-KOSTL EQ ''.
             DELETE GT_ITEM_XS INDEX L_TABIX.
             CONTINUE.
           ENDIF.
*        READ TABLE GT_ZCO010_6 INTO GS_ZCO010_6 WITH KEY VKORG = GS_VBAK_XM-VKORG VKBUR = GS_VBAK_XM-VKBUR BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          GS_ITEM_XS-KOSTL = GS_ZCO010_6-KOSTL.
*          MODIFY GT_ITEM_XS FROM GS_ITEM_XS.
*          ELSE.
*             DELETE GT_ITEM_XS INDEX L_TABIX.
*               CONTINUE.
*        ENDIF.
     ELSE.
         "再判断是不是产品订单
          READ TABLE GT_VBAK_CP INTO GS_VBAK_CP
           WITH KEY  VBELN = GS_ITEM_XS-XBLNR BINARY SEARCH .
           IF SY-SUBRC EQ 0 .
             GS_ITEM_XS-VBELN = GS_VBAK_CP-VBELN .
             "成本中心
                  LOOP AT GT_ZCO010_6 INTO GS_ZCO010_6
                  WHERE VKORG = GS_VBAK_CP-VKORG
                  AND VKBUR = GS_VBAK_CP-VKBUR
                  AND YXQC <= GS_ITEM_XS-BUDAT .
                  GS_ITEM_XS-KOSTL = GS_ZCO010_6-KOSTL.
                  MODIFY GT_ITEM_XS FROM GS_ITEM_XS.
               "   L_KS  = 1.
                  EXIT.
                ENDLOOP.

            IF S_KOSTL IS NOT INITIAL AND GS_ITEM_XS-KOSTL EQ '' .
             DELETE GT_ITEM_XS INDEX L_TABIX.
             CONTINUE.
            ENDIF.
*             READ TABLE GT_ZCO010_6 INTO GS_ZCO010_6 WITH KEY VKORG = GS_VBAK_CP-VKORG VKBUR = GS_VBAK_CP-VKBUR BINARY SEARCH.
*               IF SY-SUBRC = 0.
*                 GS_ITEM_XS-KOSTL = GS_ZCO010_6-KOSTL.
*                  MODIFY GT_ITEM_XS FROM GS_ITEM_XS.
*                 ELSE.
*                DELETE GT_ITEM_XS INDEX L_TABIX.
*                 CONTINUE.
*               ENDIF.
             ELSE.
               DELETE GT_ITEM_XS INDEX L_TABIX.
               CONTINUE.
           ENDIF.
     ENDIF.
 ENDLOOP.
 SORT GT_ITEM_XS BY GJAHR MONAT KOSTL .
 "按成本中心、年度 、期间 汇总本币的销售收入
 LOOP AT GT_ITEM_XS INTO  GS_ITEM_XS.
    IF GS_ITEM_XS-SHKZG EQ 'S'.
      GS_ITEM_XS-DMBTR = GS_ITEM_XS-DMBTR * -1 .
     ENDIF.

   CLEAR:GS_COLLECT_XS.
     GS_COLLECT_XS-KOSTL = GS_ITEM_XS-KOSTL.   "成本中心.
     GS_COLLECT_XS-GJAHR = GS_ITEM_XS-GJAHR .  "会计年度
     GS_COLLECT_XS-MONAT = GS_ITEM_XS-MONAT .  "会计期间
     GS_COLLECT_XS-XSSR  = GS_ITEM_XS-DMBTR.   "销售收入
     COLLECT GS_COLLECT_XS INTO GT_COLLECT_XS.
 ENDLOOP.
   SORT GT_COLLECT_XS BY  KOSTL GJAHR MONAT.
   APPEND LINES OF GT_COLLECT_XS TO GT_COLLECT .

"按成本中心、年度、期间统计生产产值
   LOOP AT GT_ZCO010_5_2 INTO GS_ZCO010_5_2.

     CLEAR:GS_COLLECT_CZ.
     GS_COLLECT_CZ-KOSTL = GS_ZCO010_5_2-KOSTL.   "成本中心.
     GS_COLLECT_CZ-GJAHR = GS_ZCO010_5_2-GJAHR .  "会计年度
     GS_COLLECT_CZ-MONAT = GS_ZCO010_5_2-MONAT .  "会计期间
     GS_COLLECT_CZ-SCCZ  = GS_ZCO010_5_2-DQCZ  .  "生产产值
     COLLECT GS_COLLECT_CZ INTO GT_COLLECT_CZ.

  ENDLOOP.
   SORT GT_COLLECT_CZ BY  KOSTL GJAHR MONAT.

   APPEND LINES OF GT_COLLECT_CZ TO GT_COLLECT .

   SORT GT_COLLECT BY KOSTL GJAHR MONAT.

   "保留成本中心 、年度、期间记录信息
   DELETE ADJACENT DUPLICATES FROM GT_COLLECT  COMPARING  KOSTL GJAHR MONAT .

   "查询成本中心对应胡对象号
   CHECK GT_COLLECT IS NOT INITIAL.

  "查询成本中心主数据信息

   SELECT * INTO TABLE GT_CSKS
     FROM CSKS
     FOR ALL ENTRIES IN GT_COLLECT
     WHERE KOSTL = GT_COLLECT-KOSTL
           AND KOKRS = '1000'.

 SORT GT_CSKS BY KOSTL .

"查询成本中描述信息
 SELECT * INTO TABLE GT_CSKT
   FROM CSKT
   FOR ALL ENTRIES IN GT_CSKS
   WHERE  KOSTL = GT_CSKS-KOSTL
          AND KOKRS = '1000'.

  SORT GT_CSKT BY KOSTL .

 "查询对应货币价值外部
  SELECT * INTO TABLE GT_COSP
    FROM COSP
    FOR ALL ENTRIES IN GT_CSKS
    WHERE OBJNR = GT_CSKS-OBJNR
     AND  GJAHR EQ P_GJAHR
     AND  WRTTP = '04' .
  "   AND  OBJNR LIKE 'KS1000%'.
  SORT GT_COSP BY OBJNR GJAHR.

"查询对应货币价值内部
    SELECT * INTO TABLE GT_COSS
    FROM COSS
    FOR ALL ENTRIES IN GT_CSKS
    WHERE OBJNR = GT_CSKS-OBJNR
     AND  GJAHR EQ P_GJAHR
     AND  WRTTP = '04' .
   "  AND  OBJNR LIKE 'KS1000%'.
  SORT GT_COSS BY OBJNR GJAHR.

 "按对象、年度统计各期间部门费用
 LOOP AT GT_COSP INTO GS_COSP .
   CLEAR:GS_COLLECT_BMFY.
   GS_COLLECT_BMFY-OBJNR = GS_COSP-OBJNR.
   GS_COLLECT_BMFY-GJAHR = GS_COSP-GJAHR.
   GS_COLLECT_BMFY-WOG001 = GS_COSP-WOG001.
   GS_COLLECT_BMFY-WOG002 = GS_COSP-WOG002.
   GS_COLLECT_BMFY-WOG003 = GS_COSP-WOG003.
   GS_COLLECT_BMFY-WOG004 = GS_COSP-WOG004.
   GS_COLLECT_BMFY-WOG005 = GS_COSP-WOG005.
   GS_COLLECT_BMFY-WOG006 = GS_COSP-WOG006.
   GS_COLLECT_BMFY-WOG007 = GS_COSP-WOG007.
   GS_COLLECT_BMFY-WOG008 = GS_COSP-WOG008.
   GS_COLLECT_BMFY-WOG009 = GS_COSP-WOG009.
   GS_COLLECT_BMFY-WOG010 = GS_COSP-WOG010.
   GS_COLLECT_BMFY-WOG011 = GS_COSP-WOG011.
   GS_COLLECT_BMFY-WOG012 = GS_COSP-WOG012.
   COLLECT GS_COLLECT_BMFY INTO GT_COLLECT_BMFY .

  ENDLOOP.

  SORT GT_COLLECT_BMFY BY OBJNR GJAHR .

  LOOP AT GT_COSS INTO GS_COSS .
   CLEAR:GS_COLLECT_BMFY.
   GS_COLLECT_BMFY-OBJNR = GS_COSS-OBJNR.
   GS_COLLECT_BMFY-GJAHR = GS_COSS-GJAHR.
   GS_COLLECT_BMFY-WOG001 = GS_COSS-WOG001.
   GS_COLLECT_BMFY-WOG002 = GS_COSS-WOG002.
   GS_COLLECT_BMFY-WOG003 = GS_COSS-WOG003.
   GS_COLLECT_BMFY-WOG004 = GS_COSS-WOG004.
   GS_COLLECT_BMFY-WOG005 = GS_COSS-WOG005.
   GS_COLLECT_BMFY-WOG006 = GS_COSS-WOG006.
   GS_COLLECT_BMFY-WOG007 = GS_COSS-WOG007.
   GS_COLLECT_BMFY-WOG008 = GS_COSS-WOG008.
   GS_COLLECT_BMFY-WOG009 = GS_COSS-WOG009.
   GS_COLLECT_BMFY-WOG010 = GS_COSS-WOG010.
   GS_COLLECT_BMFY-WOG011 = GS_COSS-WOG011.
   GS_COLLECT_BMFY-WOG012 = GS_COSS-WOG012.
   COLLECT GS_COLLECT_BMFY INTO GT_COLLECT_BMFY .

  ENDLOOP.
   SORT GT_COLLECT_BMFY BY OBJNR GJAHR .
  FIELD-SYMBOLS: <LFS_WOG>.
  DATA: L_FIELD  TYPE STRING.
LOOP AT GT_COLLECT INTO GS_COLLECT .
 CLEAR:GS_DATA.

  GS_DATA-BUKRS = P_BUKRS.    "公司代码
  GS_DATA-KOSTL = GS_COLLECT-KOSTL.   "成本中心
  GS_DATA-GJAHR = GS_COLLECT-GJAHR.   "年度
  GS_DATA-MONAT = GS_COLLECT-MONAT.   "期间

  "读取公司货币码
  READ TABLE GT_T001 INTO GS_T001
   WITH KEY   BUKRS = P_BUKRS BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      GS_DATA-WAERS = GS_T001-WAERS.
    ENDIF.

  CLEAR:L_FIELD.

  CONCATENATE 'WOG0' GS_DATA-MONAT INTO L_FIELD .

  "成本中心类型
  READ TABLE GT_CSKS INTO GS_CSKS
    WITH KEY KOSTL = GS_COLLECT-KOSTL BINARY SEARCH .
    IF  SY-SUBRC EQ 0 .
        GS_DATA-KOSAR = GS_CSKS-KOSAR.
        READ TABLE  GT_COLLECT_BMFY INTO GS_COLLECT_BMFY
           WITH KEY  OBJNR = GS_CSKS-OBJNR
                     GJAHR = GS_DATA-GJAHR  BINARY SEARCH.
         IF SY-SUBRC EQ 0 .
             "读取当前期间的本币对象货币值
             ASSIGN COMPONENT L_FIELD OF STRUCTURE GS_COLLECT_BMFY TO <LFS_WOG> .
            GS_DATA-BMFY = <LFS_WOG>.
         ENDIF.
     ENDIF.
     "成本中心描述
  READ TABLE GT_CSKT INTO GS_CSKT
    WITH KEY KOSTL = GS_COLLECT-KOSTL BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
      GS_DATA-KOSTL_TXT = GS_CSKT-LTEXT.
    ENDIF.

    "销售收入
  READ TABLE GT_COLLECT_XS INTO GS_COLLECT_XS
    WITH KEY KOSTL = GS_COLLECT-KOSTL
             GJAHR = GS_COLLECT-GJAHR
             MONAT = GS_COLLECT-MONAT BINARY SEARCH.
   IF SY-SUBRC EQ 0 .
       GS_DATA-XSSR = GS_COLLECT_XS-XSSR.
   ENDIF.

 "生产产值
    READ TABLE GT_COLLECT_CZ INTO GS_COLLECT_CZ
         WITH KEY KOSTL = GS_COLLECT-KOSTL
         GJAHR = GS_COLLECT-GJAHR
         MONAT = GS_COLLECT-MONAT BINARY SEARCH.
   IF SY-SUBRC EQ 0 .
       GS_DATA-SCCZ = GS_COLLECT_CZ-SCCZ.
   ENDIF.

   APPEND GS_DATA TO GT_DATA.

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
  INIT_FIELDCAT 'BUKRS'          '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'          '成本中心'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL_TXT'      '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSAR'          '成本类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'          '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XSSR'           '销售收入'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SCCZ'           '生产产值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BMFY'           '部门费用'         '' '' '' '' '' '' ''.
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
*      -->P_0375   text
*      -->P_0376   text
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
ENDFORM.                    " FRM_OUTPUT

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
*&---------------------------------------------------------------------*
*&      Form  GETF4HELP_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GETF4HELP_MONTH .
* DATA:l_actual_month TYPE isellist-month.
*
*  l_actual_month = sy-datum+0(6).
*
*  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
*    EXPORTING
*      actual_month                     =   l_actual_month
**     FACTORY_CALENDAR                 = ' '
**     HOLIDAY_CALENDAR                 = ' '
**     LANGUAGE                         = SY-LANGU
**     START_COLUMN                     = 8
**     START_ROW                        = 5
*    IMPORTING
*      selected_month                   = s_gzqj-low
**     RETURN_CODE                      =
*   EXCEPTIONS
*     factory_calendar_not_found       = 1
*     holiday_calendar_not_found       = 2
*     month_not_found                  = 3
*     OTHERS                           = 4.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*ENDFORM.
*
*FORM getf4help_month1 .
*  DATA:l_actual_month TYPE isellist-month.
*
*  l_actual_month = sy-datum+0(6).
*
*  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
*    EXPORTING
*      actual_month                     =   l_actual_month
**     FACTORY_CALENDAR                 = ' '
**     HOLIDAY_CALENDAR                 = ' '
**     LANGUAGE                         = SY-LANGU
**     START_COLUMN                     = 8
**     START_ROW                        = 5
*    IMPORTING
*      selected_month                   = s_gzqj-high
**     RETURN_CODE                      =
*   EXCEPTIONS
*     factory_calendar_not_found       = 1
*     holiday_calendar_not_found       = 2
*     month_not_found                  = 3
*     OTHERS                           = 4.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*ENDFORM.                    " GETF4HELP_MONTH
