REPORT ZCO005_4.
* DES:非合同项目/整改项目汇总表
* DATE 20160516 IT02（魏云）

TABLES:BKPF,BSEG,VBAK,MAKT,SKAT,ZFI006,COSP.

TYPES:BEGIN OF TY_TOTAL,
       BUKRS    TYPE     BKPF-BUKRS,                 "公司代码
       GJAHR    TYPE     BKPF-GJAHR,                 "会计年度
       MONAT    TYPE     BKPF-MONAT,                 "会计期间
       VBELN    TYPE     VBAK-VBELN,                 "销售订单
       XMMC     TYPE     CHAR300,                    "项目名称
   "  POSN2 TYPE BSEG-POSN2,                         " 销售订单行号
       AUART     TYPE      VBAK-AUART,               "订单类型
       YSYEAR    TYPE      BSEG-GJAHR,               "验收年度
       DQCPCB    TYPE      BSEG-DMBTR,               "当期产品成本
       DYGCFY    TYPE      BSEG-DMBTR,               "当月工程费用
       LJCPCB    TYPE      COSP-WOG001,              "累计产品成本
       LJGCFY    TYPE      COSP-WOG001,               "累计工程费用
       YZCB      TYPE      P  LENGTH 15 DECIMALS 2,   "应转成本
       JZKM      TYPE      BSEG-HKONT,                "结转科目
       JZKM_TXT  TYPE      STRING ,                   "结转科目描述
       DFKM      TYPE      BSEG-HKONT,                "对方科目
       DFKM_TXT  TYPE      STRING,                     "对方科目描述
       PZLX      TYPE      BKPF-BLART,               "凭证类型
       JSFLX     TYPE      C,                        "接收方类型
       DYXSDD    TYPE      VBELN,                    "对应销售订单
       DYXSMC    TYPE      CHAR300,                  "对应销售订单名称
       DYNBDD    TYPE      AUFNR ,                   "对应内部订单
       DYNBMC    TYPE      STRING,                   "对应内部订单名称
       DYCBZX    TYPE      KOSTL,                     "对应成本中心
       DYCBMC    TYPE      STRING,                     "对应成本中心名称
       GZRQ      TYPE      BKPF-BUDAT,                 "过账日期
       BKTXT     TYPE      BKPF-BKTXT,               "文本
       GZPZ      TYPE      BKPF-BELNR,               "过账凭证
       GZPZND    TYPE      BSEG-GJAHR,               "过账凭证年度
       WAERS     TYPE      BKPF-WAERS,               "货币码
       INFO_MSG  TYPE      STRING,                   "消息
       CELLSTYLE TYPE LVC_T_STYL,                     "单元格状态
     "XMMC   TYPE STRING ,       "项目名称
       STATU    TYPE   ICONNAME,                      "过账状态栏
       SEL(1) TYPE C ,
 END OF TY_TOTAL .

 TYPES:BEGIN OF TY_VBAP,
       VBELN TYPE VBELN_VA,
       POSNR TYPE POSNR_VA,
       PSTYV TYPE PSTYV,       "销售凭证行项目类别
       AUART TYPE AUART,       "订单类型
       OBJNR TYPE OBJPO,       "对象号
       END OF TY_VBAP .

TYPES:BEGIN OF TY_SUM,
     BUKRS TYPE BKPF-BUKRS,     "公司代码
     GJAHR TYPE C   LENGTH 4 ,  "会计年度
     MONAT TYPE C   LENGTH 2,   "会计期间
     VBELN TYPE VBAK-VBELN,     "销售订单
     DQCPCB   TYPE    BSEG-DMBTR,               "当期产品成本
     DYGCFY   TYPE    BSEG-DMBTR,               "当月工程费用
     LJCPCB TYPE COSP-WOG001,    "累计产品成本
     LJGCFY TYPE COSP-WOG001,    "累计工程费用
     END OF TY_SUM .

DATA:GT_VBAP TYPE TABLE OF TY_VBAP,
     GS_VBAP TYPE TY_VBAP.

DATA:GT_VBAP_2 TYPE TABLE OF TY_VBAP,
     GS_VBAP_2 TYPE TY_VBAP.

DATA:GT_COSP TYPE TABLE OF COSP,
     GS_COSP TYPE  COSP.

DATA:GT_SUM TYPE TABLE OF TY_SUM,
     GS_SUM TYPE TY_SUM.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA:GS_ZCO005_5 TYPE ZCO005_5 ,
     GT_ZCO005_5 TYPE TABLE OF ZCO005_5 .

DATA:GS_ZCO005_5_2 TYPE ZCO005_5 ,
     GT_ZCO005_5_2 TYPE  TABLE OF ZCO005_5 .

DATA:GT_VBAP_DY TYPE TABLE OF VBAP,
     GS_VBAP_DY TYPE VBAP .

DATA:GT_VBAK TYPE TABLE OF VBAK,
     GS_VBAK TYPE VBAK .

DATA: G_OBJNAME TYPE THEAD-TDNAME.

DATA: IT_LINES TYPE TABLE OF TLINE,
      WA_LINES TYPE TLINE.

 DATA:GT_CSKT TYPE  TABLE OF CSKT,
       GS_CSKT TYPE CSKT .

DATA:GT_SKAT TYPE TABLE OF SKAT,
     GS_SKAT TYPE SKAT .

  DATA:GT_COAS TYPE TABLE OF COAS,
       GS_COAS TYPE COAS.

 "声明类及定义方法来处理data_changed_finished事件
CLASS lcl_event_receiver DEFINITION.
    PUBLIC SECTION.
    METHODS:
      handle_onf4 for event onf4 of cl_gui_alv_grid
     importing e_fieldname es_row_no er_event_data et_bad_cells e_display,
     handle_modify  FOR EVENT data_changed_finished OF cl_gui_alv_grid
     IMPORTING e_modified  et_good_cells ."E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
ENDCLASS.

DATA:GT_TOTAL TYPE TABLE OF TY_TOTAL,
     GS_TOTAL TYPE TY_TOTAL.

DATA:GS_ZCO005_1 LIKE ZCO005_1 ,
     GT_ZCO005_1 LIKE TABLE OF ZCO005_1 .


*DATA:GT_COAS TYPE TABLE OF COAS,
*     GS_COAS TYPE COAS.
*
*DATA:GT_CSKT TYPE TABLE OF CSKT,
*     GS_CSKT TYPE CSKT .

DATA:GT_T001 TYPE  TABLE OF T001,
     GS_T001 TYPE T001.

 FIELD-SYMBOLS:" <FS_DATA>  LIKE GS_DATA,
                <FS_TOTAL> LIKE GS_TOTAL.

 DEFINE INIT_FIELDCAT.      "  ALV FIELDCAT SETTING
  GW_LVC-FIELDNAME = &1.
  GW_LVC-COLTEXT   = &2.
  GW_LVC-SCRTEXT_L = &2.
  GW_LVC-SCRTEXT_M = &2.
  GW_LVC-SCRTEXT_S = &2.
  GW_LVC-REPTEXT   = &2.
  GW_LVC-OUTPUTLEN = &3.
  GW_LVC-KEY = &4.
*  IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
  IF &1 = 'LJCPCB' OR &1 = 'LJGCFY'." OR  &1 = 'YZCB'.
  GW_LVC-CFIELDNAME = 'WAERS'.
 ENDIF.
 IF &1 = 'YZCB'.
   GW_LVC-NO_SIGN = ''.
   GW_LVC-DECIMALS = 2.
 ENDIF.
 IF &1 = 'JZKM' OR &1 = 'DFKM' .
   gw_lvc-f4availabl = 'X'.
 ENDIF.
  GW_LVC-CHECKBOX = &5.
  GW_LVC-EDIT = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GW_LVC-HOTSPOT   = &7.
  GW_LVC-REF_TABLE = &8.
  GW_LVC-REF_FIELD = &9.
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
DATA gt_event_receiver TYPE REF TO lcl_event_receiver .



DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.
DATA: GW_ISTABLE TYPE LVC_S_STBL.

************************************************************************
* BAPI
************************************************************************
DATA: WA_DOCUMENTHEADER    TYPE BAPIACHE09,         "表头
      IT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,  "总账
      WA_ACCOUNTGL         TYPE BAPIACGL09,
      IT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,  "客户
      WA_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
      IT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,  "货币项目
      WA_CURRENCYAMOUNT    TYPE BAPIACCR09,
      IT_CRITERIA          TYPE TABLE OF BAPIACKEC9,  "分配-科目
      WA_CRITERIA          TYPE BAPIACKEC9,
      IT_VALUEFIELD        TYPE TABLE OF BAPIACKEV9,
      WA_VALUEFIELD        TYPE BAPIACKEV9,
      IT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
      WA_EXTENSION2        TYPE BAPIPAREX,
      IT_RETURN            TYPE TABLE OF BAPIRET2,
      WA_RETURN            TYPE BAPIRET2.

DATA: WA_OBJ TYPE BAPIACHE09.

DATA: WA_ZACCDOCUEXT TYPE ZACCDOCUEXT.

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
P_GJAHR  TYPE BKPF-GJAHR OBLIGATORY,                      "年度
P_MONAT  TYPE BKPF-MONAT OBLIGATORY.                      "期间
SELECT-OPTIONS: S_VBELN FOR BSEG-VBEL2.    "销售订单
SELECTION-SCREEN END OF BLOCK BLK1.

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

" *&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_0558   TEXT
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
  "根据相应的基础限制条件取出销售明细
  SELECT  VBAK~VBELN VBAK~AUART VBAP~POSNR
          VBAP~PSTYV VBAP~OBJNR
   FROM VBAK
   INNER JOIN VBAP
   ON VBAK~VBELN = VBAP~VBELN
   INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
   WHERE VBAK~BUKRS_VF EQ P_BUKRS
      AND VBAK~VBELN IN S_VBELN
      AND VBAK~AUART IN ('ZF1','ZZG')
      AND VBAP~PSTYV IN ('Z41','Z42','Z01','Z02').
  SORT GT_VBAP BY OBJNR .

 "GT_VBAP_2 存储销售明细的最小虚拟行
  GT_VBAP_2 = GT_VBAP .
  SORT GT_VBAP_2 BY VBELN POSNR .
  DELETE ADJACENT DUPLICATES FROM  GT_VBAP_2 COMPARING VBELN .
  SORT GT_VBAP_2  BY VBELN .

 "取出销售明细对应的实际明细
 SELECT * INTO TABLE GT_COSP
   FROM COSP
   FOR ALL ENTRIES IN GT_VBAP
   WHERE OBJNR = GT_VBAP-OBJNR
   AND GJAHR <= P_GJAHR
   AND WRTTP = '04'
   AND  BUKRS EQ P_BUKRS
   AND  ( ( KSTAR BETWEEN '6404000000'  AND '6404999999'  )
         OR
       (  KSTAR BETWEEN  '8000000000'  AND '8999999999' ) )
  .

 SORT GT_COSP BY OBJNR KSTAR.

"取出对应的销售订单维护关系
 SELECT * INTO TABLE GT_ZCO005_5
   FROM ZCO005_5
   WHERE BUKRS EQ P_BUKRS
   AND   VBELN IN  S_VBELN .

   SORT  GT_ZCO005_5 BY VBELN.

  "GT_ZCO005_5_2 只保存自建维护管关系：接收方类型为：S
   GT_ZCO005_5_2 = GT_ZCO005_5 .
   DELETE GT_ZCO005_5_2 WHERE  JSFLX NE 'S'.
   SORT GT_ZCO005_5_2 BY VBELN .
   IF GT_ZCO005_5_2 IS NOT INITIAL.
    "取出自建表对应销售订单维护的对应销售明细虚拟首行
    SELECT * INTO TABLE GT_VBAP_DY
      FROM VBAP
      FOR ALL ENTRIES IN GT_ZCO005_5_2
      WHERE VBELN = GT_ZCO005_5_2-DYXSDD
      AND  PSTYV IN ('Z41','Z42','Z01','Z02','Z31','Z32','Z21','Z22') .

   SORT  GT_VBAP_DY BY VBELN POSNR  .
   DELETE ADJACENT DUPLICATES FROM GT_VBAP_DY COMPARING VBELN .
   SORT GT_VBAP_DY BY VBELN .

   ENDIF.


 SORT GT_ZCO005_5 BY VBELN .
   IF GT_ZCO005_5 IS NOT INITIAL.
     "取出成本中心描述
     SELECT * INTO TABLE GT_CSKT
       FROM CSKT
       FOR  ALL  ENTRIES IN GT_ZCO005_5
       WHERE KOSTL = GT_ZCO005_5-DYCBZX
       AND  KOKRS = '1000'
       AND  SPRAS = '1'.
     SORT  GT_CSKT BY  KOSTL .
     "取出内部订单描述
     SELECT * INTO TABLE GT_COAS
       FROM COAS
       FOR  ALL  ENTRIES IN GT_ZCO005_5
       WHERE AUFNR = GT_ZCO005_5-DYNBDD .
     SORT  GT_COAS BY AUFNR .
  ENDIF.

  "取出ZF1、ZZG订单抬头信息
  SELECT * INTO TABLE GT_VBAK
    FROM VBAK
    WHERE  BUKRS_VF EQ P_BUKRS
    AND AUART IN ('ZF1','ZZG')
    AND VBELN IN S_VBELN.
  SORT GT_VBAK BY VBELN .

  "取出科目表主数据
  SELECT * INTO TABLE GT_SKAT
    FROM SKAT
    WHERE   KTOPL = '1000'
       AND  SPRAS = '1'.
 SORT GT_SKAT BY SAKNR .

 "取出公司代码主数据
 SELECT * INTO TABLE GT_T001
   FROM T001
   WHERE BUKRS EQ P_BUKRS .
  SORT  GT_T001 BY BUKRS .
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
   DATA:WOGLJ TYPE COSP-WOG001,
        WOGQJ TYPE COSP-WOG001.
   DATA:P_LJ TYPE I  .
   DATA:P_MONAT_01 TYPE BKPF-MONAT .
   DATA: L_FIELD  TYPE STRING.
   DATA:WAERS TYPE WAERS .
   DATA:F_FIRST TYPE D,
        F_END TYPE D.
   FIELD-SYMBOLS: <LFS_COSP>.
  "取出公司代码本币
 READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = P_BUKRS
                          BINARY SEARCH .
   IF SY-SUBRC EQ 0 .

     WAERS = GS_T001-WAERS .

   ENDIF.

    "月初第一天
 CONCATENATE P_GJAHR P_MONAT '01' INTO F_FIRST.
 "月末最后一天
 CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
   EXPORTING
     I_DATE        = F_FIRST
 IMPORTING
    E_DATE        = F_END .
   P_LJ = P_MONAT - 1.
   P_MONAT_01 = P_MONAT.

 "按照公司代码、年度、销售订单汇总 当期产品成本、当月工程费用
  "及
  "累计产品成本、累计工程费用
   LOOP AT GT_COSP INTO GS_COSP .
     CLEAR:GS_SUM,WOGLJ,WOGQJ .
     GS_SUM-BUKRS = P_BUKRS.
     GS_SUM-GJAHR = P_GJAHR.
     GS_SUM-MONAT = P_MONAT .
     GS_SUM-VBELN = GS_COSP-OBJNR+2(7).
     IF GS_COSP-GJAHR < P_GJAHR .
        WOGLJ = GS_COSP-WOG001 + GS_COSP-WOG002 + GS_COSP-WOG003
                + GS_COSP-WOG004 + GS_COSP-WOG005 + GS_COSP-WOG006
                + GS_COSP-WOG007 + GS_COSP-WOG008 + GS_COSP-WOG009
                + GS_COSP-WOG010 + GS_COSP-WOG011 + GS_COSP-WOG012
                + GS_COSP-WOG013 + GS_COSP-WOG014 + GS_COSP-WOG015
                + GS_COSP-WOG016 .

       ELSE.
         CONCATENATE 'WOG0' P_MONAT INTO L_FIELD .
         ASSIGN COMPONENT L_FIELD OF STRUCTURE GS_COSP TO <LFS_COSP> .
            IF SY-SUBRC EQ 0 .
               WOGQJ =  <LFS_COSP> .
               WOGLJ = WOGLJ + <LFS_COSP> .
            ENDIF.
         P_MONAT_01 = '01'.
         DO P_LJ TIMES.
            CONCATENATE 'WOG0' P_MONAT_01 INTO L_FIELD .
            ASSIGN COMPONENT L_FIELD OF STRUCTURE GS_COSP TO <LFS_COSP> .
            IF SY-SUBRC EQ 0 .
               WOGLJ = WOGLJ + <LFS_COSP> .

            ENDIF.
           P_MONAT_01 = P_MONAT_01 + 1.
         ENDDO.

     ENDIF.

     IF GS_COSP-KSTAR BETWEEN '6404000000'  AND '6404999999'.
        GS_SUM-LJCPCB =  WOGLJ .
        GS_SUM-DQCPCB =  WOGQJ.
        COLLECT GS_SUM INTO GT_SUM .
        CONTINUE.
    ELSE.
       GS_SUM-LJGCFY =  WOGLJ .
       GS_SUM-DYGCFY =  WOGQJ .
       COLLECT GS_SUM INTO GT_SUM .
       CONTINUE.
     ENDIF.
   ENDLOOP.
SORT GT_SUM BY GJAHR MONAT VBELN .

"根据汇总的总计表展开 赋值对应的主数据
LOOP AT GT_SUM INTO  GS_SUM .
  CLEAR:GS_TOTAL.
   MOVE-CORRESPONDING GS_SUM TO GS_TOTAL.
   "货币码
   GS_TOTAL-WAERS = WAERS .
   "凭证类型
   GS_TOTAL-PZLX = 'SA'.
   "过账日期
   GS_TOTAL-GZRQ = F_END .
   "文本
   GS_TOTAL-BKTXT = '结转非合同项目成本'.
   "订单类型
   READ TABLE GT_VBAK INTO GS_VBAK
        WITH KEY VBELN = GS_TOTAL-VBELN
                 BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
       GS_TOTAL-AUART = GS_VBAK-AUART .
   ENDIF.
     "项目名称
   PERFORM SELXMMC USING GS_TOTAL-VBELN CHANGING GS_TOTAL-XMMC.

  "读取销售订单维护的对应关系
   READ TABLE GT_ZCO005_5 INTO GS_ZCO005_5
               WITH KEY  VBELN = GS_SUM-VBELN BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
        "接收方类型
     GS_TOTAL-JSFLX = GS_ZCO005_5-JSFLX.
       "对应销售订单
     GS_TOTAL-DYXSDD = GS_ZCO005_5-DYXSDD.
        "项目名称
     IF GS_TOTAL-DYXSDD IS NOT INITIAL.
         PERFORM SELXMMC USING GS_TOTAL-DYXSDD CHANGING GS_TOTAL-DYXSMC.
       ELSE.
         "对应内部订单
         GS_TOTAL-DYNBDD = GS_ZCO005_5-DYNBDD.
         READ TABLE GT_COAS INTO GS_COAS
         WITH KEY AUFNR = GS_TOTAL-DYNBDD
                  BINARY SEARCH .
         IF SY-SUBRC EQ 0 .
             GS_TOTAL-DYNBMC = GS_COAS-KTEXT.
         ENDIF.
         "对应成本中心
         GS_TOTAL-DYCBZX = GS_ZCO005_5-DYCBZX.
         READ TABLE GT_CSKT INTO GS_CSKT
         WITH KEY KOSTL = GS_TOTAL-DYCBZX
                  BINARY SEARCH .
         IF SY-SUBRC EQ 0 .
            GS_TOTAL-DYCBMC = GS_CSKT-KTEXT.
          ENDIF.
     ENDIF.
   ENDIF.

 IF GS_TOTAL-JSFLX EQ ''.

  ELSE.
    "应转成本
    GS_TOTAL-YZCB = GS_TOTAL-LJCPCB + GS_TOTAL-LJGCFY .
     "赋值对方成本相应赋值关系
     IF GS_TOTAL-JSFLX EQ 'V'.
        GS_TOTAL-DFKM =  '8003000048'.    "销售费用-展会费
        GS_TOTAL-DFKM_TXT = '销售费用-展会费'.
       ELSEIF GS_TOTAL-JSFLX EQ 'E'.
          GS_TOTAL-DFKM =  '8003000064'.   "研发费用-材料及物料费
          GS_TOTAL-DFKM_TXT = '研发费用-材料及物料费'.
         ELSEIF GS_TOTAL-JSFLX EQ 'S'.
         GS_TOTAL-DFKM =  '8008000039'.    "项目成本-整备费
         GS_TOTAL-DFKM_TXT = '项目成本-整备费'.
      ENDIF.
 ENDIF.
  "结转科目
   GS_TOTAL-JZKM = '8008000025'.    "工程成本-其他费用
   GS_TOTAL-JZKM_TXT = '工程成本-其他费用'.
  "对方科目

  APPEND  GS_TOTAL TO GT_TOTAL .

ENDLOOP.
SORT GT_TOTAL BY BUKRS VBELN .
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
                            GT_TOTAL
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
  GW_LAYOUT-BOX_FNAME    = 'SEL'.
   GW_LAYOUT-STYLEFNAME = 'CELLSTYLE'.

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
  INIT_FIELDCAT 'STATU'         '过账状态'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'         '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'         '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'         '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VBELN'         '销售订单'         '' '' '' '' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'XMMC'          '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'         '订单类型'         '' '' '' '' '' '' ''.
 " INIT_FIELDCAT 'YSYEAR'        '验收年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQCPCB'        '当期产品成本'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYGCFY'        '当月工程费用'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJCPCB'        '累计产品成本'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJGCFY'        '累计工程费用'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YZCB'          '应转成本'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'JZKM'          '结转科目'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'JZKM_TXT'      '结转科目描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DFKM'          '对方科目'         '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'DFKM_TXT'      '对方科目描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYXSDD'         '对应销售销售订单'         '' '' '' '' '' 'VBAK' 'VBELN'.
  INIT_FIELDCAT 'DYXSMC'         '对应销售销售订单名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYNBDD'         '对应内部订单'         '' '' '' '' '' 'COAS' 'AUFNR'.
  INIT_FIELDCAT 'DYNBMC'         '对应内部订单名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DYCBZX'         '对应成本中心'         '' '' '' '' '' 'BSEG' 'KOSTL'.
  INIT_FIELDCAT 'DYCBMC'         '对应成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'JSFLX'          '接收方类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PZLX'          '凭证类型'         '' '' '' 'X' '' 'BKPF' 'BLART'.
  INIT_FIELDCAT 'GZRQ'          '过账日期'         '' '' '' 'X' '' 'BKPF' 'BUDAT'.
  INIT_FIELDCAT 'BKTXT'         '文本'             '' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'GZPZ'          '过账凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GZPZND'        '过账凭证年度'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INFO_MSG'      '过账消息文本'     '' '' '' '' '' '' ''.
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
  GW_EVENTS-NAME =  'CALLER_EXIT' .
  GW_EVENTS-FORM =  'FRM_BUTTON'.   "f4事件
  APPEND GW_EVENTS TO GT_EVENTS.
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.  "单元格修改回车事件
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_TOTAL  text
*      -->P_0590   text
*      -->P_0591   text
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
      T_OUTTAB                 = GT_TOTAL
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_modify.
     perform handle_data_changed_finished using e_modified et_good_cells.
  ENDMETHOD.                    "HANDLE_MODIFY

  METHOD handle_onf4.
      FIELD-SYMBOLS <FS_MOD_CELLS> TYPE LVC_T_MODI.
  DATA: LW_MOD_CELL TYPE LVC_S_MODI.

   CASE e_fieldname.
     WHEN 'JZKM'.
        READ TABLE GT_TOTAL INTO GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_HKONT CHANGING GS_TOTAL-JZKM.
            IF GS_TOTAL-JZKM IS NOT INITIAL.
              MODIFY GT_TOTAL FROM GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'JZKM'.
               LW_MOD_CELL-VALUE = GS_TOTAL-JZKM.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.

    WHEN 'DFKM'.
         READ TABLE GT_TOTAL INTO GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
         IF SY-SUBRC = 0.
            PERFORM SUB_HELP_HKONT CHANGING GS_TOTAL-DFKM.
            IF GS_TOTAL-DFKM IS NOT INITIAL.
              MODIFY GT_TOTAL FROM GS_TOTAL INDEX ES_ROW_NO-ROW_ID.
               ASSIGN ER_EVENT_DATA->M_DATA->* TO <FS_MOD_CELLS>.
               LW_MOD_CELL-ROW_ID = ES_ROW_NO-ROW_ID.
               LW_MOD_CELL-SUB_ROW_ID = ES_ROW_NO-SUB_ROW_ID.
               LW_MOD_CELL-FIELDNAME = 'DFKM'.
               LW_MOD_CELL-VALUE = GS_TOTAL-DFKM.
               APPEND LW_MOD_CELL TO <FS_MOD_CELLS>.

            ENDIF.
         ENDIF.

   ENDCASE.

  "**  Inform ALV Grid that event 'onf4' has been processed
  er_event_data->M_EVENT_HANDLED = 'X'.           "告知F4动作结束
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  SUB_HELP_HKONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_TOTAL_JZKM  text
*----------------------------------------------------------------------*
FORM SUB_HELP_HKONT   CHANGING P_HKONT..
" SET PARAMETER ID 'BUK' FIELD WA_HEAD-BUKRS.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      MATCHCODE_OBJECT          = 'SAKO'
*     TABNAME                   = ' '
    IMPORTING
      SELECT_VALUE              = P_HKONT
    EXCEPTIONS
      INVALID_DICTIONARY_FIELD  = 1
      INVALID_MATCHDCODE_OBJECT = 2
      NO_SELECTION              = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM SELXMMC  USING    P_VBELN TYPE VBELN
              CHANGING P_XMMC TYPE CHAR300.
  " 取项目名称 - 销售订单抬头文本
    G_OBJNAME = P_VBELN.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        ID                      = 'Z001'
        LANGUAGE                = '1'
        NAME                    = G_OBJNAME
        OBJECT                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
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
*&      Form  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED_FINISHED  USING    P_E_MODIFIED
                                            P_ET_GOOD_CELLS TYPE  LVC_T_MODI.

DATA:LW_CELL TYPE LVC_S_MODI.
DATA: LS_STABLE TYPE LVC_S_STBL.
READ TABLE   P_ET_GOOD_CELLS INTO LW_CELL INDEX 1."  WITH KEY FIELDNAME = 'DYNBDD'.
 IF SY-SUBRC EQ 0 .
   CASE LW_CELL-FIELDNAME .
     WHEN 'DFKM'.
      READ TABLE GT_TOTAL INTO GS_TOTAL  INDEX  LW_CELL-ROW_ID.
      IF SY-SUBRC EQ 0 .
         CLEAR:GS_TOTAL-DFKM_TXT.
         READ TABLE GT_SKAT INTO GS_SKAT
               WITH KEY SAKNR = GS_TOTAL-DFKM
                 BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
              GS_TOTAL-DFKM_TXT = GS_SKAT-TXT50 .
           ENDIF.
          "刷新数据到ALV
          MODIFY GT_TOTAL FROM GS_TOTAL INDEX  LW_CELL-ROW_ID.
          CLEAR GS_TOTAL.

      ENDIF.

     WHEN 'JZKM'.
         READ TABLE GT_TOTAL INTO GS_TOTAL  INDEX  LW_CELL-ROW_ID.
      IF SY-SUBRC EQ 0 .
           CLEAR:GS_TOTAL-JZKM_TXT.
           READ TABLE GT_SKAT INTO GS_SKAT
               WITH KEY SAKNR = GS_TOTAL-JZKM
                 BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
              GS_TOTAL-JZKM_TXT = GS_SKAT-TXT50 .
           ENDIF.
           "刷新数据到ALV
          MODIFY GT_TOTAL FROM GS_TOTAL INDEX  LW_CELL-ROW_ID.
          CLEAR GS_TOTAL.
      ENDIF.

    ENDCASE.

     "  *   稳定刷新
    LS_STABLE-row = 'X'." 基于行的稳定刷新
    LS_STABLE-col = 'X'." 基于列稳定刷新
    CALL METHOD GR_ALVGRID->refresh_table_display
      EXPORTING
        is_stable = LS_STABLE
      EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
 ENDIF.


ENDFORM.

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.

FORM FRM_DATA_CHANGED USING   ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
*                                    P_E_ONF4
*                                    P_E_ONF4_BEFORE
*                                    P_E_ONF4_AFTER
*                                    P_E_UCOMM TYPE SY-UCOMM.




  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .
*   CALL METHOD ref_grid->check_changed_data.
* 设置enter事件



  CALL METHOD  GR_ALVGRID->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>MC_EVT_MODIFIED
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

 CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->handle_modify FOR GR_ALVGRID.


ENDFORM.

FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

 " DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GR_ALVGRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
      READ TABLE GT_TOTAL INTO GS_TOTAL INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'GZPZ'
        AND GS_TOTAL-GZPZ IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_TOTAL-GZPZ.
        SET PARAMETER ID 'BUK' FIELD GS_TOTAL-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_TOTAL-GZPZND.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'VBELN'
        AND GS_TOTAL-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_TOTAL-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&POST'.
    DATA L_ANS.
    DATA L_SUBRC TYPE SY-SUBRC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = '确认过账'
*     DIAGNOSE_OBJECT       = ' '
      TEXT_QUESTION         = '确定要保存并过账吗？'
      TEXT_BUTTON_1         = '是'(B01)
*     ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = '否'(B02)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      ANSWER                = L_ANS
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK L_ANS EQ '1'.

  PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

  CHECK L_SUBRC EQ 0.

  "根据选中行过帐
  PERFORM POST_DATA.

    "刷新数据到内表
   CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.

  "判断是否已选中过账的号
  READ  TABLE GT_TOTAL INTO GS_TOTAL WITH KEY SEL = 'X'.
    IF SY-SUBRC NE 0.
      P_SUBRC = 4.
       MESSAGE '请选中相应的行再过账' TYPE 'E'.
    ENDIF.

 LOOP AT GT_TOTAL INTO GS_TOTAL WHERE SEL = 'X'.
       "CHECK 必填项


    "结转科目
     IF GS_TOTAL-JZKM IS INITIAL.
       MESSAGE TEXT-M03 TYPE 'S' DISPLAY LIKE 'E'.
        P_SUBRC = 4.
        EXIT.
      ENDIF.

    "对方科目
     IF GS_TOTAL-DFKM IS INITIAL.
       MESSAGE TEXT-M04 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
       EXIT.
     ENDIF.

    "凭证类型
     IF GS_TOTAL-PZLX IS INITIAL.
       MESSAGE TEXT-M05 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
       EXIT.
     ENDIF.

     "过账日期
     IF GS_TOTAL-GZRQ IS INITIAL.
       MESSAGE TEXT-M05 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
       EXIT.
     ENDIF.

     "过账文本
     CONDENSE GS_TOTAL-BKTXT NO-GAPS.
     IF GS_TOTAL-BKTXT IS INITIAL.
        MESSAGE TEXT-M06 TYPE 'S' DISPLAY LIKE 'E'.
       P_SUBRC = 4.
       EXIT.
      ENDIF.
 ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DATA .
    DATA: YZ TYPE i.
 REFRESH GT_ZCO005_1.
 LOOP AT GT_TOTAL INTO GS_TOTAL WHERE SEL = 'X' AND  GZPZ = ''.
   IF  GS_TOTAL-YZCB EQ 0 .
      GS_TOTAL-INFO_MSG = '应转成本为0的记录不能生成过账凭证'.
      MODIFY GT_TOTAL FROM GS_TOTAL.
      CONTINUE.
   ELSE.
    "凭证行:项目虚拟行
    CLEAR:GS_TOTAL-INFO_MSG,YZ.
       "付款记账数据抬头
    REFRESH: IT_ACCOUNTGL, it_accountgl, IT_CURRENCYAMOUNT, IT_CRITERIA, IT_VALUEFIELD, IT_EXTENSION2, IT_RETURN.
    CLEAR: WA_DOCUMENTHEADER, WA_OBJ.

     "* 抬头
**********************************************************************
*凭证日期
  WA_DOCUMENTHEADER-DOC_DATE     =  GS_TOTAL-GZRQ.
*过账日期
  WA_DOCUMENTHEADER-PSTNG_DATE   =  GS_TOTAL-GZRQ.
*凭证类型
  WA_DOCUMENTHEADER-DOC_TYPE     =  GS_TOTAL-PZLX.
*公司代码
  WA_DOCUMENTHEADER-COMP_CODE    =  GS_TOTAL-BUKRS.
*凭证抬头文本
  WA_DOCUMENTHEADER-HEADER_TXT   =  GS_TOTAL-BKTXT.
*创建人员
  WA_DOCUMENTHEADER-USERNAME     =  SY-UNAME.

 "凭证行
  "记账行
 " **********************************************************************

    CLEAR :WA_ZACCDOCUEXT.
    WA_ZACCDOCUEXT-POSNR = '0000000010'.
    IF GS_TOTAL-YZCB > 0 .
    WA_ZACCDOCUEXT-BSCHL =  '50'.
    ELSE.
      WA_ZACCDOCUEXT-BSCHL =  '40'.
    ENDIF.
    WA_EXTENSION2-STRUCTURE = 'ZACCDOCUEXT'.
    WA_EXTENSION2-VALUEPART1 = WA_ZACCDOCUEXT.
    APPEND WA_EXTENSION2 TO IT_EXTENSION2.

    WA_ZACCDOCUEXT-POSNR = '0000000020'.
     IF GS_TOTAL-YZCB > 0 .
    WA_ZACCDOCUEXT-BSCHL =  '40'.
    ELSE.
      WA_ZACCDOCUEXT-BSCHL =  '50'.
    ENDIF..
    WA_EXTENSION2-STRUCTURE = 'ZACCDOCUEXT'.
    WA_EXTENSION2-VALUEPART1 = WA_ZACCDOCUEXT.
    APPEND WA_EXTENSION2 TO IT_EXTENSION2.

        "科目表
   " **********************************************************************
    CLEAR:WA_ACCOUNTGL.
    WA_ACCOUNTGL-ITEMNO_ACC = '0000000010'.
    "结转科目
    WA_ACCOUNTGL-GL_ACCOUNT = GS_TOTAL-JZKM.
    "项目文本
    WA_ACCOUNTGL-ITEM_TEXT = GS_TOTAL-BKTXT.
    "销售订单
    WA_ACCOUNTGL-SALES_ORD = GS_TOTAL-VBELN.
    "行号为对应销售订单的虚拟行
    READ TABLE GT_VBAP_2 INTO GS_VBAP_2 WITH KEY VBELN = GS_TOTAL-VBELN
                         BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
    WA_ACCOUNTGL-S_ORD_ITEM = GS_VBAP_2-POSNR.
    ENDIF.
    "分配
    WA_ACCOUNTGL-ALLOC_NMBR = GS_TOTAL-VBELN.

    APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL .

    "科目表
    CLEAR:WA_ACCOUNTGL.
    WA_ACCOUNTGL-ITEMNO_ACC = '0000000020'.
    "对方科目
    WA_ACCOUNTGL-GL_ACCOUNT = GS_TOTAL-DFKM.
    "项目文本
    WA_ACCOUNTGL-ITEM_TEXT = GS_TOTAL-BKTXT.

    IF GS_TOTAL-JSFLX EQ 'S'.
               "销售订单
       WA_ACCOUNTGL-SALES_ORD = GS_TOTAL-DYXSDD.
       READ TABLE GT_VBAP_DY INTO GS_VBAP_DY WITH KEY VBELN = GS_TOTAL-DYXSDD
                             BINARY SEARCH.
         IF SY-SUBRC EQ 0 .

          WA_ACCOUNTGL-S_ORD_ITEM = GS_VBAP_DY-POSNR.
          ENDIF.
         "分配
         WA_ACCOUNTGL-ALLOC_NMBR = GS_TOTAL-DYXSDD.

    ELSEIF GS_TOTAL-DYNBDD NE ''.
          WA_ACCOUNTGL-COSTCENTER = GS_TOTAL-DYCBZX.
          WA_ACCOUNTGL-ORDERID    = GS_TOTAL-DYNBDD.
          WA_ACCOUNTGL-ALLOC_NMBR =  GS_TOTAL-DYNBDD.

    ENDIF.

    APPEND WA_ACCOUNTGL TO IT_ACCOUNTGL .

   "金额
    " **********************************************************************
   CLEAR WA_CURRENCYAMOUNT.
   WA_CURRENCYAMOUNT-ITEMNO_ACC = '0000000010'.
  " *       货币
      WA_CURRENCYAMOUNT-CURRENCY = GS_TOTAL-WAERS.
*       金额
  IF GS_TOTAL-YZCB > 0 .
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  * -1.
      ELSE.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  * -1  .
   ENDIF.

 APPEND WA_CURRENCYAMOUNT TO IT_CURRENCYAMOUNT.

 "金额
   CLEAR WA_CURRENCYAMOUNT.
   WA_CURRENCYAMOUNT-ITEMNO_ACC = '0000000020'.
  " *       货币
      WA_CURRENCYAMOUNT-CURRENCY = GS_TOTAL-WAERS.
*       金额
      IF GS_TOTAL-YZCB > 0 .
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB  .
      ELSE.
      WA_CURRENCYAMOUNT-AMT_DOCCUR = GS_TOTAL-YZCB .
       ENDIF.

 APPEND WA_CURRENCYAMOUNT TO IT_CURRENCYAMOUNT.

  "* "获利能力段
* CLEAR WA_CRITERIA.
* WA_CRITERIA-ITEMNO_ACC =  '0000000010'.
* WA_CRITERIA-FIELDNAME = 'KAUFN' ."销售订单
* WA_CRITERIA-CHARACTER = GS_TOTAL-VBEL2 .
* APPEND WA_CRITERIA TO IT_CRITERIA.
* WA_CRITERIA-FIELDNAME = 'KDPOS'. "销售订单行项目
* WA_CRITERIA-CHARACTER = GS_TOTAL-POSN2.
* APPEND WA_CRITERIA TO IT_CRITERIA.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = WA_DOCUMENTHEADER
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
     OBJ_TYPE          = WA_OBJ-OBJ_TYPE
     OBJ_KEY           = WA_OBJ-OBJ_KEY
     OBJ_SYS           = WA_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL         = IT_ACCOUNTGL
  "    ACCOUNTRECEIVABLE = IT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      CURRENCYAMOUNT    = IT_CURRENCYAMOUNT
     " CRITERIA          = IT_CRITERIA
    "  VALUEFIELD        = IT_VALUEFIELD
*     EXTENSION1        =
      RETURN            = IT_RETURN
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      EXTENSION2        = IT_EXTENSION2
*     REALESTATE        =
*     ACCOUNTWT         =
    .

   READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0 .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      "错误消息回写到ALV
      LOOP AT IT_RETURN INTO WA_RETURN .
        IF GS_TOTAL-INFO_MSG EQ ''.
           CONCATENATE WA_RETURN-ID WA_RETURN-TYPE WA_RETURN-MESSAGE
                       INTO GS_TOTAL-INFO_MSG .
         ELSE .
           CONCATENATE GS_TOTAL-INFO_MSG ';' WA_RETURN-ID
                       WA_RETURN-TYPE WA_RETURN-MESSAGE
                       INTO GS_TOTAL-INFO_MSG .
        ENDIF.
     ENDLOOP.
      "红灯状态：
      GS_TOTAL-STATU = ICON_RED_LIGHT .
      MODIFY GT_TOTAL FROM GS_TOTAL.
      CONTINUE.

  ELSE.
     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    " 回写生成的会计凭证号与会计凭证年度
     GS_TOTAL-GZPZ   =  WA_OBJ-OBJ_KEY(10).
     GS_TOTAL-GZPZND =  WA_OBJ-OBJ_KEY+14(4).
       "绿灯状态：
      GS_TOTAL-STATU = ICON_GREEN_LIGHT.
      "消息描述
       GS_TOTAL-INFO_MSG = '凭证过账成功！'.
   "设置不可以编辑状态
   "*设置不可以编辑状态
   PERFORM FRM_CELL_STYLE USING 'YZCB'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'JZKM'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'DFKM'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
   PERFORM FRM_CELL_STYLE USING 'PZLX'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'GZRQ'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    PERFORM FRM_CELL_STYLE USING 'BKTXT'
                           ''
                           CHANGING GS_TOTAL-CELLSTYLE.
    "过程成功的行写到GT_ZCO005_1
    CLEAR :GS_ZCO005_1.
    MOVE-CORRESPONDING GS_TOTAL TO GS_ZCO005_1.
    GS_ZCO005_1-VBEL2 = GS_TOTAL-VBELN.
      "创建人、创建日期、 时间
    GS_ZCO005_1-GZ_NAME = SY-UNAME.
    GS_ZCO005_1-GZ_DATE = SY-DATUM.
    GS_ZCO005_1-GZ_TIME = SY-UZEIT.
    APPEND GS_ZCO005_1 TO GT_ZCO005_1.
   ENDIF.
  ENDIF.
  MODIFY GT_TOTAL FROM GS_TOTAL.
  ENDLOOP.

  "保存数据到ZCO005_1物理表
   IF GT_ZCO005_1 IS NOT INITIAL.
      MODIFY ZCO005_1 FROM TABLE GT_ZCO005_1.
   ENDIF.

ENDFORM.

FORM FRM_BUTTON  USING E_GRID TYPE SLIS_DATA_CALLER_EXIT.
  DATA LT_F4 TYPE LVC_T_F4.
  DATA LS_F4 TYPE LVC_S_F4.

   LS_F4-FIELDNAME = 'JZKM'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
 ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

   LS_F4-FIELDNAME = 'DFKM'.      "F4对应的栏位
  LS_F4-REGISTER = 'X'.
  ls_f4-getbefore = 'X'.
  LS_F4-CHNGEAFTER = 'X'.
  INSERT LS_F4 INTO TABLE LT_F4.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
  IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
    e_grid                           = GR_ALVGRID
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

 CREATE OBJECT gt_event_receiver.
  SET HANDLER : gt_event_receiver->HANDLE_ONF4   FOR GR_ALVGRID.

  CALL METHOD GR_ALVGRID->register_f4_for_fields
     EXPORTING
       it_f4  = lt_f4[].
ENDFORM.

FORM FRM_CELL_STYLE  USING    P_FIELDNAME
                              P_EDITABLE
                     CHANGING PT_CELLSTYLE TYPE LVC_T_STYL.
  DATA: LW_CELLSTYLE TYPE LVC_S_STYL.

  READ TABLE PT_CELLSTYLE INTO LW_CELLSTYLE WITH KEY FIELDNAME = P_FIELDNAME.
  IF SY-SUBRC = 0.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    MODIFY TABLE PT_CELLSTYLE FROM LW_CELLSTYLE.
  ELSE.
    LW_CELLSTYLE-FIELDNAME = P_FIELDNAME.
    IF P_EDITABLE = 'X'.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LW_CELLSTYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.

    INSERT LW_CELLSTYLE INTO TABLE PT_CELLSTYLE.
  ENDIF.
ENDFORM.
