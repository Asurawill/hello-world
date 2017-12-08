REPORT ZFI035.
TABLES:BKPF,BSEG,ANLA.

DATA:BEGIN OF GS_DATA,
     YWLX TYPE  STRING ,                  "业务类型
     BUKRS LIKE BKPF-BUKRS,               "公司代码
     ANLN1 LIKE BSEG-ANLN1,               "资产编号
     ANLN1_TXT TYPE STRING,               "资产编号描述
     ANLAR LIKE ANLA-ANLAR,                "资产 类型
     BELNR LIKE BKPF-BELNR,               "会计凭证
     BUZEI LIKE BSEG-BUZEI,               "会计凭证行号
     GJAHR LIKE BKPF-GJAHR,               "会计年度
     AWKEY LIKE BKPF-AWKEY,                "参考凭证号
     SHKZG LIKE BSEG-SHKZG,               "借贷标识
     DMBTR LIKE BSEG-DMBTR,               "记账金额
     EBELN LIKE BSEG-EBELN,                "采购订单
     EBELP LIKE BSEG-EBELP,                "采购订单行项目
     BELNR2 LIKE BSEG-BELNR,               "发票凭证
     GJAHR2 LIKE BSEG-GJAHR,               "发票年度
     BUDAT LIKE BKPF-BUDAT,                "过账日期
     BZDAT LIKE BSEG-BZDAT,                "资产价值日
     BKTXT LIKE BKPF-BKTXT,                "摘要
     ANBWA LIKE BSEG-ANBWA,                "事务类型
     BLART LIKE BKPF-BLART,                "凭证类型
     HKONT LIKE BSEG-HKONT,                "总账科目
     HKONT_TXT TYPE STRING,                "科目描述
     CGFPH(1) ,                            "采购发票行标记
     SEL(1),
  END  OF GS_DATA.

DATA: BEGIN OF GS_YWLX ,
     YWLX LIKE BKPF-BKTXT ,                  "业务类型
      END OF GS_YWLX.

DATA: GT_DATA LIKE TABLE OF GS_DATA WITH HEADER LINE.


DATA:GT_DATA_02 LIKE  TABLE OF GS_DATA WITH HEADER LINE.

DATA:GT_APPEND LIKE TABLE OF GS_DATA WITH HEADER LINE.

DATA:GT_APPEND_02 LIKE TABLE OF GS_DATA WITH HEADER LINE.


DATA: GT_ANLA LIKE TABLE OF ANLA WITH HEADER LINE.

DATA: GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.

DATA:GT_RBKP LIKE TABLE OF RBKP WITH HEADER LINE.

DATA:GT_YWLX LIKE TABLE OF GS_YWLX WITH HEADER LINE.

 FIELD-SYMBOLS: <FS_DATA> LIKE GS_DATA.

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
    IF &1 = 'KUNNR'.
    GW_LVC-NO_ZERO = 'X'.
  ENDIF.
   IF &1 = 'WRBTR' .
   GW_LVC-CFIELDNAME = 'WAERS_1'.
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
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
SELECT-OPTIONS: S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,    "过账日期
                S_ANLN1  FOR BSEG-ANLN1,               "资产编码
                S_YWLX   FOR BKPF-BKTXT.                 "业务类型
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
  IMPORT P_BUKRS FROM MEMORY ID 'ZCMX_BUKRS' .
  PERFORM  APPENDYWLX .
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.

"F4搜索帮助
AT SELECTION-SCREEN on VALUE-REQUEST FOR  S_YWLX-LOW.
 perform getywlx.

 AT SELECTION-SCREEN on VALUE-REQUEST FOR  S_YWLX-HIGH.
 perform getywlx.

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
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
 SELECT BKPF~BUKRS BKPF~BELNR BKPF~GJAHR BKPF~BUDAT BKPF~BKTXT BKPF~BLART
        BSEG~BUZEI  BSEG~DMBTR BSEG~SHKZG BSEG~EBELN
        BSEG~EBELP BSEG~ANLN1 BSEG~BZDAT BSEG~ANBWA BSEG~HKONT
   "     ANLA~ANLAR ANLA~TXT50 AS ANLN1_TXT
  INTO CORRESPONDING FIELDS OF TABLE GT_DATA
  FROM  BKPF
   INNER JOIN BSEG
   ON BKPF~GJAHR = BSEG~GJAHR
   AND BKPF~BUKRS = BSEG~BUKRS
   AND BKPF~BELNR = BSEG~BELNR
  WHERE   BKPF~BUKRS = P_BUKRS
   AND   BKPF~BUDAT IN S_BUDAT
   AND   BSEG~ANLN1 NE ''
   AND   BSEG~ANLN1 NE '*'
   AND   BSEG~ANLN1 IN S_ANLN1.



SORT GT_DATA BY BUKRS GJAHR BUDAT BELNR BUZEI.

SELECT  * INTO CORRESPONDING FIELDS OF TABLE GT_ANLA
  FROM ANLA
  WHERE BUKRS = P_BUKRS AND ANLN1 IN S_ANLN1.

SORT GT_ANLA BY BUKRS ANLN1.

GT_DATA_02[] = GT_DATA[].




IF GT_DATA_02[] IS NOT INITIAL.
SELECT BKPF~BUKRS BKPF~GJAHR  BKPF~BELNR BKPF~BLART BKPF~AWKEY  BSEG~BUZEI  BSEG~HKONT  BSEG~EBELN BSEG~EBELP
   INTO CORRESPONDING FIELDS OF TABLE GT_APPEND
   FROM  BKPF
   INNER JOIN BSEG
   ON BKPF~BUKRS = BSEG~BUKRS
   AND BKPF~GJAHR  = BSEG~GJAHR
   AND BKPF~BELNR = BSEG~BELNR
   WHERE  BKPF~BUKRS = P_BUKRS
   AND    ( BKPF~BLART = 'RE'  )
   AND    BSEG~EBELN <> ''
   AND    BSEG~EBELP <> '00000'
   AND    BSEG~HKONT = '2202030000'.
SORT GT_APPEND BY BUKRS  EBELN EBELP BELNR BUZEI.

ENDIF.
GT_DATA_02[] = GT_DATA[].
DELETE GT_DATA_02 WHERE  ANBWA NE '290' ."只保留 290
IF GT_DATA_02[] IS  NOT  INITIAL.
   SELECT BUKRS GJAHR BELNR BUZEI ANBWA
     INTO CORRESPONDING FIELDS OF TABLE GT_APPEND_02
     FROM  BSEG
     FOR  ALL ENTRIES IN  GT_DATA_02
     WHERE  BUKRS = GT_DATA_02-BUKRS
     AND GJAHR = GT_DATA_02-GJAHR
     AND BELNR = GT_DATA_02-BELNR
     AND ANBWA IN ('200','250','210').

 SORT GT_APPEND_02 BY BUKRS GJAHR BELNR BUZEI.
ENDIF.

SELECT *
  INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
  FROM SKAT
  WHERE SPRAS = '1'
  AND KTOPL = '1000'.

SORT GT_SKAT  BY SAKNR.
  .

SELECT  * INTO CORRESPONDING FIELDS OF TABLE GT_RBKP
  FROM RBKP
  WHERE BUKRS = P_BUKRS
  AND STBLG NE ''.
  SORT  GT_RBKP BY BELNR GJAHR .

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
  DATA:T_INDEX LIKE SY-TABIX.
  DATA:T_JSTJ TYPE I.
   LOOP AT GT_DATA ASSIGNING <FS_DATA> .
     T_INDEX = SY-TABIX.

     CLEAR:GS_DATA.
     "科目描述
     READ TABLE GT_SKAT WITH KEY SAKNR = <FS_DATA>-HKONT BINARY SEARCH .
      IF SY-SUBRC = 0.
        <FS_DATA>-HKONT_TXT = GT_SKAT-TXT50.
      ENDIF.
     "资产描述 、资产类型
     READ TABLE GT_ANLA WITH KEY BUKRS = <FS_DATA>-BUKRS ANLN1 = <FS_DATA>-ANLN1 BINARY SEARCH .
     IF SY-SUBRC = 0.
       <FS_DATA>-ANLAR = GT_ANLA-ANLAR.
       <FS_DATA>-ANLN1_TXT = GT_ANLA-TXT50.
     ENDIF.
     IF <FS_DATA>-SHKZG EQ 'H'.
       <FS_DATA>-DMBTR =  <FS_DATA>-DMBTR  * -1.
     ENDIF.
     IF <FS_DATA>-CGFPH NE  'X'.
        CASE <FS_DATA>-ANBWA .
        WHEN '105' OR '120' OR '100'.
         IF <FS_DATA>-ANBWA = '105'  OR <FS_DATA>-ANBWA = '120'.
           IF '采购入账' NOT  IN S_YWLX.
               DELETE GT_DATA  INDEX T_INDEX.
               CONTINUE.
           ENDIF.
           <FS_DATA>-YWLX = '采购入账'.
          ENDIF.


       IF <FS_DATA>-ANBWA EQ '100' .
           IF  <FS_DATA>-BLART NE 'RE'.
                IF '总账入账' NOT  IN S_YWLX.
                     DELETE GT_DATA  INDEX T_INDEX.
                     CONTINUE.
                 ENDIF.
             <FS_DATA>-YWLX = '总账入账'.
             CONTINUE.
             ELSE.
               IF '采购入账' NOT  IN S_YWLX.
                  DELETE GT_DATA  INDEX T_INDEX.
                  CONTINUE.
                ENDIF.
                <FS_DATA>-YWLX = '采购入账'.
             ENDIF.

        ENDIF.
       IF  <FS_DATA>-YWLX = '采购入账'  AND ( <FS_DATA>-BLART = 'RE' OR <FS_DATA>-BLART = 'WE' ).
            IF <FS_DATA>-BLART = 'RE' .
                <FS_DATA>-BELNR2 =  <FS_DATA>-BELNR . "发票凭证
                <FS_DATA>-GJAHR2 = <FS_DATA>-GJAHR.   "发票年度
                CONTINUE.
            ENDIF.
            IF <FS_DATA>-BLART = 'WE' .
                T_INDEX = T_INDEX + 1.
                T_JSTJ = 1. "计数器归1
                LOOP AT GT_APPEND WHERE  BUKRS = <FS_DATA>-BUKRS AND EBELN = <FS_DATA>-EBELN AND EBELP = <FS_DATA>-EBELP.
                  READ TABLE GT_RBKP WITH KEY BELNR = GT_APPEND-AWKEY+0(10) GJAHR = GT_APPEND-AWKEY+10(4) BINARY SEARCH .
                   IF SY-SUBRC NE 0 .
                     IF T_JSTJ EQ 1.
                       <FS_DATA>-BELNR2 =  GT_APPEND-BELNR . "发票凭证
                       <FS_DATA>-GJAHR2 = GT_APPEND-GJAHR.   "发票年度
                    ELSE.
                    T_JSTJ = T_JSTJ + 1.
                     MOVE-CORRESPONDING <FS_DATA> TO GS_DATA.
                     GS_DATA-DMBTR = 0 .
                     GS_DATA-BELNR2 =  GT_APPEND-BELNR . "发票凭证
                     GS_DATA-GJAHR2 = GT_APPEND-GJAHR.   "发票年度
                     GS_DATA-CGFPH = 'X'.
                     INSERT GS_DATA INTO GT_DATA INDEX T_INDEX.
                     T_INDEX = T_INDEX + 1.
                  ENDIF.
                ENDIF.
             ENDLOOP.
            CONTINUE.
            ENDIF.
       ENDIF.

       WHEN '116'.
        IF '内部订单结算' NOT  IN S_YWLX.
          DELETE GT_DATA  INDEX T_INDEX.
          CONTINUE.
        ENDIF.
         <FS_DATA>-YWLX = '内部订单结算'.
         CONTINUE.
       WHEN '345' OR  '346' .
          IF '在建工程转固' NOT  IN S_YWLX.
          DELETE GT_DATA  INDEX T_INDEX.
          CONTINUE.
        ENDIF.
         <FS_DATA>-YWLX = '在建工程转固'.
         CONTINUE.

       WHEN '130'.
         IF '物料转资产' NOT  IN S_YWLX.
          DELETE GT_DATA  INDEX T_INDEX.
          CONTINUE.
        ENDIF.
         <FS_DATA>-YWLX = '物料转资产'.
         CONTINUE.
       WHEN '200' OR '250'.
          IF '报废' NOT  IN S_YWLX.
          DELETE GT_DATA  INDEX T_INDEX.
          CONTINUE.
          IF <FS_DATA>-ANBWA EQ '290'.

         ENDIF.

        ENDIF.
         <FS_DATA>-YWLX = '报废' .
         CONTINUE.


       WHEN '210' .
           IF '盘亏' NOT  IN S_YWLX.
          DELETE GT_DATA  INDEX T_INDEX.
          CONTINUE.
        ENDIF.
         <FS_DATA>-YWLX = '盘亏'.
            CONTINUE.

       WHEN '290'.
            IF  '报废' IN S_YWLX OR  '盘亏' IN S_YWLX.
                 LOOP AT  GT_APPEND_02  WHERE BUKRS = <FS_DATA>-BUKRS AND GJAHR = <FS_DATA>-GJAHR AND BELNR = <FS_DATA>-BELNR AND ( ANBWA EQ '200' OR  ANBWA EQ '250' ).
                 <FS_DATA>-YWLX = '报废' .
                  CONTINUE..

             ENDLOOP.
              LOOP AT  GT_APPEND_02  WHERE BUKRS = <FS_DATA>-BUKRS AND GJAHR = <FS_DATA>-GJAHR AND BELNR = <FS_DATA>-BELNR AND ANBWA EQ '210' .
                 <FS_DATA>-YWLX = '盘亏' .
                  CONTINUE..

             ENDLOOP.
            ENDIF.
           IF <FS_DATA>-YWLX EQ ''.
              DELETE GT_DATA  INDEX T_INDEX.
              CONTINUE.
           ENDIF.

       WHEN OTHERS.
          IF '报废' IN S_YWLX .
            IF <FS_DATA>-ANBWA EQ '' AND <FS_DATA>-HKONT+0(4) EQ '1606'.
              <FS_DATA>-YWLX = '报废'.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF '盘亏' IN S_YWLX .
            IF <FS_DATA>-ANBWA EQ '' AND <FS_DATA>-HKONT+0(4) EQ'1901'.
              <FS_DATA>-YWLX = '盘亏'.
         CONTINUE.
            ENDIF.
          ENDIF.
         IF '其他' IN S_YWLX.
               IF <FS_DATA>-ANBWA EQ '' AND <FS_DATA>-HKONT+0(4) EQ '1606'.
                 <FS_DATA>-YWLX = '报废'.
                 " DELETE GT_DATA  INDEX T_INDEX.

                ENDIF.
             IF <FS_DATA>-ANBWA EQ '' AND <FS_DATA>-HKONT+0(4) EQ'1901'.
              <FS_DATA>-YWLX = '盘亏'.
             "   DELETE GT_DATA  INDEX T_INDEX.

             ENDIF.
              IF <FS_DATA>-YWLX NE '报废' AND <FS_DATA>-YWLX NE '盘亏'.
              <FS_DATA>-YWLX = '其他'.
               CONTINUE.
             ENDIF.
             IF <FS_DATA>-YWLX NE  '其他'.
               DELETE GT_DATA  INDEX T_INDEX.
               CONTINUE.
              ENDIF.
          ENDIF.
          "执行到这 说明以上都没符合.
         "即可删除当前行.
          DELETE GT_DATA INDEX T_INDEX.
        ENDCASE.
     ENDIF.


   ENDLOOP.

SORT GT_DATA BY YWLX EBELN EBELP GJAHR BELNR BUZEI.

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
  INIT_FIELDCAT 'YWLX'          '业务类型'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1'          '资产编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1_TXT'      '资产编号描述'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLAR'          '资产类型'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '会计凭证'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'          '会计凭证行号'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'          '记账金额'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELP'          '采购订单行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR2'         '发票凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR2'          '发票年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZDAT'          '资产价值日'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKTXT'          '摘要'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANBWA'          '事务类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLART'          '凭证类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT'          '总账科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT'      '科目描述'         '' '' '' '' '' '' ''.

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
*      -->P_0300   text
*      -->P_0301   text
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
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'BELNR'
        AND GS_DATA-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF RS_SELFIELD-FIELDNAME = 'BELNR2'
        AND GS_DATA-BELNR2 IS NOT INITIAL.
         SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR2.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR2.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0250   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPENDYWLX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPENDYWLX .
  GS_YWLX-YWLX = '采购入账'.
  APPEND GS_YWLX TO GT_YWLX.
  GS_YWLX-YWLX = '总帐入账'.
   APPEND GS_YWLX TO GT_YWLX.
 GS_YWLX-YWLX = '内部订单结算'.
   APPEND GS_YWLX TO GT_YWLX.
    GS_YWLX-YWLX = '在建工程转固'.
   APPEND GS_YWLX TO GT_YWLX.
    GS_YWLX-YWLX = '物料转资产'.
   APPEND GS_YWLX TO GT_YWLX.
    GS_YWLX-YWLX = '报废'.
   APPEND GS_YWLX TO GT_YWLX.
    GS_YWLX-YWLX = '盘亏'.
   APPEND GS_YWLX TO GT_YWLX.
    GS_YWLX-YWLX = '其他'.
   APPEND GS_YWLX TO GT_YWLX.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GETYWLX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETYWLX .
CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'YWLX' "大写,可选值内表的字段名
      value_org       = 'S' "就写'S'
      dynpprog        = sy-repid "返回的输入框所在的main program
      dynpnr          = sy-dynnr "返回的输入框所在屏幕
      dynprofield     = 'P_YWLX' "返回的输入框名
      WINDOW_TITLE    = '业务类型'
    TABLES
      value_tab       = GT_YWLX"可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
