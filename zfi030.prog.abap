REPORT ZFI030.
TABLES:VBKPF,BSEG,VBSEGS,VBSEGD,VBSEGK,VBSEGA, /BEV1/RBVBAK.
TABLES:SKAT,T001,ANLA,CSKT,AUFK,MAKT,MARA,T023T,T053S.
DATA:BEGIN OF  W_DATA .
  DATA:BUKRS lIKE VBKPF-BUKRS,"公司代码
       BELNR LIKE VBKPF-BELNR,"凭证编码
       GJAHR LIKE VBKPF-GJAHR,"会计年度
       BLART LIKE VBKPF-BLART,"凭证类型
       BSTAT LIKE VBKPF-BSTAT,"凭证状态
       BLDAT LIKE VBKPF-BLDAT,"凭证日期
       BUDAT LIKE VBKPF-BUDAT,"过账日期
       MONAT LIKE VBKPF-MONAT,"过账日期
       XBLNR LIKE VBKPF-XBLNR,"参照号
  "     PPNAM LIKE VBKPF-PPNAM,"制单人
       USNAM LIKE VBKPF-USNAM,"制单人
       BKTXT LIKE VBKPF-BKTXT,"抬头文本
      " STBLG LIKE VBKPF-STBLG,"冲销凭证
      " AWKEY LIKE VBKPF-AWKEY,"发票凭证
       REBZG LIKE VBSEGK-REBZG,"发票凭证
       BUZEI LIKE VBSEGS-BUZEI,"行项目号
       SHKZG  LIKE VBSEGS-SHKZG,"借贷标识
       XNEGP LIKE VBSEGS-XNEGP,"反记账标识
       HKONT LIKE VBSEGS-SAKNR,"科目
       HKONT_TXT20 LIKE SKAT-TXT20,"科目名称
       DMBTR  LIKE VBSEGS-DMBTR,"本位币金额
       WAERS LIKE T001-WAERS,"本位币
       WRBTR LIKE VBSEGS-WRBTR,"凭证货币金额
       WAERS_1 LIKE VBKPF-WAERS,"货币
       KUNNR LIKE KNA1-KUNNR,"客户
       KUNNR_NAME1 LIKE KNA1-NAME1, "客户描述
       LIFNR  LIKE LFA1-LIFNR,"供应商
       LIFNR_NAME1 LIKE LFA1-NAME1,"供应商描述
       ANLN1 LIKE ANLA-ANLN1,"资产
       ANLN1_TXT50 LIKE ANLA-TXT50, "资产描述
       KOSTL LIKE CSKT-KOSTL, "成本中心
       KTEXT LIKE CSKT-KTEXT, "成本中心描述
       AUFNR LIKE AUFK-AUFNR,"内部订单
       AUFNR_KTEXT LIKE AUFK-KTEXT, "内部订单描述
       KDAUF LIKE VBSEGS-KDAUF,"销售订单
       KDPOS LIKE VBSEGS-KDPOS,"销售订单行号
       MATNR LIKE VBSEGS-MATNR,"物料号
       MAKTX LIKE MAKT-MAKTX,"物料描述
       MATKL LIKE MARA-MATKL,"物料组
       MAKTL_WGBEZ LIKE T023T-WGBEZ,"物料组描述
       RSTGR LIKE VBSEGS-RSTGR, "原因代码
       RSTGR_TXT40 LIKE T053S-TXT40, "原因代码描述
       SGTXT LIKE VBSEGS-SGTXT,"行项目文本
       ZUONR LIKE VBSEGS-ZUONR, "分配
       MENGE LIKE VBSEGS-MENGE,"数量
       MEINS LIKE VBSEGS-MEINS,"单位
       PS_PSP_PNR LIKE VBSEGS-PS_PSP_PNR ,"工作分解结构元素
       POST1 TYPE PS_POST1,"WBS描述
       EBELN LIKE VBSEGS-EBELN,"采购订单
       EBELP LIKE VBSEGS-EBELP,"采购订单行项目
       BOX   TYPE C .
  DATA:END OF W_DATA.
  DATA: GT_DATA LIKE TABLE OF W_DATA WITH HEADER LINE.
  DATA: GT_T001 LIKE TABLE OF T001 WITH HEADER LINE.
  DATA:GT_ANLA LIKE TABLE OF ANLA WITH HEADER LINE.
  DATA: GT_CSKT LIKE TABLE OF CSKT WITH HEADER LINE.
  DATA: GT_AUFK LIKE TABLE OF AUFK WITH HEADER LINE .
  DATA: GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.
  DATA:GT_MARA  LIKE TABLE OF MARA WITH HEADER LINE .
  DATA:GT_KNA1 LIKE TABLE OF KNA1 WITH HEADER LINE.
  DATA:GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.
  DATA: GT_T023T LIKE TABLE OF T023T WITH HEADER LINE.
  DATA: GT_T053S LIKE TABLE OF T053S WITH HEADER LINE .
  DATA: GT_LFA1 LIKE TABLE OF LFA1 WITH HEADER LINE .

  "************************************************************************
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

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
ENDIF.

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

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME .
SELECT-OPTIONS: S_BUKRS FOR VBKPF-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_GJAHR FOR VBKPF-GJAHR OBLIGATORY NO INTERVALS NO-EXTENSION,"会计年度
                S_MONAT FOR VBKPF-MONAT,"记账期间
                S_BELNR FOR VBKPF-BELNR,"凭证编码
                S_BLDAT FOR VBKPF-BLDAT,"凭证日期
                S_BUDAT FOR VBKPF-BUDAT,"过账日期
                S_BLART FOR VBKPF-BLART,"凭证类型
               " S_PPNAM FOR VBKPF-PPNAM,"制单人
                S_USNAM FOR VBKPF-USNAM,"制单人
                S_HKONT FOR BSEG-HKONT,"科目
                S_LIFNR FOR BSEG-LIFNR,"供应商
                S_KUNNR FOR BSEG-KUNNR,"客户
                S_KOSTL FOR BSEG-KOSTL,"成本中心
                S_AUFNR FOR BSEG-AUFNR,"内部订单
                S_VBEL2 FOR BSEG-VBEL2,"销售订单号
                S_EBELN FOR VBSEGS-EBELN,"采购订单号
                S_DMBTR FOR BSEG-DMBTR,"行项目本位币金额
                S_SGTXT FOR BSEG-SGTXT,"文本
                S_ZUONR FOR BSEG-ZUONR,"分配
                S_MATNR FOR BSEG-MATNR,"物料号
                S_ANLN1 FOR VBSEGS-ANLN1,"资产号
                S_PROJK FOR /BEV1/RBVBAK-PS_PSP_PNR."WBS元素



SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_USNAM-LOW.
  PERFORM GETUSNAM.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_USNAM-HIGH.
  PERFORM GETUSNAM.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*权限检查检查公司代码
  " PERFORM FRM_AUTH_CHECK USING '03'.
  PERFORM FRM_AUTH_CHECK.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示
*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*----------------------------------------------------------------------*
  "FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD S_BUKRS-LOW.
*  IF SY-SUBRC <> 0.
*   MESSAGE s899(mm) WITH '您没有公司代码' S_BUKRS '的权限' DISPLAY LIKE 'E'.
*  LEAVE LIST-PROCESSING.
*   ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  FRM_GET_DATA .
SELECT A~BUKRS A~BELNR A~GJAHR A~BLART A~BSTAT A~BLDAT A~MONAT A~XBLNR
  A~USNAM  A~WAERS AS WAERS_1 A~BKTXT   B~BUZEI B~SHKZG B~XNEGP B~SAKNR AS HKONT B~DMBTR B~WRBTR B~ANLN1
   B~KOSTL B~AUFNR B~KDAUF B~KDPOS B~MATNR  B~RSTGR B~SGTXT B~ZUONR B~MENGE B~MEINS B~PS_PSP_PNR B~EBELN B~EBELP
  FROM VBKPF  AS A
  INNER JOIN VBSEGS AS B
  ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
 INTO CORRESPONDING  FIELDS OF TABLE GT_DATA
 WHERE
        A~BUKRS IN S_BUKRS
        AND   A~GJAHR IN S_GJAHR
        AND   A~MONAT IN S_MONAT
        AND   A~BELNR IN S_BELNR
        AND   BLDAT IN S_BLDAT
        AND   BUDAT IN S_BUDAT
        AND   BLART IN S_BLART
        AND   USNAM IN S_USNAM
        AND   SAKNR IN S_HKONT
        AND   ANLN1 IN S_ANLN1
        AND   KOSTL IN S_KOSTL
        AND   AUFNR IN S_AUFNR
        AND  KDAUF IN S_VBEL2
        AND   ZUONR IN S_ZUONR
        AND   MATNR IN S_MATNR
        AND   PS_PSP_PNR IN S_PROJK
        AND   EBELN IN S_EBELN .
SELECT A~BUKRS A~BELNR A~GJAHR A~BLART A~BSTAT A~BLDAT A~MONAT A~XBLNR
  A~USNAM A~WAERS AS WAERS_1  A~BKTXT   B~BUZEI B~SHKZG B~XNEGP B~HKONT B~DMBTR B~WRBTR
  B~KUNNR B~RSTGR B~SGTXT B~ZUONR B~REBZG
  FROM VBKPF  AS A
  INNER JOIN VBSEGD AS B
 ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
 APPENDING  CORRESPONDING  FIELDS OF TABLE GT_DATA
 WHERE  A~BUKRS IN S_BUKRS
        AND   A~GJAHR IN S_GJAHR
        AND   A~MONAT IN S_MONAT
        AND  A~BELNR IN S_BELNR
        AND   BLDAT IN S_BLDAT
        AND   BUDAT IN S_BUDAT
        AND   BLART IN S_BLART
        AND   USNAM IN S_USNAM
        AND   HKONT IN S_HKONT
        AND   ZUONR IN S_ZUONR
        AND   KUNNR IN S_KUNNR

   .
SELECT A~BUKRS A~BELNR A~GJAHR A~BLART A~BSTAT A~BLDAT A~MONAT A~XBLNR
  A~USNAM A~WAERS AS WAERS_1  A~BKTXT   B~BUZEI B~SHKZG B~XNEGP B~HKONT B~DMBTR B~WRBTR
 B~LIFNR B~RSTGR B~SGTXT B~ZUONR B~REBZG
  FROM VBKPF  AS A
  INNER JOIN VBSEGK AS B
 ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
 APPENDING  CORRESPONDING  FIELDS OF TABLE GT_DATA
 WHERE  A~BUKRS IN S_BUKRS
        AND   A~GJAHR IN S_GJAHR
        AND   A~MONAT IN S_MONAT
        AND  A~BELNR IN S_BELNR
        AND   BLDAT IN S_BLDAT
        AND   BUDAT IN S_BUDAT
        AND   BLART IN S_BLART
        AND   USNAM IN S_USNAM
        AND   HKONT IN S_HKONT
        AND   ZUONR IN S_ZUONR
        AND   LIFNR  IN S_LIFNR   .
 SELECT A~BUKRS A~BELNR A~GJAHR A~BLART A~BSTAT A~BLDAT A~MONAT A~XBLNR
  A~USNAM A~WAERS AS WAERS_1   A~BKTXT   B~BUZEI B~SHKZG B~XNEGP B~HKONT B~DMBTR B~WRBTR B~ANLN1
   B~KOSTL B~AUFNR B~KDAUF B~KDPOS B~MATNR   B~SGTXT B~ZUONR B~MENGE B~MEINS B~PS_PSP_PNR
  FROM VBKPF  AS A
  INNER JOIN VBSEGA AS B
  ON A~BUKRS = B~BUKRS AND A~GJAHR = B~GJAHR AND A~BELNR = B~BELNR
  APPENDING CORRESPONDING  FIELDS OF TABLE GT_DATA
  WHERE
       A~BUKRS IN S_BUKRS
        AND   A~GJAHR IN S_GJAHR
        AND   A~MONAT IN S_MONAT
        AND  A~BELNR IN S_BELNR
        AND   BLDAT IN S_BLDAT
        AND   BUDAT IN S_BUDAT
        AND   BLART IN S_BLART
        AND   USNAM IN S_USNAM
        AND   ANLN1 IN S_ANLN1
        AND   HKONT IN S_HKONT
        AND   KOSTL IN S_KOSTL
        AND   AUFNR IN S_AUFNR
        AND  KDAUF IN S_VBEL2
        AND   ZUONR IN S_ZUONR
        AND   MATNR IN S_MATNR
        AND   PS_PSP_PNR IN S_PROJK .
 SORT GT_DATA  BY BUKRS GJAHR BELNR BUZEI.
 DELETE ADJACENT DUPLICATES FROM GT_DATA  COMPARING BUKRS GJAHR BELNR BUZEI.
 IF GT_DATA[] IS NOT INITIAL.
 "读取公司代码名称文本
   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T001
     FROM T001
     WHERE BUKRS IN  S_BUKRS.
   SORT GT_T001 BY BUKRS .

 "读取科目名称

   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
     FROM SKAT
     FOR ALL ENTRIES IN GT_DATA
     WHERE SPRAS = '1'
     AND KTOPL = '1000'
     AND SAKNR = GT_DATA-HKONT.
 SORT GT_SKAT BY SAKNR .

"读取资产编号描述
   SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_ANLA
     FROM ANLA
     WHERE BUKRS IN  S_BUKRS.
   SORT GT_ANLA  BY BUKRS ANLN1 .

 "读取成本中心描述
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CSKT
    FROM CSKT
    WHERE SPRAS = '1'
    AND KOKRS = '1000' .
   SORT GT_CSKT BY KOSTL.
 " 读取物料组文本
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T023T
    FROM T023T
    WHERE SPRAS = '1'.
  SORT GT_T023T BY MATKL.

  "读取原因代码文本
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T053S
    FROM T053S
    WHERE SPRAS = '1'
    AND BUKRS IN S_BUKRS.
 SORT GT_T053S BY BUKRS  RSTGR.

"查询物料基本视图 物料组
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MARA
  FROM MARA
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR.
  SORT GT_MARA BY MATNR.
"读取物料描述
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
   FROM MAKT
   FOR ALL ENTRIES IN GT_DATA
   WHERE  MATNR = GT_DATA-MATNR
   AND SPRAS = '1'.
 SORT GT_MAKT BY MATNR.
 "读取内部订单信息

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AUFK
    FROM AUFK
    FOR ALL ENTRIES IN GT_DATA
    WHERE AUFNR = GT_DATA-AUFNR.

  SORT GT_AUFK BY AUFNR.

"查询客户信息
SELECT  * INTO CORRESPONDING FIELDS OF TABLE GT_KNA1
  FROM  KNA1
  FOR ALL ENTRIES IN GT_DATA
  WHERE  KUNNR = GT_DATA-KUNNR.
  SORT GT_KNA1 BY KUNNR.

    .
"查询供应商信息你
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
   FROM LFA1
   FOR ALL  ENTRIES IN GT_DATA
   WHERE LIFNR = GT_DATA-LIFNR  .
 SORT  GT_LFA1 BY LIFNR.
  ENDIF.

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
LOOP AT GT_DATA.
  "读取科目描述
  READ TABLE  GT_SKAT WITH KEY SAKNR = GT_DATA-HKONT BINARY SEARCH.
  IF SY-SUBRC = 0.
    GT_DATA-HKONT_TXT20 = GT_SKAT-TXT20.
   ENDIF.
 "读取本位币
 READ TABLE GT_T001 WITH KEY BUKRS = GT_DATA-BUKRS BINARY SEARCH.
 IF SY-SUBRC = 0.
 GT_DATA-WAERS = GT_T001-WAERS.
 ENDIF.

"读取客户名称
READ TABLE GT_KNA1 WITH KEY KUNNR = GT_DATA-KUNNR BINARY SEARCH.
IF SY-SUBRC = 0.
  GT_DATA-KUNNR_NAME1 = GT_KNA1-NAME1.
ENDIF.
"读取供应商名称
READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DATA-LIFNR BINARY SEARCH.
IF SY-SUBRC = 0.
  GT_DATA-LIFNR_NAME1 = GT_LFA1-NAME1.
ENDIF.

"读取资产描述
READ TABLE GT_ANLA WITH KEY BUKRS = GT_DATA-BUKRS ANLN1 = GT_DATA-ANLN1 BINARY SEARCH.
IF SY-SUBRC = 0.
  GT_DATA-ANLN1_TXT50 = GT_ANLA-TXT50 .
ENDIF.

"读取成本中心描述
READ TABLE GT_CSKT WITH KEY KOSTL = GT_DATA-KOSTL BINARY SEARCH.
IF SY-SUBRC = 0.
  GT_DATA-KTEXT = GT_CSKT-KTEXT.

  ENDIF.

"读取内部订单描述
READ TABLE GT_AUFK  WITH KEY AUFNR = GT_DATA-AUFNR BINARY SEARCH.
IF SY-SUBRC = 0.
  GT_DATA-AUFNR_KTEXT = GT_AUFK-KTEXT.
 ENDIF.

"读取物料描述信息
READ TABLE GT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
IF SY-SUBRC = 0.
 GT_DATA-MAKTX = GT_MAKT-MAKTX.

 ENDIF.

"读取物料组
 READ TABLE GT_MARA WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
 IF SY-SUBRC = 0.
   GT_DATA-MATKL = GT_MARA-MATKL.

  ENDIF.

  "读取物料组名称
 READ TABLE GT_T023T WITH KEY MATKL = GT_DATA-MATKL BINARY SEARCH.
  IF SY-SUBRC = 0.
    GT_DATA-MAKTL_WGBEZ = GT_T023T-WGBEZ.

  ENDIF.

"读取原因代码文本
 READ TABLE GT_T053S WITH KEY RSTGR = GT_DATA-RSTGR BINARY SEARCH.
 IF SY-SUBRC = 0.
   GT_DATA-RSTGR_TXT40 = GT_T053S-TXT40.

  ENDIF.

"*WBS内码转换WBS元素
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = GT_DATA-PS_PSP_PNR
      IMPORTING
        OUTPUT = GT_DATA-POST1.
  IF GT_DATA-SHKZG = 'H'.
    GT_DATA-DMBTR =  GT_DATA-DMBTR * -1 .
    GT_DATA-WRBTR =  GT_DATA-WRBTR * -1 .

  ENDIF.


  MODIFY GT_DATA.
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
*&      Form  GETPPNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETUSNAM .
 DATA:BEGIN OF WS_USNAM,
         BNAME      LIKE USER_ADDR-BNAME,
         NAME_TEXTC LIKE USER_ADDR-NAME_TEXTC,
       END OF WS_USNAM.
  DATA:TS_USNAM LIKE TABLE OF WS_USNAM.
  DATA:L_BUKRS LIKE T001-BUKRS.
  DATA:BUKRS1_NAME TYPE  CHAR30.
  DATA:BUKRS2_NAME TYPE  CHAR30 .
  PERFORM FRM_GET_FIELD_VALUE USING 'S_BUKRS-LOW' CHANGING L_BUKRS.
  CONCATENATE L_BUKRS 'FI%' INTO BUKRS1_NAME .
  CONCATENATE L_BUKRS 'CO%' INTO BUKRS2_NAME .
  SELECT BNAME NAME_TEXTC  INTO CORRESPONDING FIELDS OF TABLE TS_USNAM
     FROM USER_ADDR
  WHERE BNAME LIKE BUKRS1_NAME  OR BNAME LIKE BUKRS2_NAME.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BNAME' "大写,可选值内表的字段名
      VALUE_ORG       = 'S' "就写'S'
      DYNPPROG        = SY-REPID "返回的输入框所在的main program
      DYNPNR          = SY-DYNNR "返回的输入框所在屏幕
      DYNPROFIELD     = 'S_USNAM' "返回的输入框名
      WINDOW_TITLE    = '制单人'
    TABLES
      VALUE_TAB       = TS_USNAM "可选值的内表
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
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
  GW_LAYOUT-BOX_FNAME     = 'BOX'.
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
  INIT_FIELDCAT 'BUKRS'   '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'   '凭证编码'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'GJAHR'   '会计年度'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLART'   '凭证类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BSTAT'   '凭证状态'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BLDAT'   '凭证日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'   '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'   '期间'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XBLNR'  '参照号'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'USNAM'   '制单人'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKTXT'   '抬头文本'        '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'STBLG'   '冲销凭证'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUZEI'   '行项目号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SHKZG'   '借贷标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'XNEGP'   '反记账标识'       '' '' '' '' '' '' ''."反记账
    INIT_FIELDCAT 'HKONT'   '科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HKONT_TXT20'   '科目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR'   '本位币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'   '本位币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WRBTR'   '凭证货币金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS_1' '货币'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'   '客户'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR_NAME1'   '客户描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'  '供应商'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR_NAME1' '供应商描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1'   '资产'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1_TXT50'   '资产描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'   '成本中心'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT'   '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR'   '内部订单'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR_KTEXT' '内部订单描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDAUF'   '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDPOS'   '销售订单行号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'   '物料号'         '' '' '' '' '' 'MARA' 'MATNR'.
  INIT_FIELDCAT 'MAKTX'   '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATKL'   '物料组'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTL_WGBEZ'   '物料组描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'RSTGR'   '原因代码'         '' '' '' '' '' 'BSEG' 'RSTGR'.
  INIT_FIELDCAT 'RSTGR_TXT40'   '原因代码描述'         '' '' '' '' '' '' ''. "150730 add by it02
  INIT_FIELDCAT 'SGTXT'  '行项目文本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZUONR'   '分配'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REBZG'   '发票凭证号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'   '数量'         '' '' '' '' '' '' ''. "数量
  INIT_FIELDCAT 'MEINS'   '单位'         '' '' '' '' '' '' ''. "单位
  INIT_FIELDCAT 'POST1'   '工作分解结构元素'        '' '' '' '' '' '' ''."工作分解结构元素
  INIT_FIELDCAT 'EBELN'   '采购订单'         '' '' '' '' '' '' ''. "采购订单
  INIT_FIELDCAT 'EBELP'   '采购订单行项目'        '' '' '' '' '' '' ''."采购订单项目
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
 REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
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
*GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
*  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
*  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.



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
"      IT_EVENTS                = GT_EVENTS[]
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
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
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
*        AND GS_DATA-BSTAT <> 'V'
*        AND GS_DATA-BELNR IS NOT INITIAL.
*        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
*        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
*        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.
*
    "跳转到FBV0预制凭证事务
      READ TABLE GT_DATA  INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'BELNR'
        AND GT_DATA-BSTAT = 'V'
        AND GT_DATA-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLP' FIELD GT_DATA-BELNR.
        SET PARAMETER ID 'BUK' FIELD GT_DATA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GT_DATA-GJAHR.
        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
      ENDIF.
***打印
*    WHEN '&PRNT'.
**需进行保存操作
*      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*        IMPORTING
*          E_GRID = G_REF_GRID.
*
*      READ TABLE GT_DATA INTO GS_DATA
*      WITH KEY ZBOX = 'X'.
*      IF SY-SUBRC = 0.
*        PERFORM FRM_PRINT_DATA.
*      ELSE.
*        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
*      ENDIF.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
FORM FRM_GET_FIELD_VALUE USING VALUE(F_FIELDNAME) CHANGING VALUE(F_FIELDVALUE).
  DATA: DYNPRO_VALUES TYPE TABLE OF DYNPREAD WITH HEADER LINE,
        FIELD_VALUE   LIKE LINE OF DYNPRO_VALUES.

  CLEAR: FIELD_VALUE, DYNPRO_VALUES[].
  FIELD_VALUE-FIELDNAME = F_FIELDNAME.
  APPEND FIELD_VALUE TO DYNPRO_VALUES.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = SY-REPID
      DYNUMB             = SY-DYNNR
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = DYNPRO_VALUES.

  CHECK NOT DYNPRO_VALUES[]  IS INITIAL.
  READ TABLE DYNPRO_VALUES INDEX 1.
  CHECK SY-SUBRC = 0.
  F_FIELDVALUE =  DYNPRO_VALUES-FIELDVALUE.
ENDFORM.                    "frm_get_field_value
