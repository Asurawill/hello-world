REPORT ZFI019.


TABLES:T001 ,RBKP,LFA1,EKKO.

DATA:BEGIN OF GS_DATA ,
  BUKRS LIKE RSEG-BUKRS,   "公司代码
  BELNR LIKE RSEG-BELNR,   "SAP发票编号
  GJAHR LIKE RBKP-GJAHR ,"年度
  BUZEI LIKE RSEG-BUZEI,   "发票行项目
  TBTKZ LIKE RSEG-TBTKZ,    "后续借/贷
  STBLG LIKE RBKP-STBLG,    "冲销关于
  XBLNR LIKE RBKP-XBLNR,    "参照
  KP_LIFNR LIKE RBKP-LIFNR,  " 开票供应商
  KP_LIFNR_NAME1 LIKE LFA1-NAME1,  "开票供应商名称
  CG_LIFNR LIKE EKKO-LIFNR,    "采购供应商
  CG_LIFNR_NAME1 LIKE LFA1-NAME1,  "采购供应商名称
  WRBTR LIKE RSEG-WRBTR,  "金额
  MWSKZ LIKE RSEG-MWSKZ,  "税码
  HSJE  LIKE RSEG-WRBTR,"含税金额
  EBELN LIKE RSEG-EBELN,  "采购订单
  EBELP LIKE RSEG-EBELP, "订单行项目
  MATNR LIKE RSEG-MATNR, "物料
  MAKTX LIKE MAKT-MAKTX,"物料描述
  MATKL LIKE EKPO-MATKL, "物料组
  WGBEZ LIKE V023-WGBEZ, "物料组描述
  SAKTO LIKE  EKKN-SAKTO,  "科目
  SAKTO_TXT TYPE STRING,"科目描述
  PS_PSP_PNR LIKE EKKN-PS_PSP_PNR,  "WBS元素
  POST1 LIKE PRPS-POST1, "WBS元素描述
  KNTTP LIKE RSEG-KNTTP, "科目分配类别
  LFBNR LIKE RSEG-LFBNR,  "物料凭证
  LFPOS LIKE RSEG-LFPOS, "物料凭证行项目
  SHKZG LIKE RSEG-SHKZG, "借贷标记
  "MWSKZ LIKE RSEG-MWSKZ,  "税吗
  FI_BELNR LIKE BKPF-BELNR, "会计凭证
  FI_BUZEI LIKE BSEG-BUZEI,"会计凭证行号
  RBSTAT LIKE RBKP-RBSTAT,"预制发票
  MENGE LIKE MSEG-MENGE,"数量
  MEINS LIKE MSEG-MEINS,"单位
  "XREF3 LIKE BSEG-XREF3,"参考码
  SEL(1),
  END OF GS_DATA.

  DATA: GT_DATA LIKE TABLE OF GS_DATA WITH HEADER LINE.

  DATA: GT_EKKO LIKE TABLE OF EKKO WITH HEADER LINE.

  DATA:GT_EKKN LIKE TABLE OF EKKN WITH HEADER LINE.

  DATA:GT_EKPO LIKE TABLE OF EKPO WITH HEADER LINE.

  DATA: GT_A003 LIKE TABLE OF A003 WITH HEADER LINE.

  DATA: GT_KONP LIKE TABLE OF KONP WITH HEADER LINE.

  DATA:GT_LFA1 LIKE TABLE OF LFA1 WITH HEADER LINE.

  DATA:GT_PRPS LIKE TABLE OF PRPS WITH HEADER LINE.

  DATA:GT_BKPF LIKE TABLE OF BKPF WITH HEADER LINE.

  DATA:GT_MARA LIKE TABLE OF MARA WITH HEADER LINE.

  DATA:GT_MAKT LIKE TABLE OF MAKT WITH HEADER LINE.

  DATA:GT_T023T LIKE TABLE OF T023T WITH HEADER LINE.

  DATA: GT_SKAT LIKE TABLE OF SKAT WITH HEADER LINE.

  DATA: GT_MSEG LIKE TABLE OF MSEG WITH HEADER LINE.
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
*    IF &1 = 'KUNNR'.
*    GW_LVC-NO_ZERO = 'X'.
*  ENDIF.
*   IF &1 = 'WRBTR' .
*   GW_LVC-CFIELDNAME = 'WAERS_1'.
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
DATA: GT_FTAXP LIKE  TABLE OF FTAXP .
DATA: GS_FTAXP LIKE FTAXP.
DATA: INMEG TYPE STRING.
************************************************************************
* GLOBAL VARIANT
************************************************************************
************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
SELECT-OPTIONS: S_BELNR  FOR RBKP-BELNR ,    "发票编号
                S_BUDAT  FOR RBKP-BUDAT ,"过账日期
                S_KP  FOR LFA1-LIFNR ,  "开票供应商
                S_CG  FOR LFA1-LIFNR,  "采购供应商
                S_EBELN  FOR EKKO-EBELN .   "采购订单号
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
    CONCATENATE '无权查询' P_BUKRS '采购发票明细表'  INTO  INMEG .
    MESSAGE INMEG   TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示*&---------------------------------------------------------------------*
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
   AUTHORITY-CHECK OBJECT 'ZFI_BUK_CK' ID 'ACTVT' FIELD P_ACTVT
                                     ID 'BUKRS' FIELD P_BUKRS.
                                .
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
  SELECT A~BUKRS A~BELNR  A~GJAHR A~LIFNR AS KP_LIFNR A~STBLG A~XBLNR A~STBLG A~RBSTAT
    B~BUZEI B~SHKZG B~TBTKZ B~WRBTR B~MWSKZ B~EBELN B~EBELP
    B~MATNR B~KNTTP B~LFBNR  B~LFPOS B~MWSKZ B~TBTKZ
    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM RBKP AS A INNER JOIN RSEG AS B
    ON A~BUKRS = B~BUKRS AND A~BELNR = B~BELNR
    WHERE  A~BUDAT IN S_BUDAT AND A~BUKRS = P_BUKRS AND A~BELNR IN S_BELNR AND B~LIFNR IN S_KP AND EBELN  IN S_EBELN
    .
  SELECT A~BUKRS A~BELNR AS FI_BELNR  A~GJAHR A~STBLG A~BSTAT  B~BUZEI AS FI_BUZEI B~SHKZG B~WRBTR AS HSJE B~LIFNR AS KP_LIFNR B~EBELN B~EBELP
    B~XREF3 AS CG_LIFNR_NAME1 B~MATNR B~PROJK AS  PS_PSP_PNR  HKONT AS SAKTO
    APPENDING CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM BKPF AS A
    INNER JOIN BSEG  AS B
    ON  A~BUKRS = B~BUKRS AND A~BELNR = B~BELNR AND A~GJAHR = B~GJAHR
    WHERE A~BUKRS = P_BUKRS AND A~BUDAT IN S_BUDAT  AND A~BLART NE 'RE' AND B~LIFNR IN S_KP AND B~KOART = 'K'.
  SORT GT_DATA BY BUKRS  GJAHR BELNR BUZEI .
  CHECK GT_DATA[] IS NOT INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
    FROM LFA1  .

  SORT GT_LFA1 BY LIFNR .

 SELECT MBLNR MJAHR ZEILE  MENGE MEINS
   FROM MSEG
   INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
   FOR ALL ENTRIES  IN GT_DATA
   WHERE MBLNR = GT_DATA-LFBNR AND ZEILE = GT_DATA-LFPOS.

SORT GT_MSEG BY MBLNR ZEILE.

SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MARA
  FROM MARA
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR.

SORT GT_MARA BY MATNR.

SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
  FROM MAKT
  FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR = GT_DATA-MATNR
  AND SPRAS = '1'.

 SORT GT_MAKT BY MATNR.

CHECK  GT_MAKT[] IS NOT INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_T023T
  FROM T023T
  WHERE SPRAS = '1'.

SORT GT_T023T BY MATKL .

 SELECT  EBELN LIFNR
  INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
   FROM EKKO
   FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS = GT_DATA-BUKRS AND EBELN = GT_DATA-EBELN.
 SORT GT_EKKO BY EBELN LIFNR.

 SELECT  EBELN  EBELP SAKTO  PS_PSP_PNR
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
   FROM EKKN
   FOR ALL ENTRIES IN GT_DATA
   WHERE EBELN = GT_DATA-EBELN AND EBELP = GT_DATA-EBELP.
 SORT  GT_EKKN BY EBELN EBELP.

SELECT EBELN EBELP MATKL
   INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
  FROM EKPO
  FOR ALL ENTRIES IN GT_DATA
  WHERE EBELN = GT_DATA-EBELN AND EBELP = GT_DATA-EBELP.
 SORT GT_EKPO BY EBELN EBELP.

SELECT * INTO CORRESPONDING FIELDS OF TABLE  GT_BKPF
  FROM BKPF
  WHERE BUKRS = P_BUKRS.

SORT GT_BKPF BY BUKRS BELNR GJAHR .

 CHECK GT_EKKN[] IS NOT INITIAL.
  SELECT PSPNR POST1
    FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    WHERE PBUKR = P_BUKRS.
 SORT GT_PRPS BY PSPNR.

SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_SKAT
  FROM SKAT
  WHERE SPRAS = '1' AND KTOPL = '1000' .
 SORT GT_SKAT BY SAKNR.
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
      "采购供应商
     IF GT_DATA-CG_LIFNR_NAME1 NE ''.
        READ TABLE GT_EKKO WITH KEY  EBELN = GT_DATA-EBELN  BINARY SEARCH.
           IF SY-SUBRC = 0.
              GT_DATA-CG_LIFNR = GT_EKKO-LIFNR.
             IF  GT_DATA-CG_LIFNR IN S_CG .
               "供应商名称
               READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DATA-CG_LIFNR BINARY SEARCH .
                IF SY-SUBRC = 0.
                 GT_DATA-CG_LIFNR_NAME1 = GT_LFA1-NAME1.
              ENDIF.
             ELSE.
              CONTINUE.
            ENDIF.
         ENDIF.

      ENDIF.

    IF GT_DATA-SHKZG = 'H'.
       GT_DATA-WRBTR = GT_DATA-WRBTR * -1.
    ENDIF.
     IF GT_DATA-SHKZG = 'S'.
       GT_DATA-HSJE = GT_DATA-HSJE * -1.
    ENDIF.
   "开票供应商
     READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DATA-KP_LIFNR BINARY SEARCH .
     IF SY-SUBRC = 0 .
        GT_DATA-KP_LIFNR_NAME1 = GT_LFA1-NAME1.
      ENDIF.
    "物料描述
    READ TABLE GT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
       GT_DATA-MAKTX = GT_MAKT-MAKTX.
    ENDIF.
    "物料组
    READ TABLE GT_MARA WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-MATKL = GT_MARA-MATKL.
    ENDIF.
    "物料组名称
    READ TABLE GT_T023T WITH KEY MATKL = GT_DATA-MATKL BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-WGBEZ = GT_T023T-WGBEZ .
     ENDIF.
    "项目描述
     IF GT_DATA-PS_PSP_PNR NE '00000000'.
       READ TABLE GT_PRPS WITH KEY PSPNR = GT_DATA-PS_PSP_PNR .
            IF SY-SUBRC = 0..
               GT_DATA-POST1 = GT_PRPS-POST1. "项目描述
             ENDIF.
     ENDIF.
    READ TABLE GT_EKKN WITH KEY EBELN = GT_DATA-EBELN EBELP = GT_DATA-EBELP BINARY SEARCH.
     IF SY-SUBRC = 0.

       GT_DATA-SAKTO = GT_EKKN-SAKTO .

       GT_DATA-PS_PSP_PNR = GT_EKKN-PS_PSP_PNR.
          READ TABLE GT_PRPS WITH KEY PSPNR = GT_DATA-PS_PSP_PNR .
            IF SY-SUBRC = 0..
               GT_DATA-POST1 = GT_PRPS-POST1. "项目描述
             ENDIF.

      ENDIF.
    " 科目描述
      READ TABLE GT_SKAT WITH KEY SAKNR = GT_DATA-SAKTO .
       IF  SY-SUBRC = 0.
             GT_DATA-SAKTO_TXT = GT_SKAT-TXT50.
           ENDIF.

  "含税金额
 IF GS_DATA-BELNR NE ''. "该项不为空，就代表从 RSEG 取值
      CALL FUNCTION 'GET_TAX_PERCENTAGE'
       EXPORTING
        ALAND         = 'CN'
        DATAB         = '20000101'
        MWSKZ         = GT_DATA-MWSKZ
        TXJCD         =  'TAXCN'
*     EXPORT        = ' '
      TABLES
        T_FTAXP       = GT_FTAXP
            .

    READ TABLE GT_FTAXP INTO GS_FTAXP INDEX 1 .
     IF SY-SUBRC = 0.
        GT_DATA-HSJE = GT_DATA-WRBTR  * ( 1 + GS_FTAXP-KBETR / 1000 ) .
     ENDIF.

 ENDIF.

    "会计凭证
     READ TABLE GT_BKPF WITH KEY AWKEY+0(10) = GT_DATA-BELNR .
     IF SY-SUBRC = 0.
       GT_DATA-FI_BELNR = GT_BKPF-BELNR.
     ENDIF.
     "数量 、单位
    READ TABLE GT_MSEG WITH KEY MBLNR = GT_DATA-LFBNR ZEILE = GT_DATA-LFPOS.
     IF SY-SUBRC  = 0.
        GT_DATA-MENGE = GT_MSEG-MENGE.
        GT_DATA-MEINS = GT_MSEG-MEINS.
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
    INIT_FIELDCAT 'BUKRS'   '公司代码'  '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BELNR'  'SAP发票编号'  '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'BUZEI'          '发票行项目'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'TBTKZ'          '后续借/贷'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'STBLG'          '冲销关于'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'XBLNR'          '参照'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'KP_LIFNR'          '开票供应商'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'KP_LIFNR_NAME1'          '开票供应商名称'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'CG_LIFNR'          '采购供应商'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'CG_LIFNR_NAME1'          '采购供应商名称'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WRBTR'          '金额'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MWSKZ'          '税码'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'HSJE'          '含税金额'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELN'          '采购订单'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'EBELP'          '订单行项目'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATNR'          '物料'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MAKTX'          '物料描述'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MATKL'          '物料组'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'WGBEZ'          '物料组描述'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SAKTO'          '科目'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'SAKTO_TXT'          '科目描述'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'PS_PSP_PNR'          'WBS元素'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'POST1'          'WBS元素描述'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'KNTTP'          '科目分配类别'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LFBNR'          '物料凭证'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'LFPOS'          '物料凭证行项目'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'FI_BELNR'          '会计凭证'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'RBSTAT'          '预制发票'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MENGE'          '数量'             '' '' '' '' '' '' ''.
    INIT_FIELDCAT 'MEINS'          '单位'             '' '' '' '' '' '' ''.

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
*      -->P_0421   text
*      -->P_0422   text
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

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.

FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

*  CASE R_UCOMM.
** 双击
**    WHEN '&IC1'.
**      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
**      CHECK SY-SUBRC = 0.
**      IF RS_SELFIELD-FIELDNAME = 'BELNR'
**        AND GS_DATA-BELNR IS NOT INITIAL.
**        SET PARAMETER ID 'BLN' FIELD GS_DATA-BELNR.
**        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
**        SET PARAMETER ID 'GJR' FIELD GS_DATA-GJAHR.
**        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
**      ENDIF.
**      IF RS_SELFIELD-FIELDNAME = 'VBELN'
**        AND GS_DATA-VBELN IS NOT INITIAL.
**        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
**        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
**      ENDIF.
*
*  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
