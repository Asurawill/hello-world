REPORT ZCO010_4.
"

*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160329
*& Request       :
*& Descriptions  : 生产订单投料明细表
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

TABLES:MKPF,MSEG,MAKT,CKMLHD.

  TYPES:BEGIN OF TY_DATA,
      WERKS TYPE MSEG-WERKS,                "工厂
      AUFNR TYPE MSEG-AUFNR,                "生产订单
      MATNR TYPE MSEG-MATNR,                "物料号
      MAKTX TYPE MAKT-MAKTX,                "物料描述
      MAT_KDAUF TYPE MSEG-KDAUF,            "销售订单
      MAT_KDPOS TYPE MSEG-MAT_KDPOS,        "销售订单行项目
      KALNR  TYPE CKMLHD-KALNR,             "成本估算号
      SHKZG TYPE MSEG-SHKZG,                "借贷标识
      MENGE TYPE MSEG-MENGE,                "数量
      MEINS TYPE MSEG-MEINS,                "单位
      BUDAT TYPE MKPF-BUDAT,               "过账日期
      GJAHR TYPE BKPF-GJAHR,               "会计年度
      MONAT TYPE BKPF-MONAT,               " 会计期间
      BWART TYPE MSEG-BWART,               "移动类型
      BWART_TXT TYPE STRING,                "移动类型描述
      SOBKZ TYPE MSEG-SOBKZ,               "特殊库存
      "SHKZG TYPE MSEG-SHKZG,               "借贷标识
      AUFPL TYPE AFKO-AUFPL,                "计划号码
      ARBID TYPE AFVC-ARBID,                "对象标识
      KOSTL TYPE CRCO-KOSTL,               "成本中心
      LTEXT TYPE CSKT-LTEXT,               "成本中心描述
      EBELN TYPE EBELN,                    "采购订单
      EBELP TYPE EBELP,                    "采购订单行项目
      MBLNR TYPE MSEG-MBLNR,               "物料凭证
      MJAHR TYPE MSEG-MJAHR,               "物料凭证年度
      ZEILE TYPE MSEG-ZEILE,               "物料凭证项目
      SEL(1),
    END OF TY_DATA.

 DATA:GS_DATA TYPE TY_DATA,
      GT_DATA TYPE TABLE OF TY_DATA.

 DATA:GS_MAKT TYPE MAKT,
      GT_MAKT TYPE TABLE OF MAKT.

 DATA:GS_AFKO TYPE AFKO,
      GT_AFKO TYPE TABLE OF AFKO.

 DATA:GS_AFVC TYPE AFVC,
      GT_AFVC TYPE TABLE OF AFVC.

  DATA:GS_AFVC_1 TYPE AFVC,
       GT_AFVC_1 TYPE TABLE OF AFVC.

 DATA:GS_CRHD TYPE CRHD,
      GT_CRHD TYPE TABLE OF CRHD.

 DATA:GS_CRTX TYPE CRTX,
      GT_CRTX TYPE TABLE OF CRTX.

 DATA:GS_CRCO TYPE CRCO,
      GT_CRCO TYPE TABLE OF CRCO.

 DATA:GS_CSKT TYPE CSKT,
      GT_CSKT TYPE TABLE OF CSKT.

DATA:GT_CKMLHD TYPE TABLE OF CKMLHD,
     GS_CKMLHD TYPE CKMLHD.

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
   IF &1 = 'MENGE' .
   GW_LVC-QFIELDNAME = 'MEINS'.
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
P_BUKRS  TYPE BKPF-BUKRS OBLIGATORY.                      "公司代码
SELECT-OPTIONS: S_BUDAT  FOR MKPF-BUDAT OBLIGATORY,   "过账日期
                S_AUFNR  FOR MSEG-AUFNR,   "生产订单
                S_MATNR  FOR MSEG-MATNR.   "销售订单
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

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0347   text
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
  "读取已过账凭证信息
SELECT MKPF~MBLNR MKPF~MJAHR MKPF~BUDAT
       MSEG~ZEILE MSEG~SHKZG MSEG~MENGE
       MSEG~WERKS MSEG~MATNR MSEG~MAT_KDAUF
       MSEG~MAT_KDPOS MSEG~MEINS MSEG~BWART
       MSEG~SOBKZ MSEG~AUFNR MSEG~EBELN MSEG~EBELP
       INTO CORRESPONDING FIELDS OF TABLE GT_DATA
    FROM MKPF
    INNER JOIN MSEG
    ON MKPF~MJAHR = MSEG~MJAHR
    AND MKPF~MBLNR = MSEG~MBLNR
    WHERE MKPF~BUDAT IN S_BUDAT
    AND MSEG~BWART IN ('261','262','Z05','Z06','543','544')
    AND MSEG~WERKS EQ P_BUKRS
    AND MSEG~MATNR IN S_MATNR .
  SORT GT_DATA BY WERKS AUFNR.
  CHECK GT_DATA IS NOT INITIAL.
  "读取物料主数据信息
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FROM MAKT
    FOR ALL ENTRIES IN GT_DATA
    WHERE MATNR = GT_DATA-MATNR
    AND   SPRAS = '1'.
  SORT GT_MAKT BY MATNR.

 "读取生产订单信息
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AFKO
     FROM AFKO
     FOR ALL ENTRIES IN GT_DATA
     WHERE AUFNR = GT_DATA-AUFNR.
   SORT GT_AFKO BY AUFNR.

  "读取生产订单对应工序
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_AFVC
    FROM AFVC
    FOR ALL ENTRIES IN GT_AFKO
    WHERE AUFPL = GT_AFKO-AUFPL.

  SORT GT_AFVC BY AUFPL ascending APLZL descending.

  "只保留生产订单对应的最后一道工序
  DELETE ADJACENT DUPLICATES FROM GT_AFVC COMPARING AUFPL.

 "读取分配工作中心到成本中心数据
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CRCO
    FROM CRCO
     FOR ALL ENTRIES IN GT_AFVC
    WHERE OBJID = GT_AFVC-ARBID
    AND OBJTY = 'A'
    AND LASET = '    1'
   AND LANUM = '0001'.
SORT  GT_CRCO BY OBJID.

"读取成本中心主数据信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CSKT
  FROM CSKT
  FOR ALL ENTRIES IN GT_CRCO
  WHERE KOKRS = '1000'
  AND KOSTL = GT_CRCO-KOSTL
  AND SPRAS = '1'.
SORT GT_CSKT BY KOSTL.


"读取物料分类账信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CKMLHD
  FROM CKMLHD
  FOR ALL ENTRIES IN GT_DATA
  WHERE BWKEY EQ P_BUKRS
  AND MATNR EQ GT_DATA-MATNR
  AND VBELN EQ GT_DATA-MAT_KDAUF
  AND POSNR EQ GT_DATA-MAT_KDPOS.

  SORT GT_CKMLHD BY BWKEY MATNR VBELN POSNR.




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

 DATA:
      KOSTL TYPE CRCO-KOSTL,               "成本中心
      LTEXT TYPE CSKT-LTEXT,               "成本中心描述

      AUFPL TYPE AFKO-AUFPL,                 "计划号码
      ARBID TYPE AFVC-ARBID.                " 对象标识

 LOOP AT GT_DATA ASSIGNING <FS_DATA> .

   AT NEW AUFNR .
     CLEAR: KOSTL,LTEXT,ARBID,AUFPL.
         READ TABLE GT_AFKO INTO GS_AFKO
          WITH KEY AUFNR = <FS_DATA>-AUFNR BINARY SEARCH .
         IF SY-SUBRC EQ 0 .
           "计划号码
           AUFPL = GS_AFKO-AUFPL.
           READ TABLE GT_AFVC INTO GS_AFVC
              WITH KEY AUFPL = GS_AFKO-AUFPL BINARY SEARCH .
             IF SY-SUBRC EQ 0 .
                ARBID = GS_AFVC-ARBID.   "对象标识
                   "成本中心
                   READ TABLE GT_CRCO INTO GS_CRCO
                     WITH KEY OBJID = GS_AFVC-ARBID BINARY SEARCH.
                     IF SY-SUBRC EQ 0 .
                       KOSTL = GS_CRCO-KOSTL.
                      ENDIF.
                   READ TABLE GT_CSKT INTO GS_CSKT
                     WITH KEY KOSTL = KOSTL BINARY SEARCH.
                    IF SY-SUBRC EQ 0 .
                      LTEXT = GS_CSKT-LTEXT.
                       "成本中心描述
                   ENDIF.
             ENDIF.
        ENDIF.
   ENDAT.
*  ""计划号码
*   <FS_DATA>-AUFPL = AUFPL .
*  "对象标识
*   <FS_DATA>-ARBID = ARBID.
    "成本中心
   <FS_DATA>-KOSTL = KOSTL.
     "成本中心描述
    <FS_DATA>-LTEXT = LTEXT.
   "是否跨部门



  IF <FS_DATA>-SHKZG EQ 'S'.
    <FS_DATA>-MENGE = <FS_DATA>-MENGE * -1.
  ENDIF.
  "会计年度
  <FS_DATA>-GJAHR = <FS_DATA>-BUDAT+0(4).
  "会计期间
  <FS_DATA>-MONAT = <FS_DATA>-BUDAT+4(2).
  "物料描述
  READ TABLE GT_MAKT INTO GS_MAKT
    WITH KEY MATNR = <FS_DATA>-MATNR BINARY SEARCH .
   IF SY-SUBRC EQ 0 .
   <FS_DATA>-MAKTX =  GS_MAKT-MAKTX.
   ENDIF.

  "成本估算号
  READ TABLE GT_CKMLHD INTO GS_CKMLHD
       WITH KEY BWKEY = <FS_DATA>-WERKS
                MATNR = <FS_DATA>-MATNR
                VBELN = <FS_DATA>-MAT_KDAUF
                POSNR = <FS_DATA>-MAT_KDPOS
                BINARY SEARCH.
  IF SY-SUBRC EQ 0 .
     <FS_DATA>-KALNR = GS_CKMLHD-KALNR.
  ENDIF.

  "移动类型描述
  IF <FS_DATA>-BWART EQ '261' OR <FS_DATA>-BWART EQ '262' .
    <FS_DATA>-BWART_TXT = '计划内投料'.
  ELSEIF <FS_DATA>-BWART EQ 'Z05' OR <FS_DATA>-BWART EQ 'Z06' .
   <FS_DATA>-BWART_TXT = '计划外投料'.
  ELSEIF <FS_DATA>-BWART EQ '543' OR <FS_DATA>-BWART EQ '544' .
    <FS_DATA>-BWART_TXT = '外协消耗'.
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
  INIT_FIELDCAT 'WERKS'          '工厂'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'          '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAT_KDAUF'      '销售订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAT_KDPOS'      '销售订单行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KALNR'          '成本估算号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SHKZG'          '借贷标识'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE'          '数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'          '会计期间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'          '移动类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART_TXT'      '移动类型描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SOBKZ'          '特殊库存'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUFNR'          '生产订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOSTL'          '成本中心'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LTEXT'          '成本中心描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELN'          '采购订单'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EBELP'          '采购订单行项目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MBLNR'          '物料凭证'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MJAHR'          '物料凭证年度'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZEILE'          '物料凭证项目'         '' '' '' '' '' '' ''.


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
*      -->P_0442   text
*      -->P_0443   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
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
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  SELXMMC
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_GT_DATA_VBELN  TEXT
*      <--P_GT_DATA_XMMC  TEXT
*----------------------------------------------------------------------*
FORM SELXMMC  USING    P_VBELN TYPE VBELN
              CHANGING P_XMMC TYPE STRING.


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
