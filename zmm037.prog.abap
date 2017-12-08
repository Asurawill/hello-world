*&---------------------------------------------------------------------*
*&程序名称/PROGRAM NAME         :  ZMM037
*&程序描述/PROGRAM DES.         :  励丰SBU采购申请单打印
*&申请人/APPLICANT              :  14061
*&申请日期/DATE OF APP          :   2017.5.9
*&作者/AUTHOR                   :   XIAYUANYUAN
*&完成日期/COMPLETION DATE      :
*&---------------------------------------------------------------------*
*&摘要：1).业务场景
*
*&      2).调拨方案实现：
*&
*&---------------------------------------------------------------------*
*&变更记录：
*&---------------------------------------------------------------------*                                                      *
*&DATE         DEVELOPER           REQNO       DESCRIPTIONS            *
*& ==========  ==================  ==========  ========================*
*& 2017.5.9    XIAYUANYUAN
*&---------------------------------------------------------------------*
REPORT ZMM037.

TYPE-POOLS: SLIS,VRM.

TABLES:EBAN,MARA,EBKN,PRPS.

*---------------------------------------------------------------------*
* ALV                                                                 *
*                                                                     *
*---------------------------------------------------------------------*
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV, " WITH HEADER LINE,"全局内表
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
*----------ALV结构定义-------------------------------------------------
TYPES:BEGIN OF TY_TAB,
        BOX(3),
        BANFN  TYPE EBAN-BANFN,  "采购申请


        POST1  TYPE	PROJ-POST1,      "项目名称

        BNFPO  TYPE	EBAN-BNFPO,  "行项目号
        KNTTP  TYPE EBAN-KNTTP,  "科目分配类别
        PSTYP  TYPE	EBAN-PSTYP,  "行项目类别
        MATNR  TYPE	EBAN-MATNR,  "物料号
        TXZ01  TYPE	EBAN-TXZ01,   "物料描述
        EXTWG  TYPE	MARA-EXTWG,   "品牌
        MEINS  TYPE	EBAN-MEINS,   "单位
        MENGE  TYPE	EBAN-MENGE,    "数量
        LFDAT  TYPE	EBAN-LFDAT,    "计划交货日期
        WERKS  TYPE	EBAN-WERKS,     "工厂
        LGORT  TYPE	EBAN-LGORT,     "库存地点
        PSPSP  TYPE	EBKN-PS_PSP_PNR, "项目编码
        KOSTL  TYPE	EBKN-KOSTL,      "成本中心
        AUFNR  TYPE	EBKN-AUFNR,     "内部订单
        ANLN1  TYPE	EBKN-ANLN1,    "固定的资产号
        AFNAM  TYPE	EBAN-AFNAM,    "申请人
        ERNAM  TYPE	EBAN-ERNAM,   "创建者
        BADAT  TYPE	EBAN-BADAT,   "创建日期
        POTX1  TYPE RESB-POTX1,
      END OF TY_TAB.


*TYPES:
**-- Single Value in Value Set
*       BEGIN OF VRM_VALUES2,
*         KEY(2) TYPE C,
*         TEXT(20) TYPE C,
*       END OF VRM_VALUES2.

DATA:GT_TAB TYPE TABLE OF TY_TAB,
     GS_TAB TYPE TY_TAB.

DATA:GT_TAB1 TYPE TABLE OF TY_TAB,
     GS_TAB1 TYPE TY_TAB.

DATA:GT_TAB2 TYPE TABLE OF TY_TAB,
     GS_TAB2 TYPE TY_TAB.

DATA:GT_TAB3 TYPE TABLE OF TY_TAB,
     GS_TAB3 TYPE TY_TAB.

DATA:GT_TAB4 TYPE TABLE OF TY_TAB,
     GS_TAB4 TYPE TY_TAB.

DATA:GT_TAB5 TYPE TABLE OF TY_TAB,
     GS_TAB5 TYPE TY_TAB.


*----------EBAN结构定义-------------------------------------------------
TYPES:BEGIN OF TY_EBAN,
        BANFN TYPE EBAN-BANFN,  "采购申请
        BNFPO TYPE EBAN-BNFPO,  "行项目号
        KNTTP TYPE EBAN-KNTTP,  "科目分配类别
        PSTYP TYPE  EBAN-PSTYP,  "行项目类别
        MATNR TYPE  EBAN-MATNR,  "物料号
        TXZ01 TYPE  EBAN-TXZ01,   "物料描述
        MEINS TYPE  EBAN-MEINS,   "单位
        MENGE TYPE  EBAN-MENGE,    "数量
        LFDAT TYPE  EBAN-LFDAT,    "计划交货日期
        WERKS TYPE  EBAN-WERKS,     "工厂
        LGORT TYPE  EBAN-LGORT,     "库存地点
        AFNAM TYPE  EBAN-AFNAM,    "申请人
        ERNAM TYPE  EBAN-ERNAM,   "创建者
        BADAT TYPE  EBAN-BADAT,   "创建日期
      END OF TY_EBAN.
DATA:GT_EBAN TYPE TABLE OF TY_EBAN,
     GS_EBAN TYPE TY_EBAN.
*----------EBKN结构定义-------------------------------------------------
DATA : BEGIN OF TY_EBKN,
         PS_PSP_PNR TYPE  EBKN-PS_PSP_PNR, "项目编码
         KOSTL      TYPE  EBKN-KOSTL,      "成本中心
         AUFNR      TYPE  EBKN-AUFNR,     "内部订单
         ANLN1      TYPE  EBKN-ANLN1,    "固定的资产号
         BANFN      TYPE  EBKN-BANFN,
         BNFPO      TYPE  EBKN-BNFPO,
       END OF TY_EBKN.
DATA : GT_EBKN LIKE TABLE OF TY_EBKN,
       GS_EBKN LIKE LINE OF GT_EBKN.
*----------PRPS结构定义-------------------------------------------------
TYPES:BEGIN OF TY_PRPS,
        PSPNR TYPE PRPS-PSPNR,
        PSPHI TYPE PRPS-PSPHI,
        POSID TYPE PRPS-POSID,
      END OF TY_PRPS.
DATA:GT_PRPS TYPE TABLE OF TY_PRPS,
     GS_PRPS TYPE TY_PRPS.
*----------PROJ结构定义-------------------------------------------------
TYPES:BEGIN OF TY_PROJ,
        POST1 TYPE PROJ-POST1,     "项目名称
        PSPNR TYPE PROJ-PSPNR,
      END OF TY_PROJ.
DATA:GT_PROJ TYPE TABLE OF TY_PROJ,
     GS_PROJ TYPE TY_PROJ.
*----------MARA结构定义-------------------------------------------------
TYPES:BEGIN OF TY_MARA,
        EXTWG TYPE  MARA-EXTWG,   "品牌
        MATNR TYPE  MARA-MATNR,  "物料号
      END OF TY_MARA.
DATA:GT_MARA TYPE TABLE OF TY_MARA,
     GS_MARA TYPE TY_MARA.
TYPES : BEGIN OF TY_RESB,
          POTX1 TYPE RESB-POTX1,
          MATNR TYPE RESB-MATNR,
          WERKS TYPE RESB-WERKS,
        END OF TY_RESB.
DATA : GT_RESB TYPE TABLE OF TY_RESB,
       GS_RESB TYPE TY_RESB.

*&--代码添加 BY HANDYBY 11.08.2017 17:16:47  BEGIN
DATA: GT_ZMMT037 TYPE TABLE OF ZMMT037,
      GS_ZMMT037 TYPE ZMMT037.
*&--代码添加 BY HANDYBY 11.08.2017 17:16:47  END


*----------申请人搜索帮助结构定义-------------------------------------------------
TYPES:BEGIN OF TY_AFNAM,
        AFNAM TYPE  EBAN-AFNAM,    "申请人
      END OF TY_AFNAM.
DATA:GT_AFNAM TYPE TABLE OF TY_AFNAM,
     GS_AFNAM TYPE TY_AFNAM.
*----------制单人搜索帮助结构定义-------------------------------------------------
TYPES:BEGIN OF TY_ERNAM,
        ERNAM TYPE  EBAN-ERNAM,    "创建者
      END OF TY_ERNAM.
DATA:GT_ERNAM TYPE TABLE OF TY_ERNAM,
     GS_ERNAM TYPE TY_ERNAM.
*----------------------------添加进度条-----------------------------------------
DATA:PERC TYPE I VALUE 0,
     CNT  TYPE I.
DATA: ZTEXT    TYPE STRING,
      ZPERC(3) TYPE C.
*----------------------------时间字段-----------------------------------------
DATA:ZDATA11 TYPE STRING,
     ZDATA21 TYPE STRING,
     ZDATA31 TYPE STRING,
     ZDATA41 TYPE STRING.
*----------------------------屏幕字段-----------------------------------------
DATA: ZDATA(12),
      ZDATA1(12),
      LISTBOX3(35),
      LISTBOX1(35),
      LISTBOX2(35),
      ZTEXT1(78),
      ZTEXT2(78),
      ZTEXT3(78).
*----------------------------LISTBOX值存储-----------------------------------------
DATA:GIT_FUNCTION1 TYPE VRM_VALUES,
     GWA_FUNCTION1 LIKE LINE OF GIT_FUNCTION1.
DATA:GIT_FUNCTION2 TYPE  VRM_VALUES,
     GWA_FUNCTION2 LIKE  LINE OF GIT_FUNCTION2.
DATA:GIT_FUNCTION3 TYPE VRM_VALUES,
     GWA_FUNCTION3 LIKE LINE OF GIT_FUNCTION3.
*-----------------------------屏幕的触发事件----------------------------------------
DATA:OK_CODE TYPE SY-UCOMM,
     SAVE_OK TYPE SY-UCOMM.
DATA :ZAFNAM TYPE	EBAN-AFNAM,    "申请人
      ZERNAM TYPE	EBAN-ERNAM,   "创建者
      ZBANFN TYPE EBAN-BANFN,
      ZEXTWG TYPE MARA-EXTWG,
      ZPOST1 TYPE PROJ-POST1.

*DATA:
*      CNT2 TYPE I.
*DATA : ZKEY(2).


DATA: L_FM_NAME          TYPE  RS38L_FNAM,
      OUTPUT             TYPE SSFCOMPOP,
      CONTROL_PARAMETERS TYPE SSFCTRLOP,
      LW_SSFCRESCL       TYPE SSFCRESCL,
      OPTION             TYPE SSFCRESCL.
"每页条数
DATA PAGE_NUM TYPE I VALUE 14.
"打印的数目
DATA: DATA_NUM TYPE I,
      SUM      TYPE EBAN-MENGE.
DATA : ZBEIZHU_A TYPE EBAN-TXZ01,
       ZBEIZHU_B TYPE EBAN-TXZ01.
*---------------------------------------------------------------------*
* SELECTION-SCREEN                                                    *
*  定义屏幕                                                                   *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK X WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:S_POSID FOR PRPS-POSID NO-EXTENSION NO INTERVALS .
SELECT-OPTIONS:S_BANFN FOR EBAN-BANFN,
               S_AFNAM FOR EBAN-AFNAM,
               S_ERNAM FOR EBAN-ERNAM,
               S_MATNR FOR EBAN-MATNR,
               S_EXTWG FOR MARA-EXTWG,
               S_MATKL FOR EBAN-MATKL,
               S_AUFNR FOR EBKN-AUFNR,
               S_WERKS FOR EBAN-WERKS,
               S_BADAT FOR EBAN-BADAT,
               S_LFDAT FOR EBAN-LFDAT.
*&--代码添加 BY HANDYBY 16.08.2017 15:18:33  BEGIN
PARAMETERS P1 AS CHECKBOX .
PARAMETERS P2 AS CHECKBOX .
*&--代码添加 BY HANDYBY 16.08.2017 15:18:33  END
SELECTION-SCREEN END OF BLOCK X.

*---------------------------------------------------------------------*
* INITIALIZATION                                                      *
*                                                                     *
*---------------------------------------------------------------------*
INITIALIZATION."初始化列表

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                 *
*                                                                     *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN."相当于选择屏幕的PAI，用来响应屏幕元素的输入

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_AFNAM-LOW."申请人搜索帮助
  PERFORM  S_AFNAM .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_AFNAM-HIGH.
  PERFORM  S_AFNAM .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ERNAM-LOW."创建者搜索帮助
  PERFORM  S_ERNAM .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ERNAM-HIGH.
  PERFORM  S_ERNAM .
*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*                                                                     *
*---------------------------------------------------------------------*
START-OF-SELECTION.
*根据条件选择数据
*&--代码添加 BY HANDYBY 11.08.2017 16:32:09  BEGIN
  PERFORM FRM_CHECK_VAR .
*&--代码添加 BY HANDYBY 11.08.2017 16:32:09  END
  PERFORM FRM_SELDATA.
  IF GT_TAB IS INITIAL.
    MESSAGE '项目WBS输入有误！' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  PERFORM FRM_ALV.



END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      FORM  S_AFNAM
*&---------------------------------------------------------------------*
*       搜索帮助S_AFNAM
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM S_AFNAM .
  SELECT
  EBAN~AFNAM   "创建者
  INTO CORRESPONDING FIELDS OF TABLE GT_AFNAM
  FROM EBAN.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'S_AFNAM'   "表格要显示的字段
      DYNPPROG        = SY-REPID        "返回的程序
      DYNPNR          = SY-DYNNR        "屏幕
      DYNPROFIELD     = 'S_AFNAM'       "往页面回填值的地方
      VALUE_ORG       = 'S'             "显示类型
    TABLES
      VALUE_TAB       = GT_AFNAM    "传进去的表格 帮助的内表
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  S_AFNAM
*&---------------------------------------------------------------------*
*       搜索帮助S_ERNAM
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM S_ERNAM .
  SELECT
  EBAN~ERNAM   "创建者
  INTO CORRESPONDING FIELDS OF TABLE GT_ERNAM
  FROM EBAN.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'S_ERNAM'   "表格要显示的字段
      DYNPPROG        = SY-REPID        "返回的程序
      DYNPNR          = SY-DYNNR        "屏幕
      DYNPROFIELD     = 'S_ERNAM'       "往页面回填值的地方
      VALUE_ORG       = 'S'             "显示类型
    TABLES
      VALUE_TAB       = GT_ERNAM    "传进去的表格 帮助的内表
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_SELDATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_SELDATA .

*------------查询EBAN表和EBKN表--------------------------------------------------
  SELECT
    EBAN~BANFN  "采购申请
    EBAN~BNFPO  "行项目号
    EBAN~KNTTP  "科目分配类别
    EBAN~PSTYP  "行项目类别
    EBAN~MATNR  "物料号
    EBAN~TXZ01  "物料描述
    EBAN~MEINS   "单位
    EBAN~MENGE    "数量
    EBAN~LFDAT   "计划交货日期
    EBAN~WERKS     "工厂
    EBAN~LGORT     "库存地点
    EBAN~AFNAM    "申请人
    EBAN~ERNAM   "创建者
    EBAN~BADAT   "创建日期
    FROM EBAN
      INTO CORRESPONDING FIELDS OF TABLE GT_EBAN
    WHERE EBAN~BANFN IN S_BANFN
    AND EBAN~AFNAM IN S_AFNAM
    AND EBAN~ERNAM IN S_ERNAM
    AND EBAN~MATNR IN S_MATNR
    AND EBAN~MATKL IN S_MATKL
    AND EBAN~WERKS IN S_WERKS
    AND EBAN~BADAT IN S_BADAT
    AND EBAN~LFDAT IN S_LFDAT
* ADD BY HANDYBY BEGIN 2017.07.20 21：26
    AND EBAN~LOEKZ NE 'X' .
* ADD BY HANDYBY END OF 2017.07.20 21：26

  IF GT_EBAN IS NOT INITIAL.
    SELECT
       POTX1
       MATNR
       WERKS
      FROM RESB
      INTO CORRESPONDING FIELDS OF TABLE GT_RESB
      FOR ALL ENTRIES IN GT_EBAN
      WHERE MATNR = GT_EBAN-MATNR
      AND   WERKS = GT_EBAN-WERKS.

*------------查询EBKN表--------------------------------------------------
    SELECT
        PS_PSP_PNR
        KOSTL
        AUFNR
        ANLN1
        BANFN
        BNFPO
      FROM EBKN
      INTO CORRESPONDING FIELDS OF TABLE GT_EBKN
      FOR ALL ENTRIES IN GT_EBAN
      WHERE BANFN = GT_EBAN-BANFN
      AND   BNFPO = GT_EBAN-BNFPO
      AND   AUFNR IN S_AUFNR.
*------------查询PRPS表--------------------------------------------------
    IF GT_EBKN IS NOT INITIAL.
      SELECT
        PRPS~PSPNR
        PRPS~PSPHI
        PRPS~POSID
      INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
      FROM PRPS
      FOR ALL ENTRIES IN GT_EBKN
      WHERE PRPS~PSPNR = GT_EBKN-PS_PSP_PNR
      AND POSID IN S_POSID.
*------------查询PROJ表--------------------------------------------------
      IF GT_PRPS IS NOT INITIAL.
        SELECT
             PROJ~POST1
             PROJ~PSPNR
             INTO  CORRESPONDING FIELDS OF TABLE GT_PROJ
             FROM PROJ
             FOR ALL ENTRIES IN GT_PRPS
             WHERE PSPNR = GT_PRPS-PSPHI.
      ENDIF.
    ENDIF.
  ENDIF.
*&------品牌
*------------查询MARA表--------------------------------------------------
  IF GT_EBAN IS NOT INITIAL.
    SELECT
    MARA~EXTWG   "品牌
    MARA~MATNR  "物料号
    INTO  TABLE GT_MARA
    FROM MARA
    FOR ALL ENTRIES IN GT_EBAN
    WHERE MARA~EXTWG IN S_EXTWG
    AND   MATNR = GT_EBAN-MATNR.
  ENDIF.

  SORT GT_EBKN BY BANFN BNFPO.
  SORT GT_MARA BY MATNR.
  SORT GT_PRPS BY PSPNR.
  SORT GT_PROJ BY PSPNR.
  SORT GT_RESB BY MATNR WERKS.
  LOOP AT GT_EBAN INTO GS_EBAN.
    READ TABLE GT_RESB INTO GS_RESB WITH KEY MATNR = GS_EBAN-MATNR WERKS = GS_EBAN-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-POTX1  = GS_RESB-POTX1.
    ENDIF.


    GS_TAB-BANFN =  GS_EBAN-BANFN.  "采购申请
    GS_TAB-BNFPO = GS_EBAN-BNFPO.  "行项目号
    GS_TAB-KNTTP = GS_EBAN-KNTTP.  "科目分配类别
    IF GS_EBAN-PSTYP = '3'.
      GS_TAB-PSTYP = 'L'.
    ELSE.
      GS_TAB-PSTYP = GS_EBAN-PSTYP.  "行项目类别
    ENDIF.

    GS_TAB-MATNR = GS_EBAN-MATNR.  "物料号
    GS_TAB-TXZ01 = GS_EBAN-TXZ01.  "物料描述
    GS_TAB-MEINS = GS_EBAN-MEINS.   "单位
    GS_TAB-MENGE = GS_EBAN-MENGE.    "数量
    GS_TAB-LFDAT = GS_EBAN-LFDAT.   "计划交货日期
    GS_TAB-WERKS = GS_EBAN-WERKS.     "工厂
    GS_TAB-LGORT = GS_EBAN-LGORT.     "库存地点
    GS_TAB-AFNAM = GS_EBAN-AFNAM.    "申请人
    GS_TAB-ERNAM = GS_EBAN-ERNAM.   "创建者
    GS_TAB-BADAT = GS_EBAN-BADAT.   "创建日期
    READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_EBAN-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-EXTWG =  GS_MARA-EXTWG.   "品牌
    ENDIF.
    READ TABLE GT_EBKN INTO GS_EBKN WITH KEY BANFN = GS_EBAN-BANFN
                                             BNFPO = GS_EBAN-BNFPO BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_TAB-PSPSP = GS_EBKN-PS_PSP_PNR. "项目编码
      GS_TAB-KOSTL = GS_EBKN-KOSTL.      "成本中心
      GS_TAB-AUFNR = GS_EBKN-AUFNR.     "内部订单
      GS_TAB-ANLN1 = GS_EBKN-ANLN1.    "固定的资产号
      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY PSPNR = GS_EBKN-PS_PSP_PNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE GT_PROJ INTO GS_PROJ WITH KEY PSPNR = GS_PRPS-PSPHI BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_TAB-POST1 =  GS_PROJ-POST1."项目名称
          APPEND GS_TAB TO GT_TAB.

        ELSE.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND GS_TAB TO GT_TAB.
    IF S_POSID IS NOT INITIAL.
      DELETE GT_TAB WHERE POST1 IS INITIAL.
    ENDIF.
    CLEAR GS_TAB.
  ENDLOOP.
  SORT GT_TAB BY BANFN BNFPO MATNR."排序
  DELETE ADJACENT DUPLICATES FROM GT_TAB COMPARING BANFN BNFPO.
*---------------------------------屏幕日期传值-------------------------------------
*  zdata11 = sy-datum+0(4).
*  zdata21 = sy-datum+4(2).
*  zdata31 = sy-datum+6(2).
*  CONCATENATE zdata11 '/' zdata21 '/' zdata31  INTO zdata41.
*  zdata = zdata41.
*  zdata1 = zdata41.

*&--代码添加 BY HANDYBY 11.08.2017 16:21:47  BEGIN
  IF GT_TAB IS INITIAL.
    MESSAGE '项目WBS输入有误！' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
* 根据选择屏幕勾选的值判断是要取已经打印过的数据还是未打印的数据
  SELECT *
    INTO TABLE GT_ZMMT037
    FROM ZMMT037 .
  SORT GT_ZMMT037 BY BANFN BNFPO MATNR POST1 .

  IF P1 = 'X' AND P2 IS INITIAL .
    LOOP AT GT_TAB INTO GS_TAB .
      READ TABLE GT_ZMMT037 WITH KEY BANFN = GS_TAB-BANFN
                                     BNFPO = GS_TAB-BNFPO
                                     MATNR = GS_TAB-MATNR
                                     POST1 = GS_TAB-POST1 BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC NE 0 .
        DELETE TABLE GT_TAB FROM GS_TAB .
      ENDIF.
    ENDLOOP.
  ELSEIF P1 IS INITIAL AND P2 = 'X' .
    LOOP AT GT_TAB INTO GS_TAB .
      READ TABLE GT_ZMMT037 WITH KEY BANFN = GS_TAB-BANFN
                                     BNFPO = GS_TAB-BNFPO
                                     MATNR = GS_TAB-MATNR
                                     POST1 = GS_TAB-POST1 BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        DELETE TABLE GT_TAB FROM GS_TAB .
      ENDIF.
    ENDLOOP.
  ELSEIF P1 = 'X' AND P2 = 'X' .

  ENDIF.

  IF GT_TAB IS INITIAL .
    MESSAGE '没数据' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
*&--代码添加 BY HANDYBY 11.08.2017 16:21:47  END


ENDFORM.                    " FRM_SELDATA


*&---------------------------------------------------------------------*
*&      FORM  FRM_ALV
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_ALV .
  PERFORM SHOW_LAYOUT.
  PERFORM SHOW_FIELDCT.
  PERFORM SHOW_ALV.
ENDFORM.                    " FRM_ALV
*&---------------------------------------------------------------------*
*&      FORM  SHOW_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM SHOW_LAYOUT .
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-BOX_FIELDNAME = 'BOX'.

ENDFORM.                    " SHOW_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  SHOW_FIELDCT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM SHOW_FIELDCT .

*&-------------  宏
  DEFINE CATALOG.
    CLEAR :GS_FIELDCAT.

     &1 = &1 + 1 .
      GS_FIELDCAT-COL_POS          = &1.
      GS_FIELDCAT-FIELDNAME        = &2.
      GS_FIELDCAT-SELTEXT_M        = &3.
      GS_FIELDCAT-NO_ZERO          = &4.
      GS_FIELDCAT-OUTPUTLEN        = &5.
      APPEND GS_FIELDCAT TO  GT_FIELDCAT.
  END-OF-DEFINITION.

* USE THE MACRO
  DATA:L_COLPOS TYPE LVC_S_FCAT-COL_POS VALUE 0.

  CATALOG:
   L_COLPOS  'BANFN' '采购申请' 'X' '' ,
   L_COLPOS 'BNFPO' '行项目号' '' '' ,
   L_COLPOS  'KNTTP' '科目分配类别' '' '' ,
   L_COLPOS  'PSTYP' '行项目类别' '' ''  ,
   L_COLPOS  'MATNR' '物料号' '' '40',
   L_COLPOS  'TXZ01' '物料描述' '' '',
   L_COLPOS  'EXTWG       ' '品牌' '' '',
   L_COLPOS  'MEINS' '单位' '' '',
   L_COLPOS  'MENGE' '数量' '' '',
   L_COLPOS  'LFDAT' '计划交货日期' '' '' ,
   L_COLPOS  'WERKS' '工厂' '' '',
   L_COLPOS  'LGORT' '库存地点'  '' '',
   L_COLPOS  'PSPSP' '项目编码' '' '' ,
   L_COLPOS  'POST1' '项目名称' '' '',
   L_COLPOS  'KOSTL' '成本中心' '' '' ,
   L_COLPOS  'AUFNR' '内部订单' '' '',
   L_COLPOS  'ANLN1' '固定的资产号' '' '',
   L_COLPOS  'AFNAM' '申请人' '' '',
   L_COLPOS  'ERNAM' '创建者' '' '',
   L_COLPOS  'BADAT' '创建日期' '' ''.

ENDFORM.                    " SHOW_FIELDCT
*&---------------------------------------------------------------------*
*&      FORM  SHOW_ALV
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM SHOW_ALV .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = GT_TAB
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STATUS'.
ENDFORM. "F_SET_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN 'PRINT'.
*------------------删除重复行-------------------------------------*
*      FREE: gt_tab1.
*      LOOP AT gt_tab  INTO gs_tab WHERE box = 'X'.
*        gs_tab1-banfn  = gs_tab-banfn.
*        gs_tab1-post1  = gs_tab-post1.
*        gs_tab1-banfn = gs_tab-banfn.
*        gs_tab1-afnam  = gs_tab-afnam.
*        gs_tab1-ernam  = gs_tab-ernam.
*        gs_tab1-werks = gs_tab-werks.
*        APPEND gs_tab1 TO gt_tab1.
*        ENDLOOP.
*      SORT gt_tab1 BY banfn.
*      DELETE ADJACENT DUPLICATES FROM gt_tab1 COMPARING banfn bnfpo."因为GT_TAB是ALV展示所用的内表不能删除或改变，所以更改新的内表
      CALL SCREEN 9000 STARTING AT 35 3.

  ENDCASE.
  RS_SELFIELD-REFRESH = 'X'.
ENDFORM. "USER_COMMAND
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STATUS1'.
*  SET TITLEBAR 'XXX'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN '&F03'.
      LEAVE TO SCREEN 0.
    WHEN '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&F12'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'PRINT'.

      LOOP AT  GIT_FUNCTION1 INTO GWA_FUNCTION1 WHERE  KEY = LISTBOX1.
        LISTBOX1  = GWA_FUNCTION1-TEXT.
      ENDLOOP.
      LOOP AT GIT_FUNCTION2 INTO GWA_FUNCTION2 WHERE  KEY = LISTBOX2.
        LISTBOX2  = GWA_FUNCTION2-TEXT.
      ENDLOOP.
      LOOP AT  GIT_FUNCTION3 INTO GWA_FUNCTION3 WHERE  KEY = LISTBOX3.
        LISTBOX3  = GWA_FUNCTION3-TEXT.
      ENDLOOP.

      GT_TAB4 = GT_TAB.
      LOOP AT GT_TAB4 INTO GS_TAB4 WHERE BOX = 'X'.
        IF GS_TAB4-POST1 IS NOT INITIAL .
          GT_TAB4 = GT_TAB2.
          PERFORM FRM_PRINT.
        ELSE.
          DELETE GT_TAB4 WHERE BOX NE 'X'.
          GT_TAB5 = GT_TAB4.
          DELETE ADJACENT DUPLICATES FROM GT_TAB4 COMPARING POST1.
*         DESCRIBE TABLE GT_TAB4 LINES CNT2.
          PERFORM FRM_PRINT2.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SETVALUE1  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE SETVALUE1 INPUT.
  REFRESH:GIT_FUNCTION1.

  GWA_FUNCTION1-KEY   = '01'.                         "个就是变量下拉框的值
  GWA_FUNCTION1-TEXT  = '汽运'.                      "这个是TEXT
  APPEND GWA_FUNCTION1 TO GIT_FUNCTION1.
  GWA_FUNCTION1-KEY   = '02'.
  GWA_FUNCTION1-TEXT  = '空运'.
  APPEND GWA_FUNCTION1 TO GIT_FUNCTION1.
  GWA_FUNCTION1-KEY   = '03'.
  GWA_FUNCTION1-TEXT  = '火车'.
  APPEND GWA_FUNCTION1 TO GIT_FUNCTION1.
  GWA_FUNCTION1-KEY   = '04'.
  GWA_FUNCTION1-TEXT  = '快递'.
  APPEND GWA_FUNCTION1 TO GIT_FUNCTION1.

  "调用函数显示LISTBOX1里面的值
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'LISTBOX1'
      VALUES = GIT_FUNCTION1.

ENDMODULE.                 " SETVALUE1  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SETVALUE2  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE SETVALUE2 INPUT.
  REFRESH:GIT_FUNCTION2.



  GWA_FUNCTION2-KEY   = '01'.
  GWA_FUNCTION2-TEXT  = '采购部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.
  GWA_FUNCTION2-KEY   = '02'.
  GWA_FUNCTION2-TEXT  = '物控部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.


  GWA_FUNCTION2-KEY   = '03'.
  GWA_FUNCTION2-TEXT  = '工程部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '04'.
  GWA_FUNCTION2-TEXT  = '技术服务部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '05'.
  GWA_FUNCTION2-TEXT  = 'Lemuse营销部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '06'.
  GWA_FUNCTION2-TEXT  = 'Lemuse研发部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '07'.
  GWA_FUNCTION2-TEXT  = 'Lemuse制造部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '08'.
  GWA_FUNCTION2-TEXT  = '公共文化设施事业部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '09'.
  GWA_FUNCTION2-TEXT  = '数字文化体验事业部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '10'.
  GWA_FUNCTION2-TEXT  = '文化旅游展演事业部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.


  GWA_FUNCTION2-KEY   = '11'.
  GWA_FUNCTION2-TEXT  = '市场与客户体验部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '12'.
  GWA_FUNCTION2-TEXT  = '品牌营销部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '13'.
  GWA_FUNCTION2-TEXT  = '西北办事处'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '14'.
  GWA_FUNCTION2-TEXT  = '人力资源部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '15'.
  GWA_FUNCTION2-TEXT  = '计划财务部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '16'.
  GWA_FUNCTION2-TEXT  = '管理工程部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '17'.
  GWA_FUNCTION2-TEXT  = '经营管理部'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '18'.
  GWA_FUNCTION2-TEXT  = '综合办公室'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.

  GWA_FUNCTION2-KEY   = '19'.
  GWA_FUNCTION2-TEXT  = '总裁办'.
  APPEND GWA_FUNCTION2 TO GIT_FUNCTION2.


  "调用函数显示LISTBOX2里面的值
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'LISTBOX2'
      VALUES = GIT_FUNCTION2.

ENDMODULE.                 " SETVALUE2  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SETVALUE3  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE SETVALUE3 INPUT.
  REFRESH:GIT_FUNCTION3.

  GWA_FUNCTION3-KEY   = '01'.
  GWA_FUNCTION3-TEXT  = '产品销售'.
  APPEND GWA_FUNCTION3 TO GIT_FUNCTION3.
  GWA_FUNCTION3-KEY   = '02'.
  GWA_FUNCTION3-TEXT  = '项目销售'.
  APPEND GWA_FUNCTION3 TO GIT_FUNCTION3.

  "调用函数显示LISTBOX3里面的值
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'LISTBOX3'
      VALUES = GIT_FUNCTION3.


ENDMODULE.                 " SETVALUE3  INPUT
*&---------------------------------------------------------------------*
*&      FORM  FRM_PRINT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_PRINT .


  DATA :LS_ZZS_11 TYPE  ZMM037_STR.
  DATA :GT_L TYPE TABLE OF  ZMM037_STR,
        GS_L TYPE ZMM037_STR.
  DATA:ZDATA12 TYPE STRING,
       ZDATA22 TYPE STRING,
       ZDATA32 TYPE STRING,
       ZDATA42 TYPE STRING.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME = 'ZSF_ZMM037' "SMARTFROMS的名字
*     VARIANT  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME  = L_FM_NAME.
  IF SY-SUBRC <> 0.

  ENDIF.
  "打印设置
  CONTROL_PARAMETERS-NO_OPEN   = 'X'.
  CONTROL_PARAMETERS-NO_CLOSE  = 'X'.



  OUTPUT-TDDEST = 'LP01'.
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
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

*------------------------  打印数据传值 --------------------------
  SUM = 0.
  GT_TAB1 = GT_TAB.
  LOOP AT GT_TAB1 INTO GS_TAB1 WHERE BOX = 'X'.
    DELETE GT_TAB1 WHERE BOX NE 'X'.
    GT_TAB2 = GT_TAB1.
    DATA_NUM = 0.
    ZAFNAM = GS_TAB1-AFNAM.
    ZERNAM = GS_TAB1-ERNAM.
    ZBANFN = GS_TAB1-BANFN.
    ZPOST1 = GS_TAB1-POST1.
*    zextwg = gs_tab1-extwg.
    AT NEW BANFN.

      LS_ZZS_11-BANFN = ZBANFN."单据编号
      LS_ZZS_11-AFNAM  =  ZAFNAM."申请人
      LS_ZZS_11-ERNAM  = ZERNAM."制单
      LS_ZZS_11-POST1  = ZPOST1."项目名称
      LS_ZZS_11-ZDATA = ZDATA."希望到库日期
      LS_ZZS_11-ZDATA1 = ZDATA1."合同签订日期
      LS_ZZS_11-ZDATA2 = SY-DATUM."申请日期
      LS_ZZS_11-LISTBOX1  = LISTBOX1."运输要求
      LS_ZZS_11-LISTBOX2  = LISTBOX2."申请部门
      LS_ZZS_11-LISTBOX3 = LISTBOX3."客户/工厂项目
      LS_ZZS_11-ZTEXT1  = ZTEXT1.
      LS_ZZS_11-ZTEXT2  = ZTEXT2.
      LS_ZZS_11-ZTEXT3 = ZTEXT3.
    ENDAT.
    IF GS_TAB1-WERKS = '1700'.
      LS_ZZS_11-WERKS = '广州励丰'.
    ELSEIF GS_TAB1-WERKS = '1710'.
      LS_ZZS_11-WERKS = '励丰演艺'.
    ELSEIF GS_TAB1-WERKS = '1720'.
      LS_ZZS_11-WERKS = '香港励丰'.
    ELSE.
      MESSAGE '工厂选择错误' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    AT END OF BANFN.

      LOOP AT GT_TAB2 INTO GS_TAB2 WHERE BANFN = GS_TAB1-BANFN
                                      AND BOX = 'X'     .

        DATA_NUM = DATA_NUM + 1."数据条目  总数据条数
*        gs_l-bnfpo  = gs_tab2-bnfpo."行号
        GS_L-MATNR  = GS_TAB2-MATNR."物料号
        GS_L-TXZ01 =  GS_TAB2-TXZ01."物料描述
        GS_L-EXTWG = GS_TAB2-EXTWG."品牌
        GS_L-MEINS = GS_TAB2-MEINS."单位
        GS_L-MENGE = GS_TAB2-MENGE."数量
*&.-----------取总数量
        SUM = SUM + GS_TAB2-MENGE.
        ZDATA12 = GS_TAB2-LFDAT+0(4).
        ZDATA22 = GS_TAB2-LFDAT+4(2).
        ZDATA32 = GS_TAB2-LFDAT+6(2).
        GS_L-ZBEIZHU = GS_TAB2-POTX1.
        CONCATENATE ZDATA12 '/' ZDATA22 '/' ZDATA32  INTO ZDATA42.
        GS_L-LFDAT = ZDATA42.

*&--代码添加 BY HANDYBY 11.08.2017 17:13:47  BEGIN
* 塞数据进自建表
        GS_ZMMT037-BANFN = GS_TAB2-BANFN .
        GS_ZMMT037-BNFPO = GS_TAB2-BNFPO .
        GS_ZMMT037-MATNR = GS_TAB2-MATNR .
        GS_ZMMT037-POST1 = GS_TAB2-POST1 .
        GS_ZMMT037-WERKS = GS_TAB2-WERKS .
        GS_ZMMT037-LGORT = GS_TAB2-LGORT .

        APPEND GS_ZMMT037 TO GT_ZMMT037 .
        CLEAR GS_ZMMT037 .
*&--代码添加 BY HANDYBY 11.08.2017 17:13:47  END

        APPEND GS_L TO GT_L.
        CLEAR : GS_L.
      ENDLOOP.
*------------------------  空行 --------------------------
      DATA_NUM = DATA_NUM MOD PAGE_NUM."最后页面行数
      IF DATA_NUM <> 0.
        DATA_NUM = PAGE_NUM - DATA_NUM."空行数
        CLEAR GS_L.
        DO DATA_NUM TIMES.
          APPEND GS_L TO GT_L.
        ENDDO.
      ENDIF.

      CALL FUNCTION L_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = OUTPUT
          ZSF_05             = LS_ZZS_11  "表头和表尾
          GS_ZF              = GS_L       "表体
          PAGE_NUM           = PAGE_NUM    "分页的
          GV_SUM             = SUM    "总计的
        TABLES
          GT_SF              = GT_L
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.
      IF SY-SUBRC = 0.
      ENDIF.
      CLEAR GT_L.
      CLEAR GS_L.
      CLEAR SUM.
    ENDAT.
  ENDLOOP.
  CLEAR LISTBOX1.
  CLEAR LISTBOX2.
  CLEAR LISTBOX3.

  "#  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
  ENDIF.

*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  BEGIN
* 塞数据进自建表
  MODIFY ZMMT037 FROM TABLE GT_ZMMT037 .
  IF SY-SUBRC = 0 .
    COMMIT WORK AND WAIT .
  ELSE .
    ROLLBACK WORK .
  ENDIF.

  REFRESH GT_ZMMT037 .
*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  END

ENDFORM.                    " FRM_PRINT
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM FRM_PRINT2 .

  DATA :LS_ZZS_11 TYPE  ZMM037_STR.
  DATA :GT_L TYPE TABLE OF  ZMM037_STR,
        GS_L TYPE ZMM037_STR.


  DATA:ZDATA12 TYPE STRING,
       ZDATA22 TYPE STRING,
       ZDATA32 TYPE STRING,
       ZDATA42 TYPE STRING.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME = 'ZSF_ZMM037' "SMARTFROMS的名字
*     VARIANT  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME  = L_FM_NAME.
  IF SY-SUBRC <> 0.

  ENDIF.
  "打印设置
  CONTROL_PARAMETERS-NO_OPEN   = 'X'.
  CONTROL_PARAMETERS-NO_CLOSE  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = OUTPUT
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  OUTPUT-TDDEST = 'LP01'.
  OUTPUT-RQPOSNAME = ''.
  OUTPUT-TDDATASET = ''.
  OUTPUT-TDSUFFIX1 = ''.
  OUTPUT-TDSUFFIX2 = ''.
  OUTPUT-TDIMMED   = 'X'.
  OUTPUT-TDDELETE  = 'X'.



*------------------------  打印数据传值 --------------------------
  LOOP AT GT_TAB4 INTO GS_TAB4.
*  READ TABLE GT_TAB4 INTO GS_TAB4 WITH KEY box = 'X' .

    DATA_NUM = 0.
    ZAFNAM = GS_TAB4-AFNAM.
    ZERNAM = GS_TAB4-ERNAM.
    ZBANFN = GS_TAB4-BANFN.
    ZPOST1 = GS_TAB4-POST1.
*    zextwg = gs_tab4-extwg.

    LS_ZZS_11-BANFN = ZBANFN."单据编号
    LS_ZZS_11-AFNAM  =  ZAFNAM."申请人
    LS_ZZS_11-ERNAM  = ZERNAM."制单
    LS_ZZS_11-POST1  = ZPOST1."项目名称
    LS_ZZS_11-ZDATA = ZDATA."希望到库日期
    LS_ZZS_11-ZDATA1 = ZDATA1."合同签订日期
    LS_ZZS_11-ZDATA2 = SY-DATUM."申请日期
    LS_ZZS_11-LISTBOX1  = LISTBOX1."运输要求
    LS_ZZS_11-LISTBOX2  = LISTBOX2."申请部门
    LS_ZZS_11-LISTBOX3 = LISTBOX3."客户/工厂项目
    LS_ZZS_11-ZTEXT1  = ZTEXT1.
    LS_ZZS_11-ZTEXT2  = ZTEXT2.
    LS_ZZS_11-ZTEXT3 = ZTEXT3.
    DATA: CNT TYPE I.
    CLEAR CNT.
    DESCRIBE TABLE GT_TAB5 LINES CNT.
    IF GS_TAB4-WERKS = '1700'.
      LS_ZZS_11-WERKS = '广州励丰'.
    ELSEIF GS_TAB4-WERKS = '1710'.
      LS_ZZS_11-WERKS = '励丰演艺'.
    ELSEIF GS_TAB4-WERKS = '1720'.
      LS_ZZS_11-WERKS = '香港励丰'.
    ELSE.
      MESSAGE '工厂选择错误' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    SUM = 0.
    LOOP AT GT_TAB5 INTO GS_TAB5." WHERE BANFN = GS_TAB4-BANFN.

      DATA_NUM = DATA_NUM + 1."数据条目  总数据条数
*        gs_l-bnfpo  = gs_tab5-bnfpo."行号
      GS_L-MATNR  = GS_TAB5-MATNR."物料号
      GS_L-TXZ01 =  GS_TAB5-TXZ01."物料描述
      GS_L-EXTWG =  GS_TAB5-EXTWG."品牌
      GS_L-MEINS = GS_TAB5-MEINS."单位
      GS_L-MENGE = GS_TAB5-MENGE."数量

*          IF cnt > 1.
      LS_ZZS_11-BANFN = ''.
      CONCATENATE  GS_TAB5-BANFN GS_TAB5-POTX1 INTO ZBEIZHU_B.
      GS_L-ZBEIZHU = ZBEIZHU_B .
*          ELSEIF cnt LE 1.
*            ls_zzs_11-banfn = gs_tab5-banfn.
*            gs_l-zbeizhu = '' .
*          ENDIF.
*&.-----------取总数量
      SUM = SUM + GS_TAB5-MENGE.
      ZDATA12 = GS_TAB5-LFDAT+0(4).
      ZDATA22 = GS_TAB5-LFDAT+4(2).
      ZDATA32 = GS_TAB5-LFDAT+6(2).
      CONCATENATE ZDATA12 '/' ZDATA22 '/' ZDATA32  INTO ZDATA42.
      GS_L-LFDAT = ZDATA42.

*&--代码添加 BY HANDYBY 11.08.2017 17:13:47  BEGIN
* 塞数据进自建表
      GS_ZMMT037-BANFN = GS_TAB2-BANFN .
      GS_ZMMT037-BNFPO = GS_TAB2-BNFPO .
      GS_ZMMT037-MATNR = GS_TAB2-MATNR .
      GS_ZMMT037-POST1 = GS_TAB2-POST1 .
      GS_ZMMT037-WERKS = GS_TAB2-WERKS .
      GS_ZMMT037-LGORT = GS_TAB2-LGORT .

      APPEND GS_ZMMT037 TO GT_ZMMT037 .
      CLEAR GS_ZMMT037 .
*&--代码添加 BY HANDYBY 11.08.2017 17:13:47  END

      APPEND GS_L TO GT_L.
      CLEAR : GS_L.
    ENDLOOP.
*------------------------  空行 --------------------------

    DATA_NUM = DATA_NUM MOD PAGE_NUM."最后页面行数
    IF DATA_NUM <> 0.
      DATA_NUM = PAGE_NUM - DATA_NUM."空行数
      CLEAR GS_L.
      DO DATA_NUM TIMES.
        APPEND GS_L TO GT_L.
      ENDDO.
    ENDIF.


    CALL FUNCTION L_FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = CONTROL_PARAMETERS
        OUTPUT_OPTIONS     = OUTPUT
        ZSF_05             = LS_ZZS_11  "表头和表尾
        GS_ZF              = GS_L       "表体
        PAGE_NUM           = PAGE_NUM    "分页的
        GV_SUM             = SUM    "总计的
      TABLES
        GT_SF              = GT_L
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4.
    IF SY-SUBRC = 0.
    ENDIF.
    CLEAR GT_L.
    CLEAR GS_L.
    CLEAR SUM.
  ENDLOOP.

  CLEAR LISTBOX1.
  CLEAR LISTBOX2.
  CLEAR LISTBOX3.
  "#  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
  ENDIF.

*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  BEGIN
* 塞数据进自建表
  MODIFY ZMMT037 FROM TABLE GT_ZMMT037 .
  IF SY-SUBRC = 0 .
    COMMIT WORK AND WAIT .
  ELSE .
    ROLLBACK WORK .
  ENDIF.

  REFRESH GT_ZMMT037 .
*&--代码添加 BY HANDYBY 11.08.2017 17:14:26  END

ENDFORM.
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
