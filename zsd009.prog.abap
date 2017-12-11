*&---------------------------------------------------------------------*
*& Report  ZFI004
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/03/10
*& Request       :
*& Descriptions  : 发货延期报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZSD009.

************************************************************************
* Tables
************************************************************************
TABLES VBAK.
************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA.
TYPES:ZBOX  TYPE C.
        INCLUDE TYPE ZSD009_1.
TYPES:POSTDT TYPE ZMM002I-POSTDT, "移入整备库日期
      YQTS   TYPE I,            "延期入库天数
      YQFJ   TYPE KONV-KBETR,    "延期罚金
      SJCKRQ TYPE EKBE-BUDAT,    "实际出库日期
      YQFHTS TYPE I,            "延期发货天数
      YQYY   TYPE ZSD009-ZSD001, "延期原因
      CCF    TYPE KONV-KWERT,    "仓储费
      ZBF    TYPE KONV-KWERT,    "整备费
      ZJE    TYPE KONV-KWERT,    "总金额
      Z001   TYPE STRING.        "销售抬头长文本
TYPES  END OF TY_DATA.

*TYPES:BEGIN OF TY_ZMM002I,
*        DBDH      TYPE ZDBDH,         "调拨单号
*        WERKS     TYPE WERKS_D,       "工厂
*        VBELN     TYPE VBELN_VA,      "销售凭证
*        POSNR(6)  TYPE C,         "销售凭证行项目
*        POSTDT    TYPE ZMM002I-POSTDT, "过账日期
*      END OF TY_ZMM002I.

TYPES:BEGIN OF TY_LIKP,
        VBELN TYPE CHAR16,
        LFART TYPE LIKP-LFART,
      END OF TY_LIKP.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_ZSD009 TYPE TABLE OF ZSD009_1.
DATA GS_ZSD009 TYPE ZSD009_1.

DATA GT_ZMM002I TYPE TABLE OF ZMM002I.
DATA GS_ZMM002I TYPE ZMM002I.

DATA GT_EKBE TYPE TABLE OF EKBE.
DATA GS_EKBE TYPE EKBE.

DATA GT_EKKN TYPE TABLE OF EKKN.
DATA GS_EKKN TYPE EKKN.

DATA GT_LIPS TYPE TABLE OF LIPS.
DATA GS_LIPS TYPE LIPS.

DATA GT_LIKP TYPE TABLE OF TY_LIKP.
DATA GS_LIKP TYPE TY_LIKP.

DATA GT_MKPF TYPE TABLE OF MKPF.
DATA GS_MKPF TYPE MKPF.

DATA GT_VBAP TYPE TABLE OF VBAP.
DATA GS_VBAP TYPE VBAP.

*保存延期原因
DATA GT_ZSD009_1 TYPE TABLE OF ZSD009.
DATA GS_ZSD009_1 TYPE ZSD009.

DATA:IT_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
DATA:IT_T001 TYPE T001 OCCURS 0 WITH HEADER LINE.
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
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************

************************************************************************
* Selection Screen
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME .
SELECT-OPTIONS:  S_VKORG FOR VBAK-VKORG,
                 S_VBELN FOR VBAK-VBELN .

SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.

  SELECT VKORG FROM TVKO
    INTO CORRESPONDING FIELDS OF TABLE IT_TVKO.

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

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& 权限控制
*&---------------------------------------------------------------------*
FORM FRM_AUTH_CHECK.
  LOOP AT IT_TVKO WHERE VKORG IN S_VKORG.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
             ID 'VKORG' FIELD IT_TVKO-VKORG
*             ID 'VTWEG' FIELD 'DUMMY'
*             ID 'SPART' FIELD 'DUMMY'
*             ID 'ACTVT' FIELD 'DUMMY'
             .
    IF SY-SUBRC <> 0.
      MESSAGE E430(VELO) WITH IT_TVKO-VKORG.
    ENDIF.
  ENDLOOP.
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
  SELECT * FROM ZSD009_1
    INTO CORRESPONDING FIELDS OF TABLE GT_ZSD009
    WHERE VBELN IN S_VBELN
    AND   VKORG IN S_VKORG
    AND   PSTYV IN ('Z01','Z02','Z31','Z32','Z41','Z42').

  CHECK GT_ZSD009 IS NOT INITIAL.

  SELECT * FROM VBAP
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    FOR ALL ENTRIES IN GT_ZSD009
    WHERE VBELN = GT_ZSD009-VBELN.

  SELECT *
    FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE GT_ZMM002I
    FOR ALL ENTRIES IN GT_ZSD009
    WHERE VBELN = GT_ZSD009-VBELN.

*按日期降序排序，
  SORT GT_ZMM002I BY VBELN POSNR POSTDT DESCENDING .

*查询销售对应的公司间采购订单的采购历史
  SELECT *
    FROM EKKN
    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
    FOR ALL ENTRIES IN GT_ZSD009
    WHERE VBELN = GT_ZSD009-VBELN.

  IF GT_EKKN IS NOT INITIAL .
    SELECT *
      FROM EKBE
      INTO CORRESPONDING FIELDS OF TABLE GT_EKBE
      FOR ALL ENTRIES IN GT_EKKN
      WHERE EBELN = GT_EKKN-EBELN
      AND   EBELP = GT_EKKN-EBELP.
  ENDIF.

*查询销售订单对应公司间的物料凭证
  SELECT *
    FROM LIPS
    INTO CORRESPONDING FIELDS OF TABLE GT_LIPS
    FOR ALL ENTRIES IN GT_ZSD009
    WHERE VBELN = GT_ZSD009-VBELN.

  IF GT_LIPS IS NOT INITIAL .
    SELECT *
      FROM LIKP
      INTO CORRESPONDING FIELDS OF TABLE GT_LIKP
      FOR ALL ENTRIES IN GT_LIPS
      WHERE VBELN = GT_LIPS-VBELN
      AND   LFART = 'ZLDF'.

    SELECT *
      FROM MKPF
      INTO CORRESPONDING FIELDS OF TABLE GT_MKPF
      FOR ALL ENTRIES IN GT_LIKP
      WHERE XBLNR = GT_LIKP-VBELN.
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

  DATA L_POSTDT       TYPE ZMM002I-POSTDT."最大调拨日期
  DATA L_SJCKRQ       TYPE EKBE-BUDAT.    "最大出库日期
  DATA L_E_DAYS       TYPE VTBBEWE-ATAGE. "两日期间的天数
  DATA L_I_DATE_FROM  TYPE VTBBEWE-DBERVON.
  DATA L_I_DATE_TO    TYPE VTBBEWE-DBERBIS.

*获取销售长文本
  DATA LT_LINE TYPE TABLE OF TLINE.
  DATA LS_LINE TYPE TLINE.
  DATA L_NAME TYPE THEAD-TDNAME.

  LOOP AT GT_ZSD009 INTO GS_ZSD009.

* 判断查看创建人的权限
    AUTHORITY-CHECK OBJECT 'Z_SD_USER'
             ID 'VKORG'   FIELD GS_ZSD009-VKORG
             ID 'USR20_1' FIELD GS_ZSD009-ERNAM.
    IF SY-SUBRC <> 0.
      IF GS_ZSD009-ERNAM NE SY-UNAME.
        CLEAR GS_ZSD009.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING GS_ZSD009 TO GS_DATA.

*根据虚拟件，找到发货件，最晚的调拨日期
    LOOP AT  GT_VBAP INTO GS_VBAP
    WHERE    VBELN = GS_DATA-VBELN
    AND      UEPOS = GS_DATA-POSNR.

*移入整备库日期
      CLEAR:L_POSTDT,
            L_E_DAYS,
            L_I_DATE_FROM,
            L_I_DATE_TO.
      READ TABLE GT_ZMM002I INTO GS_ZMM002I
      WITH KEY VBELN = GS_VBAP-VBELN
               POSNR = GS_VBAP-POSNR.
      IF SY-SUBRC = 0.
        IF GS_ZMM002I-POSTDT > L_POSTDT.
          L_POSTDT = GS_ZMM002I-POSTDT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    GS_DATA-POSTDT = L_POSTDT.

    IF GS_DATA-POSTDT IS NOT INITIAL.

      L_I_DATE_FROM =  GS_DATA-VDATU .
      L_I_DATE_TO   =  GS_DATA-POSTDT.

*计算两日期之间的天数
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          I_DATE_FROM = L_I_DATE_FROM
*         I_KEY_DAY_FROM       =
          I_DATE_TO   = L_I_DATE_TO
*         I_KEY_DAY_TO         =
*         I_FLG_SEPARATE       = ' '
        IMPORTING
          E_DAYS      = L_E_DAYS
*         E_MONTHS    =
*         E_YEARS     =
        .
    ENDIF.
*延期入库天数
    GS_DATA-YQTS =  L_E_DAYS.

*延期罚金 = 延期入库天数 * “延期罚金率
    GS_DATA-YQFJ = GS_DATA-YQTS * GS_DATA-KBETR.

*实际出库日期(北京1100，与1310美国公司间，以采购订单上面历史记录为准)
    CLEAR L_SJCKRQ.
    IF GS_DATA-VKORG = '1000'
    OR GS_DATA-VKORG = '1310'.

*根据虚拟件，找到发货件，最晚的出库日期
      LOOP AT  GT_VBAP INTO GS_VBAP
      WHERE    VBELN = GS_DATA-VBELN
      AND      UEPOS = GS_DATA-POSNR.

        READ TABLE GT_EKKN INTO GS_EKKN
        WITH KEY VBELN = GS_VBAP-VBELN
                 VBELP = GS_VBAP-POSNR.
        IF SY-SUBRC = 0.
          READ TABLE GT_EKBE INTO GS_EKBE
          WITH KEY EBELN = GS_EKKN-EBELN
                   EBELP = GS_EKKN-EBELP
                   VGABE = '6'.
          IF SY-SUBRC = 0.
            IF GS_EKBE-BUDAT > L_SJCKRQ.
              L_SJCKRQ = GS_EKBE-BUDAT.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

*单个瓶体的对应的子件的最大调拨日期
      GS_DATA-SJCKRQ = L_SJCKRQ.

*实际出库日期（深圳，以外项交货单的物料凭证为准）
      CLEAR L_SJCKRQ.
    ELSEIF GS_DATA-VKORG = '1100'
    OR     GS_DATA-VKORG = '1110'.

*根据虚拟件，找到发货件，最晚的出库日期
      LOOP AT  GT_VBAP INTO GS_VBAP
      WHERE    VBELN = GS_DATA-VBELN
      AND      UEPOS = GS_DATA-POSNR.

        READ TABLE GT_LIPS INTO GS_LIPS
        WITH KEY VBELV = GS_VBAP-VBELN
                 POSNV = GS_VBAP-POSNR.
        IF SY-SUBRC = 0.
          READ TABLE GT_LIKP INTO GS_LIKP
          WITH KEY VBELN = GS_LIPS-VBELN.
          IF SY-SUBRC = 0.
            READ TABLE GT_MKPF INTO GS_MKPF
            WITH KEY XBLNR = GS_LIKP-VBELN.
            IF SY-SUBRC = 0.
              IF GS_EKBE-BUDAT > L_SJCKRQ.
                L_SJCKRQ = GS_EKBE-BUDAT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

*单个瓶体的对应的子件的最大出库日期
      GS_DATA-SJCKRQ = L_SJCKRQ.
    ENDIF.

*延期发货天数
    CLEAR:L_E_DAYS,
          L_I_DATE_FROM,
          L_I_DATE_TO.

    L_I_DATE_FROM = GS_DATA-VDATU.
    L_I_DATE_TO   = GS_DATA-SJCKRQ.

*计算两日期之间的天数
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        I_DATE_FROM = L_I_DATE_FROM
*       I_KEY_DAY_FROM       =
        I_DATE_TO   = L_I_DATE_TO
*       I_KEY_DAY_TO         =
*       I_FLG_SEPARATE       = ' '
      IMPORTING
        E_DAYS      = L_E_DAYS
*       E_MONTHS    =
*       E_YEARS     =
      .
*延期发货天数
    GS_DATA-YQFHTS = L_E_DAYS.
    IF GS_DATA-YQFHTS < 0.
      GS_DATA-YQFHTS  = 0.
    ENDIF.

*延期原因
    GS_DATA-YQYY   = GS_DATA-ZSD001.

*仓储费(当小于0为0)
    GS_DATA-CCF = GS_DATA-ZMENG * 3 * ( GS_DATA-YQFHTS - 15 ).
    IF GS_DATA-CCF < 0.
      GS_DATA-CCF = 0.
    ENDIF.

*整备费 = 100 *屏体面积 or 0
    IF GS_DATA-MVGR4   = 'Z01'
     OR GS_DATA-MVGR4  = 'Z04'
     OR GS_DATA-MVGR4  = 'Z07'
     OR GS_DATA-MVGR4  = 'Z12'
     OR GS_DATA-MVGR4  = 'Z14'.
      IF GS_DATA-YQFHTS > 150.
        GS_DATA-ZBF = 100 * GS_DATA-ZMENG.
      ELSE.
        GS_DATA-ZBF = 0.
      ENDIF.
    ENDIF.

    IF GS_DATA-MVGR4   = 'Z02'
     OR GS_DATA-MVGR4  = 'Z05'
     OR GS_DATA-MVGR4  = 'Z08'
     OR GS_DATA-MVGR4  = 'Z09'
     OR GS_DATA-MVGR4  = 'Z13'.
      IF GS_DATA-YQFHTS > 60.
        GS_DATA-ZBF = 100 * GS_DATA-ZMENG.
      ELSE.
        GS_DATA-ZBF = 0.
      ENDIF.
    ENDIF.

    IF  GS_DATA-MVGR4   = 'Z03'
     OR GS_DATA-MVGR4  = 'Z06'.
      IF GS_DATA-YQFHTS > 90.
        GS_DATA-ZBF = 100 * GS_DATA-ZMENG.
      ELSE.
        GS_DATA-ZBF = 0.
      ENDIF.
    ENDIF.

    IF  GS_DATA-MVGR4   = 'Z10'
     OR GS_DATA-MVGR4  = 'Z11'.
      IF GS_DATA-YQFHTS > 30.
        GS_DATA-ZBF = 100 * GS_DATA-ZMENG.
      ELSE.
        GS_DATA-ZBF = 0.
      ENDIF.
    ENDIF.

*总金额
    GS_DATA-ZJE = GS_DATA-CCF + GS_DATA-ZBF - GS_DATA-YQFJ.

*获取销售订单上的文本，项目名称
    REFRESH LT_LINE.
    IF GS_DATA-VBELN IS NOT INITIAL.
      L_NAME = GS_DATA-VBELN.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          CLIENT                  = SY-MANDT
          ID                      = 'Z001'
          LANGUAGE                = SY-LANGU
          NAME                    = L_NAME
          OBJECT                  = 'VBBK'
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      LOOP AT LT_LINE INTO LS_LINE  .
        CONCATENATE GS_DATA-Z001 LS_LINE-TDLINE INTO GS_DATA-Z001.
      ENDLOOP.

    ENDIF.
    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.
  ENDLOOP.

*根据销售订单号进行排序
  SORT GT_DATA BY VBELN.
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
*  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'ZBOX'.
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
  INIT_FIELDCAT 'VBELN'    TEXT-001         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'VKORG'    TEXT-002         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AUART'    TEXT-003         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'Z001'     TEXT-004         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZSD0302'  TEXT-005         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ERDAT'    TEXT-006         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ERNAM'    TEXT-007         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNNR'    TEXT-008        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'    TEXT-009         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEZEI_1'  TEXT-010         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEZEI_2'  TEXT-011         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZTXT'    TEXT-012         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LANDX'    TEXT-013         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZTXT_1'  TEXT-014         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEZEI'    TEXT-015         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSNR'    TEXT-016         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'    TEXT-017         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ARKTX'    TEXT-018         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZMENG'    TEXT-019         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VDATU'    TEXT-020         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSTDT'   TEXT-021         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQTS'     TEXT-022         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KBETR'    TEXT-023         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQFJ'     TEXT-024         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJCKRQ'   TEXT-025         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQFHTS'   TEXT-026         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YQYY'     TEXT-027         '20' '' '' 'X' '' '' ''.
  INIT_FIELDCAT 'CCF'      TEXT-028         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZBF'      TEXT-029         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJE'      TEXT-030         '' '' '' '' '' '' ''.
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
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

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


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.

  CASE R_UCOMM.
* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'VBELN'
        AND GS_DATA-VBELN IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD GS_DATA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
*
    WHEN '&DATA_SAVE'.
*更新数据库表
      REFRESH GT_ZSD009_1.
      READ TABLE GT_DATA INTO GS_DATA
       WITH KEY ZBOX = 'X'.

*判断是否选中行项目,保存延期原因
      IF SY-SUBRC = 0.
        LOOP AT GT_DATA INTO GS_DATA WHERE ZBOX = 'X'.
          GS_ZSD009_1-VBELN  = GS_DATA-VBELN.
          GS_ZSD009_1-POSNR  = GS_DATA-POSNR.
          GS_ZSD009_1-ZSD001 = GS_DATA-YQYY.
          APPEND GS_ZSD009_1 TO GT_ZSD009_1.
          CLEAR GS_ZSD009_1.
        ENDLOOP.

        MODIFY ZSD009 FROM TABLE GT_ZSD009_1.
        IF SY-SUBRC = 0.
          MESSAGE S002(Z001).
        ENDIF.
      ELSE.
        MESSAGE S003(Z001) DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.
ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&Form  frm_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*-->RR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM FRM_DATA_CHANGED USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  PERFORM FRM_DATA_ENTER USING ER_DATA_CHANGED..
ENDFORM.     "frm_data_changed
*&---------------------------------------------------------------------*
*&      Form  frm_data_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DATA_ENTER USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDFORM.                    "frm_data_enter
