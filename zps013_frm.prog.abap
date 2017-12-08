*&---------------------------------------------------------------------*
*&  包含                ZPS013_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DEAL_DATA .

  DATA: BEGIN OF LS_BQYSK ,
          FIELD(6) TYPE C,
          ZYSRQ    TYPE STRING,
          ZYSJE    TYPE STRING,
          VALUE    TYPE STRING,
        END OF LS_BQYSK .
  DATA LT_BQYSK LIKE TABLE OF LS_BQYSK .
  DATA: LT_BQYSK2 LIKE LT_BQYSK,
        LS_BQYSK2 LIKE LINE OF LT_BQYSK2.
  DATA: LT_BQYSK3 LIKE LT_BQYSK,
        LS_BQYSK3 LIKE LINE OF LT_BQYSK3.

  LS_BQYSK-FIELD = 'ZYSRQ1'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ2'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ3'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ4'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ5'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ6'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ7'.
  APPEND LS_BQYSK TO LT_BQYSK .
  LS_BQYSK-FIELD = 'ZYSRQ8'.
  APPEND LS_BQYSK TO LT_BQYSK .
  CLEAR LS_BQYSK .


  DATA: BEGIN OF LS_PRPS ,
          PSPNR  TYPE PRPS-PSPNR,
          POSID  TYPE PRPS-POSID,
          POST1  TYPE PRPS-POST1,
          PSPHI  TYPE PRPS-PSPHI,
          ZBMMC  TYPE PRPS-ZBMMC,
          ZJBR   TYPE PRPS-ZJBR,
          ZKHBM  TYPE PRPS-ZKHBM,
          ZHTJR  TYPE PRPS-ZHTJR,
          ZYSRQ1 TYPE PRPS-ZYSRQ1,
          ZYSJE1 TYPE PRPS-ZYSJE1,
          ZYSRQ2 TYPE PRPS-ZYSRQ2,
          ZYSJE2 TYPE PRPS-ZYSJE2,
          ZYSRQ3 TYPE PRPS-ZYSRQ3,
          ZYSJE3 TYPE PRPS-ZYSJE3,
          ZYSRQ4 TYPE PRPS-ZYSRQ4,
          ZYSJE4 TYPE PRPS-ZYSJE4,
          ZYSRQ5 TYPE PRPS-ZYSRQ5,
          ZYSJE5 TYPE PRPS-ZYSJE5,
          ZYSRQ6 TYPE PRPS-ZYSRQ6,
          ZYSJE6 TYPE PRPS-ZYSJE6,
          ZYSRQ7 TYPE PRPS-ZYSRQ7,
          ZYSJE7 TYPE PRPS-ZYSJE7,
          ZYSRQ8 TYPE PRPS-ZYSRQ8,
          ZYSJE8 TYPE PRPS-ZYSJE8,
          ZYYKXX TYPE PRPS-ZYYKXX,
          ZZXWT  TYPE PRPS-ZZXWT,
        END OF LS_PRPS .
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .
  DATA: BEGIN OF LS_PRPS2 ,
          POSID TYPE ZFI005-XMBH,
        END OF LS_PRPS2 .
  DATA LT_PRPS2 LIKE TABLE OF LS_PRPS2 .
  DATA: BEGIN OF LS_PROJ ,
          PSPNR TYPE PROJ-PSPNR,
          PSPID TYPE PROJ-PSPID,
          POST1 TYPE PROJ-POST1,
          VBUKR TYPE PROJ-VBUKR,
        END OF LS_PROJ .
  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  DATA: BEGIN OF LS_KNA1 ,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF LS_KNA1 .
  DATA LT_KNA1 LIKE TABLE OF LS_KNA1 .
  DATA: BEGIN OF LS_ZFI005 ,
          XSHKNO TYPE ZFI005-XSHKNO,
          BUKRS  TYPE ZFI005-BUKRS,
          BUKRST TYPE ZFI005-BUKRST,
          XMBH   TYPE ZFI005-XMBH,
          HSL    TYPE ZFI005-HSL,
        END OF LS_ZFI005 .
  DATA LT_ZFI005 LIKE TABLE OF LS_ZFI005 .

  DATA L_MONTH TYPE VTBBEWE-ATAGE .
  DATA L_NUM TYPE I VALUE IS INITIAL .
  DATA: L_DATE_FROM TYPE VTBBEWE-DBERVON,
        L_DATE_TO   TYPE VTBBEWE-DBERBIS.

* 部门名称KEY转换函数专用参数
*  DATA: L_VRMID   TYPE VRM_ID,
*        LT_VALUES TYPE VRM_VALUES WITH HEADER LINE.

  SELECT PSPNR
         POSID
         POST1
         PSPHI
         ZBMMC
         ZJBR
         ZKHBM
         ZHTJR
         ZYSRQ1 "预收日期1
         ZYSJE1 "预收金额1
         ZYSRQ2
         ZYSJE2
         ZYSRQ3
         ZYSJE3
         ZYSRQ4
         ZYSJE4
         ZYSRQ5
         ZYSJE5
         ZYSRQ6
         ZYSJE6
         ZYSRQ7
         ZYSJE7
         ZYSRQ8
         ZYSJE8
         ZYYKXX
         ZZXWT
    INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
    FROM PRPS
   WHERE POSID IN S_POSID .
  SORT LT_PRPS BY PSPHI .
  IF LT_PRPS IS NOT INITIAL .
** 客户编号加前导零
*    LOOP AT LT_PRPS INTO LS_PRPS .
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = LS_PRPS-ZKHBM
*        IMPORTING
*          OUTPUT = LS_PRPS-ZKHBM.
*      MODIFY LT_PRPS FROM LS_PRPS .
*      CLEAR LS_PRPS .
*    ENDLOOP.
    SELECT PSPNR
           PSPID
           POST1
           VBUKR
      INTO CORRESPONDING FIELDS OF TABLE LT_PROJ
      FROM PROJ
       FOR ALL ENTRIES IN LT_PRPS
     WHERE PSPNR = LT_PRPS-PSPHI
       AND VBUKR = P_BUKRS .
    SORT LT_PROJ BY PSPNR .
    LOOP AT LT_PRPS INTO LS_PRPS .
      READ TABLE LT_PROJ WITH KEY PSPNR = LS_PRPS-PSPHI BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC <> 0 .
        DELETE LT_PRPS WHERE PSPNR = LS_PRPS-PSPNR .
      ENDIF.
    ENDLOOP.
    LOOP AT LT_PRPS INTO LS_PRPS .
      LS_PRPS2-POSID = LS_PRPS-POSID .
      APPEND LS_PRPS2 TO LT_PRPS2 .
      CLEAR LS_PRPS .
      CLEAR LS_PRPS2 .
    ENDLOOP.

    SELECT KUNNR
           NAME1
      INTO CORRESPONDING FIELDS OF TABLE LT_KNA1
      FROM KNA1
       FOR ALL ENTRIES IN LT_PRPS
     WHERE KUNNR = LT_PRPS-ZKHBM .
    SORT LT_KNA1 BY KUNNR .

    SELECT XSHKNO
           BUKRS
           BUKRST
           XMBH
           HSL
      INTO CORRESPONDING FIELDS OF TABLE LT_ZFI005
      FROM ZFI005
       FOR ALL ENTRIES IN LT_PRPS2
     WHERE XMBH = LT_PRPS2-POSID .
    SORT LT_ZFI005 BY XMBH .

* 计算字段值并填充ALV
    LOOP AT LT_PRPS INTO LS_PRPS .
      L_NUM = L_NUM + 1 .
      GS_ALV-NUM = L_NUM .
      CASE LS_PRPS-ZBMMC .
        WHEN '1' .
          GS_ALV-ZBMMC = '数字文化体验BU'.
        WHEN '2' .
          GS_ALV-ZBMMC = '技术服务部'.
        WHEN '3' .
          GS_ALV-ZBMMC = '文化旅游展演BU'.
        WHEN '4' .
          GS_ALV-ZBMMC = '公共文化设施BU'.
        WHEN OTHERS.
      ENDCASE.
      GS_ALV-ZJBR = LS_PRPS-ZJBR .
      READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY KUNNR = LS_PRPS-ZKHBM BINARY SEARCH .
      IF SY-SUBRC = 0 .
        GS_ALV-NAME1 = LS_KNA1-NAME1 .
      ENDIF.
      GS_ALV-ZHTJR = LS_PRPS-ZHTJR .
      READ TABLE LT_ZFI005 INTO LS_ZFI005 WITH KEY XMBH+0(24) = LS_PRPS-POSID BINARY SEARCH .
      IF SY-SUBRC = 0 .
        GS_ALV-HSL = LS_ZFI005-HSL .
      ENDIF.
      IF GS_ALV-ZHTJR IS NOT INITIAL AND ABS( GS_ALV-ZHTJR ) NE '0'.
        GS_ALV-LJHKL = GS_ALV-HSL / GS_ALV-ZHTJR .
      ENDIF.
      GS_ALV-ZMYSK = GS_ALV-ZHTJR - GS_ALV-HSL .
* 算本期应收款
      MOVE-CORRESPONDING LT_BQYSK TO LT_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ1' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ1 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE1 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ1 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2 INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ2' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ2 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE2 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ2 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ3' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ3 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE3 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ3 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ4' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ4 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE4 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ4 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ5' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ5 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE5 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ5 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ6' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ6 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE6 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ6 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ7' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ7 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE7 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ7 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      READ TABLE LT_BQYSK2 INTO LS_BQYSK2 WITH KEY FIELD = 'ZYSRQ8' .
      LS_BQYSK2-ZYSRQ = LS_PRPS-ZYSRQ8 .
      LS_BQYSK2-ZYSJE = LS_PRPS-ZYSJE8 .
      LS_BQYSK2-VALUE = SY-DATUM - LS_PRPS-ZYSRQ8 .
      MODIFY LT_BQYSK2 FROM LS_BQYSK2  INDEX SY-TABIX .
      CLEAR LS_BQYSK2 .

      SORT LT_BQYSK2 BY VALUE .
      MOVE-CORRESPONDING LT_BQYSK2 TO LT_BQYSK3 .
      DELETE LT_BQYSK2 WHERE VALUE < 0 .
      IF LT_BQYSK2 IS NOT INITIAL .
        READ TABLE LT_BQYSK2 INTO LS_BQYSK2 INDEX 1 .
        SORT LT_BQYSK3 BY ZYSRQ DESCENDING .
        READ TABLE LT_BQYSK3 WITH KEY ZYSRQ = LS_BQYSK2-ZYSRQ TRANSPORTING NO FIELDS .
        LOOP AT LT_BQYSK3 INTO LS_BQYSK3 FROM SY-TABIX .
          GS_ALV-BQYSK = GS_ALV-BQYSK + LS_BQYSK3-ZYSJE .
        ENDLOOP.
        GS_ALV-BQYSK = GS_ALV-BQYSK - GS_ALV-HSL .
      ENDIF.

* 开始其它字段逻辑
      GS_ALV-ZYSRQ = LS_BQYSK2-ZYSRQ .
      IF GS_ALV-BQYSK IS NOT INITIAL .
        L_DATE_FROM = LS_BQYSK2-ZYSRQ .
        L_DATE_TO = SY-DATUM .
        CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          EXPORTING
            I_DATE_FROM = L_DATE_FROM
            I_DATE_TO   = L_DATE_TO
          IMPORTING
            E_MONTHS    = L_MONTH.
        GS_ALV-ZL = L_MONTH .
      ENDIF.
      GS_ALV-ZJZYLX_MONTH = GS_ALV-BQYSK * '0.08' * L_MONTH / 12 .
      GS_ALV-ZJZYLX_DATE = GS_ALV-ZJZYLX_DATE / 30 .
      GS_ALV-ZYYKXX = LS_PRPS-ZYYKXX .
      GS_ALV-ZZXWT = LS_PRPS-ZZXWT .
      READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPNR = LS_PRPS-PSPHI BINARY SEARCH .
      IF SY-SUBRC = 0 .
        GS_ALV-PSPID = LS_PROJ-PSPID .
        GS_ALV-POST1 = LS_PROJ-POST1 .
      ENDIF.

      APPEND GS_ALV TO GT_ALV.
      CLEAR GS_ALV .

      REFRESH LT_BQYSK2 .
      REFRESH LT_BQYSK3 .
      CLEAR: L_DATE_FROM ,L_DATE_TO ,L_MONTH .


    ENDLOOP.

  ELSE .

    MESSAGE 'WBS无数据!' TYPE 'E' DISPLAY LIKE 'S'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .
  PERFORM INIT_LAYOUT.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.            "字段列定义
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
*  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT  .
*                          TABLES GT_LVC              "输出
*                            GT_SORT
*                            GT_ALV
*                     USING 'ALV_PF_STATUS'
*                           'ALV_USER_COMMAND'
*                           GS_LAYOUT
*                           GS_VARIANT
*                           GS_GRID_SETTINGS.

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
  GS_LAYOUT-ZEBRA        = 'X'.
  GS_LAYOUT-CWIDTH_OPT   = 'X'.
  GS_LAYOUT-BOX_FNAME = 'SEL'.
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
  INIT_FIELDCAT 'NUM'        '序号'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'JD'        '阶段'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZBMMC'        '部门'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJBR'        '铁三角'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'        '客户名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LJHKL'        '累计回款率'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHTJR'        '合同金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HSL'        '累计收款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZMYSK'        '账面应收款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BQYSK'        '按合同约定本期应收款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZYSRQ'        '按约定应开始收款时间'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZL'        '账龄（月）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJZYLX_MONTH'        '资金占用利息（累计）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJZYLX_DATE'        '资金占用利息（每天）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZYYKXX'        '收款条件'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZZXWT'        '最新进展及问题'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PSPID'        '项目编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      I_CALLBACK_PROGRAM = SY-REPID
*     I_CALLBACK_PF_STATUS_SET = PU_STATUS
*     I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   = ''
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    = PW_GRID_SETTINGS
      IS_LAYOUT_LVC      = GS_LAYOUT
      IT_FIELDCAT_LVC    = GT_LVC
*     IT_EXCLUDING       = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        = PT_SORT[]
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = 'A'
*     IS_VARIANT         = PW_VARIANT
*     IT_EVENTS          = GT_EVENTS
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB           = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
