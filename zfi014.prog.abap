*&---------------------------------------------------------------------*
*& Report  ZFI014
*& 现金流量表
*&---------------------------------------------------------------------*
*&
*& 作者：汉得 唐博
*& 开发日期：2015-2-2
*&---------------------------------------------------------------------*

REPORT ZFI014.

TABLES: BKPF,BSEG,ZFI003.

*&---------------------------------------------------------------------*
*& 选择屏幕
*&---------------------------------------------------------------------*
SELECT-OPTIONS S_BUKRS FOR BKPF-BUKRS OBLIGATORY.
PARAMETERS P_GJAHR TYPE BKPF-GJAHR DEFAULT SY-DATUM(4).
SELECT-OPTIONS S_MONAT FOR BKPF-MONAT DEFAULT SY-DATUM+4(2) NO-EXTENSION.
PARAMETERS P_GJAHR2 TYPE BKPF-GJAHR.
SELECT-OPTIONS S_MONAT2 FOR BKPF-MONAT NO-EXTENSION.
*&---------------------------------------------------------------------*
*& 全局变量
*&---------------------------------------------------------------------*
RANGES R_GRAY FOR SCREEN-NAME.
RANGES R_HKONT FOR BSEG-HKONT.
RANGES R_HKONT2 FOR BSEG-HKONT.
DATA  T_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA  W_FIELDCAT LIKE LINE OF T_FIELDCAT.
DATA: IT_LISTHEADER TYPE SLIS_T_LISTHEADER,
      WA_LISTHEADER TYPE SLIS_LISTHEADER,
      IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
*&---------------------------------------------------------------------*
*& 内表、工作区
*&---------------------------------------------------------------------*
DATA: BEGIN OF T_ALV OCCURS 0.
  INCLUDE STRUCTURE ZFI003.
DATA: RSTGR TYPE BSEG-RSTGR,
      LOW TYPE I,
      HIGH TYPE I,
      SUB TYPE STRING,
      EXIT TYPE STRING,
      BELNRS TYPE TABLE OF BSEG,
      BELNRS_LAST TYPE TABLE OF BSEG,
      END OF T_ALV,
  W_ALV LIKE LINE OF T_ALV,
  T_BKPF TYPE TABLE OF BKPF WITH HEADER LINE,
  T_BKPF_LAST TYPE TABLE OF BKPF WITH HEADER LINE,
  T_BSEG TYPE TABLE OF BSEG WITH HEADER LINE,
  T_BSEG_LAST TYPE TABLE OF BSEG WITH HEADER LINE,
  BEGIN OF T_BSEG_RSTGR OCCURS 0,
    RSTGR TYPE BSEG-RSTGR,
    DMBTR TYPE BSEG-DMBTR,
  END OF T_BSEG_RSTGR,
  T_BSEG_RSTGR_LAST LIKE TABLE OF T_BSEG_RSTGR WITH HEADER LINE,
  T_SUB TYPE TABLE OF STRING WITH HEADER LINE.

  DATA:
  T_FAGLFLEXT TYPE TABLE OF FAGLFLEXT WITH HEADER LINE,
  T_FAGLFLEXT2 TYPE TABLE OF FAGLFLEXT WITH HEADER LINE,
  T_FAGLFLEXT_SUM TYPE TABLE OF FAGLFLEXT WITH HEADER LINE,
  T_FAGLFLEXT_SUM2 TYPE TABLE OF FAGLFLEXT WITH HEADER LINE,
  T_ZFI003_BSEG TYPE TABLE OF BSEG WITH HEADER LINE.
*&---------------------------------------------------------------------*
*& 初始化
*&---------------------------------------------------------------------*
INITIALIZATION.
P_GJAHR2 = P_GJAHR.

S_MONAT2-SIGN =  'I'.
S_MONAT2-OPTION =  'EQ'.
S_MONAT2-LOW = S_MONAT-LOW - 1.
IF S_MONAT2-LOW EQ '00'.
  S_MONAT2-LOW = '12'.
  SUBTRACT 1 FROM P_GJAHR2.
ENDIF.
APPEND S_MONAT2.
CLEAR S_MONAT2.
CLEAR R_HKONT[].
CLEAR R_HKONT2[].
*PERFORM APPEND_RANGE USING:
*      'R_GRAY' 'I' 'EQ' 'P_MONAT' '',
*      'R_GRAY' 'I' 'EQ' 'S_MONAT2' '',
*      'R_GRAY' 'I' 'EQ' 'P_GJAHR' '',
*      'R_GRAY' 'I' 'EQ' 'P_GJAHR2' ''.
*PERFORM APPEND_RANGE USING:
*      'R_HKONT' 'I' 'CP' '1001*' '',
*      'R_HKONT' 'I' 'CP' '1002*' '',
*      'R_HKONT' 'I' 'CP' '1015*' '',
*      'R_HKONT' 'I' 'EQ' '1121010000' '',
*      'R_HKONT' 'I' 'EQ' '1121020000' '',
*      'R_HKONT' 'I' 'EQ' '2201010000' '',
*      'R_HKONT' 'I' 'EQ' '2201020000' ''.
*PERFORM APPEND_RANGE USING:
*      'R_HKONT2' 'I' 'CP' '1001*' '',
*      'R_HKONT2' 'I' 'CP' '1002*' '',
*      'R_HKONT2' 'I' 'EQ' '1121010000' '',
*      'R_HKONT2' 'I' 'EQ' '1121020000' ''.
PERFORM APPEND_RANGE USING:
      'R_HKONT' 'I' 'CP' '1001*' '',
      'R_HKONT' 'I' 'CP' '1002*' '',
      'R_HKONT' 'I' 'CP' '1012*' ''.
PERFORM APPEND_RANGE USING:
      'R_HKONT2' 'I' 'CP' '1001*' '',
      'R_HKONT2' 'I' 'CP' '1002*' '',
      'R_HKONT2' 'I' 'CP' '1012*' ''.
*&---------------------------------------------------------------------*
*& 权限检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  AUTHORITY-CHECK OBJECT 'A_PERI_BUK'
*           ID 'BUKRS' FIELD P_BUKRS.
*  IF sy-subrc <> 0.
*    MESSAGE e899(mm) WITH '您没有公司代码' P_BUKRS '的权限'.
*  ENDIF.
*&---------------------------------------------------------------------*
*& 屏幕显示
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN."会计期间，不允许挑选
*    IF SCREEN-NAME IN R_GRAY.
*      SCREEN-INPUT = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*&---------------------------------------------------------------------*
*& 开始选择
*&---------------------------------------------------------------------*
START-OF-SELECTION.
PERFORM GET_ALV_DATA.
PERFORM BUILD_FCAT.
PERFORM SHOW_ALV.
*&---------------------------------------------------------------------*
*& 子函数
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  APPEND_HKONT
*&---------------------------------------------------------------------*
*       HKONT
*----------------------------------------------------------------------*
*      -->P_0794   text
*----------------------------------------------------------------------*
FORM APPEND_RANGE  USING VALUE(RANGE) VALUE(SIGN) VALUE(OPTION) VALUE(LOW) VALUE(HIGH).
  FIELD-SYMBOLS <FS_RANGE> TYPE ANY.
  FIELD-SYMBOLS <FS_RANGE_TAB> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <FS_COMP> TYPE ANY.
  DATA L_NAME TYPE STRING.
  ASSIGN (RANGE) TO <FS_RANGE>.
  CONCATENATE RANGE '[]' INTO L_NAME.
  ASSIGN (L_NAME) TO <FS_RANGE_TAB>.
  ASSIGN COMPONENT 'SIGN' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = SIGN.
  ASSIGN COMPONENT 'OPTION' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = OPTION.
  ASSIGN COMPONENT 'LOW' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = LOW.
  ASSIGN COMPONENT 'HIGH' OF STRUCTURE <FS_RANGE> TO <FS_COMP>.
  <FS_COMP> = HIGH.
  APPEND <FS_RANGE> TO <FS_RANGE_TAB>.
  CLEAR <FS_RANGE>.
ENDFORM.                    " APPEND_HKONT
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_ALV .
  CLEAR IS_LAYOUT.
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSIN                        = ' '
*     I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = SY-REPID
     I_CALLBACK_PF_STATUS_SET          = 'SET_PF_STATUS'
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
     I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
     IS_LAYOUT                         = IS_LAYOUT
     IT_FIELDCAT                       = T_FIELDCAT
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
     I_SAVE                            = 'A'
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = T_ALV[]
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
*Implement suitable error handling here
  ENDIF.
ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FCAT .
  CLEAR: T_FIELDCAT[], W_FIELDCAT.
  PERFORM APPEND_FIELDCAT USING:
      '项目' 'ITEM',
      '行次' 'LINE_INDEX',
      '金额' 'AMOUNT',
      '上期' 'AMOUNT_LAST'.
ENDFORM.                    " BUILD_FCAT
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DESC   text
*      -->FIELD   text
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT  USING    VALUE(DESC)
                               VALUE(FIELD).
  W_FIELDCAT-SELTEXT_S =
  W_FIELDCAT-SELTEXT_M =
  W_FIELDCAT-SELTEXT_L =
  W_FIELDCAT-REPTEXT_DDIC = DESC.
  W_FIELDCAT-FIELDNAME = FIELD.
  W_FIELDCAT-REF_TABNAME = 'ZFI003'.
  W_FIELDCAT-REF_FIELDNAME = FIELD.
  APPEND W_FIELDCAT TO T_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
**  Type H is used to display headers i.e. big font
*   DATA L_RESULS_COUNT TYPE I.
*   L_RESULS_COUNT = LINES( GT_RESULT[] ).
*   WRITE L_RESULS_COUNT TO WA_LISTHEADER-INFO.
*   CONCATENATE WA_LISTHEADER-INFO 'Results' INTO WA_LISTHEADER-INFO SEPARATED BY SPACE.
*   WA_LISTHEADER-TYP  = 'H'.
*  APPEND wa_listheader TO it_listheader.
*  CLEAR wa_listheader.
*  Type S is used to display key and value pairs
  DATA L_COUNT TYPE I.
  CLEAR IT_LISTHEADER[].
  CLEAR L_COUNT.
  SELECT COUNT( * ) FROM T001 INTO L_COUNT WHERE BUKRS IN S_BUKRS.
  IF L_COUNT > 1.
    WA_LISTHEADER-INFO = '*'.
  ELSE.
    SELECT SINGLE BUTXT FROM T001 INTO WA_LISTHEADER-INFO WHERE BUKRS IN S_BUKRS.
  ENDIF.
  IF S_MONAT-HIGH IS NOT INITIAL.
    CONCATENATE '公司名称:' WA_LISTHEADER-INFO '　　　　　　　'
    P_GJAHR '年' S_MONAT-LOW '-' S_MONAT-HIGH '期间　　　　　　　货币:元' INTO WA_LISTHEADER-INFO.
  ELSE.
    CONCATENATE '公司名称:' WA_LISTHEADER-INFO '　　　　　　　'
    P_GJAHR '年' S_MONAT-LOW '期间　　　　　　　货币:元' INTO WA_LISTHEADER-INFO.
  ENDIF.
**  Type A is used to display italic font
  WA_LISTHEADER-TYP = 'A'.
  WA_LISTHEADER-INFO = WA_LISTHEADER-INFO.
  APPEND WA_LISTHEADER TO IT_LISTHEADER.
  CLEAR WA_LISTHEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_LISTHEADER.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  GET_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ALV_DATA .
  CLEAR: T_ALV[],T_BKPF[],T_BKPF_LAST[],T_BSEG_RSTGR[],T_BSEG_RSTGR_LAST[].
  PERFORM APPEND_ALV USING:
        '1' ' 一、经营活动产生的现金流量：' '' '' '' '' '',
        '2' '  销售商品、提供劳务收到的现金' 'A01' '' '' '' '',
        '3' '  收到的税费返还' 'A02' '' '' '' '',
        '4' '  收到的其他与经营活动有关的现金' 'A03' '' '' '' '',
        '5' '  经营活动现金流入小计' '' '2' '4' '' '',
        '6' '  购买商品、接受劳务支付的现金' 'B01' '' '' '' '',
        '7' '  支付给职工以及为职工支付的现金' 'B02' '' '' '' '',
        '8' '  支付的各项税费' 'B03' '' '' '' '',
        '9' '  支付其他与经营活动有关的现金' 'B04' '' '' '' '',
        '10' '  经营活动现金流出小计' '' '6' '9' '' '',
        '11' '  经营活动产生的现金流量净额' '' '' '' '5,-10' '',
        '12' ' 二、投资活动产生的现金流量：' '' '' '' '' '',
        '13' '  收回投资收到的现金' 'C01' '' '' '' '',
        '14' '  取得投资收益收到的现金' 'C02' '' '' '' '',
        '15' '  处置固定资产、无形资产和其他长期资产收回的现金净额' 'C03' '' '' '' '',
        '16' '  处置子公司及其他营业单位收到的现金净额' '' '' '' '' '',
        '17' '  收到其他与投资活动有关的现金' 'C04' '' '' '' '',
        '18' '  投资活动现金流入小计' '' '13' '17' '' '',
        '19' '  购建固定资产、无形资产和其他长期资产支付的现金' 'D01' '' '' '' '',
        '20' '  投资支付的现金' 'D02' '' '' '' '',
        '21' '  取得子公司及其他营业单位支付的现金净额' '' '' '' '' '',
        '22' '  支付其他与投资活动有关的现金' 'D03' '' '' '' '',
        '23' '  投资活动现金流出小计' '' '19' '22' '' '',
        '24' '  投资活动产生的现金流量净额' '' '' '' '18,-23' '',
        '25' ' 三、筹资活动产生的现金流量：' '' '' '' '' '',
        '26' '  吸收投资收到的现金' 'E01' '' '' '' '',
        '27' '  取得借款收到的现金' 'E02' '' '' '' '',
        '28' '  收到的其他与筹资活动有关的现金' 'E03' '' '' '' 'FRM_AMOUNT28',
        '29' '  筹资活动现金流入小计' '' '26' '28' '' '',
        '30' '  偿还债务所支付的现金' 'F01' '' '' '' '',
        '31' '  分配股利、利润或偿付利息所支付的现金' 'F02' '' '' '' '',
        '32' '  支付的其他与筹资活动有关的现金' 'F03' '' '' '' '',
        '33' '  筹资活动现金流出小计' '' '30' '32' '' '',
        '34' '  筹资活动产生的现金流量净额' '' '' '' '29,-33' '',
        '35' ' 四、汇率变动对现金的影响' 'G01' '' '' '' '',
        '36' ' 五、现金及现金等价物净增加额' '' '' '' '11,24,34,35' '',
        '37' '  加：期初现金及现金等价物余额' '' '' '' '' 'FRM_AMOUNT37',
        '38' ' 六、期末现金及现金等价物余额' '' '' '' '36,37' ''.

  SELECT *
    FROM BKPF
    INTO CORRESPONDING FIELDS OF TABLE T_BKPF
    WHERE BUKRS IN S_BUKRS
    AND GJAHR EQ P_GJAHR
    AND MONAT IN S_MONAT
    .

  SELECT *
    FROM BKPF
    INTO CORRESPONDING FIELDS OF TABLE T_BKPF_LAST
    WHERE BUKRS IN S_BUKRS
    AND GJAHR EQ P_GJAHR2
    AND MONAT IN S_MONAT2
    .

  IF T_BKPF[] IS NOT INITIAL.
    SELECT *
      FROM BSEG
      INTO CORRESPONDING FIELDS OF TABLE T_BSEG
      FOR ALL ENTRIES IN T_BKPF
      WHERE BUKRS EQ T_BKPF-BUKRS
      AND BUKRS EQ T_BKPF-BUKRS
      AND BELNR EQ T_BKPF-BELNR
      AND GJAHR EQ T_BKPF-GJAHR
      AND HKONT IN R_HKONT.
  ENDIF.

  IF T_BKPF_LAST[] IS NOT INITIAL.
    SELECT *
      FROM BSEG
      INTO CORRESPONDING FIELDS OF TABLE T_BSEG_LAST
      FOR ALL ENTRIES IN T_BKPF_LAST
      WHERE BUKRS EQ T_BKPF_LAST-BUKRS
      AND BUKRS EQ T_BKPF_LAST-BUKRS
      AND BELNR EQ T_BKPF_LAST-BELNR
      AND GJAHR EQ T_BKPF_LAST-GJAHR
      AND HKONT IN R_HKONT.
  ENDIF.

  PERFORM SUM_RSTGR TABLES T_BSEG T_BSEG_RSTGR.
  PERFORM SUM_RSTGR TABLES T_BSEG_LAST T_BSEG_RSTGR_LAST.

  LOOP AT T_ALV.
    IF T_ALV-RSTGR IS NOT INITIAL.
      READ TABLE T_BSEG_RSTGR WITH KEY RSTGR = T_ALV-RSTGR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        T_ALV-AMOUNT = T_BSEG_RSTGR-DMBTR.
        LOOP AT T_BSEG WHERE RSTGR EQ T_ALV-RSTGR.
          APPEND T_BSEG TO T_ALV-BELNRS[].
        ENDLOOP.
      ENDIF.
      READ TABLE T_BSEG_RSTGR_LAST WITH KEY RSTGR = T_ALV-RSTGR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        T_ALV-AMOUNT_LAST = T_BSEG_RSTGR_LAST-DMBTR.
        LOOP AT T_BSEG_LAST WHERE RSTGR EQ T_ALV-RSTGR.
          APPEND T_BSEG_LAST TO T_ALV-BELNRS_LAST[].
        ENDLOOP.
      ENDIF.
      IF T_ALV-RSTGR(1) EQ 'B'
        OR T_ALV-RSTGR(1) EQ 'D'
        OR T_ALV-RSTGR(1) EQ 'F'.
        MULTIPLY T_ALV-AMOUNT BY -1.
        MULTIPLY T_ALV-AMOUNT_LAST BY -1.
      ENDIF.
    ELSEIF T_ALV-LOW IS NOT INITIAL AND T_ALV-HIGH IS NOT INITIAL.
      LOOP AT T_ALV INTO W_ALV FROM T_ALV-LOW TO T_ALV-HIGH.
        ADD W_ALV-AMOUNT TO T_ALV-AMOUNT.
        ADD W_ALV-AMOUNT_LAST TO T_ALV-AMOUNT_LAST.
        APPEND LINES OF W_ALV-BELNRS[] TO T_ALV-BELNRS[].
        APPEND LINES OF W_ALV-BELNRS_LAST[] TO T_ALV-BELNRS_LAST[].
      ENDLOOP.
    ELSEIF T_ALV-SUB IS NOT INITIAL.
      SPLIT T_ALV-SUB AT ',' INTO TABLE T_SUB.
      LOOP AT T_SUB.
        IF T_SUB(1) EQ '-'.
          DATA L_STRLEN.
          L_STRLEN = STRLEN( T_SUB ) - 1.
          T_SUB = T_SUB+1(L_STRLEN).
*          T_SUB = T_SUB * -1.
          READ TABLE T_ALV INTO W_ALV INDEX T_SUB.
          IF SY-SUBRC EQ 0.
            SUBTRACT W_ALV-AMOUNT FROM T_ALV-AMOUNT.
            APPEND LINES OF W_ALV-BELNRS[] TO T_ALV-BELNRS[].
            SUBTRACT W_ALV-AMOUNT_LAST FROM T_ALV-AMOUNT_LAST.
            APPEND LINES OF W_ALV-BELNRS_LAST[] TO T_ALV-BELNRS_LAST[].
          ENDIF.
        ELSE.
          READ TABLE T_ALV INTO W_ALV INDEX T_SUB.
          IF SY-SUBRC EQ 0.
            ADD W_ALV-AMOUNT TO T_ALV-AMOUNT.
            APPEND LINES OF W_ALV-BELNRS[] TO T_ALV-BELNRS[].
            ADD W_ALV-AMOUNT_LAST TO T_ALV-AMOUNT_LAST.
            APPEND LINES OF W_ALV-BELNRS_LAST[] TO T_ALV-BELNRS_LAST[].
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF T_ALV-EXIT IS NOT INITIAL.
      PERFORM (T_ALV-EXIT) IN PROGRAM.
    ENDIF.
    MODIFY T_ALV.
  ENDLOOP.
ENDFORM.                    " GET_ALV_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITEM   text
*      -->LINE_INDEX   text
*      -->LOW   text
*      -->HIGH   text
*      -->SUB   text
*----------------------------------------------------------------------*
FORM APPEND_ALV  USING    VALUE(LINE_INDEX)
                          VALUE(ITEM)
                          VALUE(RSTGR)
                          VALUE(LOW)
                          VALUE(HIGH)
                          VALUE(SUB)
                          VALUE(EXIT).
  T_ALV-LINE_INDEX = LINE_INDEX.
  T_ALV-ITEM = ITEM.
  T_ALV-RSTGR = RSTGR.
  T_ALV-LOW = LOW.
  T_ALV-HIGH = HIGH.
  T_ALV-SUB = SUB.
  T_ALV-EXIT = EXIT.
  APPEND T_ALV.
  CLEAR T_ALV.
ENDFORM.                    " APPEND_ALV
*&---------------------------------------------------------------------*
*&      Form  SUM_RSTGR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_BSEG  text
*      -->PT_BSEG_RSTGR  text
*----------------------------------------------------------------------*
FORM SUM_RSTGR  TABLES   PT_BSEG STRUCTURE T_BSEG
                         PT_BSEG_RSTGR STRUCTURE T_BSEG_RSTGR.
  DATA PT_BSEG_RSTGR_TMP LIKE TABLE OF T_BSEG_RSTGR WITH HEADER LINE.
  CLEAR PT_BSEG_RSTGR_TMP[].
  LOOP AT PT_BSEG.
    MOVE PT_BSEG-RSTGR TO PT_BSEG_RSTGR-RSTGR.
    MOVE PT_BSEG-DMBTR TO PT_BSEG_RSTGR-DMBTR.
    IF PT_BSEG-SHKZG EQ 'H'.
      MULTIPLY PT_BSEG_RSTGR-DMBTR BY -1.
    ENDIF.
    APPEND PT_BSEG_RSTGR.
    CLEAR PT_BSEG_RSTGR.
  ENDLOOP.
  SORT PT_BSEG_RSTGR BY RSTGR.
  PT_BSEG_RSTGR_TMP[] = PT_BSEG_RSTGR[].
  CLEAR PT_BSEG_RSTGR[].
  LOOP AT PT_BSEG_RSTGR_TMP.
    AT END OF RSTGR.
      SUM.
      APPEND PT_BSEG_RSTGR_TMP TO PT_BSEG_RSTGR.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " SUM_RSTGR
FORM FRM_AMOUNT37.
**
  CLEAR: T_FAGLFLEXT[].
  SELECT  RYEAR "会计年度
*          DRCRK "借方/贷方标识
*          RACCT "帐号
*          RBUKRS "公司代码
*          RBUSA "业务范围
          HSLVT "用本币计算的结转余额
          HSL01 "按本位币的期间中的业务总计
          HSL02 "按本位币的期间中的业务总计
          HSL03 "按本位币的期间中的业务总计
          HSL04 "按本位币的期间中的业务总计
          HSL05 "按本位币的期间中的业务总计
          HSL06 "按本位币的期间中的业务总计
          HSL07 "按本位币的期间中的业务总计
          HSL08 "按本位币的期间中的业务总计
          HSL09 "按本位币的期间中的业务总计
          HSL10 "按本位币的期间中的业务总计
          HSL11 "按本位币的期间中的业务总计
          HSL12 "按本位币的期间中的业务总计
          HSL13 "按本位币的期间中的业务总计
          HSL14 "按本位币的期间中的业务总计
          HSL15 "按本位币的期间中的业务总计
          HSL16 "按本位币的期间中的业务总计
    FROM FAGLFLEXT
    INTO CORRESPONDING FIELDS OF TABLE T_FAGLFLEXT
    WHERE RYEAR IN (P_GJAHR, P_GJAHR2)
*    AND DRCRK IN R_DRCRK
    AND RACCT IN R_HKONT2
    AND RBUKRS IN S_BUKRS.
*  LOOP AT T_FAGLFLEXT.
*    CLEAR: T_FAGLFLEXT-DRCRK.
*    MODIFY T_FAGLFLEXT.
*  ENDLOOP.
  SORT T_FAGLFLEXT BY RYEAR.
  CLEAR T_FAGLFLEXT_SUM[].
  LOOP AT T_FAGLFLEXT.
    AT END OF RYEAR.
      SUM.
      APPEND T_FAGLFLEXT TO T_FAGLFLEXT_SUM.
    ENDAT.
  ENDLOOP.
  READ TABLE T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    READ TABLE S_MONAT INDEX 1.
*    ADD T_FAGLFLEXT-HSLVT TO T_ALV-AMOUNT.
    PERFORM GET_YE USING S_MONAT-LOW CHANGING T_ALV-AMOUNT.
  ENDIF.
  READ TABLE T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR2 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    READ TABLE S_MONAT2 INDEX 1.
*    ADD T_FAGLFLEXT-HSLVT TO T_ALV-AMOUNT_LAST.
    PERFORM GET_YE USING S_MONAT2-LOW CHANGING T_ALV-AMOUNT_LAST.
  ENDIF.
ENDFORM.
FORM FRM_AMOUNT28.
**
  CLEAR: T_FAGLFLEXT[],T_FAGLFLEXT2[],T_FAGLFLEXT_SUM[],T_FAGLFLEXT_SUM2[].
*  SELECT  RYEAR "会计年度
*          HSLVT "用本币计算的结转余额
*          HSL01 "按本位币的期间中的业务总计
*          HSL02 "按本位币的期间中的业务总计
*          HSL03 "按本位币的期间中的业务总计
*          HSL04 "按本位币的期间中的业务总计
*          HSL05 "按本位币的期间中的业务总计
*          HSL06 "按本位币的期间中的业务总计
*          HSL07 "按本位币的期间中的业务总计
*          HSL08 "按本位币的期间中的业务总计
*          HSL09 "按本位币的期间中的业务总计
*          HSL10 "按本位币的期间中的业务总计
*          HSL11 "按本位币的期间中的业务总计
*          HSL12 "按本位币的期间中的业务总计
*          HSL13 "按本位币的期间中的业务总计
*          HSL14 "按本位币的期间中的业务总计
*          HSL15 "按本位币的期间中的业务总计
*          HSL16 "按本位币的期间中的业务总计
*    FROM FAGLFLEXT
*    INTO CORRESPONDING FIELDS OF TABLE T_FAGLFLEXT
*    WHERE RYEAR IN (P_GJAHR,P_GJAHR2)
**    AND DRCRK IN R_DRCRK
**    AND RACCT IN ('2201010000', '2201020000')
*    AND RACCT IN ('2201010000', '2201020000')
*    AND RBUKRS IN S_BUKRS.
*  SORT T_FAGLFLEXT BY RYEAR.
*  CLEAR T_FAGLFLEXT_SUM[].
*  LOOP AT T_FAGLFLEXT.
*    AT END OF RYEAR.
*      SUM.
*      APPEND T_FAGLFLEXT TO T_FAGLFLEXT_SUM.
*    ENDAT.
*  ENDLOOP.

    SELECT  RYEAR "会计年度
          HSLVT "用本币计算的结转余额
          HSL01 "按本位币的期间中的业务总计
          HSL02 "按本位币的期间中的业务总计
          HSL03 "按本位币的期间中的业务总计
          HSL04 "按本位币的期间中的业务总计
          HSL05 "按本位币的期间中的业务总计
          HSL06 "按本位币的期间中的业务总计
          HSL07 "按本位币的期间中的业务总计
          HSL08 "按本位币的期间中的业务总计
          HSL09 "按本位币的期间中的业务总计
          HSL10 "按本位币的期间中的业务总计
          HSL11 "按本位币的期间中的业务总计
          HSL12 "按本位币的期间中的业务总计
          HSL13 "按本位币的期间中的业务总计
          HSL14 "按本位币的期间中的业务总计
          HSL15 "按本位币的期间中的业务总计
          HSL16 "按本位币的期间中的业务总计
    FROM FAGLFLEXT
    INTO CORRESPONDING FIELDS OF TABLE T_FAGLFLEXT2
    WHERE RYEAR IN (P_GJAHR,P_GJAHR2)
*    AND DRCRK IN R_DRCRK
    AND RACCT LIKE '1012%'
    AND RBUKRS IN S_BUKRS.

  SORT T_FAGLFLEXT2 BY RYEAR.
  CLEAR T_FAGLFLEXT_SUM2[].
  LOOP AT T_FAGLFLEXT2.
    AT END OF RYEAR.
      SUM.
      APPEND T_FAGLFLEXT2 TO T_FAGLFLEXT_SUM2.
    ENDAT.
  ENDLOOP.
  DATA L_AMOUNT LIKE T_ALV-AMOUNT.
*  READ TABLE T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR BINARY SEARCH.
*  IF SY-SUBRC EQ 0.
*    READ TABLE S_MONAT INDEX 1.
*    CLEAR L_AMOUNT.
*    PERFORM GET_CURRENT USING S_MONAT-LOW S_MONAT-HIGH CHANGING L_AMOUNT.
*    SUBTRACT L_AMOUNT FROM T_ALV-AMOUNT.
*  ENDIF.
*  READ TABLE T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR2 BINARY SEARCH.
*  IF SY-SUBRC EQ 0.
*    READ TABLE S_MONAT2 INDEX 1.
*    CLEAR L_AMOUNT.
*    PERFORM GET_CURRENT USING S_MONAT2-LOW  S_MONAT2-HIGH CHANGING L_AMOUNT.
*    SUBTRACT L_AMOUNT FROM T_ALV-AMOUNT_LAST.
*  ENDIF.
  READ TABLE T_FAGLFLEXT_SUM2 INTO T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    READ TABLE S_MONAT INDEX 1.
    CLEAR L_AMOUNT.
    PERFORM GET_CURRENT USING S_MONAT-LOW S_MONAT-HIGH CHANGING L_AMOUNT.
    SUBTRACT L_AMOUNT FROM T_ALV-AMOUNT.
  ENDIF.
  READ TABLE T_FAGLFLEXT_SUM2 INTO T_FAGLFLEXT_SUM WITH KEY RYEAR = P_GJAHR2 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    READ TABLE S_MONAT2 INDEX 1.
    CLEAR L_AMOUNT.
    PERFORM GET_CURRENT USING S_MONAT2-LOW S_MONAT2-HIGH CHANGING L_AMOUNT.
    SUBTRACT L_AMOUNT FROM T_ALV-AMOUNT_LAST.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AMOUNT_INCREASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_AMOUNT  text
*----------------------------------------------------------------------*
FORM AMOUNT_INCREASE  CHANGING P_AMOUNT.
  ADD 1 TO P_AMOUNT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT         = P_AMOUNT
   IMPORTING
      OUTPUT        = P_AMOUNT
            .
ENDFORM.                    " AMOUNT_INCREASE
*&---------------------------------------------------------------------*
*&      Form  GET_YE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MONAT  text
*      <--P_AMOUNT  text
*----------------------------------------------------------------------*
FORM GET_YE  USING    P_MONAT
             CHANGING P_AMOUNT.
  DATA G_MONTH_INDEX TYPE CHAR2.
  DATA L_NAME TYPE STRING.
  FIELD-SYMBOLS <FS_HSL_DB> TYPE FAGLFLEXT-HSL01.
  G_MONTH_INDEX = '01'.
  ADD T_FAGLFLEXT_SUM-HSLVT TO P_AMOUNT.
  WHILE G_MONTH_INDEX LT P_MONAT.
    CONCATENATE 'T_FAGLFLEXT_SUM-HSL' G_MONTH_INDEX INTO L_NAME.
    ASSIGN (L_NAME) TO <FS_HSL_DB>.
    ADD <FS_HSL_DB> TO P_AMOUNT.
    PERFORM AMOUNT_INCREASE CHANGING G_MONTH_INDEX.
    IF G_MONTH_INDEX > '12'.
      G_MONTH_INDEX = '01'.
      EXIT.
    ENDIF.
  ENDWHILE.
ENDFORM.                    " GET_YE
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MONAT  text
*      <--P_AMOUNT  text
*----------------------------------------------------------------------*
FORM GET_CURRENT  USING    P_MONAT P_MONAT2
             CHANGING P_AMOUNT.
  DATA G_MONTH_INDEX TYPE CHAR2.
  DATA L_NAME TYPE STRING.
  FIELD-SYMBOLS <FS_HSL_DB> TYPE FAGLFLEXT-HSL01.
  IF P_MONAT2 IS INITIAL.
    P_MONAT2 = P_MONAT.
  ENDIF.
  G_MONTH_INDEX = P_MONAT - 1.
  PERFORM AMOUNT_INCREASE CHANGING G_MONTH_INDEX.
*  BREAK HANDTB.
*  ADD T_FAGLFLEXT-HSLVT TO P_AMOUNT.
  WHILE G_MONTH_INDEX LE P_MONAT2.
   CONCATENATE 'T_FAGLFLEXT_SUM-HSL' G_MONTH_INDEX INTO L_NAME.
   ASSIGN (L_NAME) TO <FS_HSL_DB>.
   ADD <FS_HSL_DB> TO P_AMOUNT.
    PERFORM AMOUNT_INCREASE CHANGING G_MONTH_INDEX.
    IF G_MONTH_INDEX > '12'.
      G_MONTH_INDEX = '01'.
      EXIT.
    ENDIF.
  ENDWHILE.
ENDFORM.                    " GET_CURRENT
FORM user_command  USING r_ucomm LIKE sy-ucomm
                                   rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN 'PRINT'."打印
*      PERFORM PRINT_PDF.
    WHEN '&IC1'."双击
      READ TABLE t_alv  INDEX rs_selfield-tabindex.
      IF T_ALV-BELNRS[] IS NOT INITIAL
        OR T_ALV-BELNRS_LAST[] IS NOT INITIAL ."如果包含凭证号
          RANGES R_BELNR FOR BSEG-BELNR.
          RANGES R_GJAHR FOR BSEG-GJAHR.
          RANGES R_BUKRS FOR BSEG-BUKRS.
          RANGES R_HKONT2 FOR BSEG-HKONT.
          CLEAR: R_BELNR[],R_GJAHR[],R_BUKRS[],R_HKONT2[].
          R_BELNR-SIGN = 'I'.
          R_BELNR-OPTION = 'EQ'.
          R_BUKRS-SIGN = 'I'.
          R_BUKRS-OPTION = 'EQ'.
          IF RS_SELFIELD-FIELDNAME NE 'AMOUNT_LAST'.
            LOOP AT T_ALV-BELNRS INTO T_BSEG.
              R_BELNR-LOW = T_BSEG-BELNR.
              R_BUKRS-LOW = T_BSEG-BUKRS.
              APPEND R_BELNR.
              APPEND R_BUKRS.
            ENDLOOP.
          ELSE.
            LOOP AT T_ALV-BELNRS_LAST INTO T_BSEG.
              R_BELNR-LOW = T_BSEG-BELNR.
              R_BUKRS-LOW = T_BSEG-BUKRS.
              APPEND R_BELNR.
              APPEND R_BUKRS.
            ENDLOOP.
          ENDIF.
          R_GJAHR-SIGN = 'I'.
          R_GJAHR-OPTION = 'EQ'.
          R_GJAHR-LOW = P_GJAHR.
          APPEND R_GJAHR.
*          R_BUKRS-SIGN = 'I'.
*          R_BUKRS-OPTION = 'EQ'.
*          R_BUKRS-LOW IN S_BUKRS.
*          APPEND R_BUKRS.
*          R_HKONT2-SIGN = 'I'.
*          R_HKONT2-OPTION = 'EQ'.
*          R_HKONT2-LOW = T_ALV-HKONT.
*          APPEND R_HKONT2.
*          IF R_BELNR[] IS NOT INITIAL.
*            APPEND R_BUKRS.
          CLEAR T_ZFI003_BSEG[].
          IF RS_SELFIELD-FIELDNAME NE 'AMOUNT_LAST'.
            T_ZFI003_BSEG[] = T_ALV-BELNRS[].
          ELSE.
            T_ZFI003_BSEG[] = T_ALV-BELNRS_LAST[].
          ENDIF.
          IF T_ZFI003_BSEG[] IS INITIAL.
            MESSAGE '该期间没有可以穿透的凭证。' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
          EXPORT T_ZFI003_BSEG TO MEMORY ID 'ZFI003_T_ALV_BELNRS'.
          DATA L_FIEDLCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
*             I_PROGRAM_NAME               =
*             I_INTERNAL_TABNAME           =
              I_STRUCTURE_NAME             = 'BSEG'
              I_CLIENT_NEVER_DISPLAY       = 'X'
*             I_INCLNAME                   =
*             I_BYPASSING_BUFFER           =
*             I_BUFFER_ACTIVE              =
            CHANGING
              CT_FIELDCAT                  = L_FIEDLCAT[]
            EXCEPTIONS
              INCONSISTENT_INTERFACE       = 1
              PROGRAM_ERROR                = 2
              OTHERS                       = 3
                    .
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
*             I_INTERFACE_CHECK                 = ' '
*             I_BYPASSING_BUFFER                = ' '
*             I_BUFFER_ACTIVE                   = ' '
              I_CALLBACK_PROGRAM                = SY-REPID
*             I_CALLBACK_PF_STATUS_SET          = ' '
              I_CALLBACK_USER_COMMAND           = 'USER_COMMAND2'
*             I_CALLBACK_TOP_OF_PAGE            = ' '
*             I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*             I_CALLBACK_HTML_END_OF_LIST       = ' '
*             I_STRUCTURE_NAME                  =
*             I_BACKGROUND_ID                   = ' '
*             I_GRID_TITLE                      =
*             I_GRID_SETTINGS                   =
*             IS_LAYOUT                         =
              IT_FIELDCAT                       = L_FIEDLCAT[]
*             IT_EXCLUDING                      =
*             IT_SPECIAL_GROUPS                 =
*             IT_SORT                           =
*             IT_FILTER                         =
*             IS_SEL_HIDE                       =
*             I_DEFAULT                         = 'X'
              I_SAVE                            = 'A'
*             IS_VARIANT                        =
*             IT_EVENTS                         =
*             IT_EVENT_EXIT                     =
*             IS_PRINT                          =
*             IS_REPREP_ID                      =
*             I_SCREEN_START_COLUMN             = 0
*             I_SCREEN_START_LINE               = 0
*             I_SCREEN_END_COLUMN               = 0
*             I_SCREEN_END_LINE                 = 0
*             I_HTML_HEIGHT_TOP                 = 0
*             I_HTML_HEIGHT_END                 = 0
*             IT_ALV_GRAPHICS                   =
*             IT_HYPERLINK                      =
*             IT_ADD_FIELDCAT                   =
*             IT_EXCEPT_QINFO                   =
*             IR_SALV_FULLSCREEN_ADAPTER        =
*           IMPORTING
*             E_EXIT_CAUSED_BY_CALLER           =
*             ES_EXIT_CAUSED_BY_USER            =
            TABLES
              T_OUTTAB                          = T_ZFI003_BSEG[]
            EXCEPTIONS
              PROGRAM_ERROR                     = 1
              OTHERS                            = 2
                    .
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.

*          SUBMIT ZFI014
*          WITH S_GJAHR IN R_GJAHR
*          WITH S_BUKRS IN R_BUKRS
*          WITH S_BELNR IN R_BELNR
*          AND RETURN.
      ELSE.
         MESSAGE '该期间没有可以穿透的凭证。' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.
FORM USER_COMMAND2  USING R_UCOMM LIKE SY-UCOMM
                                   RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE r_ucomm.
    WHEN 'PRINT'."打印
*      PERFORM PRINT_PDF.
    WHEN '&IC1'."双击
      READ TABLE T_ZFI003_BSEG  INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0.
        SET PARAMETER ID 'BLN' FIELD T_ZFI003_BSEG-BELNR .
        SET PARAMETER ID 'BUK' FIELD T_ZFI003_BSEG-BUKRS.
        SET PARAMETER ID 'GJR' FIELD T_ZFI003_BSEG-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_PDF .
  DATA: ls_docparams      TYPE sfpdocparams,     "Form Parameters for Form Processing
        gs_outpar        TYPE sfpoutputparams,  "Form Processing Output Parameter
        LV_FUNCTION_NAME  TYPE FUNCNAME,         "NAME OF THE GENERATED FM BELONGS TO PDF
        LV_INTERFACE_TYPE TYPE FPINTERFACETYPE,  "PDF INTERFACE TYPE
        ls_content_f      TYPE idcn_s_cf_content_f,
        lv_formoutput     TYPE fpformoutput,     "Form Output (PDF, PDL)
        lv_header_name TYPE string,
        lw_item like line of LS_CONTENT_F-ITEM.
  CLEAR: LS_CONTENT_F-HEADER, LS_CONTENT_F-ITEM[].
  LS_CONTENT_F-HEADER-BUKRS = S_BUKRS.
  DATA L_COUNT TYPE I.
  CLEAR L_COUNT.
  SELECT COUNT( * ) FROM T001 INTO L_COUNT WHERE BUKRS IN S_BUKRS.
  IF L_COUNT > 1.
    LS_CONTENT_F-HEADER-BUTXT = '*'.
  ELSE.
    SELECT SINGLE BUTXT FROM T001 INTO LS_CONTENT_F-HEADER-BUTXT WHERE BUKRS IN S_BUKRS.
  ENDIF.
  LS_CONTENT_F-HEADER-REPDATE = SY-DATUM.
  LS_CONTENT_F-HEADER-WAERS = '元'.
  LS_CONTENT_F-HEADER-GJAHR = P_GJAHR.
  READ TABLE S_MONAT INDEX 1.
  LS_CONTENT_F-HEADER-PER_FR = S_MONAT-LOW.
  IF S_MONAT-HIGH IS NOT INITIAL.
    LS_CONTENT_F-HEADER-PER_TO = S_MONAT-HIGH.
  ELSE.
    LS_CONTENT_F-HEADER-PER_TO = S_MONAT-LOW.
  ENDIF.

  LOOP AT T_ALV.
    lw_item-LINENUMBER = T_ALV-LINE_INDEX.
    lw_item-ITEMTEXT = T_ALV-ITEM.
*    SHIFT lw_item-ITEMTEXT BY -1 PLACES.
    lw_item-AMOUNT = T_ALV-AMOUNT.
    lw_item-AMOUNT_PREV = T_ALV-AMOUNT_LAST.
    lw_item-WAERS = 'CNY'.
    APPEND lw_item to LS_CONTENT_F-ITEM.
  ENDLOOP.

  TRY.
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name           = 'IDCN_CASHFLOW_SINGLE'
        IMPORTING
          e_funcname       = lv_function_name
          e_interface_type = lv_interface_type.
    CATCH cx_fp_api_repository.
      "Exception API (Repository)
      MESSAGE e020(j3rf_cashflow) WITH 'IDCN_CASHFLOW_SINGLE'.
    CATCH cx_fp_api_usage.
      "Exception API (Use)
      MESSAGE e020(j3rf_cashflow) WITH 'IDCN_CASHFLOW_SINGLE'.
    CATCH cx_fp_api_internal.
      "Exception API (Internal).
      MESSAGE e020(j3rf_cashflow) WITH 'IDCN_CASHFLOW_SINGLE'.
  ENDTRY.
  lv_header_name = '上期'.
  gs_outpar-device = 'PRINTER'.
  CLEAR gs_outpar-reqimm.
  gs_outpar-pdftagged = 'X'.
  gs_outpar-REQDEL = 'X'.
  gs_outpar-dest   = 'LP02'.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = gs_outpar "
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
  ENDIF.
*   calling the generated function module
  CALL FUNCTION lv_function_name
    EXPORTING
      /1bcdwb/docparams  = ls_docparams
      header             = ls_content_f-header
      item               = ls_content_f-item
      iv_period_name     = lv_header_name
    IMPORTING
      /1bcdwb/formoutput = lv_formoutput
    EXCEPTIONS       "OTHERS             = 1.
      error_message      = 1.

  IF sy-subrc <> 0.
  ENDIF.
* end form processing
  CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    MESSAGE e023(j3rf_cashflow).
  ENDIF.
ENDFORM.                    " PRINT_PDF
