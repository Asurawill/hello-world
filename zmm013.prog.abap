*&---------------------------------------------------------------------*
*& Report  ZMM013
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2015/2/13                                            *
*& Created By   : 汉得-唐博                                            *
*& Description  :库存周转率                                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZMM013.

TABLES: MSEG,EKKO,EKPO,MARA,MAKT.

* Globale Datendeklarationen
include rmcs0tp1.
* Datendeklarationen für alle Analysen
include rmcb01tp.

"公司代码
PARAMETERS P_BUKRS TYPE MSEG-BUKRS OBLIGATORY DEFAULT '1000'.
"工厂
PARAMETERS P_WERKS TYPE EKPO-WERKS OBLIGATORY DEFAULT '1000'.
*"库存地点
*SELECT-OPTIONS S_LGORT FOR EKPO-LGORT.
"物料编码
SELECT-OPTIONS S_MATNR FOR EKPO-MATNR.
"物料组
SELECT-OPTIONS S_MATKL FOR MARA-MATKL.
"物料类型
SELECT-OPTIONS S_MTART FOR MARA-MTART.
"日期
SELECT-OPTIONS S_DATS FOR SY-DATUM NO-EXTENSION.
"月份
SELECT-OPTIONS S_MONS FOR S031-SPMON NO-EXTENSION.
"单选（数量）
PARAMETERS: P_SL TYPE C RADIOBUTTON GROUP G1 USER-COMMAND UC1 DEFAULT 'X'.
"单选（金额）
PARAMETERS: P_JE TYPE C RADIOBUTTON GROUP G1.

"定义GT_ALV显示结果的内表
DATA: BEGIN OF GT_ALV OCCURS 1,
  CHECKBOX TYPE C,
  DATE_S TYPE DATS, "起始日期
  DATE_E TYPE DATS, "结束日期
  BUKRS TYPE MSEG-BUKRS, "公司代码
  BUTXT TYPE T001-BUTXT, "公司名称
  WERKS TYPE EKPO-WERKS, "工厂
  NAME1 TYPE T001W-NAME1, "工厂名称
  LGORT TYPE EKPO-LGORT, "库存地点
  LGOBE TYPE T001L-LGOBE, "库存地点描述
  MATNR TYPE EKPO-MATNR, "物料编码
  MAKTX TYPE MAKT-MAKTX, "物料描述
  MATKL TYPE MARA-MATKL, "物料组
  MTART TYPE MARA-MTART, "物料类型
  RATIO TYPE P DECIMALS 3, "周转率
  ZXHL TYPE P DECIMALS 3, "总消耗量
  AVG TYPE P DECIMALS 3, "平均库存
  RATIO_C TYPE P DECIMALS 2, "周转率
  ZXHL_C TYPE P DECIMALS 2, "总消耗量
  AVG_C TYPE P DECIMALS 2, "平均库存
  DAYS TYPE P DECIMALS 4, "周转天数
END OF GT_ALV.

DATA LIST_TAB TYPE TABLE OF ABAPLIST.
DATA LIST_ASCI TYPE TABLE OF CHAR200 WITH HEADER LINE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM UPDATE_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONS-LOW.
  PERFORM MONAT_F4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONS-HIGH.
  PERFORM MONAT_F4.

START-OF-SELECTION.
IF P_SL EQ 'X'."数量
  IF S_DATS-HIGH IS INITIAL.
    IF S_DATS-LOW IS INITIAL.
      MESSAGE S002(ZMM01) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
*   请输入日期！
    ELSE.
      S_DATS-HIGH = S_DATS-LOW.
      MODIFY S_DATS INDEX 1.
    ENDIF.
  ENDIF.
  IF S_DATS-HIGH > SY-DATUM.
    S_DATS-HIGH = SY-DATUM.
    MODIFY S_DATS INDEX 1.
  ENDIF.
  IF S_DATS-LOW > SY-DATUM.
    S_DATS-LOW = SY-DATUM.
    MODIFY S_DATS INDEX 1.
  ENDIF.
ELSE."金额
  IF S_MONS-HIGH IS INITIAL.
    IF S_MONS-LOW IS INITIAL.
      MESSAGE S002(ZMM01) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
*   请输入日期！
    ELSE.
      S_MONS-HIGH = S_MONS-LOW.
    ENDIF.
  ENDIF.
ENDIF.
DATA WERTE  TYPE TABLE OF  BCO_WERTE WITH HEADER LINE.
DATA L_ZTCODE TYPE CHAR20 VALUE 'ZMM013'.
EXPORT L_ZTCODE TO MEMORY ID 'L_ZTCODE'.
IF P_SL EQ 'X'."（数量）
"调用MC44 BDC
  RANGES S_WERKS FOR T001W-WERKS.
  CLEAR S_WERKS[].
  S_WERKS-LOW = P_WERKS.
  APPEND S_WERKS.
  SUBMIT RMCBUH30 WITH WERKE IN S_WERKS
  WITH VONDATUM = S_DATS-LOW
  WITH BISDATUM = S_DATS-HIGH
  WITH MATERIAL IN S_MATNR
  WITH MATKL IN S_MATKL
  WITH MTART IN S_MTART
  EXPORTING LIST TO MEMORY
              AND RETURN.
  CLEAR WERTE[].
  IMPORT WERTE[] FROM MEMORY ID 'ZMC44_WERTE'.
  FREE MEMORY ID 'ZMC44_WERTE'.

"SET PARAMETER ID.
"CALL TRANSACTION 'MC44' AND SKIP FIRST SCREEN MODE 'N'.
"抽取MC44数据
"IMPORT GT_MC44[] FROM MEMORY ID 'ZMM013_MC44'.
ELSE."（金额）
"调用MC.B BDC
*  SUBMIT RMCB0300 EXPORTING LIST TO MEMORY
*              AND RETURN.
  SUBMIT RMCB0300 WITH SL_WERKS-LOW = P_WERKS
  WITH SL_SPMON IN S_MONS
  WITH SL_MATNR IN S_MATNR
  WITH SL_MATKL IN S_MATKL
  WITH SL_MTART IN S_MTART
  EXPORTING LIST TO MEMORY
            AND RETURN.
  CLEAR INT_S000[].
  IMPORT INT_S000[] FROM MEMORY ID 'ZMCB_NT_S000'.
  FREE MEMORY ID 'ZMCB_NT_S000'.
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      LISTOBJECT       = LIST_TAB
    EXCEPTIONS
      NOT_FOUND        = 1
      OTHERS           = 2
            .

  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  CALL FUNCTION 'LIST_TO_ASCI'
*    EXPORTING
*     LIST_INDEX               = -1
*      WITH_LINE_BREAK          = ' '
*   IMPORTING
*     LIST_STRING_ASCII        =
*     LIST_DYN_ASCII           =
    TABLES
      LISTASCI                 = LIST_ASCI
      LISTOBJECT               = LIST_TAB
    EXCEPTIONS
      EMPTY_LIST               = 1
      LIST_INDEX_INVALID       = 2
      OTHERS                   = 3
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

"SET PARAMETER ID.
"CALL TRANSACTION 'MC.B' AND SKIP FIRST SCREEN MODE 'N'.
"抽取MC.B数据
"IMPORT GT_MCB[] FROM MEMORY ID 'ZMM013_MC.B'.
ENDIF.
"整理数据
PERFORM PROCESS_DATA.
"显示ALV
"

  DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  CLEAR: IS_LAYOUT,IT_FIELDCAT[].
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  IS_LAYOUT-BOX_FIELDNAME = 'CHECKBOX'.

  PERFORM APPEND_FIELDCAT USING:
    'DATE_S' '起始日期' '' '' '',
    'DATE_E' '结束日期' '' '' '',
    'BUKRS' '公司代码' '' '' '',
    'BUTXT' '公司名称' '' '' '',
    'WERKS' '工厂' '' '' '',
    'NAME1' '工厂名称' '' '' '',
*    'LGORT' '库存地点' '' '' '',
*    'LGOBE' '库存地点描述 ' '' '' '',
    'MATNR' '物料编码' 'MARA' 'MATNR' '',
    'MAKTX' '物料描述' '' '' '',
    'MATKL' '物料组' '' '' '',
    'MTART' '物料类型' '' '' ''.
  IF P_SL EQ 'X'."（数量）
    PERFORM APPEND_FIELDCAT USING:
    'RATIO' '周转率' '' '' '',
    'ZXHL' '总消耗量' '' '' '',
    'AVG' '平均库存' '' '' ''.
  ELSE.
    PERFORM APPEND_FIELDCAT USING:
    'RATIO_C' '周转率' '' '' '',
    'ZXHL_C' '总消耗量' '' '' '',
    'AVG_C' '平均库存估价' '' '' ''.
  ENDIF.
  PERFORM APPEND_FIELDCAT USING:
    'DAYS' '周转天数' '' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*      I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS_SET'
*      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = IS_LAYOUT
      IT_FIELDCAT                       = IT_FIELDCAT[]
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
      T_OUTTAB                          = GT_ALV[]
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
"整理数据
FORM PROCESS_DATA.
DATA SUBSTR1 TYPE CHAR50.
DATA SUBSTR2 TYPE CHAR50.
DATA SUBSTR3 TYPE CHAR50.
DATA SUBSTR4 TYPE CHAR50.
DATA SUBSTR5 TYPE CHAR50.
DATA SUBSTR6 TYPE CHAR50.
DATA SUBSTR7 TYPE CHAR50.
DATA SUBSTR8 TYPE CHAR50.
DATA BUTXT TYPE T001-BUTXT.
DATA NAME1 TYPE T001W-NAME1.
SELECT SINGLE BUTXT FROM T001 INTO BUTXT WHERE BUKRS EQ P_BUKRS.
SELECT SINGLE NAME1 FROM T001W INTO NAME1 WHERE WERKS EQ P_WERKS.
CLEAR GT_ALV[].
IF P_SL EQ 'X'."（数量）
  LOOP AT WERTE.
     GT_ALV-DATE_S = S_DATS-LOW."起始日期
     GT_ALV-DATE_E = S_DATS-HIGH."结束日期
     GT_ALV-BUKRS = P_BUKRS."公司代码
     GT_ALV-BUTXT = BUTXT."公司名称
     GT_ALV-WERKS = P_WERKS."工厂
     GT_ALV-NAME1 = NAME1."工厂名称
     GT_ALV-MATNR = WERTE-MATNR."物料编码
     SELECT SINGLE MAKTX FROM MAKT INTO GT_ALV-MAKTX WHERE MATNR EQ GT_ALV-MATNR AND SPRAS EQ SY-LANGU."物料描述
     SELECT SINGLE MATKL MTART FROM MARA INTO (GT_ALV-MATKL, GT_ALV-MTART) WHERE MATNR EQ GT_ALV-MATNR."物料组
     GT_ALV-RATIO = WERTE-K_MENGE."周转率 MVER消耗量/平均库存
     "总消耗量
     GT_ALV-ZXHL = WERTE-K_K_MENGE.
     "平均库存
     GT_ALV-AVG = WERTE-MIT_MENGE.
     GT_ALV-DAYS = GT_ALV-DATE_E - GT_ALV-DATE_S + 1."周转天数
      IF GT_ALV-RATIO IS NOT INITIAL.
        GT_ALV-DAYS = GT_ALV-DAYS / GT_ALV-RATIO.
      ELSE.
        CLEAR GT_ALV-DAYS.
      ENDIF.
     APPEND GT_ALV.
  ENDLOOP.
  "添加尾部汇总行
  DATA L_ZXHL LIKE GT_ALV-ZXHL.
  DATA L_AVG LIKE GT_ALV-AVG.
  CLEAR: L_ZXHL, L_AVG.
  LOOP AT GT_ALV.
    ADD GT_ALV-ZXHL TO L_ZXHL.
    ADD GT_ALV-AVG TO L_AVG.
  ENDLOOP.
  CLEAR: GT_ALV-MATNR, GT_ALV-MAKTX, GT_ALV-MATKL,GT_ALV-MTART,GT_ALV-RATIO,GT_ALV-ZXHL,GT_ALV-AVG,GT_ALV-RATIO_C,GT_ALV-ZXHL_C,GT_ALV-AVG_C,GT_ALV-DAYS.
  GT_ALV-MAKTX = '汇总：'.
  GT_ALV-ZXHL = L_ZXHL.
  GT_ALV-AVG = L_AVG.
  IF GT_ALV-AVG IS NOT INITIAL.
    GT_ALV-RATIO = GT_ALV-ZXHL / GT_ALV-AVG.
  ELSE.
    GT_ALV-RATIO = 99999.
  ENDIF.
  GT_ALV-DAYS = GT_ALV-DATE_E - GT_ALV-DATE_S + 1."周转天数
  IF GT_ALV-RATIO IS NOT INITIAL.
    GT_ALV-DAYS = GT_ALV-DAYS / GT_ALV-RATIO.
  ELSE.
    CLEAR GT_ALV-DAYS.
  ENDIF.
  APPEND GT_ALV.
ELSE."（金额）
  DATA L_DCPFM LIKE USR01-DCPFM.
  SELECT SINGLE DCPFM INTO L_DCPFM
      FROM USR01
      WHERE BNAME = SY-UNAME.
  LOOP AT LIST_ASCI FROM 6.
    SPLIT LIST_ASCI AT '|' INTO SUBSTR1 SUBSTR2 SUBSTR3 SUBSTR4 SUBSTR5 SUBSTR6.
    CONDENSE: SUBSTR2,SUBSTR3,SUBSTR4,SUBSTR5.
    CHECK SUBSTR2 IS NOT INITIAL.
    TRY .
      CALL FUNCTION 'AMOUNT_STRING_CONVERT'
        EXPORTING
          AMOUNT_STRING       = SUBSTR3
          DCPFM               = L_DCPFM
*         MLLN                = 'M'
*         TSND                = 'T'
          WAERS               = 'CNY'
          NEG_VALUE           = 'X'
        IMPORTING
          AMOUNT              = GT_ALV-RATIO_C
        EXCEPTIONS
          INVALID_TYPE        = 1
          OTHERS              = 2
                .

*      CALL FUNCTION 'UNITS_STRING_CONVERT'
*        EXPORTING
*          UNITS_STRING       = SUBSTR3
*          DCPFM              = L_DCPFM
**         MLLN               = 'M'
**         TSND               = 'T'
*        IMPORTING
*          UNITS              = GT_ALV-RATIO
*        EXCEPTIONS
*          INVALID_TYPE       = 1
*          OTHERS             = 2
*                .
*      GT_ALV-RATIO = SUBSTR3."周转率  S031总消耗量/平均库存估价
     "总消耗量
      CLEAR: SUBSTR7, SUBSTR8.
      SPLIT SUBSTR5 AT SPACE INTO SUBSTR7 SUBSTR8.
      CALL FUNCTION 'AMOUNT_STRING_CONVERT'
        EXPORTING
          AMOUNT_STRING       = SUBSTR7
          DCPFM               = L_DCPFM
*         MLLN                = 'M'
*         TSND                = 'T'
          WAERS               = 'CNY'
          NEG_VALUE           = 'X'
        IMPORTING
          AMOUNT              = GT_ALV-ZXHL_C
        EXCEPTIONS
          INVALID_TYPE        = 1
          OTHERS              = 2
                .
*      CALL FUNCTION 'UNITS_STRING_CONVERT'
*        EXPORTING
*          UNITS_STRING       = SUBSTR7
*          DCPFM              = L_DCPFM
**         MLLN               = 'M'
**         TSND               = 'T'
*        IMPORTING
*          UNITS              = GT_ALV-ZXHL
*        EXCEPTIONS
*          INVALID_TYPE       = 1
*          OTHERS             = 2
*                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
*      REPLACE ALL OCCURRENCES OF ',' IN SUBSTR7 WITH SPACE.
*      GT_ALV-ZXHL = SUBSTR7.
     "平均库存估价
      CLEAR: SUBSTR7, SUBSTR8.
      SPLIT SUBSTR4 AT SPACE INTO SUBSTR7 SUBSTR8.
      CALL FUNCTION 'AMOUNT_STRING_CONVERT'
        EXPORTING
          AMOUNT_STRING       = SUBSTR7
          DCPFM               = L_DCPFM
*         MLLN                = 'M'
*         TSND                = 'T'
          WAERS               = 'CNY'
          NEG_VALUE           = 'X'
        IMPORTING
          AMOUNT              = GT_ALV-AVG_C
        EXCEPTIONS
          INVALID_TYPE        = 1
          OTHERS              = 2
                .
*      CALL FUNCTION 'UNITS_STRING_CONVERT'
*        EXPORTING
*          UNITS_STRING       = SUBSTR7
*          DCPFM              = L_DCPFM
**         MLLN               = 'M'
**         TSND               = 'T'
*        IMPORTING
*          UNITS              = GT_ALV-AVG
*        EXCEPTIONS
*          INVALID_TYPE       = 1
*          OTHERS             = 2
*                .
*      REPLACE ALL OCCURRENCES OF ',' IN SUBSTR7 WITH SPACE.
*      GT_ALV-AVG = SUBSTR7.
    CATCH CX_SY_CONVERSION_NO_NUMBER.
      CONTINUE.
    ENDTRY.
     GT_ALV-DATE_S = S_MONS-LOW."起始日期
     GT_ALV-DATE_S+6(2) = '01'.
     GT_ALV-DATE_E = S_MONS-HIGH."结束日期
     GT_ALV-DATE_E+6(2) = '01'.
     CALL FUNCTION '/DSD/PR_LAST_DAY_OF_MONTHS'
       EXPORTING
         DAY_IN                  = GT_ALV-DATE_E
       IMPORTING
         LAST_DAY_OF_MONTH       = GT_ALV-DATE_E
       EXCEPTIONS
         DAY_IN_NO_DATE          = 1
         OTHERS                  = 2
               .
     IF SY-SUBRC <> 0.
* Implement suitable error handling here
     ENDIF.

     GT_ALV-BUKRS = P_BUKRS."公司代码
     GT_ALV-BUTXT = BUTXT."公司名称
     GT_ALV-WERKS = P_WERKS."工厂
     GT_ALV-NAME1 = NAME1."工厂名称
*    GT_ALV-LGORT = INT_S000-LGORT."库存地点
*    GT_ALV-LGOBE = INT_S000-LGOBE."库存地点描述
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = SUBSTR2
      IMPORTING
        OUTPUT        = GT_ALV-MATNR
              .
    SELECT SINGLE MAKTX FROM MAKT INTO GT_ALV-MAKTX WHERE MATNR EQ GT_ALV-MATNR AND SPRAS EQ SY-LANGU."物料描述
*    SELECT SINGLE MATKL FROM MARA INTO GT_ALV-MATKL WHERE MATNR EQ GT_ALV-MATNR."物料组
    SELECT SINGLE MATKL MTART FROM MARA INTO (GT_ALV-MATKL, GT_ALV-MTART) WHERE MATNR EQ GT_ALV-MATNR."物料组
*     GT_ALV-MATNR = INT_S000-MATNR.
*     GT_ALV-RATIO = SUBSTR3."周转率
    GT_ALV-DAYS = GT_ALV-DATE_E - GT_ALV-DATE_S + 1."周转天数
    IF GT_ALV-RATIO_C IS NOT INITIAL.
      GT_ALV-DAYS = GT_ALV-DAYS / GT_ALV-RATIO_C.
    ELSE.
      CLEAR GT_ALV-DAYS.
    ENDIF.
    APPEND GT_ALV.
    CLEAR GT_ALV.
  ENDLOOP.
  "添加尾部汇总行
  DATA L_ZXHL_C LIKE GT_ALV-ZXHL_C.
  DATA L_AVG_C LIKE GT_ALV-AVG_C.
  CLEAR: L_ZXHL_C, L_AVG_C.
  LOOP AT GT_ALV.
    ADD GT_ALV-ZXHL_C TO L_ZXHL_C.
    ADD GT_ALV-AVG_C TO L_AVG_C.
  ENDLOOP.
  CLEAR: GT_ALV-MATNR,GT_ALV-MAKTX,GT_ALV-MTART,GT_ALV-MATKL,GT_ALV-RATIO_C,GT_ALV-ZXHL_C,GT_ALV-AVG_C,GT_ALV-DAYS.
  GT_ALV-MAKTX = '汇总：'.
  GT_ALV-ZXHL_C = L_ZXHL_C.
  GT_ALV-AVG_C = L_AVG_C.
  IF GT_ALV-AVG_C IS NOT INITIAL.
    GT_ALV-RATIO_C = GT_ALV-ZXHL_C / GT_ALV-AVG_C.
  ELSE.
    GT_ALV-RATIO_C = 99999.
  ENDIF.
  GT_ALV-DAYS = GT_ALV-DATE_E - GT_ALV-DATE_S + 1."周转天数
  IF GT_ALV-RATIO_C IS NOT INITIAL.
    GT_ALV-DAYS = GT_ALV-DAYS / GT_ALV-RATIO_C.
  ELSE.
    CLEAR GT_ALV-DAYS.
  ENDIF.
  APPEND GT_ALV.
ENDIF.
ENDFORM.
"
"=======================================================
"

*&---------------------------------------------------------------------*
*&      FORM  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->NAME   TEXT
*      -->TEXT   TEXT
*      -->REF_TABNAME     TEXT
*      -->REF_FIELDNAME   TEXT
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT  USING NAME
                            TEXT
                            REF_TABNAME
                            REF_FIELDNAME
                            CFIELDNAME.
  IT_FIELDCAT-FIELDNAME = NAME.
  IT_FIELDCAT-SELTEXT_L    =
  IT_FIELDCAT-SELTEXT_M    =
  IT_FIELDCAT-SELTEXT_S    =
  IT_FIELDCAT-REPTEXT_DDIC = TEXT.
  IT_FIELDCAT-REF_TABNAME = REF_TABNAME.
  IT_FIELDCAT-REF_FIELDNAME = REF_FIELDNAME.
  IT_FIELDCAT-CFIELDNAME = CFIELDNAME.
  APPEND IT_FIELDCAT.
  CLEAR IT_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT

FORM  MONAT_F4.
  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE   LIKE SY-SUBRC,
        MF_MONAT        LIKE ISELLIST-MONTH,
        MF_HLP_REPID    LIKE SY-REPID.
  FIELD-SYMBOLS: <MF_FELD>.

* Wert von Dynpro lesen
  GET CURSOR FIELD MF_DYNPFIELDS-FIELDNAME.
  APPEND MF_DYNPFIELDS.
  MF_HLP_REPID = SY-REPID.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
         EXPORTING
              DYNAME               = MF_HLP_REPID
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = MF_DYNPFIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 01
              INVALID_DYNPROFIELD  = 02
              INVALID_DYNPRONAME   = 03
              INVALID_DYNPRONUMMER = 04
              INVALID_REQUEST      = 05
              NO_FIELDDESCRIPTION  = 06
              UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
         EXPORTING
              INPUT  = MF_DYNPFIELDS-FIELDVALUE
         IMPORTING
              OUTPUT = MF_MONAT
         EXCEPTIONS
              ERROR_MESSAGE = 1.
    IF MF_MONAT IS INITIAL.
*     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
      MF_MONAT = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
         EXPORTING
              ACTUAL_MONTH               = MF_MONAT
         IMPORTING
              SELECTED_MONTH             = MF_MONAT
              RETURN_CODE                = MF_RETURNCODE
         EXCEPTIONS
              FACTORY_CALENDAR_NOT_FOUND = 01
              HOLIDAY_CALENDAR_NOT_FOUND = 02
              MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
*     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
*     <MF_FELD> = MF_MONAT.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
           EXPORTING
                INPUT  =  MF_MONAT
           IMPORTING
                OUTPUT =  MF_DYNPFIELDS-FIELDVALUE.
      COLLECT MF_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
           EXPORTING
                DYNAME               = MF_HLP_REPID
                DYNUMB               = SY-DYNNR
         TABLES
                DYNPFIELDS           = MF_DYNPFIELDS
           EXCEPTIONS
                INVALID_ABAPWORKAREA = 01
                INVALID_DYNPROFIELD  = 02
                INVALID_DYNPRONAME   = 03
                INVALID_DYNPRONUMMER = 04
                INVALID_REQUEST      = 05
                NO_FIELDDESCRIPTION  = 06
                UNDEFIND_ERROR       = 07.           "<<== note 148804
    ENDIF.
  ENDIF.
ENDFORM.                               "MONAT_F4
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SCREEN .
  LOOP AT SCREEN.
    IF SCREEN-NAME CS 'S_DATS'.
      IF P_JE EQ 'X'.
        SCREEN-ACTIVE = '0'.
      ELSE.
        SCREEN-ACTIVE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-NAME CS 'S_MONS'.
      IF P_SL EQ 'X'.
        SCREEN-ACTIVE = '0'.
      ELSE.
        SCREEN-ACTIVE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
