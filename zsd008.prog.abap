*&---------------------------------------------------------------------*
*& Report  ZSD008
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2015/2/12                                            *
*& Created By   : 汉得-唐博                                            *
*& Description  :订单修改记录                                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZSD008.
TABLES: VBAK, CDPOS.

SELECT-OPTIONS S_VKORG FOR VBAK-VKORG OBLIGATORY. "销售组织
SELECT-OPTIONS S_VBELN FOR VBAK-VBELN. "销售订单
SELECT-OPTIONS S_ERDAT FOR SY-DATUM."变更日期

DATA GT_ALV TYPE TABLE OF ZSD008 WITH HEADER LINE.
DATA GT_CDPOS TYPE TABLE OF CDPOS WITH HEADER LINE.
DATA GW_ALV TYPE ZSD008.
DATA T_ZSD008 TYPE TABLE OF ZSD008 WITH HEADER LINE.
DATA T_VBELN TYPE TABLE OF VBAK-VBELN WITH HEADER LINE.
DATA T_OBJECTID TYPE TABLE OF CDPOS-OBJECTID WITH HEADER LINE.
DATA T_VBAK TYPE TABLE OF VBAK WITH HEADER LINE.

RANGES R_CHANGENR FOR ZSD008-CHANGENR."变更号

START-OF-SELECTION.

CLEAR: GT_ALV[], GT_CDPOS[], T_ZSD008[], T_VBELN[], T_OBJECTID[], R_CHANGENR[].

LOOP AT S_ERDAT.
  MOVE-CORRESPONDING S_ERDAT TO R_CHANGENR.
  IF S_ERDAT-LOW IS NOT INITIAL.
    R_CHANGENR-LOW+8(6) = '000000'.
  ENDIF.
  IF S_ERDAT-HIGH IS NOT INITIAL.
    R_CHANGENR-HIGH+8(6) = '235959'.
  ELSE.
    R_CHANGENR-HIGH(8) = R_CHANGENR-LOW(8).
    R_CHANGENR-HIGH+8(6) = '235959'.
    R_CHANGENR-OPTION = 'BT'.
  ENDIF.
  APPEND R_CHANGENR.
  CLEAR R_CHANGENR.
ENDLOOP.

"从内存获取销售订单
IMPORT T_VBELN[] FROM MEMORY ID 'ZSD007_VBELN'.

FREE MEMORY ID 'ZSD007_VBELN'.

IF T_VBELN[] IS NOT INITIAL.
  SELECT * FROM ZSD008
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    FOR ALL ENTRIES IN T_VBELN
    WHERE VBELN EQ T_VBELN-TABLE_LINE
    AND CHANGENR IN R_CHANGENR.

  LOOP AT T_VBELN.
    T_OBJECTID = T_VBELN.
    APPEND T_OBJECTID.
  ENDLOOP.

  SELECT * FROM CDHDR
    INNER JOIN CDPOS ON CDHDR~OBJECTCLAS = CDPOS~OBJECTCLAS
    AND CDHDR~OBJECTID = CDPOS~OBJECTID
    AND CDHDR~CHANGENR = CDPOS~CHANGENR
    INTO CORRESPONDING FIELDS OF TABLE GT_CDPOS
    FOR ALL ENTRIES IN T_OBJECTID
    WHERE CDPOS~OBJECTCLAS EQ 'VERKBELEG'
*    AND TABNAME EQ 'VBAP'
    AND CDPOS~OBJECTID EQ T_OBJECTID-TABLE_LINE
    AND CDHDR~UDATE IN S_ERDAT
    AND CDPOS~FNAME IN ('ZMENG','WMENG').
ELSE.
  SELECT * FROM ZSD008
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    WHERE VBELN IN S_VBELN
    AND CHANGENR IN R_CHANGENR.

  SELECT * FROM CDHDR
    INNER JOIN CDPOS ON CDHDR~OBJECTCLAS = CDPOS~OBJECTCLAS
    AND CDHDR~OBJECTID = CDPOS~OBJECTID
    AND CDHDR~CHANGENR = CDPOS~CHANGENR
    INTO CORRESPONDING FIELDS OF TABLE GT_CDPOS
    WHERE CDPOS~OBJECTCLAS EQ 'VERKBELEG'
*    AND TABNAME EQ 'VBAP'
    AND CDPOS~OBJECTID IN S_VBELN
    AND CDHDR~UDATE IN S_ERDAT
    AND CDPOS~FNAME IN ('ZMENG','WMENG').
ENDIF.

CLEAR T_VBAK[].
IF GT_ALV[] IS NOT INITIAL.
  SELECT * FROM VBAK
    INTO CORRESPONDING FIELDS OF TABLE T_VBAK
    FOR ALL ENTRIES IN GT_ALV
    WHERE VBELN EQ GT_ALV-VBELN
    AND VKORG IN S_VKORG.
ENDIF.

IF GT_CDPOS[] IS NOT INITIAL.
  SELECT * FROM VBAK
    APPENDING CORRESPONDING FIELDS OF TABLE T_VBAK
    FOR ALL ENTRIES IN GT_CDPOS
    WHERE VBELN EQ GT_CDPOS-OBJECTID(10)
    AND VKORG IN S_VKORG.
ENDIF.

SORT T_VBAK BY VBELN.

LOOP AT GT_ALV.
  READ TABLE T_VBAK WITH KEY VBELN = GT_ALV-VBELN BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
             ID 'VKORG' FIELD T_VBAK-VKORG
*             ID 'VTWEG' FIELD 'DUMMY'
*             ID 'SPART' FIELD 'DUMMY'
*             ID 'ACTVT' FIELD 'DUMMY'
             .
    IF SY-SUBRC <> 0.
      MESSAGE S001(ZSD01) WITH T_VBAK-VKORG DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
*   您没有销售组织&的权限！
    ENDIF.
  ELSE.
    DELETE GT_ALV.
  ENDIF.
ENDLOOP.

LOOP AT GT_CDPOS.
  READ TABLE T_VBAK WITH KEY VBELN = GT_CDPOS-OBJECTID(10) BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
             ID 'VKORG' FIELD T_VBAK-VKORG
*             ID 'VTWEG' FIELD 'DUMMY'
*             ID 'SPART' FIELD 'DUMMY'
*             ID 'ACTVT' FIELD 'DUMMY'
             .
    IF SY-SUBRC <> 0.
      MESSAGE S001(ZSD01) WITH T_VBAK-VKORG DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
*   您没有销售组织&的权限！
    ENDIF.
  ELSE.
    DELETE GT_CDPOS.
  ENDIF.
ENDLOOP.

DATA: L_STR1 TYPE CHAR10.
DATA: L_STR2 TYPE CHAR10.
DATA: L_STR3 TYPE CHAR10.
DATA: L_DATE TYPE SY-DATUM.
DATA: L_TIME TYPE SY-UZEIT.
DATA: L_DATE_PRE TYPE SY-DATUM.
DATA: L_TIME_PRE TYPE SY-UZEIT.
DATA: L_CHANGENR_PRE LIKE GT_ALV-CHANGENR.
"将CDPOS的数据添加到结果中
LOOP AT GT_CDPOS.
  MOVE-CORRESPONDING GT_CDPOS TO GT_ALV.
  GT_ALV-VBELN  = GT_CDPOS-OBJECTID.
  SPLIT GT_CDPOS-TABKEY AT SPACE INTO L_STR1 L_STR2.
  CONDENSE: L_STR1, L_STR2.
  GT_ALV-POSNR = L_STR2(6).
  SELECT SINGLE MATNR ARKTX
    FROM VBAP
    INTO (GT_ALV-MATNR, GT_ALV-ARKTX)
    WHERE VBELN EQ GT_ALV-VBELN
    AND POSNR EQ GT_ALV-POSNR.
  IF SY-SUBRC NE 0.
    SELECT SINGLE MATNR ARKTX
      FROM ZSD008
      INTO (GT_ALV-MATNR, GT_ALV-ARKTX)
      WHERE VBELN EQ GT_ALV-VBELN
      AND POSNR EQ GT_ALV-POSNR.
  ENDIF.
  CLEAR: L_DATE,L_TIME.
  SELECT SINGLE UDATE UTIME
    FROM CDHDR
    INTO (L_DATE,L_TIME)
    WHERE OBJECTCLAS = GT_CDPOS-OBJECTCLAS
    AND OBJECTID = GT_CDPOS-OBJECTID
    AND CHANGENR = GT_CDPOS-CHANGENR.
  "如果两次修改时间相差不超过2秒，则显示为同一个CHANGENR
  "缩小一秒（调整因为增强导致的时间误差）
  L_TIME_PRE = L_TIME - 1.
  L_DATE_PRE = L_DATE.
  IF L_TIME_PRE EQ '235959'.
    L_DATE_PRE = L_DATE - 1.
  ENDIF.
  CONCATENATE L_DATE_PRE L_TIME_PRE INTO L_CHANGENR_PRE.
  READ TABLE GT_ALV INTO GW_ALV WITH KEY VBELN = GT_ALV-VBELN CHANGENR = L_CHANGENR_PRE.
  IF SY-SUBRC EQ 0.
    L_DATE = L_DATE_PRE.
    L_TIME = L_TIME_PRE.
  ELSE.
    "缩小两秒（调整因为增强导致的时间误差）
    L_TIME_PRE = L_TIME - 2.
    L_DATE_PRE = L_DATE.
    IF L_TIME_PRE EQ '235959'
      OR L_TIME_PRE EQ '235958'.
      L_DATE_PRE = L_DATE - 1.
    ENDIF.
    CONCATENATE L_DATE_PRE L_TIME_PRE INTO L_CHANGENR_PRE.
    READ TABLE GT_ALV INTO GW_ALV WITH KEY VBELN = GT_ALV-VBELN CHANGENR = L_CHANGENR_PRE.
    IF SY-SUBRC EQ 0.
      L_DATE = L_DATE_PRE.
      L_TIME = L_TIME_PRE.
    ENDIF.
  ENDIF.
  CONCATENATE L_DATE L_TIME INTO GT_ALV-CHANGENR.
  IF GT_ALV-USERNAME IS INITIAL.
    SELECT SINGLE ERNAM INTO GT_ALV-USERNAME FROM VBAK WHERE VBELN EQ GT_ALV-VBELN.
  ENDIF.
  APPEND GT_ALV.
  CLEAR GT_ALV.
ENDLOOP.

LOOP AT GT_ALV.
  IF  GT_ALV-VALUE_NEW EQ GT_ALV-VALUE_OLD.
    DELETE GT_ALV.
  ENDIF.
ENDLOOP.

"重复记录处理
SORT GT_ALV BY VBELN CHANGENR POSNR FNAME DESCENDING.
"如果一个销售订单被连续更改了两次，然后变更前的数字相同，则只保留后面那一次
DELETE ADJACENT DUPLICATES FROM GT_ALV COMPARING VBELN POSNR FNAME MATNR ARKTX CHNGIND VALUE_OLD.
"如果一个销售订单被连续更改了两次，然后变更后的数字相同，则只保留后面那一次
DELETE ADJACENT DUPLICATES FROM GT_ALV COMPARING VBELN POSNR FNAME MATNR ARKTX CHNGIND VALUE_NEW.

IF T_VBELN[] IS NOT INITIAL.
T_ZSD008[] = GT_ALV[].
"将订单修改记录抛转到内存
EXPORT T_ZSD008[] TO MEMORY ID 'T_ZSD008'.
ELSE.
DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

CLEAR: IS_LAYOUT,IT_FIELDCAT[].
IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

PERFORM APPEND_FIELDCAT USING:
  'VBELN' '销售订单' '' '' '',
  'CHANGENR' '合同变更号' '' '' '',
  'POSNR' '行项目' '' '' '',
  'FNAME' '字段名' '' '' '',
  'MATNR' '物料号' 'MARA' 'MATNR' '',
  'ARKTX' '物料描述' '' '' '',
  'CHNGIND' '修改类型' '' '' '',
  'VALUE_NEW' '新值' '' '' '',
  'VALUE_OLD' '旧值' '' '' '',
  'USERNAME' '制单人' '' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
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
  ENDIF.
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
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM   TEXT
*      -->RS_SELFIELD   TEXT
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM TYPE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
*双击ALV行
  WHEN '&IC1'.
    READ TABLE GT_ALV INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC = 0.
      SET PARAMETER ID 'AUN' FIELD GT_ALV-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND
