*&---------------------------------------------------------------------*
*& Report  ZSD007
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2015/3/3                                             *
*& Created By   : 汉得-唐博                                            *
*& Description  :合同变更列表                                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZSD007.

TABLES: VBAK.
TYPE-POOLS: SLIS.

SELECT-OPTIONS: S_VKORG FOR VBAK-VKORG OBLIGATORY."销售组织
SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN."销售订单
SELECT-OPTIONS: S_AUART FOR VBAK-AUART MATCHCODE OBJECT Z_TVAK_SD005."订单类型
SELECT-OPTIONS: S_ERDAT FOR VBAK-ERDAT."创建日期
SELECT-OPTIONS: S_UDATE FOR VBAK-ERDAT."变更日期

DATA T_ZSD008 TYPE TABLE OF ZSD008 WITH HEADER LINE.
DATA:
  BEGIN OF T_SO OCCURS 1,
    VKORG TYPE TVKO-VKORG, "销售组织
    VBELN TYPE VBAK-VBELN, "订单编号
    AUART TYPE VBAK-AUART, "订单类型
    Z001  TYPE CHAR30, "项目名称
    ZSD0302 TYPE VBAK-ZSD0302, "销售立项号
    ERDAT  TYPE VBAK-ERDAT, "创建时间
    ERNAM  TYPE VBAK-ERNAM, "创建人
    KUNNR  TYPE VBAK-KUNNR, "客户编号
    NAME1  TYPE KNA1-NAME1, "客户名称
    BEZEI_HY  TYPE TVV1T-BEZEI, "行业
    BEZEI_JJ  TYPE TVV2T-BEZEI, "经济性质
    BZIRK TYPE VBKD-BZIRK,
    BZTXT_XZ  TYPE T171T-BZTXT, "行政区域
    LANDX  TYPE T005T-LANDX, "项目实施国家
    BZTXT_XM  TYPE T171T-BZTXT, "项目实施地区
    BEZEI  TYPE T005U-BEZEI, "项目实施省份
    CHANGENR  TYPE CHAR14, "合同变更号
    UDATE  TYPE CDHDR-UDATE, "变更日期
    TIMES  TYPE I, "变更次数
    Z004  TYPE CHAR30, "变更原因
    Z005  TYPE CHAR30, "变更摘要
    PMBG  TYPE I, "屏幕变更
    KZBG  TYPE I, "控制变更
    PTBG  TYPE I, "配套变更
    BJBG  TYPE I, "备件变更
    RJBG  TYPE I, "软件变更
    AZFSBG TYPE I, "安装方式变更
    ZBNXBG TYPE I, "质保年限变更
    FKFSBG TYPE I, "付款方式变更
    BGQHTE TYPE VBAP-NETWR, "变更前合同额
    BGHHTE TYPE VBAP-NETWR, "变更后合同额
    TZJE TYPE VBAP-NETWR, "调整金额
    WAERK TYPE VBAP-WAERK, "货币
    Z006 TYPE CHAR30, "变更提出人
    Z007 TYPE CHAR30, "销售经办人
    USERNAME TYPE CDHDR-USERNAME, "制单人
  END OF T_SO,
  T_ALV LIKE TABLE OF T_SO WITH HEADER LINE,
  T_ALV_TMP LIKE TABLE OF T_SO WITH HEADER LINE.

DATA: BEGIN OF T_ZSD007 OCCURS 1,
        VBELN TYPE ZSD008-VBELN,
        CHANGENR TYPE ZSD008-CHANGENR,
        USERNAME TYPE CDHDR-USERNAME,
        MATNR_2 TYPE CHAR2,
        MATNR_3 TYPE CHAR3,
        COUNT TYPE I,
      END OF T_ZSD007,
      T_ZSD007_MATSUM LIKE TABLE OF T_ZSD007 WITH HEADER LINE,
      T_ZSD007_CHGSUM LIKE TABLE OF T_ZSD007 WITH HEADER LINE.

DATA: BEGIN OF T_CD OCCURS 1,
        OBJECTID TYPE CDHDR-OBJECTID,
        CHANGENR TYPE ZSD008-CHANGENR,
        USERNAME TYPE CDHDR-USERNAME,
        FNAME TYPE CDPOS-FNAME,
        UDATE TYPE CDHDR-UDATE,
        UTIME TYPE CDHDR-UTIME,
        COUNT TYPE I,
      END OF T_CD,
      T_CD_FNASUM LIKE TABLE OF T_CD WITH HEADER LINE,
      T_CD_CHGSUM LIKE TABLE OF T_CD WITH HEADER LINE,

      BEGIN OF T_VBAP OCCURS 1,
        VBELN TYPE VBAP-VBELN,
        POSNR TYPE VBAP-POSNR,
        NETWR TYPE VBAP-NETWR,
      END OF T_VBAP,
      T_VBAP_NETWR LIKE TABLE OF T_VBAP WITH HEADER LINE.

"设置要获取的销售订单
DATA T_VBELN TYPE TABLE OF VBAK-VBELN WITH HEADER LINE.

START-OF-SELECTION.

CLEAR: T_SO[], T_VBELN[], T_ZSD008[], T_ZSD007[], T_ZSD007_MATSUM[], T_ZSD007_CHGSUM[], T_CD[], T_CD_FNASUM[], T_CD_CHGSUM[], T_VBAP[], T_VBAP_NETWR[].

SELECT
  A~VKORG
  A~VBELN
  A~AUART
  A~ZSD0302
  A~ERDAT
  A~ERNAM
  A~KUNNR
  A~WAERK
  A~ERNAM AS USERNAME
  B~NAME1
  C~BEZEI AS BEZEI_HY
  D~BEZEI AS BEZEI_JJ
  E~BZIRK
  F~LANDX
  G~BZTXT AS BZTXT_XM
  H~BEZEI
  FROM VBAK AS A
  INNER JOIN KNA1 AS B ON A~KUNNR = B~KUNNR
  LEFT JOIN TVV1T AS C ON A~KVGR1 = C~KVGR1 AND C~SPRAS = SY-LANGU
  LEFT JOIN TVV2T AS D ON A~KVGR2 = D~KVGR2 AND D~SPRAS = SY-LANGU
  LEFT JOIN VBKD AS E ON E~VBELN = A~VBELN AND E~POSNR = ''
  LEFT JOIN T005T AS F ON F~LAND1 = A~COUNTRY AND F~SPRAS = SY-LANGU
  LEFT JOIN T171T AS G ON G~BZIRK = A~ZSD0303 AND G~SPRAS = SY-LANGU
  LEFT JOIN T005U AS H ON H~LAND1 = A~COUNTRY AND H~BLAND = A~REGION AND H~SPRAS = SY-LANGU
*  LEFT JOIN T171T AS F ON F~BZIRK = E~BZIRK
  INTO CORRESPONDING FIELDS OF TABLE T_SO
  WHERE A~VKORG IN S_VKORG
  AND A~VBELN IN S_VBELN
  AND A~AUART IN S_AUART
  AND A~ERDAT IN S_ERDAT
  .

SORT T_SO.
DELETE ADJACENT DUPLICATES FROM T_SO COMPARING ALL FIELDS.

IF T_SO[] IS INITIAL.
  MESSAGE S003(ZFICO01) DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING.
ENDIF.

LOOP AT T_SO.
  T_VBELN = T_SO-VBELN.
  AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
           ID 'VKORG' FIELD T_SO-VKORG
*           ID 'VTWEG' FIELD 'DUMMY'
*           ID 'SPART' FIELD 'DUMMY'
*           ID 'ACTVT' FIELD 'DUMMY'
           .
  IF SY-SUBRC <> 0.
    MESSAGE S001(ZSD01) WITH T_SO-VKORG DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
* 您没有销售组织&的权限！
  ENDIF.
  APPEND T_VBELN.
ENDLOOP.

"将要获取的销售订单抛转到内存
EXPORT T_VBELN[] TO MEMORY ID 'ZSD007_VBELN'.

IF S_VBELN[] IS INITIAL.
  S_VBELN-SIGN = 'I'.
  S_VBELN-OPTION = 'CP'.
  S_VBELN-LOW = '*'.
  APPEND S_VBELN.
ENDIF.

"跳转到ZSD008
SUBMIT ZSD008 WITH S_VKORG IN S_VKORG
              WITH S_VBELN IN S_VBELN
              WITH S_ERDAT IN S_UDATE AND RETURN.

"从内存获取订单修改记录
IMPORT T_ZSD008[] FROM MEMORY ID 'T_ZSD008'.

FREE MEMORY ID 'T_ZSD008'.

  DATA T_OBJECTID TYPE TABLE OF CDPOS-OBJECTID WITH HEADER LINE.
  CLEAR T_OBJECTID[].

  LOOP AT T_VBELN.
    T_OBJECTID = T_VBELN.
    APPEND T_OBJECTID.
  ENDLOOP.

"获取行项目净价信息
SELECT VBELN POSNR NETWR
  INTO CORRESPONDING FIELDS OF TABLE T_VBAP
  FROM VBAP
  FOR ALL ENTRIES IN T_VBELN
  WHERE VBELN EQ T_VBELN-TABLE_LINE.

SORT T_VBAP BY VBELN.

LOOP AT T_VBAP.
  AT END OF VBELN.
    SUM.
    MOVE-CORRESPONDING T_VBAP TO T_VBAP_NETWR.
    APPEND T_VBAP_NETWR.
  ENDAT.
ENDLOOP.

"获取安装方式变更、质保年限变更、付款方式变更的内容
SELECT * FROM CDHDR AS A
  INNER JOIN CDPOS AS B
  ON A~OBJECTCLAS = B~OBJECTCLAS
  AND A~OBJECTID = B~OBJECTID
  AND A~CHANGENR = B~CHANGENR
  INTO CORRESPONDING FIELDS OF TABLE T_CD
  FOR ALL ENTRIES IN T_OBJECTID
  WHERE A~OBJECTCLAS EQ 'VERKBELEG'
  AND A~OBJECTID EQ T_OBJECTID-TABLE_LINE
  AND B~FNAME IN ('MVGR5','VSNMR_V','ZTERM')
  AND A~UDATE IN S_UDATE.

  LOOP AT T_CD.
    T_CD-CHANGENR(8) = T_CD-UDATE.
    T_CD-CHANGENR+8(6) = T_CD-UTIME.
    T_CD-COUNT = 1.
    MODIFY T_CD.
  ENDLOOP.

SORT T_CD BY OBJECTID CHANGENR USERNAME FNAME.

LOOP AT T_CD.
  AT END OF FNAME.
    SUM.
    MOVE-CORRESPONDING T_CD TO T_CD_FNASUM.
    APPEND T_CD_FNASUM.
    CLEAR T_CD_FNASUM.
  ENDAT.
ENDLOOP.

LOOP AT T_CD.
  AT END OF USERNAME.
    SUM.
    MOVE-CORRESPONDING T_CD TO T_CD_CHGSUM.
    APPEND T_CD_CHGSUM.
    CLEAR T_CD_CHGSUM.
  ENDAT.
ENDLOOP.

"按合同变更号汇总
LOOP AT T_ZSD008.
  MOVE-CORRESPONDING T_ZSD008 TO T_ZSD007.
  T_ZSD007-MATNR_2 = T_ZSD008-MATNR(2).
  T_ZSD007-MATNR_3 = T_ZSD008-MATNR(3).
*  IF T_ZSD007-MATNR_3 EQ '002'
*    OR T_ZSD007-MATNR_3 EQ '006'
*    OR T_ZSD007-MATNR_3 EQ '005'
*    OR T_ZSD007-MATNR_2 EQ '99'.
    T_ZSD007-COUNT = 1.
    APPEND T_ZSD007.
    CLEAR T_ZSD007.
*  ENDIF.
ENDLOOP.

SORT T_ZSD007 BY VBELN CHANGENR USERNAME MATNR_2 MATNR_3.

LOOP AT T_ZSD007.
  AT END OF MATNR_3.
    SUM.
    MOVE-CORRESPONDING T_ZSD007 TO T_ZSD007_MATSUM.
    APPEND T_ZSD007_MATSUM.
    CLEAR T_ZSD007_MATSUM.
  ENDAT.
ENDLOOP.

LOOP AT T_ZSD007.
  AT END OF USERNAME.
    SUM.
    MOVE-CORRESPONDING T_ZSD007 TO T_ZSD007_CHGSUM.
    APPEND T_ZSD007_CHGSUM.
    CLEAR T_ZSD007_CHGSUM.
  ENDAT.
ENDLOOP.

DATA L_COUNT TYPE I.

"获取最终显示的数据
LOOP AT T_SO.
  "行政区域
  SELECT SINGLE BZTXT FROM T171T INTO T_SO-BZTXT_XZ WHERE BZIRK = T_SO-BZIRK AND SPRAS = SY-LANGU.
  "项目名称：[文本]销售订单抬头文本字段Z001
  PERFORM GET_TEXT USING 'Z001' CHANGING T_SO-Z001.
  "变更提出人：[文本]销售订单抬头文本Z006
  PERFORM GET_TEXT USING 'Z006' CHANGING T_SO-Z006.
  "销售经办人：[文本]销售订单抬头文本Z007
  PERFORM GET_TEXT USING 'Z007' CHANGING T_SO-Z007.
  "变更后合同额
  READ TABLE T_VBAP_NETWR WITH KEY VBELN = T_SO-VBELN BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    T_SO-BGHHTE = T_VBAP_NETWR-NETWR.
  ENDIF.
  CLEAR T_ALV_TMP[].
  LOOP AT T_ZSD007_CHGSUM WHERE VBELN EQ T_SO-VBELN.
    MOVE-CORRESPONDING T_SO TO T_ALV.
    T_ALV-CHANGENR = T_ZSD007_CHGSUM-CHANGENR.
    IF T_ZSD007_CHGSUM-USERNAME IS NOT INITIAL.
      T_ALV-USERNAME = T_ZSD007_CHGSUM-USERNAME.
    ENDIF.
    T_ALV-UDATE = T_ZSD007_CHGSUM-CHANGENR(8).
    T_ALV-TIMES = T_ZSD007_CHGSUM-COUNT.
    READ TABLE T_ZSD007_MATSUM WITH KEY
    VBELN = T_ZSD007_CHGSUM-VBELN
    CHANGENR = T_ZSD007_CHGSUM-CHANGENR
    MATNR_2 = '00' MATNR_3 = '002' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-PMBG = T_ZSD007_MATSUM-COUNT. "屏幕变更
    ENDIF.
    READ TABLE T_ZSD007_MATSUM WITH KEY
    VBELN = T_ZSD007_CHGSUM-VBELN
    CHANGENR = T_ZSD007_CHGSUM-CHANGENR
    MATNR_2 = '00' MATNR_3 = '006' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-KZBG = T_ZSD007_MATSUM-COUNT. "控制变更
    ENDIF.
    READ TABLE T_ZSD007_MATSUM WITH KEY
    VBELN = T_ZSD007_CHGSUM-VBELN
    CHANGENR = T_ZSD007_CHGSUM-CHANGENR
    MATNR_2 = '99' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-PTBG = T_ZSD007_MATSUM-COUNT. "配套变更
    ENDIF.
*    READ TABLE T_ZSD007_MATSUM WITH KEY
*    VBELN = T_ZSD007_CHGSUM-VBELN
*    CHANGENR = T_ZSD007_CHGSUM-CHANGENR
*    MATNR_H = '002' BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      T_ALV-BJBG = T_ZSD007_MATSUM-COUNT. "备件变更
*    ENDIF.
    READ TABLE T_ZSD007_MATSUM WITH KEY
    VBELN = T_ZSD007_CHGSUM-VBELN
    CHANGENR = T_ZSD007_CHGSUM-CHANGENR
    MATNR_2 = '00' MATNR_3 = '005' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-RJBG = T_ZSD007_MATSUM-COUNT. "软件变更
    ENDIF.
    "调整金额 = “新值总额”-“旧值总额”
    LOOP AT T_ZSD008 WHERE VBELN EQ T_SO-VBELN AND CHANGENR = T_ZSD007_CHGSUM-CHANGENR AND FNAME = 'NETWR'.
      ADD T_ZSD008-VALUE_NEW TO T_ALV-TZJE.
      SUBTRACT T_ZSD008-VALUE_OLD FROM T_ALV-TZJE.
    ENDLOOP.
    "变更前合同额 ="变更后合同额" - "调整金额"
    T_ALV-BGQHTE = T_ALV-BGHHTE - T_ALV-TZJE.
    APPEND T_ALV TO T_ALV_TMP.
    CLEAR T_ALV.
  ENDLOOP.
  LOOP AT T_CD_CHGSUM WHERE OBJECTID EQ T_SO-VBELN.
    MOVE-CORRESPONDING T_SO TO T_ALV.
    T_ALV-CHANGENR = T_CD_CHGSUM-CHANGENR.
    IF T_CD_CHGSUM-USERNAME IS NOT INITIAL.
      T_ALV-USERNAME = T_CD_CHGSUM-USERNAME.
    ENDIF.
    T_ALV-UDATE = T_CD_CHGSUM-CHANGENR(8).
    T_ALV-TIMES = T_CD_CHGSUM-COUNT.
    READ TABLE T_CD_FNASUM WITH KEY
    OBJECTID = T_CD_CHGSUM-OBJECTID
    CHANGENR = T_CD_CHGSUM-CHANGENR
    FNAME = 'MVGR5' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-AZFSBG = T_CD_FNASUM-COUNT. "安装方式变更
    ENDIF.
    READ TABLE T_CD_FNASUM WITH KEY
    OBJECTID = T_CD_CHGSUM-OBJECTID
    CHANGENR = T_CD_CHGSUM-CHANGENR
    FNAME = 'VSNMR_V' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-ZBNXBG = T_CD_FNASUM-COUNT. "质保年限变更
    ENDIF.
    READ TABLE T_CD_FNASUM WITH KEY
    OBJECTID = T_CD_CHGSUM-OBJECTID
    CHANGENR = T_CD_CHGSUM-CHANGENR
    FNAME = 'ZTERM' BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ALV-FKFSBG = T_CD_FNASUM-COUNT. "付款方式变更
    ENDIF.
    "变更前合同额 ="变更后合同额" - "调整金额"
    T_ALV-BGQHTE = T_ALV-BGHHTE - T_ALV-TZJE.
    READ TABLE T_ALV_TMP WITH KEY CHANGENR = T_CD_CHGSUM-CHANGENR.
    IF SY-SUBRC EQ 0.
      ADD T_ALV-TIMES TO T_ALV_TMP-TIMES. "变更次数
      ADD T_ALV-AZFSBG TO T_ALV_TMP-AZFSBG. "安装方式变更
      ADD T_ALV-ZBNXBG TO T_ALV_TMP-ZBNXBG. "质保年限变更
      ADD T_ALV-FKFSBG TO T_ALV_TMP-FKFSBG. "付款方式变更
      MODIFY TABLE T_ALV_TMP.
    ELSE.
      APPEND T_ALV TO T_ALV_TMP.
    ENDIF.
    CLEAR T_ALV.
  ENDLOOP.
  SORT T_ALV_TMP BY VBELN CHANGENR.
  CLEAR L_COUNT.
  LOOP AT T_ALV_TMP."根据变更次数设置文本
*    MOVE-CORRESPONDING T_ALV_TMP TO T_ALV.
    ADD 1 TO L_COUNT.
    "变更原因：[文本]取销售订单抬头文本字段取“变更次数”对应数字编号后的文本，到回车结束。Z004
    PERFORM GET_LINE_TEXT USING 'Z004' L_COUNT CHANGING T_ALV_TMP-Z004.
    "变更摘要：[文本]取销售订单抬头文本字段取“变更次数”对应数字编号后的文本，到回车结束。Z005
    PERFORM GET_LINE_TEXT USING 'Z005' L_COUNT CHANGING T_ALV_TMP-Z005.
    MODIFY T_ALV_TMP.
  ENDLOOP.
  "设置变更前后后合同额
  DATA L_LINE_COUNT TYPE I.
  DATA L_CURRENT_LINE TYPE I.
  DATA L_BGQHTE LIKE T_ALV_TMP-BGQHTE.
  L_LINE_COUNT = LINES( T_ALV_TMP[] ).
  CLEAR L_BGQHTE.
  DO L_LINE_COUNT TIMES.
    L_CURRENT_LINE = L_LINE_COUNT - SY-INDEX + 1.
    READ TABLE T_ALV_TMP INDEX L_CURRENT_LINE.
    IF SY-SUBRC EQ 0.
      IF L_CURRENT_LINE < L_LINE_COUNT.
        T_ALV_TMP-BGHHTE = L_BGQHTE."变更后合同额 = 下一个的变更前合同额
        T_ALV_TMP-BGQHTE = T_ALV_TMP-BGHHTE - T_ALV_TMP-TZJE."变更前合同额 = 变更后合同额 - 调整金额
      ENDIF.
      MODIFY T_ALV_TMP INDEX L_CURRENT_LINE.
    ENDIF.
    "记录本次的变更前合同额
    L_BGQHTE = T_ALV_TMP-BGQHTE.
  ENDDO.
  APPEND LINES OF T_ALV_TMP TO T_ALV.
ENDLOOP.

LOOP AT T_ALV.
  "判断查看创建人的权限
  AUTHORITY-CHECK OBJECT 'Z_SD_USER'
           ID 'VKORG' FIELD T_ALV-VKORG
           ID 'USR20_1' FIELD T_ALV-ERNAM.
  IF SY-SUBRC <> 0 AND T_ALV-ERNAM NE SY-UNAME.
    AUTHORITY-CHECK OBJECT 'Z_SD_USER'
             ID 'VKORG' FIELD T_ALV-VKORG
             ID 'USR20_1' FIELD T_ALV-USERNAME.
    IF SY-SUBRC <> 0 AND T_ALV-USERNAME NE SY-UNAME.
      DELETE T_ALV.
      CONTINUE.
    ENDIF.
  ENDIF.
ENDLOOP.

DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

CLEAR: IS_LAYOUT,IT_FIELDCAT[].
IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

PERFORM APPEND_FIELDCAT USING:
  'VKORG' '销售组织' '' ''  '',
  'VBELN' '订单编号' '' ''  '',
  'AUART' '订单类型' '' ''  '',
  'Z001' '项目名称' '' ''  '',
  'ZSD0302' '销售立项号' '' ''  '',
  'ERDAT' '创建时间' '' ''  '',
  'ERNAM' '创建人' '' ''  '',
  'KUNNR' '客户编号' '' ''  '',
  'NAME1' '客户名称' '' ''  '',
  'BEZEI_HY' '行业' '' ''  '',
  'BEZEI_JJ' '经济性质' '' ''  '',
  'BZTXT_XZ' '行政区域' '' ''  '',
  'LANDX' '项目实施国家' '' ''  '',
  'BZTXT_XM' '项目实施地区' '' ''  '',
  'BEZEI' '项目实施省份' '' ''  '',
  'CHANGENR' '合同变更号' '' ''  '',
  'UDATE' '变更日期' '' ''  '',
  'TIMES' '变更次数' '' ''  '',
  'Z004' '变更原因' '' ''  '',
  'Z005' '变更摘要' '' ''  '',
  'PMBG' '屏幕变更' '' ''  '',
  'KZBG' '控制变更' '' ''  '',
  'PTBG' '配套变更' '' ''  '',
  'BJBG' '备件变更' '' ''  '',
  'RJBG' '软件变更' '' ''  '',
  'AZFSBG' '安装方式变更' '' ''  '',
  'ZBNXBG' '质保年限变更' '' ''  '',
  'FKFSBG' '付款方式变更' '' ''  '',
  'BGQHTE' '变更前合同额' '' ''  'WAERK',
  'BGHHTE' '变更后合同额' '' ''  'WAERK',
  'TZJE' '调整金额' '' ''  'WAERK',
  'WAERK' '货币' '' '' '',
  'Z006' '变更提出人' '' '' '',
  'Z007' '销售经办人' '' '' '',
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
*     IT_SO_GRAPHICS                   =
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
* Implement suitable error handling here
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
    READ TABLE T_ALV INDEX RS_SELFIELD-TABINDEX.
    IF SY-SUBRC = 0.
      SET PARAMETER ID 'AUN' FIELD T_ALV-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID   text
*      <--P_TEXT  text
*----------------------------------------------------------------------*
FORM GET_TEXT  USING    VALUE(P_ID)
               CHANGING P_TEXT.
  DATA L_TDNAME TYPE THEAD-TDNAME.
  DATA T_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
  L_TDNAME = T_SO-VBELN.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      ID                            = P_ID
      LANGUAGE                      = SY-LANGU
      NAME                          = L_TDNAME
      OBJECT                        = 'VBBK'
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
*     OLD_LINE_COUNTER              =
    TABLES
      LINES                         = T_TLINE[]
    EXCEPTIONS
      ID                            = 1
      LANGUAGE                      = 2
      NAME                          = 3
      NOT_FOUND                     = 4
      OBJECT                        = 5
      REFERENCE_CHECK               = 6
      WRONG_ACCESS_TO_ARCHIVE       = 7
      OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  IF T_TLINE[] IS NOT INITIAL.
    READ TABLE T_TLINE INDEX 1.
    P_TEXT = T_TLINE-TDLINE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ID   text
*      <--P_TEXT  text
*----------------------------------------------------------------------*
FORM GET_LINE_TEXT  USING VALUE(P_ID) VALUE(P_INDEX)
               CHANGING P_TEXT.
  DATA L_TDNAME TYPE THEAD-TDNAME.
  DATA T_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
  L_TDNAME = T_SO-VBELN.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      ID                            = P_ID
      LANGUAGE                      = SY-LANGU
      NAME                          = L_TDNAME
      OBJECT                        = 'VBBK'
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
*     OLD_LINE_COUNTER              =
    TABLES
      LINES                         = T_TLINE[]
    EXCEPTIONS
      ID                            = 1
      LANGUAGE                      = 2
      NAME                          = 3
      NOT_FOUND                     = 4
      OBJECT                        = 5
      REFERENCE_CHECK               = 6
      WRONG_ACCESS_TO_ARCHIVE       = 7
      OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  DATA L_LINE_COUNT.
  L_LINE_COUNT = LINES( T_TLINE[] ).
  IF L_LINE_COUNT GE P_INDEX.
    READ TABLE T_TLINE INDEX P_INDEX.
    P_TEXT = T_TLINE-TDLINE.
  ENDIF.
ENDFORM.
