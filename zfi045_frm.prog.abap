*&---------------------------------------------------------------------*
*&  包含                ZFI045_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_PROCESS_DATA .

  DATA: BEGIN OF LS_MSEG ,
          MBLNR TYPE MSEG-MBLNR,
          MJAHR TYPE MSEG-MJAHR,
          ZEILE TYPE MSEG-ZEILE,
          MATNR TYPE MSEG-MATNR,
          ANLN1 TYPE MSEG-ANLN1,
          ANLN2 TYPE MSEG-ANLN2,
          MENGE TYPE MSEG-MENGE,
          SHKZG TYPE MSEG-SHKZG,
        END OF LS_MSEG.
  DATA LT_MSEG LIKE TABLE OF LS_MSEG .
  DATA: BEGIN OF LS_MARA ,
          MATNR TYPE MARA-MATNR,
          MEINS TYPE MARA-MEINS,
        END OF LS_MARA.
  DATA LT_MARA LIKE TABLE OF LS_MARA .
  DATA: BEGIN OF LS_BSEG ,
          BUKRS TYPE BSEG-BUKRS,
          BELNR TYPE BSEG-BELNR,
          GJAHR TYPE BSEG-GJAHR,
          MATNR TYPE BSEG-MATNR,
          ANLN1 TYPE BSEG-ANLN1,
          ANLN2 TYPE BSEG-ANLN2,
          SHKZG TYPE BSEG-SHKZG,
          DMBTR TYPE BSEG-DMBTR,
        END OF LS_BSEG .
  DATA LT_BSEG LIKE TABLE OF LS_BSEG .
  DATA: BEGIN OF LS_M ,
          MATNR TYPE MARA-MATNR,
          ANLN1 TYPE MSEG-ANLN1,
          ANLN2 TYPE MSEG-ANLN2,
          MENGE TYPE MSEG-MENGE,
        END OF LS_M .
  DATA LT_M LIKE TABLE OF LS_M .
  DATA: LT_M_WC LIKE LT_M,
        LS_M_WC LIKE LINE OF LT_M_WC.
*  DATA: BEGIN OF LS_M_WC ,
*          MATNR TYPE MARA-MATNR,
*          MENGE TYPE MSEG-MENGE,
*        END OF LS_M_WC .
*  DATA LT_M_WC LIKE TABLE OF LS_M_WC .
  DATA: BEGIN OF LS_M_COL ,
          MATNR TYPE MARA-MATNR,
          MENGE TYPE MSEG-MENGE,
          DMBTR TYPE BSEG-DMBTR,
        END OF LS_M_COL .
  DATA LT_M_COL LIKE TABLE OF LS_M_COL .
  DATA: BEGIN OF LS_WC ,
          MATNR TYPE MARA-MATNR,
*          ANLN1 TYPE MSEG-ANLN1,
          DMBTR TYPE BSEG-DMBTR,
        END OF LS_WC .
  DATA LT_WC LIKE TABLE OF LS_WC .

  DATA L_NUM TYPE I VALUE IS INITIAL .
*  DATA L_BUDAT TYPE SY-DATUM .
  DATA: L_I_DATE TYPE SY-DATUM,
        L_E_DATE TYPE SY-DATUM.

  RANGES R_BUDAT FOR BKPF-BUDAT .

  CONCATENATE P_GJAHR P_MONAT '01' INTO L_I_DATE .
  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = L_I_DATE
    IMPORTING
      E_DATE = L_E_DATE.
  R_BUDAT-SIGN = 'I'.
  R_BUDAT-OPTION = 'BT'.
  R_BUDAT-LOW = L_I_DATE .
  R_BUDAT-HIGH = L_E_DATE .
  APPEND R_BUDAT .

  SELECT MBLNR
         MJAHR
         ZEILE
         MATNR
         ANLN1
         ANLN2
         MENGE
         SHKZG
    INTO CORRESPONDING FIELDS OF TABLE LT_MSEG
    FROM MSEG
   WHERE BUKRS = P_BUKRS
     AND MJAHR = P_GJAHR
     AND BUDAT_MKPF IN R_BUDAT[]
     AND BWART IN ('241','242').
  IF LT_MSEG IS NOT INITIAL .
    " 根据物料、资产编号、子资产编号汇总数量
    LOOP AT LT_MSEG INTO LS_MSEG .
      LS_M-MATNR = LS_MSEG-MATNR .
      LS_M-ANLN1 = LS_MSEG-ANLN1 .
      LS_M-ANLN2 = LS_MSEG-ANLN2 .
      IF LS_MSEG-SHKZG = 'H'.
        LS_M-MENGE = LS_MSEG-MENGE .
      ELSEIF LS_MSEG-SHKZG = 'S'.
        LS_M-MENGE = - LS_MSEG-MENGE .
      ENDIF.
      COLLECT LS_M INTO LT_M .
      CLEAR LS_M .
      CLEAR LS_MSEG .
    ENDLOOP.
    SORT LT_M BY MATNR ANLN1 ANLN2 .
    " 找出尾差将要填入的记录(物料的维度)
    MOVE-CORRESPONDING LT_M TO LT_M_WC .
    SORT LT_M_WC BY MATNR ASCENDING MENGE DESCENDING .
    DELETE ADJACENT DUPLICATES FROM LT_M_WC COMPARING MATNR .
    " 根据物料、资产编号汇总数量
    LOOP AT LT_M INTO LS_M .
      MOVE-CORRESPONDING LS_M TO LS_M_COL .
      COLLECT LS_M_COL INTO LT_M_COL .
      CLEAR LS_M_COL .
      CLEAR LS_M .
    ENDLOOP.
    SORT LT_M_COL BY MATNR .

    SELECT MATNR
           MEINS
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FROM MARA
       FOR ALL ENTRIES IN LT_M_COL
     WHERE MATNR = LT_M_COL-MATNR .
    SORT LT_MARA BY MATNR .

    SELECT A~BUKRS
           A~BELNR
           A~GJAHR
           A~MATNR
           A~ANLN1
           A~ANLN2
           A~SHKZG
           A~DMBTR
      INTO CORRESPONDING FIELDS OF TABLE LT_BSEG
      FROM BSEG AS A
     INNER JOIN BKPF AS B
        ON A~BUKRS = B~BUKRS
       AND A~BELNR = B~BELNR
       AND A~GJAHR = B~GJAHR
       FOR ALL ENTRIES IN LT_MSEG
     WHERE A~MATNR = LT_MSEG-MATNR
       AND B~BLART = 'ML'
       AND B~BUDAT IN R_BUDAT
       AND B~GJAHR = P_GJAHR
       AND B~BUKRS = P_BUKRS
       AND A~HKONT = '9009010101' .
    SORT LT_BSEG BY MATNR ANLN1 .
    " 根据物料、资产编号汇总物料总差异
    LOOP AT LT_M_COL INTO LS_M_COL .
      READ TABLE LT_BSEG WITH KEY MATNR = LS_M_COL-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
      IF SY-SUBRC = 0 .
        LOOP AT LT_BSEG INTO LS_BSEG FROM SY-TABIX .
          IF LS_BSEG-MATNR = LS_M_COL-MATNR .
            IF LS_BSEG-SHKZG = 'S'.
              LS_M_COL-DMBTR = LS_M_COL-DMBTR + LS_BSEG-DMBTR .
            ELSEIF LS_BSEG-SHKZG = 'H'.
              LS_M_COL-DMBTR = LS_M_COL-DMBTR - LS_BSEG-DMBTR .
            ENDIF.
            CLEAR LS_BSEG .
          ELSE .
            CLEAR LS_BSEG .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY LT_M_COL FROM LS_M_COL TRANSPORTING DMBTR
                                           WHERE MATNR = LS_M_COL-MATNR .
      CLEAR LS_M_COL .
    ENDLOOP.

  ELSE .
    MESSAGE '没数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF.

* 将值放进ALV
  LOOP AT LT_M INTO LS_M .
    L_NUM = L_NUM + 1 .
    GS_ALV-NUM = L_NUM .

    GS_ALV-BUKRS = P_BUKRS .
    GS_ALV-MONAT = P_MONAT .
    GS_ALV-GJAHR = P_GJAHR .
    GS_ALV-BUDAT = L_E_DATE .
    GS_ALV-MATNR = LS_M-MATNR .
    GS_ALV-ANLN1 = LS_M-ANLN1 .
    GS_ALV-ANLN2 = LS_M-ANLN2 .
    GS_ALV-MENGE_FH = LS_M-MENGE .
    READ TABLE LT_MARA INTO LS_MARA WITH KEY MATNR = LS_M-MATNR BINARY SEARCH .
    GS_ALV-MEINS = LS_MARA-MEINS .
    READ TABLE LT_M_COL INTO LS_M_COL WITH KEY MATNR = LS_M-MATNR BINARY SEARCH .
    GS_ALV-MENGE_COL = LS_M_COL-MENGE . " 总发货数量
    GS_ALV-DMBTR_COL = LS_M_COL-DMBTR . " 总差异
    IF GS_ALV-MENGE_COL IS NOT INITIAL AND GS_ALV-MENGE_COL NE 0 .
      GS_ALV-DMBTR_DW = GS_ALV-DMBTR_COL / GS_ALV-MENGE_COL .  " 单位差异
    ENDIF.
    GS_ALV-DMBTR_FT = GS_ALV-DMBTR_DW * GS_ALV-MENGE_FH .  " 分摊差异

    APPEND GS_ALV TO GT_ALV .
    CLEAR GS_ALV .

    CLEAR LS_M .
  ENDLOOP.

* 按物料维度累加尾差承担
  LOOP AT GT_ALV INTO GS_ALV .
    LS_WC-MATNR = GS_ALV-MATNR .
*    LS_WC-ANLN1 = GS_ALV-ANLN1 .
    LS_WC-DMBTR = GS_ALV-DMBTR_FT .
    COLLECT LS_WC INTO LT_WC .
    CLEAR LS_WC .
    CLEAR GS_ALV .
  ENDLOOP.
  SORT LT_WC BY MATNR .

  LOOP AT GT_ALV INTO GS_ALV .
    READ TABLE LT_M_WC WITH KEY MATNR = GS_ALV-MATNR
                                ANLN1 = GS_ALV-ANLN1
                                ANLN2 = GS_ALV-ANLN2 BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      GS_ALV-WC = 'X' .
      READ TABLE LT_WC INTO LS_WC WITH KEY MATNR = GS_ALV-MATNR BINARY SEARCH .
      GS_ALV-DMBTR_WC = LS_WC-DMBTR - GS_ALV-DMBTR_COL .
      GS_ALV-DMBTR_GZ = GS_ALV-DMBTR_FT - GS_ALV-DMBTR_WC .
      CLEAR LS_WC .
    ELSE .
      GS_ALV-DMBTR_GZ = GS_ALV-DMBTR_FT .
    ENDIF.
    MODIFY GT_ALV FROM GS_ALV .

    CLEAR GS_ALV .
  ENDLOOP.

  SORT GT_ALV BY NUM .

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
*  GS_LAYOUT-INFO_FNAME = 'CLR'.
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
  INIT_FIELDCAT 'NUM'          '序号'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'GJAHR'          '会计年度'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'MONAT'          '期间'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT'          '过账日期'         '' 'X' '' 'X' '' '' ''.
  INIT_FIELDCAT 'MATNR'          '物料'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN1'          '资产'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'ANLN2'          '子资产'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE_FH'          '发货数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MEINS'          '单位'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MENGE_COL'          '总发货数量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR_COL'          '总差异'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR_DW'          '单位差异'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR_FT'          '分摊差异'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WC'          '尾差承担'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR_WC'          '尾差'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DMBTR_GZ'          '过账金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BELNR'          '过账凭证'         '' '' '' '' '' '' ''.
  " 消息
  INIT_FIELDCAT 'TYPE'          '消息类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MSG'          '消息'         '' '' '' '' '' '' ''.

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
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_LVC
*     IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
*     IS_VARIANT               = PW_VARIANT
*     IT_EVENTS                = GT_EVENTS
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
      T_OUTTAB                 = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  delete rt_extab where fcode = '&ALL'.
*  delete rt_extab where fcode = '&SAL'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS1
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
  DATA:L_INDEX TYPE SY-TABIX.

  CASE R_UCOMM.
    WHEN '&POST'.
      PERFORM FRM_POST.
    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_POST .

  DATA L_SUBRC TYPE SY-SUBRC.
  DATA: L_BELNR TYPE BKPF-BELNR,
        L_TYPE  TYPE C,
        L_MSG   TYPE STRING.

  LOOP AT GT_ALV INTO GS_ALV WHERE BELNR IS NOT INITIAL .
    MESSAGE '请勿重复过账' TYPE 'E' .
    LEAVE LIST-PROCESSING .
  ENDLOOP.

  PERFORM FRM_FILLING_DATA .  " 将数据装进凭证抬头和行

  IF GT_HEAD IS INITIAL .
    MESSAGE '没有能过账的数据，请检查过账金额' TYPE 'W' .
    LEAVE LIST-PROCESSING .
  ENDIF.

  LOOP AT GT_HEAD INTO GS_HEAD .

    PERFORM FRM_CHECK_DOC CHANGING L_SUBRC.

    CHECK L_SUBRC EQ 0.

    PERFORM FRM_BAPI_DATA_PREP .
    PERFORM FRM_CALL_BAPI CHANGING L_BELNR L_TYPE L_MSG .

    READ TABLE GT_ALV INTO GS_ALV WITH KEY NUM = GS_HEAD-NUM BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_ALV-TYPE = L_TYPE .
      GS_ALV-MSG = L_MSG .
      GS_ALV-BELNR = L_BELNR .
      MODIFY GT_ALV FROM GS_ALV TRANSPORTING TYPE MSG BELNR
                                      WHERE NUM = GS_HEAD-NUM .
      CLEAR GS_ALV .
    ENDIF.

    CLEAR GS_HEAD .
  ENDLOOP.

  CLEAR: L_BELNR,
         L_TYPE ,
         L_MSG  .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FILLING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILLING_DATA .

  DATA L_WAERS TYPE T001-WAERS .

  PERFORM FRM_CLEAR_HEADITEM .

  SELECT SINGLE WAERS
    INTO L_WAERS
    FROM T001
   WHERE BUKRS = P_BUKRS.

  LOOP AT GT_ALV INTO GS_ALV .

*    GS_ALV-BUDAT = '20170315'.
*    P_MONAT = 03 .
    IF GS_ALV-DMBTR_GZ LT 0 .
      " 头
      GS_HEAD-NUM = GS_ALV-NUM .
      GS_HEAD-BLDAT = GS_ALV-BUDAT .
      GS_HEAD-BUDAT = GS_ALV-BUDAT .
      GS_HEAD-BLART = 'ML'.
      GS_HEAD-BUKRS = P_BUKRS .
      GS_HEAD-BKTXT = 'ML结算'.
      GS_HEAD-MONAT = P_MONAT .
      GS_HEAD-WAERS = L_WAERS .
      APPEND GS_HEAD TO GT_HEAD .
      CLEAR GS_HEAD .
      " 行
      GS_ITEM-NUM = GS_ALV-NUM .
      GS_ITEM-BUZEI = 1.
      GS_ITEM-BSCHL = '40'.
      GS_ITEM-HKONT = '9009010101'.
      GS_ITEM-MATNR = GS_ALV-MATNR .
      GS_ITEM-MEINS = GS_ALV-MEINS .
      GS_ITEM-MENGE = GS_ALV-MENGE_FH .
      GS_ITEM-WRBTR = - GS_ALV-DMBTR_GZ .
      GS_ITEM-BEWAR = '100'.
      APPEND GS_ITEM TO GT_ITEM .
      CLEAR GS_ITEM .

      GS_ITEM-NUM = GS_ALV-NUM .
      GS_ITEM-BUZEI = 2 .
      GS_ITEM-BSCHL = '75'.
      GS_ITEM-HKONT = '1601060101'.
      GS_ITEM-KOART = 'A' .
      GS_ITEM-MATNR = GS_ALV-MATNR .
      GS_ITEM-MEINS = GS_ALV-MEINS .
      GS_ITEM-ANLN1 = GS_ALV-ANLN1 .
      GS_ITEM-ANLN2 = GS_ALV-ANLN2 .
      GS_ITEM-BZDAT = GS_ALV-BUDAT .
      GS_ITEM-BEWAR = '100'.
      GS_ITEM-WRBTR = GS_ALV-DMBTR_GZ .
      APPEND GS_ITEM TO GT_ITEM .
      CLEAR GS_ITEM .

    ELSEIF GS_ALV-DMBTR_GZ GT 0 .
      " 头
      GS_HEAD-NUM = GS_ALV-NUM .
      GS_HEAD-BLDAT = GS_ALV-BUDAT .
      GS_HEAD-BUDAT = GS_ALV-BUDAT .
      GS_HEAD-BLART = 'ML'.
      GS_HEAD-BUKRS = P_BUKRS .
      GS_HEAD-BKTXT = 'ML结算'.
      GS_HEAD-MONAT = P_MONAT .
      GS_HEAD-WAERS = L_WAERS .
      APPEND GS_HEAD TO GT_HEAD .
      CLEAR GS_HEAD .
      " 行
      GS_ITEM-NUM = GS_ALV-NUM .
      GS_ITEM-BUZEI = 1.
      GS_ITEM-BSCHL = '50'.
      GS_ITEM-HKONT = '9009010101'.
      GS_ITEM-MATNR = GS_ALV-MATNR .
      GS_ITEM-MEINS = GS_ALV-MEINS .
      GS_ITEM-MENGE = GS_ALV-MENGE_FH .
      GS_ITEM-WRBTR = - GS_ALV-DMBTR_GZ .
      GS_ITEM-BEWAR = '100'.
      APPEND GS_ITEM TO GT_ITEM .
      CLEAR GS_ITEM .

      GS_ITEM-NUM = GS_ALV-NUM .
      GS_ITEM-BUZEI = 2 .
      GS_ITEM-BSCHL = '70'.
      GS_ITEM-HKONT = '1601060101'.
      GS_ITEM-KOART = 'A' .
      GS_ITEM-MATNR = GS_ALV-MATNR .
      GS_ITEM-MEINS = GS_ALV-MEINS .
      GS_ITEM-ANLN1 = GS_ALV-ANLN1 .
      GS_ITEM-ANLN2 = GS_ALV-ANLN2 .
      GS_ITEM-BZDAT = GS_ALV-BUDAT .
      GS_ITEM-BEWAR = '100'.
      GS_ITEM-WRBTR = GS_ALV-DMBTR_GZ .
      APPEND GS_ITEM TO GT_ITEM .
      CLEAR GS_ITEM .
    ENDIF.

    CLEAR GS_ALV .
  ENDLOOP.

  SORT GT_ITEM BY NUM .

  CLEAR L_WAERS .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CLEAR_HEADITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CLEAR_HEADITEM .
  REFRESH: GT_HEAD,GT_ITEM .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM FRM_CHECK_DOC  CHANGING P_SUBRC.
  " 凭证日期
  IF GS_HEAD-BLDAT IS INITIAL.
    MESSAGE TEXT-M02 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 过账日期
  IF GS_HEAD-BUDAT IS INITIAL.
    MESSAGE TEXT-M03 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 凭证类型
  IF GS_HEAD-BLART IS INITIAL.
    MESSAGE TEXT-M05 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 公司代码
  IF GS_HEAD-BUKRS IS INITIAL.
    MESSAGE TEXT-M06 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 货币
  IF GS_HEAD-WAERS IS INITIAL.
    MESSAGE TEXT-M07 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  " 销售订单
  IF GS_HEAD-MONAT IS INITIAL.
    MESSAGE TEXT-M08 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

*  "检查是否已经过账
*  IF   GS_HEAD-BELNR  IS NOT INITIAL .
**  AND  GS_HEAD-GJAHR  IS NOT INITIAL.
*    MESSAGE TEXT-M22 TYPE 'W'  DISPLAY LIKE 'E'.
*    P_SUBRC = 8.
*    EXIT.
*  ENDIF.

  " 行项目
  IF GT_ITEM IS INITIAL.
    MESSAGE TEXT-M09 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.

  DATA: L_OPREIS TYPE CKIS-OPREIS.

  READ TABLE GT_ITEM WITH KEY NUM = GS_HEAD-NUM BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC EQ 0 .

    LOOP AT GT_ITEM ASSIGNING <FS_ITEM> FROM SY-TABIX .

      IF <FS_ITEM>-NUM = GS_HEAD-NUM .

        " 记账码
        IF <FS_ITEM>-BSCHL IS INITIAL.
          MESSAGE TEXT-M11 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 物料号
        IF <FS_ITEM>-MATNR IS INITIAL.
          MESSAGE TEXT-M16 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 单位
        IF <FS_ITEM>-MEINS IS INITIAL.
          MESSAGE TEXT-M25 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        " 金额
        IF <FS_ITEM>-WRBTR IS INITIAL.
          MESSAGE TEXT-M17 TYPE 'W' DISPLAY LIKE 'E'.
          P_SUBRC = 8.
          EXIT.
        ENDIF.

        IF <FS_ITEM>-BSCHL = '75' OR <FS_ITEM>-BSCHL = '70'.
          " 资产
          IF <FS_ITEM>-ANLN1 IS INITIAL .
            MESSAGE TEXT-M19 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.
          " 次级资产
          IF <FS_ITEM>-ANLN2 IS INITIAL .
            MESSAGE TEXT-M20 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.
          " 资产价值日
          IF <FS_ITEM>-BZDAT IS INITIAL .
            MESSAGE TEXT-M21 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.
          " 事物类型
          IF <FS_ITEM>-BEWAR IS INITIAL .
            MESSAGE TEXT-M23 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.

        ELSEIF <FS_ITEM>-BSCHL = '50' OR <FS_ITEM>-BSCHL = '40'.
          " 科目
          IF <FS_ITEM>-HKONT IS INITIAL.
            MESSAGE TEXT-M12 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.
          " 发货数量
          IF <FS_ITEM>-MENGE IS INITIAL .
            MESSAGE TEXT-M24 TYPE 'W' DISPLAY LIKE 'E'.
            P_SUBRC = 8.
            EXIT.
          ENDIF.

        ENDIF.

        L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.

*        IF <FS_ITEM>-BSCHL = '40'  .
*          L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
*        ELSEIF <FS_ITEM>-BSCHL = '50'  .
*          IF L_OPREIS LT 0 .
*            L_OPREIS = L_OPREIS + <FS_ITEM>-WRBTR.
*          ELSEIF L_OPREIS GT 0 .
*            L_OPREIS = L_OPREIS - <FS_ITEM>-WRBTR.
*          ENDIF.
*        ENDIF.

      ELSE .
        EXIT .
      ENDIF.
    ENDLOOP.

  ENDIF.

  CLEAR L_OPREIS .

  IF P_SUBRC NE 0.
    EXIT.
  ENDIF.

  IF L_OPREIS NE 0.
    MESSAGE TEXT-M18 TYPE 'W' DISPLAY LIKE 'E'.
    P_SUBRC = 8.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_PREP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_PREP .
  " 清空BAPI变量
  PERFORM FRM_BAPI_DATA_CLEAR.

  DATA: L_KATYP TYPE CSKB-KATYP.

**********************************************************************
* 抬头
**********************************************************************
*凭证日期
  GS_DOCUMENTHEADER-DOC_DATE     =  GS_HEAD-BLDAT.
*过账日期
  GS_DOCUMENTHEADER-PSTNG_DATE   =  GS_HEAD-BUDAT.
*凭证类型
  GS_DOCUMENTHEADER-DOC_TYPE     =  GS_HEAD-BLART.
*公司代码
  GS_DOCUMENTHEADER-COMP_CODE    =  GS_HEAD-BUKRS.
*凭证抬头文本
  GS_DOCUMENTHEADER-HEADER_TXT   =  GS_HEAD-BKTXT.
*创建人员
  GS_DOCUMENTHEADER-USERNAME     =  SY-UNAME.
  "过账期间
  GS_DOCUMENTHEADER-FIS_PERIOD     =  GS_HEAD-MONAT .
**参考凭证号 - 业务类型
*  GS_DOCUMENTHEADER-REF_DOC_NO   =  GS_HEAD-BSTYP.


**********************************************************************
* 凭证行
**********************************************************************
  READ TABLE GT_ITEM WITH KEY NUM = GS_HEAD-NUM BINARY SEARCH TRANSPORTING NO FIELDS .
  IF SY-SUBRC = 0 .

    LOOP AT GT_ITEM INTO GS_ITEM FROM SY-TABIX .
      IF GS_ITEM-NUM = GS_HEAD-NUM .

        " 总账
        CLEAR GS_ACCOUNTGL.
*       行项目号
        GS_ACCOUNTGL-ITEMNO_ACC = GS_ITEM-BUZEI.
**       项目文本
*        GS_ACCOUNTGL-ITEM_TEXT = GS_ITEM-SGTXT.
*       物料
        GS_ACCOUNTGL-MATERIAL = GS_ITEM-MATNR.
        " 单位
        GS_ACCOUNTGL-BASE_UOM = GS_ITEM-MEINS.
*       税码
*      GS_ACCOUNTGL-TAX_CODE = GS_ITEM-MWSKZ.

        IF GS_ITEM-BSCHL = '75' OR GS_ITEM-BSCHL = '70' .
          " 科目
          GS_ACCOUNTGL-GL_ACCOUNT = GS_ITEM-HKONT.
          GS_ACCOUNTGL-ASSET_NO = GS_ITEM-ANLN1.
          GS_ACCOUNTGL-SUB_NUMBER = GS_ITEM-ANLN2.
*          GS_ACCOUNTGL-CS_TRANS_T = GS_ITEM-BEWAR.
          GS_ACCOUNTGL-ASVAL_DATE = GS_ITEM-BZDAT.
          GS_ACCOUNTGL-ACCT_TYPE = GS_ITEM-KOART.
        ELSEIF GS_ITEM-BSCHL = '50' OR GS_ITEM-BSCHL = '40' .
          " 科目
          GS_ACCOUNTGL-GL_ACCOUNT = GS_ITEM-HKONT.
          " 数量
          GS_ACCOUNTGL-QUANTITY = GS_ITEM-MENGE.
*          GS_ACCOUNTGL-CS_TRANS_T = GS_ITEM-BEWAR.
        ENDIF.
*      GS_ACCOUNTGL-ALLOC_NMBR = GS_HEAD-VBELN.
        APPEND GS_ACCOUNTGL TO GT_ACCOUNTGL.

        CLEAR GS_CURRENCYAMOUNT.
        GS_CURRENCYAMOUNT-ITEMNO_ACC = GS_ITEM-BUZEI.
*       货币
        GS_CURRENCYAMOUNT-CURRENCY = GS_HEAD-WAERS.
*       金额
        GS_CURRENCYAMOUNT-AMT_DOCCUR = GS_ITEM-WRBTR.
        APPEND GS_CURRENCYAMOUNT TO GT_CURRENCYAMOUNT.

        " 记账码 & 反记账
        CLEAR GS_EXTENSION2.
        CLEAR GS_ZACCDOCUEXT.
        GS_ZACCDOCUEXT-POSNR = GS_ITEM-BUZEI."行项目
        GS_ZACCDOCUEXT-BSCHL = GS_ITEM-BSCHL.
        IF GS_ITEM-BSCHL = '75' OR GS_ITEM-BSCHL = '70' .
          GS_ZACCDOCUEXT-ANBWA = GS_ITEM-BEWAR.
        ENDIF.
*      IF WA_ITEM-BSCHL = '11' OR WA_ITEM-BSCHL = '40'.
*        WA_ZACCDOCUEXT-XNEGP = 'X'.
*      ENDIF.
        GS_EXTENSION2-STRUCTURE  = 'ZACCDOCUEXT'.
        GS_EXTENSION2-VALUEPART1 = GS_ZACCDOCUEXT.
        APPEND GS_EXTENSION2 TO GT_EXTENSION2.

        CLEAR GS_ITEM .


      ELSE .
        EXIT .
      ENDIF.
      CLEAR GS_ITEM .
    ENDLOOP.
  ENDIF .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_DATA_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BAPI_DATA_CLEAR .
  REFRESH: GT_ACCOUNTGL, GT_ACCOUNTRECEIVABLE, GT_CURRENCYAMOUNT, GT_CRITERIA, GT_VALUEFIELD, GT_EXTENSION2, GT_RETURN.
  CLEAR: GS_DOCUMENTHEADER, GS_OBJ.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CALL_BAPI CHANGING P_BELNR P_TYPE P_MSG .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      DOCUMENTHEADER    = GS_DOCUMENTHEADER
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    IMPORTING
      OBJ_TYPE          = GS_OBJ-OBJ_TYPE
      OBJ_KEY           = GS_OBJ-OBJ_KEY
      OBJ_SYS           = GS_OBJ-OBJ_SYS
    TABLES
      ACCOUNTGL         = GT_ACCOUNTGL
      ACCOUNTRECEIVABLE = GT_ACCOUNTRECEIVABLE
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      CURRENCYAMOUNT    = GT_CURRENCYAMOUNT
      CRITERIA          = GT_CRITERIA
      VALUEFIELD        = GT_VALUEFIELD
*     EXTENSION1        =
      RETURN            = GT_RETURN
*     PAYMENTCARD       =
*     CONTRACTITEM      =
      EXTENSION2        = GT_EXTENSION2
*     REALESTATE        =
*     ACCOUNTWT         =
    .

  READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    P_MSG = GS_RETURN-MESSAGE .
*    PERFORM FRM_MESSAGE_DISPLAY.
    P_TYPE = 'E' .
    P_MSG = '过账失败' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    " 回写生成的会计凭证号与会计凭证年度
    P_BELNR = GS_OBJ-OBJ_KEY(10).
    P_TYPE = 'S' .
    P_MSG = '过账成功' .
*    P_GJAHR = GS_OBJ-OBJ_KEY+14(4).
  ENDIF.

  IF P_BELNR IS INITIAL .
    LOOP AT GT_RETURN INTO GS_RETURN WHERE TYPE = 'E' OR TYPE = 'A' .
      CONCATENATE P_MSG GS_RETURN-MESSAGE ';' INTO P_MSG .
      CLEAR GS_RETURN .
    ENDLOOP.
  ENDIF.

ENDFORM.
