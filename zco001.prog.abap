*----------------------------------------------------------------------------
*模块	MM
*
*请求类型           PROG:ZCO001
*内容描述                   采购差异监控表
*版本       V1.0
*姓名
*日期       02.02.2015 14:42:16
*-----------------------------------------------------------------------------
REPORT zco001.
*----------------------------------------------------------------------*
*       TABLES RESOURCE                                                *
*----------------------------------------------------------------------*
TABLES: ekpo,ekbe,makt,mbew,ekko.
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*       DEFINE                                                         *
*----------------------------------------------------------------------*
DEFINE  macro_fill_fcat.
  CLEAR WA_FIELDCAT.
  &1 = &1 + 1.
  WA_FIELDCAT-COL_POS       = &1.
  WA_FIELDCAT-FIELDNAME     = &2.
  WA_FIELDCAT-SELTEXT_L     = &3.
  WA_FIELDCAT-SELTEXT_M     = &3.
  WA_FIELDCAT-SELTEXT_S     = &3.
  WA_FIELDCAT-HOTSPOT       = &4.
  WA_FIELDCAT-NO_ZERO       = &5.
  WA_FIELDCAT-KEY           = &6.
  WA_FIELDCAT-REF_TABNAME   = &7.
  WA_FIELDCAT-REF_FIELDNAME = &8.   " 内表中数量参照字段
  WA_FIELDCAT-JUST          = &9.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       TYPES                                                          *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_alv,
    bukrs             TYPE ekpo-bukrs,  "公司代码
    gjahr             TYPE ekbe-gjahr,  "会计年度
    lifnr             TYPE ekko-lifnr,  "供应商
    name1             TYPE lfa1-name1,  "供应商名称
    ebeln             TYPE ekbe-ebeln,
    matnr             TYPE ekbe-matnr,  "物料代码
    maktx             TYPE makt-maktx,  "物料描述
    kzkfg             TYPE mara-kzkfg,  "是否可配置
    stprs             TYPE mbew-stprs,  "本期标准价
    vmstp             TYPE mbew-vmstp,  "上期标准价
    bpmng             TYPE ekbe-bpmng,  "本期采购数量
    dmbtr             TYPE ekbe-dmbtr,  "本期采购金额
    average_price     TYPE ekbe-dmbtr,  "本期采购均价
    different_price   TYPE ekbe-dmbtr,  "采购均价与标准价差异
    different_total   TYPE ekbe-dmbtr,  "采购差异总额
    different_rate(8) TYPE c,           "差异比率
  END OF ty_alv,

  BEGIN OF ty_ekpo,
    ebeln TYPE ekko-ebeln,
    ebelp TYPE ekpo-ebelp,
*    werks TYPE ekpo-werks,   "工厂
    lifnr TYPE ekko-lifnr,
    bukrs TYPE ekko-bukrs,
  END OF ty_ekpo,

  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    zekkn TYPE ekbe-zekkn,
    vgabe TYPE ekbe-vgabe,
    gjahr TYPE ekbe-gjahr,
    belnr TYPE ekbe-belnr,
    buzei TYPE ekbe-buzei,
    matnr TYPE ekbe-matnr,
    werks TYPE ekbe-werks,
    bpmng TYPE ekbe-bpmng,
    dmbtr TYPE ekbe-dmbtr,
    shkzg TYPE ekbe-shkzg,
    budat TYPE ekbe-budat,
    bwart TYPE ekbe-bwart,
    bewtp TYPE ekbe-bewtp,
  END OF ty_ekbe,

  BEGIN OF ty_po,
    lifnr TYPE ekko-lifnr,   "供应商
    matnr TYPE ekbe-matnr,   "物料号
    bukrs TYPE ekko-bukrs,   "公司代码
    werks TYPE ekpo-werks,   "工厂
    bpmng TYPE ekbe-bpmng,   "采购数量
    gjahr TYPE ekbe-gjahr,   "会计年度
    dmbtr TYPE ekbe-dmbtr,   "采购金额
    shkzg TYPE ekbe-shkzg,   "借贷标识
  END OF ty_po,

  BEGIN OF ty_ma,
    matnr TYPE makt-matnr,
    bwkey TYPE mbew-bwkey,
    maktx TYPE makt-maktx,
    stprs TYPE mbew-stprs,
    vmstp TYPE mbew-vmstp,
    peinh TYPE mbew-peinh,
    vmpei TYPE mbew-vmpei,
    kzkfg TYPE mara-kzkfg,
  END OF ty_ma,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,   "供应商
    name1 TYPE lfa1-name1,   "供应商名称
  END OF ty_lfa1.


*----------------------------------------------------------------------*
*       WORKAREA AND INNER TABLE                                       *
*----------------------------------------------------------------------*
DATA: wa_alv TYPE ty_alv,
      it_alv TYPE TABLE OF ty_alv.


DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_layout   TYPE slis_layout_alv.
DATA: wa_fieldcat TYPE slis_fieldcat_alv.

*----------------------------------------------------------------------*
*       SELECT-OPTION                                                  *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-001.
PARAMETERS: s_bukrs TYPE ekko-bukrs OBLIGATORY.
SELECT-OPTIONS: s_gjahr FOR ekbe-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT sy-datum+0(4).
SELECT-OPTIONS: s_budat FOR ekbe-budat+04(2) NO-EXTENSION NO INTERVALS  DEFAULT sy-datum+4(2).
SELECT-OPTIONS: s_matnr FOR ekbe-matnr.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr.

SELECTION-SCREEN END OF BLOCK blk.

*----------------------------------------------------------------------*
*       MAIN                                                           *
*----------------------------------------------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN.
PERFORM frm_auth_check.
START-OF-SELECTION.
*      TODO
  PERFORM frm_get_data.

END-OF-SELECTION.
  IF it_alv IS INITIAL.
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    PERFORM frm_init_layout.
    PERFORM frm_init_fieldcat.
    PERFORM frm_display_alv.
  ENDIF.



*----------------------------------------------------------------------*
*       FORM DEFINE                                                    *
*----------------------------------------------------------------------*
FORM frm_auth_check.
   AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
            ID 'BUKRS' FIELD s_bukrs
*            ID 'ACTVT' FIELD '__________'
            .
   IF sy-subrc <> 0.
     MESSAGE e054(VFRR) WITH s_bukrs.
   ENDIF.
endform.

FORM frm_get_data.
  DATA:
    lt_ekpo    TYPE STANDARD TABLE OF ty_ekpo,
    lw_ekpo    TYPE ty_ekpo,
    lt_ekbe    TYPE STANDARD TABLE OF ty_ekbe,
    lw_ekbe    TYPE ty_ekbe,
    lt_r_budat TYPE RANGE OF budat,
    lw_r_budat LIKE LINE OF lt_r_budat,
    l_datum    TYPE sy-datum,
    l_months   TYPE vtbbewe-atage,
    lw_alv     TYPE ty_alv,
    lt_ma      TYPE STANDARD TABLE OF ty_ma,
    lw_ma      TYPE ty_ma,
    lt_lfa1    TYPE STANDARD TABLE OF ty_lfa1,
    lw_lfa1    TYPE ty_lfa1,
    lt_po      TYPE STANDARD TABLE OF ty_po,
    lw_po      TYPE ty_po,
    lw_po_temp TYPE ty_po,
    lt_po_temp TYPE STANDARD TABLE OF ty_po.

* 期间检查 按期间组合出过账日期
  IF s_budat[] IS NOT INITIAL.
    READ TABLE s_budat INDEX 1.
    READ TABLE s_gjahr INDEX 1.
    CONCATENATE s_gjahr-low s_budat-low '01' INTO l_datum.
    l_datum = l_datum - 1.
    IF l_datum > sy-datum.
      MESSAGE s001(00) WITH '不允许输入未来期间' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
*    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
*      EXPORTING
*        I_DATE_FROM          = L_DATUM
*        I_DATE_TO            = SY-DATUM
*      IMPORTING
*        E_MONTHS             = L_MONTHS
*              .
*    IF L_MONTHS > 2.
*      MESSAGE S001(00) WITH '只能输入当前期间或上一期间' DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*   赋值
    lw_r_budat-sign   = 'I'.
    lw_r_budat-option = 'BT'.
    lw_r_budat-low    = l_datum + 1.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = lw_r_budat-low
      IMPORTING
        e_date = lw_r_budat-high.
    APPEND lw_r_budat TO lt_r_budat.
  ENDIF.
* 取采购凭证数据
*  SELECT A~LIFNR  "供应商
*         D~MATNR  "物料号
*         B~BUKRS  "公司代码
*         D~BPMNG  "采购数量
*         D~GJAHR  "会计年度
*         D~DMBTR  "采购金额
*         D~SHKZG  "借贷标识
*    INTO TABLE LT_PO
*    FROM EKKO AS A
*   INNER JOIN EKPO AS B
*      ON A~EBELN = B~EBELN
*   INNER JOIN EKET AS C
*      ON B~EBELN = C~EBELN
*     AND B~EBELP = C~EBELP
*   INNER JOIN EKBE AS D
*      ON B~EBELN = D~EBELN
*     AND B~EBELP = D~EBELP
*   WHERE B~BUKRS IN S_BUKRS    "公司代码
*     AND D~GJAHR IN S_GJAHR    "会计年度
*     AND D~MATNR IN S_MATNR    "物料号
*     AND A~LIFNR IN S_LIFNR    "供应商
*     AND D~BUDAT IN LT_R_BUDAT "过账日期
*     AND B~KNTTP NOT IN ('A','K','9')
*     AND D~BWART NOT IN ('103','104')
*     AND D~BEWTP =  'E'
*       .
  SELECT b~ebeln
         b~ebelp
*         b~werks
         a~lifnr
         a~bukrs
    INTO TABLE lt_ekpo
    FROM ekko AS a
   INNER JOIN ekpo AS b
      ON a~ebeln = b~ebeln
   WHERE a~lifnr IN s_lifnr    "供应商
     AND b~bukrs = s_bukrs    "公司代码
     AND b~matnr IN s_matnr    "物料号
      .

  IF lt_ekpo IS NOT INITIAL.
    SELECT ebeln
           ebelp
           zekkn
           vgabe
           gjahr
           belnr
           buzei
           matnr  "物料号
           werks  "工厂
           bpmng  "采购数量
           dmbtr  "采购金额
           shkzg  "借贷标识
           budat
           bwart
           bewtp
      INTO TABLE lt_ekbe
      FROM ekbe
       FOR ALL ENTRIES IN lt_ekpo
     WHERE ebeln = lt_ekpo-ebeln
      AND ebelp = lt_ekpo-ebelp
      AND gjahr IN s_gjahr
      AND matnr IN s_matnr
      AND budat IN lt_r_budat
      AND ( bewtp = 'E' )
      .
*       OR bewtp = 'Q'

*    DELETE LT_EKBE WHERE GJAHR NOT IN S_GJAHR    "会计年度
*                      OR MATNR NOT IN S_MATNR    "物料号
*                      OR BUDAT NOT IN LT_R_BUDAT "过账日期
*                      OR BWART = '103'
*                      OR BWART = '104'
*                      OR BEWTP <>  'E'
    .
  ENDIF.
  SORT lt_ekpo BY ebeln ebelp.
  LOOP AT lt_ekbe INTO lw_ekbe.
    CLEAR:
      lw_po,
      lw_ekpo.
    READ TABLE lt_ekpo INTO lw_ekpo
                    WITH KEY ebeln = lw_ekbe-ebeln
                             ebelp = lw_ekbe-ebelp
                      BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING lw_ekbe TO lw_po.
      MOVE-CORRESPONDING lw_ekpo TO lw_po.
      APPEND lw_po TO lt_po.
    ENDIF.
  ENDLOOP.
* 取供应商文本
*  LT_PO_TEMP = LT_PO.
*  DELETE LT_PO_TEMP WHERE LIFNR IS INITIAL.
*  SORT LT_PO_TEMP BY LIFNR.
*  DELETE ADJACENT DUPLICATES FROM LT_PO_TEMP COMPARING LIFNR.
*  IF LT_PO_TEMP IS NOT INITIAL.
*    SELECT LIFNR
*           NAME1
*      INTO TABLE LT_LFA1
*      FROM LFA1
*       FOR ALL ENTRIES IN LT_PO_TEMP
*     WHERE LIFNR = LT_PO_TEMP-LIFNR
*       AND SPRAS = sy-langu
*      .
*  ENDIF.
  IF lt_ekpo[] IS NOT INITIAL.
    SELECT lifnr
           name1
      INTO TABLE lt_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN lt_ekpo
     WHERE lifnr = lt_ekpo-lifnr
       AND spras = sy-langu
      .
  ENDIF.



* 取物料描述和标准价格
  lt_po_temp = lt_po.
  DELETE lt_po_temp WHERE matnr IS INITIAL.
  SORT lt_po_temp BY lifnr matnr bukrs werks.
  DELETE ADJACENT DUPLICATES FROM lt_po_temp COMPARING lifnr matnr bukrs werks.
  IF lt_po_temp IS NOT INITIAL.

    SELECT a~matnr  "物料编码
           b~bwkey  "
           a~maktx  "物料描述
           b~stprs  "本期标准价
           b~vmstp  "上期标准价
           b~peinh
           b~vmpei
           c~kzkfg
      INTO TABLE lt_ma
      FROM makt AS a
     INNER JOIN mbew AS b
        ON a~matnr = b~matnr
     INNER JOIN mara AS c
        ON a~matnr = c~matnr
       FOR ALL ENTRIES IN lt_po_temp
     WHERE
           a~matnr = lt_po_temp-matnr
       and b~BWKEY = lt_po_temp-werks
       AND a~spras = sy-langu.

*    IF S_BUKRS-LOW <> '2010'.
*      DELETE LT_MA WHERE BWKEY = '2010'.
*      SORT LT_MA BY MATNR ASCENDING STPRS DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM LT_MA COMPARING MATNR.
*    ELSE.
*      DELETE LT_MA WHERE BWKEY <> '2010'.
*    ENDIF.
  ENDIF.

* 采购金额
* 数据整合
  SORT lt_lfa1 BY lifnr.
  SORT lt_ma   BY matnr bwkey.
  SORT lt_po   BY lifnr matnr bukrs werks.
  REFRESH it_alv.
  CLEAR lw_alv.
  LOOP AT lt_po INTO lw_po.

    IF lw_po-shkzg = 'S'. "借方
      lw_alv-bpmng = lw_alv-bpmng + lw_po-bpmng. "数量
      lw_alv-dmbtr = lw_alv-dmbtr + lw_po-dmbtr. "金额
    ELSEIF lw_po-shkzg = 'H'.
      lw_alv-bpmng = lw_alv-bpmng - lw_po-bpmng. "数量
      lw_alv-dmbtr = lw_alv-dmbtr - lw_po-dmbtr. "金额
    ENDIF.

    lw_po_temp = lw_po.
    AT END OF matnr.
      lw_po = lw_po_temp.
      CLEAR:
        lw_lfa1,
        lw_ma.
*     读取供应商文本
      READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = lw_po-lifnr
                                        BINARY SEARCH.
*     读取物料描述及标准价
      READ TABLE lt_ma INTO lw_ma WITH KEY matnr = lw_po-matnr
                                    BINARY SEARCH.
      lw_alv-kzkfg           = lw_ma-kzkfg.  "是否可配置
      lw_alv-bukrs           = lw_po-bukrs.  "公司代码
      lw_alv-gjahr           = lw_po-gjahr.  "会计年度
      lw_alv-lifnr           = lw_po-lifnr.  "供应商
      lw_alv-name1           = lw_lfa1-name1."供应商名称
      lw_alv-matnr           = lw_po-matnr.  "物料代码
      lw_alv-maktx           = lw_ma-maktx.  "物料描述
      IF lw_ma-peinh <> 0.
        lw_alv-stprs           = lw_ma-stprs / lw_ma-peinh.  "本期标准价
      ENDIF.
      IF lw_ma-peinh <> 0.
        lw_alv-vmstp           = lw_ma-vmstp / lw_ma-vmpei.  "上期标准价
      ENDIF.

      IF lw_alv-bpmng <> 0.
        lw_alv-average_price   = lw_alv-dmbtr
                               / lw_alv-bpmng. "本期采购均价
      ENDIF.
*      IF S_BUDAT-LOW = SY-DATUM+4(2).
*        LW_ALV-DIFFERENT_PRICE = LW_ALV-AVERAGE_PRICE - LW_ALV-STPRS. "采购均价与标准价差异
*      ELSE.
*        LW_ALV-DIFFERENT_PRICE = LW_ALV-AVERAGE_PRICE - LW_ALV-VMSTP. "采购均价与标准价差异
*      ENDIF.
      lw_alv-different_price = lw_alv-average_price - lw_alv-stprs. "采购均价与标准价差异

      lw_alv-different_total = lw_alv-different_price * lw_alv-bpmng. "采购差异总额
      DATA:l_dmbtr TYPE dmbtr.
      CLEAR l_DMBTR.
      IF lw_alv-stprs <> 0.
        IF s_budat-low = sy-datum+4(2).
          l_dmbtr = ( lw_alv-average_price - lw_alv-stprs ) * 100 / lw_alv-stprs.
        ELSE.
          l_dmbtr = ( lw_alv-average_price - lw_alv-vmstp ) * 100 / lw_alv-stprs.
        ENDIF.
      ENDIF.
      lw_alv-different_rate = l_dmbtr.
      CONCATENATE lw_alv-different_rate '%' INTO lw_alv-different_rate.
      APPEND lw_alv TO it_alv.
      CLEAR lw_alv.
    ENDAT.
  ENDLOOP.

ENDFORM.

FORM frm_init_layout .
  CLEAR wa_layout.
*  WA_LAYOUT-F2CODE = '&ETA'.

  wa_layout-zebra             = 'X'.
  wa_layout-detail_popup      = 'X'.
  wa_layout-colwidth_optimize = 'X'.
ENDFORM.

FORM frm_init_fieldcat.

  DATA: l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.
*    L_COLPOS 'KZKFG' '是否可配置' ' ' ' ' ' ' ' ' ' ' 'C',
  CHECK it_fieldcat IS INITIAL.
  macro_fill_fcat:
    l_colpos 'BUKRS'            TEXT-001  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'GJAHR'            TEXT-002  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'LIFNR'            TEXT-003  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'NAME1'            TEXT-004  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'MATNR'            TEXT-005  ' ' 'X' ' ' 'MARA' 'MATNR' ' ',
    l_colpos 'MAKTX'            TEXT-006  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'STPRS'            TEXT-007  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'VMSTP'            TEXT-008  ' ' ' ' ' ' ' ' ' ' '',
    l_colpos 'BPMNG'            TEXT-009  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'DMBTR'            TEXT-010  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'AVERAGE_PRICE'    TEXT-011  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'DIFFERENT_PRICE'  TEXT-012  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'DIFFERENT_TOTAL'  TEXT-013  ' ' ' ' ' ' ' ' ' ' ' ',
    l_colpos 'DIFFERENT_RATE'   TEXT-014  ' ' ' ' ' ' ' ' ' ' ' '.
ENDFORM.

FORM frm_display_alv.

  SORT it_alv BY lifnr.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = wa_layout
      it_fieldcat             = it_fieldcat
      i_save                  = 'A'
*     I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      i_callback_user_command = 'USER_COMMAND'
    TABLES
      t_outtab                = it_alv
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STAT_ALV'.
ENDFORM.                    "SET_PF_STATUS

FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

*  RS_SELFIELD-REFRESH = 'X'.  "自动刷新

  CASE r_ucomm.
    WHEN '&IC1'.
      IF rs_selfield-value IS INITIAL.
        r_ucomm = '&ETA'.
      ENDIF.
  ENDCASE.
ENDFORM.
