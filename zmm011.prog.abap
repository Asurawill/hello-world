*&---------------------------------------------------------------------*
*& Report  ZMM011
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Date Created : 2015/2/3                                            *
*& Created By   : 汉得-唐博                                            *
*& Description  :外发加工明细表                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zmm011.
TABLES: ekko,ekpo,ekbe.

SELECT-OPTIONS: s_ekorg FOR ekko-ekorg, "采购组织
                s_ekgrp FOR ekko-ekgrp, "采购组
                s_lifnr FOR ekko-lifnr, "供应商
                s_ebeln FOR ekko-ebeln, "采购订单编号
                s_werks FOR ekpo-werks, "工厂
                s_matnr FOR ekpo-matnr, "物料编码
                s_bedat FOR ekko-bedat. "日期

DATA:
  BEGIN OF gt_alv OCCURS 1,
    bedat     TYPE ekko-bedat, "日期
    ebeln     TYPE ekko-ebeln, "订单号
    ebelp     TYPE ekpo-ebelp, "行项目号
    werks     TYPE ekpo-werks, "工厂
    lifnr     TYPE ekko-lifnr, "供应商
    name1     TYPE lfa1-name1, "供应商描述
    matnr     TYPE ekpo-matnr, "母件物料编号
    maktx     TYPE makt-maktx, "母件物料描述
    matnr2    TYPE ekpo-matnr, "子件物料编号
    maktx2    TYPE makt-maktx, "子件物料描述
    bprme     TYPE ekpo-bprme, "采购订单单位
    menge     TYPE ekpo-menge, "母件订单数量
    mwskz     TYPE t007a-mwskz, "税率
    menge_yjh TYPE ekbe-menge, "已交货数量
    menge_wjh TYPE ekbe-menge, "未交货数量
    menge_xq  TYPE resb-erfmg, "子件需求数量
    menge_yfl TYPE mseg-menge, "子件已发料数量
    menge_wj  TYPE ekbe-menge, "子件未结数量
    menge_yth TYPE ekbe-menge, "子件已退回数量
    menge_cl  TYPE ekbe-menge, "超领数量
    stprs     TYPE mbew-stprs, "超领单价
    amount_cl TYPE  tslvt12,  " MSEG-DMBTR, "超领金额 DES:字段长度扩大 by it02 20161229
    waers     TYPE mseg-waers, "货币码
    meins     TYPE mseg-meins, "数量单位
    matnr_k   TYPE resb-matnr, "子件物料
  END OF gt_alv,
  gt_ekbe     TYPE TABLE OF ekbe WITH HEADER LINE,
  gt_ekbe_sum TYPE TABLE OF ekbe WITH HEADER LINE,
  BEGIN OF gt_mseg OCCURS 1,
    ebeln_k TYPE mseg-ebeln,
    ebelp_k TYPE mseg-ebelp,
    matnr_k TYPE mseg-matnr,
*        RSPOS_K TYPE MSEG-RSPOS.
    mblnr   TYPE mseg-mblnr,
    mjahr   TYPE mseg-mjahr,
    zeile   TYPE mseg-zeile,
    ebeln   TYPE mseg-ebeln,
    ebelp   TYPE mseg-ebelp,
    matnr   TYPE mseg-matnr,
    menge   TYPE mseg-menge,
    shkzg   TYPE mseg-shkzg.
*    include STRUCTURE mseg.
DATA: END OF gt_mseg,
*      GT_MSEG TYPE TABLE OF MSEG WITH HEADER LINE,
gt_mseg_sum     LIKE TABLE OF gt_mseg WITH HEADER LINE,
  gt_mseg_542     LIKE TABLE OF gt_mseg WITH HEADER LINE,
  gt_mseg_sum_542 LIKE TABLE OF gt_mseg WITH HEADER LINE.

START-OF-SELECTION.

  "获取记录
  SELECT
    a~bedat "日期
    a~ebeln "订单号
    b~ebelp "行项目号
    b~werks "工厂
    a~lifnr "供应商
    e~name1 "供应商描述
    b~matnr "物料编号
    c~matnr AS matnr2"物料编号2
    d~maktx AS maktx2"物料描述
    b~bprme "采购订单单位
    b~menge "母件订单数量
    b~mwskz "税率
    c~bdmng AS menge_xq "子件需求数量
*    C~ENMNG AS MENGE_YFL "子件已发料数量
    a~waers "货币码
    b~meins "数量单位
    c~matnr AS matnr_k "子件物料
    FROM ekko AS a INNER JOIN ekpo AS b ON a~ebeln EQ b~ebeln
    INNER JOIN resb AS c ON b~ebeln EQ c~ebeln AND b~ebelp EQ c~ebelp
    LEFT JOIN makt AS d ON c~matnr EQ d~matnr AND d~spras EQ sy-langu
    LEFT JOIN lfa1 AS e ON a~lifnr EQ e~lifnr
*    LEFT JOIN T007A AS F ON F~KALSM EQ 'TAXCN' AND A~MWSKZ EQ F~MWSKZ
    INTO CORRESPONDING FIELDS OF TABLE gt_alv
    WHERE a~ekorg IN s_ekorg
    AND a~bedat IN s_bedat
    AND a~ekgrp IN s_ekgrp
    AND a~lifnr IN s_lifnr
    AND a~ebeln IN s_ebeln
    AND b~werks IN s_werks
    AND b~matnr IN s_matnr.

  IF gt_alv[] IS NOT INITIAL.
*    "获取已交货数量MENGE_YJH
    CLEAR: gt_ekbe[], gt_ekbe_sum[].

    SELECT *
      FROM ekbe
      INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
      FOR ALL ENTRIES IN gt_alv
      WHERE ebeln = gt_alv-ebeln
      AND ebelp = gt_alv-ebelp
      AND vgabe = '1'.

    LOOP AT gt_ekbe WHERE shkzg EQ 'H'.
      MULTIPLY gt_ekbe-menge BY -1.
      MODIFY gt_ekbe.
    ENDLOOP.

    SORT gt_ekbe BY ebeln ebelp.
    LOOP AT gt_ekbe.
      AT END OF ebelp.
        SUM.
        APPEND gt_ekbe TO gt_ekbe_sum.
      ENDAT.
    ENDLOOP.

*    "获取子件已发料数量MENGE_YFL
    CLEAR: gt_mseg[], gt_mseg_sum[], gt_mseg_542[], gt_mseg_sum_542[].

    SELECT *
      FROM mseg
      INTO CORRESPONDING FIELDS OF TABLE gt_mseg
      FOR ALL ENTRIES IN gt_alv
      WHERE ebeln = gt_alv-ebeln
      AND ebelp = gt_alv-ebelp
      AND bwart IN ('541', '542')
      AND sobkz EQ 'O'.

    LOOP AT gt_mseg.
      gt_mseg-ebeln_k = gt_mseg-ebeln.
      gt_mseg-ebelp_k = gt_mseg-ebelp.
      gt_mseg-matnr_k = gt_mseg-matnr.
      IF gt_mseg-shkzg EQ 'H'.
        MULTIPLY gt_mseg-menge BY -1.
      ENDIF.
      MODIFY gt_mseg.
    ENDLOOP.

    SORT gt_mseg BY ebeln_k ebelp_k matnr_k.
    LOOP AT gt_mseg.
*      AT END OF RSPOS_K.
      AT END OF matnr_k.
        SUM.
        APPEND gt_mseg TO gt_mseg_sum.
      ENDAT.
    ENDLOOP.

    SELECT *
      FROM mseg
      INTO CORRESPONDING FIELDS OF TABLE gt_mseg_542
      FOR ALL ENTRIES IN gt_alv
      WHERE ebeln = gt_alv-ebeln
      AND ebelp = gt_alv-ebelp
      AND bwart EQ '542'
      AND sobkz EQ 'O'.

    LOOP AT gt_mseg_542.
      gt_mseg_542-ebeln_k = gt_mseg_542-ebeln.
      gt_mseg_542-ebelp_k = gt_mseg_542-ebelp.
      gt_mseg_542-matnr_k = gt_mseg_542-matnr.
      IF gt_mseg_542-shkzg EQ 'S'."已退回数量，反向显示
        MULTIPLY gt_mseg_542-menge BY -1.
      ENDIF.
      MODIFY gt_mseg_542.
    ENDLOOP.

    SORT gt_mseg_542 BY ebeln_k ebelp_k matnr_k.
    LOOP AT gt_mseg_542.
*      AT END OF RSPOS_K.
      AT END OF matnr_k.
        SUM.
        APPEND gt_mseg_542 TO gt_mseg_sum_542.
      ENDAT.
    ENDLOOP.
  ENDIF.

  LOOP AT gt_alv.
    "物料的成本价
    SELECT SINGLE stprs FROM mbew INTO gt_alv-stprs WHERE matnr = gt_alv-matnr AND bwkey = gt_alv-werks.
    "母件物料描述
    SELECT SINGLE maktx FROM makt INTO gt_alv-maktx WHERE matnr = gt_alv-matnr AND spras EQ sy-langu.

    "已交货数量
    READ TABLE gt_ekbe_sum WITH KEY ebeln = gt_alv-ebeln ebelp = gt_alv-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_alv-menge_yjh = gt_ekbe_sum-menge.
    ENDIF.
    "子件已发料数量
*    READ TABLE GT_MSEG_SUM WITH KEY EBELN_K = GT_ALV-EBELN EBELP_K = GT_ALV-EBELP  RSNUM_K = GT_ALV-RSNUM RSPOS_K = GT_ALV-RSPOS BINARY SEARCH.
    READ TABLE gt_mseg_sum WITH KEY ebeln_k = gt_alv-ebeln ebelp_k = gt_alv-ebelp matnr_k = gt_alv-matnr_k BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_alv-menge_yfl = gt_mseg_sum-menge.
    ENDIF.
    "已退回数量
    READ TABLE gt_mseg_sum_542 WITH KEY ebeln_k = gt_alv-ebeln ebelp_k = gt_alv-ebelp matnr_k = gt_alv-matnr_k BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_alv-menge_yth = gt_mseg_sum_542-menge.
    ENDIF.
    "未交货数量 = 订单数量 - 已交货数量
    gt_alv-menge_wjh = gt_alv-menge - gt_alv-menge_yjh.
    "子件未结数量 = 子件需求数量 - 子件已发料数量
    gt_alv-menge_wj = gt_alv-menge_xq - gt_alv-menge_yfl.
    "超领数量 = 实际领用数量 - 子件需求数量
    gt_alv-menge_cl = gt_alv-menge_yfl - gt_alv-menge_xq.
    IF gt_alv-menge_wj < 0.
      gt_alv-menge_wj = 0.
    ENDIF.
    IF gt_alv-menge_cl < 0.
      gt_alv-menge_cl = 0.
    ENDIF.
    "成本价加总价格 = 物料的成本价 * 超领数量
    gt_alv-amount_cl = gt_alv-stprs * gt_alv-menge_cl.
    IF gt_alv-menge_cl IS INITIAL.
      gt_alv-stprs = 0.
    ENDIF.
    MODIFY gt_alv.
  ENDLOOP.

  DATA is_layout TYPE slis_layout_alv.
  DATA it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  CLEAR: is_layout,it_fieldcat[].
  is_layout-colwidth_optimize = 'X'.

  PERFORM append_fieldcat USING:
        'BEDAT' '日期' '' '' '' '',
        'EBELN' '订单号' '' '' '' '',
        'LIFNR' '供应商' '' '' '' '',
        'NAME1' '供应商描述' '' '' '' '',
        'MATNR' '母件物料编号' 'MARA' 'MATNR' '' '',
        'MAKTX' '母件物料描述' '' '' '' '',
        'MATNR2' '子件物料编号' 'MARA' 'MATNR' '' '',
        'MAKTX2' '子件物料描述' '' '' '' '',
        'BPRME' '采购订单单位' '' '' '' '',
        'MENGE' '母件订单数量' '' '' '' '',
        'MWSKZ' '税率' '' '' '' '',
        'MENGE_YJH' '已交货数量' '' '' '' 'MEINS',
        'MENGE_WJH' '未交货数量' '' '' '' 'MEINS',
        'MENGE_XQ' '子件需求数量' '' '' '' 'MEINS',
        'MENGE_YFL' '子件已发料数量' '' '' '' 'MEINS',
        'MENGE_WJ' '子件未结数量' '' '' '' 'MEINS',
        'MENGE_YTH' '子件已退回数量' '' '' '' 'MEINS',
        'MENGE_CL' '超领数量' '' '' '' 'MEINS',
        'MEINS' '数量单位' '' '' '' '',
        'STPRS' '超领单价' '' '' 'WAERS' '',
        'AMOUNT_CL' '超领金额' '' '' 'WAERS' '',
        'WAERS' '货币码' '' '' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = is_layout
      it_fieldcat        = it_fieldcat[]
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = gt_alv[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
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
FORM append_fieldcat  USING name
                            text
                            ref_tabname
                            ref_fieldname
                            cfieldname
                            qfieldname.
  it_fieldcat-fieldname = name.
  it_fieldcat-seltext_l    =
  it_fieldcat-seltext_m    =
  it_fieldcat-seltext_s    =
  it_fieldcat-reptext_ddic = text.
  it_fieldcat-ref_tabname = ref_tabname.
  it_fieldcat-ref_fieldname = ref_fieldname.
  it_fieldcat-cfieldname = cfieldname.
  it_fieldcat-qfieldname = qfieldname.
  APPEND it_fieldcat.
  CLEAR it_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT
