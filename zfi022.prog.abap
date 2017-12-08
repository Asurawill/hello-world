*&---------------------------------------------------------------------*
*& Report  ZFI022
*& 项目明细账
*&---------------------------------------------------------------------*
*&
*& 作者：汉得 唐博
*& 开发日期：2015-2-28
** 修改日期   开发人员  请求号           描述
" 20170829   IT02     ED1K906992      添加过账日期
*&---------------------------------------------------------------------*
REPORT ZFI022.

TABLES: T001,BSEG,BKPF,ZFI022, VBAK.

DATA:

  BEGIN OF T_ZFI022_HKONT OCCURS 1,
    BUKRS TYPE ZFI022-BUKRS,
    ZSO TYPE ZFI022-ZSO,
    WAERS TYPE ZFI022-WAERS,
    HKONT TYPE ZFI022-HKONT,
    TXT50 TYPE ZFI022_MX-TXT50,
    ITEMS TYPE TABLE OF ZFI022_MX,
    ZDMBTR_SUM TYPE ZFI022-DMBTR_S,
    ZDMBTR_SUM_S TYPE ZFI022-DMBTR_S,
    ZDMBTR_SUM_H TYPE ZFI022-DMBTR_S,
    DMBTR_S TYPE ZFI022-DMBTR_S,
    DMBTR_H TYPE ZFI022-DMBTR_S,
  END OF T_ZFI022_HKONT,

  BEGIN OF T_ZFI022_SO OCCURS 1,
    BUKRS TYPE ZFI022-BUKRS,
    ZSO TYPE ZFI022-ZSO,
    WAERS TYPE ZFI022-WAERS,
    ITEMS LIKE TABLE OF T_ZFI022_HKONT,
    ZDMBTR_SUM TYPE ZFI022-DMBTR_S,
    ZDMBTR_SUM_S TYPE ZFI022-DMBTR_S,
    ZDMBTR_SUM_H TYPE ZFI022-DMBTR_S,
    DMBTR_S TYPE ZFI022-DMBTR_S,
    DMBTR_H TYPE ZFI022-DMBTR_S,
  END OF T_ZFI022_SO,

  BEGIN OF T_ZFI022 OCCURS 1,
    BUKRS TYPE ZFI022-BUKRS,
    ZSO TYPE ZFI022-ZSO,
    WAERS TYPE ZFI022-WAERS,
    HKONT TYPE ZFI022-HKONT,
    TXT50 TYPE ZFI022_MX-TXT50,
    DMBTR_S TYPE ZFI022-DMBTR_S,
    DMBTR_H TYPE ZFI022-DMBTR_S,
  END OF T_ZFI022.

*DATA T_ZFI022 LIKE TABLE OF T_ZFI022_HKONT WITH HEADER LINE.
DATA T_ZFI022_MX TYPE TABLE OF ZFI022_MX WITH HEADER LINE.
DATA T_ZFI022_ALL TYPE TABLE OF ZFI022_MX WITH HEADER LINE.

DATA T_ZBSEG_VBELN TYPE TABLE OF ZFI_BSEG_VBELN WITH HEADER LINE.

DATA: BEGIN OF T_ALV OCCURS 1.
    INCLUDE STRUCTURE ZFI022_MX.
DATA: ZDMBTR_SUM TYPE ZFI022_MX-DMBTR_S,
      DIRECTION TYPE CHAR2,
      ZSO_TEXT TYPE CHAR30,
      CPLB TYPE CHAR30,      "产品类别
      bldat_2 type bkpf-bldat, "凭证日期
      budat type bkpf-budat, "过账日期
      COLOR(4) TYPE C,
      END OF T_ALV.

DATA: BEGIN OF T_VBELN OCCURS 1,
        VBELN TYPE VBAK-VBELN,
      END OF T_VBELN.

DATA: BEGIN OF T_HKONT_MATNR OCCURS 1,
        LINE_ID TYPE ZFI_V12_001-HKONT,
        LTEXT TYPE ZFI_V12_001-LTEXT,
        HKONT TYPE ZFI_V12_003-HKONT2,
        MATNR TYPE ZFI_V12_003-MATNR_FROM,
      END OF T_HKONT_MATNR.


DATA:BEGIN OF T_PZ OCCURS 1,
       BUKRS TYPE BKPF-BUKRS,  "公司代码
       BELNR TYPE BKPF-BELNR,  "会计凭证编号
       GJAHR TYPE BKPF-GJAHR,   "会计年度
      END OF T_PZ.

*DATA:GT_PZ TYPE TABLE OF TY_PZ,
*     GS_PZ TYPE TY_PZ.

DATA:GT_BKPF TYPE TABLE OF BKPF,
     GS_BKPF TYPE BKPF.

RANGES: R_HKONT FOR BSEG-HKONT.
DATA: W_HKONT TYPE ZFI022_HKONT.

"公司代码
PARAMETERS P_BUKRS TYPE T001-BUKRS OBLIGATORY.
"项目编号
SELECT-OPTIONS S_VBELN FOR VBAK-VBELN OBLIGATORY.
"科目
SELECT-OPTIONS S_HKONT FOR BSEG-HKONT.
"查询月份
SELECT-OPTIONS S_MONAT FOR BKPF-MONAT NO-EXTENSION.
"查询年
PARAMETERS P_GJAHR TYPE BSEG-GJAHR.

START-OF-SELECTION.
"权限代码检查
 AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD P_BUKRS.
  IF SY-SUBRC <> 0.
   MESSAGE s899(mm) WITH '您没有公司代码' P_BUKRS '的权限' DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING.
   ENDIF.
IF P_GJAHR IS NOT INITIAL AND S_MONAT[] IS INITIAL."如果填了年没填月，则报错
  MESSAGE S005(ZFICO01) DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING.
ENDIF.

CLEAR: R_HKONT[], T_ZFI022[], T_ZFI022_SO[], T_ZBSEG_VBELN[], T_ZFI022_MX[], T_ZFI022_ALL[], T_ALV[], T_VBELN[], T_ZBSEG_VBELN[].

"设置科目range
SELECT * FROM ZFI022_HKONT INTO CORRESPONDING FIELDS OF W_HKONT.
  R_HKONT-SIGN = W_HKONT-SIGN.
  R_HKONT-OPTION = W_HKONT-OPTI.
  R_HKONT-LOW = W_HKONT-LOW.
  R_HKONT-HIGH = W_HKONT-HIGH.
  APPEND R_HKONT.
  CLEAR R_HKONT.
ENDSELECT.
"1 获取SO号对应的会计凭证
SELECT * FROM ZFI_BSEG_VBELN
  INTO CORRESPONDING FIELDS OF TABLE T_ZBSEG_VBELN
  WHERE VBEL2 IN S_VBELN
  AND BUKRS EQ P_BUKRS.
  SORT T_ZBSEG_VBELN BY BUKRS GJAHR BELNR BUZEI.

"2 获取明细数据
IF T_ZBSEG_VBELN[] IS NOT INITIAL.
  SELECT * FROM ZFI022_MX
    INTO CORRESPONDING FIELDS OF TABLE T_ZFI022_ALL
    FOR ALL ENTRIES IN T_ZBSEG_VBELN
    WHERE BUKRS EQ T_ZBSEG_VBELN-BUKRS
    AND GJAHR EQ T_ZBSEG_VBELN-GJAHR
    AND BELNR EQ T_ZBSEG_VBELN-BELNR
    AND BUZEI EQ T_ZBSEG_VBELN-BUZEI
    .
  LOOP AT T_ZFI022_ALL.
    READ TABLE T_ZBSEG_VBELN WITH KEY
    BUKRS = T_ZFI022_ALL-BUKRS
    GJAHR = T_ZFI022_ALL-GJAHR
    BELNR = T_ZFI022_ALL-BELNR
    BUZEI = T_ZFI022_ALL-BUZEI BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      T_ZFI022_ALL-ZSO = T_ZBSEG_VBELN-VBEL2.
      MODIFY T_ZFI022_ALL.
    ENDIF.
  ENDLOOP.
ENDIF.

**DEL BEGIN
*SELECT VBELN
*  FROM VBAK
*  INTO TABLE T_VBELN
*  WHERE VBELN IN S_VBELN.
*
*"添加前导0
*LOOP AT T_VBELN.
*  SHIFT T_VBELN RIGHT DELETING TRAILING SPACE.
*  OVERLAY T_VBELN WITH '0000000000'.
*  MODIFY T_VBELN.
*ENDLOOP.
**DEL END

**DEL BEGIN
*IF T_VBELN[] IS NOT INITIAL.
**DEL END
  IF P_GJAHR IS NOT INITIAL."取期初和明细
**DEL BEGIN
*    SELECT * FROM ZFI022 INTO CORRESPONDING FIELDS OF TABLE T_ZFI022
*      FOR ALL ENTRIES IN T_VBELN
*      WHERE BUKRS EQ P_BUKRS
*      AND ZSO EQ T_VBELN-VBELN
*      AND HKONT NOT BETWEEN '1001010101' AND '1121990101'
*      AND HKONT IN S_HKONT
*      AND ( ( GJAHR EQ P_GJAHR
*      AND MONAT LT S_MONAT-LOW )
*      OR GJAHR LT P_GJAHR )
*      .
*    "处理销售订单号
*    LOOP AT T_ZFI022.
*      SHIFT T_ZFI022-ZSO LEFT DELETING LEADING '0'.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT        = T_ZFI022-ZSO
*       IMPORTING
*         OUTPUT        = T_ZFI022-ZSO
*                      .
*      MODIFY T_ZFI022.
*    ENDLOOP.
*
*    SELECT * FROM ZFI022_MX INTO CORRESPONDING FIELDS OF TABLE T_ZFI022_MX
*      FOR ALL ENTRIES IN T_VBELN
*      WHERE BUKRS EQ P_BUKRS
*      AND ZSO EQ T_VBELN-VBELN
*      AND HKONT NOT BETWEEN '1001010101' AND '1121990101'
*      AND HKONT IN S_HKONT
*      AND MONAT IN S_MONAT
*      AND GJAHR EQ P_GJAHR
*    .
**DEL END
    LOOP AT T_ZFI022_ALL
      WHERE
*      HKONT NOT BETWEEN '1001010101' AND '1121990101'
      HKONT IN S_HKONT
      AND HKONT IN R_HKONT
      AND ( ( GJAHR EQ P_GJAHR
      AND MONAT LT S_MONAT-LOW )
      OR GJAHR LT P_GJAHR ).
      MOVE-CORRESPONDING T_ZFI022_ALL TO T_ZFI022.
      COLLECT T_ZFI022.
    ENDLOOP.
    LOOP AT T_ZFI022_ALL INTO T_ZFI022_MX
      WHERE
*      HKONT NOT BETWEEN '1001010101' AND '1121990101'
      HKONT IN S_HKONT
      AND HKONT IN R_HKONT
      AND MONAT IN S_MONAT
      AND GJAHR EQ P_GJAHR.
      APPEND T_ZFI022_MX.
    ENDLOOP.
  ELSE."取明细
**DEL BEGIN
*    SELECT * FROM ZFI022_MX INTO CORRESPONDING FIELDS OF TABLE T_ZFI022_MX
*      FOR ALL ENTRIES IN T_VBELN
*      WHERE BUKRS EQ P_BUKRS
*      AND ZSO EQ T_VBELN-VBELN
*      AND HKONT NOT BETWEEN '1001010101' AND '1121990101'
*      AND HKONT IN S_HKONT
*      .
**DEL END
    LOOP AT T_ZFI022_ALL INTO T_ZFI022_MX
      WHERE
*      HKONT NOT BETWEEN '1001010101' AND '1121990101'
      HKONT IN S_HKONT
      AND HKONT IN R_HKONT.
      APPEND T_ZFI022_MX.
    ENDLOOP.
  ENDIF.
  SORT T_ZFI022_MX BY BUKRS ZSO WAERS HKONT BLDAT.
**DEL BEGIN
*ENDIF.
**DEL END

**DEL BEGIN
*"处理销售订单号
*LOOP AT T_ZFI022_MX.
*  SHIFT T_ZFI022_MX-ZSO LEFT DELETING LEADING '0'.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT        = T_ZFI022_MX-ZSO
*   IMPORTING
*     OUTPUT        = T_ZFI022_MX-ZSO
*                  .
*  MODIFY T_ZFI022_MX.
*ENDLOOP.
**DEL END

SORT T_ZFI022 BY BUKRS ZSO WAERS HKONT TXT50.

"添加没有期初的明细记录
LOOP AT T_ZFI022_MX.
  READ TABLE T_ZFI022 WITH KEY BUKRS = T_ZFI022_MX-BUKRS ZSO = T_ZFI022_MX-ZSO
    WAERS = T_ZFI022_MX-WAERS HKONT = T_ZFI022_MX-HKONT BINARY SEARCH.
  IF SY-SUBRC NE 0.
    MOVE-CORRESPONDING T_ZFI022_MX TO T_ZFI022.
    CLEAR: T_ZFI022-DMBTR_S, T_ZFI022-DMBTR_H.
    APPEND T_ZFI022.
    SORT T_ZFI022 BY BUKRS ZSO WAERS HKONT.
  ENDIF.
ENDLOOP.

*"获取项目期初数据
LOOP AT T_ZFI022.
  AT END OF WAERS.
    SUM.
    MOVE-CORRESPONDING T_ZFI022 TO T_ZFI022_SO.
*    T_ZFI022_SO-ZDMBTR_SUM_S = T_ZFI022_SO-DMBTR_S.
*    T_ZFI022_SO-ZDMBTR_SUM_H = T_ZFI022_SO-DMBTR_H.
    T_ZFI022_SO-ZDMBTR_SUM = T_ZFI022_SO-DMBTR_S - T_ZFI022_SO-DMBTR_H.
    CLEAR: T_ZFI022_SO-DMBTR_S, T_ZFI022_SO-DMBTR_H.
    APPEND T_ZFI022_SO.
  ENDAT.
ENDLOOP.

"获取项目科目期初数据
LOOP AT T_ZFI022.
  AT END OF TXT50.
    SUM.
    MOVE-CORRESPONDING T_ZFI022 TO T_ZFI022_HKONT.
*    T_ZFI022_HKONT-ZDMBTR_SUM_S = T_ZFI022_HKONT-DMBTR_S.
*    T_ZFI022_HKONT-ZDMBTR_SUM_H = T_ZFI022_HKONT-DMBTR_H.
    T_ZFI022_HKONT-ZDMBTR_SUM = T_ZFI022_HKONT-DMBTR_S - T_ZFI022_HKONT-DMBTR_H.
    CLEAR: T_ZFI022_HKONT-DMBTR_S, T_ZFI022_HKONT-DMBTR_H.
    APPEND T_ZFI022_HKONT.
  ENDAT.
ENDLOOP.

"添加明细数据
LOOP AT T_ZFI022_SO.
  LOOP AT T_ZFI022_HKONT
    WHERE BUKRS = T_ZFI022_SO-BUKRS
    AND ZSO = T_ZFI022_SO-ZSO
*    AND SHKZG = T_ZFI022_SO-SHKZG
    AND WAERS = T_ZFI022_SO-WAERS.
    "添加期初行
    MOVE-CORRESPONDING T_ZFI022_HKONT TO T_ALV.
    T_ALV-BKTXT = '期初余额'.
    T_ALV-GJAHR = P_GJAHR.
    T_ALV-MONAT = S_MONAT-LOW.
    APPEND T_ALV.
    CLEAR T_ALV.
    LOOP AT T_ZFI022_MX
      WHERE BUKRS = T_ZFI022_HKONT-BUKRS
      AND ZSO = T_ZFI022_HKONT-ZSO
*      AND SHKZG = T_ZFI022_HKONT-SHKZG
      AND WAERS = T_ZFI022_HKONT-WAERS
      AND HKONT = T_ZFI022_HKONT-HKONT.
      ADD T_ZFI022_MX-DMBTR_S TO T_ZFI022_HKONT-ZDMBTR_SUM."累计项目/科目余额
      ADD T_ZFI022_MX-DMBTR_S TO T_ZFI022_HKONT-ZDMBTR_SUM_S."累计项目/科目余额
      SUBTRACT T_ZFI022_MX-DMBTR_H FROM T_ZFI022_HKONT-ZDMBTR_SUM."累计项目/科目余额
      ADD T_ZFI022_MX-DMBTR_H TO T_ZFI022_HKONT-ZDMBTR_SUM_H."累计项目/科目余额
      ADD T_ZFI022_MX-DMBTR_S TO T_ZFI022_SO-ZDMBTR_SUM.   "累计项目余额
      ADD T_ZFI022_MX-DMBTR_S TO T_ZFI022_SO-ZDMBTR_SUM_S.   "累计项目余额
      SUBTRACT T_ZFI022_MX-DMBTR_H FROM T_ZFI022_SO-ZDMBTR_SUM.   "累计项目余额
      ADD T_ZFI022_MX-DMBTR_H TO T_ZFI022_SO-ZDMBTR_SUM_H.   "累计项目余额
      MOVE-CORRESPONDING T_ZFI022_MX TO T_ALV. "添加ALV明细行
      T_ALV-ZDMBTR_SUM = T_ZFI022_HKONT-ZDMBTR_SUM.
      APPEND T_ALV.
      CLEAR T_ALV.
      APPEND T_ZFI022_MX TO T_ZFI022_HKONT-ITEMS.
    ENDLOOP.
    "如果没期初没明细，删除该记录
    IF SY-SUBRC NE 0
      AND T_ZFI022_HKONT-DMBTR_S IS INITIAL
      AND T_ZFI022_HKONT-DMBTR_H IS INITIAL.
      DATA L_LINECOUNT TYPE I.
      CLEAR L_LINECOUNT.
      L_LINECOUNT = LINES( T_ALV[] ).
      DELETE T_ALV INDEX L_LINECOUNT.
    ELSE.
      "添加小计行
      MOVE-CORRESPONDING T_ZFI022_HKONT TO T_ALV.
      T_ALV-BKTXT = '小计'.
      T_ALV-COLOR = 'C210'."颜色
      T_ALV-GJAHR = P_GJAHR.
      IF S_MONAT-HIGH IS INITIAL.
        T_ALV-MONAT = S_MONAT-LOW.
      ELSE.
        T_ALV-MONAT = S_MONAT-HIGH.
      ENDIF..
      T_ALV-DMBTR_S = T_ZFI022_HKONT-ZDMBTR_SUM_S.
      T_ALV-DMBTR_H = T_ZFI022_HKONT-ZDMBTR_SUM_H.
      T_ALV-ZDMBTR_SUM = T_ZFI022_HKONT-ZDMBTR_SUM.
      APPEND T_ALV.
      CLEAR T_ALV.
    ENDIF.
    APPEND T_ZFI022_HKONT TO T_ZFI022_SO-ITEMS.
  ENDLOOP.
  "添加合计行
  MOVE-CORRESPONDING T_ZFI022_SO TO T_ALV.
  T_ALV-BKTXT = '合计：'.
  T_ALV-COLOR = 'C210'."颜色
  T_ALV-GJAHR = P_GJAHR.
  IF S_MONAT-HIGH IS INITIAL.
    T_ALV-MONAT = S_MONAT-LOW.
  ELSE.
    T_ALV-MONAT = S_MONAT-HIGH.
  ENDIF..
  T_ALV-DMBTR_S = T_ZFI022_SO-ZDMBTR_SUM_S.
  T_ALV-DMBTR_H = T_ZFI022_SO-ZDMBTR_SUM_H.
  T_ALV-ZDMBTR_SUM = T_ZFI022_SO-ZDMBTR_SUM.
  APPEND T_ALV.
  CLEAR T_ALV.
  MODIFY T_ZFI022_SO.
ENDLOOP.

SELECT
  A~HKONT AS LINE_ID
  A~LTEXT
  B~HKONT2 AS HKONT
  B~MATNR_FROM AS MATNR
  FROM ZFI_V12_001 AS A
  INNER JOIN ZFI_V12_003 AS B ON A~HKONT EQ B~HKONT
  INTO CORRESPONDING FIELDS OF TABLE T_HKONT_MATNR
  WHERE A~FORM EQ 'A'.

DATA L_DMBTR LIKE T_ZFI022_SO-DMBTR_S.

"add by it02 20170829 begin 取会计凭证抬头信息
 move-corresponding t_alv[] to t_pz[].
 sort t_pz by bukrs belnr gjahr.
 delete t_pz where belnr is initial.
 delete adjacent duplicates from t_pz comparing bukrs belnr gjahr.
   if t_pz[] is not initial.
       select * into table gt_bkpf
         from bkpf
         for all entries in t_pz
         where bukrs = t_pz-bukrs
         and   belnr = t_pz-belnr
         and   gjahr = t_pz-gjahr.
       sort gt_bkpf by bukrs belnr gjahr .
   endif.

"add by it02 20170829 end 取会计凭证抬头信息

"设置借贷平,项目文本
LOOP AT T_ALV.
  DATA L_TDNAME TYPE THEAD-TDNAME.
  DATA LT_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
  CLEAR: L_TDNAME, LT_TLINE[].
  L_TDNAME = T_ALV-ZSO.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      ID                            = 'Z001'
      LANGUAGE                      = SY-LANGU
      NAME                          = L_TDNAME
      OBJECT                        = 'VBBK'
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
*     OLD_LINE_COUNTER              =
    TABLES
      LINES                         = LT_TLINE[]
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
  IF LT_TLINE[] IS NOT INITIAL.
    READ TABLE LT_TLINE INDEX 1.
    IF SY-SUBRC EQ 0.
      T_ALV-ZSO_TEXT = LT_TLINE-TDLINE.
    ENDIF.
  ENDIF.
  L_DMBTR = T_ALV-DMBTR_S - T_ALV-DMBTR_H.
  IF T_ALV-ZDMBTR_SUM > 0.
    T_ALV-DIRECTION = '借'.
  ELSEIF T_ALV-ZDMBTR_SUM < 0.
    T_ALV-DIRECTION = '贷'.
  ELSE.
    T_ALV-DIRECTION = '平'.
  ENDIF.
  "CPLB设置产品类别
  "获取物料前三位，科目
  LOOP AT T_HKONT_MATNR.
    IF T_ALV-HKONT CP T_HKONT_MATNR-HKONT
      AND T_ALV-MATNR CP T_HKONT_MATNR-MATNR.
      T_ALV-CPLB = T_HKONT_MATNR-LTEXT.
      EXIT.
    ENDIF.
  ENDLOOP.

  "add by it02 20170829 begin
  "查询会计凭证抬头信息
  if t_alv-belnr is not initial.
      read table gt_bkpf into gs_bkpf with key bukrs = t_alv-bukrs
                                           belnr = t_alv-belnr
                                           gjahr = t_alv-gjahr
                                           binary search.
     if sy-subrc eq 0.
         t_alv-budat = gs_bkpf-budat.
         t_alv-bldat_2 = gs_bkpf-bldat.
     endif.
  endif.
   "add by it02 20170829 end
  MODIFY T_ALV.
ENDLOOP.

DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

CLEAR: IS_LAYOUT,IT_FIELDCAT[].
IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
IS_LAYOUT-INFO_FIELDNAME = 'COLOR'. " 行颜色字段

PERFORM APPEND_FIELDCAT USING:
  'ZSO' '项目编码' '' '' '',
  'ZSO_TEXT' '项目名称' '' '' '',
  'BLDAT_2' '凭证日期' 'BKPF' 'BLDAT' '',
  'BUDAT' '过账日期' 'BKPF' 'BUDAT' '',
  'BELNR' '凭证号' 'BSEG' 'BELNR' '',
  'BUZEI' '行项目号' 'BSEG' 'BUZEI' '',
  'HKONT' '科目编码' '' '' '',
  'TXT50' '科目名称' '' '' '',
  'GSBER' '业务范围' '' '' '',
  'GTEXT' '业务范围描述' '' '' '',
  'MATNR' '物料编码' 'MARA' 'MATNR' '',
  'MAKTX' '物料描述' '' '' '',
  'CPLB' '产品类别' '' '' '',
  'BKTXT' '摘要' '' '' '',
  'WAERS' '货币' '' '' '',
  'DMBTR_S' '借方本币' '' '' 'WAERS',
  'DMBTR_H' '贷方本币' '' '' 'WAERS',
  'DIRECTION' '方向' '' '' '',
  'ZDMBTR_SUM' '余额本币' '' '' 'WAERS'.
  ."

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
      I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ''
      I_CALLBACK_HTML_TOP_OF_PAGE       = 'FRM_TOP_OF_PAGE'
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
FORM USER_COMMAND  USING R_UCOMM LIKE SY-UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE r_ucomm.
    WHEN '&IC1'."双击
      READ TABLE T_ALV  INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0 AND T_ALV-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD T_ALV-BELNR.
        SET PARAMETER ID 'BUK' FIELD T_ALV-BUKRS.
        SET PARAMETER ID 'GJR' FIELD T_ALV-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.

FORM frm_top_of_page USING document TYPE REF TO cl_dd_document.


  DATA: lw_back TYPE sdydo_key VALUE space.
  DATA: text        TYPE sdydo_text_element,
        g_year      TYPE char4,
        g_stmonth   TYPE char2,
        g_enmonth   TYPE char2,
        today       TYPE sdydo_text_element,
        nextday     TYPE sdydo_text_element,
        date        TYPE sdydo_text_element,
        companyname TYPE sdydo_text_element,
        position    TYPE i,
        l_contents  TYPE string,
        l_month     TYPE numc2.

*&---TOP-OF-PAGE
  DATA:   mondata  TYPE sdydo_text_element,     "月产量
          yeardata TYPE sdydo_text_element,    "年产量
          unit     TYPE sdydo_text_element.     "单位

  SEARCH document->html_table FOR document->cursor.

  SELECT SINGLE
         butxt INTO companyname
         FROM t001
         WHERE bukrs EQ p_bukrs
         .

  position = sy-tabix.

  CALL METHOD document->new_line. "换行

*-----------------------------标题----------------------------------*
  CALL METHOD document->html_insert   "设置表单表头标题的显示格式
    EXPORTING
      contents = '<DIV CLASS=MSONORMAL ALIGN=CENTER STYLE="TEXT-ALIGN:CENTER" >'
    CHANGING
      position = position.

  text = '项目明细账'.

  CONCATENATE  '<SPAN ALIGN="CENTER" STYLE=" FONT-WEIGHT:BOLD;FONT-SIZE:25">' text '</SPAN> </DIV>' INTO l_contents.

  CALL METHOD document->html_insert
    EXPORTING
      contents = l_contents
    CHANGING
      position = position.

  CALL METHOD document->new_line. "换行

*-----------------------------编制单位----------------------------------*
  CALL METHOD document->add_text
    EXPORTING
      text         = '编制单位：'
      sap_emphasis = 'STRONG'.

  CALL METHOD document->add_text
    EXPORTING
      text         = companyname
      sap_emphasis = 'Key'.

  CALL METHOD document->add_text
    EXPORTING
      text         = '　　　　　　　　　　　　　　　'
      sap_emphasis = 'Key'.

*------------------------------日期-------------------------------------*
 IF P_GJAHR IS NOT INITIAL."如果填写了年，则显示日期范围
   IF S_MONAT-HIGH IS INITIAL.
     CALL METHOD document->add_text
      EXPORTING
        text         = '期间：'
        sap_emphasis = 'STRONG'.

     CONCATENATE P_GJAHR '年' S_MONAT-LOW '月' INTO DATE.

     CALL METHOD document->add_text
      EXPORTING
        text         = date
        sap_emphasis = 'Success'.
   ELSE.
     CALL METHOD document->add_text
      EXPORTING
        text         = '期间从：'
        sap_emphasis = 'STRONG'.
    CONCATENATE P_GJAHR '年' S_MONAT-LOW '月' INTO DATE.

    CALL METHOD document->add_text
      EXPORTING
        text         = date
        sap_emphasis = 'Key'.

    CALL METHOD document->add_text
      EXPORTING
        text         = '　　　　　　　　　　　　　　　'
        sap_emphasis = 'Key'.

    CALL METHOD document->add_text
      EXPORTING
        text         = '期间到：'
        sap_emphasis = 'STRONG'.

   CONCATENATE P_GJAHR '年' S_MONAT-HIGH '月' INTO DATE.

    CALL METHOD document->add_text
      EXPORTING
        text         = date
        sap_emphasis = 'Success'.
   ENDIF.
 ELSE.

     CALL METHOD document->add_text
      EXPORTING
        text         = '期间：'
        sap_emphasis = 'STRONG'.

    CALL METHOD document->add_text
      EXPORTING
        text         = '全部'
        sap_emphasis = 'Success'.
 ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
