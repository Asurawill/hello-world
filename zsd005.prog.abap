*&-----------------------------------------------------------------------------*
*& Report  ZSD005
*&
*& 屏幕明细表
*&
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
*& Date Created : 2015/1/27                                                   *
*& Created By   : 汉得-唐博                                                   *
*& Description  :屏幕明细表                                                   *
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
REPORT ZSD005.

TABLES VBAK.

*DATA: GT_ALV TYPE TABLE OF ZSD005 WITH HEADER LINE.
DATA BEGIN OF GT_ALV OCCURS 1.
  INCLUDE STRUCTURE ZSD005.
DATA:
      VBBK_Z001 TYPE CHAR100,
      KBETR_RATIO TYPE CHAR10,
      MRP_STATE TYPE CHAR1,
      VBBK_Z003 TYPE CHAR100,
      KBETR_TOTAL TYPE FAGLFLEXA-HSL,
      KBETR_TAXTOTAL TYPE FAGLFLEXA-HSL,
      DATE_CJ TYPE DATS,
      DATE_SH TYPE DATS,
      DATE_WG TYPE DATS,
      DATE_QW TYPE DATS,
*      WAERS TYPE BKPF-WAERS,
      END OF GT_ALV.


"销售组织
SELECT-OPTIONS: S_VKORG FOR VBAK-VKORG OBLIGATORY.
"销售订单
SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN.
"订单类型
SELECT-OPTIONS: S_AUART FOR VBAK-AUART MATCHCODE OBJECT Z_TVAK_SD005.
"创建日期
SELECT-OPTIONS: S_ERDAT FOR VBAK-ERDAT.

INITIALIZATION.

START-OF-SELECTION.

DATA L_VKORG TYPE VBAK-VKORG.
SELECT VKORG FROM TVKO INTO L_VKORG WHERE VKORG IN S_VKORG.
  AUTHORITY-CHECK OBJECT 'V_KNA1_VKO'
           ID 'VKORG' FIELD L_VKORG.
*           ID 'VTWEG' FIELD 'DUMMY'
*           ID 'SPART' FIELD 'DUMMY'
*           ID 'ACTVT' FIELD 'DUMMY'.
  IF SY-SUBRC <> 0.
    MESSAGE S001(ZSD01) WITH L_VKORG DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
*  您没有销售组织&的权限！
  ENDIF.
ENDSELECT.

SELECT *
  FROM ZSD005
  INTO CORRESPONDING FIELDS OF TABLE GT_ALV
  WHERE VKORG IN S_VKORG
  AND VBELN IN S_VBELN
  AND AUART IN S_AUART
  AND ERDAT IN S_ERDAT
*  AND ERNAM EQ SY-UNAME
  .

  LOOP AT GT_ALV.
    "判断查看创建人的权限
    AUTHORITY-CHECK OBJECT 'Z_SD_USER'
             ID 'VKORG' FIELD GT_ALV-VKORG
             ID 'USR20_1' FIELD GT_ALV-ERNAM.
    IF SY-SUBRC <> 0.
      IF GT_ALV-ERNAM NE SY-UNAME.
        DELETE GT_ALV.
        CONTINUE.
      ENDIF.
    ENDIF.

    "项目名称	VBBK_Z001
    PERFORM GET_TEXT USING 'Z001' CHANGING GT_ALV-VBBK_Z001.
    "置换比例	KBETR_RATIO
    GT_ALV-KBETR_1 = ABS( GT_ALV-KBETR_1 ).
    DIVIDE GT_ALV-KBETR_1 BY 10.
    WRITE GT_ALV-KBETR_1 TO GT_ALV-KBETR_RATIO.
    CONCATENATE GT_ALV-KBETR_RATIO '%' INTO GT_ALV-KBETR_RATIO.
    "MRP状态  MRP_STATE
    IF GT_ALV-ERDAT_1 IS INITIAL.
      GT_ALV-MRP_STATE = 'N'.
    ELSE.
      GT_ALV-MRP_STATE = 'Y'.
    ENDIF.
    "完工日期	VBBK_Z003
*    PERFORM GET_TEXT USING 'Z003' CHANGING GT_ALV-VBBK_Z003.
    "不含税总价  KBETR_TOTAL
    "
    IF GT_ALV-AUART EQ 'ZF1'.
      CLEAR: GT_ALV-KBETR_TOTAL, GT_ALV-KBETR_TAXTOTAL.
    ELSE.
      IF GT_ALV-KSCHL EQ 'ZP01'."当单价取ZP01时 单价乘以面积
        GT_ALV-KBETR_TOTAL = ( GT_ALV-KBETR * GT_ALV-ZSD0109 ) / '1.17'.
        "含税总价	KBETR_TAXTOTAL
        GT_ALV-KBETR_TAXTOTAL = GT_ALV-KBETR * GT_ALV-ZSD0109.
      ELSE.
        GT_ALV-KBETR_TOTAL = GT_ALV-KBETR / '1.17'.
        "含税总价	KBETR_TAXTOTAL
        GT_ALV-KBETR_TAXTOTAL = GT_ALV-KBETR.
      ENDIF.
    ENDIF.
    GT_ALV-DATE_CJ = GT_ALV-ERDAT.
    GT_ALV-DATE_SH = GT_ALV-ERDAT_1.
    GT_ALV-DATE_QW = GT_ALV-VDATU.
    GT_ALV-DATE_WG = GT_ALV-ZSD0301.
    MODIFY GT_ALV.
  ENDLOOP.

  SORT GT_ALV BY VBELN POSNR.
  DELETE ADJACENT DUPLICATES FROM GT_ALV COMPARING VBELN POSNR.

  DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  CLEAR: IS_LAYOUT,IT_FIELDCAT[].
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  PERFORM APPEND_FIELDCAT USING:
    'VBELN' '订单编号' '' '' '',
    'AUART' '订单类型' '' '' '',
    'VBBK_Z001' '项目名称' '' '' '',
    'BSTKD' '合同编号' '' '' '',
    'ZSD0302' '销售立项号' '' '' '',
    'DATE_CJ' '创建时间' '' '' '',
    'ERNAM' '创建人' '' '' '',
    'KUNNR' '客户编号' '' '' '',
    'NAME1' '客户名称' '' '' '',
    'DATE_SH' '审核日期' '' '' '',
    'BEZEI' '行业' '' '' '',
    'BEZEI_1' '经济性质' '' '' '',
    'BZTXT' '行政区域' '' '' '',
    'LANDX' '项目实施国家' '' '' '',
    'BZTXT_SSDQ' '项目实施地区' '' '' '',
    'BEZEI_T005U' '项目实施省份' '' '' '',
    'NAME1_1' '项目经理 1' '' '' '',
    'CITY1' '经理1 占比' '' '' '',
    'NAME1_2' '项目经理 2' '' '' '',
    'CITY1_1' '经理2 占比' '' '' '',
    'NAME1_3' '经办人 1' '' '' '',
    'CITY1_2' '经办人1 占比' '' '' '',
    'NAME1_4' '经办人 2' '' '' '',
    'CITY1_3' '经办人2 占比' '' '' '',
    'NAME1_5' '经办人 3' '' '' '',
    'CITY1_4' '经办人3 占比' '' '' '',
    'BEZEI_2' '主管副总' '' '' '',
    'BEZEI_8' '部门' '' '' '',
    'BEZEI_9' '销售类型' '' '' '',
    'KBETR_RATIO' '置换比例' '' '' '',
    'MRP_STATE' 'MRP状态' '' '' '',
    'DATE_QW' '期望发货日期' '' '' '',
    'DATE_WG' '完工日期' '' '' '',
    'VTEXT_1' '产品类型' '' '' '',
    'POSNR' '订单行编号' '' '' '',
    'PSTYV' '行项目类型' '' '' '',
    'MATNR' '物料编号' '' '' '',
    'ARKTX' '物料描述' '' '' '',
    'BEZEI_3' '规格型号' '' '' '',
    'BEZEI_4' '控制设备' '' '' '',
    'BEZEI_5' '像素间距' '' '' '',
    'BEZEI_6' '发光管型号' '' '' '',
    'BEZEI_7' '安装方式' '' '' '',
    'ZSD002' '用途' '' '' '',
    'ZSD0101' '箱体宽度' '' '' '',
    'ZSD0105' '屏幕宽度' '' '' '',
    'ZSD0103' '箱体列数' '' '' '',
    'ZSD0102' '箱体高度' '' '' '',
    'ZSD0106' '屏幕高度' '' '' '',
    'ZSD0104' '箱体行数' '' '' '',
    'ZSD0107' '屏体面积' '' '' '',
    'ZSD0108' '屏数' '' '' '',
    'ZSD0109' '总面积' '' '' '',
    'KBETR' '单价' '' '' 'WAERS',
    'KBETR_TOTAL' '不含税总价' '' '' 'WAERS',
    'KBETR_TAXTOTAL' '含税总价' '' '' 'WAERS',
    'WAERS' '货币' '' '' ''.

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
  L_TDNAME = GT_ALV-VBELN.
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
