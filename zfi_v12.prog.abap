*&-----------------------------------------------------------------------------*
*& Report  ZFI_V12
*&
*& 项目成本SAP科目发生明细
*&
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
*& Date Created : 2015/2/10                                                   *
*& Created By   : 汉得-唐博                                                   *
*& Description  :项目成本SAP科目发生明细                                                   *
*&----------------------------------------------------------------------------*
*&----------------------------------------------------------------------------*
REPORT ZFI_V12.

TABLES: VBAK,BKPF.

SELECT-OPTIONS: S_VBELN FOR VBAK-VBELN."项目号
SELECT-OPTIONS: S_BUDAT FOR BKPF-BUDAT."过账日期

DATA: GT_ALV TYPE TABLE OF ZFI_V12_005 WITH HEADER LINE.

START-OF-SELECTION.

SELECT * FROM ZFI_V12_005
  INTO CORRESPONDING FIELDS OF TABLE GT_ALV
  WHERE VBELN IN S_VBELN "项目号
  AND BUDAT IN S_BUDAT"过账日期
  .

DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

CLEAR: IS_LAYOUT,IT_FIELDCAT[].
IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

PERFORM APPEND_FIELDCAT USING:
      'VBELN' '项目号' '' '' '',
      'BSTKD' '合同号' '' '' '',
      'BUKRS' '公司代码' '' '' '',
      'KM' '科目号' '' '' '',
      'HKONT_OLD' '旧科目号' '' '' '',
      'KM_NAME' '描述' '' '' '',
      'GSBER' '业务范围' '' '' '',
      'MATKL' '物料组' '' '' '',
      'MATNR' '物料' 'MARA' 'MATNR' '',
      'BELNR' '凭证号' '' '' '',
      'GJAHR' '会计年度' '' '' '',
      'BUZEI' '凭证行项目号' '' '' '',
      'BLART' '凭证类型' '' '' '',
      'BUDAT' '过账日期' '' '' '',
      'SHKZG' '借贷标识' '' '' '',
      'DMBTR_S' '借方金额' 'BSEG' 'DMBTR' 'WAERS',
      'DMBTR_H' '贷方金额' 'BSEG' 'DMBTR' 'WAERS',
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
    WHEN 'PRINT'."打印
*      PERFORM PRINT_PDF.
    WHEN '&IC1'."双击
      READ TABLE GT_ALV INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0.
        SET PARAMETER ID 'BLN' FIELD GT_ALV-BELNR .
        SET PARAMETER ID 'BUK' FIELD GT_ALV-BUKRS.
        SET PARAMETER ID 'GJR' FIELD GT_ALV-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.
