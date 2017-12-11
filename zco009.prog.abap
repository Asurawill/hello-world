*&---------------------------------------------------------------------*
*& 程序名称:ZCO009
*& 作者    :汪昱
*& 开发日期:
*& 请求号  :
*& 描述    :CO凭证导入
*& 开发申请：
*& 变更记录
*&
** 修改日期  20160106
"开发人员  IT02
" 请求号 描述 增加旧成本中心 、 新成本中心 及消息输出提示
*&---------------------------------------------------------------------*
REPORT ZCO009.

TYPES:BEGIN OF TY_DATA,
        DOCDATE    TYPE CO_BLDAT, "凭证日期
        POSTGDATE  TYPE CO_BUDAT, "过账日期
        PERIOD     TYPE CO_PERIO, "期间
        DOC_HDR_TX TYPE CO_BLTXT, "抬头文本
        SEND_CCTR   TYPE SKOSTL,   "旧成本中心
        SEN_ORDER  TYPE SAUFNR,  "订单号(旧)
        SEN_WBS_EL TYPE S_PS_POSID, "WBS元素(旧)
        COST_ELEM  TYPE KSTAR,     "成本要素
        VALUE_TCUR TYPE BGTXXX,    "金额
        TRANS_CURR TYPE TWAER,     "货币
        REC_CCTR  TYPE EKOSTL,    "新成本中心
        REC_ORDER  TYPE EAUFNR,    "订单号（新）
        REC_WBS_EL TYPE E_PS_POSID, "WBS元素(新)
      END OF TY_DATA.

DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GS_DOC_HEADER TYPE BAPIDOCHDRU12P
      .
DATA GT_DOC_ITEMS  TYPE TABLE OF BAPIRCITM.
DATA GS_DOC_ITEMS  TYPE BAPIRCITM.

DATA GT_RETURN     TYPE TABLE OF BAPIRET2.
DATA GS_RETURN     TYPE BAPIRET2.

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_FILE TYPE RLGRAP-FILENAME MEMORY ID ZCO009.
SELECTION-SCREEN END OF BLOCK B1.

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F4_HELP_FOR_FILENAME.

START-OF-SELECTION.

  PERFORM F_INIT.
  PERFORM CHECK_FILENAME.
  PERFORM GET_DATA_FROM_FILE. " 导入文件数据
  PERFORM CALL_BAPI.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      FORM  F4_HELP_FOR_FILENAME
*&---------------------------------------------------------------------*
*       针对文件的SEARCH HELP
*----------------------------------------------------------------------*
*      -->P_FILENAME  TEXT
*----------------------------------------------------------------------*
FORM F4_HELP_FOR_FILENAME.
  DATA: L_FILE         TYPE STRING,
        L_FILE_IMPORT  TYPE STRING,
        L_PATH_INITIAL TYPE STRING,
        LT_FILETABLE   TYPE FILETABLE,
        LW_FILETABLE   LIKE LINE OF LT_FILETABLE,
        L_RC           TYPE I.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  L_FILE = P_FILE.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      FILE_FILTER             = 'EXCEL 文件 (*.XLS;*XLSX)|*.XLS;*.XLSX'
    CHANGING
      FILE_TABLE              = LT_FILETABLE
      RC                      = L_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
  ENDIF.

  IF L_RC > 0.
    READ TABLE LT_FILETABLE INTO LW_FILETABLE INDEX 1.
    IF SY-SUBRC = 0.
      P_FILE = LW_FILETABLE-FILENAME.
    ENDIF.
  ENDIF.

*    CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
**   EXPORTING
**     DEF_FILENAME           = ' '
**     DEF_PATH               = ' '
**     MASK                   = ' '
**     MODE                   = ' '
**     TITLE                  = ' '
*    IMPORTING
*      FILENAME         = L_FILE
**     PATH             =
**     FILE             =
*    EXCEPTIONS
*      SELECTION_CANCEL = 1
*      SELECTION_ERROR  = 2
*      OTHERS           = 3.
*  IF SY-SUBRC <> 0.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INIT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_FILENAME .
  DATA: L_S1 TYPE STRING,
        L_S2 TYPE STRING.

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_FILE .
  DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA,
        L_LINE TYPE I.

  DATA: LV_EXRAT TYPE CHAR30.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  CLEAR GT_DATA.
*  IF T_ZFIFB01[] IS INITIAL."从EXCEL读入
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      I_LINE_HEADER        = 'X'       " NOT INCLUDE FILE HEADER
      I_TAB_RAW_DATA       = IT_RAW    " WORK TABLE
      I_FILENAME           = P_FILE
    TABLES
      I_TAB_CONVERTED_DATA = GT_DATA
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*  ELSE."从RFC接口读入
*    LOOP AT T_ZFIFB01.
*      MOVE-CORRESPONDING T_ZFIFB01 TO GW_INPUT.
*      APPEND GW_INPUT TO GT_INPUT.
*    ENDLOOP.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BAPI .
  DATA G_TEXT TYPE STRING.
  CLEAR G_TEXT.

  READ TABLE GT_DATA INTO GS_DATA INDEX 1.
  IF SY-SUBRC = 0.
    GS_DOC_HEADER-CO_AREA    = '1000'.

    GS_DOC_HEADER-DOCDATE    = GS_DATA-DOCDATE.

    GS_DOC_HEADER-POSTGDATE  = GS_DATA-POSTGDATE.

    GS_DOC_HEADER-DOC_HDR_TX = GS_DATA-DOC_HDR_TX.

    GS_DOC_HEADER-USERNAME   = SY-UNAME.

    LOOP AT GT_DATA INTO GS_DATA.
      GS_DOC_ITEMS-SEND_CCTR  = GS_DATA-SEND_CCTR. "旧成本中心
      GS_DOC_ITEMS-SEN_WBS_EL = GS_DATA-SEN_WBS_EL."WBS元素号（旧）
      GS_DOC_ITEMS-COST_ELEM  = GS_DATA-COST_ELEM. "成本要素
      GS_DOC_ITEMS-VALUE_TCUR = GS_DATA-VALUE_TCUR."金额
      GS_DOC_ITEMS-REC_ORDER  = GS_DATA-REC_ORDER. "订单号（新）
      GS_DOC_ITEMS-TRANS_CURR = GS_DATA-TRANS_CURR."货币
      GS_DOC_ITEMS-REC_CCTR   = GS_DATA-REC_CCTR.  "新成本中心
      GS_DOC_ITEMS-SEN_ORDER  = GS_DATA-SEN_ORDER. "订单号(旧)
      GS_DOC_ITEMS-REC_WBS_EL = GS_DATA-REC_WBS_EL."WBS元素号（新）
      APPEND GS_DOC_ITEMS TO GT_DOC_ITEMS.
      CLEAR GS_DOC_ITEMS.
    ENDLOOP.

    CALL FUNCTION 'BAPI_ACC_PRIMARY_COSTS_POST'
      EXPORTING
        DOC_HEADER = GS_DOC_HEADER
*       IGNORE_WARNINGS       = ' '
* IMPORTING
*       DOC_NO     =
      TABLES
        DOC_ITEMS  = GT_DOC_ITEMS
        RETURN     = GT_RETURN
*       SEND_CRITERIA         =
*       REC_CRITERIA          =
*       CUSTOMER_FIELDS       =
      .

    READ TABLE GT_RETURN INTO GS_RETURN
    WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
      COMMIT WORK AND WAIT.

*      READ TABLE GT_RETURN INTO GS_RETURN
*      WITH KEY TYPE = 'S'.
*
*      CONCATENATE GS_RETURN-MESSAGE_V1 '凭证创建成功' INTO G_TEXT.
*      MESSAGE G_TEXT  TYPE  'I'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
     LOOP AT GT_RETURN INTO GS_RETURN.
             PERFORM STORE_MESSAGES USING GS_RETURN-ID
                                 GS_RETURN-TYPE
                                 GS_RETURN-MESSAGE_V1
                                 GS_RETURN-MESSAGE_V2
                                 GS_RETURN-MESSAGE_V3
                                 GS_RETURN-MESSAGE_V4
                                 GS_RETURN-NUMBER.


         ENDLOOP.
         "at lsat call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA L_EXIT_COMMAND TYPE BAL_S_EXCM.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO         = SPACE
    IMPORTING
      E_EXIT_COMMAND     = L_EXIT_COMMAND
    EXCEPTIONS
      INCONSISTENT_RANGE = 1
      NO_MESSAGES        = 2
      OTHERS             = 3.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_RETURN_ID  text
*      -->P_GS_RETURN_TYPE  text
*      -->P_GS_RETURN_MESSAGE_V1  text
*      -->P_GS_RETURN_MESSAGE_V2  text
*      -->P_GS_RETURN_MESSAGE_V3  text
*      -->P_GS_RETURN_MESSAGE_V4  text
*      -->P_GS_RETURN_NUMBER  text
*----------------------------------------------------------------------*
FORM STORE_MESSAGES USING P_MSGID
                          P_MSGTY
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                          P_TXTNR.
* Store the messages to be displayed
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      ARBGB                  = P_MSGID
      MSGTY                  = P_MSGTY
      MSGV1                  = P_MSGV1
      MSGV2                  = P_MSGV2
      MSGV3                  = P_MSGV3
      MSGV4                  = P_MSGV4
      TXTNR                  = P_TXTNR
    EXCEPTIONS
      MESSAGE_TYPE_NOT_VALID = 1
      NOT_ACTIVE             = 2
      OTHERS                 = 3.
ENDFORM.
