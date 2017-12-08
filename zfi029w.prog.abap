REPORT ZFI029W.
*&---------------------------------------------------------------------*
*& REPORT  ZFIR029
*&
*&---------------------------------------------------------------------*
*& 应收票据导入
*& BY HAND
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

TYPES: BEGIN OF TY_INPUT,
         NUMBER     TYPE TEXT20,                                      "
         COMP_CODE  TYPE BAPIACHE09-COMP_CODE,                        " 公司代码
         DOC_DATE   TYPE BAPIACHE09-DOC_DATE,                         " 凭证日期
         PSTNG_DATE TYPE BAPIACHE09-PSTNG_DATE,                       " 过账日期
         DOC_TYPE   TYPE BAPIACHE09-DOC_TYPE,                         " 凭证类型
         REF_DOC_NO TYPE BAPIACHE09-REF_DOC_NO,                       " 参考凭证号
         HEADER_TXT TYPE BAPIACHE09-HEADER_TXT,                       " 凭证抬头文本
         BRNCH      TYPE BKPF-BRNCH,                                  " 分支号
         NUMPG      TYPE BKPF-NUMPG,                                  " 页数
         POSNR      TYPE POSNR_ACC,                                    " 行项目编号
         BSCHL      TYPE BSCHL,                " 记账码
         UMSKZ      TYPE UMSKZ,                " 特别总账标识
         GL_ACCOUNT TYPE BAPIACAP09-GL_ACCOUNT,                       " 科目代码
         CUSTOMER   TYPE BAPIACAR09-CUSTOMER,                         " 客户编号
         VENDOR_NO  TYPE BAPIACAP09-VENDOR_NO,                        " 供应商
         CURRENCY   TYPE BAPIACCR09-CURRENCY,                         " 货币码
         AMT_DOCCUR TYPE CHAR28,                                      " 交易货币金额
         EXCH_RATE  TYPE CHAR15,                                      " 汇率
         BUS_AREA   TYPE BAPIACGL09-BUS_AREA,                         " 业务范围
         VALUE_DATE TYPE BAPIACGL09-VALUE_DATE,                       " 起息日
         RSTGR      TYPE RSTGR,                           " 原因代码
         BLINE_DATE TYPE BAPIACAR09-BLINE_DATE,                       " 付款基准日期
         COSTCENTER TYPE BAPIACGL09-COSTCENTER,                       " 成本中心
         ORDERID    TYPE BAPIACGL09-ORDERID,                          " 统计型内部订单
         ITEM_TEXT  TYPE BAPIACGL09-ITEM_TEXT,                        " 凭证行文本
         WBANK      TYPE BSED-WBANK,                                  " 出票者
         WLZBP      TYPE BSED-WLZBP,                                  " 票号
         ZUONR      TYPE BSEG-ZUONR,                                  " 分配
         WDATE      TYPE BSED-WDATE,                                  " 签发日期
*         XREF1      TYPE BSEG-XREF1,                                  " 参考代码1
*         XREF2      TYPE BSEG-XREF2,                                  " 参考代码2
         XNEGP      TYPE BSEG-XNEGP,                                  " 反记账
*         KKBER      TYPE BSEG-KKBER,                                  " 贷方控制范围
         VBEL2      TYPE BSEG-VBEL2,                                   "销售凭证
         POSN2      TYPE BSEG-POSN2,                                   "销售凭证项目
         MATNR      TYPE BSEG-MATNR,                                  "物料号
         WERKS      TYPE BSEG-WERKS,                                  "工厂
         MENGE      TYPE CHAR28,                                     "数量
         MEINS      TYPE BSEG-MEINS,                                 "单位
         PROJK      TYPE CHAR24,                                     "BSEG-PROJK, "WBS 元素
         TCODE      TYPE TCODE,                                      "事物码
         DMBTR      TYPE CHAR13, "BSEG-DMBTR,                        "金额
       END OF TY_INPUT.

TYPES: BEGIN OF TY_TBSL,
         BSCHL TYPE TBSL-BSCHL,
         SHKZG TYPE TBSL-SHKZG,
       END OF TY_TBSL.

TYPES: BEGIN OF TY_OUTPUT,
         NUMBER TYPE TEXT20,                                         "
         BELNR  TYPE BSEG-BELNR,
         GJAHR  TYPE BSEG-GJAHR,
         BUKRS  TYPE BSEG-BUKRS,
         MSG    TYPE STRING,
         SLBOX,
       END OF TY_OUTPUT.

*--------------------------------------------------------------------*
* DATA
*--------------------------------------------------------------------*


DATA: GT_INPUT TYPE STANDARD TABLE OF TY_INPUT.
DATA: GW_INPUT TYPE TY_INPUT.
FIELD-SYMBOLS: <GS_INPUT> TYPE TY_INPUT.


*记账码对应的正负

DATA: GT_TBSL TYPE STANDARD TABLE OF TY_TBSL.
FIELD-SYMBOLS: <GS_TBSL> TYPE TY_TBSL.

DATA: GT_OUTPUT TYPE STANDARD TABLE OF TY_OUTPUT.
DATA: GW_OUTPUT TYPE TY_OUTPUT.


*ALV

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA: GS_FIELDCAT TYPE LINE OF SLIS_T_FIELDCAT_ALV.
DATA: GS_LAYOUT TYPE SLIS_LAYOUT_ALV.

*RFC 接口
DATA T_ZFIFB01 TYPE TABLE OF ZFIFB01 WITH HEADER LINE.
DATA T_ZFIFB01OUT	TYPE TABLE OF ZFIFB01OUT WITH HEADER LINE.


*&--代码添加 BY HANDYBY 09.06.2017 09:41:28  BEGIN
* 模板下载
DATA:P_OBJID(20)       TYPE C.
*&--代码添加 BY HANDYBY 09.06.2017 09:41:28  END
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
*&--代码添加 BY HANDYBY 09.06.2017 09:35:54  BEGIN
SELECTION-SCREEN PUSHBUTTON 2(13) BUT1 USER-COMMAND CMD2 MODIF ID 11.    "模板下载
*&--代码添加 BY HANDYBY 09.06.2017 09:35:54  END

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_FILE TYPE RLGRAP-FILENAME MEMORY ID M1,
            P_MODE TYPE RFPDO-ALLGAZMD DEFAULT 'E'.
SELECTION-SCREEN END OF BLOCK B1.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F4_HELP_FOR_FILENAME.

*&--代码添加 BY HANDYBY 09.06.2017 10:10:33  BEGIN
INITIALIZATION.
  CALL FUNCTION 'ICON_CREATE'                                            "调用模板下载按钮的图片
    EXPORTING
      NAME   = ICON_EXPORT
      TEXT   = '模板下载'
    IMPORTING
      RESULT = BUT1
    EXCEPTIONS
      OTHERS = 0.

AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CMD2'.                                                         "模板下载
      PERFORM DOWNLOAD_TPT USING TEXT-001
                                 P_OBJID
                           CHANGING P_FILE.
    WHEN OTHERS.
  ENDCASE.
*&--代码添加 BY HANDYBY 09.06.2017 10:10:33  END

START-OF-SELECTION.

  PERFORM F_INIT.
  PERFORM CHECK_FILENAME.
  PERFORM GET_DATA_FROM_FILE. " 导入文件数据
  PERFORM CALL_BAPI.

END-OF-SELECTION.
  PERFORM F_ALV_FIELDCAT.
  PERFORM F_LAYOUT.
  PERFORM F_DISPLAY.

*&---------------------------------------------------------------------*
*&      FORM  F_INIT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM F_INIT .
  FREE: GT_INPUT,
        GT_TBSL,
        GT_OUTPUT.
* 记账码对应的正负
  SELECT  BSCHL
          SHKZG
    INTO TABLE GT_TBSL
    FROM TBSL.
  CLEAR T_ZFIFB01[].
  IMPORT T_ZFIFB01[] FROM MEMORY ID 'ZFIFB01'.
ENDFORM. " F_INIT




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
*&      FORM  CHECK_FILENAME
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM CHECK_FILENAME .

  DATA: L_S1 TYPE STRING,
        L_S2 TYPE STRING.

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  IF P_FILE IS INITIAL AND T_ZFIFB01[] IS INITIAL..
    MESSAGE '请输入文件名.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    RETURN.
    SPLIT P_FILE AT '.' INTO L_S1 L_S2.
    TRANSLATE L_S2 TO UPPER CASE.
    IF L_S2 <> 'XLS' AND L_S2 <> 'XLSX'.
      MESSAGE '文件格式不对！' TYPE 'E'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM. " CHECK_FILENAME


*&---------------------------------------------------------------------*
*&      FORM  GET_DATA_FROM_FILE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_FILE .

  DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA,
        L_LINE TYPE I.

  DATA: LV_EXRAT TYPE CHAR30.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  CLEAR: GT_INPUT.
  IF T_ZFIFB01[] IS INITIAL."从EXCEL读入
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        I_LINE_HEADER        = 'X'       " NOT INCLUDE FILE HEADER
        I_TAB_RAW_DATA       = IT_RAW    " WORK TABLE
        I_FILENAME           = P_FILE
      TABLES
        I_TAB_CONVERTED_DATA = GT_INPUT[]
      EXCEPTIONS
        CONVERSION_FAILED    = 1
        OTHERS               = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE."从RFC接口读入
    LOOP AT T_ZFIFB01.
      MOVE-CORRESPONDING T_ZFIFB01 TO GW_INPUT.
      APPEND GW_INPUT TO GT_INPUT.
    ENDLOOP.
  ENDIF.
*
*  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
*    EXPORTING
*      FILENAME                = P_FILE
*      I_BEGIN_COL             = 1
*      I_BEGIN_ROW             = 2
*      I_END_COL               = 256
*      I_END_ROW               = 65000
*    TABLES
*      INTERN                  = GT_INPUT[]
*    EXCEPTIONS
*      INCONSISTENT_PARAMETERS = 1
*      UPLOAD_OLE              = 2
*      OTHERS                  = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  IF GT_INPUT IS INITIAL.
*    MESSAGE '上传文件不包含任何有效数据！' TYPE 'E'.
*    STOP.
*  ENDIF.


  LOOP AT GT_INPUT ASSIGNING <GS_INPUT>.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <GS_INPUT>-GL_ACCOUNT
      IMPORTING
        OUTPUT = <GS_INPUT>-GL_ACCOUNT.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <GS_INPUT>-CUSTOMER
      IMPORTING
        OUTPUT = <GS_INPUT>-CUSTOMER.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <GS_INPUT>-VENDOR_NO
      IMPORTING
        OUTPUT = <GS_INPUT>-VENDOR_NO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <GS_INPUT>-COSTCENTER
      IMPORTING
        OUTPUT = <GS_INPUT>-COSTCENTER.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <GS_INPUT>-ORDERID
      IMPORTING
        OUTPUT = <GS_INPUT>-ORDERID.

    CONDENSE <GS_INPUT>-EXCH_RATE.
    CONDENSE <GS_INPUT>-AMT_DOCCUR.
    CONDENSE <GS_INPUT>-MENGE.
  ENDLOOP.

  SORT GT_INPUT BY NUMBER POSNR.

ENDFORM. " GET_DATA_FROM_FILE




*&---------------------------------------------------------------------*
*&      FORM  CALL_BAPI
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM CALL_BAPI .


  DATA: I_GROUP  TYPE APQI-GROUPID,
        E_MSGID  TYPE SY-MSGID,
        E_MSGNO  TYPE SY-MSGNO,
        E_MSGTY  TYPE SY-MSGTY,
        E_MSGV1  TYPE SY-MSGV1,
        E_MSGV2  TYPE SY-MSGV2,
        E_MSGV3  TYPE SY-MSGV3,
        E_MSGV4  TYPE SY-MSGV4,
        E_SUBRC  TYPE SY-SUBRC,
        T_BLNTAB TYPE STANDARD TABLE OF BLNTAB,
        T_FTPOST TYPE STANDARD TABLE OF FTPOST,
        T_FTTAX  TYPE STANDARD TABLE OF FTTAX.

  DATA: LV_POSNR  TYPE NUMC10,
        LV_POSNRT TYPE CHAR10.

  DATA: LS_FTPOST TYPE FTPOST.

  DATA: LS_OUTPUT TYPE TY_OUTPUT.

  DEFINE  FILL_FTPOST.
    CLEAR LS_FTPOST.
    LS_FTPOST-STYPE = &1.
    LS_FTPOST-COUNT = &2.
    LS_FTPOST-FNAM  = &3.
    LS_FTPOST-FVAL  = &4.
    APPEND LS_FTPOST TO T_FTPOST.
  END-OF-DEFINITION.




*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*


  CLEAR: LV_POSNR,
         LV_POSNRT.

  LOOP AT GT_INPUT ASSIGNING <GS_INPUT>.

    AT NEW NUMBER.
      CLEAR: LS_OUTPUT.
      CLEAR: I_GROUP.
      CLEAR: E_MSGID,
             E_MSGNO,
             E_MSGTY,
             E_MSGV1,
             E_MSGV2,
             E_MSGV3,
             E_MSGV4,
             E_SUBRC.
      FREE: T_BLNTAB,
            T_FTPOST,
            T_FTTAX.

      FILL_FTPOST 'K' '0001' 'BKPF-BUKRS' <GS_INPUT>-COMP_CODE.          " 公司代码
      FILL_FTPOST 'K' '0001' 'BKPF-BLDAT' <GS_INPUT>-DOC_DATE.           " 凭证日期
      FILL_FTPOST 'K' '0001' 'BKPF-BUDAT' <GS_INPUT>-PSTNG_DATE.         " 过账日期
      FILL_FTPOST 'K' '0001' 'BKPF-MONAT' <GS_INPUT>-PSTNG_DATE+4(2).    " 期间
      FILL_FTPOST 'K' '0001' 'BKPF-BLART' <GS_INPUT>-DOC_TYPE.           " 凭证类型
      FILL_FTPOST 'K' '0001' 'BKPF-XBLNR' <GS_INPUT>-REF_DOC_NO.         " 参考凭证号
      FILL_FTPOST 'K' '0001' 'BKPF-BKTXT' <GS_INPUT>-HEADER_TXT.         " 凭证抬头文本
      FILL_FTPOST 'K' '0001' 'BKPF-BRNCH' <GS_INPUT>-BRNCH.              " 分支号
      FILL_FTPOST 'K' '0001' 'BKPF-NUMPG' <GS_INPUT>-NUMPG.              " 分支号
      FILL_FTPOST 'K' '0001' 'BKPF-WAERS' <GS_INPUT>-CURRENCY.           " 货币码
      FILL_FTPOST 'K' '0001' 'BKPF-KURSF' <GS_INPUT>-EXCH_RATE.          " 汇率
      IF <GS_INPUT>-TCODE IS NOT INITIAL .
        FILL_FTPOST 'K' '0001' 'BKPF-TCODE' <GS_INPUT>-TCODE.              " 事物码
      ENDIF.
    ENDAT.

    FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-BSCHL' <GS_INPUT>-BSCHL.       " 记账码
    FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-UMSKZ' <GS_INPUT>-UMSKZ.       " 特别总账标识
    "IT02 250609 begin
*    IF <GS_INPUT>-CUSTOMER IS NOT INITIAL.
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-KUNNR' <GS_INPUT>-CUSTOMER.  " 客户编号
*    ELSEIF <GS_INPUT>-VENDOR_NO IS NOT INITIAL.
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-LIFNR' <GS_INPUT>-VENDOR_NO. " 科目代码
*    ELSE.
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-HKONT' <GS_INPUT>-GL_ACCOUNT." 科目代码
*    ENDIF.
    "it02150609 end
    IF <GS_INPUT>-CUSTOMER IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-KUNNR' <GS_INPUT>-CUSTOMER.  " 客户编号
    ELSEIF <GS_INPUT>-VENDOR_NO IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-LIFNR' <GS_INPUT>-VENDOR_NO. " 科目代码
    ENDIF.

    IF <GS_INPUT>-GL_ACCOUNT NE ''.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-HKONT' <GS_INPUT>-GL_ACCOUNT." 科目代码
    ENDIF.

    FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-WRBTR' <GS_INPUT>-AMT_DOCCUR.  " 交易货币金额
    IF <GS_INPUT>-BUS_AREA IS NOT INITIAL.
      "  FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-GSBER' <GS_INPUT>-BUS_AREA.  " 业务范围
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-GSBER' <GS_INPUT>-BUS_AREA.  " 业务范围 LEYARDIT02 150505
    ENDIF.
    IF <GS_INPUT>-VALUE_DATE IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-VALUT' <GS_INPUT>-VALUE_DATE. " 起息日
    ENDIF.
    IF <GS_INPUT>-RSTGR IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-RSTGR' <GS_INPUT>-RSTGR.      " 原因代码
    ENDIF.
    IF <GS_INPUT>-BLINE_DATE IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-ZFBDT' <GS_INPUT>-BLINE_DATE. " 付款基准日期
    ENDIF.
    IF <GS_INPUT>-COSTCENTER IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-KOSTL' <GS_INPUT>-COSTCENTER." 成本中心
    ENDIF.
    IF <GS_INPUT>-ORDERID IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-AUFNR' <GS_INPUT>-ORDERID.   " 统计型内部订单
    ENDIF.
    FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-SGTXT' <GS_INPUT>-ITEM_TEXT.   " 凭证行文本
    IF <GS_INPUT>-WBANK IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSED-WBANK' <GS_INPUT>-WBANK.       " 出票者
    ENDIF.
    IF <GS_INPUT>-WLZBP IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSED-WLZBP' <GS_INPUT>-WLZBP.       " 票号
    ENDIF.
    IF <GS_INPUT>-ZUONR IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-ZUONR' <GS_INPUT>-ZUONR.       " 分配
    ENDIF.
    IF <GS_INPUT>-WDATE IS NOT INITIAL.
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSED-WDATE' <GS_INPUT>-WDATE.       " 签发日期
    ENDIF.
*    IF <GS_INPUT>-XREF1 IS NOT INITIAL .
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-XREF1' <GS_INPUT>-XREF1.       " 参考代码1
*    ENDIF.
*    IF <GS_INPUT>-XREF2 IS NOT INITIAL .
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-XREF2' <GS_INPUT>-XREF2.       " 参考代码2
*    ENDIF.
    IF <GS_INPUT>-XNEGP IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-XNEGP' <GS_INPUT>-XNEGP.       " 反记账
    ENDIF.
*    IF <GS_INPUT>-KKBER IS NOT INITIAL .
*      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-KKBER' <GS_INPUT>-KKBER.       " 贷方控制范围
*    ENDIF.
    IF <GS_INPUT>-VBEL2 IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-KDAUF' <GS_INPUT>-VBEL2.       " 销售订单
    ENDIF.
    IF <GS_INPUT>-POSN2 IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-KDPOS' <GS_INPUT>-POSN2.       " 销售订单行项目
    ENDIF.
    IF <GS_INPUT>-MATNR IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-MATNR' <GS_INPUT>-MATNR.       " 物料号
    ENDIF.
    IF <GS_INPUT>-WERKS IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-WERKS' <GS_INPUT>-WERKS.       " 工厂
    ENDIF.
    IF <GS_INPUT>-MENGE IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-MENGE' <GS_INPUT>-MENGE.       " 数量
    ENDIF.
    IF <GS_INPUT>-MEINS IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-MEINS' <GS_INPUT>-MEINS.       " 单位
    ENDIF.
    IF <GS_INPUT>-PROJK IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'COBL-PS_POSID' <GS_INPUT>-PROJK.    " WBS元素
    ENDIF.
    IF <GS_INPUT>-DMBTR IS NOT INITIAL .
      FILL_FTPOST 'P' <GS_INPUT>-POSNR 'BSEG-DMBTR' <GS_INPUT>-DMBTR.       " 金额
    ENDIF.



    AT END OF NUMBER.

      ADD 1 TO LV_POSNR.
      LV_POSNRT = LV_POSNR.
      CONDENSE LV_POSNRT.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = LV_POSNRT
        IMPORTING
          OUTPUT = LV_POSNRT.

      CONCATENATE SY-UZEIT LV_POSNRT <GS_INPUT>-NUMBER INTO I_GROUP.
      CALL FUNCTION 'POSTING_INTERFACE_START'
        EXPORTING
          I_FUNCTION         = 'C'
          I_GROUP            = I_GROUP
          I_KEEP             = 'X'
          I_MODE             = P_MODE
          I_USER             = SY-UNAME
          I_XBDCC            = 'X'
        EXCEPTIONS
          CLIENT_INCORRECT   = 1
          FUNCTION_INVALID   = 2
          GROUP_NAME_MISSING = 3
          MODE_INVALID       = 4
          UPDATE_INVALID     = 5
          OTHERS             = 6.
      IF SY-SUBRC <> 0.
        LS_OUTPUT-NUMBER = <GS_INPUT>-NUMBER.
        MESSAGE ID     SY-MSGID
                TYPE   SY-MSGTY
                NUMBER SY-MSGNO
                WITH   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                INTO   LS_OUTPUT-MSG.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
        EXPORTING
          I_TCODE                  = 'FB01'
        IMPORTING
          E_MSGID                  = E_MSGID
          E_MSGNO                  = E_MSGNO
          E_MSGTY                  = E_MSGTY
          E_MSGV1                  = E_MSGV1
          E_MSGV2                  = E_MSGV2
          E_MSGV3                  = E_MSGV3
          E_MSGV4                  = E_MSGV4
          E_SUBRC                  = E_SUBRC
        TABLES
          T_BLNTAB                 = T_BLNTAB
          T_FTPOST                 = T_FTPOST
          T_FTTAX                  = T_FTTAX
        EXCEPTIONS
          ACCOUNT_MISSING          = 1
          COMPANY_CODE_MISSING     = 2
          POSTING_KEY_INVALID      = 3
          POSTING_KEY_MISSING      = 4
          RECORD_TYPE_INVALID      = 5
          TRANSACTION_CODE_INVALID = 6
          AMOUNT_FORMAT_ERROR      = 7
          TOO_MANY_LINE_ITEMS      = 8
          COMPANY_CODE_INVALID     = 9
          SCREEN_NOT_FOUND         = 10
          NO_AUTHORIZATION         = 11
          OTHERS                   = 12.
      IF E_MSGID = 'F5' AND E_MSGNO = '312'.
        LS_OUTPUT-BUKRS = E_MSGV2.
        LS_OUTPUT-GJAHR = <GS_INPUT>-PSTNG_DATE+0(4).
        LS_OUTPUT-BELNR = E_MSGV1.
        CALL FUNCTION 'POSTING_INTERFACE_END'.
        UPDATE BKPF SET BRNCH = <GS_INPUT>-BRNCH NUMPG = <GS_INPUT>-NUMPG WHERE BUKRS = LS_OUTPUT-BUKRS AND GJAHR = LS_OUTPUT-GJAHR AND BELNR = LS_OUTPUT-BELNR.
        COMMIT WORK.
      ELSE.
      ENDIF.
      LS_OUTPUT-NUMBER = <GS_INPUT>-NUMBER.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = E_MSGID
          MSGNR               = E_MSGNO
          MSGV1               = E_MSGV1
          MSGV2               = E_MSGV2
          MSGV3               = E_MSGV3
          MSGV4               = E_MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LS_OUTPUT-MSG.
      APPEND LS_OUTPUT TO GT_OUTPUT.
    ENDAT.

  ENDLOOP.

ENDFORM. " CALL_BAPI


*&---------------------------------------------------------------------*
*&      FORM  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM F_ALV_FIELDCAT .

  DATA: POS TYPE I.

  DEFINE FIELDCAT.
    CLEAR: GS_FIELDCAT.
    POS = POS + 1.
    GS_FIELDCAT-COL_POS = POS.
    GS_FIELDCAT-FIELDNAME = &1.
    GS_FIELDCAT-REF_TABNAME = &2.
    GS_FIELDCAT-REF_FIELDNAME = &3.
    GS_FIELDCAT-KEY = &4.
    GS_FIELDCAT-NO_ZERO = &5.
    GS_FIELDCAT-NO_OUT = &6.
    GS_FIELDCAT-SELTEXT_L = &7.
    GS_FIELDCAT-SELTEXT_M = &7.
    GS_FIELDCAT-SELTEXT_S = &7.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  END-OF-DEFINITION.

  FIELDCAT 'NUMBER' ''     ''      ''  '' '' '凭证接口编号'.
  FIELDCAT 'BELNR'  'BSEG' 'BELNR' ''  '' '' '凭证号'.
  FIELDCAT 'BUKRS'  'BKPF' 'BUKRS' ''  '' '' '公司代码'.
  FIELDCAT 'GJAHR'  'BKPF' 'GJAHR' ''  '' '' '年度'.
  FIELDCAT 'MSG'    ''     ''      ''  '' '' '返回消息'.

ENDFORM. " F_ALV_FIELDCAT




*&---------------------------------------------------------------------*
*&      FORM  F_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*

FORM F_LAYOUT .
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-BOX_FIELDNAME = 'SLBOX'.
ENDFORM. " F_LAYOUT




*&---------------------------------------------------------------------*
*&      FORM  F_DISPLAY
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*

FORM F_DISPLAY .


*  可以保存格式

  DATA:GS_VARIANT   TYPE DISVARIANT.
  GS_VARIANT-REPORT = SY-REPID.
  IF T_ZFIFB01[] IS INITIAL."输入到ALV
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        I_SAVE             = 'A'         "可以保存格式
        IS_VARIANT         = GS_VARIANT  "LAYOUT参数 可以保存格式
        IS_LAYOUT          = GS_LAYOUT
        IT_FIELDCAT        = GT_FIELDCAT
      TABLES
        T_OUTTAB           = GT_OUTPUT
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
  ELSE."输出到RFC接口
    CLEAR T_ZFIFB01OUT[].
    LOOP AT GT_OUTPUT INTO GW_OUTPUT.
      MOVE-CORRESPONDING GW_OUTPUT TO T_ZFIFB01OUT.
      APPEND T_ZFIFB01OUT.
      CLEAR T_ZFIFB01OUT.
    ENDLOOP.
    EXPORT T_ZFIFB01OUT[] TO MEMORY ID 'ZFIFB01OUT'.
  ENDIF.
ENDFORM. "F_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TPT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_001  text
*      -->P_P_OBJID  text
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM DOWNLOAD_TPT   USING    P_TEXT
                   VALUE(P_OBJID)
                   CHANGING PV_FILE.

*&--代码添加 BY HANDYBY 09.06.2017 10:23:18  BEGIN
  P_OBJID = 'ZFI029W.XLS'.
*&--代码添加 BY HANDYBY 09.06.2017 10:23:18  END


  DATA: LV_FNAME TYPE STRING,
        LV_TITLE TYPE STRING,
        LV_PATH  TYPE STRING VALUE 'D:/',
        LV_FPATH TYPE STRING VALUE 'D:/'.

  DATA: LS_WDATB   LIKE WWWDATATAB.
  DATA: LV_SUBRC   TYPE SY-SUBRC.
  DATA: GV_MSG TYPE STRING .

  LV_FNAME = P_TEXT.

  CONCATENATE P_TEXT '下载' INTO LV_TITLE.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = LV_TITLE
      DEFAULT_EXTENSION    = 'XLS'
      DEFAULT_FILE_NAME    = LV_FNAME
      INITIAL_DIRECTORY    = 'D:\'
      FILE_FILTER          = 'Excel文件(*.XLS)|*.XLS|全部文件 (*.*)|*.*|'
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = LV_FNAME
      PATH                 = LV_PATH
      FULLPATH             = LV_FPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    SELECT SINGLE RELID
                  OBJID
      FROM WWWDATA
      INTO CORRESPONDING FIELDS OF LS_WDATB
      WHERE SRTF2 = 0
      AND RELID = 'MI'
      AND OBJID = P_OBJID.                        "p_objid就是传入模板的参数
    IF LS_WDATB IS INITIAL.
      MESSAGE '模板文件不存在！' TYPE 'E'.
    ELSE.
      PV_FILE = LV_FPATH.
      IF PV_FILE IS NOT INITIAL.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            KEY         = LS_WDATB
            DESTINATION = PV_FILE
          IMPORTING
            RC          = LV_SUBRC.
        IF LV_SUBRC NE 0.
          MESSAGE '模板下载失败！' TYPE 'E'.
        ELSE.
          CLEAR GV_MSG.
          CONCATENATE '模板下载到本地文件' PV_FILE INTO GV_MSG.
          MESSAGE GV_MSG TYPE 'S' .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
