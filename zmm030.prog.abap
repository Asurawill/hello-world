REPORT ZMM030.
*&---------------------------------------------------------------------*
* 作者: HAND
* 开发日期: 2015-11-19
* 请求号:
* 申请者: N/A
* 功能/技术文档:
* 描述: 采购信息记录批量导入
*&---------------------------------------------------------------------*
*                           变更记录
*&---------------------------------------------------------------------*
* 修改日期: 2015-11-19  IT02
* 描述:
*
*
*
*
*
*
*&---------------------------------------------------------------------*



TYPE-POOLS: SLIS,TRUXS.

*-->上传接口定义
TYPES: BEGIN OF TY_UPLOAD,
        LIFNR(40) TYPE C,     "供应商
        NAME1(40) TYPE C,     "供应商描述
        MATNR(40) TYPE C,     "物料
        MAKTX(40) TYPE C,     "物料描述
        EKORG(40) TYPE C,     "采购组织
        WERKS(40) TYPE C,     "工厂
   "     ESOKZ(40) TYPE C,     "信息类别
      "  IDNLF(40) TYPE C,     "供应商物料编码
      "  NORBM(40) TYPE C,     "标准数量
*        MEINS(40) TYPE C,     "计量单位
*        UMREZ(40) TYPE C,     "分子
*        UMREN(40) TYPE C,      "分母
        NETPR(40) TYPE C,     "材料价格
        PEINH(40) TYPE C,     "价格单位
        MWSKZ(40) TYPE C,     "材料税代码
        DATAB(40) TYPE C,     "有效开始日期
        DATBI(40) TYPE C,     "有效结束日期
        INFNR	TYPE EINA-INFNR,
       END OF TY_UPLOAD.
DATA: WYT_UPLOAD TYPE STANDARD TABLE OF TY_UPLOAD,
      WYS_UPLOAD TYPE TY_UPLOAD.
DATA: IT_DOWNLOAD TYPE STANDARD TABLE OF TY_UPLOAD,
      WA_DOWNLOAD TYPE TY_UPLOAD.

DATA: WYT_MES TYPE STANDARD TABLE OF BAPI_METH_MESSAGE,
      WYS_MES TYPE BAPI_METH_MESSAGE.

TYPES: BEGIN OF TY_INFO,
        INFNR	TYPE EINA-INFNR,
        LIFNR	TYPE EINA-LIFNR,
        MATNR	TYPE EINA-MATNR,
        EKORG	TYPE EINE-EKORG,
        WERKS	TYPE EINE-WERKS,
*        MEINS TYPE EINA-MEINS,
*        UMREZ TYPE EINA-UMREZ,
*        UMREN TYPE EINA-UMREN,
       END OF TY_INFO.
DATA: WYT_INFO TYPE STANDARD TABLE OF TY_INFO,
      WYS_INFO TYPE TY_INFO.

DATA T_RAW_DATA TYPE TRUXS_T_TEXT_DATA.


DATA: GDT_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
      GDS_BDCDATA TYPE BDCDATA,
      GDT_BDCMESSAGE TYPE STANDARD TABLE OF BDCMSGCOLL,
      GDS_BDCMESSAGE TYPE BDCMSGCOLL.
DATA: GDT_RETURN TYPE STANDARD TABLE OF BAPIRETURN,
      GDS_RETURN TYPE BAPIRETURN.

DATA:G_REPID LIKE SY-REPID.
DATA: LIT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT1  TYPE SLIS_LAYOUT_ALV.
DATA: WA_ALV_FIELD TYPE SLIS_FIELDCAT_ALV.
DATA: G_GLAY TYPE LVC_S_GLAY.
DATA: LT_FCAT TYPE SLIS_T_FIELDCAT_ALV.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS P_FILE LIKE RLGRAP-FILENAME OBLIGATORY.
PARAMETERS: R_UP RADIOBUTTON GROUP RAD1 DEFAULT 'X',
            R_DOWN RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN END OF BLOCK B1.


*&---------------------------------------------------------------------*
*& EVENT INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.                         "初始化

*&---------------------------------------------------------------------*
*& EVENT AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_FILEPATH.
*&---------------------------------------------------------------------*
** Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF R_UP = 'X'.
    PERFORM FRM_INPUT.
    PERFORM FRM_INFORECORD_MODIFY.
    PERFORM PRM_WRITE_LOG.
  ENDIF.
  IF R_DOWN = 'X'.
    PERFORM FRM_DOWNLOAD.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_GET_FILEPATH.
  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    EXPORTING
*     DEF_FILENAME     = ' '
*     DEF_PATH         = ' '
      MASK             = 'Excel Files,*.xls,All Files,*.*. '
      MODE             = 'O'
*     TITLE            = ' '
    IMPORTING
      FILENAME         = P_FILE
*     PATH             =
*     FILE             =
    EXCEPTIONS
      SELECTION_CANCEL = 1
      SELECTION_ERROR  = 2
      OTHERS           = 3.
  CASE SY-SUBRC.
    WHEN 0.
    WHEN 2.
      MESSAGE 'Cancel.' TYPE 'S'.
    WHEN OTHERS.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.
ENDFORM.                    "FRM_GET_FILEPATH

*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_INPUT.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR          =
     I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             = T_RAW_DATA
      I_FILENAME                 = P_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = WYT_UPLOAD
*   EXCEPTIONS
*     CONVERSION_FAILED          = 1
*     OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    "FRM_INPUT

*&---------------------------------------------------------------------*
*&      Form  FRM_INFORECORD_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_INFORECORD_MODIFY.

*-->循环获取到的导入数据，进行信息记录编辑
  LOOP AT WYT_UPLOAD INTO WYS_UPLOAD.
    WYS_INFO-LIFNR = WYS_UPLOAD-LIFNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WYS_INFO-LIFNR
      IMPORTING
        OUTPUT = WYS_INFO-LIFNR.
    WYS_INFO-MATNR = WYS_UPLOAD-MATNR.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = WYS_INFO-MATNR
      IMPORTING
        OUTPUT = WYS_INFO-MATNR.

    WYS_INFO-EKORG = WYS_UPLOAD-EKORG.
    WYS_INFO-WERKS = WYS_UPLOAD-WERKS.

    CLEAR WYS_INFO-INFNR.
    SELECT SINGLE EINA~INFNR                          "EINA~MEINS EINA~UMREZ EINA~UMREN
      INTO  WYS_INFO-INFNR                                           "( WYS_INFO-INFNR ,WYS_INFO-MEINS ,WYS_INFO-UMREZ ,WYS_INFO-UMREN )
      FROM EINA
      JOIN EINE ON EINA~INFNR = EINE~INFNR
      WHERE EINA~MATNR = WYS_INFO-MATNR
        AND EINA~LIFNR = WYS_INFO-LIFNR
        AND EINE~EKORG = WYS_INFO-EKORG
        AND EINE~WERKS = WYS_INFO-WERKS.
    "若不存在就新增
   "IF WYS_INFO-INFNR IS INITIAL.
   "   PERFORM FRM_INFO_ADD USING WYS_UPLOAD WYS_INFO.
      "若存在则修改
    "ELSE.
      WYS_UPLOAD-INFNR = WYS_INFO-INFNR.
      PERFORM FRM_INFO_EDIT USING WYS_UPLOAD WYS_INFO.
   "ENDIF.

  ENDLOOP.
ENDFORM.                    "FRM_INFORECORD_MODIFY

*&---------------------------------------------------------------------*
*&      Form  FRM_INFO_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WYS_UPLOAD text
*      -->WYS_INFO   text
*----------------------------------------------------------------------*
FORM FRM_INFO_ADD USING WYS_UPLOAD TYPE TY_UPLOAD
                        WYS_INFO TYPE TY_INFO.
  DATA:GDF_MESSAGE TYPE CHAR255.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0100'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINA-LIFNR'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '/00'.
  PERFORM FRM_BDC_FIELD       USING 'EINA-LIFNR' WYS_INFO-LIFNR.
  PERFORM FRM_BDC_FIELD       USING 'EINA-MATNR' WYS_INFO-MATNR.
  PERFORM FRM_BDC_FIELD       USING 'EINE-EKORG' WYS_INFO-EKORG.
  PERFORM FRM_BDC_FIELD       USING 'EINE-WERKS' WYS_INFO-WERKS.
  PERFORM FRM_BDC_FIELD       USING 'RM06I-NORMB' 'X'.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0101'.
*  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINA-IDNLF'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '/00'.
*  IF   WYS_UPLOAD-IDNLF IS NOT INITIAL.
*  PERFORM FRM_BDC_FIELD       USING 'EINA-IDNLF' WYS_UPLOAD-IDNLF.
*  ENDIF.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0102'.
  "PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINE-NORBM'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '=KO'.
  PERFORM FRM_BDC_FIELD       USING 'EINE-NETPR' WYS_UPLOAD-NETPR.
  PERFORM FRM_BDC_FIELD       USING 'EINE-PEINH' WYS_UPLOAD-PEINH.
  PERFORM FRM_BDC_FIELD       USING 'EINE-MWSKZ' WYS_UPLOAD-MWSKZ.
*  IF  WYS_UPLOAD-NORBM IS  NOT INITIAL.
*  PERFORM FRM_BDC_FIELD       USING 'EINE-NORBM' WYS_UPLOAD-NORBM.
*  ENDIF.
  PERFORM FRM_BDC_FIELD       USING 'EINE-WEBRE' 'X'.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMV13A' '0201'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'RV13A-DATBI'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '=SICH'.
  PERFORM FRM_BDC_FIELD       USING 'RV13A-DATAB' WYS_UPLOAD-DATAB.
  PERFORM FRM_BDC_FIELD       USING 'RV13A-DATBI' WYS_UPLOAD-DATBI.

  CALL TRANSACTION 'ME11' USING GDT_BDCDATA MODE 'N' UPDATE 'S'"UPDATE 'S' 同步更新 *很重要*
                          MESSAGES INTO GDT_BDCMESSAGE.
  IF GDT_BDCMESSAGE IS NOT INITIAL.
    CLEAR:GDS_BDCMESSAGE,GDS_RETURN.
    LOOP AT GDT_BDCMESSAGE INTO GDS_BDCMESSAGE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = GDS_BDCMESSAGE-MSGID
          MSGNR               = GDS_BDCMESSAGE-MSGNR
          MSGV1               = GDS_BDCMESSAGE-MSGV1
          MSGV2               = GDS_BDCMESSAGE-MSGV2
          MSGV3               = GDS_BDCMESSAGE-MSGV3
          MSGV4               = GDS_BDCMESSAGE-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = GDF_MESSAGE.

      "IF GDS_BDCMESSAGE-MSGTYP = 'E'.
      GDS_RETURN-TYPE = GDS_BDCMESSAGE-MSGTYP.
      GDS_RETURN-CODE = GDS_BDCMESSAGE-MSGNR.
      GDS_RETURN-MESSAGE = GDF_MESSAGE.
      GDS_RETURN-MESSAGE_V1 = GDS_BDCMESSAGE-MSGV1.
      GDS_RETURN-MESSAGE_V2 = GDS_BDCMESSAGE-MSGV2.
      GDS_RETURN-MESSAGE_V3 = GDS_BDCMESSAGE-MSGV3.
      GDS_RETURN-MESSAGE_V4 = GDS_BDCMESSAGE-MSGV4.
      APPEND GDS_RETURN TO GDT_RETURN.
      "ENDIF.
      CLEAR:GDS_BDCMESSAGE,GDS_RETURN.
    ENDLOOP.
    READ TABLE GDT_BDCMESSAGE INTO GDS_BDCMESSAGE WITH KEY MSGTYP = 'E'. "如果有错误消息
    IF SY-SUBRC = 0 .
      GDS_RETURN-TYPE = 'E'.
      GDS_RETURN-CODE = '01'.
      CONCATENATE '供应商：' WYS_UPLOAD-LIFNR
                  '物料：' WYS_UPLOAD-MATNR
                  '采购组织：' WYS_UPLOAD-EKORG
                  '工厂：' WYS_UPLOAD-WERKS
      '创建失败' INTO
      GDS_RETURN-MESSAGE .
      APPEND GDS_RETURN TO GDT_RETURN.
    ELSE.
      GDS_RETURN-TYPE = 'S'.
      GDS_RETURN-CODE = '01'.
      CONCATENATE '供应商：' WYS_UPLOAD-LIFNR
                  '物料：' WYS_UPLOAD-MATNR
                  '采购组织：' WYS_UPLOAD-EKORG
                  '工厂：' WYS_UPLOAD-WERKS
      '创建成功' INTO
      GDS_RETURN-MESSAGE .
      APPEND GDS_RETURN TO GDT_RETURN.
    ENDIF.
    CLEAR GDS_BDCMESSAGE.
    CLEAR: GDT_BDCMESSAGE[],GDT_BDCDATA[].
  ENDIF.

ENDFORM.                    "FRM_INFO_ADD

*&---------------------------------------------------------------------*
*&      Form  FRM_INFO_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WYS_UPLOAD text
*      -->WYS_INFO   text
*----------------------------------------------------------------------*
FORM FRM_INFO_EDIT  USING WYS_UPLOAD TYPE TY_UPLOAD
                          WYS_INFO TYPE TY_INFO.
  DATA:GDF_MESSAGE TYPE CHAR255.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0100'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINA-LIFNR'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '/00'.
  PERFORM FRM_BDC_FIELD       USING 'EINA-LIFNR' WYS_INFO-LIFNR.
  PERFORM FRM_BDC_FIELD       USING 'EINA-MATNR' WYS_INFO-MATNR.
  PERFORM FRM_BDC_FIELD       USING 'EINE-EKORG' WYS_INFO-EKORG.
  PERFORM FRM_BDC_FIELD       USING 'EINE-WERKS' WYS_INFO-WERKS.
  PERFORM FRM_BDC_FIELD       USING 'RM06I-NORMB' 'X'.

*  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0101'.
*  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINA-IDNLF'.
*  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '/00'.
*  IF WYS_UPLOAD-IDNLF IS NOT INITIAL.
*    PERFORM FRM_BDC_FIELD       USING 'EINA-IDNLF' WYS_UPLOAD-IDNLF.
*  ENDIF.
  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0101'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '/00'.


  PERFORM FRM_BDC_DYNPRO      USING 'SAPMM06I' '0102'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'EINE-MWSKZ'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '=KO'.
  PERFORM FRM_BDC_FIELD       USING 'EINE-MWSKZ' WYS_UPLOAD-MWSKZ.
*  IF WYS_UPLOAD-NORBM IS NOT INITIAL.
*  PERFORM FRM_BDC_FIELD       USING 'EINE-NORBM' WYS_UPLOAD-NORBM.
*  ENDIF.
  PERFORM FRM_BDC_FIELD       USING 'EINE-WEBRE' 'X'.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPLV14A' '0102'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'VAKE-DATAB(01)'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '=NEWD'.

  PERFORM FRM_BDC_DYNPRO      USING 'SAPMV13A' '0201'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_CURSOR' 'KONP-KBETR(01)'.
  PERFORM FRM_BDC_FIELD       USING 'BDC_OKCODE' '=SICH'.
  PERFORM FRM_BDC_FIELD       USING 'RV13A-DATAB' WYS_UPLOAD-DATAB.
  PERFORM FRM_BDC_FIELD       USING 'RV13A-DATBI' WYS_UPLOAD-DATBI.
  PERFORM FRM_BDC_FIELD       USING 'KONP-KBETR(01)' WYS_UPLOAD-NETPR.
  PERFORM FRM_BDC_FIELD       USING 'KONP-KPEIN(01)' WYS_UPLOAD-PEINH.


  CALL TRANSACTION 'ME12' USING GDT_BDCDATA MODE 'N' UPDATE 'S'"UPDATE 'S' 同步更新 *很重要*
                          MESSAGES INTO GDT_BDCMESSAGE.
  IF GDT_BDCMESSAGE IS NOT INITIAL.
    CLEAR:GDS_BDCMESSAGE,GDS_RETURN.
    LOOP AT GDT_BDCMESSAGE INTO GDS_BDCMESSAGE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = GDS_BDCMESSAGE-MSGID
          MSGNR               = GDS_BDCMESSAGE-MSGNR
          MSGV1               = GDS_BDCMESSAGE-MSGV1
          MSGV2               = GDS_BDCMESSAGE-MSGV2
          MSGV3               = GDS_BDCMESSAGE-MSGV3
          MSGV4               = GDS_BDCMESSAGE-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = GDF_MESSAGE.

      IF GDS_BDCMESSAGE-MSGTYP = 'E'.
        GDS_RETURN-TYPE = GDS_BDCMESSAGE-MSGTYP.
        GDS_RETURN-CODE = GDS_BDCMESSAGE-MSGNR.
        GDS_RETURN-MESSAGE = GDF_MESSAGE.
        GDS_RETURN-MESSAGE_V1 = GDS_BDCMESSAGE-MSGV1.
        GDS_RETURN-MESSAGE_V2 = GDS_BDCMESSAGE-MSGV2.
        GDS_RETURN-MESSAGE_V3 = GDS_BDCMESSAGE-MSGV3.
        GDS_RETURN-MESSAGE_V4 = GDS_BDCMESSAGE-MSGV4.
        APPEND GDS_RETURN TO GDT_RETURN.
      ENDIF.
      CLEAR:GDS_BDCMESSAGE,GDS_RETURN.
    ENDLOOP.
    READ TABLE GDT_BDCMESSAGE INTO GDS_BDCMESSAGE WITH KEY MSGTYP = 'E'. "如果有错误消息
    IF SY-SUBRC = 0 .
      GDS_RETURN-TYPE = 'E'.
      GDS_RETURN-CODE = '01'.
      CONCATENATE '供应商：' WYS_UPLOAD-LIFNR
                  '物料：' WYS_UPLOAD-MATNR
                  '采购组织：' WYS_UPLOAD-EKORG
                  '工厂：' WYS_UPLOAD-WERKS
                  '信息记录:'WYS_UPLOAD-INFNR
      '修改失败' INTO
      GDS_RETURN-MESSAGE .
      APPEND GDS_RETURN TO GDT_RETURN.
    ELSE.
      GDS_RETURN-TYPE = 'S'.
      GDS_RETURN-CODE = '01'.
      CONCATENATE '供应商：' WYS_UPLOAD-LIFNR
                  '物料：' WYS_UPLOAD-MATNR
                  '采购组织：' WYS_UPLOAD-EKORG
                  '工厂：' WYS_UPLOAD-WERKS
                  '信息记录:'WYS_UPLOAD-INFNR
      '修改成功' INTO
      GDS_RETURN-MESSAGE .
      APPEND GDS_RETURN TO GDT_RETURN.
    ENDIF.
    CLEAR GDS_BDCMESSAGE.
    CLEAR: GDT_BDCMESSAGE[],GDT_BDCDATA[].
  ENDIF.

ENDFORM.                    "FRM_INFO_EDIT

*&---------------------------------------------------------------------*
*&      Form  frm_bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM FRM_BDC_DYNPRO USING PROGRAM
                      DYNPRO.
  CLEAR GDS_BDCDATA.
  GDS_BDCDATA-PROGRAM = PROGRAM.
  GDS_BDCDATA-DYNPRO  = DYNPRO.
  GDS_BDCDATA-DYNBEGIN = 'X'.
  APPEND GDS_BDCDATA TO GDT_BDCDATA.
ENDFORM. "frm_bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  frm_bdc_field
*&---------------------------------------------------------------------*
FORM FRM_BDC_FIELD USING FNAM FVAL.
  CLEAR GDS_BDCDATA.
  GDS_BDCDATA-FNAM = FNAM.
  GDS_BDCDATA-FVAL = FVAL.
  CONDENSE  GDS_BDCDATA-FVAL." è￥????
  APPEND GDS_BDCDATA TO GDT_BDCDATA.
ENDFORM. "frm_bdc_field

*&---------------------------------------------------------------------*
*&      Form  PRM_WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRM_WRITE_LOG .
  CLEAR LIT_FCAT[].
  G_REPID = SY-REPID.
  WA_LAYOUT1-COLWIDTH_OPTIMIZE = 'X'.
  WA_LAYOUT1-ZEBRA = 'X'.

  PERFORM PRM_ALV_FIELDCAT USING 'TYPE'  ''  '' '消息类型'  1.
  PERFORM PRM_ALV_FIELDCAT USING 'CODE'  ''  '' '消息代码'  2.
  PERFORM PRM_ALV_FIELDCAT USING 'MESSAGE'  ''  '' '消息文本'  3.
  PERFORM PRM_ALV_FIELDCAT USING 'LOG_NO'  ''  '' '日志号' 4.
  PERFORM PRM_ALV_FIELDCAT USING 'LOG_MSG_NO'  ''  '' '内部邮件序列号'5.
  PERFORM PRM_ALV_FIELDCAT USING 'MESSAGE_V1'  ''  '' '消息变量' 6.
  PERFORM PRM_ALV_FIELDCAT USING 'MESSAGE_V2'  ''  '' '消息变量' 7.
  PERFORM PRM_ALV_FIELDCAT USING 'MESSAGE_V3'  ''  '' '消息变量' 8.
  PERFORM PRM_ALV_FIELDCAT USING 'MESSAGE_V4'  ''  '' '消息变量' 9.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
*     i_callback_pf_status_set = 'F_SETSTATUS'
*     i_callback_user_command  = 'F_USER_COMMAND'
*      I_SCREEN_START_COLUMN = 5
*      I_SCREEN_START_LINE = 5
*      I_SCREEN_END_COLUMN = 80
*      I_SCREEN_END_LINE = 15
      IS_LAYOUT                = WA_LAYOUT1
      IT_FIELDCAT              = LIT_FCAT
      I_SAVE                   = 'X'
    TABLES
      T_OUTTAB                 = GDT_RETURN
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM. " PRM_WRITE_LOG

*&---------------------------------------------------------------------*
*&      Form  PRM_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FIELDNAME      text
*      -->REF_FIELDNAME  text
*      -->REF_TABNAME    text
*      -->FIELDTEXT      text
*      -->COL_POS        text
*----------------------------------------------------------------------*
FORM PRM_ALV_FIELDCAT USING FIELDNAME REF_FIELDNAME REF_TABNAME FIELDTEXT COL_POS.
  WA_ALV_FIELD-FIELDNAME = FIELDNAME.
  WA_ALV_FIELD-REF_FIELDNAME = REF_FIELDNAME.
  WA_ALV_FIELD-REF_TABNAME  = REF_TABNAME.
  WA_ALV_FIELD-SELTEXT_S = FIELDTEXT.
  WA_ALV_FIELD-SELTEXT_L = FIELDTEXT.
  WA_ALV_FIELD-SELTEXT_M = FIELDTEXT.
  WA_ALV_FIELD-REPTEXT_DDIC = FIELDTEXT.
  WA_ALV_FIELD-COL_POS = COL_POS.
  WA_ALV_FIELD-NO_ZERO = 'X'.
  APPEND WA_ALV_FIELD TO LIT_FCAT.
  CLEAR WA_ALV_FIELD.

ENDFORM. "prm_alv_fieldcat

FORM FRM_DOWNLOAD.

  DATA: FILENAME TYPE STRING.

  CLEAR:  IT_DOWNLOAD,
          WA_DOWNLOAD.

  WA_DOWNLOAD-LIFNR = '供应商'.
  WA_DOWNLOAD-NAME1 = '供应商描述'.
  WA_DOWNLOAD-MATNR = '物料'.
  WA_DOWNLOAD-MAKTX = '物料描述'.
  WA_DOWNLOAD-EKORG = '采购组织'.
  WA_DOWNLOAD-WERKS = '工厂'.
  "WA_DOWNLOAD-ESOKZ = '信息类别'.
  "WA_DOWNLOAD-IDNLF = '供应商物料编码'.
 " WA_DOWNLOAD-NORBM = '标准数量'.
*  WA_DOWNLOAD-MEINS = '计量单位'.
*  WA_DOWNLOAD-UMREZ = '转换分子'.
*  WA_DOWNLOAD-UMREN = '转换分母'.

  WA_DOWNLOAD-NETPR = '含税价'.
  WA_DOWNLOAD-PEINH = '价格单位'.
  WA_DOWNLOAD-MWSKZ = '材料税代码'.
  WA_DOWNLOAD-DATAB = '有效开始日期'.
  WA_DOWNLOAD-DATBI = '有效结束日期'.

  APPEND WA_DOWNLOAD TO IT_DOWNLOAD.
  CONCATENATE P_FILE '.XLS' INTO FILENAME.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      FILENAME                        = FILENAME
     FILETYPE                        = 'DAT'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      DATA_TAB                        = IT_DOWNLOAD
*     FIELDNAMES                      =
*   EXCEPTIONS
*     FILE_WRITE_ERROR                = 1
*     NO_BATCH                        = 2
*     GUI_REFUSE_FILETRANSFER         = 3
*     INVALID_TYPE                    = 4
*     NO_AUTHORITY                    = 5
*     UNKNOWN_ERROR                   = 6
*     HEADER_NOT_ALLOWED              = 7
*     SEPARATOR_NOT_ALLOWED           = 8
*     FILESIZE_NOT_ALLOWED            = 9
*     HEADER_TOO_LONG                 = 10
*     DP_ERROR_CREATE                 = 11
*     DP_ERROR_SEND                   = 12
*     DP_ERROR_WRITE                  = 13
*     UNKNOWN_DP_ERROR                = 14
*     ACCESS_DENIED                   = 15
*     DP_OUT_OF_MEMORY                = 16
*     DISK_FULL                       = 17
*     DP_TIMEOUT                      = 18
*     FILE_NOT_FOUND                  = 19
*     DATAPROVIDER_EXCEPTION          = 20
*     CONTROL_FLUSH_ERROR             = 21
*     OTHERS                          = 22
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF SY-SUBRC = 0.
    MESSAGE '模板已成功导出' TYPE 'S'.
  ENDIF.

ENDFORM.
