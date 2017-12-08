REPORT ZMM020.
*&---------------------------------------------------------------------*
*& 程序名称:ZMM020
*& 作者    :汪昱
*& 开发日期:
*& 请求号  :
*& 描述    :劳务采购订单批导
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

TYPE-POOLS: SLIS.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        PO_OUTSIDE  TYPE STRING,            "外围汇总采购订单号
        BSART       TYPE EKKO-BSART,        "订单类型
        LIFNR       TYPE EKKO-LIFNR,        "供应商
        EKORG       TYPE EKKO-EKORG,        "采购组织
        EKGRP       TYPE EKKO-EKGRP,        "采购组
        BUKRS       TYPE EKKO-BUKRS,        "公司代码
        EBELP       TYPE EKPO-EBELP,        "行项目号
        KNTTP       TYPE EKPO-KNTTP,        "科目分配类别
        DISUB_PSPNR TYPE PS_POSID,          "WBS元素
        MATNR       TYPE EKPO-MATNR,        "物料编码
        TXZ01       TYPE EKPO-TXZ01,        "短文本
        MATKL       TYPE EKPO-MATKL,        "物料组
        MENGE       TYPE   C LENGTH 16,     "净价（不含税价
        NETPR       TYPE   C LENGTH 13,     "净价（含税价
        MWSKZ       TYPE EKPO-MWSKZ,        "税码
        WERKS       TYPE EKPO-WERKS,        "工厂
        BEDNR       TYPE EKPO-BEDNR,        "需求跟踪号
        BZ          TYPE STRING,            "备注
*        BPRME       TYPE EKPO-BPRME,        "订单价格单位
      END OF TY_DATA.

TYPES:BEGIN OF TY_OUTPUT,
        PO_OUTSIDE  TYPE STRING,            "外围汇总采购订单号
        BSART       TYPE EKKO-BSART,        "订单类型
        LIFNR       TYPE EKKO-LIFNR,        "供应商
        EBELP       TYPE EKPO-EBELP,        "行项目号
        EKORG       TYPE EKKO-EKORG,        "采购组织
        EKGRP       TYPE EKKO-EKGRP,        "采购组
        BUKRS       TYPE EKKO-BUKRS,        "公司代码
        KNTTP       TYPE EKPO-KNTTP,        "科目分配类别
        DISUB_PSPNR TYPE PS_POSID,          "WBS元素
        MATNR       TYPE EKPO-MATNR,        "物料编码
        TXZ01       TYPE EKPO-TXZ01,        "短文本
        MATKL       TYPE EKPO-MATKL,        "物料组
        MENGE       TYPE EKPO-MENGE,        "数量
        NETPR       TYPE EKPO-NETPR,        "含税价
        MWSKZ       TYPE EKPO-MWSKZ,        "税码
        WERKS       TYPE EKPO-WERKS,        "工厂
        BEDNR       TYPE EKPO-BEDNR,        "需求跟踪号
        BZ          TYPE STRING,            "备注
*        BPRME       TYPE EKPO-BPRME,        "订单价格单位
        EBELN       TYPE EKKO-EBELN,        "采购订单号
        TYPE        TYPE ICON-NAME,         "消息类型
        MESSAGE     TYPE STRING,            "消息
      END OF TY_OUTPUT.
*----------------------------------------------------------------------*
*ALV data declarations
*----------------------------------------------------------------------*
DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
      GD_REPID     LIKE SY-REPID.

DATA GT_DATA TYPE TABLE OF TY_DATA.
DATA GS_DATA TYPE TY_DATA.

DATA GT_OUTPUT  TYPE TABLE OF TY_OUTPUT.
DATA GS_OUTPUT  TYPE TY_OUTPUT.
DATA GS_OUTPUT1 TYPE TY_OUTPUT.

*----------------------------------------------------------------------*
*BAPI DATA data declarations
*----------------------------------------------------------------------*
DATA:
  POHEADER                TYPE BAPIMEPOHEADER,
  POHEADERX               TYPE BAPIMEPOHEADERX,
  POADDRVENDOR            TYPE BAPIMEPOADDRVENDOR,
  RETURN                  TYPE STANDARD TABLE OF BAPIRET2                WITH HEADER LINE,
  POITEM                  TYPE STANDARD TABLE OF BAPIMEPOITEM            WITH HEADER LINE,
  POITEMX                 TYPE STANDARD TABLE OF BAPIMEPOITEMX           WITH HEADER LINE,
  POACCOUNT               TYPE STANDARD TABLE OF BAPIMEPOACCOUNT         WITH HEADER LINE,
  POACCOUNTX              TYPE STANDARD TABLE OF BAPIMEPOACCOUNTX        WITH HEADER LINE,
  BAPIMEPOSCHEDULE        TYPE STANDARD TABLE OF BAPIMEPOSCHEDULE        WITH HEADER LINE,
  BAPIMEPOSCHEDULX        TYPE STANDARD TABLE OF BAPIMEPOSCHEDULX        WITH HEADER LINE,
  BAPIMEPOTEXTHEADER      TYPE STANDARD TABLE OF BAPIMEPOTEXTHEADER      WITH HEADER LINE,
  BAPIMEPOTEXT            TYPE STANDARD TABLE OF BAPIMEPOTEXT            WITH HEADER LINE,
  BAPI_TE_MEPOHEADER      TYPE STANDARD TABLE OF BAPI_TE_MEPOHEADER      WITH HEADER LINE,
  BAPI_TE_MEPOHEADERX     TYPE STANDARD TABLE OF BAPI_TE_MEPOHEADERX     WITH HEADER LINE,
  BAPI_TE_MEPOITEM        TYPE STANDARD TABLE OF BAPI_TE_MEPOITEM        WITH HEADER LINE,
  BAPI_TE_MEPOITEMX       TYPE STANDARD TABLE OF BAPI_TE_MEPOITEMX       WITH HEADER LINE,
  BAPI_TE_MEPOACCOUNTING  TYPE STANDARD TABLE OF BAPI_TE_MEPOACCOUNTING  WITH HEADER LINE,
  BAPI_TE_MEPOACCOUNTINGX TYPE STANDARD TABLE OF BAPI_TE_MEPOACCOUNTINGX WITH HEADER LINE,
  EXTENSIONIN             TYPE STANDARD TABLE OF BAPIPAREX               WITH HEADER LINE.

DATA: LW_ITEM             TYPE BAPIMECONFITEM,
      LT_ITEM             TYPE BAPIMECONF_T_ITEM,
      LW_ITEMX            TYPE BAPIMECONFITEMX,
      LT_ITEMX            TYPE BAPIMECONF_T_ITEMX,
      LT_CONFIRMATION     TYPE BAPIMECONF_T_DETAIL,
      LW_CONFIRMATION     TYPE BAPIMECONFDETAIL,
      LT_CONFIRMATIONX    TYPE BAPIMECONF_T_DETAILX,
      LW_CONFIRMATIONX    TYPE BAPIMECONFDETAILX,
      LW_EXP_CONFIRMATION TYPE BAPIMECONFDETAIL,
      LT_EXP_CONFIRMATION TYPE STANDARD TABLE OF BAPIMECONF_T_DETAIL,
      RETURN2             TYPE  BAPICONF_T_RETURN,
      W_RETURN2           LIKE LINE OF RETURN2.

DATA: POHEADER_CHANGE       TYPE BAPIMEPOHEADER,
      POHEADERX_CHANGE      TYPE BAPIMEPOHEADERX,
      RETURN_CHANGE         TYPE STANDARD TABLE OF BAPIRET2           WITH HEADER LINE,
      POITEM_CHANGE         TYPE STANDARD TABLE OF BAPIMEPOITEM       WITH HEADER LINE,
      POITEMX_CHANGE        TYPE STANDARD TABLE OF BAPIMEPOITEMX      WITH HEADER LINE,
      POSCHEDULE_CHANGE     TYPE STANDARD TABLE OF BAPIMEPOSCHEDULE   WITH HEADER LINE,
      POSCHEDULEX_CHANGE    TYPE STANDARD TABLE OF BAPIMEPOSCHEDULX   WITH HEADER LINE,
      POTEXTHEADER_CHANGE   TYPE STANDARD TABLE OF BAPIMEPOTEXTHEADER WITH HEADER LINE,
      POTEXTITEM_CHANGE     TYPE STANDARD TABLE OF BAPIMEPOTEXT       WITH HEADER LINE,
      POCONFIRMATION_CHANGE TYPE STANDARD TABLE OF BAPIEKES           WITH HEADER LINE,
      EXTENSIONIN_CHANGE    TYPE STANDARD TABLE OF BAPIPAREX          WITH HEADER LINE.


*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:P_UP TYPE CHAR1   RADIOBUTTON  GROUP  G1 DEFAULT 'X' USER-COMMAND G_UCMD,
          P_DOWN TYPE CHAR1 RADIOBUTTON  GROUP  G1 .
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS: PO TYPE RLGRAP-FILENAME MODIF ID ZUP MEMORY ID ZMM020. "采购订单导入摸版
SELECTION-SCREEN END OF BLOCK BLK2.

SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_DO TYPE RLGRAP-FILENAME MODIF ID  ZDO. "模版下载
SELECTION-SCREEN END OF BLOCK BLK3.
*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'ZUP'.
      IF P_UP = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-GROUP1 = 'ZDO'.
      IF P_DOWN = 'X'.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PO.
  PERFORM SELECT_PATH."选择路径

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DO.
  PERFORM SELECT_PATH_1."选择路径

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

START-OF-SELECTION.
  IF P_UP = 'X'.
    PERFORM UPLOAD_PO_DATA."数据导入

    PERFORM BUILD_FIELDCATALOG.

    PERFORM BUILD_LAYOUT.

    PERFORM DISPLAY_ALV_REPORT TABLES GT_OUTPUT.
  ELSE.
    PERFORM FRM_DOWNLOAD  .
  ENDIF.

*&---------------------------------------------------------------------*
*& Form SELECT_PATH
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*

FORM SELECT_PATH .
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.* ,*.*.'
      MODE             = '0'
      TITLE            = '请选择要上传的信息文件'
    IMPORTING
      FILENAME         = PO
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " SELECT_PATH
*&---------------------------------------------------------------------*
*&      Form  SELECT_PATH_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_PATH_1 .
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.* ,*.*.'
      MODE             = '0'
      TITLE            = '请选择要上传的信息文件'
    IMPORTING
      FILENAME         = P_DO
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_DATA1  text
*----------------------------------------------------------------------*
FORM FRM_XLS_TO_SAP  TABLES P_I_DATA .

  REFRESH  P_I_DATA[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = PO
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 3
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = P_I_DATA[]
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN  '1'.
        MESSAGE '存在错误的传入参数' TYPE 'E'.
      WHEN  '2'.
        MESSAGE 'OLE控件错误，请检查EXCEL插件及系统' TYPE 'E'.
      WHEN  '3'.
        MESSAGE '数据上载出错' TYPE 'E'.
      WHEN OTHERS.
    ENDCASE.

*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF P_I_DATA[] IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.
ENDFORM.                    " FRM_XLS_TO_SAP
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_PO_DATA .
  PERFORM FRM_XLS_TO_SAP TABLES GT_DATA.
  PERFORM FRM_FILL_DATA1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
  PERFORM FIELD_CAT USING :
       'TYPE'    '消息类型' '',
        'MESSAGE' '消息内容' '',
        'EBELN'   '采购订单号' '',
        'PO_OUTSIDE'    '外围汇总采购订单号' '',
        'BSART'   '订单类型' '',
        'LIFNR'   '供应商'   '',
        'EKORG'   '采购组织'  '',
        'EKGRP'   '采购组'  '',
        'BUKRS'   '公司代码'  '',
        'EBELP'   '行项目号'  '',
        'KNTTP'   '科目分配类别'  '',
        'DISUB_PSPNR'  'WBS元素' '',
        'MATNR'   '物料编码'  '',
        'TXZ01'   '短文本'  '',
        'NETPR'   '含税价'  '',
        'MATKL'   '物料组'  '',
        'MENGE'   '数量'   '',
        'NETPR'   '含税价'  '',
        'MWSKZ'   '税码'   '',
        'WERKS'   '工厂'  '',
        'BEDNR'   '需求跟踪号' '',
        'BZ'      '备注' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GD_LAYOUT-ZEBRA = 'X'.

* GD_LAYOUT-GROUP_CHANGE_EDIT = 'X'.

  GD_LAYOUT-HEADER_TEXT = '导入结果查询'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA1 .

  MOVE-CORRESPONDING GT_DATA TO GT_OUTPUT.
  SORT GT_OUTPUT BY PO_OUTSIDE BSART LIFNR EBELP.

  DATA FLAG  TYPE C.
  DATA FLAG1 TYPE C.
  DATA L_TABIX TYPE SY-TABIX.

  LOOP AT  GT_OUTPUT INTO GS_OUTPUT.
    CLEAR FLAG.
    AT NEW PO_OUTSIDE.
      FLAG  = 'X'.
    ENDAT.

*抬头
    IF FLAG = 'X'.
      CLEAR: POHEADER,POHEADERX.
      POHEADER-DOC_TYPE   = GS_OUTPUT-BSART. "订单类型
*    POHEADER-PO_NUMBER  = IT_HEAD-EBELN. "采购订单号
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_OUTPUT-LIFNR
        IMPORTING
          OUTPUT = GS_OUTPUT-LIFNR.
      POHEADER-VENDOR     = GS_OUTPUT-LIFNR. "供应商

      POHEADER-PURCH_ORG  = GS_OUTPUT-EKORG. "采购组织
      POHEADER-PUR_GROUP  = GS_OUTPUT-EKGRP. "采购组
      POHEADER-COMP_CODE  = GS_OUTPUT-BUKRS. "公司代码
*    POHEADER-PMNTTRMS   = GS_DATA-ZTERM.    "付款条件代码
*    POHEADER-CURRENCY   = GS_DATA-WAERS.    "币别

      POHEADERX-DOC_TYPE   = 'X'.            "订单类型
      POHEADERX-COMP_CODE  = 'X'.            "公司代码
      POHEADERX-PO_NUMBER  = 'X'.            "采购订单号
      POHEADERX-VENDOR     = 'X'.            "供应商
      POHEADERX-PURCH_ORG  = 'X'.            "采购组织
      POHEADERX-PUR_GROUP  = 'X'.            "采购组
*    POHEADERX-PMNTTRMS   = 'X'.             "付款条件代码
*    POHEADERX-CURRENCY   = 'X'.             "币别

*外部合同编号
      IF GS_OUTPUT-PO_OUTSIDE IS NOT INITIAL.
        CLEAR BAPIMEPOTEXTHEADER.
*      BAPIMEPOTEXT-PO_NUMBER   =  GS_OUTPUT-EBELN. "采购订单号
*      BAPIMEPOTEXTHEADER-PO_ITEM     =  GS_OUTPUT-EBELP. "采购订单行项目
        BAPIMEPOTEXTHEADER-TEXT_ID     =  'F06'.
        BAPIMEPOTEXTHEADER-TEXT_FORM   =  '*'.
        BAPIMEPOTEXTHEADER-TEXT_LINE   =  GS_OUTPUT-PO_OUTSIDE. "外围汇总采购订单号
        APPEND BAPIMEPOTEXTHEADER.
      ENDIF.

    ENDIF.

*行项目
    POITEM-PO_ITEM      =  GS_OUTPUT-EBELP.  "采购订单行项目
    POITEM-ACCTASSCAT   =  GS_OUTPUT-KNTTP.  "科目分配类别

*&--代码添加 BY HANDYBY 09.06.2017 15:20:23  BEGIN
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = GS_OUTPUT-MATNR
      IMPORTING
        OUTPUT       = GS_OUTPUT-MATNR
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
*&--代码添加 BY HANDYBY 09.06.2017 15:20:23  END

    POITEM-MATERIAL     =  GS_OUTPUT-MATNR.  "物料号
    POITEM-SHORT_TEXT   =  GS_OUTPUT-TXZ01.  "物料描述
    POITEM-QUANTITY     =  GS_OUTPUT-MENGE.  "采购订单数量
    POITEM-NET_PRICE    =  GS_OUTPUT-NETPR.  "净价（不含税价）
*    POITEM-PO_PRICE     = '2'.
    POITEM-PLANT        =  GS_OUTPUT-WERKS.   "工厂
    POITEM-TAX_CODE     =  GS_OUTPUT-MWSKZ.   "税码
    POITEM-CONV_NUM1    = '1'.                "订单价格单位分子
    POITEM-CONV_DEN1    = '1'.                "订单价格单位分母
    POITEM-MATL_GROUP   =  GS_OUTPUT-MATKL.   "物料组
    POITEM-TRACKINGNO   =  GS_OUTPUT-BEDNR.   "需求跟踪号
    POITEM-PO_UNIT      = 'PC'.               "订单单位
*    POITEM-ORDERPR_UN   =  GS_OUTPUT-BPRME.   "订单价格单位
    APPEND POITEM.

    POITEMX-PO_ITEM      =  GS_OUTPUT-EBELP.  "采购订单行项目
    POITEMX-ACCTASSCAT   =  'X'.              "科目分配类别

    IF POITEM-MATERIAL IS NOT INITIAL.
      POITEMX-MATERIAL     =  'X'.              "采购订单物料号
    ELSE.
      POITEMX-SHORT_TEXT   =  'X'.              "物料描述
      POITEMX-PO_UNIT      =  'X'.              "订单单位
      POITEMX-MATL_GROUP   =  'X'.              "物料组
    ENDIF.

    POITEMX-QUANTITY     =  'X'.              "采购订单数量
    POITEMX-NET_PRICE    =  'X'.              "净价（不含税价）
    POITEMX-PO_PRICE     =  'X'.
    POITEMX-PRICE_UNIT   =  'X'.              "价格单位
    POITEMX-PLANT        =  'X'.              "工厂
    POITEMX-STGE_LOC     =  'X'.              "库存地点
    POITEMX-TAX_CODE     =  'X'.              "税码
    POITEMX-CONV_NUM1    =  'X'.              "订单价格单位分子
    POITEMX-CONV_DEN1    =  'X'.              "订单价格单位分母
    POITEMX-TRACKINGNO   =  'X'.              "需求跟踪号
*    POITEMX-ORDERPR_UN   =  'X'.              "订单价格单位
    APPEND POITEMX.

*科目分配 成本中心 承若项目
    IF  GS_OUTPUT-DISUB_PSPNR IS NOT INITIAL.
      CLEAR:POACCOUNT, POACCOUNTX.
      POACCOUNT-PO_ITEM      =  GS_OUTPUT-EBELP. "行项目
      POACCOUNT-SERIAL_NO    = '01'.
      POACCOUNTX-PO_ITEM     = GS_OUTPUT-EBELP. "行项目
      POACCOUNTX-SERIAL_NO   = '01'.
      POACCOUNTX-PO_ITEMX    = 'X'.

      IF GS_OUTPUT-DISUB_PSPNR IS NOT INITIAL.
        POACCOUNT-WBS_ELEMENT   = GS_OUTPUT-DISUB_PSPNR."WBS元素
        POACCOUNTX-WBS_ELEMENT  = 'X'.
      ENDIF.

      APPEND  POACCOUNT.
      APPEND  POACCOUNTX.
    ENDIF.

*行项目文本
    IF GS_OUTPUT-BZ IS NOT INITIAL.
      CLEAR BAPIMEPOTEXT.
*      BAPIMEPOTEXT-PO_NUMBER   =  GS_OUTPUT-EBELN. "采购订单号
      BAPIMEPOTEXT-PO_ITEM     =  GS_OUTPUT-EBELP. "采购订单行项目
      BAPIMEPOTEXT-TEXT_ID     =  'F01'.
      BAPIMEPOTEXT-TEXT_FORM   =  '*'.
      BAPIMEPOTEXT-TEXT_LINE   =  GS_OUTPUT-BZ. "备注
      APPEND BAPIMEPOTEXT.
    ENDIF.

    CLEAR FLAG1.
    AT END OF PO_OUTSIDE.
      FLAG1 = 'X'.
    ENDAT.

    IF FLAG1 = 'X'.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          POHEADER     = POHEADER
          POHEADERX    = POHEADERX
          POADDRVENDOR = POADDRVENDOR
        TABLES
          RETURN       = RETURN[]
          POITEM       = POITEM[]
          POITEMX      = POITEMX[]
          POSCHEDULE   = BAPIMEPOSCHEDULE[]
          POSCHEDULEX  = BAPIMEPOSCHEDULX[]
          POACCOUNT    = POACCOUNT[]
          POACCOUNTX   = POACCOUNTX[]
          EXTENSIONIN  = EXTENSIONIN[]
          POTEXTITEM   = BAPIMEPOTEXT[]
          POTEXTHEADER = BAPIMEPOTEXTHEADER[].

      READ TABLE RETURN
      WITH KEY   TYPE  = 'E'.
      IF SY-SUBRC = 0.
        CLEAR L_TABIX.
        LOOP AT GT_OUTPUT INTO GS_OUTPUT1
        WHERE PO_OUTSIDE = GS_OUTPUT-PO_OUTSIDE.

*计算工时
          L_TABIX = SY-TABIX.
          GS_OUTPUT1-TYPE = ICON_RED_LIGHT.

          LOOP AT RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
*          AND ROW =  L_TABIX.
            CONCATENATE GS_OUTPUT1-MESSAGE
            RETURN-MESSAGE INTO GS_OUTPUT1-MESSAGE.
          ENDLOOP.

          MODIFY GT_OUTPUT FROM GS_OUTPUT1.
          CLEAR GS_OUTPUT1.
        ENDLOOP.
        ROLLBACK WORK.
      ELSE.
        READ TABLE RETURN WITH KEY TYPE = 'S'.
        IF SY-SUBRC = 0.
          LOOP AT GT_OUTPUT INTO GS_OUTPUT1
           WHERE PO_OUTSIDE = GS_OUTPUT-PO_OUTSIDE.
            GS_OUTPUT1-TYPE    = ICON_GREEN_LIGHT.
            GS_OUTPUT1-EBELN   = RETURN-MESSAGE_V2.
            GS_OUTPUT1-MESSAGE = '采购订单创建成功'.
            MODIFY GT_OUTPUT FROM GS_OUTPUT1.
            CLEAR GS_OUTPUT1.
          ENDLOOP.
          COMMIT WORK AND WAIT .
        ELSE.
          CLEAR L_TABIX.
          LOOP AT GT_OUTPUT INTO GS_OUTPUT1
            WHERE PO_OUTSIDE = GS_OUTPUT-PO_OUTSIDE.
            GS_OUTPUT1-TYPE = ICON_RED_LIGHT.
*计算工时
*            L_TABIX = SY-TABIX.

            LOOP AT RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
*             AND ROW =  L_TABIX.
              CONCATENATE GS_OUTPUT1-MESSAGE
              RETURN-MESSAGE INTO GS_OUTPUT1-MESSAGE.
            ENDLOOP.

            MODIFY GT_OUTPUT FROM GS_OUTPUT1.
            CLEAR GS_OUTPUT1.
          ENDLOOP.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

      REFRESH:RETURN[],
              POITEM[],
              POITEMX[],
              BAPIMEPOSCHEDULE[],
              BAPIMEPOSCHEDULX[],
              POACCOUNT[],
              POACCOUNTX[],
              EXTENSIONIN[],
              BAPIMEPOTEXT[],
              BAPIMEPOTEXTHEADER[].
    ENDIF.
    CLEAR:RETURN,
        POITEM,
        POITEMX,
        BAPIMEPOSCHEDULE,
        BAPIMEPOSCHEDULX,
        POACCOUNT,
        POACCOUNTX,
        EXTENSIONIN,
        BAPIMEPOTEXT,
        BAPIMEPOTEXTHEADER.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3775   text
*      -->P_3776   text
*      -->P_3777   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    PNAME
                         PTEXT
                         PKEY.
  CLEAR FIELDCATALOG.
  FIELDCATALOG-FIELDNAME = PNAME.
  FIELDCATALOG-SELTEXT_M = PTEXT.
  FIELDCATALOG-KEY       = PKEY.
  APPEND FIELDCATALOG TO FIELDCATALOG.

ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
* Display report using ALV grid
*----------------------------------------------------------------------*

FORM DISPLAY_ALV_REPORT TABLES TDATA.
  GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GD_REPID
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_grid_title       = outtext
      IS_LAYOUT          = GD_LAYOUT
      IT_FIELDCAT        = FIELDCATALOG[]
*     it_special_groups  = gd_tabgroup
*     IT_EVENTS          = GT_XEVENTS
      I_SAVE             = 'A'
*     is_variant         = z_template
    TABLES
      T_OUTTAB           = TDATA
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD .
  DATA: L_OBJDATA     LIKE WWWDATATAB,
        L_MIME        LIKE W3MIME,
        L_DESTINATION LIKE RLGRAP-FILENAME,
        L_OBJNAM      TYPE STRING,
        L_RC          LIKE SY-SUBRC,
        L_ERRTXT      TYPE STRING.

  DATA: L_FILENAME TYPE STRING,
        L_RESULT,
        L_SUBRC    TYPE SY-SUBRC.

  DATA: L_OBJID TYPE WWWDATATAB-OBJID .


*从服务器上取出上传模版
  DATA:  LS_WWWDATA_ITEM LIKE WWWDATATAB.
  SELECT SINGLE *
  INTO CORRESPONDING FIELDS OF LS_WWWDATA_ITEM
  FROM WWWDATA WHERE OBJID = 'ZMM020' .

  "判断模版不存在则报错
  IF SY-SUBRC NE 0 OR LS_WWWDATA_ITEM-OBJID EQ SPACE.
    CONCATENATE '模板文件：' L_OBJID '不存在，请用TCODE：SMW0进行加载'
    INTO L_ERRTXT.
    MESSAGE E000(SU) WITH L_ERRTXT.
  ENDIF.

  L_FILENAME = P_DO.

  "判断本地地址是否已经存在此文件。
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = L_FILENAME
    RECEIVING
      RESULT               = L_RESULT
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.
  IF L_RESULT EQ 'X'.  "如果存在则删除原始文件，重新覆盖
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
      EXPORTING
        FILENAME             = L_FILENAME
      CHANGING
        RC                   = L_SUBRC
      EXCEPTIONS
        FILE_DELETE_FAILED   = 1
        CNTL_ERROR           = 2
        ERROR_NO_GUI         = 3
        FILE_NOT_FOUND       = 4
        ACCESS_DENIED        = 5
        UNKNOWN_ERROR        = 6
        NOT_SUPPORTED_BY_GUI = 7
        WRONG_PARAMETER      = 8
        OTHERS               = 9.
    IF L_SUBRC <> 0. "如果删除失败，则报错。
      CONCATENATE '同名EXCEL文件已打开' '请关闭该EXCEL后重试。'
      INTO L_ERRTXT.
      MESSAGE E000(SU) WITH L_ERRTXT.
    ENDIF.
  ENDIF.

  L_DESTINATION   = P_DO.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT' "
    EXPORTING
      KEY         = LS_WWWDATA_ITEM
      DESTINATION = L_DESTINATION.
  IF L_RC NE 0.
    CONCATENATE '模板文件' '下载失败' INTO L_ERRTXT.
    MESSAGE E000(SU) WITH L_ERRTXT.
  ENDIF.
ENDFORM.
