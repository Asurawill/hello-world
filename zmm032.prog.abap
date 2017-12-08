REPORT ZMM032.
"供应商主数据导入
"add   it02-weiyun(魏云)
"date  20160526

"导入模版数据
TYPES:BEGIN OF TY_INPUT,
        XH(10)     TYPE C ,       "序号
        BUKRS      TYPE BUKRS,    "公司代码
        EKORG      TYPE EKORG,     "采购组织
        KTOKK      TYPE KTOKK,    "账户组
        ANRED      TYPE ANRED,    "标题
        NAME1      TYPE NAME1_GP, "名称1
        NAME2      TYPE NAME2_GP,  "名称2
        NAME3      TYPE NAME3_GP,  "名称3
        SORT1      TYPE SORTL,     "搜索项
        STREET     TYPE AD_STREET, "住宅街道
        HOUSE_NUM1 TYPE AD_HSNM1, "门牌号
        ORT01      TYPE ORT01_GP,  "城市
        LAND1      TYPE LAND1_GP,  "国家代码
        REGIO      TYPE REGIO,     "地区（省/自治区/直辖市、县）
        "  spras      TYPE spras,     "语言代码
        LAISO      TYPE LAISO,      "语言代码 （字母）
        TELF1      TYPE AD_TLNMBR1,     "第一个电话号
        TELFX      TYPE  TELFX,    "传真号
        TELF2      TYPE  TELF2,     "第二个电话号
        SMTP_ADDR  TYPE   ADR6-SMTP_ADDR, "电子邮箱
        STCEG      TYPE STCEG,      "增值税登记号
        J_1KFREPRE TYPE REPRES,     "法定代表人
        STCD5      TYPE STCD5,      "税号5
        AKONT      TYPE  AKONT,     "统驭科目
        ZTERM1     TYPE  DZTERM,     "付款条件
        REPRF      TYPE REPRF,      "检查双重发票或信贷凭单的标志
        WAERS      TYPE WAERS,      "货币码
        ZTERM2     TYPE DZTERM,      "付款条件
        WEBRE      TYPE WEBRE,      "标识：基于收货的发票验证
        EKGRP      TYPE EKGRP,      "采购组
        BANKS      TYPE BANKS,      "银行国家代码
        BANKL      TYPE BANKL,      "银行代码
        KOINH      TYPE KOINH_FI,      "帐户持有人姓名
        BANKN      TYPE BANKN,      "银行帐户
        LXR        TYPE NAME1,       "联系人
        TEL        TYPE TELF1,         "电话1
        MOBDH      TYPE AD_MBNMBR1,   "移动电话
        FAX        TYPE AD_FXNMBR1,   "传真
        EMAIL      TYPE AD_SMTPADR,   "EMAIL
        CPF        TYPE LIFNR ,       "出票方

      END OF TY_INPUT .



TYPES:BEGIN  OF  TY_OUTPUT,
        STATU      TYPE   ICONNAME,     "状态烂
        " message    TYPE   char200  ,    "消息
        XH(10)     TYPE C ,       "序号
        LIFNR      TYPE LIFNR,    "供应商编号
        BUKRS      TYPE BUKRS,    "公司代码
        EKORG      TYPE EKORG,     "采购组织
        KTOKK      TYPE KTOKK,    "账户组
        ANRED      TYPE ANRED,    "标题
        NAME1      TYPE NAME1_GP, "名称1
        NAME2      TYPE NAME2_GP,  "名称2
        NAME3      TYPE NAME3_GP,  "名称3
        SORT1      TYPE SORTL,     "搜索项
        STREET     TYPE AD_STREET, "住宅街道
        HOUSE_NUM1 TYPE AD_HSNM1, "门牌号
        ORT01      TYPE ORT01_GP,  "城市
        LAND1      TYPE LAND1_GP,  "国家代码
        REGIO      TYPE REGIO,     "地区（省/自治区/直辖市、县）
        SPRAS      TYPE SPRAS,     "语言代码
        LAISO      TYPE LAISO,      "语言代码 （字母）
        TELF1      TYPE AD_TLNMBR1,     "第一个电话号
        TELFX      TYPE  TELFX,    "传真号
        TELF2      TYPE  TELF2,     "第二个电话号
        SMTP_ADDR  TYPE   ADR6-SMTP_ADDR, "电子邮箱
        STCEG      TYPE STCEG,      "增值税登记号
        J_1KFREPRE TYPE REPRES,     "法定代表人
        STCD5      TYPE STCD5,      "税号5
        AKONT      TYPE  AKONT,     "统驭科目
        ZTERM1     TYPE  DZTERM,     "付款条件
        REPRF      TYPE REPRF,      "检查双重发票或信贷凭单的标志
        WAERS      TYPE WAERS,      "货币码
        ZTERM2     TYPE DZTERM,      "付款条件
        WEBRE      TYPE WEBRE,      "标识：基于收货的发票验证
        EKGRP      TYPE EKGRP,      "采购组
        BANKS      TYPE BANKS,      "银行国家代码
        BANKL      TYPE BANKL,      "银行代码
        KOINH      TYPE KOINH_FI,      "帐户持有人姓名
        BANKN      TYPE BANKN,      "银行帐户
        LXR        TYPE NAME1,       "联系人
        TEL        TYPE TELF1,         "电话1
        MOBDH      TYPE AD_MBNMBR1,   "移动电话
        FAX        TYPE AD_FXNMBR1,   "传真
        EMAIL      TYPE AD_SMTPADR,   "EMAIL
        CPF        TYPE LIFNR ,       "出票方
        INFO       TYPE STRING ,      "消息
        SEL(1),
*&--代码添加 BY HANDYBY 22.08.2017 11:28:27  BEGIN
*        FLAG_LF    TYPE C,  "供应商是否已创建 创建:X；否:空
*        FLAG_BK    TYPE C,  "公司代码是否已创建  创建:X；否:空
*        FLAG_KG    TYPE C,  "采购组织是否已创建  创建:X；否:空
*&--代码添加 BY HANDYBY 22.08.2017 11:28:27  END

      END OF TY_OUTPUT.



DATA:GS_INPUT TYPE  TY_INPUT,
     GT_INPUT TYPE TABLE OF TY_INPUT.


DATA:GS_OUTPUT   TYPE TY_OUTPUT,
     GS_OUTPUT_1 TYPE TY_OUTPUT,
     GT_OUTPUT   TYPE TABLE OF TY_OUTPUT.


DATA:GT_LFA1 TYPE TABLE OF  LFA1,
     GS_LFA1 TYPE LFA1.

DATA:GT_T024E TYPE TABLE OF T024E,
     GS_T024E TYPE T024E.

DATA G_TEST TYPE C.    "测试
DATA:G_XH(5)  TYPE C .
DATA:LV_FLG.


DATA G_LIFNR_1 TYPE LIFNR."供应商编号

"DATA:lv_flg.
DATA:LT_NRIV         TYPE TABLE OF NRIV,
     LS_NRIV         TYPE NRIV,
     LS_TNRO         TYPE TNRO,
     GT_V_077K_B     TYPE TABLE OF V_077K_B, "取出流水号对象
     GS_V_077K_B     TYPE V_077K_B,
     LS_MAIN         TYPE VMDS_EI_MAIN,
     LS_MESG         TYPE CVIS_MESSAGE,

     LS_MAIN1        TYPE VMDS_EI_MAIN,
     LS_MAIN2        TYPE VMDS_EI_MAIN,
     LS_MESG1        TYPE CVIS_MESSAGE,

     LT_MSG          TYPE BAPIRET2_T,
     LWA_MSG         TYPE BAPIRET2,

     LT_VEND         TYPE VMDS_EI_EXTERN_T,
     LWA_VEND        TYPE VMDS_EI_EXTERN,

     LT_COMPANY      TYPE VMDS_EI_COMPANY_T,
     LWA_COMPANY     TYPE VMDS_EI_COMPANY,

     LT_BANKDETAILS  TYPE CVIS_EI_BANKDETAIL_T,
     LWA_BANKDETAILS TYPE CVIS_EI_CVI_BANKDETAIL, "CVIS_EI_BANKDETAIL.

     LT_CONTACTS     TYPE VMDS_EI_CONTACTS_T, "cmds_ei_contacts_t,
     LWA_CONTACT     TYPE VMDS_EI_CONTACTS,

     LT_PURCHASING   TYPE VMDS_EI_PURCHASING_T,
     LWA_PURCHASING  TYPE VMDS_EI_PURCHASING,

     LT_FUNCTIONS    TYPE VMDS_EI_FUNCTIONS_T,
     LWA_FUNCTIONS   TYPE VMDS_EI_FUNCTIONS,
     L_TABIX         TYPE SY-TABIX,

     "  **************************add fax_lxr******
     LT_TEL          TYPE CVIS_EI_TLX_T,
     LWA_TEL         TYPE CVIS_EI_TLX_STR,
     LT_FAX          TYPE CVIS_EI_FAX_T,
     LWA_FAX         TYPE CVIS_EI_FAX_STR,
     LT_MAIL         TYPE CVIS_EI_SMTP_T,
     LWA_MAIL        TYPE CVIS_EI_SMTP_STR,
     LT_TEL01        TYPE CVIS_EI_TTX_T,
     LWA_TEL01       TYPE CVIS_EI_TTX_STR,

     LT_PHONE        TYPE CVIS_EI_PHONE_T,
     LWA_PHONE       TYPE CVIS_EI_PHONE_STR,
     LT_PHONE1       TYPE CVIS_EI_PHONE_T,
     LWA_PHONE1      TYPE CVIS_EI_PHONE_STR,

     LT_FAX_LXR      TYPE  CVIS_EI_FAX_T,
     LWA_FAX_LXR     TYPE CVIS_EI_FAX_STR,
*固定电话
     LT_TTX_LXR      TYPE CVIS_EI_TTX_T,
     LWA_TTX_LXR     TYPE CVIS_EI_TTX_STR,
*联系人移动电话
     LT_TLX_LXR      TYPE CVIS_EI_TLX_T,
     LWA_TLX_LXR     TYPE CVIS_EI_TLX_STR,
* 电邮
     LT_SMTP_LXR     TYPE CVIS_EI_SMTP_T,
     LWA_SMTP_LXR    TYPE CVIS_EI_SMTP_STR.

DATA:GT_T077K TYPE TABLE OF T077K,
     GS_T077K TYPE T077K.

DATA:GT_T002 TYPE TABLE OF T002,
     GS_T002 TYPE T002.

DATA:GT_TPAER TYPE TABLE OF TPAER,
     GS_TPAER TYPE TPAER.

*&--代码添加 BY HANDYBY 22.08.2017 15:36:12  BEGIN
DATA: BEGIN OF GS_KNVK ,
        PARNR TYPE KNVK-PARNR,
        NAME1 TYPE KNVK-NAME1,
        LIFNR TYPE KNVK-LIFNR,
      END OF GS_KNVK .
DATA GT_KNVK LIKE TABLE OF GS_KNVK .
*&--代码添加 BY HANDYBY 22.08.2017 15:36:12  END

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS:     SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,   "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,  "SY-REPID 指 当前的主程序
      GT_EVENTS        TYPE SLIS_T_EVENT WITH HEADER LINE, "保存AVL事件
      GW_EVENTS        LIKE LINE OF GT_EVENTS.
DATA: GT_EXCLUDE TYPE SLIS_T_EXTAB,
      GS_EXCLUDE TYPE SLIS_EXTAB.

DATA: GR_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROWS TYPE LVC_T_ROW,
      GT_ROID TYPE LVC_T_ROID,
      WA_ROWS TYPE LVC_S_ROW,
      WA_ROID TYPE LVC_S_ROID.
DATA: GS_VARIANT TYPE DISVARIANT.

DATA: GW_ISTABLE TYPE LVC_S_STBL.
************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  gw_lvc-fieldname = &1.
  gw_lvc-coltext   = &2.
  gw_lvc-scrtext_l = &2.
  gw_lvc-scrtext_m = &2.
  gw_lvc-scrtext_s = &2.
  gw_lvc-reptext   = &2.
  gw_lvc-outputlen = &3.
  IF &4 = 'X'.
    gw_lvc-key = 'X'.
  ENDIF.
  gw_lvc-checkbox = &5.
  gw_lvc-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  gw_lvc-hotspot   = &7.
  gw_lvc-ref_field = &9.
  gw_lvc-ref_table = &8.
  APPEND gw_lvc TO gt_lvc.
  CLEAR gw_lvc.
END-OF-DEFINITION.
*----------------------------------------------------------------------------------
*Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2012. Sap Release 702

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETER:P_UP TYPE CHAR1   RADIOBUTTON  GROUP  G1 DEFAULT 'X' USER-COMMAND G_UCMD,
          P_DOWN TYPE CHAR1 RADIOBUTTON  GROUP  G1 .
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_FN TYPE RLGRAP-FILENAME  ."MODIF ID ZUP MEMORY ID ZSD001. "主数据文件路径
SELECTION-SCREEN END OF BLOCK BLK2.



************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At selection screen
************************************************************************
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FN.
  PERFORM FRM_GET_FN.

TOP-OF-PAGE.


AT SELECTION-SCREEN.


START-OF-SELECTION.
  IF P_UP = 'X'.
    PERFORM FRM_CHECH_FILENAME.             "检测文件名
*供应商主数据主数据
    .
    PERFORM FRM_UPLOAD_DATA TABLES GT_INPUT."上传文件到内表
    SORT GT_INPUT BY XH .
    PERFORM FRM_GET_DATA.
    PERFORM FRM_ALV_SHOW.
  ELSE.
    PERFORM FRM_DOWNLOAD  .
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_FN .
  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
*   EXPORTING
*     DEF_FILENAME           = ' '
*     DEF_PATH               = ' '
*     MASK                   = ' '
*     MODE                   = ' '
*     TITLE                  = ' '
    IMPORTING
      FILENAME         = P_FN
*     PATH             =
*     FILE             =
    EXCEPTIONS
      SELECTION_CANCEL = 1
      SELECTION_ERROR  = 2
      OTHERS           = 3.
  IF SY-SUBRC <> 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECH_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECH_FILENAME .
  IF P_FN IS INITIAL.
*    MESSAGE i010."主数据文件，路径和文件名，不能为空！
    MESSAGE '主数据文件，路径和文件名，不能为空！' TYPE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  SELECT * INTO TABLE GT_T077K
    FROM T077K
    WHERE KTOKK LIKE 'Z%'.

  SORT GT_T077K BY KTOKK .

  SELECT *  INTO TABLE GT_TPAER
  FROM TPAER
  WHERE PARGR = 'L1'
    AND PAPFL = 'X'.

  SORT GT_TPAER BY PARGR .

  SELECT * INTO TABLE GT_T002
    FROM T002 .

  SORT  GT_T002 BY LAISO.


  LOOP AT GT_INPUT INTO GS_INPUT.
    MOVE-CORRESPONDING GS_INPUT TO GS_OUTPUT.
    GS_OUTPUT-STATU = ICON_YELLOW_LIGHT.
    READ TABLE GT_T002 INTO GS_T002 WITH KEY LAISO = GS_INPUT-LAISO BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      GS_OUTPUT-SPRAS = GS_T002-SPRAS .
    ENDIF.
    APPEND GS_OUTPUT TO GT_OUTPUT.
    CLEAR GS_OUTPUT.
  ENDLOOP.
  SORT GT_OUTPUT BY XH .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ALV_SHOW .
  PERFORM INIT_LAYOUT.             "设置输出格式
*  perform init_sort.               "设置排序、合计
*  perform init_variant.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
*  perform frm_build_event.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_OUTPUT
                     USING 'SET_PF_STATUS'
                           'USER_COMMAND'
                           GW_LAYOUT
                           GW_VARIANT
                           GW_GRID_SETTINGS.
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
  GW_LAYOUT-ZEBRA       = 'X'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
  GW_LAYOUT-BOX_FNAME  = 'SEL'.

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

  INIT_FIELDCAT 'STATU'  '状态'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INFO' '日志消息'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR' '供应商编码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS' '公司代码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKORG' '采购组织'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTOKK' '账户组'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANRED' '标题'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1' '名称1'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME2' '名称2'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME3' '名称3'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SORT1' '搜索项'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STREET' '住宅街道'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'HOUSE_NUM1' '门牌号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ORT01' '城市'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LAND1' '国家代码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REGIO' '地区（省/自治区/直辖市、县)'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LAISO' '语言代码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELF1' '第一个电话号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELFX' '传真号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELF2' '第二个电话号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SMTP_ADDR' '电子邮箱'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STCEG' '增值税登记号'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'J_1KFREPRE' '法定代表人'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STCD5' '税号5'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AKONT' '统驭科目'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM1' '付款条件'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REPRF' '检查双重发票或信贷凭单的标志'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS' '货币码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM2' '付款条件'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WEBRE' '标识：基于收货的发票验证'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EKGRP' '采购组'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKS' '银行国家代码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKL' '银行代码'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KOINH' '帐户持有人姓名'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKN' '银行帐户'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LXR'   '联系人'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL' '电话1'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FAX' '传真'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'EMAIL' 'EMAIL'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CPF' '出票方'               '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCLUDE .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_T_DATA  text
*      -->P_0358   text
*      -->P_0359   text
*      -->P_GW_LAYOUT  text
*      -->P_GW_VARIANT  text
*      -->P_GW_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT TABLES PT_LVC TYPE LVC_T_FCAT
                       PT_SORT TYPE LVC_T_SORT
                       PT_DATA
                USING PU_STATUS
                      PU_UCOMM
                      PW_LAYOUT TYPE LVC_S_LAYO
                      PW_VARIANT TYPE DISVARIANT
                      PW_GRID_SETTINGS TYPE LVC_S_GLAY.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
      IT_SORT_LVC              = PT_SORT[]
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
    TABLES
      T_OUTTAB                 = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "FRM_OUTPUT

FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARDSTATUS' EXCLUDING RT_EXTAB.
ENDFORM.                    "SET_PF_STATUS

*&--------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&--------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                    RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA  L_CHECK TYPE C.
  CLEAR L_CHECK.
  CASE R_UCOMM.

    WHEN '&IC1'. "双击
      READ TABLE GT_OUTPUT INTO GS_OUTPUT INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0 .
        CASE RS_SELFIELD-FIELDNAME.
          WHEN 'LIFNR'.
            SET PARAMETER ID  'LIF' FIELD GS_OUTPUT-LIFNR.
            SET PARAMETER ID  'BUK' FIELD GS_OUTPUT-BUKRS.
            CALL TRANSACTION  'XK03' WITH AUTHORITY-CHECK." AND SKIP FIRST SCREEN.
        ENDCASE.
      ENDIF.


    WHEN '&UPLOAD'.
*      DATA GT_KNA1 TYPE TABLE OF KNA1.
*      DATA GS_KNA1 TYPE KNA1.
*
*      REFRESH GT_KNA1.
**查询供应商名称
      SELECT * FROM LFA1
      INTO TABLE GT_LFA1 .
      SORT GT_LFA1 BY NAME1  KTOKK.

*&--代码添加 BY HANDYBY 22.08.2017 10:39:03  BEGIN
      DATA: BEGIN OF LS_LFB1 ,
              LIFNR TYPE LFB1-LIFNR,
              BUKRS TYPE LFB1-BUKRS,
            END OF LS_LFB1 .
      DATA LT_LFB1 LIKE TABLE OF LS_LFB1 .
      DATA: BEGIN OF LS_LFM1 ,
              LIFNR TYPE LFM1-LIFNR,
              EKORG TYPE LFM1-EKORG,
            END OF LS_LFM1 .
      DATA LT_LFM1 LIKE TABLE OF LS_LFM1 .
      " 公司代码
      SELECT LIFNR
             BUKRS
        INTO CORRESPONDING FIELDS OF TABLE LT_LFB1
        FROM LFB1 .
      SORT LT_LFB1 BY LIFNR BUKRS .

      " 采购组织
      SELECT LIFNR
             EKORG
        INTO CORRESPONDING FIELDS OF TABLE LT_LFM1
        FROM LFM1 .
*&--代码添加 BY HANDYBY 22.08.2017 10:39:03  END

      SELECT * FROM T024E
        INTO TABLE GT_T024E
       .
      SORT GT_T024E BY EKORG BUKRS.

*检查是否已经进行测试导入，并且无报错

*      READ TABLE gt_output INTO gs_output
*      WITH KEY statu = icon_red_light.
*      IF sy-subrc = 0.
*        l_check = 'X'.
*      ENDIF.
*
*      READ TABLE gt_output INTO gs_output
*      WITH KEY statu = icon_yellow_light.
*      IF sy-subrc = 0.
*        l_check = 'X'.
*      ENDIF.

*检查客户和账户组的重复性
      LOOP AT GT_OUTPUT INTO GS_OUTPUT.

        READ TABLE GT_LFA1 INTO GS_LFA1
        WITH KEY NAME1 = GS_OUTPUT-NAME1
                 KTOKK = GS_OUTPUT-KTOKK
                 BINARY SEARCH .
        IF SY-SUBRC = 0.
*&--代码注释 BY HANDYBY 22.08.2017 10:44:23  BEGIN
*          L_CHECK = 'X'.
*          GS_OUTPUT-STATU = ICON_RED_LIGHT.
*          GS_OUTPUT-INFO = '该供应商名称已经在系统中存在'.
*&--代码注释 BY HANDYBY 22.08.2017 10:44:23  END
*&--代码添加 BY HANDYBY 22.08.2017 10:38:37  BEGIN
          GS_OUTPUT-LIFNR = GS_LFA1-LIFNR .
          READ TABLE LT_LFB1 WITH KEY LIFNR = GS_LFA1-LIFNR
                                      BUKRS = GS_OUTPUT-BUKRS
                                      BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            L_CHECK = 'X'.
            GS_OUTPUT-STATU = ICON_RED_LIGHT.
            GS_OUTPUT-INFO = '该公司代码已经在系统中存在'.
          ELSE .
            READ TABLE LT_LFM1 WITH KEY LIFNR = GS_LFA1-LIFNR
                                        EKORG = GS_OUTPUT-EKORG
                                        BINARY SEARCH TRANSPORTING NO FIELDS .
            IF SY-SUBRC = 0 .
              L_CHECK = 'X'.
              GS_OUTPUT-STATU = ICON_RED_LIGHT.
              GS_OUTPUT-INFO = '该采购组织已经在系统中存在'.
            ENDIF.
          ENDIF.
        ELSE .
*&--代码添加 BY HANDYBY 22.08.2017 10:38:37  END
        ENDIF.

        READ TABLE GT_T024E INTO GS_T024E WITH KEY EKORG = GS_OUTPUT-EKORG
                                                   BUKRS = GS_OUTPUT-BUKRS
                                                   BINARY SEARCH .
        IF SY-SUBRC NE 0 .
          L_CHECK = 'X'.
          GS_OUTPUT-STATU = ICON_RED_LIGHT.
          GS_OUTPUT-INFO = '采购组织与公司代码无对应关系'.
        ENDIF.

        IF GS_OUTPUT-KTOKK = 'Z003'.
          L_CHECK = 'X'.
          GS_OUTPUT-STATU = ICON_RED_LIGHT.
          GS_OUTPUT-INFO = '关联方供应商请FB01手工创建'.
        ENDIF.

        MODIFY GT_OUTPUT FROM GS_OUTPUT.
        CLEAR GS_OUTPUT .
      ENDLOOP.

      " IF l_check <> 'X'.
      "g_test = ''.
      IF  L_CHECK NE  'X'.
        PERFORM FRM_FILL_DATA.                 "将数据填充内表
      ENDIF.
      "ELSE.
      "  MESSAGE '请进行测试导入并无报错再进行正式导入' TYPE 'S' DISPLAY LIKE 'E'.
      "ENDIF.

*    WHEN '&TEST'.
*      CLEAR l_check.
*      REFRESH gt_kna1.
*
**查询客户名称
*      SELECT * FROM kna1
*      INTO CORRESPONDING FIELDS OF TABLE gt_kna1.
*
**检查客户和账户组的重复性
*      LOOP AT gt_output INTO gs_output.
*        READ TABLE gt_kna1 INTO gs_kna1
*        WITH KEY name1 = gs_output-name1 ktokd = gs_output-ktokd.
*        IF sy-subrc = 0.
*          l_check = 'X'.
*          gs_output-statu   = icon_red_light.
*          gs_output-message = '该客户名称已经在系统中存在'.
*          MODIFY gt_output FROM gs_output.
*          CLEAR gs_output.
*        ENDIF.
*      ENDLOOP.
*
*      IF l_check <> 'X'.
*        g_test = 'X'.
*        PERFORM frm_fill_data.                 "将数据填充内表
*      ENDIF.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.  "自动刷新
ENDFORM. "USER_COM
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA .
  "*根据对象号取出流水码号
  SELECT * FROM NRIV
    INTO CORRESPONDING FIELDS OF TABLE LT_NRIV
    WHERE OBJECT    = 'KREDITOR'.
  "    *取出账户组对应的编号
  SELECT  * FROM T077Y
  INNER JOIN T077K ON
  T077Y~KTOKK = T077K~KTOKK
  INTO CORRESPONDING FIELDS OF TABLE GT_V_077K_B .
  SORT GT_V_077K_B BY KTOKK .

*&--代码添加 BY HANDYBY 22.08.2017 11:18:39  BEGIN
  DATA L_FLAG TYPE C .
  SORT GT_LFA1 BY LIFNR .

  DATA: BEGIN OF LS_LFBK ,
          LIFNR TYPE LFBK-LIFNR,
          BANKS TYPE LFBK-BANKS,
          BANKL TYPE LFBK-BANKL,
          BANKN TYPE LFBK-BANKN,
          KOINH TYPE LFBK-KOINH,
        END OF LS_LFBK .
  DATA LT_LFBK LIKE TABLE OF LS_LFBK .
  SELECT LIFNR
         BANKS
         BANKL
         BANKN
         KOINH
    INTO CORRESPONDING FIELDS OF TABLE LT_LFBK
    FROM LFBK
     FOR ALL ENTRIES IN GT_OUTPUT
   WHERE LIFNR = GT_OUTPUT-LIFNR
     AND BANKS = GT_OUTPUT-BANKS
     AND BANKL = GT_OUTPUT-BANKL
     AND BANKN = GT_OUTPUT-BANKN .
  SORT LT_LFBK BY LIFNR BANKS BANKL BANKN .

  SELECT PARNR
         NAME1
         LIFNR
    INTO CORRESPONDING FIELDS OF TABLE GT_KNVK
    FROM KNVK
     FOR ALL ENTRIES IN GT_OUTPUT
   WHERE LIFNR = GT_OUTPUT-LIFNR .
  SORT GT_KNVK BY LIFNR NAME1 .

  DATA: BEGIN OF LS_ADR2 ,
          LIFNR      TYPE LFA1-LIFNR,
          PRSNR      TYPE KNVK-PRSNR,
          ADRNR      TYPE LFA1-ADRNR,
          TEL_NUMBER TYPE ADR2-TEL_NUMBER,
        END OF LS_ADR2 .
  DATA LT_ADR2 LIKE TABLE OF LS_ADR2 .
  SELECT A~LIFNR
         B~PRSNR
         A~ADRNR
         C~TEL_NUMBER
    INTO CORRESPONDING FIELDS OF TABLE LT_ADR2
    FROM LFA1 AS A
   INNER JOIN KNVK AS B
      ON A~LIFNR = B~LIFNR
   INNER JOIN ADR2 AS C
      ON A~ADRNR = C~ADDRNUMBER
     AND B~PRSNR = C~PERSNUMBER
     FOR ALL ENTRIES IN GT_OUTPUT
   WHERE A~LIFNR = GT_OUTPUT-LIFNR
     AND C~R3_USER NE 'X' .
  SORT LT_ADR2 BY LIFNR TEL_NUMBER .

  DATA: BEGIN OF LS_ADR3 ,
          LIFNR      TYPE LFA1-LIFNR,
          PRSNR      TYPE KNVK-PRSNR,
          ADRNR      TYPE LFA1-ADRNR,
          FAX_NUMBER TYPE ADR3-FAX_NUMBER,
        END OF LS_ADR3 .
  DATA LT_ADR3 LIKE TABLE OF LS_ADR3 .
  SELECT A~LIFNR
         B~PRSNR
         A~ADRNR
         C~FAX_NUMBER
    INTO CORRESPONDING FIELDS OF TABLE LT_ADR3
    FROM LFA1 AS A
   INNER JOIN KNVK AS B
      ON A~LIFNR = B~LIFNR
   INNER JOIN ADR3 AS C
      ON A~ADRNR = C~ADDRNUMBER
     AND B~PRSNR = C~PERSNUMBER
     FOR ALL ENTRIES IN GT_OUTPUT
   WHERE A~LIFNR = GT_OUTPUT-LIFNR .
  SORT LT_ADR3 BY LIFNR FAX_NUMBER .

  DATA: BEGIN OF LS_ADR6 ,
          LIFNR     TYPE LFA1-LIFNR,
          PRSNR     TYPE KNVK-PRSNR,
          ADRNR     TYPE LFA1-ADRNR,
          SMTP_ADDR TYPE ADR6-SMTP_ADDR,
        END OF LS_ADR6 .
  DATA LT_ADR6 LIKE TABLE OF LS_ADR6 .
  SELECT A~LIFNR
         B~PRSNR
         A~ADRNR
         C~SMTP_ADDR
    INTO CORRESPONDING FIELDS OF TABLE LT_ADR6
    FROM LFA1 AS A
   INNER JOIN KNVK AS B
      ON A~LIFNR = B~LIFNR
   INNER JOIN ADR6 AS C
      ON A~ADRNR = C~ADDRNUMBER
     AND B~PRSNR = C~PERSNUMBER
     FOR ALL ENTRIES IN GT_OUTPUT
   WHERE A~LIFNR = GT_OUTPUT-LIFNR .
  SORT LT_ADR6 BY LIFNR SMTP_ADDR.

  DATA: LT_T024W TYPE TABLE OF T024W,
        LS_T024W TYPE T024W.
  SELECT *
    INTO TABLE LT_T024W
    FROM T024W .
  SORT LT_T024W BY EKORG .
*&--代码添加 BY HANDYBY 22.08.2017 11:18:39  END

*  "*读取修改对象
*  READ TABLE GT_V_077K_B INTO GS_V_077K_B
*  WITH KEY KTOKK  = 'Z002'.
  DATA:G_LIFNR TYPE LIFNR .
  CLEAR:L_TABIX .
  DATA:GT_LFBK TYPE TABLE OF LFBK,
       GS_LFBK TYPE LFBK.
  LOOP AT GT_OUTPUT INTO GS_OUTPUT . "代码注释 BY HANDYBY "WHERE  LIFNR IS INITIAL.  .
    CLEAR:GS_OUTPUT_1.
    MOVE GS_OUTPUT TO GS_OUTPUT_1 .
    AT NEW XH .
      L_TABIX =  1.
      CLEAR:G_LIFNR,GS_V_077K_B,LS_NRIV.
      REFRESH:LT_BANKDETAILS,LT_CONTACTS.",lt_nriv,gt_v_077k_b.

*&--代码添加 BY HANDYBY 22.08.2017 11:14:43  BEGIN
      IF GS_OUTPUT_1-LIFNR IS NOT INITIAL .
        L_FLAG = 'X'.
*        READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = GS_OUTPUT_1-LIFNR BINARY SEARCH .
*        IF SY-SUBRC = 0 .
        G_LIFNR = GS_OUTPUT_1-LIFNR .
*        CLEAR GS_LFA1 .
*        ENDIF.
      ELSE .
*&--代码添加 BY HANDYBY 22.08.2017 11:14:43  END
        L_FLAG = ''.
        READ TABLE GT_V_077K_B INTO GS_V_077K_B WITH KEY KTOKK  = GS_OUTPUT_1-KTOKK  BINARY SEARCH .
        IF SY-SUBRC EQ 0 .
*        READ TABLE lt_nriv INTO  ls_nriv
*   WITH KEY nrrangenr = gs_v_077k_b-numkr.
          CLEAR:LS_NRIV .
          SELECT SINGLE * INTO LS_NRIV
            FROM NRIV WHERE  OBJECT    = 'KREDITOR' AND NRRANGENR = GS_V_077K_B-NUMKR.
*当状态0取当前值，当状态为0时候，取起始值
          IF LS_NRIV-NRLEVEL <> 0.
            G_LIFNR = LS_NRIV-NRLEVEL +  L_TABIX .
          ELSE.
            G_LIFNR = LS_NRIV-FROMNUMBER . .
          ENDIF.
        ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 11:24:43  BEGIN
      ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 11:24:43  END


*添加前置零
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = G_LIFNR
        IMPORTING
          OUTPUT = G_LIFNR.

    ENDAT .


    "添加银行数据
    CLEAR:LWA_BANKDETAILS.
    IF GS_OUTPUT-BANKS IS NOT INITIAL.
*&--代码注释 BY HANDYBY 22.08.2017 11:24:59  BEGIN
*      LWA_BANKDETAILS-TASK = 'I'.
*&--代码注释 BY HANDYBY 22.08.2017 11:24:59  END
*&--代码添加 BY HANDYBY 22.08.2017 11:25:15  BEGIN
      IF L_FLAG = ''.
        LWA_BANKDETAILS-TASK = 'I'.
      ELSE .
        READ TABLE LT_LFBK WITH KEY LIFNR = GS_OUTPUT-LIFNR
                                    BANKS = GS_OUTPUT-BANKS
                                    BANKL = GS_OUTPUT-BANKL
                                    BANKN = GS_OUTPUT-BANKN
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LWA_BANKDETAILS-TASK = 'U'.
        ELSE .
          LWA_BANKDETAILS-TASK = 'I'.
        ENDIF.
      ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 11:25:15  END
      LWA_BANKDETAILS-DATA_KEY-BANKS = GS_OUTPUT_1-BANKS.
      LWA_BANKDETAILS-DATA_KEY-BANKL = GS_OUTPUT_1-BANKL.
      LWA_BANKDETAILS-DATA_KEY-BANKN = GS_OUTPUT_1-BANKN.
      LWA_BANKDETAILS-DATA-KOINH = GS_OUTPUT_1-KOINH.
      LWA_BANKDETAILS-DATAX-KOINH = GS_OUTPUT_1-KOINH.

      APPEND LWA_BANKDETAILS TO LT_BANKDETAILS.
      LWA_VEND-CENTRAL_DATA-BANKDETAIL-BANKDETAILS = LT_BANKDETAILS.
    ENDIF.
    "添加多个联系人
    PERFORM FRM_FILL_CONTACT TABLES GT_KNVK
                             USING GS_OUTPUT-LXR
                                   GS_OUTPUT_1-TEL
                                   GS_OUTPUT_1-MOBDH
                                   GS_OUTPUT_1-FAX
                                   GS_OUTPUT_1-EMAIL
                                   GS_OUTPUT-LIFNR .

    LWA_VEND-CENTRAL_DATA-CONTACT-CONTACTS = LT_CONTACTS.


    AT END OF XH .
*&--代码注释 BY HANDYBY 22.08.2017 15:51:56  BEGIN
*      "验证名称1 是否已经存在
*      CLEAR:GS_LFA1 .
*      SELECT SINGLE * INTO GS_LFA1 FROM LFA1 WHERE NAME1 = GS_OUTPUT_1-NAME1.    "名称1
*      IF GS_LFA1 IS NOT INITIAL.
*        LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE XH = GS_OUTPUT_1-XH .
*          GS_OUTPUT-STATU = ICON_RED_LIGHT..
*          CONCATENATE  GS_OUTPUT-XH  '创建失败：已存在相同的供应商名称' GS_OUTPUT_1-NAME1  INTO GS_OUTPUT-INFO  .
*          MODIFY GT_OUTPUT FROM GS_OUTPUT.
*
*        ENDLOOP.
*        CONTINUE.
*      ENDIF.
*&--代码注释 BY HANDYBY 22.08.2017 15:51:56  END

      "主数据
*&--代码添加 BY HANDYBY 22.08.2017 15:52:46  BEGIN
      IF L_FLAG = ''.
        LWA_VEND-HEADER-OBJECT_TASK = 'I'.
      ELSE .
        LWA_VEND-HEADER-OBJECT_TASK = 'U'.
      ENDIF .

*&--代码添加 BY HANDYBY 22.08.2017 15:52:46  END
      LWA_VEND-HEADER-OBJECT_INSTANCE-LIFNR = G_LIFNR.
      "   lwa_vend-central_data-central-data-bukrs = gs_output-bukrs .
      "lwa_vend-central_data-central-datax-bukrs= 'X'.
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATA-KTOKK =  GS_OUTPUT_1-KTOKK.       "账号组
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATA-STCEG = GS_OUTPUT_1-STCEG.        "增值税登记号
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATA-J_1KFREPRE = GS_OUTPUT_1-J_1KFREPRE.        "法定代表人
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATA-STCD5 = GS_OUTPUT_1-STCD5.        "税号5
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATAX-STCEG = 'X'.
      LWA_VEND-CENTRAL_DATA-CENTRAL-DATAX-KTOKK = 'X'.

      "地址数据
      IF L_FLAG = ''.
        LWA_VEND-CENTRAL_DATA-ADDRESS-TASK = 'I'.
      ELSE .
        LWA_VEND-CENTRAL_DATA-ADDRESS-TASK = 'U'.
      ENDIF .

      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-TITLE = GS_OUTPUT_1-ANRED.    "标题
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-NAME  = GS_OUTPUT_1-NAME1.    "名称1
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-NAME_2  = GS_OUTPUT_1-NAME2.   "名称2
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-NAME_3  = GS_OUTPUT_1-NAME3.   "名称3
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-SORT1 = GS_OUTPUT_1-SORT1.     "排序
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-STREET = GS_OUTPUT_1-STREET.   "街道
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-HOUSE_NO = GS_OUTPUT_1-HOUSE_NUM1. "住宅号
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-CITY = GS_OUTPUT_1-ORT01.        "城市
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-COUNTRY = GS_OUTPUT_1-LAND1.     "国家
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-REGION = GS_OUTPUT_1-REGIO.     "地区
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATA-LANGU = GS_OUTPUT_1-SPRAS.     "语言

      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-TITLE = GS_OUTPUT_1-ANRED.    "标题
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-NAME  = GS_OUTPUT_1-NAME1.    "名称1
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-NAME_2  = GS_OUTPUT_1-NAME2.   "名称2
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-NAME_3  = GS_OUTPUT_1-NAME3.   "名称3
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-SORT1 = GS_OUTPUT_1-SORT1.     "排序
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-STREET = GS_OUTPUT_1-STREET.   "街道
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-HOUSE_NO = GS_OUTPUT_1-HOUSE_NUM1. "住宅号
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-CITY = GS_OUTPUT_1-ORT01.        "城市
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-COUNTRY = GS_OUTPUT_1-LAND1.     "国家
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-REGION = GS_OUTPUT_1-REGIO.     "地区
      LWA_VEND-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-LANGU = GS_OUTPUT_1-SPRAS.     "语言

      "  *电话
      CLEAR:LWA_PHONE.
      REFRESH LT_PHONE.
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  BEGIN
      IF L_FLAG = ''.
        LWA_PHONE-CONTACT-TASK = 'I'.
      ELSE .
        READ TABLE LT_ADR2 WITH KEY LIFNR = GS_OUTPUT_1-LIFNR
                                    TEL_NUMBER = GS_OUTPUT_1-TELF1
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LWA_PHONE-CONTACT-TASK = 'U'.
        ELSE .
          LWA_PHONE-CONTACT-TASK = 'I'.
        ENDIF.
      ENDIF .
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  END

      LWA_PHONE-CONTACT-DATA-TELEPHONE = GS_OUTPUT_1-TELF1.
      LWA_PHONE-CONTACT-DATA-R_3_USER  = '1'.
      LWA_PHONE-CONTACT-DATAX-TELEPHONE = 'X'.
      LWA_PHONE-CONTACT-DATAX-R_3_USER  = 'X'.
      APPEND LWA_PHONE TO LT_PHONE.
      CLEAR:LWA_PHONE.

*移动电话
*&--代码添加 BY HANDYBY 22.08.2017 17:23:24  BEGIN
      IF L_FLAG = ''.
        LWA_PHONE-CONTACT-TASK = 'I'.
      ELSE .
        READ TABLE LT_ADR2 WITH KEY LIFNR = GS_OUTPUT_1-LIFNR
                                    TEL_NUMBER = GS_OUTPUT_1-TELF2
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LWA_PHONE-CONTACT-TASK = 'U'.
        ELSE .
          LWA_PHONE-CONTACT-TASK = 'I'.
        ENDIF.
      ENDIF .
*&--代码添加 BY HANDYBY 22.08.2017 17:23:24  END
      LWA_PHONE-CONTACT-TASK = 'I'.
      LWA_PHONE-CONTACT-DATA-TELEPHONE = GS_OUTPUT_1-TELF2.
      LWA_PHONE-CONTACT-DATA-R_3_USER  = '2'.
      LWA_PHONE-CONTACT-DATAX-TELEPHONE = 'X'.
      LWA_PHONE-CONTACT-DATAX-R_3_USER  = 'X'.
      APPEND LWA_PHONE TO LT_PHONE.
      LWA_VEND-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE = LT_PHONE.

      " *传真
      CLEAR:LWA_FAX.
      REFRESH LT_FAX.
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  BEGIN
      IF L_FLAG = ''.
        LWA_FAX-CONTACT-TASK = 'I'.
      ELSE .
        READ TABLE LT_ADR3 WITH KEY LIFNR = GS_OUTPUT_1-LIFNR
                                    FAX_NUMBER = GS_OUTPUT_1-TELFX
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LWA_FAX-CONTACT-TASK = 'U'.
        ELSE .
          LWA_FAX-CONTACT-TASK = 'I'.
        ENDIF.
      ENDIF .
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  END

      LWA_FAX-CONTACT-DATA-FAX = GS_OUTPUT_1-TELFX.
      LWA_FAX-CONTACT-DATAX-FAX = 'X'.
      APPEND LWA_FAX TO LT_FAX.
      LWA_VEND-CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX = LT_FAX.

*电子邮件
      CLEAR:LWA_MAIL.
      REFRESH LT_MAIL.
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  BEGIN
      IF L_FLAG = ''.
        LWA_MAIL-CONTACT-TASK = 'I'.
      ELSE .
        READ TABLE LT_ADR6 WITH KEY LIFNR = GS_OUTPUT_1-LIFNR
                                    SMTP_ADDR = GS_OUTPUT_1-SMTP_ADDR
                                    BINARY SEARCH TRANSPORTING NO FIELDS .
        IF SY-SUBRC = 0 .
          LWA_MAIL-CONTACT-TASK = 'U'.
        ELSE .
          LWA_MAIL-CONTACT-TASK = 'I'.
        ENDIF.
      ENDIF .
*&--代码添加 BY HANDYBY 22.08.2017 15:55:09  END

      LWA_MAIL-CONTACT-DATA-E_MAIL = GS_OUTPUT_1-SMTP_ADDR.
      LWA_MAIL-CONTACT-DATAX-E_MAIL = 'X'.
      APPEND LWA_MAIL TO LT_MAIL.
      LWA_VEND-CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP = LT_MAIL.

      "公司代码数据
      CLEAR:LWA_COMPANY.
      REFRESH:LT_COMPANY.
      LWA_VEND-COMPANY_DATA-CURRENT_STATE = 'X'.
      LWA_COMPANY-TASK = 'I'.
      LWA_COMPANY-DATA_KEY-BUKRS =  GS_OUTPUT_1-BUKRS.  "公司代码
      LWA_COMPANY-DATA-AKONT = GS_OUTPUT_1-AKONT.       "统驭科目
      LWA_COMPANY-DATA-ZTERM = GS_OUTPUT_1-ZTERM1.      "付款条件
      LWA_COMPANY-DATA-REPRF = GS_OUTPUT_1-REPRF.       "检查双重发票或信贷凭单的标志

      LWA_COMPANY-DATAX-AKONT = 'X'.
      LWA_COMPANY-DATAX-ZTERM = 'X'.
      LWA_COMPANY-DATA-REPRF = 'X'.

      APPEND LWA_COMPANY TO LT_COMPANY .
      LWA_VEND-COMPANY_DATA-COMPANY = LT_COMPANY .


      "*采购数据
      CLEAR:LWA_PURCHASING .
      REFRESH:LT_PURCHASING.
      LWA_PURCHASING-TASK = 'I'.
      LWA_PURCHASING-DATA_KEY-EKORG = GS_OUTPUT_1-EKORG .  "采购组织
      LWA_PURCHASING-DATA-WAERS = GS_OUTPUT_1-WAERS .      "货币码
      LWA_PURCHASING-DATA-ZTERM = GS_OUTPUT_1-ZTERM2 .      "付款条件
      LWA_PURCHASING-DATA-WEBRE = GS_OUTPUT_1-WEBRE .       ""标识：基于收货的发票验证
      LWA_PURCHASING-DATA-EKGRP = GS_OUTPUT_1-EKGRP.       "采购组
      LWA_PURCHASING-DATAX-WAERS = 'X' .      "货币码
      LWA_PURCHASING-DATAX-ZTERM = 'X' .      "付款条件
      LWA_PURCHASING-DATAX-WEBRE = 'X' .       ""标识：基于收货的发票验证
      LWA_PURCHASING-DATAX-EKGRP = 'X'.   "采购组

      "添加业务合作伙伴
      REFRESH:LT_FUNCTIONS.
      READ TABLE GT_T077K INTO GS_T077K WITH KEY KTOKK = GS_OUTPUT_1-KTOKK
                                        BINARY SEARCH .
      IF SY-SUBRC EQ 0.
        LOOP AT GT_TPAER INTO GS_TPAER WHERE PARGR = GS_T077K-PARGE  .
          CLEAR:LWA_FUNCTIONS.
          LWA_FUNCTIONS-TASK = 'I'.
          LWA_FUNCTIONS-DATA_KEY-PARVW = GS_TPAER-PARVW .
*          READ TABLE LT_T024W INTO LS_T024W WITH KEY EKORG = GS_OUTPUT_1-EKORG BINARY SEARCH .
*          LWA_FUNCTIONS-DATA_KEY-WERKS = LS_T024W-WERKS .
          LWA_FUNCTIONS-DATA-PARTNER = G_LIFNR .
          LWA_FUNCTIONS-DATAX-PARTNER  = 'X'.
          APPEND LWA_FUNCTIONS TO LT_FUNCTIONS.
*          CLEAR LS_T024W .
        ENDLOOP.
      ENDIF.
      IF GS_OUTPUT_1-KTOKK  EQ 'Z006' AND  GS_OUTPUT_1-CPF NE ''.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GS_OUTPUT_1-CPF
          IMPORTING
            OUTPUT = GS_OUTPUT_1-CPF.

        LOOP AT LT_FUNCTIONS INTO LWA_FUNCTIONS WHERE DATA_KEY-PARVW = 'RS'.

          LWA_FUNCTIONS-DATA-PARTNER =  GS_OUTPUT_1-CPF.
          MODIFY LT_FUNCTIONS FROM LWA_FUNCTIONS  .
        ENDLOOP.

      ENDIF.
      LWA_PURCHASING-FUNCTIONS-FUNCTIONS = LT_FUNCTIONS.

      APPEND LWA_PURCHASING TO LT_PURCHASING.
      LWA_VEND-PURCHASING_DATA-PURCHASING = LT_PURCHASING.

      APPEND LWA_VEND TO LT_VEND.
      LS_MAIN-VENDORS = LT_VEND.

      "   *创建数据到SAP中
      G_TEST = 'X'.
      PERFORM FRM_WRITE_DATA_SAP USING G_TEST  ."首次先TEST执行

      "判断执行效果
      REFRESH  LT_MSG.
      LT_MSG = LS_MESG-MESSAGES.
      IF LT_MSG IS INITIAL.  "TEST 测试运行OK
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
        WAIT UP TO '0.5' SECONDS.
        CLEAR :G_TEST .
        PERFORM FRM_WRITE_DATA_SAP USING G_TEST  ."正式执行创建供应商编号
        REFRESH LT_MSG .
        LT_MSG = LS_MESG-MESSAGES.
        IF LT_MSG IS INITIAL .  "正式运行OK
          LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
            GS_OUTPUT-STATU = ICON_OKAY.
            GS_OUTPUT-LIFNR = G_LIFNR .
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
            IF L_FLAG = '' .
              CONCATENATE  GS_OUTPUT-LIFNR   '创建成功'   INTO GS_OUTPUT-INFO  .
            ELSE .
              CONCATENATE  GS_OUTPUT-LIFNR   '修改成功'   INTO GS_OUTPUT-INFO  .
            ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
            MODIFY GT_OUTPUT FROM GS_OUTPUT.
          ENDLOOP.
          PERFORM FRM_GET_NEXT CHANGING G_LIFNR_1 GS_OUTPUT_1-KTOKK ."执行下个编号

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.

          WAIT UP TO '0.5' SECONDS.

        ELSE.
          "正式运行失败或警告
          LOOP AT LT_MSG INTO LWA_MSG.
            IF LWA_MSG-TYPE = 'W'.  "正式执行警告
              LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
                GS_OUTPUT-STATU = ICON_YELLOW_LIGHT..
                GS_OUTPUT-LIFNR = G_LIFNR .
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
                IF L_FLAG = '' .
                  CONCATENATE  GS_OUTPUT-LIFNR   '创建成功,但存在警告：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                ELSE .
                  CONCATENATE  GS_OUTPUT-LIFNR   '修改成功,但存在警告：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
                MODIFY GT_OUTPUT FROM GS_OUTPUT.
              ENDLOOP.
              PERFORM FRM_GET_NEXT CHANGING G_LIFNR_1 GS_OUTPUT_1-KTOKK .

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              WAIT UP TO '0.5' SECONDS.
              EXIT.
            ELSEIF LWA_MSG-TYPE = 'E' OR LWA_MSG-TYPE = 'A'.  "正式执行失败
              LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
                GS_OUTPUT-STATU = ICON_RED_LIGHT..
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
                IF L_FLAG = '' .
                  CONCATENATE  GS_OUTPUT-LIFNR   '创建失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                ELSE .
                  CONCATENATE  GS_OUTPUT-LIFNR   '修改失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
                MODIFY GT_OUTPUT FROM GS_OUTPUT.
              ENDLOOP.
              ROLLBACK WORK.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ELSE .
        "首次TEST 执行 警告或错误
        LOOP AT LT_MSG INTO LWA_MSG.
          IF LWA_MSG-TYPE = 'W'.  "TEST警告
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

            WAIT UP TO '0.5' SECONDS.
            CLEAR:G_TEST.
            PERFORM FRM_WRITE_DATA_SAP USING G_TEST  ."TABLES LT_DATA正式运行
            REFRESH LT_MSG .
            LT_MSG = LS_MESG-MESSAGES.
            IF LT_MSG IS INITIAL .
              LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
                GS_OUTPUT-STATU = ICON_OKAY.
                GS_OUTPUT-LIFNR = G_LIFNR .
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
                IF L_FLAG = '' .
                  CONCATENATE  GS_OUTPUT-LIFNR   '创建成功'   INTO GS_OUTPUT-INFO  .
                ELSE .
                  CONCATENATE  GS_OUTPUT-LIFNR   '修改成功'   INTO GS_OUTPUT-INFO  .
                ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
                MODIFY GT_OUTPUT FROM GS_OUTPUT.
              ENDLOOP.
              PERFORM FRM_GET_NEXT CHANGING G_LIFNR_1 GS_OUTPUT_1-KTOKK .

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              WAIT UP TO '0.5' SECONDS.
              EXIT.
            ELSE.
              "正式运行失败或警告
              LOOP AT LT_MSG INTO LWA_MSG.
                IF LWA_MSG-TYPE = 'W'.  "警告
                  LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
                    GS_OUTPUT-STATU = ICON_YELLOW_LIGHT..
                    GS_OUTPUT-LIFNR = G_LIFNR .
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
                    IF L_FLAG = '' .
                      CONCATENATE  GS_OUTPUT-LIFNR   '创建成功,但存在警告：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                    ELSE .
                      CONCATENATE  GS_OUTPUT-LIFNR   '修改成功,但存在警告：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                    ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
                    MODIFY GT_OUTPUT FROM GS_OUTPUT.
                  ENDLOOP.
                  PERFORM FRM_GET_NEXT CHANGING G_LIFNR_1 GS_OUTPUT_1-KTOKK .

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      WAIT = 'X'.

                  WAIT UP TO '0.5' SECONDS.
                  EXIT.
                ELSEIF LWA_MSG-TYPE = 'E' OR LWA_MSG-TYPE = 'A'.  "失败
                  LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
                    GS_OUTPUT-STATU = ICON_RED_LIGHT..
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
                    IF L_FLAG = '' .
                      CONCATENATE  GS_OUTPUT-LIFNR   '创建失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                    ELSE .
                      CONCATENATE  GS_OUTPUT-LIFNR   '修改失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
                    ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
                    MODIFY GT_OUTPUT FROM GS_OUTPUT.
                  ENDLOOP.
                  ROLLBACK WORK.
                  EXIT.
                ENDIF.
              ENDLOOP.
            ENDIF.
            EXIT.
          ELSEIF LWA_MSG-TYPE = 'E' OR LWA_MSG-TYPE = 'A'.  "失败
            ROLLBACK WORK.
            LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE  XH = GS_OUTPUT_1-XH .
              GS_OUTPUT-STATU = ICON_RED_LIGHT..
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  BEGIN
              IF L_FLAG = '' .
                CONCATENATE  GS_OUTPUT-LIFNR   '创建失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
              ELSE .
                CONCATENATE  GS_OUTPUT-LIFNR   '修改失败：' LWA_MSG-MESSAGE  INTO GS_OUTPUT-INFO  .
              ENDIF.
*&--代码添加 BY HANDYBY 22.08.2017 18:31:33  END
              MODIFY GT_OUTPUT FROM GS_OUTPUT.
            ENDLOOP.

            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      .
      FREE LS_MAIN.
      REFRESH LT_VEND.
    ENDAT .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD .
  DATA: LV_FNAME TYPE STRING,
        LV_TITLE TYPE STRING,
        LV_PATH  TYPE STRING VALUE 'D:/',
        LV_FPATH TYPE STRING VALUE 'D:/'.

  DATA: LS_WDATB   LIKE WWWDATATAB.
  DATA: LV_SUBRC   TYPE SY-SUBRC.
  DATA: GV_MSG  TYPE STRING,
        P_OBJID TYPE W3OBJID.

  P_OBJID = 'ZMM032'.
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
    CLEAR:LS_WDATB .
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
      P_FN = LV_FPATH.
      IF P_FN IS NOT INITIAL.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            KEY         = LS_WDATB
            DESTINATION = P_FN
          IMPORTING
            RC          = LV_SUBRC.
        IF LV_SUBRC NE 0.
          MESSAGE '模板下载失败！' TYPE 'E'.
        ELSE.
          CLEAR GV_MSG.
          CONCATENATE '模板下载到本地文件' P_FN INTO GV_MSG.
          MESSAGE GV_MSG TYPE 'S' .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_UPLOAD_DATA TABLES FU_DATA.
  REFRESH  FU_DATA[].
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FN
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65000
    TABLES
      INTERN                  = FU_DATA[]
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  IF FU_DATA[] IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_CONTACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_OUTPUT_LXR  text
*      -->P_GS_OUTPUT_TEL  text
*      -->P_GS_OUTPUT_MOBDH  text
*      -->P_GS_OUTPUT_FAX  text
*      -->P_GS_OUTPUT_EMAIL  text
*----------------------------------------------------------------------*
FORM FRM_FILL_CONTACT  TABLES PT_KNVK STRUCTURE GS_KNVK
                        USING    FU_NAME FU_PHONE FU_PHONE_M FU_FAX FU_MAIL
                                P_LIFNR .

  DATA:G_PARNR TYPE PARNR.
  IF FU_NAME IS NOT INITIAL OR FU_PHONE IS NOT INITIAL.
*&--代码添加 BY HANDYBY 22.08.2017 15:39:54  BEGIN
    DATA LS_KNVK LIKE LINE OF PT_KNVK .
    IF P_LIFNR IS NOT INITIAL .
      READ TABLE PT_KNVK INTO LS_KNVK WITH KEY LIFNR = P_LIFNR
                                               NAME1 = FU_NAME BINARY SEARCH .
      IF SY-SUBRC = 0 .
        G_PARNR = LS_KNVK-PARNR .

        LWA_CONTACT-TASK = 'U'.
        LWA_CONTACT-ADDRESS_TYPE_3-TASK = 'U'.
        LWA_PHONE1-CONTACT-TASK = 'U'.
        LWA_FAX_LXR-CONTACT-TASK = 'U'.
        LWA_SMTP_LXR-CONTACT-TASK = 'U'.

        CLEAR LS_KNVK .
      ELSE .
        CLEAR LWA_CONTACT.
        CLEAR:G_PARNR.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = 'AP'
            OBJECT                  = 'PARTNER'
            QUANTITY                = '1'
          IMPORTING
            NUMBER                  = G_PARNR
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

        LWA_CONTACT-TASK = 'I'.
        LWA_CONTACT-ADDRESS_TYPE_3-TASK = 'I'.
        LWA_PHONE1-CONTACT-TASK = 'I'.
        LWA_FAX_LXR-CONTACT-TASK = 'I'.
        LWA_SMTP_LXR-CONTACT-TASK = 'I'.
      ENDIF.

    ELSE .

*&--代码添加 BY HANDYBY 22.08.2017 15:39:54  END
      CLEAR LWA_CONTACT.
      CLEAR:G_PARNR.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = 'AP'
          OBJECT                  = 'PARTNER'
          QUANTITY                = '1'
        IMPORTING
          NUMBER                  = G_PARNR
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      LWA_CONTACT-TASK = 'I'.
      LWA_CONTACT-ADDRESS_TYPE_3-TASK = 'I'.
      LWA_PHONE1-CONTACT-TASK = 'I'.
      LWA_FAX_LXR-CONTACT-TASK = 'I'.
      LWA_SMTP_LXR-CONTACT-TASK = 'I'.
    ENDIF.


    LWA_CONTACT-DATA_KEY-PARNR = G_PARNR.
*lwa_contact-address_type_3-postal-data-firstname = 'aa'.
    LWA_CONTACT-ADDRESS_TYPE_3-POSTAL-DATA-LASTNAME = FU_NAME.
*lwa_contact-address_type_3-postal-datax-firstname = 'X'.
    LWA_CONTACT-ADDRESS_TYPE_3-POSTAL-DATAX-LASTNAME = 'X'.
    "联系人电话
    CLEAR:LWA_PHONE1.
    REFRESH LT_PHONE1.

    LWA_PHONE1-CONTACT-DATA-TELEPHONE = FU_PHONE..
    LWA_PHONE1-CONTACT-DATAX-TELEPHONE = 'X'.
    APPEND LWA_PHONE1 TO LT_PHONE1.

    LWA_PHONE1-CONTACT-DATA-TELEPHONE  = FU_PHONE_M.
    LWA_PHONE1-CONTACT-DATA-R_3_USER   = '2'.
    LWA_PHONE1-CONTACT-DATAX-TELEPHONE = 'X'.
    LWA_PHONE1-CONTACT-DATAX-R_3_USER  = 'X'.
    APPEND LWA_PHONE1 TO LT_PHONE1.

    LWA_CONTACT-ADDRESS_TYPE_3-COMMUNICATION-PHONE-PHONE = LT_PHONE1.

    "传真
    CLEAR: LWA_FAX_LXR.
    REFRESH: LT_FAX_LXR .

    LWA_FAX_LXR-CONTACT-DATA-FAX = FU_FAX.
    LWA_FAX_LXR-CONTACT-DATAX-FAX = 'X'.
    APPEND LWA_FAX_LXR TO LT_FAX_LXR .
    LWA_CONTACT-ADDRESS_TYPE_3-COMMUNICATION-FAX-FAX = LT_FAX_LXR.
    "电子邮件
    CLEAR:LWA_SMTP_LXR .
    REFRESH: LT_SMTP_LXR.

    LWA_SMTP_LXR-CONTACT-DATA-E_MAIL = FU_MAIL.
    LWA_SMTP_LXR-CONTACT-DATAX-E_MAIL = 'X'.
    APPEND LWA_SMTP_LXR TO  LT_SMTP_LXR .
    LWA_CONTACT-ADDRESS_TYPE_3-COMMUNICATION-SMTP-SMTP = LT_SMTP_LXR.


    APPEND LWA_CONTACT TO LT_CONTACTS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_TEST  text
*----------------------------------------------------------------------*
FORM CREATE_BAPI  USING    P_G_TEST.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_NEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_KUNNR  text
*      <--P_GS_OUTPUT_1_KTOKD  text
*----------------------------------------------------------------------*
FORM FRM_GET_NEXT  CHANGING  L_LIFNR TYPE LIFNR
                           L_KT0KK TYPE KTOKK.
  READ TABLE GT_V_077K_B INTO GS_V_077K_B
     WITH KEY KTOKK = L_KT0KK BINARY SEARCH .
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE '
    EXPORTING
      OBJECT           = 'KREDITOR'
    EXCEPTIONS
      FOREIGN_LOCK     = 1
      OBJECT_NOT_FOUND = 2
      SYSTEM_FAILURE   = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*  **如果号码范围存在
  IF SY-SUBRC EQ 0.
    " *  ****得到一个号码，
    CALL FUNCTION 'NUMBER_GET_NEXT '
      EXPORTING
        NR_RANGE_NR             = GS_V_077K_B-NUMKR    "这个就是维护的间隔号
        OBJECT                  = 'KREDITOR'           "这个就是流水号对象
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = L_LIFNR             "获得的流水号
*       quantity                = quant
*       returncode              = code
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    " *  ***将号码累加
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE '
      EXPORTING
        OBJECT           = 'KREDITOR'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    RAISE NUM_RANGE_ERROR .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_WRITE_DATA_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_TEST  text
*----------------------------------------------------------------------*
FORM FRM_WRITE_DATA_SAP  USING    P_RUN .
  "perform   create_bapi using  g_test  .
  REFRESH LT_MSG.

  CALL METHOD VMD_EI_API=>MAINTAIN_BAPI
    EXPORTING
      IV_TEST_RUN              = P_RUN
      IV_COLLECT_MESSAGES      = 'X'
      IS_MASTER_DATA           = LS_MAIN
    IMPORTING
      ES_MASTER_DATA_CORRECT   = LS_MAIN1
      ES_MESSAGE_CORRECT       = LS_MESG1
      ES_MASTER_DATA_DEFECTIVE = LS_MAIN2
      ES_MESSAGE_DEFECTIVE     = LS_MESG.


ENDFORM.
