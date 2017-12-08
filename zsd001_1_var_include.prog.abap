*&---------------------------------------------------------------------*
*&  包含                ZSD001_VAR_INCLUDE
*&---------------------------------------------------------------------*
*客户基本数据
TYPES:BEGIN OF TY_BASIS,
        KTOKD       TYPE   KNA1-KTOKD  , "账户组
        KUNNR       TYPE   KNA1-KUNNR   , "客户编号
        TXT30       TYPE   T077X-TXT30 , "账户组描述
        BUKRS       TYPE   KNB1-BUKRS  , "公司
        ANRED       TYPE   KNVK-ANRED  , "称谓
        NAME1       TYPE   C LENGTH 80 , "adrc-name1  ,"名称1
        SORTL       TYPE   C LENGTH 80 , "adrc-sort1  ,"搜索项
        LAND1       TYPE   KNA1-LAND1  , "国家
        LANDX       TYPE   T005T-LANDX , "国家描述
        REGIO       TYPE   KNA1-REGIO  , "地区（省）
        BEZEI       TYPE   T005U-BEZEI , "省描述
        CITY1       TYPE   ADRC-CITY1,   "市
        BZIRK       TYPE   KNVV-BZIRK  , "行政区域
        BZTXT       TYPE   T171T-BZTXT , "行政区域描述
        STRAS       TYPE   C LENGTH 60, "KNA1-STRAS  , "注册地址
        STR_SUPPL3  TYPE   C LENGTH 80, "办公地址
        POST_CODE1  TYPE   C LENGTH 80, "邮政编号 new
        TEL1        TYPE   C LENGTH 80, "座机
        TEL2        TYPE   C LENGTH 80, "移动电话
        FAX         TYPE   C LENGTH 80, "adrc-fax_number ,"传真
        SMTP_ADDR   TYPE   ADR6-SMTP_ADDR, "电子邮箱
        NAME1_L     TYPE   KNVK-NAME1,  "主要联系人
        TEL_NUMBER  TYPE   C LENGTH 80, "联系人座机
        TEL_M       TYPE   C LENGTH 80, "联系人移动电话
        KDGRP       TYPE   KNVV-KDGRP,  "客户来源
        KTEXT       TYPE   T151T-KTEXT, "客户来源描述
        PLTYP       TYPE   KNVV-PLTYP,  "客户类型
        PTEXT       TYPE   T189T-PTEXT, "客户类型描述
        KLABC       TYPE   KNVV-KLABC,  "客户等级
        BANKS       TYPE   KNBK-BANKS,  "银行国家
        BANKL       TYPE   KNBK-BANKL,  "客户开户行
        BANKN       TYPE   KNBK-BANKN,  "银行帐号
        BKREF       TYPE   KNBK-BKREF,  "参考细节
        STCEG       TYPE   C LENGTH 80, "纳税人识别号
        REMARK      TYPE   AD_REMARK1,  "其他
        KNURL       TYPE   AD_EXTENS1,  "网页，数据线
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  BEGIN
        KUNNR_EXIST TYPE C ,  "客户是否存在标识
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  END
      END OF TY_BASIS.

*客户的公司代码数据
TYPES:BEGIN OF TY_COM,
        KTOKD       TYPE  KNA1-KTOKD  , "科目组
        KUNNR       TYPE  KNA1-KUNNR  , "客户编号
        BUKRS       TYPE  KNB1-BUKRS  , "公司代码
        ZUAWA       TYPE  KNB1-ZUAWA  , "排序码 new
        AKONT       TYPE  KNB1-AKONT  , "统驭科目
        ZWELS       TYPE  KNB1-ZWELS  , "付款方法 new
        ZTERM       TYPE  KNB1-ZTERM  , "付款条件(公司代码数据)
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  BEGIN
        BUKRS_EXIST TYPE C ,  "公司代码是否存在标识
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  END
      END OF TY_COM.

TYPES:BEGIN OF TY_ORG,
        KTOKD       TYPE  KNA1-KTOKD,   "科目组
        KUNNR       TYPE  KNA1-KUNNR,   "客户编码
        VKORG       TYPE  KNVV-VKORG  , "销售组织
        VTWEG       TYPE  KNVV-VTWEG  , "分销渠道
        SPART       TYPE  KNVV-SPART  , "产品组
        WAERS       TYPE  KNVV-WAERS,   "币别
        KALKS       TYPE  KNVV-KALKS  , "客户定价过程 new
        VSBED       TYPE  KNVV-VSBED  , "装运条件
        INCO1       TYPE  KNVV-INCO1  , "国际贸易条款1
        INCO2       TYPE  KNVV-INCO2,   "国际贸易条款2
        ZTERM       TYPE  KNVV-ZTERM  , "付款条款
        KTGRD       TYPE  KNVV-KTGRD  , "账户分配组
        TAXKD       TYPE  KNVI-TAXKD  , "税分类
        BZIRK       TYPE  KNVV-BZIRK,   "行政区域
        KLABC       TYPE  KNVV-KLABC,   "客户等级
        PLTYP       TYPE  KNVV-PLTYP,   "客户类型
        KDGRP       TYPE  KNVV-KDGRP,   "客户来源
        KUNN2       TYPE  KNVP-KUNN2,  "销售业务员
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  BEGIN
        VKORG_EXIST TYPE C ,  "销售组织是否存在标识
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  END
      END OF TY_ORG.

*银行信息数据
TYPES:BEGIN OF TY_BANK,
        KUNNR TYPE  KNBK-KUNNR,   "客户编号
        BANKS TYPE   KNBK-BANKS,  "银行国家
        BANKL TYPE   KNBK-BANKL,  "客户开户行
        BANKN TYPE   KNBK-BANKN,  "银行帐号
        BKREF TYPE   KNBK-BKREF,  "参考细节
      END OF TY_BANK.

*导入模版数据
TYPES:BEGIN OF TY_DATA,
        KTOKD      TYPE   KNA1-KTOKD  , "账户组
        TXT30      TYPE   T077X-TXT30 , "账户组描述
        BUKRS      TYPE   KNB1-BUKRS,   "公司代码
        ANRED      TYPE   KNVK-ANRED  , "称谓
        NAME1      TYPE   C LENGTH 80 , "adrc-name1  ,"名称1
        SORTL      TYPE   C LENGTH 80 , "adrc-sort1  ,"搜索项
        LAND1      TYPE   KNA1-LAND1  , "国家
        LANDX      TYPE   T005T-LANDX , "国家描述
        REGIO      TYPE   KNA1-REGIO  , "地区（省）
        BEZEI      TYPE   T005U-BEZEI , "省描述
        CITY1      TYPE   ADRC-CITY1,   "市
        BZIRK      TYPE   KNVV-BZIRK  , "行政区域
        BZTXT      TYPE   T171T-BZTXT , "行政区域描述
        STRAS(60)  TYPE   C, "KNA1-STRAS  , "注册地址
        STR_SUPPL3 TYPE   C LENGTH 80,  "办公地址
        POST_CODE1 TYPE   C LENGTH 80,  "邮政编号 new
        TEL1       TYPE   C LENGTH 80,  "座机
        TEL2       TYPE   C LENGTH 80,  "移动电话
        FAX        TYPE   C LENGTH 80,  "adrc-fax_number ,"传真
        KNURL      TYPE   AD_EXTENS1,
        SMTP_ADDR  TYPE   ADR6-SMTP_ADDR, "电子邮箱
        NAME1_L    TYPE   KNVK-NAME1,  "主要联系人
        TEL_NUMBER TYPE   C LENGTH 80, "联系人座机
        TEL_M      TYPE   C LENGTH 80, "联系人移动电话
        KDGRP      TYPE   KNVV-KDGRP,  "客户来源
        KTEXT      TYPE   T151T-KTEXT, "客户来源描述
        PLTYP      TYPE   KNVV-PLTYP,  "客户类型
        PTEXT      TYPE   T189T-PTEXT, "客户类型描述
        KLABC      TYPE   KNVV-KLABC,  "客户等级
        BANKS      TYPE   KNBK-BANKS,  "银行国家
        BANKL      TYPE   KNBK-BANKL,  "客户开户行
        BANKA      TYPE   BNKA-BANKA,  "客户开户行描述
        BANKN      TYPE   KNBK-BANKN,  "银行帐号
        BKREF      TYPE   KNBK-BKREF,  "参考细节
        STCEG      TYPE   C LENGTH 80, "纳税人识别号
        REMARK     TYPE   AD_REMARK1, "ADRC-REMARK, "其他
        AKONT      TYPE   KNB1-AKONT,  "统驭科目
        VKORG      TYPE   KNVV-VKORG  , "销售组织
        VTWEG      TYPE   KNVV-VTWEG  , "分销渠道
        SPART      TYPE   KNVV-SPART  , "产品组
        INCO1      TYPE   KNVV-INCO1  , "国际贸易条款1
        INCO2      TYPE   KNVV-INCO2  , "国际贸易条款2
        ZTERM      TYPE   KNVV-ZTERM  , "付款条件(销售区域数据)
        TEXT1      TYPE   T052U-TEXT1,  "付款条款描述
        WAERS      TYPE   KNVV-WAERS  , "货币
        KUNN2      TYPE   KNVP-KUNN2,   "销售业务员
        KALKS      TYPE   KNVV-KALKS,   "客户定价过程
        VSBED      TYPE   KNVV-VSBED,   "装运条件
        KTGRD      TYPE   KNVV-KTGRD,   "账户分配组
        TAXKD      TYPE   KNVI-TAXKD,  "税分类
        KUNNR      TYPE   KNA1-KUNNR  , "客户编号
      END OF TY_DATA.

TYPES:BEGIN OF TY_OUTPUT,
        STATU       TYPE   ICONNAME,     "状态烂
        MESSAGE     TYPE   CHAR200  ,    "消息
        KTOKD       TYPE   KNA1-KTOKD  , "账户组
        TXT30       TYPE   T077X-TXT30 , "账户组描述
        BUKRS       TYPE   KNB1-BUKRS,   "公司代码
        ANRED       TYPE   KNVK-ANRED  , "称谓
        NAME1       TYPE   C LENGTH 80 , "adrc-name1  ,"名称1
        SORTL       TYPE   C LENGTH 80 , "adrc-sort1  ,"搜索项
        LAND1       TYPE   KNA1-LAND1  , "国家
        LANDX       TYPE   T005T-LANDX , "国家描述
        REGIO       TYPE   KNA1-REGIO  , "地区（省）
        BEZEI       TYPE   T005U-BEZEI , "省描述
        CITY1       TYPE   ADRC-CITY1,   "市
        BZIRK       TYPE   KNVV-BZIRK  , "行政区域
        BZTXT       TYPE   T171T-BZTXT , "行政区域描述
        STRAS(60)   TYPE   C, "KNA1-STRAS  , "注册地址
        STR_SUPPL3  TYPE   C LENGTH 80, "办公地址
        POST_CODE1  TYPE   C LENGTH 80, "邮政编号 new
        TEL1        TYPE   C LENGTH 80, "座机
        TEL2        TYPE   C LENGTH 80, "移动电话
        FAX         TYPE   C LENGTH 80, "adrc-fax_number ,"传真
        KNURL       TYPE   AD_EXTENS1,  "网页
        SMTP_ADDR   TYPE   ADR6-SMTP_ADDR, "电子邮箱
        NAME1_L     TYPE   KNVK-NAME1,  "主要联系人
        TEL_NUMBER  TYPE   C LENGTH 80, "联系人座机
        TEL_M       TYPE   C LENGTH 80, "联系人移动电话
        KDGRP       TYPE   KNVV-KDGRP,  "客户来源
        KTEXT       TYPE   T151T-KTEXT, "客户来源描述
        PLTYP       TYPE   KNVV-PLTYP,  "客户类型
        PTEXT       TYPE   T189T-PTEXT, "客户类型描述
        KLABC       TYPE   KNVV-KLABC,  "客户等级
        BANKS       TYPE   KNBK-BANKS,  "银行国家
        BANKL       TYPE   KNBK-BANKL,  "客户开户行
        BANKA       TYPE   BNKA-BANKA,  "客户开户行描述
        BANKN       TYPE   KNBK-BANKN,  "银行帐号
        BKREF       TYPE   KNBK-BKREF,  "参考细节
        STCEG       TYPE   C LENGTH 80, "纳税人识别号
        REMARK      TYPE   C LENGTH 80, "ADRC-REMARK, "其他
        AKONT       TYPE   KNB1-AKONT,  "统驭科目
        VKORG       TYPE   KNVV-VKORG  , "销售组织
        VTWEG       TYPE   KNVV-VTWEG  , "分销渠道
        SPART       TYPE   KNVV-SPART  , "产品组
        INCO1       TYPE   KNVV-INCO1  ,                     "国际贸易条款1
        INCO2       TYPE   KNVV-INCO2  ,                     "国际贸易条款2
        ZTERM       TYPE   KNVV-ZTERM  , "付款条件(销售区域数据)
        TEXT1       TYPE   T052U-TEXT1, "付款条款描述
        WAERS       TYPE   KNVV-WAERS  , "货币
        KUNN2       TYPE   KNVP-KUNN2,    "销售业务员
        KALKS       TYPE   KNVV-KALKS,    "客户定价过程
        VSBED       TYPE   KNVV-VSBED,    "装运条件
        KTGRD       TYPE   KNVV-KTGRD,    "账户分配组
        TAXKD       TYPE   KNVI-TAXKD,   "税分类
        KUNNR       TYPE   KNA1-KUNNR  , "客户编号
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  BEGIN
        KUNNR_EXIST TYPE C ,  "客户是否存在标识
        BUKRS_EXIST TYPE C ,  "公司代码是否存在标识
        VKORG_EXIST TYPE C ,  "销售组织是否存在标识
*&--代码添加 BY HANDYBY 25.06.2017 11:29:41  END
      END OF TY_OUTPUT.


TYPES:BEGIN OF TY_KNVP,
        KUNNR TYPE KNVP-KUNNR,
        VKORG TYPE KNVP-VKORG,
        VTWEG TYPE KNVP-VTWEG,
        SPART TYPE KNVP-SPART,
      END OF TY_KNVP.

DATA:W_KNVP TYPE TY_KNVP.

DATA:
  T_DATA TYPE STANDARD TABLE OF TY_DATA,
  W_DATA TYPE TY_DATA.

DATA:LV_FLG.
DATA:G_PARNR TYPE PARNR..
DATA:LS_MAIN       TYPE CMDS_EI_MAIN,
     LS_MAIN1      TYPE CMDS_EI_MAIN,
     LS_MAIN2      TYPE CMDS_EI_MAIN,
     LS_MESG       TYPE CVIS_MESSAGE,
     LS_MESG1      TYPE CVIS_MESSAGE,
     LT_MSG        TYPE BAPIRET2_T,
     LWA_MSG       TYPE BAPIRET2,
     LT_CUST       TYPE CMDS_EI_EXTERN_T,
     LWA_CUST      TYPE CMDS_EI_EXTERN,
     LWA_SALE      TYPE CMDS_EI_SALES,
     LT_SALE       TYPE CMDS_EI_SALES_T,
     LT_FUNCTIONS  TYPE CMDS_EI_FUNCTIONS_T,
     LWA_FUNCTIONS TYPE CMDS_EI_FUNCTIONS,
     LT_COMPANY    TYPE CMDS_EI_COMPANY_T,
     LT_TEXT       TYPE CVIS_EI_TEXT_T,
     LWA_TEXT      TYPE CVIS_EI_TEXT,
     LT_CONTENT    TYPE TLINE_TAB,
     LWA_CONTENT   TYPE TLINE,
     LWA_TAX       TYPE CMDS_EI_TAX_IND,
     LT_TAX        TYPE CMDS_EI_TAX_IND_T,
     LT_REMARK     TYPE CVIS_EI_REM_T,
     LWA_REMARK    TYPE CVIS_EI_REM,
     LT_TEL        TYPE CVIS_EI_TLX_T,
     LWA_TEL       TYPE CVIS_EI_TLX_STR,
     LT_FAX        TYPE CVIS_EI_FAX_T,
     LWA_FAX       TYPE CVIS_EI_FAX_STR,
     LT_MAIL       TYPE CVIS_EI_SMTP_T,
     LWA_MAIL      TYPE CVIS_EI_SMTP_STR,
     LT_TEL01      TYPE CVIS_EI_TTX_T,
     LWA_TEL01     TYPE CVIS_EI_TTX_STR,
*银行信息
     LT_BANK       TYPE CVIS_EI_BANKDETAIL_T,
     LWA_BANK      TYPE CVIS_EI_CVI_BANKDETAIL,

     LT_CONTACT    TYPE CMDS_EI_CONTACTS_T,
     LWA_CONTACT   TYPE CMDS_EI_CONTACTS,
     LT_VERSION    TYPE CVI_EI_VERSION_TYPE1_T,
     LWA_VERSION   TYPE CVI_EI_VERSION_TYPE1,
     LT_PHONE      TYPE CVIS_EI_PHONE_T,
     LWA_PHONE     TYPE CVIS_EI_PHONE_STR,
     LT_PHONE1     TYPE CVIS_EI_PHONE_T,
     LWA_PHONE1    TYPE CVIS_EI_PHONE_STR,
**************************add fax_lxr******
     LT_FAX_LXR    TYPE CVIS_EI_FAX_T,
     LWA_FAX_LXR   TYPE CVIS_EI_FAX_STR,
*固定电话
     LT_TTX_LXR    TYPE CVIS_EI_TTX_T,
     LWA_TTX_LXR   TYPE CVIS_EI_TTX_STR,
*联系人移动电话
     LT_TLX_LXR    TYPE CVIS_EI_TLX_T,
     LWA_TLX_LXR   TYPE CVIS_EI_TLX_STR,
* 电邮
     LT_SMTP_LXR   TYPE CVIS_EI_SMTP_T,
     LWA_SMTP_LXR  TYPE CVIS_EI_SMTP_STR,
****************************add_fac_lxr*****
     LWA_COMPANY   TYPE CMDS_EI_COMPANY.
CONSTANTS:CON_INSERT TYPE C VALUE 'I',
          CON_UPDATE TYPE C VALUE 'M'.
DATA:T_TPAKD TYPE STANDARD TABLE OF TPAKD,
     W_TPAKD TYPE TPAKD.
DATA:
  T_BASIS TYPE STANDARD TABLE OF TY_BASIS,
  W_BASIS TYPE TY_BASIS,
  W_ORG   TYPE TY_ORG,
  T_ORG   TYPE STANDARD TABLE OF TY_ORG,
  T_COM   TYPE STANDARD TABLE OF TY_COM,
  W_COM   TYPE TY_COM.

DATA:T_BANK TYPE STANDARD TABLE OF TY_BANK,
     W_BANK TYPE TY_BANK.

DATA:L_COUNT_ZP TYPE KNVP-PARZA,
     L_COUNT_SM TYPE KNVP-PARZA,
     L_COUNT_WE TYPE KNVP-PARZA.

DATA:T_TPAER TYPE STANDARD TABLE OF TPAER,
     W_TPAER TYPE V_TPAER_SD.
DATA:T_TKUPA TYPE STANDARD TABLE OF TKUPA,
     W_TKUPA TYPE TKUPA.

DATA GT_OUTPUT TYPE TABLE OF TY_OUTPUT.
DATA GS_OUTPUT TYPE TY_OUTPUT.

DATA G_KUNNR TYPE KUNNR."客户编号
DATA G_TEST TYPE C.    "测试

DATA GT_V_077D_B TYPE TABLE OF V_077D_B."取出流水号对象
DATA GS_V_077D_B TYPE V_077D_B.

* 取客户对应的联系人
DATA: BEGIN OF GS_KNVK,
        PARNR TYPE KNVK-PARNR,
        KUNNR TYPE KNVK-KUNNR,
        NAME1 TYPE KNVK-NAME1,
*          TELF1 TYPE KNVK-TELF1,
      END OF GS_KNVK .
DATA GT_KNVK LIKE TABLE OF GS_KNVK .

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

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
