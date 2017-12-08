*&---------------------------------------------------------------------*
*& Report  ZSD002
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/01/20
*& Request       :
*& Descriptions  : 客户主数据查询报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*

REPORT ZSD002.
************************************************************************
* Tables
************************************************************************
TABLES: KNA1, KNB1,KNVV.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF TY_DATA,
        KUNNR       TYPE KNA1-KUNNR,  "客户编号
        ERDAT       TYPE KNA1-ERDAT,  "创建日期
        BUKRS       TYPE KNB1-BUKRS,  "公司代码
        KTOKD       TYPE KNA1-KTOKD,  "账户组
        TEXT30      TYPE KNA1-ANRED,  "账户组描述
        ANRED       TYPE KNA1-ANRED,  "标题
        NAME1       TYPE ADRC-NAME1,  "客户名称
        SORTL       TYPE KNA1-SORTL,  "搜索项
        LAND1       TYPE KNA1-LAND1,  "国家
        LANDX       TYPE T005T-LANDX, "国家描述
        REGIO       TYPE KNA1-REGIO,  "省
        BEZEI       TYPE T005U-BEZEI, "省描述
        ORT01       TYPE KNA1-ORT01,  "市
        BZIRK       TYPE KNVV-BZIRK,  "行政区域
        BZTXT       TYPE T171T-BZTXT, "行政区域描述
        STRAS       TYPE ADRC-STREET,  "注册地址
        STR_SUPPL3  TYPE ADRC-STR_SUPPL3, "办公室地址
        PSTLZ       TYPE KNA1-PSTLZ,      "邮政编码
        TELF1       TYPE KNA1-TELF1,      "座机
        TELF2       TYPE KNA1-TELF2,      "移动电话
        TELFX       TYPE KNA1-TELFX,      "传真
        KNURL       TYPE KNA1-KNURL,      "网页
        SMTP_SRCH   TYPE ADR6-SMTP_SRCH,  "电子邮箱
        NAME1_L     TYPE KNVK-NAME1,      "主要联系人
        TEL_NUMBER1 TYPE ADR2-TEL_NUMBER, "座机
        TEL_NUMBER2 TYPE ADR2-TEL_NUMBER, "手机
        KDGRP       TYPE KNVV-KDGRP,  "客户来源
        KTEXT       TYPE T151T-KTEXT, "客户来源描述
        PLTYP       TYPE KNVV-PLTYP,  "客户类型
        PTEXT       TYPE T189T-PTEXT, "代理
        KLABC       TYPE KNVV-KLABC,  "客户等级
        BANKS       TYPE KNBK-BANKS,  "银行国家
        BANKL       TYPE KNBK-BANKL,  "客户开户行
        BANKA       TYPE BNKA-BANKA,  "客户开户行描述
        BANKN       TYPE KNBK-BANKN,  "银行帐号
        BKREF       TYPE KNBK-BKREF,  "参考细节
        STCEG       TYPE KNA1-STCEG,  "纳税人识别号
        REMARK      TYPE ADRCT-REMARK, "其他
        AKONT       TYPE KNB1-AKONT, "统驭科目
        VKORG       TYPE KNVV-VKORG, "销售组织
        VTEXT       TYPE TVKOT-VTEXT, "销售组织描述
        VTWEG       TYPE KNVV-VTWEG, "分销渠道
        SPART       TYPE KNVV-SPART, "产品组
        INCO1       TYPE KNVV-INCO1, "国际贸易条款1
        INCO2       TYPE KNVV-INCO2, "国际贸易条款2
        ZTERM       TYPE KNVV-ZTERM, "付款条款
        TEXT1       TYPE T052U-TEXT1, "销售付款条款描述
        WAERS       TYPE KNVV-WAERS, "货币
        KUNN2       TYPE KNVP-KUNN2, "销售业务人员
        NAME1_Y     TYPE KNA1-NAME1, "业务员姓名
        ADRNR       TYPE KNA1-ADRNR, "地址码
      END OF TY_DATA.

TYPES:BEGIN OF TY_KNBK,
        KUNNR TYPE KNBK-KUNNR,  "客户编码
        BANKS TYPE KNBK-BANKS,  "银行国家
        BANKL TYPE KNBK-BANKL,  "客户开户行
        BANKA TYPE BNKA-BANKA,  "客户开户行描述
        BANKN TYPE KNBK-BANKN,  "银行帐号
        BKREF TYPE KNBK-BKREF,  "参考细节
      END OF TY_KNBK.

************************************************************************
* Internal Table
************************************************************************
DATA: GT_DATA    TYPE TABLE OF TY_DATA,
      GT_KNVK    TYPE TABLE OF KNVK,
      GT_KNBK    TYPE TABLE OF TY_KNBK,
      GT_ADRC    TYPE TABLE OF ADRC,
      GT_ADR2    TYPE TABLE OF ADR2,
      GT_ADR6    TYPE TABLE OF ADR6,
      GT_T077X   TYPE TABLE OF T077X,
      GT_T005T   TYPE TABLE OF T005T,
      GT_T005U   TYPE TABLE OF T005U,
      GT_T171T   TYPE TABLE OF T171T,
      GT_T151T   TYPE TABLE OF T151T,
      GT_T189T   TYPE TABLE OF T189T,
      GT_T052U   TYPE TABLE OF T052U,
      GT_TVKOT   TYPE TABLE OF TVKOT,
      GT_KNVP    TYPE TABLE OF KNVP,
      GT_KNA1    TYPE TABLE OF KNA1,
      GT_ADRCT   TYPE TABLE OF ADRCT,
      GT_ZSD_002 TYPE TABLE OF ZSD_002.

************************************************************************
* WorkArea
************************************************************************
DATA: GS_DATA    TYPE TY_DATA,
      GS_KNVK    TYPE KNVK,
      GS_KNBK    TYPE TY_KNBK,
      GS_ADRC    TYPE ADRC,
      GS_ADR2    TYPE ADR2,
      GS_ADR6    TYPE ADR6,
      GS_T077X   TYPE T077X,
      GS_T005T   TYPE T005T,
      GS_T005U   TYPE T005U,
      GS_T171T   TYPE T171T,
      GS_T151T   TYPE T151T,
      GS_T189T   TYPE T189T,
      GS_T052U   TYPE T052U,
      GS_TVKOT   TYPE TVKOT,
      GS_KNVP    TYPE KNVP,
      GS_KNA1    TYPE KNA1,
      GS_ADRCT   TYPE ADRCT,
      GS_ZSD_002 TYPE ZSD_002.

DATA:IT_TVKO TYPE TVKO OCCURS 0 WITH HEADER LINE.
DATA:IT_T001 TYPE T001 OCCURS 0 WITH HEADER LINE.
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

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS.

DATA: GT_LVC           TYPE LVC_T_FCAT,
      GT_SORT          TYPE LVC_T_SORT,
      GW_LAYOUT        TYPE LVC_S_LAYO,                    "alv的格式
      GW_VARIANT       TYPE DISVARIANT,
      GW_GRID_SETTINGS TYPE LVC_S_GLAY,
      GW_LVC           TYPE LVC_S_FCAT,
      GW_SORT          TYPE LVC_S_SORT,
      GW_GRID_SETTING  TYPE LVC_S_GLAY,
      G_REPID          LIKE SY-REPID,                      "SY-REPID 指 当前的主程序
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
* Global Variant
************************************************************************


************************************************************************
* Constant
************************************************************************


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_KUNNR FOR KNA1-KUNNR,
                S_ERDAT FOR KNA1-ERDAT,
                S_BUKRS FOR KNB1-BUKRS,
*&--代码注释 BY HANDYBY 21.06.2017 10:36:07  BEGIN
*    S_VKORG FOR KNVV-VKORG OBLIGATORY,
*&--代码注释 BY HANDYBY 21.06.2017 10:36:07  END
*&--代码添加 BY HANDYBY 21.06.2017 10:35:56  BEGIN
    S_VKORG FOR KNVV-VKORG ,
*&--代码添加 BY HANDYBY 21.06.2017 10:35:56  END

                S_BZIRK FOR KNVV-BZIRK,
                S_KDGRP FOR KNVV-KDGRP,
                S_PLTYP FOR KNVV-PLTYP.
SELECTION-SCREEN END OF BLOCK BLK1.

*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.

  SELECT VKORG FROM TVKO
    INTO CORRESPONDING FIELDS OF TABLE IT_TVKO.

  SELECT BUKRS FROM T001
    INTO CORRESPONDING FIELDS OF TABLE IT_T001.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA."处理数逻辑
  PERFORM FRM_ALV_SHOW. "ALV显示

*&---------------------------------------------------------------------*
*& 程序结束处理
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*  PERFORM xxxxxxx.

*&---------------------------------------------------------------------*
*& 权限控制
*&---------------------------------------------------------------------*
FORM FRM_AUTH_CHECK.
  LOOP AT IT_TVKO WHERE VKORG IN S_VKORG.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
             ID 'VKORG' FIELD IT_TVKO-VKORG.
    IF SY-SUBRC <> 0.
      MESSAGE E430(VELO) WITH IT_TVKO-VKORG.
    ENDIF.
  ENDLOOP.
*
*  LOOP AT IT_T001 WHERE BUKRS IN S_BUKRS.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*             ID 'BUKRS' FIELD IT_T001-BUKRS.
*    IF SY-SUBRC <> 0.
*      MESSAGE E011(ZFICO01) WITH IT_T001-BUKRS.
*    ENDIF.
*  ENDLOOP.
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

*客户基本数据，公司代码数据，销售数据
*  SELECT *
*    FROM KNA1
*    LEFT OUTER JOIN KNVV ON
*    KNA1~KUNNR = KNVV~KUNNR
*    LEFT OUTER JOIN KNB1 ON
*    KNA1~KUNNR = KNB1~KUNNR
*    INTO CORRESPONDING FIELDS OF TABLE GT_DATA
*    WHERE KNA1~KUNNR IN S_KUNNR
*    AND   KNA1~ERDAT IN S_ERDAT
*    AND   KNVV~VKORG IN S_VKORG
*    AND   KNVV~BZIRK IN S_BZIRK
*    AND   KNVV~KDGRP IN S_KDGRP
*    AND   KNVV~PLTYP IN S_PLTYP
*    AND   KNB1~BUKRS IN S_BUKRS.

  SELECT *  FROM ZSD_002
    INTO CORRESPONDING FIELDS OF TABLE GT_ZSD_002
    WHERE ZSD_002~KUNNR IN S_KUNNR
    AND   ZSD_002~ERDAT IN S_ERDAT
    AND   ZSD_002~VKORG IN S_VKORG
    AND   ZSD_002~BZIRK IN S_BZIRK
    AND   ZSD_002~KDGRP IN S_KDGRP
    AND   ZSD_002~PLTYP IN S_PLTYP
    AND   ZSD_002~BUKRS IN S_BUKRS.

*匹配销售组织和公司代码
  DELETE  GT_ZSD_002 WHERE ( BUKRS = '1000'
                  AND    VKORG <> '1000').

  DELETE  GT_ZSD_002 WHERE ( BUKRS = '1310'
                  AND    VKORG <> '1310').

  DELETE  GT_ZSD_002 WHERE ( BUKRS = '1200'
                AND    VKORG <> '1200').

  DELETE  GT_ZSD_002 WHERE ( BUKRS = '1300'
              AND    VKORG <> '1300').

  DELETE  GT_ZSD_002 WHERE ( BUKRS = '1800'
              AND    VKORG <> '1800').

  DELETE  GT_ZSD_002 WHERE  BUKRS = '1500'
             AND   ( VKORG <> '1500'
             AND      VKORG <> '1510').

*1100公司代码 对应1100，1110 两个销售组织
  DELETE  GT_ZSD_002 WHERE BUKRS = '1100'
                    AND ( VKORG <> '1100'
                    AND  VKORG <> '1110').

  CHECK GT_ZSD_002 IS NOT INITIAL.

*联系人信息
  SELECT * FROM KNVK
  INTO CORRESPONDING FIELDS OF TABLE GT_KNVK
  FOR ALL ENTRIES IN GT_ZSD_002
  WHERE KUNNR = GT_ZSD_002-KUNNR.

*联系人电话
  SELECT * FROM ADR2
    INTO CORRESPONDING FIELDS OF TABLE GT_ADR2
   FOR ALL ENTRIES IN GT_ZSD_002
   WHERE ADDRNUMBER = GT_ZSD_002-ADRNR.

*银行信息
  SELECT * FROM KNBK
   INNER JOIN BNKA ON
   BNKA~BANKS = KNBK~BANKS
   AND BNKA~BANKL = KNBK~BANKL
   INTO CORRESPONDING FIELDS OF TABLE GT_KNBK
   FOR ALL ENTRIES IN GT_ZSD_002
   WHERE KUNNR = GT_ZSD_002-KUNNR.

*地址信息
  SELECT * FROM ADRC
   INTO CORRESPONDING FIELDS OF TABLE GT_ADRC
   FOR ALL ENTRIES IN GT_ZSD_002
   WHERE ADDRNUMBER = GT_ZSD_002-ADRNR.

*其他
  IF GT_ADRC IS NOT INITIAL .
    SELECT * FROM ADRCT
     INTO CORRESPONDING FIELDS OF TABLE GT_ADRCT
     FOR ALL ENTRIES IN GT_ADRC
     WHERE ADDRNUMBER = GT_ADRC-ADDRNUMBER.
  ENDIF.

*邮件地址信息
  SELECT * FROM ADR6
  INTO CORRESPONDING FIELDS OF TABLE GT_ADR6
  FOR ALL ENTRIES IN GT_ZSD_002
  WHERE ADDRNUMBER = GT_ZSD_002-ADRNR.

*查询账户组描述
  SELECT * FROM T077X
  INTO CORRESPONDING FIELDS OF TABLE GT_T077X
  FOR ALL ENTRIES IN GT_ZSD_002
  WHERE KTOKD = GT_ZSD_002-KTOKD.

*国家描述
  SELECT * FROM T005T
  INTO CORRESPONDING FIELDS OF TABLE  GT_T005T
  FOR ALL ENTRIES IN GT_ZSD_002
  WHERE LAND1 = GT_ZSD_002-LAND1.

*省描述
  SELECT * FROM T005U
  INTO CORRESPONDING FIELDS OF TABLE GT_T005U
  FOR ALL ENTRIES IN GT_ZSD_002
  WHERE LAND1 = GT_ZSD_002-LAND1
  AND   BLAND = GT_ZSD_002-REGIO.

*行政区域描述
  SELECT * FROM T171T
   INTO CORRESPONDING FIELDS OF TABLE GT_T171T
   FOR ALL ENTRIES IN GT_ZSD_002
   WHERE BZIRK = GT_ZSD_002-BZIRK.

*客户来源描述
  SELECT * FROM T151T
    INTO CORRESPONDING FIELDS OF TABLE GT_T151T
    FOR ALL ENTRIES IN GT_ZSD_002
    WHERE KDGRP = GT_ZSD_002-KDGRP.

*客户类型描述
  SELECT * FROM T189T
    INTO CORRESPONDING FIELDS OF TABLE GT_T189T
    FOR ALL ENTRIES IN GT_ZSD_002
    WHERE PLTYP = GT_ZSD_002-PLTYP.

*销售组织描述
  SELECT * FROM TVKOT
    INTO CORRESPONDING FIELDS OF TABLE GT_TVKOT
    FOR ALL ENTRIES IN GT_ZSD_002
    WHERE VKORG = GT_ZSD_002-VKORG.

*付款条款描述
  SELECT * FROM T052U
    INTO CORRESPONDING FIELDS OF TABLE GT_T052U
    FOR ALL ENTRIES IN GT_ZSD_002
    WHERE ZTERM = GT_ZSD_002-ZTERM.

*销售业务人员
  SELECT * FROM KNVP
    INTO CORRESPONDING FIELDS OF TABLE GT_KNVP
    FOR ALL ENTRIES IN GT_ZSD_002
    WHERE KUNNR = GT_ZSD_002-KUNNR
    AND   VKORG = GT_ZSD_002-VKORG
    AND   VTWEG = GT_ZSD_002-VTWEG
    AND   PARVW = 'Z3'.

* 销售人员描述
  IF GT_KNVP IS NOT INITIAL.
    SELECT * FROM KNA1
      INTO CORRESPONDING FIELDS OF TABLE GT_KNA1
      FOR ALL ENTRIES IN GT_KNVP
      WHERE KUNNR = GT_KNVP-KUNN2.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
  LOOP AT GT_ZSD_002 INTO GS_ZSD_002.

    MOVE-CORRESPONDING GS_ZSD_002 TO GS_DATA.


*账户组描述
    READ TABLE GT_T077X INTO GS_T077X
    WITH KEY KTOKD = GS_DATA-KTOKD
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-TEXT30 = GS_T077X-TXT30.
    ENDIF.

*国家描述
    READ TABLE GT_T005T INTO GS_T005T
    WITH KEY LAND1 = GS_DATA-LAND1
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-LANDX = GS_T005T-LANDX.
    ENDIF.

*省描述
    READ TABLE GT_T005U INTO GS_T005U
    WITH KEY LAND1 = GS_DATA-LAND1
             BLAND = GS_DATA-REGIO
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0 .
      GS_DATA-BEZEI = GS_T005U-BEZEI.
    ENDIF.

*行政区域描述
    READ TABLE GT_T171T INTO GS_T171T
    WITH KEY BZIRK = GS_DATA-BZIRK
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-BZTXT = GS_T171T-BZTXT.
    ENDIF.

*地址信息
    READ TABLE GT_ADRC INTO GS_ADRC
    WITH KEY ADDRNUMBER = GS_DATA-ADRNR.
    IF SY-SUBRC = 0.
      GS_DATA-STRAS      = GS_ADRC-STREET.
      GS_DATA-STR_SUPPL3 = GS_ADRC-STR_SUPPL3.
      GS_DATA-KNURL      = GS_ADRC-EXTENSION1.
      GS_DATA-NAME1      = GS_ADRC-NAME1.
    ENDIF.

*其他
    READ TABLE GT_ADRCT INTO GS_ADRCT
    WITH KEY ADDRNUMBER = GS_DATA-ADRNR
             LANGU      = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-REMARK = GS_ADRCT-REMARK.
    ENDIF.

*电子邮件
    READ TABLE GT_ADR6 INTO GS_ADR6
    WITH KEY ADDRNUMBER = GS_DATA-ADRNR.
    IF SY-SUBRC = 0.
      GS_DATA-SMTP_SRCH = GS_ADR6-SMTP_SRCH.
    ENDIF.

*联系人信息
    READ TABLE GT_KNVK INTO GS_KNVK
    WITH KEY KUNNR = GS_DATA-KUNNR.
    IF SY-SUBRC = 0.
      GS_DATA-NAME1_L = GS_KNVK-NAME1.
    ENDIF.

*联系人座机
    READ TABLE GT_ADR2 INTO GS_ADR2
    WITH KEY R3_USER    = 1
             ADDRNUMBER = GS_DATA-ADRNR
             PERSNUMBER = GS_KNVK-PRSNR.
    IF SY-SUBRC = 0.
      GS_DATA-TEL_NUMBER1 = GS_ADR2-TEL_NUMBER.
    ENDIF.

*联系人手机
    READ TABLE GT_ADR2 INTO GS_ADR2
    WITH KEY R3_USER    = 3
           ADDRNUMBER = GS_DATA-ADRNR
           PERSNUMBER = GS_KNVK-PRSNR.
    IF SY-SUBRC = 0.
      GS_DATA-TEL_NUMBER1 = GS_ADR2-TEL_NUMBER.
    ENDIF.

*客户来源描述
    READ TABLE GT_T151T INTO GS_T151T
    WITH KEY KDGRP = GS_DATA-KDGRP.
    IF SY-SUBRC = 0.
      GS_DATA-KTEXT = GS_T151T-KTEXT.
    ENDIF.

*客户类型描述
    READ TABLE GT_T189T INTO GS_T189T
    WITH KEY PLTYP = GS_DATA-PLTYP
            SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-PTEXT = GS_T189T-PTEXT.
    ENDIF.

*银行信息
    READ TABLE GT_KNBK INTO GS_KNBK
    WITH KEY KUNNR = GS_DATA-KUNNR.
    IF SY-SUBRC = 0.
      GS_DATA-BANKS = GS_KNBK-BANKS.
      GS_DATA-BANKL = GS_KNBK-BANKL.
      GS_DATA-BANKA = GS_KNBK-BANKA.
      GS_DATA-BANKN = GS_KNBK-BANKN.
      GS_DATA-BKREF = GS_KNBK-BKREF.
    ENDIF.

*销售组织描述
    READ TABLE GT_TVKOT INTO GS_TVKOT
    WITH KEY VKORG = GS_DATA-VKORG.
    IF SY-SUBRC = 0.
      GS_DATA-VTEXT = GS_TVKOT-VTEXT.
    ENDIF.

*付款条款描述
    READ TABLE GT_T052U INTO GS_T052U
    WITH KEY ZTERM = GS_DATA-ZTERM
             SPRAS = SY-LANGU.
    IF SY-SUBRC = 0.
      GS_DATA-TEXT1 = GS_T052U-TEXT1.
    ENDIF.

*销售业务员
    READ TABLE GT_KNVP INTO GS_KNVP
    WITH KEY KUNNR = GS_DATA-KUNNR
             VKORG = GS_DATA-VKORG
             VTWEG = GS_DATA-VTWEG
             PARVW = 'Z3'.
    IF SY-SUBRC = 0.
      GS_DATA-KUNN2 = GS_KNVP-KUNN2.
    ENDIF.

*销售业务员描述
    READ TABLE GT_KNA1 INTO GS_KNA1
    WITH KEY KUNNR = GS_KNVP-KUNN2.
    IF SY-SUBRC = 0.
      GS_DATA-NAME1_Y = GS_KNA1-NAME1.
    ENDIF.

    APPEND GS_DATA TO GT_DATA .
    CLEAR GS_DATA.
    CLEAR GS_KNVP.
    CLEAR GS_KNA1.
  ENDLOOP.
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
  PERFORM INIT_SORT.               "设置排序、合计
  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.
  PERFORM FRM_EXCLUDE.
  PERFORM FRM_BUILD_EVENT.
  PERFORM FRM_OUTPUT TABLES GT_LVC              "输出
                            GT_SORT
                            GT_DATA
                     USING 'ALV_PF_STATUS'
                           'ALV_USER_COMMAND'
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
  GW_LAYOUT-ZEBRA = 'X'.
  GW_LAYOUT-CWIDTH_OPT  = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SORT .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIANT .

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
  INIT_FIELDCAT 'KUNNR'          '客户'               '' 'X' '' '' 'X' 'KNA1' 'KUNNR'.
  INIT_FIELDCAT 'ERDAT'          '创建日期'           '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'KTOKD'          '账户组'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'          '公司代码'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT30'         '账户组描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANRED'          '标题'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'          '客户名称'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SORTL'          '搜索项'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LAND1'          '国家'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LANDX'          '国家描述'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REGIO'          '省'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEZEI'          '省描述'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ORT01'          '市'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZIRK'          '行政区域'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZTXT'          '行政区域描述'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STRAS'          '注册地址'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STR_SUPPL3'     '办公室地址'          '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELF1'          '座机'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELF2'          '移动电话'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TELFX'          '传真'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KNURL'          '网页'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SMTP_SRCH'      '电子邮箱'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1_L'        '主要联系人'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL_NUMBER1'    '座机'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL_NUMBER2'    '手机'            '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDGRP'          '客户来源'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT'          '客户来源描述'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PLTYP'          '客户类型'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PTEXT'          '客户类型描述'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KLABC'          '客户等级'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKS'          '银行国家'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKL'          '客户开户行'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKA'          '客户开户行描述'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKN'          '银行帐号'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKREF'          '参考细节'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STCEG'          '纳税人识别号'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REMARK'         '其他'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AKONT'          '统驭科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VKORG'          '销售组织'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VTEXT'          '销售组织描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VTWEG'          '分销渠道'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SPART'          '产品组'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INCO1'          '国际贸易条款1'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INCO2'          '国际贸易条款2'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM'          '付款条款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT1'          '销售付款条款描述'  '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'          '货币'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNN2'          '销售业务人员'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1_Y'        '业务员姓名'       '' '' '' '' '' '' ''.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .

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
  REFRESH GT_EXCLUDE.
  CLEAR GS_EXCLUDE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_IT_DATA  text
*      -->P_0443   text
*      -->P_0444   text
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
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = PU_STATUS
      I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      I_GRID_SETTINGS          = PW_GRID_SETTINGS
      IS_LAYOUT_LVC            = PW_LAYOUT
      IT_FIELDCAT_LVC          = PT_LVC[]
      IT_EXCLUDING             = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
      IT_SORT_LVC              = PT_SORT[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
      IS_VARIANT               = PW_VARIANT
      IT_EVENTS                = GT_EVENTS[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = PT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.

* 双击
    WHEN '&IC1'.
      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
      CHECK SY-SUBRC = 0.
      IF RS_SELFIELD-FIELDNAME = 'KUNNR'
        AND GS_DATA-KUNNR IS NOT INITIAL.
        SET PARAMETER ID 'KUN' FIELD GS_DATA-KUNNR.
        SET PARAMETER ID 'BUK' FIELD GS_DATA-BUKRS.
        SET PARAMETER ID 'VKO' FIELD GS_DATA-VKORG.
        SET PARAMETER ID 'VTW' FIELD GS_DATA-VTWEG.
        SET PARAMETER ID 'SPA' FIELD GS_DATA-SPART.
        CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND
