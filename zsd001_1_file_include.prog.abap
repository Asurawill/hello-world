*&---------------------------------------------------------------------*
*&  包含                ZSD001_FILE_INCLUDE
*&---------------------------------------------------------------------*
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

ENDFORM.                    "frm_get_fn
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DO .
  CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
*   EXPORTING
*     DEF_FILENAME           = ' '
*     DEF_PATH               = ' '
*     MASK                   = ' '
*     MODE                   = ' '
*     TITLE                  = ' '
    IMPORTING
      FILENAME         = P_DO
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
*&      Form  frm_chech_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_CHECH_FILENAME .
  IF P_FN IS INITIAL.
*    MESSAGE i010."主数据文件，路径和文件名，不能为空！
    MESSAGE '主数据文件，路径和文件名，不能为空！' TYPE 'E'.
    STOP.
  ENDIF.
ENDFORM.                    "frm_chech_filename
*&---------------------------------------------------------------------*
*&      Form  frm_upload_data
*&---------------------------------------------------------------------*
*       text
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
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  IF FU_DATA[] IS INITIAL.
    MESSAGE '文件为空！' TYPE 'I'.
    STOP.
  ENDIF.

ENDFORM.                    "frm_upload_data
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_DATA .
* 主数据
  DATA:LT_DATA TYPE STANDARD TABLE OF TY_KNVP,
       LW_DATA TYPE TY_KNVP.
  DATA:L_KUNNR1 TYPE KNVP-KUNNR.
  DATA:L_KUNNR TYPE KNA1-KUNNR.
  DATA:LT_KNA1 TYPE STANDARD TABLE OF KNA1.
  DATA:LW_KNA1 TYPE KNA1.
  DATA LT_NRIV TYPE TABLE OF NRIV.
  DATA LS_NRIV TYPE NRIV.
  DATA LS_TNRO TYPE TNRO.
*&--代码注释 BY HANDYBY 25.06.2017 11:56:24  BEGIN
*    DATA L_TABIX TYPE SY-TABIX.
*&--代码注释 BY HANDYBY 25.06.2017 11:56:24  END
*&--代码添加 BY HANDYBY 25.06.2017 11:56:41  BEGIN
  DATA L_TABIX TYPE I VALUE IS INITIAL .
*&--代码添加 BY HANDYBY 25.06.2017 11:56:41  END


  REFRESH LT_CUST.

  REFRESH:LT_NRIV.
  CLEAR:LS_NRIV,
        LS_TNRO.

*根据对象号取出流水码号
  SELECT * FROM NRIV
    INTO CORRESPONDING FIELDS OF TABLE LT_NRIV
    WHERE OBJECT    = 'DEBITOR'.

**取出缓存数量
*  SELECT  SINGLE * FROM TNRO
*     INTO CORRESPONDING FIELDS OF  LS_TNRO
*    WHERE OBJECT = 'DEBITOR'.

*取出账户组对应的编号
  SELECT * FROM T077D
  INNER JOIN T077X ON
  T077D~KTOKD = T077X~KTOKD
  INTO CORRESPONDING FIELDS OF TABLE GT_V_077D_B.

  CLEAR SY-TABIX.
  REFRESH:T_BASIS,
          T_COM,
          T_ORG.
  CLEAR L_TABIX.

*根据内部流水给客户编号
*&--代码注释 BY HANDYBY 25.06.2017 11:52:13  BEGIN
*    LOOP AT T_DATA INTO W_DATA.
*    L_TABIX = SY-TABIX.
*
*    READ TABLE GT_V_077D_B INTO GS_V_077D_B
*    WITH KEY KTOKD = W_DATA-KTOKD.
*    IF SY-SUBRC = 0.
*      READ TABLE LT_NRIV INTO LS_NRIV
*      WITH KEY NRRANGENR = GS_V_077D_B-NUMKR.
*
**当状态0取当前值，当状态为0时候，取起始值
*      IF LS_NRIV-NRLEVEL <> 0.
*        W_DATA-KUNNR = LS_NRIV-NRLEVEL + L_TABIX .
*      ELSE.
*        W_DATA-KUNNR = LS_NRIV-FROMNUMBER + L_TABIX - 1.
*      ENDIF.
*    ENDIF.
*
*    MOVE-CORRESPONDING W_DATA TO W_BASIS.
*    MOVE-CORRESPONDING W_DATA TO W_COM.
*    MOVE-CORRESPONDING W_DATA TO W_ORG.
*    APPEND W_BASIS TO T_BASIS.
*    APPEND W_COM TO T_COM.
*    APPEND W_ORG TO T_ORG.
*    MODIFY T_DATA FROM W_DATA.
*    CLEAR:W_COM,W_ORG,W_BASIS,W_DATA.
*  ENDLOOP.
*&--代码注释 BY HANDYBY 25.06.2017 11:52:13  END
*&--代码添加 BY HANDYBY 25.06.2017 11:52:37  BEGIN
  LOOP AT GT_OUTPUT INTO GS_OUTPUT.
* 判断客户是否已存在,不存在给编号，存在跳过给编号环节
    IF GS_OUTPUT-KUNNR_EXIST = 'X'.
*      CONTINUE .
    ELSE .
      L_TABIX = L_TABIX + 1 .

      READ TABLE GT_V_077D_B INTO GS_V_077D_B
            WITH KEY KTOKD = GS_OUTPUT-KTOKD.
      IF SY-SUBRC = 0.
        READ TABLE LT_NRIV INTO LS_NRIV
        WITH KEY NRRANGENR = GS_V_077D_B-NUMKR.

*当状态0取当前值，当状态为0时候，取起始值
        IF LS_NRIV-NRLEVEL <> 0.
          GS_OUTPUT-KUNNR = LS_NRIV-NRLEVEL + L_TABIX .
        ELSE.
          GS_OUTPUT-KUNNR = LS_NRIV-FROMNUMBER + L_TABIX - 1.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING GS_OUTPUT TO W_BASIS.
    MOVE-CORRESPONDING GS_OUTPUT TO W_COM.
    MOVE-CORRESPONDING GS_OUTPUT TO W_ORG.
    APPEND W_BASIS TO T_BASIS.
    APPEND W_COM TO T_COM.
    APPEND W_ORG TO T_ORG.
    MODIFY GT_OUTPUT FROM GS_OUTPUT.
    CLEAR:W_COM,W_ORG,W_BASIS,GS_OUTPUT.
  ENDLOOP.
*&--代码添加 BY HANDYBY 25.06.2017 11:52:37  END

*
  SORT T_BASIS   BY KUNNR.
  SORT T_ORG     BY KUNNR VKORG VTWEG SPART.
  SORT T_COM     BY KUNNR BUKRS.

*&--代码添加 BY HANDYBY 25.06.2017 16:46:07  BEGIN
* 找到客户对应的联系人
  SELECT PARNR
         KUNNR
         NAME1
*         TELF1
    INTO CORRESPONDING FIELDS OF TABLE GT_KNVK
    FROM KNVK
     FOR ALL ENTRIES IN T_BASIS
   WHERE KUNNR = T_BASIS-KUNNR .
  SORT GT_KNVK BY KUNNR NAME1 .
*&--代码添加 BY HANDYBY 25.06.2017 16:46:07  END

  "客户+销售组织信息
  LOOP AT T_BASIS INTO W_BASIS.
    REFRESH T_TPAER.
    CLEAR W_TKUPA.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF  W_TKUPA
      FROM TKUPA
      WHERE KTOKD = W_BASIS-KTOKD.

*获取默认合作伙伴
    SELECT *
   FROM TPAER
   INTO CORRESPONDING FIELDS OF TABLE T_TPAER
   WHERE PARGR = W_TKUPA-PARGR
   AND   PAPFL = 'X'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = W_BASIS-KUNNR
      IMPORTING
        OUTPUT = LWA_CUST-HEADER-OBJECT_INSTANCE-KUNNR.

    LWA_CUST-CENTRAL_DATA-CENTRAL-DATA-KTOKD = W_BASIS-KTOKD."账户组
    LWA_CUST-CENTRAL_DATA-CENTRAL-DATA-STCEG = W_BASIS-STCEG."纳税人识别号
*&--代码添加 BY HANDYBY 25.06.2017 12:20:45  BEGIN
    IF W_BASIS-KUNNR_EXIST = 'X' .
      LWA_CUST-HEADER-OBJECT_TASK = 'U'.                       "插入
    ELSE.
      LWA_CUST-HEADER-OBJECT_TASK = 'I'.                       "插入
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 12:20:45  END
*&--代码注释 BY HANDYBY 25.06.2017 16:04:07  BEGIN
*    LWA_CUST-HEADER-OBJECT_TASK = 'I'.                       "插入
*&--代码注释 BY HANDYBY 25.06.2017 16:04:07  END

* 地址数据

    IF W_BASIS-KUNNR_EXIST = 'X' .
      LWA_CUST-CENTRAL_DATA-ADDRESS-TASK = 'U'.                          "插入
    ELSE.
      LWA_CUST-CENTRAL_DATA-ADDRESS-TASK = 'I'.                          "插入
    ENDIF.
*&--代码注释 BY HANDYBY 25.06.2017 16:03:46  BEGIN
*    LWA_CUST-CENTRAL_DATA-ADDRESS-TASK = CON_INSERT.                          "插入
*&--代码注释 BY HANDYBY 25.06.2017 16:03:46  END
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-NAME       = W_BASIS-NAME1.     "客户名称
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-TITLE      = W_BASIS-ANRED.     "称谓
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-SORT1      = W_BASIS-SORTL.     "搜索项
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-COUNTRY    = W_BASIS-LAND1.     "国家
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-REGION     = W_BASIS-REGIO.     "省
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-CITY       = W_BASIS-CITY1.     "市
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-STREET     = W_BASIS-STRAS.     "注册地址

*&--代码添加 BY HANDYBY 22.05.2017 09:10:04  BEGIN
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-STR_SUPPL3 = W_BASIS-STR_SUPPL3."办公地址
*&--代码添加 BY HANDYBY 22.05.2017 09:10:04  END

    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-POSTL_COD1 = W_BASIS-POST_CODE1."邮政编码
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-EXTENS_1   = W_BASIS-KNURL."网页
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATA-LANGU      = '1'.

    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-C_O_NAME   = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-NAME       = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-SORT1      = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-STREET     = 'X'.

*&--代码添加 BY HANDYBY 22.05.2017 09:08:51  BEGIN
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-STR_SUPPL3 = 'X'.
*&--代码添加 BY HANDYBY 22.05.2017 09:08:51  END

    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-POSTL_COD1 = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-CITY       = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-COUNTRY    = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-REGION     = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-LANGU      = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-TITLE      = 'X'.
    LWA_CUST-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-EXTENS_1   = 'X'.

*银行数据
    IF W_BASIS-BANKS IS NOT INITIAL."银行国家
      SELECT SINGLE
        KNBK~KUNNR
        KNBK~BANKS
        KNBK~BANKL
        KNBK~BANKN
      FROM KNBK
      INTO W_BANK
      WHERE KNBK~KUNNR  = W_BASIS-KUNNR
      AND    KNBK~BANKS = W_BASIS-BANKS
      AND    KNBK~BANKL = W_BASIS-BANKL
      AND    KNBK~BANKN = W_BASIS-BANKN.

      IF W_BANK IS INITIAL.
        LWA_BANK-TASK = 'I'.
      ELSE.
        LWA_BANK-TASK = 'U'.
      ENDIF.

      LWA_BANK-DATA_KEY-BANKS = W_BASIS-BANKS."银行国家
      LWA_BANK-DATA_KEY-BANKL = W_BASIS-BANKL."开户行
      LWA_BANK-DATA_KEY-BANKN = W_BASIS-BANKN."银行帐号
      LWA_BANK-DATA-BKREF     = W_BASIS-BKREF."参考细节

      LWA_BANK-DATAX-KOINH = 'X'.
      APPEND LWA_BANK TO LT_BANK.
      LWA_CUST-CENTRAL_DATA-BANKDETAIL-BANKDETAILS = LT_BANK.
      CLEAR: LWA_BANK,
      LT_BANK.
    ENDIF.

*联系人信息
    REFRESH LT_CONTACT.
*&--代码注释 BY HANDYBY 25.06.2017 16:19:48  BEGIN
*    PERFORM FRM_FILL_CONTACT USING W_BASIS-NAME1_L W_BASIS-TEL_NUMBER W_BASIS-TEL_M.
*&--代码注释 BY HANDYBY 25.06.2017 16:19:48  END
*&--代码添加 BY HANDYBY 25.06.2017 16:19:54  BEGIN
    PERFORM FRM_FILL_CONTACT USING W_BASIS  .
*&--代码添加 BY HANDYBY 25.06.2017 16:19:54  END

    LWA_CUST-CENTRAL_DATA-CONTACT-CONTACTS = LT_CONTACT.

*地址注释
    CLEAR LWA_REMARK.
    REFRESH LT_REMARK.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  BEGIN
    IF W_BASIS-KUNNR_EXIST = ''.
      LWA_REMARK-TASK = 'I'.
    ELSE.
      LWA_REMARK-TASK = 'U'.
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  END
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  BEGIN
*    LWA_REMARK-TASK = CON_INSERT
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  END
    LWA_REMARK-DATA-LANGU = SY-LANGU.
    LWA_REMARK-DATA-ADR_NOTES = W_BASIS-REMARK.
    LWA_REMARK-DATAX-LANGU = 'X'.
    LWA_REMARK-DATAX-ADR_NOTES = 'X'.
    APPEND LWA_REMARK TO LT_REMARK.
    LWA_CUST-CENTRAL_DATA-ADDRESS-REMARK-REMARKS = LT_REMARK.

*电话
    CLEAR:LWA_PHONE.
    REFRESH LT_PHONE.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  BEGIN
    IF W_BASIS-KUNNR_EXIST = ''.
      LWA_PHONE-CONTACT-TASK = 'I'.
    ELSE.
      LWA_PHONE-CONTACT-TASK = 'U'.
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  END
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  BEGIN
*    LWA_PHONE-CONTACT-TASK = CON_INSERT.
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  END
    LWA_PHONE-CONTACT-DATA-TELEPHONE = W_BASIS-TEL1.
    LWA_PHONE-CONTACT-DATA-R_3_USER  = '1'.
    LWA_PHONE-CONTACT-DATAX-TELEPHONE = 'X'.
    LWA_PHONE-CONTACT-DATAX-R_3_USER  = 'X'.
    APPEND LWA_PHONE TO LT_PHONE.

*移动电话
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  BEGIN
    CLEAR LWA_PHONE .
    IF W_BASIS-KUNNR_EXIST = ''.
      LWA_PHONE-CONTACT-TASK = 'I'.
    ELSE.
      LWA_PHONE-CONTACT-TASK = 'U'.
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  END
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  BEGIN
*    LWA_PHONE-CONTACT-TASK = CON_INSERT.
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  END
    LWA_PHONE-CONTACT-DATA-TELEPHONE = W_BASIS-TEL2.
    LWA_PHONE-CONTACT-DATA-R_3_USER  = '2'.
    LWA_PHONE-CONTACT-DATAX-TELEPHONE = 'X'.
    LWA_PHONE-CONTACT-DATAX-R_3_USER  = 'X'.
    APPEND LWA_PHONE TO LT_PHONE.
    LWA_CUST-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE = LT_PHONE.

*传真
    CLEAR:LWA_FAX.
    REFRESH LT_FAX.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  BEGIN
    IF W_BASIS-KUNNR_EXIST = ''.
      LWA_FAX-CONTACT-TASK = 'I'.
    ELSE.
      LWA_FAX-CONTACT-TASK = 'U'.
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  END
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  BEGIN
*    LWA_FAX-CONTACT-TASK = CON_INSERT.
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  END
    LWA_FAX-CONTACT-DATA-FAX = W_BASIS-FAX.
    LWA_FAX-CONTACT-DATAX-FAX = 'X'.
    APPEND LWA_FAX TO LT_FAX.
    LWA_CUST-CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX = LT_FAX.

*电子邮件
    CLEAR:LWA_MAIL.
    REFRESH LT_MAIL.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  BEGIN
    IF W_BASIS-KUNNR_EXIST = ''.
      LWA_MAIL-CONTACT-TASK = 'I'.
    ELSE.
      LWA_MAIL-CONTACT-TASK = 'U'.
    ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:20:29  END
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  BEGIN
*    LWA_MAIL-CONTACT-TASK = CON_INSERT.
*&--代码注释 BY HANDYBY 25.06.2017 17:21:07  END
    LWA_MAIL-CONTACT-DATA-E_MAIL = W_BASIS-SMTP_ADDR.
    LWA_MAIL-CONTACT-DATAX-E_MAIL = 'X'.
    APPEND LWA_MAIL TO LT_MAIL.
    LWA_CUST-CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP = LT_MAIL.

* 公司代码数据
    REFRESH LT_COMPANY.
    LOOP AT T_COM INTO W_COM WHERE KUNNR = W_BASIS-KUNNR.
      CLEAR LWA_COMPANY.

*当公司代码数据为0,CONTINUE.
      IF W_COM-BUKRS IS INITIAL.
        CONTINUE.
      ENDIF.

*&--代码添加 BY HANDYBY 25.06.2017 17:24:32  BEGIN
      IF W_COM-BUKRS_EXIST = '' .
        LWA_COMPANY-TASK = 'I'.
      ELSE.
        LWA_COMPANY-TASK = 'U'.
      ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:24:32  END
*&--代码注释 BY HANDYBY 25.06.2017 17:25:00  BEGIN
*    LWA_COMPANY-TASK = 'I'.
*&--代码注释 BY HANDYBY 25.06.2017 17:25:00  END
      LWA_COMPANY-DATA_KEY-BUKRS = W_COM-BUKRS. "公司代码
      LWA_COMPANY-DATA-AKONT     = W_COM-AKONT. "统驭科目
      LWA_COMPANY-DATA-ZTERM     = W_COM-ZTERM. "付款条款

      LWA_COMPANY-DATAX-AKONT = 'X'.
      LWA_COMPANY-DATAX-ZTERM = 'X'.
      APPEND LWA_COMPANY TO LT_COMPANY.
      CLEAR:W_COM.
    ENDLOOP.
    LWA_CUST-COMPANY_DATA-COMPANY = LT_COMPANY.

*销售组织数据
    REFRESH LT_SALE.
    LOOP AT T_ORG INTO W_ORG WHERE KUNNR = W_BASIS-KUNNR.
      CLEAR LWA_SALE.

*当无销售组织，continue.
      IF  W_ORG-VKORG IS INITIAL
       OR  W_ORG-VTWEG IS INITIAL
       OR  W_ORG-SPART IS INITIAL.
        CONTINUE.
      ENDIF.

*&--代码添加 BY HANDYBY 25.06.2017 17:25:20  BEGIN
      IF W_ORG-VKORG_EXIST = '' .
        LWA_SALE-TASK = 'I'.
      ELSE.
        LWA_SALE-TASK = 'U'.
      ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:25:20  END
*&--代码注释 BY HANDYBY 25.06.2017 17:25:45  BEGIN
*    LWA_SALE-TASK = 'I'.
*&--代码注释 BY HANDYBY 25.06.2017 17:25:45  END
      LWA_SALE-DATA_KEY-VKORG = W_ORG-VKORG."销售组织
      LWA_SALE-DATA_KEY-VTWEG = W_ORG-VTWEG."分销渠道
      LWA_SALE-DATA_KEY-SPART = W_ORG-SPART."产品组
      LWA_SALE-DATA-WAERS     = W_ORG-WAERS."货币
      LWA_SALE-DATA-VSBED     = W_ORG-VSBED."装运条件
      LWA_SALE-DATA-INCO1     = W_ORG-INCO1."国际贸易条款1
      LWA_SALE-DATA-INCO2     = W_ORG-INCO2."国际贸易条款2
      LWA_SALE-DATA-ZTERM     = W_ORG-ZTERM."付款条款
      LWA_SALE-DATA-KTGRD     = W_ORG-KTGRD."账户分配组
      LWA_SALE-DATA-KALKS     = W_ORG-KALKS."客户定价过程
      LWA_SALE-DATA-BZIRK     = W_ORG-BZIRK."行政区域
      LWA_SALE-DATA-KLABC     = W_ORG-KLABC."客户等级
      LWA_SALE-DATA-PLTYP     = W_ORG-PLTYP."客户类型
      LWA_SALE-DATA-KDGRP     = W_ORG-KDGRP."客户来源

      IF W_ORG-KALKS IS NOT INITIAL.
        LWA_SALE-DATAX-KALKS = 'X'.   "客户定价过程
      ENDIF.
      IF W_ORG-ZTERM IS NOT INITIAL.  "付款条款
        LWA_SALE-DATAX-ZTERM = 'X'.
      ENDIF.
      IF W_ORG-WAERS IS NOT INITIAL.  "货币
        LWA_SALE-DATAX-WAERS = 'X'.
      ENDIF.
      IF W_ORG-INCO1 IS NOT INITIAL.  "国际贸易条款1
        LWA_SALE-DATAX-INCO1 = 'X'.
      ENDIF.
      IF W_ORG-INCO2 IS NOT INITIAL.  "国际贸易条款2
        LWA_SALE-DATAX-INCO2 = 'X'.
      ENDIF.
      IF W_ORG-BZIRK IS NOT INITIAL.  "行政区域
        LWA_SALE-DATAX-BZIRK = 'X'.
      ENDIF.
      IF W_ORG-VSBED IS NOT INITIAL.
        LWA_SALE-DATAX-VSBED = 'X'.   "装运条件
      ENDIF.
      IF W_ORG-KTGRD IS NOT INITIAL.
        LWA_SALE-DATAX-KTGRD = 'X'.   "账户分配
      ENDIF.
      IF W_ORG-KLABC IS NOT INITIAL.
        LWA_SALE-DATAX-KLABC = 'X'.   "客户等级
      ENDIF.
      IF W_ORG-PLTYP IS NOT INITIAL.
        LWA_SALE-DATAX-PLTYP = 'X'.   "客户类型
      ENDIF.
      IF W_ORG-KDGRP IS NOT INITIAL.
        LWA_SALE-DATAX-KDGRP = 'X'.   "客户来源
      ENDIF.

      REFRESH LT_FUNCTIONS.
      CLEAR W_KNVP.
      MOVE-CORRESPONDING W_ORG TO W_KNVP.

*合作伙伴
      LOOP AT T_TPAER INTO W_TPAER.
        SELECT SINGLE KUNNR
          FROM KNVP
          INTO L_KUNNR1
          WHERE KUNNR = W_BASIS-KUNNR
          AND VKORG   = W_ORG-VKORG
          AND VTWEG   = W_ORG-VTWEG
          AND SPART   = W_ORG-SPART.
        IF SY-SUBRC NE 0.
          PERFORM FRM_FILL_PARTNER USING W_TPAER-PARVW LWA_CUST-HEADER-OBJECT_INSTANCE-KUNNR W_KNVP.
        ENDIF.
        CLEAR W_TPAER.
      ENDLOOP.

*销售人员
      IF W_ORG-KUNN2 IS NOT INITIAL.
*&--代码添加 BY HANDYBY 25.06.2017 17:36:11  BEGIN
        IF W_ORG-VKORG_EXIST = ''.
          LWA_FUNCTIONS-TASK           = 'I'.
        ELSE.
          LWA_FUNCTIONS-TASK           = 'U'.
        ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:36:11  END
*&--代码注释 BY HANDYBY 25.06.2017 17:36:39  BEGIN
*        LWA_FUNCTIONS-TASK           = 'I'.
*&--代码注释 BY HANDYBY 25.06.2017 17:36:39  END

        LWA_FUNCTIONS-DATA_KEY-PARVW = 'Z3'.

*添加前置零
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = W_ORG-KUNN2
          IMPORTING
            OUTPUT = W_ORG-KUNN2.

        LWA_FUNCTIONS-DATA-PARTNER   = W_ORG-KUNN2.

        LWA_FUNCTIONS-DATAX-PARTNER  = 'X'.
        APPEND LWA_FUNCTIONS TO LT_FUNCTIONS.
      ENDIF.

*税
      CLEAR LWA_TAX.
      REFRESH LT_TAX.
*&--代码添加 BY HANDYBY 25.06.2017 17:38:02  BEGIN
      IF W_BASIS-KUNNR_EXIST = ''.
        LWA_TAX-TASK           = 'I'.
        LWA_TAX-DATA_KEY-ALAND = 'CN'.
        LWA_TAX-DATA_KEY-TATYP = 'MWST' .
        LWA_TAX-DATA-TAXKD     =  W_ORG-TAXKD."税分类
        LWA_TAX-DATAX-TAXKD    = 'X'.
        APPEND LWA_TAX TO LT_TAX.
*      ELSE.
*        LWA_TAX-TASK           = 'U'.
      ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:38:02  END
*&--代码注释 BY HANDYBY 25.06.2017 17:38:29  BEGIN
*    LWA_TAX-TASK           = 'I'.
*&--代码注释 BY HANDYBY 25.06.2017 17:38:29  END


      LWA_SALE-FUNCTIONS-FUNCTIONS = LT_FUNCTIONS.

      APPEND LWA_SALE TO LT_SALE.
      CLEAR LWA_SALE.
      CLEAR:W_ORG.
    ENDLOOP.

    LWA_CUST-CENTRAL_DATA-TAX_IND-TAX_IND = LT_TAX.
    LWA_CUST-SALES_DATA-SALES = LT_SALE.

    REFRESH LT_SALE.
    APPEND LWA_CUST TO LT_CUST.
    MOVE-CORRESPONDING W_BASIS TO LW_DATA.
    APPEND LW_DATA TO LT_DATA.
    CLEAR LW_DATA.
    CLEAR LWA_CUST.
  ENDLOOP.

  LS_MAIN-CUSTOMERS = LT_CUST.

*创建数据到SAP中
  PERFORM FRM_WRITE_DATA_SAP TABLES LT_DATA
                           .
  FREE LS_MAIN.
  REFRESH LT_CUST.

ENDFORM.                    " FRM_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_WRITE_DATA_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_WRITE_DATA_SAP TABLES FU_DATA TYPE STANDARD TABLE
                         .
  DATA:L_ILINES TYPE I .
  DATA:L_MESSAGE TYPE STRING.
  DATA:L_IINDEX TYPE I.
  DATA:LW_DATA TYPE TY_KNVP.
  DATA:L_SUCCESS TYPE C.
*  FREE:cmd_ei_api=>gt_global_kna1_old.

  FREE:LS_MAIN1,LS_MAIN2,LS_MESG,LS_MESG1.

  CALL METHOD CMD_EI_API=>MAINTAIN_BAPI
    EXPORTING
      IV_TEST_RUN              = G_TEST
      IV_COLLECT_MESSAGES      = 'X'
      IS_MASTER_DATA           = LS_MAIN
    IMPORTING
      ES_MASTER_DATA_CORRECT   = LS_MAIN1
      ES_MESSAGE_CORRECT       = LS_MESG1
      ES_MASTER_DATA_DEFECTIVE = LS_MAIN2
      ES_MESSAGE_DEFECTIVE     = LS_MESG.

  REFRESH LT_MSG.
  CLEAR LV_FLG.

  LT_MSG = LS_MESG-MESSAGES.
  L_IINDEX = 0.

*&--代码添加 BY HANDYBY 29.06.2017 17:39:47  BEGIN
  SORT GT_OUTPUT BY KUNNR .
*&--代码添加 BY HANDYBY 29.06.2017 17:39:47  END

  LOOP AT GT_OUTPUT INTO GS_OUTPUT.
    L_IINDEX = L_IINDEX + 1.

*没有任何改行的信息，表明成功
    READ TABLE LT_MSG INTO LWA_MSG WITH KEY ROW = L_IINDEX.
    IF SY-SUBRC NE 0.

*读取返回数据中的客户号
      READ TABLE LT_CUST INTO LWA_CUST INDEX L_IINDEX.
      IF SY-SUBRC = 0.
        GS_OUTPUT-KUNNR = LWA_CUST-HEADER-OBJECT_INSTANCE-KUNNR.
      ENDIF.

*测试通过绿灯，正式运行OK标识
      IF G_TEST = ''.
        GS_OUTPUT-STATU = ICON_OKAY.
      ELSE.
        GS_OUTPUT-STATU = ICON_GREEN_LIGHT.
      ENDIF.

*&--代码注释 BY HANDYBY 25.06.2017 17:53:12  BEGIN
*      CONCATENATE '客户'  GS_OUTPUT-KUNNR '创建/修改成功' INTO L_MESSAGE.
*&--代码注释 BY HANDYBY 25.06.2017 17:53:12  END
*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  BEGIN
      IF GS_OUTPUT-KUNNR_EXIST = ''.
        CONCATENATE '客户'  GS_OUTPUT-KUNNR '创建成功' INTO L_MESSAGE.
      ELSE.
        CONCATENATE '客户'  GS_OUTPUT-KUNNR '修改成功' INTO L_MESSAGE.
      ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  END

      GS_OUTPUT-MESSAGE = L_MESSAGE.
      MODIFY GT_OUTPUT FROM GS_OUTPUT.
*      CLEAR GS_OUTPUT.

*当正式运行的时候，号码段流水
      IF G_TEST = ''.
        PERFORM FRM_GET_NEXT CHANGING G_KUNNR GS_OUTPUT-KTOKD .
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      WAIT UP TO '0.5' SECONDS.

      CLEAR GS_OUTPUT.

*记录错误信息
    ELSE.
      GS_OUTPUT-STATU = ICON_RED_LIGHT.
      CLEAR LV_FLG.

      LOOP AT  LT_MSG INTO LWA_MSG
      WHERE ROW = L_IINDEX.
        CLEAR L_MESSAGE.
        IF LWA_MSG-TYPE = 'E' OR LWA_MSG-TYPE = 'A'.
          GS_OUTPUT-MESSAGE = LWA_MSG-MESSAGE.
          LV_FLG = '1'.
          EXIT.
        ENDIF.

*提示对应的警告消息
        IF LWA_MSG-TYPE = 'W'.
          GS_OUTPUT-MESSAGE = LWA_MSG-MESSAGE.
          LV_FLG = '2'.
          CONTINUE.
        ENDIF.
        CLEAR:LWA_MSG.
      ENDLOOP.

      MODIFY GT_OUTPUT FROM GS_OUTPUT.

*当不包含错误的时候和警告
      IF LV_FLG <> '1' AND LV_FLG <> '2'.
        CLEAR L_MESSAGE.

*测试通过绿灯，正式运行OK标识
        IF G_TEST = ''.
          GS_OUTPUT-STATU = ICON_OKAY.
        ELSE.
          GS_OUTPUT-STATU = ICON_GREEN_LIGHT.
        ENDIF.

*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  BEGIN
        IF GS_OUTPUT-KUNNR_EXIST = ''.
          CONCATENATE '客户'  GS_OUTPUT-KUNNR '创建成功' INTO L_MESSAGE.
        ELSE.
          CONCATENATE '客户'  GS_OUTPUT-KUNNR '修改成功' INTO L_MESSAGE.
        ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  END
*&--代码注释 BY HANDYBY 25.06.2017 17:56:15  BEGIN
*    CONCATENATE '客户'  GS_OUTPUT-KUNNR '创建/修改成功' INTO L_MESSAGE.
*&--代码注释 BY HANDYBY 25.06.2017 17:56:15  END

        GS_OUTPUT-MESSAGE = L_MESSAGE.
        MODIFY GT_OUTPUT FROM GS_OUTPUT.
*        CLEAR GS_OUTPUT.

*当正式运行的时候，号码段流水
        IF G_TEST = ''.
          PERFORM FRM_GET_NEXT CHANGING G_KUNNR GS_OUTPUT-KTOKD .
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        WAIT UP TO '0.5' SECONDS.

        CLEAR GS_OUTPUT.


*当存在警告时候
      ELSEIF LV_FLG = '2'.
        GS_OUTPUT-STATU = ICON_YELLOW_LIGHT.

*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  BEGIN
        IF GS_OUTPUT-KUNNR_EXIST = ''.
          CONCATENATE '客户' GS_OUTPUT-KUNNR '创建成功,但存在警告' L_MESSAGE INTO L_MESSAGE.
        ELSE.
          CONCATENATE '客户' GS_OUTPUT-KUNNR '修改成功,但存在警告' L_MESSAGE INTO L_MESSAGE.
        ENDIF.
*&--代码添加 BY HANDYBY 25.06.2017 17:54:19  END
*&--代码注释 BY HANDYBY 25.06.2017 17:57:51  BEGIN
*    CONCATENATE '客户' GS_OUTPUT-KUNNR '创建成功,但存在警告' L_MESSAGE INTO L_MESSAGE.
*&--代码注释 BY HANDYBY 25.06.2017 17:57:51  END

        GS_OUTPUT-MESSAGE = L_MESSAGE.
        MODIFY GT_OUTPUT FROM GS_OUTPUT.
*        CLEAR GS_OUTPUT.

*当正式运行的时候，号码段流水
        IF G_TEST = ''.
          PERFORM FRM_GET_NEXT CHANGING G_KUNNR GS_OUTPUT-KTOKD .
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        WAIT UP TO '0.5' SECONDS.

        CLEAR GS_OUTPUT.

      ELSE.
*当存在错误的时候
        ROLLBACK WORK.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " FRM_WRITE_DATA_SAP
*&---------------------------------------------------------------------*
*&      Form  frm_fill_partner
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FU_PARVW   text
*----------------------------------------------------------------------*
FORM FRM_FILL_PARTNER USING FU_PARVW FU_KUNNR FU_DATA TYPE TY_KNVP.

  DATA:L_PARZA TYPE KNVP-PARZA.
  CLEAR L_PARZA.
  CLEAR LWA_FUNCTIONS.
  LWA_FUNCTIONS-TASK           = 'I'.
  LWA_FUNCTIONS-DATA_KEY-PARVW = FU_PARVW.
  LWA_FUNCTIONS-DATA-PARTNER   = FU_KUNNR.
  LWA_FUNCTIONS-DATAX-PARTNER  = 'X'.
  APPEND LWA_FUNCTIONS TO LT_FUNCTIONS.


ENDFORM.                    "frm_fill_partner
*&---------------------------------------------------------------------*
*&      Form  FRM_INSERT_PARTNER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INSERT_PARTNER .
  CALL METHOD CMD_EI_API=>MAINTAIN_BAPI
    EXPORTING
      IV_TEST_RUN              = G_TEST
      IV_COLLECT_MESSAGES      = 'X'
      IS_MASTER_DATA           = LS_MAIN
    IMPORTING
      ES_MASTER_DATA_CORRECT   = LS_MAIN1
      ES_MESSAGE_CORRECT       = LS_MESG1
      ES_MASTER_DATA_DEFECTIVE = LS_MAIN2
      ES_MESSAGE_DEFECTIVE     = LS_MESG.
  LT_MSG = LS_MESG-MESSAGES.

  LOOP AT LT_MSG INTO LWA_MSG.
    WRITE: / LWA_MSG-TYPE,LWA_MSG-MESSAGE.
    IF LWA_MSG-TYPE = 'E' OR LWA_MSG-TYPE = 'A'.
      LV_FLG = 'X'.
    ENDIF.
  ENDLOOP.

  IF LV_FLG IS INITIAL.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " FRM_INSERT_PARTNER
*&---------------------------------------------------------------------*
*&      Form  frm_fill_partner01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FU_PARVW   text
*      -->FU_KUNNR   text
*      -->FU_DATA    text
*----------------------------------------------------------------------*
FORM FRM_FILL_PARTNER01 USING FU_PARVW FU_KUNNR FU_DATA TYPE TY_KNVP.
  IF FU_KUNNR IS NOT INITIAL.
    DATA:L_PARZA TYPE KNVP-PARZA.
    CLEAR L_PARZA.
    CLEAR LWA_FUNCTIONS.
    LWA_FUNCTIONS-TASK = 'I'.
    LWA_FUNCTIONS-DATA_KEY-PARVW = 'I'.
    LWA_FUNCTIONS-DATA_KEY-PARVW = FU_PARVW.


    IF NOT FU_KUNNR IS INITIAL.

*      SELECT SINGLE MAX( parza )
*    FROM knvp
*    INTO l_parza
*    WHERE kunnr = fu_data-kunnr
*    AND   vkorg = fu_data-vkorg
*    AND   vtweg = fu_data-vtweg
*    AND   spart = fu_data-spart
*    AND   parvw = fu_parvw.

      IF FU_PARVW = 'WE'.
        L_COUNT_WE = L_COUNT_WE + 1.
        L_PARZA =  L_COUNT_WE.
      ELSEIF FU_PARVW = 'ZP'.
        L_COUNT_ZP = L_COUNT_ZP + 1.
        L_PARZA =  L_COUNT_ZP.
      ELSEIF FU_PARVW = 'SM'.
        L_COUNT_SM = L_COUNT_SM + 1.
        L_PARZA =  L_COUNT_SM.
      ENDIF.
*      l_parza = l_parza + 1.
      LWA_FUNCTIONS-DATA_KEY-PARZA = L_PARZA.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = FU_KUNNR
        IMPORTING
          OUTPUT = LWA_FUNCTIONS-DATA-PARTNER.
*      lwa_functions-data-partner = fu_kunnr.
      LWA_FUNCTIONS-DATAX-PARTNER = 'X'.
    ELSE.
*    lwa_functions-data-partner = 'AY9'.
      LWA_FUNCTIONS-DATA-DEFPA = 'X'.
      LWA_FUNCTIONS-DATAX-DEFPA = 'X'.
      LWA_FUNCTIONS-DATAX-PARTNER = 'X'.
    ENDIF.
    APPEND LWA_FUNCTIONS TO LT_FUNCTIONS.
  ENDIF.

ENDFORM.                    "frm_fill_partner01

*&---------------------------------------------------------------------*
*& Form frm_fill_contact
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* -->FU_NAME text
* -->FU_PHONE text
*----------------------------------------------------------------------*
*&--代码注释 BY HANDYBY 25.06.2017 16:20:10  BEGIN
*  FORM FRM_FILL_CONTACT USING FU_NAME FU_PHONE FU_PHONE_M.
*&--代码注释 BY HANDYBY 25.06.2017 16:20:10  END
*&--代码添加 BY HANDYBY 25.06.2017 16:20:20  BEGIN
FORM FRM_FILL_CONTACT USING PW_BASIS TYPE TY_BASIS .
*&--代码添加 BY HANDYBY 25.06.2017 16:20:20  END
*&--代码注释 BY HANDYBY 25.06.2017 16:22:35  BEGIN
*    IF FU_NAME IS NOT INITIAL OR FU_PHONE IS NOT INITIAL.
*&--代码注释 BY HANDYBY 25.06.2017 16:22:35  END
*&--代码添加 BY HANDYBY 25.06.2017 16:22:44  BEGIN
  IF PW_BASIS-NAME1_L IS NOT INITIAL OR PW_BASIS-TEL_NUMBER IS NOT INITIAL.
*&--代码添加 BY HANDYBY 25.06.2017 16:22:44  END
    CLEAR LWA_CONTACT.
    CLEAR:G_PARNR.

*&--代码添加 BY HANDYBY 25.06.2017 16:45:13  BEGIN
    READ TABLE GT_KNVK INTO GS_KNVK  WITH KEY KUNNR = PW_BASIS-KUNNR
                                               NAME1 = PW_BASIS-NAME1_L
*                                               TELF1 = PW_BASIS-TEL_NUMBER
                                               BINARY SEARCH .
    IF SY-SUBRC = 0 .
      G_PARNR = GS_KNVK-PARNR .
      LWA_CONTACT-TASK = 'U'.
      LWA_CONTACT-ADDRESS_TYPE_3-TASK = 'U'.
      LWA_PHONE1-CONTACT-TASK = 'U'.
      CLEAR GS_KNVK .
    ELSE.
*&--代码添加 BY HANDYBY 25.06.2017 16:45:13  END

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = 'Z1'
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
      LWA_CONTACT-ADDRESS_TYPE_3-TASK = ''.
      LWA_PHONE1-CONTACT-TASK = 'I'.

    ENDIF.

**********************************************************************

    LWA_CONTACT-DATA_KEY-PARNR = G_PARNR.
*lwa_contact-address_type_3-postal-data-firstname = 'aa'.
    LWA_CONTACT-ADDRESS_TYPE_3-POSTAL-DATA-LASTNAME = PW_BASIS-NAME1_L.
*lwa_contact-address_type_3-postal-datax-firstname = 'X'.
    LWA_CONTACT-ADDRESS_TYPE_3-POSTAL-DATAX-LASTNAME = 'X'.
    "联系人电话
    CLEAR:LWA_PHONE1.
    REFRESH LT_PHONE1.
    LWA_PHONE1-CONTACT-DATA-TELEPHONE = PW_BASIS-TEL_NUMBER .
    LWA_PHONE1-CONTACT-DATAX-TELEPHONE = 'X'.
    APPEND LWA_PHONE1 TO LT_PHONE1.

    LWA_PHONE1-CONTACT-DATA-TELEPHONE  = PW_BASIS-TEL_M.
    LWA_PHONE1-CONTACT-DATA-R_3_USER   = '2'.
    LWA_PHONE1-CONTACT-DATAX-TELEPHONE = 'X'.
    LWA_PHONE1-CONTACT-DATAX-R_3_USER  = 'X'.
    APPEND LWA_PHONE1 TO LT_PHONE1.

    LWA_CONTACT-ADDRESS_TYPE_3-COMMUNICATION-PHONE-PHONE = LT_PHONE1.
    APPEND LWA_CONTACT TO LT_CONTACT.

  ENDIF.
ENDFORM. "frm_fill_contact
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
                            T_DATA
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
  INIT_FIELDCAT 'STATU'  ''               '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'MESSAGE' '日志'               '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'KTOKD'   '账户组'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXT30'   '账户组描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUKRS'   '公司代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ANRED'  '标题'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'  '客户名称'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SORTL'  '搜索项'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LAND1'  '国家'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LANDX'  '国家描述'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REGIO'  '省'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BEZEI'  '省描述'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CITY1'  '市'                 '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZIRK'  '行政区域'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BZTXT'  '行政区域描述'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STRAS'   '注册地址'          '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STR_SUPPL3'   '办公室地址'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST_CODE1'   '邮政编号'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL1'        '座机'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL2'        '移动电话'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FAX'         '传真'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KNURL'        '网页'               '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SMTP_ADDR'    '电子邮箱'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1_L'      '主要联系人'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL_NUMBER'   '座机'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEL_M'        '手机'          '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KDGRP'        '客户来源'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTEXT'        '客户来源描述'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PLTYP'        '客户类型'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PTEXT'        '客户类型描述'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KLABC'        '客户等级'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKS'        '银行国家'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKL'        '客户开户行'      '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKA'        '客户开户行描述'   '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BANKN'        '银行帐号'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BKREF'        '参考细节'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STCEG'        '纳税人识别号'        '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'REMARK'       '其他'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'AKONT'        '统驭科目'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VKORG'        '销售组织'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VTEXT'        '销售组织描述'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VTWEG'        '分销渠道'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SPART'        '产品组'           '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INCO1'        '国际贸易条款1'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'INCO2'        '国际贸易条款2'    '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZTERM'        '付款条款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TEXT1'        '销售付款条款描述'  '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WAERS'        '货币'             '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KUNN2'        '销售业务员'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KALKS'        '客户定价过程'     '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'VSBED'        '装运条件'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KTGRD'        '账户分配组'       '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TAXKD'        '税分类'           '' '' '' '' '' '' ''.
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
*      -->P_T_DATA  text
*      -->P_1440   text
*      -->P_1441   text
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
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZSD001_STATUS' EXCLUDING RT_EXTAB.
ENDFORM.                    "SET_PF_STATUS

*&--------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&--------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                    RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA  L_CHECK TYPE C.
  CLEAR L_CHECK.

*&--代码添加 BY HANDYBY 22.06.2017 21:37:07  BEGIN
  DATA: LT_KNB1 TYPE TABLE OF KNB1,
        LS_KNB1 TYPE KNB1.
  SELECT * FROM KNB1
    INTO CORRESPONDING FIELDS OF TABLE LT_KNB1 .

  DATA: LT_KNVV TYPE TABLE OF KNVV,
        LS_KNVV TYPE KNVV.
  SELECT * FROM KNVV
    INTO CORRESPONDING FIELDS OF TABLE LT_KNVV .
*&--代码添加 BY HANDYBY 22.06.2017 21:37:07  END

  CASE R_UCOMM.

    WHEN '&IC1'. "双击
*      IF G_TEST IS INITIAL."
*        CLEAR W_LOG.
*        READ TABLE T_LOG INTO W_LOG INDEX RS_SELFIELD-TABINDEX.
*        CASE RS_SELFIELD-FIELDNAME.
*          WHEN 'KUNNR'.
*            SET PARAMETER ID  'KUN' FIELD W_LOG-KUNNR.
*            SET PARAMETER ID  'BUK' FIELD ''.
*            SET PARAMETER ID  'VKO' FIELD ''.
*            SET PARAMETER ID  'VTW' FIELD ''.
*            SET PARAMETER ID  'SPA' FIELD ''.
*            CALL TRANSACTION  'XD03' WITH AUTHORITY-CHECK." AND SKIP FIRST SCREEN.
*        ENDCASE.
*      ELSE.
*        MESSAGE '请在非测试模式下查看' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.

    WHEN '&UPLOAD'.
      DATA GT_KNA1 TYPE TABLE OF KNA1.
      DATA GS_KNA1 TYPE KNA1.


      REFRESH GT_KNA1.
*查询客户名称
      SELECT * FROM KNA1
      INTO CORRESPONDING FIELDS OF TABLE GT_KNA1.

*检查是否已经进行测试导入，并且无报错
      READ TABLE GT_OUTPUT INTO GS_OUTPUT
      WITH KEY STATU = ICON_RED_LIGHT.
      IF SY-SUBRC = 0.
        L_CHECK = 'X'.
      ENDIF.

      READ TABLE GT_OUTPUT INTO GS_OUTPUT
      WITH KEY STATU = ICON_YELLOW_LIGHT.
      IF SY-SUBRC = 0.
        L_CHECK = 'X'.
      ENDIF.

*检查客户和账户组的重复性
      LOOP AT GT_OUTPUT INTO GS_OUTPUT.
        READ TABLE GT_KNA1 INTO GS_KNA1
        WITH KEY NAME1 = GS_OUTPUT-NAME1 KTOKD = GS_OUTPUT-KTOKD.
        IF SY-SUBRC = 0.
*&--代码添加 BY HANDYBY 25.06.2017 11:39:36  BEGIN
          GS_OUTPUT-KUNNR_EXIST = 'X' .
          GS_OUTPUT-KUNNR = GS_KNA1-KUNNR .
*&--代码添加 BY HANDYBY 25.06.2017 11:39:36  END
*&--代码添加 BY HANDYBY 22.06.2017 21:40:26  BEGIN
          READ TABLE LT_KNB1 INTO LS_KNB1 WITH KEY KUNNR = GS_KNA1-KUNNR
                                                   BUKRS = GS_OUTPUT-BUKRS  .
          IF SY-SUBRC = 0 .
            GS_OUTPUT-BUKRS_EXIST = 'X' .
            L_CHECK = 'X'.
            GS_OUTPUT-STATU   = ICON_RED_LIGHT.
            GS_OUTPUT-MESSAGE = '该客户对应的公司代码已经在系统中存在'.
          ELSE.
            READ TABLE LT_KNVV INTO LS_KNVV WITH KEY KUNNR = GS_KNA1-KUNNR
                                                     VKORG = GS_OUTPUT-VKORG
                                                     VTWEG = GS_OUTPUT-VTWEG
                                                     SPART = GS_OUTPUT-SPART .
            IF SY-SUBRC = 0 .
              GS_OUTPUT-VKORG_EXIST = 'X' .
              L_CHECK = 'X'.
              GS_OUTPUT-STATU   = ICON_RED_LIGHT.
              GS_OUTPUT-MESSAGE = '该客户对应的销售组织已经在系统中存在'.
            ENDIF.
          ENDIF.

        ENDIF.

        MODIFY GT_OUTPUT FROM GS_OUTPUT.
        CLEAR GS_OUTPUT.
*&--代码添加 BY HANDYBY 25.06.2017 11:33:36  BEGIN
        CLEAR GS_KNA1 .
        CLEAR LS_KNB1 .
        CLEAR LS_KNVV .
*&--代码添加 BY HANDYBY 25.06.2017 11:33:36  END
      ENDLOOP.

      IF L_CHECK <> 'X'.
        G_TEST = ''.
        PERFORM FRM_FILL_DATA.                 "将数据填充内表
      ELSE.
        MESSAGE '请进行测试导入并无报错再进行正式导入' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&TEST'.
      CLEAR L_CHECK.
      REFRESH GT_KNA1.

*查询客户名称
      SELECT * FROM KNA1
      INTO CORRESPONDING FIELDS OF TABLE GT_KNA1.

*检查客户和账户组的重复性
      LOOP AT GT_OUTPUT INTO GS_OUTPUT.
        READ TABLE GT_KNA1 INTO GS_KNA1
        WITH KEY NAME1 = GS_OUTPUT-NAME1 KTOKD = GS_OUTPUT-KTOKD.
        IF SY-SUBRC = 0.
*&--代码添加 BY HANDYBY 25.06.2017 11:39:36  BEGIN
          GS_OUTPUT-KUNNR_EXIST = 'X' .
          GS_OUTPUT-KUNNR = GS_KNA1-KUNNR .
*&--代码添加 BY HANDYBY 25.06.2017 11:39:36  END
*&--代码添加 BY HANDYBY 22.06.2017 21:40:26  BEGIN
          READ TABLE LT_KNB1 INTO LS_KNB1 WITH KEY KUNNR = GS_KNA1-KUNNR
                                                   BUKRS = GS_OUTPUT-BUKRS  .
          IF SY-SUBRC = 0 .
            GS_OUTPUT-BUKRS_EXIST = 'X' .
            L_CHECK = 'X'.
            GS_OUTPUT-STATU   = ICON_RED_LIGHT.
            GS_OUTPUT-MESSAGE = '该客户对应的公司代码已经在系统中存在'.
          ELSE.
            READ TABLE LT_KNVV INTO LS_KNVV WITH KEY KUNNR = GS_KNA1-KUNNR
                                                     VKORG = GS_OUTPUT-VKORG
                                                     VTWEG = GS_OUTPUT-VTWEG
                                                     SPART = GS_OUTPUT-SPART .
            IF SY-SUBRC = 0 .
              GS_OUTPUT-VKORG_EXIST = 'X' .
              L_CHECK = 'X'.
              GS_OUTPUT-STATU   = ICON_RED_LIGHT.
              GS_OUTPUT-MESSAGE = '该客户对应的销售组织已经在系统中存在'.
            ENDIF.
          ENDIF.
*&--代码添加 BY HANDYBY 22.06.2017 21:40:26  END
*&--代码注释 BY HANDYBY 22.06.2017 21:55:58  BEGIN
*    L_CHECK = 'X'.
*          GS_OUTPUT-STATU   = ICON_RED_LIGHT.
*          GS_OUTPUT-MESSAGE = '该客户名称已经在系统中存在'.
*          MODIFY GT_OUTPUT FROM GS_OUTPUT.
*          CLEAR GS_OUTPUT.
*&--代码注释 BY HANDYBY 22.06.2017 21:55:58  END
        ENDIF.
        MODIFY GT_OUTPUT FROM GS_OUTPUT.
        CLEAR GS_OUTPUT.
*&--代码添加 BY HANDYBY 25.06.2017 11:33:36  BEGIN
        CLEAR GS_KNA1 .
        CLEAR LS_KNB1 .
        CLEAR LS_KNVV .
*&--代码添加 BY HANDYBY 25.06.2017 11:33:36  END
      ENDLOOP.

      IF L_CHECK <> 'X'.
        G_TEST = 'X'.
        PERFORM FRM_FILL_DATA.                 "将数据填充内表
      ENDIF.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.  "自动刷新
ENDFORM. "USER_COM
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  LOOP AT T_DATA INTO W_DATA.
    MOVE-CORRESPONDING W_DATA TO GS_OUTPUT.
    GS_OUTPUT-STATU = ICON_YELLOW_LIGHT.
    APPEND GS_OUTPUT TO GT_OUTPUT.
    CLEAR GS_OUTPUT.
  ENDLOOP.
ENDFORM.

*--------------------------------------------------------------------*
*  form frm_get_next
*  获得当前的范围对象和区间编号对应的流水号
*
*--------------------------------------------------------------------*
FORM FRM_GET_NEXT CHANGING L_KUNNR TYPE KUNNR
                           L_KTOKD TYPE KTOKD.

*读取修改对象
  READ TABLE GT_V_077D_B INTO GS_V_077D_B
  WITH KEY KTOKD  = L_KTOKD.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE '
    EXPORTING
      OBJECT           = 'DEBITOR'
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
  IF SY-SUBRC EQ 0 .
*  ****得到一个号码，
    CALL FUNCTION 'NUMBER_GET_NEXT '
      EXPORTING
        NR_RANGE_NR             = GS_V_077D_B-NUMKR    "这个就是维护的间隔号
        OBJECT                  = 'DEBITOR'           "这个就是流水号对象
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = L_KUNNR             "获得的流水号
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
*  ***将号码累加
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE '
      EXPORTING
        OBJECT           = 'DEBITOR'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ELSE .
    RAISE NUM_RANGE_ERROR .
  ENDIF .

ENDFORM.                    "frm_get_next
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD .
*  TYPES:BEGIN OF TY_EXCEL,
*          T1  TYPE CHAR50,
*          T2  TYPE CHAR50,
*          T3  TYPE CHAR50,
*          T4  TYPE CHAR50,
*          T5  TYPE CHAR50,
*          T6  TYPE CHAR50,
*          T7  TYPE CHAR50,
*          T8  TYPE CHAR50,
*          T9  TYPE CHAR50,
*          T10 TYPE CHAR50,
*          T11 TYPE CHAR50,
*          T12 TYPE CHAR50,
*          T13 TYPE CHAR50,
*          T14 TYPE CHAR50,
*          T15 TYPE CHAR50,
*          T16 TYPE CHAR50,
*          T17 TYPE CHAR50,
*          T18 TYPE CHAR50,
*          T19 TYPE CHAR50,
*          T20 TYPE CHAR50,
*          T21 TYPE CHAR50,
*          T22 TYPE CHAR50,
*          T23 TYPE CHAR50,
*          T24 TYPE CHAR50,
*          T25 TYPE CHAR50,
*          T26 TYPE CHAR50,
*          T27 TYPE CHAR50,
*          T28 TYPE CHAR50,
*          T29 TYPE CHAR50,
*          T30 TYPE CHAR50,
*          T31 TYPE CHAR50,
*          T32 TYPE CHAR50,
*          T33 TYPE CHAR50,
*          T34 TYPE CHAR50,
*          T35 TYPE CHAR50,
*          T36 TYPE CHAR50,
*          T37 TYPE CHAR50,
*          T38 TYPE CHAR50,
*          T39 TYPE CHAR50,
*          T40 TYPE CHAR50,
*          T41 TYPE CHAR50,
*          T42 TYPE CHAR50,
*          T43 TYPE CHAR50,
*          T44 TYPE CHAR50,
*          T45 TYPE CHAR50,
*          T46 TYPE CHAR50,
*          T47 TYPE CHAR50,
*          T48 TYPE CHAR50,
*          T49 TYPE CHAR50,
*          T50 TYPE CHAR50,
*        END OF TY_EXCEL.
*
*  DATA LT_EXCEL TYPE TABLE OF TY_EXCEL.
*  DATA LS_EXCEL TYPE TY_EXCEL.
*
*  LS_EXCEL-T1   = '账户组'.
*  LS_EXCEL-T2   = '账户组描述'.
*  LS_EXCEL-T3   = '公司代码'.
*  LS_EXCEL-T4   = '标题'.
*  LS_EXCEL-T5   = '客户名称'.
*  LS_EXCEL-T6   = '搜索项'.
*  LS_EXCEL-T7   = '国家'.
*  LS_EXCEL-T8   = '国家描述'.
*  LS_EXCEL-T9   = '省'.
*  LS_EXCEL-T10  = '省描述'.
*  LS_EXCEL-T11  = '市'.
*  LS_EXCEL-T12  = '行政区域'.
*  LS_EXCEL-T13  = '行政区域描述'.
*  LS_EXCEL-T14  = '注册地址'.
*  LS_EXCEL-T15  = '办公地址'.
*  LS_EXCEL-T16  =  '邮政编码'.
*  LS_EXCEL-T17  =  '座机'.
*  LS_EXCEL-T18  =  '移动电话'.
*  LS_EXCEL-T19  =  '传真'.
*  LS_EXCEL-T20  =  '网页'.
*  LS_EXCEL-T21  =  '电子邮箱'.
*  LS_EXCEL-T22  =  '主要联系人'.
*  LS_EXCEL-T23  =  '座机'.
*  LS_EXCEL-T24  =  '手机'.
*  LS_EXCEL-T25  =  '客户来源'.
*  LS_EXCEL-T26  =  '客户来源描述'.
*  LS_EXCEL-T27  =  '客户类型'.
*  LS_EXCEL-T28  =  '客户类型描述'.
*  LS_EXCEL-T29  =  '客户等级'.
*  LS_EXCEL-T30  =  '银行国家'.
*  LS_EXCEL-T31  =  '客户开户行'.
*  LS_EXCEL-T32  =  '客户开户行描述'.
*  LS_EXCEL-T33  =  '银行帐号'.
*  LS_EXCEL-T34  =  '参考细节'.
*  LS_EXCEL-T35  =  '纳税人识别号'.
*  LS_EXCEL-T36  =  '其它'.
*  LS_EXCEL-T37  =  '统驭科目'.
*  LS_EXCEL-T38  =  '销售组织'.
*  LS_EXCEL-T39  =  '分销渠道'.
*  LS_EXCEL-T40  =  '产品组'.
*  LS_EXCEL-T41  =  '国际贸易条款（部分一）'.
*  LS_EXCEL-T42  =  '国际贸易条款（部分二）'.
*  LS_EXCEL-T43  =  '销售付款条件'.
*  LS_EXCEL-T44  =  '销售付款条件描述'.
*  LS_EXCEL-T45  =  '货币'.
*  LS_EXCEL-T46  =  '销售业务员'.
*  LS_EXCEL-T47  =  '客户定价过程'.
*  LS_EXCEL-T48  =  '装运条件'.
*  LS_EXCEL-T49  =  '帐户分配组'.
*  LS_EXCEL-T50  =  '税分类'.
*  APPEND LS_EXCEL TO LT_EXCEL.
*  CLEAR LS_EXCEL.
*
**  CALL FUNCTION 'GUI_DOWNLOAD' "根据文件路径建立文件
**    EXPORTING
**      　　FILENAME = P_DO
**      　　FILETYPE = 'BIN'
**    TABLES
**      　　DATA_TAB = LT_EXCEL.
*
*  CALL FUNCTION 'WS_DOWNLOAD'
*    EXPORTING
*      FILENAME                = P_DO
*      FILETYPE                = 'DAT'
*    TABLES
*      DATA_TAB                = LT_EXCEL
*    EXCEPTIONS
*      INVALID_FILESIZE        = 1
*      INVALID_TABLE_WIDTH     = 2
*      INVALID_TYPE            = 3
*      NO_BATCH                = 4
*      UNKNOWN_ERROR           = 5
*      GUI_REFUSE_FILETRANSFER = 6
*      CUSTOMER_ERROR          = 7
*      OTHERS                  = 8.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.


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
  FROM WWWDATA WHERE OBJID = 'ZSD001' .

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
