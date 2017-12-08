REPORT ZHRPA01_01.
TYPE-POOLS:SLIS.
TABLES:PERNR,PYORGSCREEN,PYTIMESCREEN.
NODES:PERSON,GROUP,PERAS.
INFOTYPES: 0000 NAME P0000,
           0001 NAME P0001 ,
           0002 NAME P0002,
         "  0529 NAME P0059,
           0185 NAME P0185,
           0041 NAME P0041,
          " 0000 NAME P0000,
           0534 NAME P0534,
           9006 NAME P9006,
           9022 NAME P9022,
          " 0041 NAME P0041,
           0016 NAME P0016,
           3211 NAME P3211,
           "3527 NAME P3527,
           0009 NAME P0009,
           9105 NAME P9105,
           0031 NAME P0031,
           9379 NAME P9379,
*&--代码添加 BY HANDYBY 07.06.2017 20:49:05  BEGIN
           9029 NAME P9029 .
*&--代码添加 BY HANDYBY 07.06.2017 20:49:05  END

TYPES:
  BEGIN OF GST_DATA ,
    ZZXH        TYPE N LENGTH 10 , "序号
    PERNR       TYPE PERSNO, "工号
    ENAME       TYPE EMNAM, "姓名
    RUFNM       TYPE PAD_RUFNM,  "曾用户
    GBDAT       TYPE GBDAT, "身份证出生日期
    ZZSJRQ      TYPE ZDESJRQ, "实际出生日期
    ZSRTS       TYPE C LENGTH 30 , "生日提醒
    JSRDT       TYPE C LENGTH 30, "距生日当天
    ZNL         TYPE N LENGTH 4 , "年龄
    GESCH       TYPE GESCH, "性别
    OBJIDJT     TYPE ORGEH, "集团组织
    JTTEXT      TYPE STRING , "
    OBJIDKG     TYPE ORGEH, "控股组织
    KGTEXT      TYPE STRING , "控股组织名称
    OBJIDC1     TYPE ORGEH, "一级组织
    C10RGEN     TYPE C LENGTH 50 , "一级部门
    OBJIDC2     TYPE ORGEH, "二级组织
    C20RGEN     TYPE C LENGTH 50, "二级部门
    OBJIDC3     TYPE ORGEH, "三级组织
    C30RGEN     TYPE C LENGTH 50, "三级部门
    OBJIDC4     TYPE ORGEH , "四级组织
    C40RGEN     TYPE C LENGTH 50 , "四级部门
    PLANS       TYPE PLANS, "职位
    KOSTL       TYPE KOSTL, "成本中心
    KOSTL_KTEXT TYPE KTEXT, "成本中心文本
    "  PLANS_PLSTX TYPE T528T-PLSTX ,"岗位名称
    PLANS_PLSTX TYPE STRING , "岗位名称
    WERKS       TYPE PERSA, "人事范围
    WERKS_NAME1 TYPE  T500P-NAME1 , "人事范围名称
    PERSG       TYPE PERSG, "员工组
    PERSG_PTEXT TYPE T501T-PTEXT , "员工组名称
    PERSK       TYPE PERSK, "员工子组
    PERSK_PTEXT TYPE T503T-PTEXT , "员工子组名称
    ABKRS       TYPE ABKRS, "工资范围
    ABKRS_ATEXT TYPE T549T-ATEXT, "工资范围名称
    ICNUM       TYPE PSG_IDNUM, "身份证号码
    USEFR       TYPE P25_USEFR, "身份证有效期起始日
    USETO       TYPE P25_USETO,  "身份证有效期截止日
    ICNUMZ1     TYPE PSG_IDNUM, "外籍护照号码
    ICNUM02     TYPE PSG_IDNUM, "中国护照号码
    ZZJGSF      TYPE ZDEJGSF, "籍贯省份
    ZZJGSF_TXT  TYPE ZDEJGSF, "籍贯省份
    ZZHKSF      TYPE ZDEHKSF, "户口省份
    ZZHKSF_TXT  TYPE C LENGTH 50, "户口省份文本
    ZZHKXZ      TYPE ZDEHKXZ, "户口性质
    ZZHKXZ_TXT  TYPE C LENGTH 50 , "户口性质文本
    PCODE       TYPE PCN_PCODE, "政治面貌
    PCODE_PTEXT TYPE C LENGTH 50 , "政治面貌文本
    RACKY       TYPE PCN_RACKY, "民族
    RACKY_LTEXT TYPE T505S-LTEXT , "民族文本
    FAMST_FATXT TYPE FATXT, "婚姻状况
    ZZXXDZ1     TYPE ZDEXXDZ, "户籍地址
    ZZXXDZ2     TYPE ZDEXXDZ, "现居住地址
    ZZXL        TYPE ZDEXL, "最高学历
    ZZXL_TXT    TYPE C LENGTH 50 , "最高学历文本
    ZZXW        TYPE ZDEXW, "最高学位
    ZZXW_TXT    TYPE C LENGTH 50 , "最高学位文本
    ZZXXMC      TYPE ZDEXXMC, "毕业学校
    ZZZYMC1     TYPE C LENGTH 100, "专业
    BEGDA       TYPE BEGDA, "最高学历起始日期
    ENDDA       TYPE ENDDA, "毕业日期
    DATD1       TYPE D , " 当前司龄起算日期（入职日期）
    DATD2       TYPE D, "累计司龄起算日期
    DATD3       TYPE D, "社会工龄起算日期
    CTTYP       TYPE CTTYP, "当前合同类型
    CTTYP_CTTXT TYPE  T547S-CTTXT, "合同类型文本
    HTBEGDA     TYPE BEGDA, " 当前合同起始日期
    CTEDT       TYPE CTEDT,  " 当前合同结束日期
    ZHTDQYF     TYPE C LENGTH 30, " 距当前合同到期月份数
    ZHTDQTX     TYPE C LENGTH 50, "合同到期提醒
    ZZHTCS      TYPE ZDEHTCS, "自2008年后合同签订次数
    ZZHTGS      TYPE ZDEHTGS, " 合同公司主体
    ZZHTGS_TXT  TYPE C LENGTH 50 , "合同主体文本
    ZZQSRQ      TYPE ZDEQSRQ, " 当前合同签署日期
    ZZQSBZ      TYPE ZDEQSBZ,                              " 当前合同签署备注
    ZZSYQ       TYPE ZDESYQ, " 试用期截止日
    ZZSYQDQ     TYPE C LENGTH 30, "距试用期到期天数
    ZZZTX       TYPE C LENGTH 30, "  转正提醒
    ZZMYSTS     TYPE C LENGTH 50 , "距满月考核到期天数
    ZZMYTX      TYPE C LENGTH 30, " 满月提醒
    ZZGSSJHM    TYPE ZDEGSSJHM, " 公司手机号
    ZZGSDH      TYPE ZDEGSDH, "  公司短号
    ZZGRSJHY    TYPE ZDEGRSJHY, " 手机号码一
    ZZGRJHE     TYPE ZDEGRSJHE,  " 手机号码二
    ZZZJH       TYPE ZDEZJH, "家庭座机号
    ZZQQ        TYPE ZDEQQ, "QQ
    ZZWX        TYPE ZDEWX, " 微信号
    ZZGSYX      TYPE ZDEGSYX, " 公司邮箱
    ZZGRYX      TYPE ZDEGRYX, " 个人邮箱
    ZZDYXM      TYPE ZDEDYXM, " 第一紧急联系人姓名
    ZZDYGX      TYPE ZDEDYGX,   " 第一紧急联系人与本人关系
    ZZDYDZ      TYPE ZDEDYDZ, " 第一紧急联系人地址
    ZZDYHM      TYPE ZDEDYHM,   "  第一紧急联系人手机号码
    ZZXZ        TYPE ZDEXZ, " 星座
    ZZXZ_TXT    TYPE C LENGTH 30, "星座文本
    ZZXX        TYPE ZDEXX, " 血型
    ZZXX_TXT    TYPE C LENGTH 30 , "血型文本
    NAME2       TYPE PAD_NAME2, " 英文名
    ZZZJXY      TYPE ZDEZJXY, "  宗教信仰
    RFPNR       TYPE RFPNR,  " 关联工号
    ZZQQSX      TYPE ZZQQSX, "股票期权时限

*&--代码添加 BY HANDYBY 07.06.2017 17:20:42  BEGIN
    DDTEXT      TYPE VAL_TEXT,
    ZZBZ1       TYPE P9029-ZZBZ1,
    ZZBZ2       TYPE P9029-ZZBZ2,
    ZZBZ3       TYPE P9029-ZZBZ3,
*&--代码添加 BY HANDYBY 07.06.2017 17:20:42  END

    BOX         TYPE C LENGTH 1,
  END OF GST_DATA.
DATA: GS_DATA TYPE GST_DATA.
DATA: GT_DATA TYPE STANDARD TABLE OF GST_DATA.
DATA: GT_HRP1001 LIKE TABLE OF HRP1001 WITH  HEADER LINE.
DATA: GT_HRP1000  LIKE TABLE OF HRP1000 WITH HEADER LINE.
DATA: JSN TYPE I .
DATA:T_NLVALUE TYPE T71GS-LOVAL.
DATA:T_NIAN TYPE STRING.
DATA:PID LIKE P0001-PERNR.
DATA:NINDEX TYPE I .
DATA:L_DAYS  TYPE P,
     L_MONTH TYPE P,
     L_YEARS TYPE P,
     L_STR   TYPE STRING.
DATA:YC TYPE N LENGTH 3.
"**INTERNAL TABLE DECLARTION
DATA :
  GR_ALV     TYPE REF TO CL_SALV_TABLE,
  GR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV WITH HEADER LINE,

      G_SAVE      TYPE C VALUE 'X',
      G_VARIANT   TYPE DISVARIANT,
      GX_VARIANT  TYPE DISVARIANT,
      G_EXIT      TYPE C,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.
"*&---------------------------------------------------------------------*
*  DEFINE VARIABLE
*&---------------------------------------------------------------------*
DATA:
 " GDS_DATA TYPE GTS_DATA,
  LDF_MGTXT TYPE MGTXT.
*&---------------------------------------------------------------------*
* DEFINE CONSTANTS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* DEFINE SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETERS P_DATE TYPE SY-DATUM  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.
*&---------------------------------------------------------------------*
* INITIALIZATION.                                                      *
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.                                          *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
START-OF-SELECTION.

  GET PERAS .
  PERFORM SEL_DATA.




  "GET
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* GET.
*&---------------------------------------------------------------------*
  "GET PERNR.

END-OF-SELECTION.
  PERFORM  FRM_DEIT_DATA.
  PERFORM  FRM_ADD_FIELDCATALOG.
  PERFORM  FRM_ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      FORM  FRM_ADD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_ADD_FIELDCATALOG .
  CLEAR IT_FIELDCAT[].
  PERFORM FRM_FILL_CAT USING:
       1 ''  'ZZXH' '序号' ,
       2 '' 'PERNR' '工号',
       3 '' 'ENAME' '姓名',
       4 '' 'RUFNM' '曾用名',
       5 '' 'GBDAT' '身份证出生日期',
       6 '' 'ZZSJRQ' '实际出生日期',
       7 '' 'ZSRTS' '生日提醒',
       8 '' 'JSRDT' '距生日当天(天数)',
       8 '' 'ZNL' '年龄',
       9 '' 'GESCH' '性别',
       10 '' 'JTTEXT' '集团',
       10 '' 'KGTEXT' '控股公司' ,
       10 '' 'C10RGEN' '一级部门',
       11 '' 'C20RGEN' '二级部门',
       12 '' 'C30RGEN' '三级部门',
       13 '' 'C40RGEN' '四级部门',
       13 '' 'KOSTL' '成本中心',
       13 '' 'KOSTL_KTEXT' '成本中心描述',
       14 '' 'PLANS_PLSTX' '职位',
       15 '' 'WERKS_NAME1' '人事范围',
       16 '' 'PERSG_PTEXT' '员工组',
      17 '' 'PERSK_PTEXT' '员工子组',
       18 '' 'ABKRS_ATEXT' '工资范围',
       19 '' 'ICNUM' '身份证号码',
       20 '' 'USEFR' '身份证有效期起始日',
       21 '' 'USETO' '身份证有效期截止日',
       22 '' 'ICNUMZ1' '外籍护照号码',
       23 '' 'ICNUM02' '中国护照号码',
       24 '' 'ZZJGSF_TXT' '籍贯省份',
       25 '' 'ZZHKSF_TXT' '户口省份',
       26 '' 'ZZHKXZ_TXT' '户口性质',
       27 '' 'PCODE_PTEXT' '政治面貌',
       28 '' 'RACKY_LTEXT' '民族',
       29 '' 'FAMST_FATXT' '婚姻状况',
       30 '' 'ZZXXDZ1' '户籍地址',
       31 '' 'ZZXXDZ2' '现居住地址',
       32'' 'ZZXL_TXT' '最高学历',
       33 '' 'ZZXW_TXT' '最高学位',
       34 '' 'ZZXXMC' '毕业学校',
       35 '' 'ZZZYMC1' '专业',
       36 '' 'BEGDA' '最高学历起始日期',
       37 '' 'ENDDA' '毕业日期',
       38 '' 'DATD3' '社会工龄起算日期',
       39 '' 'DATD2' '累计司龄起算日期',
       40 '' 'DATD1' '当前司龄起算日期（入职日期）' ,
       41 '' 'CTTYP_CTTXT' '当前合同类型' ,
       42 '' 'HTBEGDA' '当前合同起始日期' ,
       43'' 'CTEDT' '当前合同结束日期' ,
       44 '' 'ZHTDQYF' '距当前合同到期月份数' ,
       45 '' 'ZHTDQTX' '合同到期提醒' ,
       46 '' 'ZZHTCS' '自2008年后合同签订次数' ,
       47 '' 'ZZHTGS_TXT' '合同公司主体' ,
       48 '' 'ZZQSRQ' '当前合同签署日期' ,
       49 '' 'ZZQSBZ' ' 当前合同签署备注' ,
       50 '' 'ZZSYQ' '试用期截止日' ,
       51 '' 'ZZSYQDQ' '距试用期到期天数' ,
       52 '' 'ZZZTX' '转正提醒' ,
       53 '' 'ZZMYSTS' '距满月考核到期天数' ,
       54'' 'ZZMYTX' '满月提醒' ,
       55 '' 'ZZGSSJHM' '公司手机号' ,
       56 '' 'ZZGSDH' ' 公司短号' ,
       57 '' 'ZZGRSJHY' '手机号码一' ,
       58 '' 'ZZGRJHE' '手机号码二' ,
       59 '' 'ZZZJH' '家庭座机号' ,
       60 '' 'ZZQQ' 'QQ' ,
       61 '' 'ZZWX' '微信号' ,
       62 '' 'ZZGSYX' '公司邮箱' ,
       63 '' 'ZZGRYX' '个人邮箱' ,
       64 '' 'ZZDYXM' '第一紧急联系人姓名' ,
       65 '' 'ZZDYGX' '第一紧急联系人与本人关系' ,
       66 '' 'ZZDYDZ' '第一紧急联系人地址' ,
       67'' 'ZZDYHM' '第一紧急联系人手机号码' ,
      68 '' 'ZZXZ_TXT' '星座' ,
       69 '' 'ZZXX_TXT' '血型' ,
       70 '' 'NAME2' '英文名' ,
       71 '' 'ZZZJXY' '宗教信仰' ,
       72 '' 'RFPNR' '关联工号',
       73 '' 'ZZQQSX' '股票期权时限',

*&--代码添加 BY HANDYBY 07.06.2017 17:09:50  BEGIN
     74 '' 'DDTEXT' '工资成本归属单位' ,
     75 '' 'ZZBZ1' '备注2' ,
     76 '' 'ZZBZ2' '备注3' ,
     77 '' 'ZZBZ3' '备注4' .
*&--代码添加 BY HANDYBY 07.06.2017 17:09:50  END


ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_ALV_DISPLAY .
  DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.

* L_LAYOUT-CWIDTH_OPT = 'X'.
  L_LAYOUT-BOX_FIELDNAME = 'BOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
   "  I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
      I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_SAVE                   = 'A'
      I_GRID_SETTINGS          = L_GRID_SETTINGS
      IS_LAYOUT                = L_LAYOUT
      IS_VARIANT               = G_VARIANT
    TABLES
      T_OUTTAB                 = GT_DATA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZHRPA01_01_STATUS'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DEIT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FRM_DEIT_DATA .

ENDFORM.
FORM FRM_FILL_CAT USING U_POS U_EDIT U_FNAME U_NAME ."U_WAERS.
  DATA:LW_FIELDCAT LIKE LINE OF IT_FIELDCAT.
  LW_FIELDCAT-COL_POS     = U_POS.
  LW_FIELDCAT-EDIT        = U_EDIT.
  LW_FIELDCAT-FIELDNAME   = U_FNAME.
  LW_FIELDCAT-SELTEXT_L   = U_NAME.
  IF  U_FNAME = 'PERNR'.
    LW_FIELDCAT-HOTSPOT = 'X'.
    LW_FIELDCAT-LZERO = 'X'.
  ENDIF.
  "LW_FIELDCAT-CFIELDNAME =  U_WAERS .
*  IF U_FNAME = 'LFIMG'.
*   LW_FIELDCAT-QFIELDNAME = 'MEINS' .
*  ENDIF.
  " LW_FIELDCAT-NO_ZERO   = 'X'.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    "FRM_FILL_CAT
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA:L_INDEX TYPE SY-TABIX.
  DATA: IT_LINE LIKE LINE OF   GT_DATA.

  CASE R_UCOMM.
    WHEN '&IC1'."双击
      IF RS_SELFIELD-FIELDNAME = 'PERNR'.
        READ TABLE GT_DATA INTO IT_LINE  INDEX RS_SELFIELD-TABINDEX.
        IF SY-SUBRC = 0.
          SET PARAMETER ID 'PER' FIELD IT_LINE-PERNR.  "双击员工编号跳转到PA20查看人事信息
          CALL TRANSACTION 'PA20' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  RS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  SEL_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEL_DATA ."查询数据
  CLEAR:P0000,P0001,P0002,P0185,P0041,P0534,P9006,P9022,P0016,P3211,P0009,P9105,P9379.
  CLEAR :GS_DATA .

*PROVIDE * FROM P0001  BETWEEN PN-BEGDA AND PN-ENDDA .
*
*ENDPROVIDE.
  IF PNPBUKRS[] IS NOT INITIAL.
    DELETE P0001[] WHERE BUKRS NOT IN PNPBUKRS.
  ENDIF.
  IF PNPPERNR[] IS NOT INITIAL..
    DELETE P0000[] WHERE PERNR NOT IN PNPPERNR.
  ENDIF.
  IF PNPPERSG[] IS NOT INITIAL.
    DELETE P0001[] WHERE PERSG NOT IN PNPPERSG.
  ENDIF.
  IF PNPWERKS[] IS NOT INITIAL.
    DELETE P0001[] WHERE WERKS NOT IN PNPWERKS.
  ENDIF.

  IF P_DATE IS NOT INITIAL.
    DELETE P0000[] WHERE BEGDA > P_DATE.
    DELETE P0000[] WHERE ENDDA < P_DATE.
    DELETE P0001[] WHERE BEGDA > P_DATE.
    DELETE P0001[] WHERE ENDDA < P_DATE.
    DELETE P0000[] WHERE BEGDA > P_DATE.
    DELETE P0000[] WHERE ENDDA < P_DATE.
    DELETE P0002[] WHERE BEGDA > P_DATE.
    DELETE P0002[] WHERE ENDDA < P_DATE.
    DELETE P0185[] WHERE BEGDA > P_DATE.
    DELETE P0185[] WHERE ENDDA < P_DATE.
    DELETE P0534[] WHERE BEGDA > P_DATE.
    DELETE P0534[] WHERE ENDDA < P_DATE.
    DELETE P9006[] WHERE BEGDA > P_DATE.
    DELETE P9006[] WHERE ENDDA < P_DATE.
*  DELETE P9022[] WHERE BEGDA > P_DATE.
*  DELETE P9022[] WHERE ENDDA < P_DATE.
    DELETE P0016[] WHERE BEGDA > P_DATE.
    DELETE P0016[] WHERE ENDDA < P_DATE.
    DELETE P3211[] WHERE BEGDA > P_DATE.
    DELETE P3211[] WHERE ENDDA < P_DATE.
    DELETE P0009[] WHERE BEGDA > P_DATE.
    DELETE P0009[] WHERE ENDDA < P_DATE.
    DELETE P0031[] WHERE BEGDA > P_DATE.
    DELETE P0031[] WHERE ENDDA < P_DATE.
    DELETE P9379[] WHERE BEGDA > P_DATE.
    DELETE P9379[] WHERE ENDDA < P_DATE.

*&--代码添加 BY HANDYBY 07.06.2017 21:22:06  BEGIN
    DELETE P9029[] WHERE ENDDA < P_DATE.
    DELETE P9029[] WHERE ENDDA < P_DATE.
*&--代码添加 BY HANDYBY 07.06.2017 21:22:06  END


  ENDIF.
  CHECK P0000[] IS NOT INITIAL.
  CHECK P0001[] IS NOT INITIAL.
  LOOP AT P0000.
    IF P0000-STAT2 = '0'.
      EXIT.  "排除离职 员工
    ENDIF.
    GS_DATA-PERNR = PERAS-PERNR. "员工编号
  ENDLOOP.
  CHECK  NOT GS_DATA-PERNR IS INITIAL.  "先检查员工编号是否已赋值
  SELECT * INTO TABLE GT_HRP1001
    FROM HRP1001
    WHERE PLVAR = '01'
     AND OTYPE = 'P'
    AND OBJID =  GS_DATA-PERNR
    AND PROZT = 0
    AND RSIGN = 'B'
    AND RELAT = '008'
    AND ISTAT = '1'
  AND ( BEGDA <= P_DATE AND ENDDA >= P_DATE ).
  IF GT_HRP1001[] IS NOT INITIAL.
    SELECT *
    INTO TABLE GT_HRP1000
    FROM HRP1000
    WHERE PLVAR = '01'                "计划版本
    AND OTYPE IN ('O','S','P')      "对象类型
    AND ISTAT = '1'.                 "计划状态
  ENDIF.
  NINDEX = NINDEX + 1 .
  GS_DATA-ZZXH = NINDEX."组织序号
  LOOP AT P0001 .
    GS_DATA-ENAME = P0001-ENAME.  "姓  名
    " CONDENSE GS_DATA-ENAME NO-GAPS.
    GS_DATA-WERKS = P0001-WERKS ."人事范围
    SELECT SINGLE NAME1 INTO GS_DATA-WERKS_NAME1 FROM T500P WHERE PERSA = GS_DATA-WERKS."人事范围名称
    GS_DATA-PERSG = P0001-PERSG."员工组
    SELECT SINGLE PTEXT INTO GS_DATA-PERSG_PTEXT FROM T501T WHERE  SPRSL = '1' AND PERSG = GS_DATA-PERSG."员工组名称
    GS_DATA-PERSK = P0001-PERSK.
    SELECT SINGLE PTEXT INTO GS_DATA-PERSK_PTEXT FROM T503T WHERE  SPRSL = '1'AND  PERSK = GS_DATA-PERSK."员工子组名称
    GS_DATA-ABKRS = P0001-ABKRS. "工资范围
    SELECT SINGLE ATEXT INTO GS_DATA-ABKRS_ATEXT FROM T549T WHERE SPRSL = '1'AND ABKRS = GS_DATA-ABKRS. .
    GS_DATA-KOSTL = P0001-KOSTL ."成本中心
    SELECT SINGLE KTEXT  INTO GS_DATA-KOSTL_KTEXT FROM CSKT WHERE SPRAS = '1' AND KOKRS = '1000' AND KOSTL =  P0001-KOSTL .
    GS_DATA-PLANS = P0001-PLANS."职位
    SELECT SINGLE PLSTX INTO GS_DATA-PLANS_PLSTX
      FROM T528T
      WHERE  SPRSL = '1'
      AND    OTYPE = 'S'
      AND    PLANS = GS_DATA-PLANS
      AND    ENDDA >= P_DATE
      AND    BEGDA <= P_DATE.
*      AND    ENDDA = '99991231'.
    "兼职职位 拼接到主职位之后
    LOOP  AT GT_HRP1001 WHERE OBJID = GS_DATA-PERNR
                             AND BEGDA <= P_DATE
                             AND ENDDA >= P_DATE.
      LOOP AT GT_HRP1000 WHERE OBJID = GT_HRP1001-SOBID
                               AND BEGDA <= P_DATE
                               AND ENDDA >= P_DATE.
        CONCATENATE GS_DATA-PLANS_PLSTX '兼' GT_HRP1000-STEXT INTO GS_DATA-PLANS_PLSTX .
        EXIT.
      ENDLOOP.
    ENDLOOP.

    DATA:STRU_TAB LIKE TABLE OF   QCAT_STRU WITH HEADER LINE.
    GS_DATA-OBJIDC4 = P0001-ORGEH."组织编号4.
    DATA:OBJID TYPE HRP1000-OBJID.
    OBJID = GS_DATA-OBJIDC4 .
    DATA:P_SHORT  TYPE SHORT_D.
    DATA:P_LONG  TYPE  STEXT .
    CALL FUNCTION 'HR_HCP_READ_OBJECT_TEXT'
      EXPORTING
        IM_PLVAR = '01'
        IM_OTYPE = 'O'
*       IM_VIEW_OBJID       =
*       IM_VIEW_KOKRS       =
        IM_OBJID = OBJID
    "   IM_ISTAT = ' '
        IM_BEGDA = P_DATE
        IM_ENDDA = P_DATE
      IMPORTING
        SHORT    = P_SHORT
        LONG     = P_LONG.
    IF P_SHORT NE '' .
      CASE P_SHORT  .
        WHEN 'A0'.
          GS_DATA-JTTEXT  = P_LONG .
        WHEN 'B0'.
          GS_DATA-KGTEXT = P_LONG.
        WHEN 'C1' .
          GS_DATA-C10RGEN  =  P_LONG.
        WHEN 'C2'.
          GS_DATA-C20RGEN  =  P_LONG.
        WHEN 'C3'.
          GS_DATA-C30RGEN  =  P_LONG.
        WHEN 'C4'.
          GS_DATA-C40RGEN  =  P_LONG.
      ENDCASE.

    ENDIF.

    " STRU_TAB 存储当前组织及以上所有组织编码
    CALL FUNCTION 'RHPH_STRUCTURE_READ'
      EXPORTING
        PLVAR             = '01'
        OTYPE             = 'O'
        OBJID             = OBJID
        WEGID             = 'A002'
        BEGDA             = P_DATE
        ENDDA             = P_DATE
*        BEGDA             = P0001-BEGDA
*        ENDDA             = P0001-ENDDA
        PUP_INFO          = 'X'
        WITH_STEXT        = 'X'
 "      TDEPTH            =
      TABLES
        STRU_TAB          = STRU_TAB
      EXCEPTIONS
        CATALOGUE_PROBLEM = 1
        ROOT_NOT_FOUND    = 2
        WEGID_NOT_FOUND   = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.

* IMPLEMENT SUITABLE ERROR HANDLING HERE
    ELSE.
      LOOP AT STRU_TAB.
        IF STRU_TAB-SHORT = 'C4' .
          GS_DATA-C40RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF STRU_TAB-SHORT = 'C3'.
          GS_DATA-C30RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF STRU_TAB-SHORT = 'C2'.
          GS_DATA-C20RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'C1'.
          GS_DATA-C10RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'B0'.
          GS_DATA-KGTEXT  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'A0'.
          GS_DATA-JTTEXT  =  STRU_TAB-STEXT.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  PID = GS_DATA-PERNR.
  LOOP AT P0002 .
    CLEAR JSN.
    GS_DATA-RUFNM = P0002-RUFNM ."曾用名
    CONDENSE GS_DATA-RUFNM NO-GAPS.
    GS_DATA-GBDAT = P0002-GBDAT."身份证出生日期
    GS_DATA-ZZSJRQ = P0002-ZZSJRQ."实际出生日期
    DATA:P1_MD TYPE N LENGTH 4.
    DATA:P2_MD TYPE N LENGTH 4.
    DATA:P1_M TYPE N  LENGTH 2.
    DATA:P2_M TYPE N  LENGTH 2.
    DATA: P1_D TYPE N LENGTH 2.
    DATA: P2_D TYPE N LENGTH 2.
    DATA:P_Y TYPE N LENGTH 2.
    DATA: P1_DATE TYPE SY-DATUM.
    IF GS_DATA-ZZSJRQ = '00000000'.
      "  P_DATE = SY-DATUM .
      P1_M =  SY-DATUM+4(2).
      P1_D = SY-DATUM+6(2).
      P2_M = GS_DATA-GBDAT+4(2).
      P2_D = GS_DATA-GBDAT+6(2).
      P2_MD = GS_DATA-GBDAT+4(4).
      DATA:P TYPE I VALUE 0.
      DO 6 TIMES .
        P1_DATE = SY-DATUM + P.
        P1_MD = P1_DATE+4(4).
        IF P1_MD = P2_MD .
          L_STR = P .
          CONCATENATE  L_STR '天' INTO GS_DATA-JSRDT. "距生日几天
          " GS_DATA-JSRDT =

          GS_DATA-ZSRTS = '提醒'. "生日提醒
          EXIT.
        ENDIF.
        P = P + 1.
      ENDDO.
    ELSE.
      P2_MD = GS_DATA-ZZSJRQ+4(4).
      " P_DATE = SY-DATUM  +  5.
      P1_M =  SY-DATUM+4(2).
      P1_D = SY-DATUM+6(2).
      P2_M = GS_DATA-ZZSJRQ+4(2).
      P2_D = GS_DATA-ZZSJRQ+6(2).
      DATA:P1 TYPE I VALUE 0.
      DO 6 TIMES .
        P1_DATE = SY-DATUM + P1.
        P1_MD = P1_DATE+4(4).
        IF P1_MD = P2_MD .
          L_STR = P1 .
          CONCATENATE  L_STR '天' INTO GS_DATA-JSRDT. "距生日几天
          GS_DATA-ZSRTS = '提醒'. "生日提醒
          EXIT.
        ENDIF.
        P1 = P1 + 1.
      ENDDO.

    ENDIF.
*  IF L_DAYS <= 5 AND L_DAYS >  0.
*  GS_DATA-ZSRTS = '提醒'. "生日提醒
*  ENDIF.

    CALL FUNCTION 'HR_AUPBS_AGE'
      EXPORTING
        PERNR            = PID
        BSDTE            = SY-DATUM
*       REACTION         = ' '
      IMPORTING
        VALUE            = T_NLVALUE
      EXCEPTIONS
        RECORD_NOT_FOUND = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
    ENDIF.
    T_NIAN = T_NLVALUE .
    GS_DATA-ZNL = T_NIAN+(2) ."年龄
    IF P0002-GESCH = '1' .
      GS_DATA-GESCH = '男'.  "
    ELSEIF  P0002-GESCH = '2' ."性别
      GS_DATA-GESCH = '女'.
    ENDIF.
    GS_DATA-ZZJGSF = P0002-ZZJGSF.""籍贯省份
    GS_DATA-ZZHKSF = P0002-ZZHKSF."户口省份
    "籍贯省份文本
    SELECT SINGLE BEZEI INTO GS_DATA-ZZJGSF_TXT FROM T005U WHERE SPRAS = '1' AND BLAND = P0002-ZZJGSF .
    "户口省份文本
    SELECT SINGLE BEZEI INTO GS_DATA-ZZHKSF_TXT FROM T005U WHERE SPRAS = '1' AND BLAND = P0002-ZZHKSF .
    GS_DATA-ZZHKXZ = P0002-ZZHKXZ."户口性质
    SELECT SINGLE DDTEXT INTO GS_DATA-ZZHKXZ_TXT FROM DD07T  "户口性质文本
    WHERE DOMNAME = 'ZDOHKXZ' AND VALPOS = GS_DATA-ZZHKXZ AND DDLANGUAGE = SY-LANGU.
    GS_DATA-RACKY = P0002-RACKY. "民族
    SELECT SINGLE LTEXT INTO GS_DATA-RACKY_LTEXT FROM  T505S  WHERE SPRSL = '1' AND RACKY = GS_DATA-RACKY."民族文本
    SELECT SINGLE FTEXT INTO GS_DATA-FAMST_FATXT FROM T502T WHERE SPRSL = '1' AND FAMST = P0002-FAMST."婚姻状况
    " GS_DATA-FATXT = P0002-FATXT.
    GS_DATA-ZZXZ = P0002-ZZXZ."星座
    SELECT SINGLE DDTEXT INTO GS_DATA-ZZXZ_TXT FROM DD07T  "星座文本
    WHERE DOMNAME = 'ZDOXZ' AND VALPOS = GS_DATA-ZZXZ AND DDLANGUAGE = SY-LANGU.
    GS_DATA-ZZXX = P0002-ZZXX ."血型
    SELECT SINGLE DDTEXT INTO GS_DATA-ZZXX_TXT FROM DD07T  "血型文本
    WHERE DOMNAME = 'ZDOXX' AND VALPOS = GS_DATA-ZZXX AND DDLANGUAGE = SY-LANGU.
    GS_DATA-NAME2 = P0002-NAME2."英文名
    GS_DATA-ZZZJXY = P0002-ZZZJXY."宗教信仰
  ENDLOOP.
  LOOP AT P0185 .
    IF P0185-ICTYP = '01'.
      GS_DATA-ICNUM = P0185-ICNUM. "身份证号码
      GS_DATA-USEFR = P0185-USEFR."身份证号有效期起始日
      GS_DATA-USETO = P0185-USETO."身份证号有效期截止日
    ENDIF.
    IF P0185-ICTYP = 'Z1'.
      GS_DATA-ICNUMZ1 = P0185-ICNUM ."外籍护照号码
    ENDIF.
    IF P0185-ICTYP = '02'.
      GS_DATA-ICNUM02 = P0185-ICNUM . "中国护照号码
    ENDIF.

    "  GS_DATA-ICNUMZ1 = P0185-ICNUMZ.
    " GS_DATA-ICNUM02 = P0185-ICNUMZ.

  ENDLOOP.
  LOOP AT P0534.
    GS_DATA-PCODE = P0534-PCODE."政治面貌
    SELECT SINGLE PTEXT INTO GS_DATA-PCODE_PTEXT FROM T7CN5R WHERE LANGU = '1' AND PCODE = GS_DATA-PCODE ."政治面貌文本
  ENDLOOP.
  LOOP AT P9006.
    IF P9006-ZZDZLX = '1'.
      GS_DATA-ZZXXDZ1 = P9006-ZZXXDZ. "户籍地址
    ENDIF.
    IF P9006-ZZDZLX = '2'.
      GS_DATA-ZZXXDZ2 = P9006-ZZXXDZ. "现居住地址
    ENDIF.
  ENDLOOP.
  LOOP AT P9022 .
    IF P9022-ZZBS = 'X'.
      GS_DATA-ZZXL = P9022-ZZXL. "最高学历
      SELECT SINGLE DDTEXT INTO GS_DATA-ZZXL_TXT  FROM DD07T  "最高学历文本
      WHERE DOMNAME     =  'ZDOXL'
      AND   VALPOS      = GS_DATA-ZZXL
      AND   DDLANGUAGE  = SY-LANGU.
      GS_DATA-ZZXW = P9022-ZZXW."最高学位
      SELECT SINGLE DDTEXT INTO GS_DATA-ZZXW_TXT  FROM DD07T  "最高学位文本
       WHERE DOMNAME     =  'ZDOXW'
       AND   VALPOS      = GS_DATA-ZZXW
      AND   DDLANGUAGE  = SY-LANGU.
      GS_DATA-ZZXW = P9022-ZZXW."最高学位
      GS_DATA-ZZXXMC = P9022-ZZXXMC."毕业学校
      GS_DATA-ZZZYMC1 = P9022-ZZZYMC1 ."专业1
      IF  P9022-ZZZYMC2 NE '' ..
        CONCATENATE P9022-ZZZYMC1 '/' P9022-ZZZYMC2 INTO GS_DATA-ZZZYMC1  ."专业
      ENDIF.

      "   GS_DATA-ZZZYMC1 = P9022-ZZZYMC1 + P9022-ZZZYMC2.
      GS_DATA-BEGDA = P9022-BEGDA."最高学历起始日期
      GS_DATA-ENDDA = P9022-ENDDA."毕业日期
    ENDIF.
  ENDLOOP.
  LOOP AT P0041.
    CASE P0041-DAR01 .
      WHEN 'D1' .
        GS_DATA-DATD1 = P0041-DAT01." 当前司龄起算日期（入职日期）
      WHEN 'D2'.
        GS_DATA-DATD2 = P0041-DAT01."累计司龄起算日期
      WHEN 'D3'.
        GS_DATA-DATD3 = P0041-DAT01."社会工龄起算日期
    ENDCASE.
    CASE P0041-DAR02 .
      WHEN 'D1' .
        GS_DATA-DATD1 = P0041-DAT02." 当前司龄起算日期（入职日期）
      WHEN 'D2'.
        GS_DATA-DATD2 = P0041-DAT02."累计司龄起算日期
      WHEN 'D3'.
        GS_DATA-DATD3 = P0041-DAT02."社会工龄起算日期
    ENDCASE.
    CASE P0041-DAR03 .
      WHEN 'D1' .
        GS_DATA-DATD1 = P0041-DAT03." 当前司龄起算日期（入职日期）
      WHEN 'D2'.
        GS_DATA-DATD2 = P0041-DAT03."累计司龄起算日期
      WHEN 'D3'.
        GS_DATA-DATD3 = P0041-DAT03."社会工龄起算日期
    ENDCASE.
    CASE P0041-DAR04 .
      WHEN 'D1' .
        GS_DATA-DATD1 = P0041-DAT04." 当前司龄起算日期（入职日期）
      WHEN 'D2'.
        GS_DATA-DATD2 = P0041-DAT04."累计司龄起算日期
      WHEN 'D3'.
        GS_DATA-DATD3 = P0041-DAT04."社会工龄起算日期
    ENDCASE.


  ENDLOOP.
  "DATA:MONTHS　TYPE P15_MONTHS.
  "DATA:L_MONTH TYPE N LENGTH 3.
  LOOP AT P0016.
    GS_DATA-CTTYP = P0016-CTTYP.
    SELECT SINGLE CTTXT INTO GS_DATA-CTTYP_CTTXT FROM T547S WHERE SPRSL = '1' AND CTTYP = P0016-CTTYP."合同类型文本
    GS_DATA-HTBEGDA = P0016-BEGDA."当前合同起始日期
    GS_DATA-CTEDT = P0016-CTEDT."当前合同结束日期
    IF P0016-CTTYP NE 'Z2'.
      CLEAR L_MONTH.
      CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
        EXPORTING
          BEGDA  = SY-DATUM
          ENDDA  = GS_DATA-CTEDT
        IMPORTING
          MONTHS = YC.
      IF YC NE '000'.
        GS_DATA-ZHTDQYF = YC.
        SHIFT GS_DATA-ZHTDQYF LEFT DELETING LEADING '0'.
        CONCATENATE GS_DATA-ZHTDQYF '月' INTO GS_DATA-ZHTDQYF .
      ENDIF.
      IF ( GS_DATA-CTEDT -  SY-DATUM ) <= 30  AND ( GS_DATA-CTEDT -  SY-DATUM )  >= 0.
        GS_DATA-ZHTDQTX = '提醒' .
      ENDIF.
    ENDIF.
    " GS_DATA-ZHTDQYF
    " GS_DATA-ZHTDQTX

  ENDLOOP.

  LOOP AT  P3211 .
    GS_DATA-ZZHTCS = P3211-ZZHTCS."自2008年后合同签订次数
    GS_DATA-ZZHTGS = P3211-ZZHTGS."合同公司主体
    "合同公司主体 文本
    SELECT SINGLE DDTEXT INTO GS_DATA-ZZHTGS_TXT  FROM DD07T  "合同公司主体文本
        WHERE DOMNAME     =  'ZDOHTGS'
        AND   VALPOS      = GS_DATA-ZZHTGS
    AND   DDLANGUAGE  = SY-LANGU.
    GS_DATA-ZZQSRQ = P3211-ZZQSRQ. "当前合同签署日期
    GS_DATA-ZZQSBZ = P3211-ZZQSBZ."当前合同签署备注
    GS_DATA-ZZSYQ = P3211-ZZSYQ."试用期截止日
    IF GS_DATA-ZZSYQ = '00000000'.
      GS_DATA-ZZSYQDQ =''.
    ELSE.
      CLEAR: L_DAYS ,L_MONTH ,L_YEARS.
      CALL FUNCTION 'HR_GET_TIME_BETWEEN_DATES'
        EXPORTING
          BEG_DATE       = SY-DATUM
          END_DATE       = GS_DATA-ZZSYQ
        IMPORTING
          DAYS           = L_DAYS
          MONTHS         = L_MONTH
          YEARS          = L_YEARS
        EXCEPTIONS
          INVALID_PERIOD = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
        CALL FUNCTION 'HR_GET_TIME_BETWEEN_DATES'
          EXPORTING
            BEG_DATE       = GS_DATA-ZZSYQ
            END_DATE       = SY-DATUM
          IMPORTING
            DAYS           = L_DAYS
            MONTHS         = L_MONTH
            YEARS          = L_YEARS
          EXCEPTIONS
            INVALID_PERIOD = 1
            OTHERS         = 2.
        IF L_DAYS NE 0 OR L_MONTH NE 0  OR  L_YEARS NE '0'.
          READ TABLE P0000 WITH KEY PERNR = GS_DATA-PERNR MASSN = 'Z3'.
          IF SY-SUBRC = 0 .
            CLEAR  GS_DATA-ZZSYQDQ ."若有Z3转正事件就情空
          ELSE.
            GS_DATA-ZZZTX = '提醒'."转正提醒
            IF L_YEARS NE '0'.
              L_STR = L_YEARS  .
              CONCATENATE  '超' L_STR '年' INTO  GS_DATA-ZZSYQDQ  .
            ENDIF.
            IF L_MONTH NE '0'.
              L_STR = L_MONTH  .
              IF   GS_DATA-ZZSYQDQ = ''.
                CONCATENATE  '超' L_STR '月' INTO  GS_DATA-ZZSYQDQ  .
              ELSE.
                CONCATENATE  GS_DATA-ZZSYQDQ L_STR '月' INTO  GS_DATA-ZZSYQDQ  .
              ENDIF.

            ENDIF.
            IF L_DAYS NE '0'.

              L_STR = L_DAYS.
              IF   GS_DATA-ZZSYQDQ = ''.
                CONCATENATE  '超' L_STR '天' INTO  GS_DATA-ZZSYQDQ  .
              ELSE.
                CONCATENATE  GS_DATA-ZZSYQDQ L_STR '天' INTO  GS_DATA-ZZSYQDQ  .
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ELSE.
        IF L_DAYS > 0 OR L_MONTH > 0 OR  L_YEARS NE '0'. .
          IF (  GS_DATA-ZZSYQ - SY-DATUM ) <= 15  AND  ( GS_DATA-ZZSYQ - SY-DATUM >= 0 ).
            GS_DATA-ZZZTX = '提醒'. "转正提醒
          ENDIF.
        ENDIF.
        IF L_YEARS NE '0'.
          L_STR = L_YEARS   .
          CONCATENATE L_STR '年' INTO  GS_DATA-ZZSYQDQ .
        ENDIF.
        IF L_MONTH NE '0'.
          L_STR = L_MONTH  .
          CONCATENATE L_STR '月' INTO  GS_DATA-ZZSYQDQ .
        ENDIF.
        IF L_DAYS NE '0'.
          L_STR = L_DAYS.
          CONCATENATE  GS_DATA-ZZSYQDQ L_STR '天' INTO  GS_DATA-ZZSYQDQ  .
        ENDIF.

      ENDIF.
    ENDIF.
    "距满月考核到期天数
    DATA:DATD11 TYPE SY-DATUM."存储当前司龄日期加一月
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = GS_DATA-DATD1
        DAYS      = '00'
        MONTHS    = '01'
        SIGNUM    = '+'
        YEARS     = '00'
      IMPORTING
        CALC_DATE = DATD11.
    CLEAR: L_DAYS , L_MONTH ,L_YEARS.
    CLEAR L_STR.
    CALL FUNCTION 'HR_GET_TIME_BETWEEN_DATES'
      EXPORTING
        BEG_DATE       = SY-DATUM
        END_DATE       = DATD11
      IMPORTING
        DAYS           = L_DAYS
        MONTHS         = L_MONTH
        YEARS          = L_YEARS
      EXCEPTIONS
        INVALID_PERIOD = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here

    ELSE.
      IF L_YEARS NE 0 OR L_MONTH NE 0 OR L_DAYS NE 0 .
        IF L_YEARS NE 0 .
          L_STR = L_YEARS .
          CONCATENATE L_STR '年' INTO  GS_DATA-ZZMYSTS .
        ENDIF.
        IF L_MONTH NE 0 .
          L_STR = L_MONTH.
          CONCATENATE GS_DATA-ZZMYSTS L_STR '月' INTO GS_DATA-ZZMYSTS.
        ENDIF.
        IF L_DAYS  NE 0 .
          L_STR = L_DAYS.
          CONCATENATE GS_DATA-ZZMYSTS L_STR '天' INTO GS_DATA-ZZMYSTS.
        ENDIF.
      ENDIF.
      IF L_YEARS EQ 0 AND L_MONTH EQ 0 AND ( L_DAYS <= 5 AND L_DAYS > 0 ) .
        GS_DATA-ZZMYTX = '提醒'. "满月提醒
      ENDIF.
    ENDIF.

    "GS_DATA-ZZMYSTS
    "GS_DATA-ZZMYTX
    " GS_DATA-ZZSYQDQ
    "GS_DATA-ZZSYQDQ
    "
    "IF GS_DATA-ZZZTX =
    "GS_DATA-ZZMYTX
  ENDLOOP.
  LOOP AT P9105.
    GS_DATA-ZZGSSJHM = P9105-ZZGSSJHM. "公司手机号
    GS_DATA-ZZGSDH = P9105-ZZGSDH. "公司短号
    GS_DATA-ZZGRSJHY = P9105-ZZGRSJHY. "手机号码一
    GS_DATA-ZZGRJHE = P9105-ZZGRSJHE. "手机号码二
    GS_DATA-ZZZJH = P9105-ZZZJH."家庭座机号
    GS_DATA-ZZQQ = P9105-ZZQQ."QQ
    GS_DATA-ZZWX = P9105-ZZWX."微信号
    GS_DATA-ZZGSYX = P9105-ZZGSYX."公司邮箱
    GS_DATA-ZZGRYX = P9105-ZZGRYX."个人邮箱
    GS_DATA-ZZDYXM = P9105-ZZDYXM."第一紧急联系人姓名
    GS_DATA-ZZDYGX = P9105-ZZDYGX."第一紧急联系人与本人关系
    GS_DATA-ZZDYDZ = P9105-ZZDYDZ."第一紧急联系人地址
    GS_DATA-ZZDYHM = P9105-ZZDYHM."第一紧急联系人手机号码

  ENDLOOP.
  LOOP AT P0031.
    GS_DATA-RFPNR = P0031-RFP01 ."关联工号
  ENDLOOP.
  LOOP AT P9379 .
    GS_DATA-ZZQQSX = P9379-ZZQQSX ."股票期权时限
  ENDLOOP.

*&--代码添加 BY HANDYBY 09.06.2017 18:47:50  BEGIN
  DATA:BEGIN OF LS_DD07T,
         DOMNAME    TYPE DD07T-DOMNAME,
         DDLANGUAGE TYPE DD07T-DDLANGUAGE,
         AS4LOCAL   TYPE DD07T-AS4LOCAL,
         VALPOS     TYPE DD07T-VALPOS,
         AS4VERS    TYPE DD07T-AS4VERS,
         DDTEXT     TYPE DD07T-DDTEXT,
       END OF LS_DD07T .
  DATA LT_DD07T LIKE TABLE OF LS_DD07T .

  SELECT DOMNAME
         DDLANGUAGE
         AS4LOCAL
         VALPOS
         AS4VERS
         DDTEXT
    INTO CORRESPONDING FIELDS OF TABLE LT_DD07T
    FROM DD07T
   WHERE DOMNAME = 'ZDOHRCBGS'
     AND DDLANGUAGE = SY-LANGU
     AND AS4LOCAL = 'A'.
  SORT LT_DD07T BY VALPOS .
*&--代码添加 BY HANDYBY 09.06.2017 18:47:50  END

*&--代码添加 BY HANDYBY 07.06.2017 20:50:27  BEGIN
  LOOP AT P9029.
    READ TABLE LT_DD07T INTO LS_DD07T WITH KEY VALPOS = P9029-ZZBZ0 BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GS_DATA-DDTEXT = LS_DD07T-DDTEXT .
      CLEAR LS_DD07T .
*      EXIT .
    ENDIF.
*    GS_DATA-ZZBZ0 = P9029-ZZBZ0 ."工资成本归属单位
    GS_DATA-ZZBZ1 = P9029-ZZBZ1 ."备注2
    GS_DATA-ZZBZ2 = P9029-ZZBZ2 ."备注3
    GS_DATA-ZZBZ3 = P9029-ZZBZ3 ."备注4
  ENDLOOP.
*&--代码添加 BY HANDYBY 07.06.2017 20:50:27  END

  IF  GS_DATA-ZZXH NE '0000000000'.
    APPEND GS_DATA TO GT_DATA.
  ENDIF.
ENDFORM.
