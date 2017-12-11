*&---------------------------------------------------------------------*
*& REPORT  ZMM005A
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& DATE CREATED : 2015/2/6                                             *
*& CREATED BY   : 汉得-唐博                                            *
*& DESCRIPTION  :采购订单打印                                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&DATE         DEVELOPER           REQNO       DESCRIPTIONS            *
*& ==========  ==================  ==========  ========================*
*& 2017-01-22  IT02&魏云           ED1K905221  付款条件/方式更改为采购订单取值
*& 2017-05-16  IT02&魏云           ED1K905469  增加外币订单打印
*&---------------------------------------------------------------------*
REPORT ZMM005A.

TABLES: EKKO,EKPO.

DATA: CONTROL    TYPE SSFCTRLOP.
DATA: JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA: G_NAME TYPE RS38L_FNAM.
DATA:L_FORMNAME TYPE TDSFNAME VALUE 'ZSFMM005A'.
DATA: GT_ZMM005 TYPE TABLE OF ZMM005A WITH HEADER LINE.

DATA GT_HEAD TYPE ZMM005_1 WITH HEADER LINE.
DATA GT_FOOT TYPE ZMM005_1 WITH HEADER LINE.
DATA:T_KSSKAUSP LIKE TABLE OF  KSSKAUSP WITH HEADER LINE."订单号
DATA X_STYLE1 TYPE C VALUE 'X'.
DATA X_STYLE2 TYPE C.
DATA X_STYLE3 TYPE C.
DATA X_STYLE4 TYPE C.
DATA X_STYLE5 TYPE C.
DATA X_STYLE6 TYPE C.
DATA X_STYLE7 TYPE C.
DATA X_STYLE8 TYPE C.
DATA G_STYLE TYPE CHAR10.
DATA G_LOGO TYPE CHAR20.
DATA OK_CODE TYPE CHAR20.

DATA:GT_ZMD14 TYPE TABLE OF ZMD14_ZHRZ WITH HEADER LINE,
     GS_ZMD14 TYPE ZMD14_ZHRZ.


DATA: BEGIN OF GT_ALV OCCURS 0,
        CHECKBOX TYPE C.
        INCLUDE STRUCTURE ZMM005A.
DATA:HTJJ   TYPE EKPO-NETWR,   "合同净价
     BZJG   TYPE MBEW-STPRS,   "标准价格
     BZJGDW TYPE MBEW-PEINH,    "标准价格单位
     ZJCGJJ TYPE EKPO-NETWR,    "最近采购净价
     JJBL   TYPE EKPO-NETWR,    "降价比例
     JJBL_% TYPE STRING,
     BHHTJE TYPE TSLVT12,    "本行合同金额
     XMMC   TYPE STRING,        "项目名称
     PPV    TYPE MBEW-STPRS,    "PPV
     GSMNG  TYPE GSMNG,        "计划数量
     SQSL   TYPE EKPO-MENGE,   "申请数量
     CGCSG  TYPE EBAN-MENGE,   "采购超申购数量
     CJHSL  TYPE EKPO-MENGE,   "超计划数量
     CJHJE  TYPE  TSLVT12,  "超计划金额
     CJHL   TYPE  KONV-KBETR,  "超级率
     CJHL_1 TYPE  STRING,

     AEDAT2 TYPE DATS,
     END OF GT_ALV.

TYPES:BEGIN OF TY_CGSQ,
        BANFN TYPE BANFN,
        BNFPO TYPE BNFPO,
      END OF TY_CGSQ .



TYPES:BEGIN OF TY_CGMX,
        EBELN TYPE EKKO-EBELN,
        EBELP TYPE EKPO-EBELP,
      END OF TY_CGMX.



DATA:GT_CGMX TYPE TABLE OF TY_CGMX,
     GS_CGMX TYPE TY_CGMX.

DATA:GT_CGSQ TYPE TABLE OF TY_CGSQ,
     GS_CGSQ TYPE TY_CGSQ.

DATA:GT_EBAN TYPE TABLE OF EBAN,
     GS_EBAN TYPE EBAN.


DATA:GT_MBEW TYPE TABLE OF MBEW,
     GS_MBEW TYPE MBEW.

DATA:GT_EKPO TYPE TABLE OF EKPO,
     GS_EKPO TYPE EKPO.

DATA:GT_LFA1 TYPE TABLE OF LFA1,
     GS_LFA1 TYPE LFA1.

DATA:GT_ALV_SEL LIKE TABLE OF   GT_ALV .

DATA:G_HWZLYQ(100) TYPE C, "货物质量要求
     G_HTHCDZ(100) TYPE C , "合同回传地址
     G_FPHCDZ(100) TYPE C , "发票回传地址
     G_ZBQ(20)     TYPE C.  "质保期


***---------------------------------------------------------------
* 定制控制 编辑长文本对象定义
DATA:CONTAINER_1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     EDITOR_1    TYPE REF TO CL_GUI_TEXTEDIT,
     CONTAINER_2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     EDITOR_2    TYPE REF TO CL_GUI_TEXTEDIT,
     CONTAINER_3 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     EDITOR_3    TYPE REF TO CL_GUI_TEXTEDIT.
DATA: INIT_1,
      INIT_2,
      INIT_3,
      W_YSFS           TYPE C LENGTH 50,  "运输方式
      W_THFS           TYPE C LENGTH 50,  "提货方式
      W_HWZLYQ(100)    TYPE C OCCURS 0,  "货物质量要求
      HWZLYQ_LINE(100) TYPE C,
      W_HTHCDZ(100)    TYPE C OCCURS 0,   "合同回传地址
      HTHCDZ_LINE(100) TYPE C,
      W_FPHCDZ(100)    TYPE C OCCURS 0,  "发票回传地址
      FPHCDZ_LINE(100) TYPE C,
      W_ZBQ            TYPE C LENGTH 20. "质保期

*&--代码添加 BY HANDYBY 28.07.2017 14:43:00  BEGIN
DATA G_MSG TYPE STRING .
*&--代码添加 BY HANDYBY 28.07.2017 14:43:00  END

SELECT-OPTIONS: S_EBELN FOR EKKO-EBELN."采购订单
SELECT-OPTIONS: S_BSART FOR EKKO-BSART."采购订单类型
SELECT-OPTIONS: S_AEDAT FOR EKKO-AEDAT."采购订单日期
SELECT-OPTIONS: S_BUKRS FOR EKKO-BUKRS."公司代码
SELECT-OPTIONS: S_EKORG FOR EKKO-EKORG."采购组织
SELECT-OPTIONS: S_EKGRP FOR EKKO-EKGRP."采购组

START-OF-SELECTION.

*&--代码添加 BY HANDYBY 28.07.2017 14:38:41  BEGIN
  LOOP AT S_EKORG.
    AUTHORITY-CHECK OBJECT 'Z_EKORG'
            ID 'EKORG' FIELD S_EKORG-LOW .
    IF SY-SUBRC NE 0 .
      CONCATENATE '你没权限看采购组织' S_EKORG-LOW '的数据' INTO G_MSG .
      MESSAGE G_MSG TYPE 'E' .
    ENDIF.
  ENDLOOP.
*&--代码添加 BY HANDYBY 28.07.2017 14:38:41  END

  CLEAR GT_ZMM005[].
*  SELECT * FROM ZMM005A INTO CORRESPONDING FIELDS OF TABLE GT_ZMM005
*    WHERE EBELN IN S_EBELN
*    AND BSART IN S_BSART
*    AND AEDAT IN S_AEDAT
*    AND BUKRS IN S_BUKRS
*    AND EKORG IN S_EKORG
*    AND EKGRP IN S_EKGRP
*    .
  SELECT
  A~EBELN
  A~EBELP
  A~MATNR
  A~MENGE
  A~MEINS
  A~NETPR
  A~PEINH
  A~BANFN
  A~BNFPO
  A~WERKS
  A~TXZ01
  B~BUKRS
  B~BSART
  B~AEDAT
  B~LIFNR
  B~EKORG
  B~EKGRP
  B~WKURS
  B~KNUMV
  B~WAERS
  B~ZTERM AS ZTERM_1
  C~NAME1
  C~TELF1
  C~TELFX
  D~STREET
  E~BUTXT
  F~ZTERM
  FROM EKPO AS A
  INNER JOIN EKKO AS B ON A~EBELN = B~EBELN
  INNER JOIN LFA1 AS C ON C~LIFNR = B~LIFNR
  INNER JOIN T001 AS E ON E~BUKRS = B~BUKRS
  LEFT JOIN LFM1 AS F ON F~LIFNR = B~LIFNR AND F~EKORG = B~EKORG
  LEFT JOIN ADRC AS D ON D~ADDRNUMBER = C~ADRNR
  INTO CORRESPONDING FIELDS OF TABLE GT_ZMM005
  WHERE A~LOEKZ EQ SPACE
  AND A~EBELN IN S_EBELN
  AND B~BSART IN S_BSART
  AND B~AEDAT IN S_AEDAT
  AND B~BUKRS IN S_BUKRS
  AND B~EKORG IN S_EKORG
  AND B~EKGRP IN S_EKGRP
  .
  SORT GT_ZMM005 BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM GT_ZMM005 COMPARING EBELN EBELP.

  IF GT_ZMM005[] IS NOT INITIAL .

    MOVE-CORRESPONDING GT_ZMM005[] TO GT_CGSQ .
    DELETE GT_CGSQ WHERE BANFN IS INITIAL .
    SORT GT_CGSQ BY BANFN BNFPO .
    DELETE ADJACENT DUPLICATES FROM GT_CGSQ COMPARING BANFN .
    SELECT * INTO TABLE GT_MBEW FROM MBEW WHERE BWKEY IN S_BUKRS .
    IF GT_CGSQ IS NOT INITIAL.
      SELECT * INTO TABLE GT_EBAN FROM EBAN
         FOR ALL ENTRIES IN GT_CGSQ
         WHERE BANFN = GT_CGSQ-BANFN
         AND   BNFPO = GT_CGSQ-BNFPO .
      SORT GT_EBAN BY BANFN BNFPO .

      "查询存储ZMD14_ZHRZ :MD14： 计划订单转采购申请数量
      SELECT * INTO TABLE GT_ZMD14
        FROM  ZMD14_ZHRZ
        FOR  ALL ENTRIES IN GT_CGSQ
        WHERE BANFN = GT_CGSQ-BANFN
        AND   BNFPO = GT_CGSQ-BNFPO .
      SORT GT_ZMD14 BY BANFN BNFPO .

    ENDIF.

  ENDIF.
  SORT GT_MBEW BY MATNR BWKEY .
  DATA W_EKPO TYPE EKPO.
  DATA W_EKKO TYPE EKKO.
  DATA T_FTAXP TYPE TABLE OF FTAXP WITH HEADER LINE.
  DATA L_TDNAME TYPE THEAD-TDNAME.
  DATA L_BZ2 TYPE CHAR200 ."150624 IT02 ADD
  LOOP AT GT_ZMM005.
    CLEAR L_BZ2."150624 IT02 ADD
    SELECT SINGLE * FROM EKPO INTO CORRESPONDING FIELDS OF W_EKPO
      WHERE EBELN EQ GT_ZMM005-EBELN
      AND EBELP EQ GT_ZMM005-EBELP
      AND LOEKZ EQ SPACE.
    IF SY-SUBRC NE 0.
      DELETE GT_ZMM005.
    ELSE.

      GT_ZMM005-MAKTX = W_EKPO-TXZ01."物料描述取PO行文本
      SELECT SINGLE * FROM EKKO INTO CORRESPONDING FIELDS OF W_EKKO
        WHERE EBELN EQ GT_ZMM005-EBELN.

      IF SY-SUBRC EQ 0.
        "获取价格金额
        SELECT SINGLE KBETR KWERT FROM KONV
          INTO (GT_ZMM005-KBETR, GT_ZMM005-KWERT)
          WHERE KNUMV EQ W_EKKO-KNUMV
          AND KPOSN EQ W_EKPO-EBELP
          AND KSCHL IN ('PB00','PBXX')
          AND KINAK EQ SPACE "激活状态
          .
        "获取交货日期
        SELECT SINGLE EINDT FROM EKET INTO GT_ZMM005-EINDT
          WHERE EBELN EQ W_EKPO-EBELN
          AND EBELP EQ W_EKPO-EBELP
          AND ETENR EQ '0001'.

        DATA:
          JYEAR  TYPE C LENGTH 4,
          JMONTH TYPE I,
          JDAY   TYPE I.
        CLEAR JYEAR.
        CLEAR JMONTH.
        CLEAR JDAY.
        JYEAR = GT_ZMM005-EINDT+0(4).
        JMONTH = GT_ZMM005-EINDT+4(2).
        JDAY = GT_ZMM005-EINDT+6(2).

        IF W_EKKO-LANDS IS NOT INITIAL.
          CLEAR T_FTAXP[].
          CALL FUNCTION 'GET_TAX_PERCENTAGE'
            EXPORTING
              ALAND   = W_EKKO-LANDS
              DATAB   = SY-DATUM
              MWSKZ   = W_EKPO-MWSKZ
              TXJCD   = W_EKPO-TXJCD
*             EXPORT  = ' '
            TABLES
              T_FTAXP = T_FTAXP[].
          IF T_FTAXP[] IS NOT INITIAL.
            READ TABLE T_FTAXP INDEX 1.
            GT_ZMM005-KBETR_TAX = T_FTAXP-KBETR / 10.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR L_TDNAME.
      CONCATENATE GT_ZMM005-EBELN GT_ZMM005-EBELP INTO L_TDNAME.
      "付款条件
*      SELECT SINGLE VTEXT FROM TVZBT INTO GT_ZMM005-ZTERM_TEXT WHERE ZTERM EQ GT_ZMM005-ZTERM AND SPRAS EQ '1'.
*      SELECT SINGLE VTEXT FROM TVZBT INTO GT_ZMM005-ZTERM_TEXT_EN WHERE ZTERM EQ GT_ZMM005-ZTERM AND SPRAS EQ 'E'.
*      SELECT SINGLE TEXT1 FROM T052U INTO GT_ZMM005-ZTERM_TEXT WHERE ZTERM EQ GT_ZMM005-ZTERM AND SPRAS EQ '1'.
*      SELECT SINGLE TEXT1 FROM T052U INTO GT_ZMM005-ZTERM_TEXT_EN WHERE ZTERM EQ GT_ZMM005-ZTERM AND SPRAS EQ 'E'.
      "更改：原由供应商的付款条件更改为采购订单的付款条件 IT02 20170122
      SELECT SINGLE TEXT1 FROM T052U INTO GT_ZMM005-ZTERM_TEXT WHERE ZTERM EQ GT_ZMM005-ZTERM_1 AND SPRAS EQ '1'.
      SELECT SINGLE TEXT1 FROM T052U INTO GT_ZMM005-ZTERM_TEXT_EN WHERE ZTERM EQ GT_ZMM005-ZTERM_1 AND SPRAS EQ 'E'.
      "资产号
      SELECT SINGLE ANLN1 FROM EKKN INTO GT_ZMM005-ANLN1 WHERE EBELN = GT_ZMM005-EBELN AND EBELP = GT_ZMM005-EBELP AND ZEKKN = '01'.
      "抬头文本
      PERFORM READ_TEXT USING 'F03' GT_ZMM005-EBELN 'EKKO' '1' CHANGING GT_ZMM005-F03_H1.
      PERFORM READ_TEXT USING 'F03' GT_ZMM005-EBELN 'EKKO' '2' CHANGING GT_ZMM005-F03_H2.
      PERFORM READ_TEXT USING 'F04' GT_ZMM005-EBELN 'EKKO' '1' CHANGING GT_ZMM005-F04_H1.
      PERFORM READ_TEXT USING 'F04' GT_ZMM005-EBELN 'EKKO' '2' CHANGING GT_ZMM005-F04_H2.
      PERFORM READ_TEXT USING 'F00' GT_ZMM005-EBELN 'EKKO' '0' CHANGING GT_ZMM005-BZ_H."备注
      "行项目文本
      PERFORM READ_TEXT USING 'F02' L_TDNAME 'EKPO' '1' CHANGING GT_ZMM005-YCD."原产地
      PERFORM READ_TEXT USING 'F01' L_TDNAME 'EKPO' '0' CHANGING GT_ZMM005-BZ."备注
      IF GT_ZMM005-BZ NE '' .
        CONCATENATE '备注：' GT_ZMM005-BZ INTO GT_ZMM005-BZ.   "IT02 150624
      ENDIF.
      CLEAR :L_BZ2.  "IT02 150624
      PERFORM READ_TEXT USING 'F03' L_TDNAME 'EKPO' '0' CHANGING L_BZ2. "采购申请传输文本   "IT02 150624
      IF L_BZ2 NE '' .  "IT02 150624
        IF  GT_ZMM005-BZ NE '' .
          CONCATENATE GT_ZMM005-BZ '；' '采购申请传送文本：' L_BZ2 INTO GT_ZMM005-BZ.  "IT02 150624
        ELSE.
          CONCATENATE '采购申请传送文本：' L_BZ2 INTO GT_ZMM005-BZ.  "IT02 150624
        ENDIF.
      ENDIF.  "IT02 150624
      WRITE GT_ZMM005-AEDAT TO GT_ZMM005-AEDAT_TEXT.
      MODIFY GT_ZMM005.
    ENDIF.
  ENDLOOP.
  IF GT_ZMM005[] IS INITIAL.
    MESSAGE S000(ZMM01) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
*   没有符合条件的记录。
  ENDIF.

  DATA IS_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  CLEAR: IS_LAYOUT,IT_FIELDCAT[].
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  IS_LAYOUT-BOX_FIELDNAME = 'CHECKBOX'.

  PERFORM APPEND_FIELDCAT USING:
    'BSART' '采购订单类型' '' '' '',
    'EBELN' '采购订单' '' '' '',
    'EBELP' '项目' '' '' '',
    'MATNR' '物料编码' 'MARA' 'MATNR' '',
    'MAKTX' '物料描述' '' '' '',
   " 'AMOUNT' '数量' '' '' '',
    'MENGE' '数量' '' '' '',
    'LIFNR' '供应商' '' '' '',
    'NAME1' '供应商名称' '' '' '',
    'EKORG' '采购组织' '' '' '',
    'EKGRP' '采购组' '' '' '',
    'AEDAT2' '采购订单日期' '' '' '',
    'EINDT' '交货日期' '' '' '',
    'HTJJ'  '合同净价' '' '' '' ,
    'PEINH' '合同价格单位' '' '' '',
    'BZJG'  '标准价格'   '' '' '',
    'BZJGDW' '标准价格单位'  '' '' '' ,
    'ZJCGJJ' '最近采购净价'  '' '' '' ,
    'JJBL_%' '降价比例' '' '' '' ,
    'BHHTJE' '本行合同金额' '' '' '' ,
    'XMMC'   '项目名称' '' '' '' ,
    'ZTERM_1' '付款条件' '' '' '' ,
   'PPV'     'PPV' '' '' '' ,
   'GSMNG'   '计划数量' '' '' '' ,
   'SQSL'    '申请数量' '' '' '',
   'CGCSG'   '采购超申购数量'  '' '' '' ,
   'CJHSL'    '超计划数量'  '' '' '',
   'CJHJE'    '超计划金额'  '' '' '',
   'CJHL_1'    '超计划率' '' '' '' .


  LOOP AT GT_ZMM005.
    MOVE-CORRESPONDING GT_ZMM005 TO GT_ALV.
    GT_ALV-AEDAT2 = GT_ALV-AEDAT.
*-------------交货日期与采购订单日期相减-----------
    DATA:
          DAYS TYPE I.

    CLEAR DAYS.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        I_DATE_FROM = GT_ALV-AEDAT2
*       I_KEY_DAY_FROM       =
        I_DATE_TO   = GT_ZMM005-EINDT
*       I_KEY_DAY_TO         =
*       I_FLG_SEPARATE       = ' '
      IMPORTING
        E_DAYS      = DAYS.   "工期
*         E_MONTHS             = MONTHS
*         E_YEARS              = YEARS.

    "申请数量
    READ TABLE GT_EBAN INTO GS_EBAN WITH KEY BANFN = GT_ALV-BANFN
                                              BNFPO = GT_ALV-BNFPO
                                              BINARY SEARCH .
    IF SY-SUBRC EQ 0.
      GT_ALV-SQSL = GS_EBAN-MENGE .

    ENDIF.

    "计划数量
    READ TABLE GT_ZMD14 INTO GS_ZMD14 WITH KEY BANFN = GT_ALV-BANFN
                                              BNFPO = GT_ALV-BNFPO
                                              BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
      GT_ALV-GSMNG = GS_ZMD14-GSMNG .

    ENDIF.
    "合同净价
    GT_ALV-HTJJ = GT_ZMM005-NETPR.
    "标准价格

    IF GT_ZMM005-MATNR IS NOT INITIAL.

      READ TABLE GT_MBEW INTO GS_MBEW WITH KEY MATNR = GT_ZMM005-MATNR
                                               BWKEY = GT_ZMM005-BUKRS
                                   BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GT_ALV-BZJG = GS_MBEW-STPRS .
        GT_ALV-BZJGDW = GS_MBEW-PEINH .
      ENDIF.

      REFRESH:GT_EKPO .
      SELECT * INTO TABLE GT_EKPO
        FROM EKPO
        WHERE  WERKS = GT_ZMM005-WERKS
        AND    MATNR = GT_ZMM005-MATNR
        AND    LOEKZ NE 'L'
        AND    PRDAT < GT_ZMM005-PRDAT .

      SORT GT_EKPO BY MATNR ASCENDING WERKS ASCENDING PRDAT DESCENDING .

      DELETE ADJACENT DUPLICATES FROM GT_EKPO COMPARING MATNR WERKS .
      "最近采购净价
      READ TABLE GT_EKPO INTO GS_EKPO WITH KEY MATNR = GT_ZMM005-MATNR
                                               WERKS = GT_ZMM005-WERKS
                                               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GT_ALV-ZJCGJJ = GS_EKPO-NETPR / GS_EKPO-PEINH * GT_ALV-PEINH .
      ELSE.
        GT_ALV-ZJCGJJ = GT_ZMM005-NETPR / GT_ALV-PEINH * GT_ALV-PEINH ..
      ENDIF.

    ELSE.
      REFRESH:GT_EKPO .
      SELECT  * INTO TABLE GT_EKPO
    FROM EKPO
    WHERE WERKS = GT_ZMM005-WERKS
     AND  TXZ01 = GT_ZMM005-TXZ01
     AND  LOEKZ NE 'L'
     AND  PRDAT < GT_ZMM005-PRDAT
    .
      SORT GT_EKPO BY TXZ01 ASCENDING WERKS ASCENDING PRDAT DESCENDING .

      DELETE ADJACENT DUPLICATES FROM GT_EKPO COMPARING TXZ01 WERKS .
      "最近采购净价
      READ TABLE GT_EKPO INTO GS_EKPO WITH KEY TXZ01 = GT_ALV-TXZ01
                                               WERKS = GT_ALV-WERKS
                                               BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
        GT_ALV-ZJCGJJ = GS_EKPO-NETPR / GS_EKPO-PEINH * GT_ALV-PEINH .
      ELSE.
        GT_ALV-ZJCGJJ = GT_ZMM005-NETPR / GT_ALV-PEINH * GT_ALV-PEINH .
      ENDIF.
    ENDIF.
    IF GT_ALV-HTJJ NE 0 .
      GT_ALV-JJBL = ( GT_ALV-HTJJ - GT_ALV-ZJCGJJ )  /  GT_ALV-HTJJ  * 100 .
      GT_ALV-JJBL_% = GT_ALV-JJBL .
      CONCATENATE GT_ALV-JJBL_% '%' INTO GT_ALV-JJBL_%.
    ENDIF.
    "本行合同金额
    GT_ALV-BHHTJE = GT_ALV-KBETR / GT_ALV-PEINH * GT_ALV-MENGE .
    CONCATENATE GT_ALV-EBELN GT_ALV-EBELP INTO L_TDNAME.
    "项目名称
    PERFORM READ_TEXT USING 'F01' L_TDNAME 'EKPO' '0' CHANGING GT_ALV-XMMC."备注
    "PPV
    GT_ALV-PPV = GT_ALV-HTJJ  -  GT_ALV-BZJG .
    "采购超申购数量
    GT_ALV-CGCSG = GT_ALV-MENGE - GT_ALV-SQSL .
    "超计划数量
    GT_ALV-CJHSL = GT_ALV-SQSL  - GT_ALV-GSMNG .
    IF GT_ALV-CJHSL < 0 .
      GT_ALV-CJHSL = 0 .
    ENDIF.
    "超计划金额
    GT_ALV-CJHJE =  GT_ALV-HTJJ *  GT_ALV-CJHSL .

    "超计划率
    IF GT_ALV-GSMNG NE 0 .
      GT_ALV-CJHL = ( GT_ALV-CJHSL /  GT_ALV-GSMNG ) * 100 .

    ENDIF.

    GT_ALV-CJHL_1 = GT_ALV-CJHL .
    CONCATENATE GT_ALV-CJHL_1 '%' INTO GT_ALV-CJHL_1 .
    APPEND GT_ALV.
    CLEAR GT_ALV.
  ENDLOOP.

  CLEAR GT_ZMM005[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = IS_LAYOUT
      IT_FIELDCAT              = IT_FIELDCAT[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_ALV[]
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
  ENDIF.
*&---------------------------------------------------------------------*
*&      FORM  APPEND_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->STYLE   TEXT
*      -->TEXT01   TEXT
*      -->TEXT02   TEXT
*      -->TEXT03   TEXT
*      -->TEXT04   TEXT
*      -->TEXT05   TEXT
*      -->TEXT06   TEXT
*      -->TEXT07   TEXT
*      -->TEXT08   TEXT
*      -->TEXT09   TEXT
*      -->TEXT10   TEXT
*----------------------------------------------------------------------*
FORM APPEND_HEAD  USING    VALUE(STYLE)
                           VALUE(TEXT01)
                           VALUE(TEXT02)
                           VALUE(TEXT03)
                           VALUE(TEXT04)
                           VALUE(TEXT05)
                           VALUE(TEXT06)
                           VALUE(TEXT07)
                           VALUE(TEXT08)
                           VALUE(TEXT09)
                           VALUE(TEXT10).
  GT_HEAD-STYLE = STYLE.
  GT_HEAD-TEXT01 = TEXT01.
  GT_HEAD-TEXT02 = TEXT02.
  GT_HEAD-TEXT03 = TEXT03.
  GT_HEAD-TEXT04 = TEXT04.
  GT_HEAD-TEXT05 = TEXT05.
  GT_HEAD-TEXT06 = TEXT06.
  GT_HEAD-TEXT07 = TEXT07.
  GT_HEAD-TEXT08 = TEXT08.
  GT_HEAD-TEXT09 = TEXT09.
  GT_HEAD-TEXT10 = TEXT10.
  APPEND GT_HEAD.
  CLEAR GT_HEAD.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  APPEND_FOOT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->STYLE   TEXT
*      -->TEXT01   TEXT
*      -->TEXT02   TEXT
*      -->TEXT03   TEXT
*      -->TEXT04   TEXT
*      -->TEXT05   TEXT
*      -->TEXT06   TEXT
*      -->TEXT07   TEXT
*      -->TEXT08   TEXT
*      -->TEXT09   TEXT
*      -->TEXT10   TEXT
*----------------------------------------------------------------------*
FORM APPEND_FOOT  USING    VALUE(STYLE)
                           VALUE(TEXT01)
                           VALUE(TEXT02)
                           VALUE(TEXT03)
                           VALUE(TEXT04)
                           VALUE(TEXT05)
                           VALUE(TEXT06)
                           VALUE(TEXT07)
                           VALUE(TEXT08)
                           VALUE(TEXT09)
                           VALUE(TEXT10).
  GT_FOOT-STYLE = STYLE.
  GT_FOOT-TEXT01 = TEXT01.
  GT_FOOT-TEXT02 = TEXT02.
  GT_FOOT-TEXT03 = TEXT03.
  GT_FOOT-TEXT04 = TEXT04.
  GT_FOOT-TEXT05 = TEXT05.
  GT_FOOT-TEXT06 = TEXT06.
  GT_FOOT-TEXT07 = TEXT07.
  GT_FOOT-TEXT08 = TEXT08.
  GT_FOOT-TEXT09 = TEXT09.
  GT_FOOT-TEXT10 = TEXT10.
  APPEND GT_FOOT.
  CLEAR GT_FOOT.
ENDFORM.
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
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM   TEXT
*      -->RS_SELFIELD   TEXT
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM TYPE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
*双击ALV行
    WHEN '&PRINT'.
      DATA GT_ZMM005_HEAD LIKE TABLE OF GT_ZMM005 WITH HEADER LINE.
      DATA GT_ZMM005_ITEM LIKE TABLE OF GT_ZMM005 WITH HEADER LINE.
      CALL SCREEN 9000 STARTING AT 11 10 ENDING AT 50 26.
      CLEAR GT_ZMM005[].
      CHECK X_STYLE1 IS NOT INITIAL
      OR X_STYLE2 IS NOT INITIAL
      OR X_STYLE3 IS NOT INITIAL
      OR X_STYLE4 IS NOT INITIAL
      OR X_STYLE5 IS NOT INITIAL
      OR X_STYLE6 IS NOT INITIAL
      OR X_STYLE7 IS NOT INITIAL
      OR X_STYLE8 IS NOT INITIAL.
      LOOP AT GT_ALV WHERE CHECKBOX EQ 'X'.
        MOVE-CORRESPONDING GT_ALV TO GT_ZMM005.
        APPEND GT_ZMM005.
        CLEAR GT_ZMM005.
      ENDLOOP.
      IF GT_ZMM005[] IS INITIAL.
        LOOP AT GT_ALV.
          MOVE-CORRESPONDING GT_ALV TO GT_ZMM005.
          IF GT_ZMM005-KWERT < 0.
            GT_ZMM005-MENGE = GT_ZMM005-MENGE * -1.
          ENDIF.
          APPEND GT_ZMM005.
          CLEAR GT_ZMM005.
        ENDLOOP.
      ENDIF.
      DATA:TMP LIKE KSSKAUSP-OBJEK.
      DATA:ATWRT1 LIKE KSSKAUSP-ATWRT.
      DATA:ATWRT2 LIKE KSSKAUSP-ATWRT.
      IF GT_ZMM005[] IS NOT INITIAL.
        LOOP AT GT_ZMM005.
          IF GT_ZMM005-KWERT < 0.
            GT_ZMM005-MENGE = GT_ZMM005-MENGE * -1.
          ENDIF.
          CLEAR: ATWRT1 ,ATWRT2.
          TMP = GT_ZMM005-MATNR.
          SELECT SINGLE ATWRT INTO ATWRT1 FROM KSSKAUSP WHERE OBJEK = TMP AND  ATINN = '0000000815' .
          IF ATWRT1 <> '' AND ATWRT1 <>'0' .
            CONCATENATE GT_ZMM005-MAKTX  '-' ATWRT1 INTO GT_ZMM005-MAKTX .
          ENDIF.
          SELECT SINGLE ATWRT INTO ATWRT2 FROM KSSKAUSP WHERE OBJEK = TMP AND  ATINN = '0000000813' .
          IF ATWRT2 <> '' AND ATWRT2 <>'0' .
            CONCATENATE GT_ZMM005-MAKTX  '' ATWRT2 INTO GT_ZMM005-MAKTX .
          ENDIF.
          MODIFY GT_ZMM005.
        ENDLOOP.
      ENDIF.
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME           = L_FORMNAME         "SMARTFORMS的名字
        IMPORTING
          FM_NAME            = G_NAME                "对应的SMARTFORMS的函数
        EXCEPTIONS
          NO_FORM            = 1
          NO_FUNCTION_MODULE = 2
          OTHERS             = 3.
*    WAIT UP TO 1 SECONDS.
      IF SY-SUBRC <> 0.
*     ERROR HANDLING
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

      CONTROL-NO_OPEN = 'X'.
      CONTROL-NO_CLOSE = 'X'.

*   START PRINTING

      CALL FUNCTION 'SSF_OPEN'
        EXPORTING
          CONTROL_PARAMETERS = CONTROL
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
      IF SY-SUBRC <> 0.
*     ERROR HANDLING
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.
      CLEAR: GT_ZMM005_HEAD[],GT_ZMM005_ITEM[].
      GT_ZMM005_HEAD[] = GT_ZMM005[].
      SORT GT_ZMM005_HEAD BY EBELN.
      DELETE ADJACENT DUPLICATES FROM GT_ZMM005_HEAD COMPARING EBELN.
      DATA:X1FOOT(100) TYPE C.
      DATA:X2FOOT(100) TYPE C.
      LOOP AT GT_ZMM005_HEAD.
        CLEAR GT_ZMM005_ITEM[].
        LOOP AT GT_ZMM005 WHERE EBELN EQ GT_ZMM005_HEAD-EBELN."获取该PO的行项目
          APPEND GT_ZMM005 TO GT_ZMM005_ITEM.
        ENDLOOP.
        CLEAR: GT_HEAD[],GT_FOOT[].
        IF GT_ZMM005_ITEM[] IS NOT INITIAL.
          READ TABLE GT_ZMM005_ITEM INDEX 1.
        ENDIF.



        "获取需求方的地址编号
        DATA L_ADRNR TYPE EKPO-ADRNR.
        DATA L_TEL_NUMBER TYPE ADRC-TEL_NUMBER.
        DATA L_FAX_NUMBER TYPE ADRC-FAX_NUMBER.
        DATA L_STREET TYPE ADRC-STREET.
        DATA L_STREET1 TYPE ADRC-STREET.
        DATA L_NAME1 TYPE CHAR100.
        DATA L_NAME1_S TYPE KNVK-NAME1.
        DATA L_TELFX TYPE LFA1-TELFX.

        DATA L_BSART_TXT TYPE CHAR20. "订单类型
        DATA L_EKGRP_TXT TYPE CHAR20. "业务员
        DATA L_ZTERM_TXT TYPE CHAR20. "付款条件
        DATA L_WKURS_TXT TYPE CHAR20. "汇率

        DATA L_EBELP TYPE EKPO-EBELP."行项目号
        CLEAR: L_ADRNR, L_TEL_NUMBER, L_FAX_NUMBER, L_STREET, L_NAME1, L_BSART_TXT, L_EKGRP_TXT, L_ZTERM_TXT, L_WKURS_TXT, L_EBELP.
        SELECT MIN( EBELP ) FROM EKPO INTO L_EBELP
          WHERE EBELN EQ GT_ZMM005_ITEM-EBELN
          AND ADRNR NE SPACE
          AND LOEKZ EQ SPACE.
        IF L_EBELP IS NOT INITIAL.
          SELECT SINGLE ADRNR FROM EKPO INTO L_ADRNR
            WHERE EBELN EQ GT_ZMM005_ITEM-EBELN
            AND EBELP EQ L_EBELP.
        ELSE.
          SELECT SINGLE ADRNR FROM T001W INTO L_ADRNR
            WHERE WERKS EQ GT_ZMM005_ITEM-BUKRS.
        ENDIF.
        "获取需方电话、传真
*      SELECT SINGLE TEL_NUMBER FAX_NUMBER STREET FROM ADRC INTO (L_TEL_NUMBER, L_FAX_NUMBER, L_STREET)
        SELECT SINGLE STREET FROM ADRC INTO L_STREET
          WHERE ADDRNUMBER = L_ADRNR.
        CONCATENATE '到货地址：' L_STREET INTO L_STREET1.
        "获取供应商联系人
        CLEAR: L_NAME1_S,L_NAME1.
        "获取供应商联系人(如果NAMEV = Y的一条都没有，则取第一个)
        SELECT NAME1 FROM KNVK INTO L_NAME1_S WHERE LIFNR EQ GT_ZMM005_ITEM-LIFNR AND NAMEV EQ 'Y' AND NAME1 NE SPACE.
          IF L_NAME1 IS INITIAL.
            L_NAME1 = L_NAME1_S.
          ELSE.
            CONCATENATE L_NAME1 L_NAME1_S INTO L_NAME1 SEPARATED BY '/'.
          ENDIF.
        ENDSELECT.
        IF L_NAME1 IS INITIAL.
          SELECT SINGLE NAME1 FROM KNVK INTO L_NAME1 WHERE LIFNR EQ GT_ZMM005_ITEM-LIFNR AND NAME1 NE SPACE.
        ENDIF.
        "订单类型
        SELECT SINGLE BATXT FROM T161T INTO L_BSART_TXT WHERE SPRAS = SY-LANGU AND BSART EQ GT_ZMM005_ITEM-BSART AND BSTYP EQ 'F'.
        "业务员、需方电话、传真
        SELECT SINGLE EKNAM TELFX TEL_NUMBER FROM T024 INTO (L_EKGRP_TXT, L_FAX_NUMBER, L_TEL_NUMBER) WHERE EKGRP EQ GT_ZMM005_ITEM-EKGRP.

        L_ZTERM_TXT = GT_ZMM005_ITEM-ZTERM_TEXT.
        "汇率
        DATA WKURS_I TYPE P DECIMALS 2.
        WKURS_I = GT_ZMM005_ITEM-WKURS.
        L_WKURS_TXT = WKURS_I.
        SHIFT L_WKURS_TXT LEFT DELETING LEADING SPACE.
        DATA L_GYSHTBH TYPE CHAR30."供应商合同编号
        DATA L_YSFS TYPE CHAR30."运输方式
        DATA L_BZ TYPE CHAR200."备注
        PERFORM READ_TEXT USING 'F01' GT_ZMM005_ITEM-EBELN 'EKKO' '1' CHANGING L_GYSHTBH.
        PERFORM READ_TEXT USING 'F02' GT_ZMM005_ITEM-EBELN 'EKKO' '1' CHANGING L_YSFS.
        PERFORM READ_TEXT USING 'F00' GT_ZMM005_ITEM-EBELN 'EKKO' '0' CHANGING L_BZ.
        "供应商合同编号：READ TEXT(文本对象：EKKO，TEXTID：F01）
        CONCATENATE '备注:' L_BZ INTO L_BZ.
        DATA: L_LIFNR2(100) TYPE C.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT' "供应商号去0
          EXPORTING
            INPUT  = GT_ZMM005_ITEM-LIFNR
          IMPORTING
            OUTPUT = L_LIFNR2.
        "运输方式
        CASE W_YSFS.
          WHEN '1'.
            GT_ZMM005_ITEM-YSFS = '汽运' . "
          WHEN '2'.
            GT_ZMM005_ITEM-YSFS = '空运' .
          WHEN '3'.
            GT_ZMM005_ITEM-YSFS = '快递' .
        ENDCASE.
        "提货方式
        CASE W_THFS.
          WHEN '1'.
            GT_ZMM005_ITEM-THFS = '送货上门' .
          WHEN '2'.
            GT_ZMM005_ITEM-THFS = '自提' .
        ENDCASE.
        "货物质量要求
        CLEAR:G_HWZLYQ.
        REFRESH W_HWZLYQ .
        CALL METHOD EDITOR_1->GET_TEXT_AS_R3TABLE
          IMPORTING
            TABLE = W_HWZLYQ.

        LOOP AT W_HWZLYQ INTO HWZLYQ_LINE .
          CONDENSE HWZLYQ_LINE NO-GAPS.
          IF G_HWZLYQ IS INITIAL.
            G_HWZLYQ = HWZLYQ_LINE .
          ELSE.
            CONCATENATE G_HWZLYQ  HWZLYQ_LINE INTO G_HWZLYQ .
          ENDIF.
        ENDLOOP.

        "合同回传地址
        REFRESH W_HTHCDZ .
        CLEAR:G_HTHCDZ.
        CALL METHOD EDITOR_2->GET_TEXT_AS_R3TABLE
          IMPORTING
            TABLE = W_HTHCDZ.
        LOOP AT W_HTHCDZ INTO HTHCDZ_LINE .
          CONDENSE HTHCDZ_LINE NO-GAPS.
          IF G_HTHCDZ IS INITIAL.
            G_HTHCDZ = HTHCDZ_LINE .
          ELSE.
            CONCATENATE G_HTHCDZ  HTHCDZ_LINE INTO G_HTHCDZ .
          ENDIF.
        ENDLOOP.

        "发票回传地址
        REFRESH W_FPHCDZ .
        CLEAR:G_FPHCDZ .
        CALL METHOD EDITOR_3->GET_TEXT_AS_R3TABLE
          IMPORTING
            TABLE = W_FPHCDZ.
        LOOP AT W_FPHCDZ INTO FPHCDZ_LINE .
          CONDENSE FPHCDZ_LINE NO-GAPS.
          IF G_FPHCDZ IS INITIAL.
            G_FPHCDZ = FPHCDZ_LINE .
          ELSE.
            CONCATENATE G_FPHCDZ  FPHCDZ_LINE INTO G_FPHCDZ .
          ENDIF.
        ENDLOOP.

        "质保期
        CLEAR:G_ZBQ .
        G_ZBQ = W_ZBQ .
        CASE 'X'.
          WHEN X_STYLE1.
            G_STYLE = 'STYLE1'.
            G_LOGO = 'L1'.
            PERFORM APPEND_HEAD USING:
              'TITLE' 'LEYARD 采购订单' '' '' '' '' '' '' '' '' '',
              'HEAD6' '订单类型：' GT_ZMM005_ITEM-BSART '订单日期：' GT_ZMM005_ITEM-AEDAT_TEXT '订单编号：' GT_ZMM005_ITEM-EBELN '' '' '' '',
              'HEAD6' '需方单位：' GT_ZMM005_ITEM-BUTXT '供应商编码： ' L_LIFNR2 '供应商合同编号：' L_GYSHTBH '' '' '' '',
              'HEAD6' '需方电话：' L_TEL_NUMBER '供应商全称：' GT_ZMM005_ITEM-NAME1 '币种：' GT_ZMM005_ITEM-WAERS '' '' '' '',
              'HEAD6' '业务员：' L_EKGRP_TXT '供方电话： ' GT_ZMM005_ITEM-TELF1 '运输方式：' GT_ZMM005_ITEM-YSFS '' '' '' '',
              'HEAD6' '需方传真：' L_FAX_NUMBER '供方联系人： ' L_NAME1 '提货方式'GT_ZMM005_ITEM-THFS '' '' '' '',
              'HEAD6' '' '' '供方传真：' GT_ZMM005_ITEM-TELFX '' '' '' '' '' '',
              'HEAD1' L_BZ '' '' '' '' '' '' '' '' '',
              'HEAD1' L_STREET1 '' '' '' '' '' '' '' '' ''"到货地址
              .
            CONCATENATE  '需方：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '供方：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            PERFORM APPEND_FOOT USING:
              'FOOT2' X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '需方代表人签字（盖章）：' '供方代表人签字（盖章）：' '' '' '' '' '' '' '' '',
              'FOOT2' '审核人：' '审核人：' '' '' '' '' '' '' '' '',
              'FOOT2' '日期：' '日期：' '' '' '' '' '' '' '' ''.
          WHEN X_STYLE2.
            G_STYLE = 'STYLE2'.
            G_LOGO = 'L1'.
            DATA L_NO TYPE CHAR20.
            DATA: L_STREET2(100) TYPE C.
            DATA: L_TEL_NUMBER2(100) TYPE C.
            DATA: L_FAX_NUMBER2(100) TYPE C.
            DATA: L_NAME12(100) TYPE C.
            DATA: L_STREET2A(100) TYPE C.
            DATA: L_TELF12(100) TYPE C.
            DATA: L_TELFX2(100) TYPE C.
            DATA: L_WAERS2(100) TYPE C.
            DATA: L_ZTERM2(100) TYPE C.
            CLEAR: L_STREET2,L_TEL_NUMBER2,L_FAX_NUMBER2,L_NAME12,L_STREET2A,L_TELF12,L_TELFX2.
            CONCATENATE '订单编号:' GT_ZMM005_ITEM-EBELN INTO L_NO.
            CONDENSE L_NO.
            CONCATENATE '甲      方：' GT_ZMM005_ITEM-BUTXT INTO  X1FOOT.
            CONCATENATE '住      所：' L_STREET INTO L_STREET2.
            CONCATENATE '联系 电 话：' L_TEL_NUMBER INTO L_TEL_NUMBER2.
            CONCATENATE '传      真：' L_FAX_NUMBER INTO L_FAX_NUMBER2.
            CONCATENATE '乙      方：' GT_ZMM005_ITEM-NAME1 INTO L_NAME12.
            CONCATENATE '供应商编码：' L_LIFNR2 INTO L_LIFNR2.
            CONCATENATE '住      所：' GT_ZMM005_ITEM-STREET INTO L_STREET2A.
            CONCATENATE '联系 电 话：' GT_ZMM005_ITEM-TELF1 INTO L_TELF12.
            CONCATENATE '传      真：' GT_ZMM005_ITEM-TELFX INTO L_TELFX2.
            CONCATENATE '币      种：' GT_ZMM005_ITEM-WAERS INTO L_WAERS2.
            CONCATENATE '付款 条 件：' GT_ZMM005_ITEM-ZTERM_TEXT INTO L_ZTERM2.
            PERFORM APPEND_HEAD USING:"输出格式2
              'TITLE' 'LEYARD 采购订单' '' '' '' '' '' '' '' '' '',
              'HEAD_RIGHT' L_NO '' '' '' '' ''  '' '' '' '',
              'HEAD1' X1FOOT '' '' '' '' '' '' '' '' '',"甲      方：利亚德电视技术有限公司
              'HEAD1' L_STREET2 '' '' '' '' '' '' '' '' '',"住      所
              'HEAD1' '法定代表人： 李军' '' '' '' '' '' '' '' '' '',"法定代表人： 李军
              'HEAD1' L_TEL_NUMBER2 '' '' '' '' '' '' '' '' '',"联系 电 话：010-62888888
              'HEAD1' L_FAX_NUMBER2 '' '' '' '' '' '' '' '' '',"传      真：010-62877624
              'HEAD0' '' '' '' '' '' '' '' '' '' '',
              'HEAD1' L_NAME12 '' '' '' '' '' '' '' '' '',"乙      方
              'HEAD1' L_LIFNR2 '' '' '' '' '' '' '' '' '', "供应商编码：EKKO-LIFNR
              'HEAD1' L_STREET2A '' '' '' '' '' '' '' '' '',"住      所
              'HEAD1' '法定代表人：' '' '' '' '' '' '' '' '' '',"法定代表人
              'HEAD1' L_TELF12 '' '' '' '' '' '' '' '' '',"联系 电 话
              'HEAD1' L_TELFX2 '' '' '' '' '' '' '' '' '',"传      真
              'HEAD1' L_WAERS2 '' '' '' '' '' '' '' '' '',"币种：EKKO-WAERS
              'HEAD1' L_ZTERM2 '' '' '' '' '' '' '' '' '',"付款条件：LFB1-ZTERM
              'HEAD0' '' '' '' '' '' '' '' '' '' '',
              'HEAD21' '    根据《中华人民共和国合同法》及相关法律、法规，甲、乙双方本着诚实、信用的原则，'&
              '经过平等、友好协商，就甲方向乙方采购产品事宜，达成如下条款，以资共同信守：' '' '' '' '' '' '' '' '' '',
              'HEAD1' '第一条：订购产品' '' '' '' '' '' '' '' '' ''.
            CONCATENATE  '甲方（盖章）：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '乙方（盖章）：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            CONCATENATE '联系人电话：' L_TEL_NUMBER INTO L_TEL_NUMBER2.
            CONCATENATE '联系人电话：' GT_ZMM005_ITEM-TELF1 INTO L_TELF12.
            PERFORM APPEND_FOOT USING:
             " 'FOOT2' '甲方（盖章）：利亚德电视技术有限公司' '乙方（盖章）：' '' '' '' '' '' '' '' '',
             'FOOT2' X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '授权代表签字：' '授权代表签字：' '' '' '' '' '' '' '' '',
              'FOOT2' L_TEL_NUMBER2 L_TELF12 '' '' '' '' '' '' '' '',
              'FOOT2' '签订日期：      年    月    日' '' '' '' '' '' '' '' '' ''.
*            'FOOT2' '签订地点：北京市海淀区' '' '' '' '' '' '' '' '' ''.
          WHEN X_STYLE3.
            G_STYLE = 'STYLE3'.
            G_LOGO = 'L1'.
            PERFORM APPEND_HEAD USING:
              'TITLE' 'LEYARD委托加工合同' '' '' '' '' '' '' '' '' '',
              'HEAD6' '订单编号：' GT_ZMM005_ITEM-EBELN '订单日期：' GT_ZMM005_ITEM-AEDAT '付款条件：' GT_ZMM005_ITEM-ZTERM_TEXT '' '' '' '',
              'HEAD6' '甲方：' GT_ZMM005_ITEM-BUTXT '送货地址：' L_STREET '' '' '' '' '' '',
              'HEAD6' '乙方：' GT_ZMM005_ITEM-NAME1 '供应商编码：'  L_LIFNR2 '币种：' GT_ZMM005_ITEM-WAERS '' '' '' '',
              'HEAD21' '甲乙双方根据《中华人民共和国合同法》等相关法律、法规的规定，在平等互利的基础上，'&
              '就甲方委托乙方加工物料的相关事宜协商一致，达成如下条款，并签订本合同' '' '' '' '' '' '' '' '' '',
              'HEAD1' '第一条    委托加工物料及内容' '' '' '' '' '' '' '' '' ''
              .
            CONCATENATE  '甲方：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '乙方：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            CONCATENATE '联系人电话：' L_TEL_NUMBER INTO L_TEL_NUMBER2.
            CONCATENATE '联系人电话：' GT_ZMM005_ITEM-TELF1 INTO L_TELF12.
            PERFORM APPEND_FOOT USING:"输出格式3
             " 'FOOT2' '甲方：深圳利亚德光电有限公司' '乙方：' '' '' '' '' '' '' '' '',
             'FOOT2'  X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '授权代表签字：' '授权代表签字：' '' '' '' '' '' '' '' '',
              'FOOT2' L_TEL_NUMBER2 L_TELF12 '' '' '' '' '' '' '' '',
              'FOOT2' '签订日期：' '签订日期：' '' '' '' '' '' '' '' ''.
          WHEN X_STYLE4.
            G_STYLE = 'STYLE4'.
            G_LOGO = 'L1'.
            PERFORM APPEND_HEAD USING:
              'TITLE' 'LEYARD采购订单' '' '' '' '' '' '' '' '' '',
              'TITLE' 'PURCHASE  CONFIRMATION' '' '' '' '' '' '' '' '' '',
              'HEAD4' '合同号:' GT_ZMM005_ITEM-EBELN '日期:' GT_ZMM005_ITEM-AEDAT_TEXT '' '' '' '' '' '',
              'HEAD4' 'CONTRACT NO.:' GT_ZMM005_ITEM-EBELN 'DATE:' GT_ZMM005_ITEM-AEDAT_TEXT '' '' '' '' '' ''.
            .
            IF GT_ZMM005_ITEM-BUKRS EQ '1100'.
              PERFORM APPEND_FOOT USING:
              'FOOT2' GT_ZMM005_ITEM-NAME1 'SHENZHEN LEYARD OPTO-ELECTRONIC CO LTD.' '' '' '' '' '' '' '' ''.
            ELSEIF GT_ZMM005_ITEM-BUKRS EQ '1000'.
              PERFORM APPEND_FOOT USING:
              'FOOT2' GT_ZMM005_ITEM-NAME1 'LEYARD OPTOELECTRONIC CO., LTD. ' '' '' '' '' '' '' '' ''.
            ELSEIF GT_ZMM005_ITEM-BUKRS EQ '1500'.
              PERFORM APPEND_FOOT USING:
              'FOOT2' GT_ZMM005_ITEM-NAME1 'LEYARD   TV   TECHNOLOGY   CO.,LTD.' '' '' '' '' '' '' '' ''.
            ENDIF.
            PERFORM APPEND_FOOT USING:
*            'FOOT2' 'EVLITE ELECTRONICS CO.,LTD ' GT_ZMM005_ITEM-NAME1 '' '' '' '' '' '' '' '',
              'FOOT2' 'SIGNATURE:' 'SIGNATURE: ' '' '' '' '' '' '' '' ''.
*-----------------2017年5月31日：格式5-深圳金立翔国内常规订单---------
          WHEN X_STYLE5.
            G_STYLE = 'STYLE5'.
            G_LOGO = 'L2'.
            DATA:
              XFDH TYPE C LENGTH 30,
              XFCZ TYPE C LENGTH 30.
            CLEAR XFDH.
            CLEAR XFCZ.

            XFDH = '0755-29174931'.
            XFCZ = '0755-86232733'.
            PERFORM APPEND_HEAD USING:
             'TITLE' '金立翔 采购订单' '' '' '' '' '' '' '' '' '',
             'HEAD6' '订单类型:'GT_ZMM005_ITEM-BSART '订单日期:'GT_ZMM005_ITEM-AEDAT_TEXT '订  单  编  号:'GT_ZMM005_ITEM-EBELN '' '' '' '',
             'HEAD6' '需方单位:'GT_ZMM005_ITEM-BUTXT '供应商编码: 'L_LIFNR2 '供应商合同编号:'L_GYSHTBH '' '' '' '',
             'HEAD6' '需方电话:'XFDH'供应商全称:'GT_ZMM005_ITEM-NAME1 '币          种:'GT_ZMM005_ITEM-WAERS '' '' '' '',
             'HEAD6' '业务员:'L_EKGRP_TXT '供方电话:'GT_ZMM005_ITEM-TELF1'运  输  方  式:'GT_ZMM005_ITEM-YSFS '' '' '' '',
             'HEAD6' '需方传真:'XFCZ'供方联系人:'L_NAME1'提  货  方  式:'GT_ZMM005_ITEM-THFS '' '' '' '',
             'HEAD6' '' '' '供方传真:'GT_ZMM005_ITEM-TELFX '' '' '' '' '' '',
             'HEAD1' L_BZ '' '' '' '' '' '' '' '' '',    "备注
             'HEAD1' L_STREET1 '' '' '' '' '' '' '' '' ''"到货地址
             .
            CONCATENATE  '需方：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '供方：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            PERFORM APPEND_FOOT USING:
              'FOOT2' X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '需方代表人签字（盖章）：' '供方代表人签字（盖章）：' '' '' '' '' '' '' '' '',
              'FOOT2' '审核人：' '审核人：' '' '' '' '' '' '' '' '',
              'FOOT2' '日期：' '日期：' '' '' '' '' '' '' '' ''.
*-----------------2017年5月31日：格式6-深圳金立翔外协订单---------

          WHEN X_STYLE6.
            G_STYLE = 'STYLE6'.
            G_LOGO = 'L2'.
            DATA:
              XFDH_6 TYPE C LENGTH 30,
              XFCZ_6 TYPE C LENGTH 30.
            CLEAR XFDH_6.
            CLEAR XFCZ_6.

            XFDH_6 = '0755-29174931'.
            XFCZ_6 = '0755-86232733'.
            PERFORM APPEND_HEAD USING:
              'TITLE' '金立翔委托加工合同' '' '' '' '' '' '' '' '' '',
              'HEAD6' '订单编号:'GT_ZMM005_ITEM-EBELN '订单日期:'GT_ZMM005_ITEM-AEDAT '付  款  条  件:'GT_ZMM005_ITEM-ZTERM_TEXT '' '' '' '',
              'HEAD6' '甲    方:'GT_ZMM005_ITEM-BUTXT '送货地址:'L_STREET '' '' '' '' '' '',
              'HEAD6' '电    话:'XFDH_6 '传    真:'XFCZ_6 '' '' '' '' '' '',
              'HEAD6' '乙    方:'GT_ZMM005_ITEM-NAME1 '供应商编码:'L_LIFNR2 '币          种:'GT_ZMM005_ITEM-WAERS '' '' '' '',
              'HEAD21' '甲乙双方根据《中华人民共和国合同法》等相关法律、法规的规定，在平等互利的基础上，'&
              '就甲方委托乙方加工物料的相关事宜协商一致，达成如下条款，并签订本合同' '' '' '' '' '' '' '' '' '',
              'HEAD1' '第一条    委托加工物料及内容' '' '' '' '' '' '' '' '' ''
              .
            CONCATENATE  '甲方：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '乙方：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            CONCATENATE '联系人电话：' L_TEL_NUMBER INTO L_TEL_NUMBER2.
            CONCATENATE '联系人电话：' GT_ZMM005_ITEM-TELF1 INTO L_TELF12.
            PERFORM APPEND_FOOT USING:"输出格式3
             " 'FOOT2' '甲方：深圳利亚德光电有限公司' '乙方：' '' '' '' '' '' '' '' '',
             'FOOT2'  X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '授权代表签字：' '授权代表签字：' '' '' '' '' '' '' '' '',
              'FOOT2' L_TEL_NUMBER2 L_TELF12 '' '' '' '' '' '' '' '',
              'FOOT2' '签订日期：' '签订日期：' '' '' '' '' '' '' '' ''.
*-------------------20170607: 格式7-湖南君泽物料采购订单---------------
          WHEN X_STYLE7.
            G_STYLE = 'STYLE7'.
            G_LOGO = 'L7'.
            PERFORM APPEND_HEAD USING:
              'TITLE' '君泽照明采购订单' '' '' '' '' '' '' '' '' '',
              'HEAD6' '订单类型：' GT_ZMM005_ITEM-BSART '订单日期：' GT_ZMM005_ITEM-AEDAT_TEXT '订单编号：' GT_ZMM005_ITEM-EBELN '' '' '' '',
              'HEAD6' '需方单位：' GT_ZMM005_ITEM-BUTXT '供应商编码： ' L_LIFNR2 '供应商合同编号：' L_GYSHTBH '' '' '' '',
              'HEAD6' '需方电话：' L_TEL_NUMBER '供应商全称：' GT_ZMM005_ITEM-NAME1 '币种：' GT_ZMM005_ITEM-WAERS '' '' '' '',
              'HEAD6' '业务员：' L_EKGRP_TXT '供方电话： ' GT_ZMM005_ITEM-TELF1 '运输方式：' GT_ZMM005_ITEM-YSFS '' '' '' '',
              'HEAD6' '需方传真：' L_FAX_NUMBER '供方联系人： ' L_NAME1 '提货方式'GT_ZMM005_ITEM-THFS '' '' '' '',
              'HEAD6' '' '' '供方传真：' GT_ZMM005_ITEM-TELFX '' '' '' '' '' '',
              'HEAD1' L_BZ '' '' '' '' '' '' '' '' ''.
*              'HEAD1' L_STREET1 '' '' '' '' '' '' '' '' ''"到货地址

            CONCATENATE  '需方：' GT_ZMM005_ITEM-BUTXT  INTO  X1FOOT.
            CONCATENATE  '供方：' GT_ZMM005_ITEM-NAME1  INTO  X2FOOT.
            PERFORM APPEND_FOOT USING:
              'FOOT2' X1FOOT X2FOOT '' '' '' '' '' '' '' '',
              'FOOT2' '需方代表人签字（盖章）：' '供方代表人签字（盖章）：' '' '' '' '' '' '' '' '',
              'FOOT2' '审核人：' '审核人：' '' '' '' '' '' '' '' '',
              'FOOT2' '日期：' '日期：' '' '' '' '' '' '' '' ''.

        ENDCASE.



        CALL FUNCTION G_NAME
          EXPORTING
            CONTROL_PARAMETERS = CONTROL
            G_STYLE            = G_STYLE
            G_LOGO             = G_LOGO
            G_HWZLYQ           = G_HWZLYQ
            G_HTHCDZ           = G_HTHCDZ
            G_FPHCDZ           = G_FPHCDZ
            G_ZBQ              = G_ZBQ
            G_HWZLYQ_B         = G_HWZLYQ
            G_YEAR             = JYEAR
            G_MONTH            = JMONTH
            G_DAY              = JDAY
            G_DAYS             = DAYS    "工期
          TABLES
            GT_HEAD            = GT_HEAD[]
            GT_CONTENT         = GT_ZMM005_ITEM[]
            GT_FOOT            = GT_FOOT[]
          EXCEPTIONS
            FORMATTING_ERROR   = 1
            INTERNAL_ERROR     = 2
            SEND_ERROR         = 3
            USER_CANCELED      = 4
            OTHERS             = 5.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          DATA W_ZMM005_COUNT TYPE ZMM005_COUNT.
          CLEAR W_ZMM005_COUNT."增加打印次数
          SELECT SINGLE * FROM ZMM005_COUNT INTO CORRESPONDING FIELDS OF W_ZMM005_COUNT
            WHERE EBELN = GT_ZMM005_ITEM-EBELN
            AND STYLE EQ G_STYLE.
          IF SY-SUBRC EQ 0.
            ADD 1 TO W_ZMM005_COUNT-PRTCNT.
          ELSE.
            W_ZMM005_COUNT-EBELN = GT_ZMM005_ITEM-EBELN.
            W_ZMM005_COUNT-STYLE = G_STYLE.
            W_ZMM005_COUNT-PRTCNT = '1'.
          ENDIF.
          MODIFY ZMM005_COUNT FROM W_ZMM005_COUNT.
          COMMIT WORK.
        ENDIF.
      ENDLOOP.
      CALL FUNCTION 'SSF_CLOSE'
        IMPORTING
          JOB_OUTPUT_INFO  = JOB_OUTPUT_INFO
        EXCEPTIONS
          FORMATTING_ERROR = 1
          INTERNAL_ERROR   = 2
          SEND_ERROR       = 3
          OTHERS           = 4.

      IF SY-SUBRC <> 0.
*     ERROR HANDLING
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
*    READ TABLE GT_ALV INDEX RS_SELFIELD-TABINDEX.
*    IF SY-SUBRC = 0.
*      SET PARAMETER ID 'AUN' FIELD GT_ALV-VBELN.
*      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*    ENDIF.
      "BEGIN 增加外币格式打印 IT02 20170510
    WHEN '&PRT'.
      READ TABLE GT_ALV WITH KEY CHECKBOX = 'X'.
      IF SY-SUBRC EQ 0.
        CALL SCREEN 9001 STARTING AT 11 10 ENDING AT 90 30.
      ELSE.
        MESSAGE '请至少选中一行数据！' TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
      "END 增加外币格式打印

      "BEGIN 增加深圳金立翔公司间采购打印 HANDSSY 20170718
    WHEN '&GSJ'.



      READ TABLE GT_ALV WITH KEY CHECKBOX = 'X'.
      IF SY-SUBRC EQ 0.
        INCLUDE ZMM005A_GSJDY.
      ELSE.
        MESSAGE '请至少选中一行数据！' TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

      "END 增加外币格式打印
  ENDCASE.




ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS_SET
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->FIELD1   TEXT
*      -->FIELD2   TEXT
*----------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ZPOPUP'.
*  SET TITLEBAR 'XXX'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  IF OK_CODE EQ 'CANCEL'.
    CLEAR: X_STYLE1,X_STYLE2,X_STYLE3,X_STYLE4,X_STYLE5,X_STYLE6.
  ENDIF.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      FORM  READ_TEXT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->ID   TEXT
*      -->NAME  TEXT
*      -->OBJECT   TEXT
*      <--TDLINE  TEXT
*----------------------------------------------------------------------*
FORM READ_TEXT  USING    VALUE(ID)
                         VALUE(NAME)
                         VALUE(OBJECT)
                         VALUE(IND)"第IND行，若IND=0，则取全部
                CHANGING TDLINE.
  DATA T_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
  DATA TDNAME TYPE THEAD-TDNAME.
  TDNAME = NAME.
  CLEAR: TDLINE, T_TLINE[].
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      ID                      = ID
      LANGUAGE                = SY-LANGU
      NAME                    = TDNAME
      OBJECT                  = OBJECT
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      LINES                   = T_TLINE[]
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
*IMPLEMENT SUITABLE ERROR HANDLING HERE
  ENDIF.
  IF T_TLINE[] IS NOT INITIAL.
    IF IND GT 0.
      READ TABLE T_TLINE INDEX IND.
      IF SY-SUBRC EQ 0.
        TDLINE = T_TLINE-TDLINE. "文本内容
      ENDIF.
    ELSE.
      LOOP AT T_TLINE.
        IF TDLINE IS INITIAL.
          TDLINE = T_TLINE-TDLINE.
        ELSE.
          CONCATENATE TDLINE T_TLINE-TDLINE INTO TDLINE SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      MODULE  LISTBOX_9000  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE LISTBOX_9000 OUTPUT.
  TYPE-POOLS:VRM.
  DATA: VID    TYPE VRM_ID VALUE 'W_YSFS',
        VLIST  TYPE VRM_VALUES,
        VALUES LIKE LINE OF VLIST.
  CLEAR VLIST.
  CLEAR VALUES.
  MOVE '1' TO VALUES-KEY.
  MOVE '汽运' TO VALUES-TEXT.
  APPEND VALUES TO VLIST.
  MOVE '2' TO VALUES-KEY.
  MOVE '空运' TO VALUES-TEXT.
  APPEND VALUES TO VLIST.
  MOVE '3' TO VALUES-KEY.
  MOVE '快递' TO VALUES-TEXT.
  APPEND VALUES TO VLIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = VID
      VALUES          = VLIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  VID = 'W_THFS'.
  CLEAR VLIST.
  CLEAR VALUES.
  MOVE '1' TO VALUES-KEY.
  MOVE '送货上门' TO VALUES-TEXT.
  APPEND VALUES TO VLIST.
  MOVE '2' TO VALUES-KEY.
  MOVE '自提' TO VALUES-TEXT.
  APPEND VALUES TO VLIST.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = VID
      VALUES          = VLIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE '下拉框出错，请联系管理员！' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  CREATE_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE CREATE_CONTAINER OUTPUT.
  "货物质量要求
  IF INIT_1 IS INITIAL .
    INIT_1 = 'X'.
    CREATE OBJECT CONTAINER_1
      EXPORTING
        CONTAINER_NAME = 'HWZLYQ'.

    CREATE OBJECT EDITOR_1
      EXPORTING
        PARENT                     = CONTAINER_1
        WORDWRAP_MODE              = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION          = 50
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.


  ENDIF.
  CALL METHOD EDITOR_1->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = W_HWZLYQ.
  "合同回传地址
  IF INIT_2 IS INITIAL .
    INIT_2 = 'X'.
    CREATE OBJECT CONTAINER_2
      EXPORTING
        CONTAINER_NAME = 'HTHCDZ'.

    CREATE OBJECT EDITOR_2
      EXPORTING
        PARENT                     = CONTAINER_2
        WORDWRAP_MODE              = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION          = 50
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.


  ENDIF.
  CALL METHOD EDITOR_2->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = W_HTHCDZ.

  "发票回传地址
  IF INIT_3 IS INITIAL .
    INIT_3 = 'X'.
    CREATE OBJECT CONTAINER_3
      EXPORTING
        CONTAINER_NAME = 'FPHCDZ'.

    CREATE OBJECT EDITOR_3
      EXPORTING
        PARENT                     = CONTAINER_3
        WORDWRAP_MODE              = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION          = 50
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.


  ENDIF.
  CALL METHOD EDITOR_3->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = W_FPHCDZ.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  LISTBOX_9001  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE LISTBOX_9001 OUTPUT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  CREATE_CONTAINER_9001  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE CREATE_CONTAINER_9001 OUTPUT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'ZPOPUP'.
*  SET TITLEBAR 'XXX'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  IF OK_CODE EQ 'CANCEL'.
    LEAVE TO SCREEN 0.
  ELSEIF OK_CODE EQ 'OK'.
    IF GT_CGMX IS NOT INITIAL.
      PERFORM PRT_WB_DD USING GT_CGMX. "执行采购外币订单打印
    ENDIF.
  ENDIF.
ENDMODULE.

*&SPWIZARD: DATA INCL. INSERTED BY SP WIZARD. DO NOT CHANGE THIS LINE!
INCLUDE ZMM005A_WBDY .
