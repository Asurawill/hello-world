*&---------------------------------------------------------------------*
*& Report  ZPP009
*&
*&---------------------------------------------------------------------*
*&
*& 库存实际消耗报表
*&---------------------------------------------------------------------*

REPORT ZPP012.

TYPE-POOLS:SLIS,VRM,ICON.

TABLES: MARA,MARC,SYST.

TYPES:BEGIN OF TY_TAB,
        WERKS        TYPE MARC-WERKS,
        MATNR        TYPE MARC-MATNR,
        MATKL        TYPE MARA-MATKL,
        PLIFZ        TYPE N  LENGTH 6 , "                MARC-PLIFZ,MODIFY BY IT02 11218  ,"(因汇总时天数太长报错)
        MAKTX        TYPE MAKT-MAKTX,
        ZGXLX(2)     TYPE C,
        ZGXLX_MS(10) TYPE C,
        MNGKC        TYPE MDEZ-MNG01, " 库存数量
        PLUMI        TYPE MDEZ-PLUMI, " 收货/发货标识
        DAT00        TYPE MDEZ-DAT00,
        MNG01        TYPE MDEZ-MNG01,
        KYTS         TYPE CHAR10, "可用天数
      END OF TY_TAB.

TYPES:BEGIN OF TY_MARC,
        WERKS         TYPE MARC-WERKS,
        MATNR         TYPE MARC-MATNR,
        MATKL         TYPE MARA-MATKL,
        PLIFZ         TYPE MARC-PLIFZ,
        IUID_RELEVANT TYPE MARC-IUID_RELEVANT,
      END OF TY_MARC.

DATA:GT_TAB TYPE STANDARD TABLE OF TY_TAB WITH HEADER LINE.
DATA:GT_TAB_B TYPE STANDARD TABLE OF TY_TAB WITH HEADER LINE.
DATA:GT_TAB02 TYPE STANDARD TABLE OF TY_TAB WITH HEADER LINE.
DATA:GT_MARC TYPE STANDARD TABLE OF TY_MARC WITH HEADER LINE.

DATA: IT_TABLE     TYPE REF TO DATA,
      IT_STRUCTURE TYPE        LVC_T_FCAT,
      WA_STRUCTURE TYPE        LVC_S_FCAT.

DATA: WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT   TYPE LVC_S_LAYO.

FIELD-SYMBOLS:<DYN_TABLE> TYPE STANDARD TABLE,
              <DYN_WA>,
              <DYN_FIELD>.
FIELD-SYMBOLS:<DYN_TABLE_TMP> TYPE STANDARD TABLE,
              <DYN_WA_TMP>,
              <DYN_FIELD_TMP>.


SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
S_WERKS FOR MARC-WERKS NO-EXTENSION NO INTERVALS DEFAULT '1100' ,  "工厂
S_MATNR FOR MARA-MATNR ,
S_DISPO FOR MARC-DISPO DEFAULT '201' TO '206',
S_FEVOR FOR MARC-FEVOR,
S_MATKL FOR MARA-MATKL,
S_EXTWG FOR MARA-EXTWG,
S_BUDAT FOR SYST-DATUM OBLIGATORY NO-EXTENSION NO INTERVALS DEFAULT SY-DATUM.
PARAMETERS:
  P1 AS CHECKBOX,
  P2 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BLK.

AT SELECTION-SCREEN .
  IF S_MATNR[] IS INITIAL AND S_WERKS[] IS INITIAL.
    MESSAGE '请输入合适的选择条件,查询物料太多，将超过系统负荷！' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA.
  PERFORM FRM_CREATE_STRUCTURE.   " 定义内表的结构
  PERFORM CREATE_DYNAMIC_TABLE.   " 按照定义的内表结构，产生一个内表
  PERFORM WRITE_DATA_TO_DYNTABLE.  " 向动态内表中写数

END-OF-SELECTION.

  PERFORM FRM_INIT_LAYOUT .
  PERFORM FRM_SHOW_ALV.   " 从动态内表中取数，并写到屏幕


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  DATA:LT_MDEZX TYPE STANDARD TABLE OF MDEZ WITH HEADER LINE.
  DATA:LT_MARC TYPE STANDARD TABLE OF TY_MARC WITH HEADER LINE.

  CLEAR:LT_MARC,LT_MARC[].

  SELECT
    WERKS
    MARC~MATNR
    MARA~MATKL
    PLIFZ
    FROM MARC INNER JOIN MARA
    ON MARC~MATNR = MARA~MATNR
    INTO CORRESPONDING FIELDS OF TABLE LT_MARC
    WHERE MARC~MATNR IN S_MATNR
    AND WERKS IN S_WERKS
    AND DISPO IN S_DISPO
    AND FEVOR IN S_FEVOR
    AND MATKL IN S_MATKL
    AND EXTWG IN S_EXTWG.


  DATA:MNG_GJ TYPE MDEZ-MNG01. " 供给汇总
  DATA:MNG_XQ TYPE MDEZ-MNG01. " 需求汇总

* MD04，取得库存数量
  LOOP AT LT_MARC.
    CLEAR:LT_MDEZX,LT_MDEZX[].
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
*       PLSCN                    =
        MATNR                    = LT_MARC-MATNR
        WERKS                    = LT_MARC-WERKS
*       BERID                    =
*       ERGBZ                    =
*       AFIBZ                    =
*       INPER                    =
*       DISPLAY_LIST_MDPSX       =
*       DISPLAY_LIST_MDEZX       =
*       DISPLAY_LIST_MDSUX       =
*       NOBUF                    =
*       PLAUF                    =
*       I_VRFWE                  =
*       IS_SFILT                 =
*       IS_AFILT                 =
*   IMPORTING
*       E_MT61D                  =
*       E_MDKP                   =
*       E_CM61M                  =
*       E_MDSTA                  =
*       E_ERGBZ                  =
      TABLES
*       MDPSX                    =
        MDEZX                    = LT_MDEZX[]
*       MDSUX                    =
      EXCEPTIONS
        MATERIAL_PLANT_NOT_FOUND = 1
        PLANT_NOT_FOUND          = 2
        OTHERS                   = 3.

    CLEAR:GT_TAB.
    CLEAR:MNG_GJ,MNG_XQ.

    LOOP AT LT_MDEZX.
      GT_TAB-MATNR  = LT_MARC-MATNR.
      GT_TAB-MATKL  = LT_MARC-MATKL.
      GT_TAB-WERKS  = LT_MARC-WERKS.
      GT_TAB-PLIFZ  = LT_MARC-PLIFZ.
      GT_TAB-PLUMI  = LT_MDEZX-PLUMI. " 收货/发货标识
      GT_TAB-DAT00  = LT_MDEZX-DAT00. " 收货/需求日期
      GT_TAB-MNG01  = LT_MDEZX-MNG01. " 收货数量或需求数量
      IF LT_MDEZX-PLUMI = '+'.
        MNG_GJ  = MNG_GJ + LT_MDEZX-MNG01. " 供给数量汇总
      ENDIF.
      IF LT_MDEZX-PLUMI = '-'.
        MNG_XQ  = MNG_XQ + LT_MDEZX-MNG01. " 需求数量汇总
      ENDIF.
      COLLECT GT_TAB .
      CLEAR GT_TAB.
    ENDLOOP.

    " IUID_RELEVANT 做为不显示标记
    IF P1 = 'X' AND MNG_GJ <= 0. " 供给数量汇总 > 0, 不满足
      LT_MARC-IUID_RELEVANT = 'X'.
      MODIFY LT_MARC.
    ENDIF.

    IF P2 = 'X' AND  MNG_XQ >= 0. " 需求数量汇总 < 0, 不满足
      LT_MARC-IUID_RELEVANT = 'X'.
      MODIFY LT_MARC.
    ENDIF.

    " 取得工厂库存

*&--代码添加 BY HANDYBY 26.04.2017 15:07:25  BEGIN
    SORT GT_TAB BY MATNR WERKS .
    SORT LT_MDEZX BY PLUMI .
    READ TABLE GT_TAB WITH KEY MATNR = LT_MARC-MATNR
                               WERKS = LT_MARC-WERKS
                               BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC = 0 .
      LOOP AT GT_TAB FROM SY-TABIX .
        IF GT_TAB-MATNR = LT_MARC-MATNR AND
            GT_TAB-WERKS = LT_MARC-WERKS .
          READ TABLE LT_MDEZX WITH KEY PLUMI = 'B' BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT LT_MDEZX FROM SY-TABIX .
              IF LT_MDEZX-PLUMI = 'B'.
                ""  排除不参与MRP运算库存地点库存 ，排除安全库存
                IF  LT_MDEZX-DELKZ <> 'LB' AND  LT_MDEZX-DELKZ <> 'SH' .
                  GT_TAB-MNGKC = LT_MDEZX-MNG01 + GT_TAB-MNGKC.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
          MODIFY GT_TAB.
          CLEAR GT_TAB.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
*&--代码添加 BY HANDYBY 26.04.2017 15:07:25  END

*&--代码注释 BY HANDYBY 26.04.2017 15:07:08  BEGIN
*    LOOP AT GT_TAB WHERE MATNR  = LT_MARC-MATNR AND WERKS  = LT_MARC-WERKS.
*      LOOP AT LT_MDEZX.
*        IF LT_MDEZX-PLUMI = 'B'.
*          ""  排除不参与MRP运算库存地点库存 ，排除安全库存
*          IF  LT_MDEZX-DELKZ <> 'LB' AND  LT_MDEZX-DELKZ <> 'SH' .
*            GT_TAB-MNGKC = LT_MDEZX-MNG01 + GT_TAB-MNGKC.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      MODIFY GT_TAB.
*      CLEAR GT_TAB.
*    ENDLOOP.
*&--代码注释 BY HANDYBY 26.04.2017 15:07:08  END


  ENDLOOP.

  GT_MARC[] = LT_MARC[]. " 全局

*筛选条件，供给数量汇总>0, 需求数量汇总<0
  LOOP AT GT_TAB.
    READ TABLE GT_MARC WITH KEY WERKS = GT_TAB-WERKS
                                MATNR = GT_TAB-MATNR
                                IUID_RELEVANT = 'X'.
    IF SY-SUBRC = 0.
      DELETE GT_TAB.
    ENDIF.
  ENDLOOP.

  DELETE GT_MARC WHERE IUID_RELEVANT = 'X'.

ENDFORM.                    " FRM_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_CREATE_STRUCTURE .
  DATA:L_LOW  TYPE MKPF-BUDAT,
       L_HIGH TYPE MKPF-BUDAT.

  DATA: L_RESULT TYPE STRING,
        L_LINE   TYPE I,
        L_MID    TYPE STRING.

  DATA: L_STR1 TYPE STRING,
        L_STR2 TYPE STRING,
        L_STR3 TYPE STRING.


  DATA: L_LINES TYPE I.

  READ TABLE S_BUDAT INDEX 1.
  L_LOW  = S_BUDAT-LOW.
  L_HIGH = S_BUDAT-LOW + 29. " 查询日期起，自动加29天

  IF L_HIGH IS NOT INITIAL.
    L_LINE = L_HIGH - L_LOW + 1.
  ELSE.
    L_LINE = 1.
  ENDIF.


  L_LINES = 8.
* 创建静态结构部分
  PERFORM FRM_ALV_FCAT_SET USING:  1   'WERKS' TEXT-010 'WERKS'  'MARC'  SPACE   SPACE SPACE   SPACE,
                                   2   'MATNR' TEXT-011 'MATNR'  'MAKT'  SPACE   SPACE SPACE   SPACE,
                                   3   'MATKL' TEXT-013 'MATKL'  'MARA'  SPACE   SPACE SPACE   SPACE,
                                   4   'MAKTX' TEXT-012 'MAKTX'  'MAKT'  SPACE   SPACE SPACE   SPACE,
                                   5   'ZGXLX_MS'  '供需类型' ''  ''  SPACE   SPACE SPACE   SPACE,
                                   6   'MNGKC'  '工厂库存' ''  ''  SPACE    '18'  'QUAN'    'X',
                                   7   'PLIFZ'  '采购L/T' ''  ''  SPACE   SPACE SPACE   SPACE,
                                   8   'KYTS'  '可用天数' ''  ''  SPACE   SPACE SPACE   SPACE.

  " 按天显示列
  DO L_LINE TIMES.
    CLEAR WA_STRUCTURE.
    " 字段名称
    CONCATENATE 'DYNAMIC_MENGE' L_LOW INTO WA_STRUCTURE-FIELDNAME.
    " 列名称
    L_STR1 = L_LOW+0(4).
    L_STR2 = L_LOW+4(2).
    L_STR3 = L_LOW+6(2).
    CONCATENATE L_STR1 L_STR2 L_STR3 INTO WA_STRUCTURE-SCRTEXT_L
    SEPARATED BY '/'.
    WA_STRUCTURE-SCRTEXT_L = WA_STRUCTURE-SCRTEXT_L.
    WA_STRUCTURE-SCRTEXT_M = WA_STRUCTURE-SCRTEXT_L.
    WA_STRUCTURE-SCRTEXT_S = WA_STRUCTURE-SCRTEXT_L.
    WA_STRUCTURE-COL_POS    = L_LINES.
    WA_STRUCTURE-OUTPUTLEN = 18.

    WA_STRUCTURE-DATATYPE = 'QUAN'.
    WA_STRUCTURE-NO_ZERO  = 'X'.
*     wa_structure-DECIMALS_O = '0'.

*    wa_structure-ref_field  =  'MNG01'.
*    wa_structure-ref_table  =  'MDEZ'.
*    wa_structure-qfieldname =  'MEINH'.
    "

    APPEND WA_STRUCTURE TO IT_STRUCTURE.

    ADD 1 TO L_LOW.
    CLEAR:L_STR1,L_STR2,L_STR3.
    L_LINES = L_LINES + 1.
  ENDDO.

*  " 按周显示列
*  L_LINES = 21.
*  DO 16 TIMES.
*    CLEAR WA_STRUCTURE.
*    CONCATENATE 'DYNAMIC_MENGE' L_LOW INTO WA_STRUCTURE-FIELDNAME.
*    " 列名称
*    L_STR1 = L_LOW+0(4).
*    L_STR2 = L_LOW+4(2).
*    L_STR3 = L_LOW+6(2).
*    CONCATENATE L_STR1 L_STR2 L_STR3 INTO WA_STRUCTURE-SCRTEXT_L
*    SEPARATED BY '/'.
*    WA_STRUCTURE-SCRTEXT_L = WA_STRUCTURE-SCRTEXT_L.
*    WA_STRUCTURE-SCRTEXT_M = WA_STRUCTURE-SCRTEXT_L.
*    WA_STRUCTURE-SCRTEXT_S = WA_STRUCTURE-SCRTEXT_L.
**    wa_structure-ref_field  =  'MNG01'.
**    wa_structure-ref_table  =  'MDEZ'.
*    WA_STRUCTURE-OUTPUTLEN = 18.
*    WA_STRUCTURE-DATATYPE = 'QUAN'.
*    WA_STRUCTURE-NO_ZERO = 'X'.
**    wa_structure-DECIMALS_O = '0'.
*
**    wa_structure-ref_field  =  'MNG01'.
**    wa_structure-ref_table  =  'MDEZ'.
**    wa_structure-qfieldname =  'MEINH'.
*
*
*    WA_STRUCTURE-COL_POS    = L_LINES.
*    APPEND WA_STRUCTURE TO IT_STRUCTURE.
*    ADD 7 TO L_LOW.
*    CLEAR:L_STR1,L_STR2,L_STR3.
*    L_LINES = L_LINES + 1.
*  ENDDO.

ENDFORM.                    " FRM_CREATE_STRUCTURE

*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DYNAMIC_TABLE .
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = IT_STRUCTURE
    IMPORTING
      EP_TABLE        = IT_TABLE.

  ASSIGN IT_TABLE->* TO <DYN_TABLE>.
ENDFORM.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA_TO_DYNTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_DATA_TO_DYNTABLE .

  DATA:WA_NEW_LINE TYPE REF TO DATA.
  CREATE DATA WA_NEW_LINE LIKE LINE OF <DYN_TABLE>.
  ASSIGN WA_NEW_LINE->* TO <DYN_WA>.

  DATA:L_FIELDNAME TYPE LVC_S_FCAT-FIELDNAME.
  DATA:DATE1 TYPE SYST-DATUM.
  DATA:DATE2 TYPE SYST-DATUM.
  DATA:DATE3 TYPE SYST-DATUM.

  DATA:LT_MAKT TYPE STANDARD TABLE OF MAKT WITH HEADER LINE.
  CLEAR:LT_MAKT,LT_MAKT[].

  " 取得B库存数据
  LOOP AT GT_TAB.
    IF GT_TAB-PLUMI = 'B'.
      MOVE-CORRESPONDING GT_TAB TO GT_TAB_B.
      APPEND GT_TAB_B.
      CLEAR GT_TAB_B.
    ENDIF.
  ENDLOOP.

*删除库存数据，只保留需求数量和供给数量
  DELETE GT_TAB WHERE  PLUMI = 'B'.

*日期出来
  " 列开始日期
  DATE1 = S_BUDAT-LOW.
  " 按日期最后一天
  DATE2 = S_BUDAT-LOW + 29.
*  " 按周最后一周
*  DATE3 = DATE2 + 112.                                      " 16周*7天

*处理数据，取得查询日期之间的数据
  DELETE GT_TAB WHERE DAT00 < DATE1.
*  DELETE GT_TAB WHERE DAT00 > DATE3.

  DATA:L_DAY_1 TYPE   MDEZ-MNG01.                             "　 day1
  DATA:L_DAY_2 TYPE   MDEZ-MNG01.                             "　 day2
  DATA:L_DAY_3 TYPE   MDEZ-MNG01.                             "　 day3
  DATA:L_DAY_4 TYPE   MDEZ-MNG01.                             "　 day4
  DATA:L_DAY_5 TYPE   MDEZ-MNG01.                             "　 day5
  DATA:L_DAY_6 TYPE   MDEZ-MNG01.                             "  day6
  DATA:L_DAY_7 TYPE   MDEZ-MNG01.                             "　 day7
  DATA:L_DAY_8 TYPE   MDEZ-MNG01.                             "　 dat8
  DATA:L_DAY_9 TYPE   MDEZ-MNG01.                             "　  day9
  DATA:L_DAY_10 TYPE  MDEZ-MNG01.                             "　 day10
  DATA:L_DAY_11 TYPE  MDEZ-MNG01.                             "　 day11
  DATA:L_DAY_12 TYPE  MDEZ-MNG01.                             "　 day12
  DATA:L_DAY_13 TYPE  MDEZ-MNG01.                             "　 day13
  DATA:L_DAY_14 TYPE  MDEZ-MNG01.                             "　 day14
  DATA:L_DAY_15 TYPE  MDEZ-MNG01.                             "　 day15
  DATA:L_DAY_16 TYPE  MDEZ-MNG01.                             "　 day16
  DATA:L_DAY_17 TYPE  MDEZ-MNG01.                             "　 day17
  DATA:L_DAY_18 TYPE  MDEZ-MNG01.                             "　 day18
  DATA:L_DAY_19 TYPE  MDEZ-MNG01.                             "　 day19
  DATA:L_DAY_20 TYPE  MDEZ-MNG01.                             "　 day20
  DATA:L_DAY_21 TYPE  MDEZ-MNG01.                             "  day21
  DATA:L_DAY_22 TYPE  MDEZ-MNG01.                             "　 day22
  DATA:L_DAY_23 TYPE  MDEZ-MNG01.                             "　 dat23
  DATA:L_DAY_24 TYPE  MDEZ-MNG01.                             "　  day24
  DATA:L_DAY_25 TYPE  MDEZ-MNG01.                             "　 day25
  DATA:L_DAY_26 TYPE  MDEZ-MNG01.                             "　 day26
  DATA:L_DAY_27 TYPE  MDEZ-MNG01.                             "　 day27
  DATA:L_DAY_28 TYPE  MDEZ-MNG01.                             "　 day28
  DATA:L_DAY_29 TYPE  MDEZ-MNG01.                             "　 day29
  DATA:L_DAY_30 TYPE  MDEZ-MNG01.                             "　 day30
  DATA:L_DAY_31 TYPE  MDEZ-MNG01.                             "　 day31


*  DATA:L_MNG_1 TYPE MDEZ-MNG01.                             "　第一周累计数量
*  DATA:L_MNG_2 TYPE MDEZ-MNG01.                             "　第二周累计数量
*  DATA:L_MNG_3 TYPE MDEZ-MNG01.                             "　第三周累计数量
*  DATA:L_MNG_4 TYPE MDEZ-MNG01.                             "　第四周累计数量
*  DATA:L_MNG_5 TYPE MDEZ-MNG01.                             "　第五周累计数量
*  DATA:L_MNG_6 TYPE MDEZ-MNG01.                             " 第6周累计数量
*  DATA:L_MNG_7 TYPE MDEZ-MNG01.                             "　第7周累计数量
*  DATA:L_MNG_8 TYPE MDEZ-MNG01.                             "　第8周累计数量
*  DATA:L_MNG_9 TYPE MDEZ-MNG01.                             "　第9周累计数量
*  DATA:L_MNG_10 TYPE MDEZ-MNG01.                            "　第10周累计数量
*  DATA:L_MNG_11 TYPE MDEZ-MNG01.                            "　第11周累计数量
*  DATA:L_MNG_12 TYPE MDEZ-MNG01.                            "　第12周累计数量
*  DATA:L_MNG_13 TYPE MDEZ-MNG01.                            "　第13周累计数量
*  DATA:L_MNG_14 TYPE MDEZ-MNG01.                            "　第14周累计数量
*  DATA:L_MNG_15 TYPE MDEZ-MNG01.                            "　第15周累计数量
*  DATA:L_MNG_16 TYPE MDEZ-MNG01.                            "　第16周累计数量

*  DATA:DATE_1 TYPE SYST-DATUM.                              " 第一周
*  DATA:DATE_2 TYPE SYST-DATUM.                              "　第二周
*  DATA:DATE_3 TYPE SYST-DATUM.                              " 第三周
*  DATA:DATE_4 TYPE SYST-DATUM.                              " 第四周
*  DATA:DATE_5 TYPE SYST-DATUM.                              " 第五周
*  DATA:DATE_6 TYPE SYST-DATUM.                              " 第6周
*  DATA:DATE_7 TYPE SYST-DATUM.                              " 第7周
*  DATA:DATE_8 TYPE SYST-DATUM.                              " 第8周
*  DATA:DATE_9 TYPE SYST-DATUM.                              " 第9周
*  DATA:DATE_10 TYPE SYST-DATUM.                             " 第10周
*  DATA:DATE_11 TYPE SYST-DATUM.                             " 第11周
*  DATA:DATE_12 TYPE SYST-DATUM.                             " 第12周
*  DATA:DATE_13 TYPE SYST-DATUM.                             " 第13周
*  DATA:DATE_14 TYPE SYST-DATUM.                             " 第14周
*  DATA:DATE_15 TYPE SYST-DATUM.                             " 第15周
*  DATA:DATE_16 TYPE SYST-DATUM.                             " 第16周

*  DATE_1  = DATE2 + 1.
*  DATE_2 = DATE_1 + 7.
*  DATE_3 = DATE_2 + 7.
*  DATE_4 = DATE_3 + 7.
*  DATE_5 = DATE_4 + 7.
*  DATE_6 = DATE_5 + 7.
*  DATE_7 = DATE_6 + 7.
*  DATE_8 = DATE_7 + 7.
*  DATE_9 = DATE_8 + 7.
*  DATE_10 = DATE_9 + 7.
*  DATE_11 = DATE_10 + 7.
*  DATE_12 = DATE_11 + 7.
*  DATE_13 = DATE_12 + 7.
*  DATE_14 = DATE_13 + 7.
*  DATE_15 = DATE_14 + 7.
*  DATE_16 = DATE_15 + 7.


  IF GT_MARC[] IS NOT INITIAL.
    SELECT *
      FROM MAKT
      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
      FOR ALL ENTRIES IN GT_MARC
      WHERE MATNR = GT_MARC-MATNR
      AND SPRAS = SY-LANGU.
  ENDIF.

  SORT GT_MARC BY MATNR.
  LOOP AT GT_MARC.
*需求数量
*    CLEAR:
*    L_MNG_1,
*    L_MNG_2,
*    L_MNG_3,
*    L_MNG_4,
*    L_MNG_5,
*    L_MNG_6,
*    L_MNG_7,
*    L_MNG_8,
*    L_MNG_9,
*    L_MNG_10,
*    L_MNG_11,
*    L_MNG_12,
*    L_MNG_13,
*    L_MNG_14,
*    L_MNG_15,
*    L_MNG_16.

    CLEAR L_FIELDNAME.
    LOOP AT GT_TAB WHERE PLUMI = '-' AND MATNR = GT_MARC-MATNR AND WERKS = GT_MARC-WERKS.

      IF GT_TAB-DAT00 BETWEEN DATE1 AND DATE2." 按天
        CONCATENATE 'DYNAMIC_MENGE' GT_TAB-DAT00 INTO L_FIELDNAME.
        ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
        IF GT_TAB-MNG01 IS NOT INITIAL.
          <DYN_FIELD> = GT_TAB-MNG01.
        ENDIF.
      ENDIF.
    ENDLOOP.
*
*      IF GT_TAB-DAT00 >= DATE2 AND GT_TAB-DAT00 < DATE_1.   " 按1周
*        L_MNG_1 = L_MNG_1 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_1 AND GT_TAB-DAT00 < DATE_2 . " 按2周
*        L_MNG_2 = L_MNG_2 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_2 AND GT_TAB-DAT00 < DATE_3 . " 按3周
*        L_MNG_3 = L_MNG_3 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_3 AND GT_TAB-DAT00 < DATE_4 . " 按4周
*        L_MNG_4 = L_MNG_4 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_4 AND GT_TAB-DAT00 < DATE_5 . " 按5周
*        L_MNG_5 = L_MNG_5 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_5 AND GT_TAB-DAT00 < DATE_6 . " 按6周
*        L_MNG_6 = L_MNG_6 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_6 AND GT_TAB-DAT00 < DATE_7 . " 按7周
*        L_MNG_7 = L_MNG_7 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_7 AND GT_TAB-DAT00 < DATE_8 . " 按8周
*        L_MNG_8 = L_MNG_8 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_8 AND GT_TAB-DAT00 < DATE_9 . " 按9周
*        L_MNG_9 = L_MNG_9 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_9 AND GT_TAB-DAT00 < DATE_10 . " 按10周
*        L_MNG_10 = L_MNG_10 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_10 AND GT_TAB-DAT00 < DATE_11 . " 按11周
*        L_MNG_11 = L_MNG_11 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_11 AND GT_TAB-DAT00 < DATE_12 . " 按12周
*        L_MNG_12 = L_MNG_12 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_12 AND GT_TAB-DAT00 < DATE_13 . " 按13周
*        L_MNG_13 = L_MNG_13 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_13 AND GT_TAB-DAT00 < DATE_14 . " 按14周
*        L_MNG_14 = L_MNG_14 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_14 AND GT_TAB-DAT00 < DATE_15 . " 按15周
*        L_MNG_15 = L_MNG_15 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_15 AND GT_TAB-DAT00 < DATE_16 . " 按16周
*        L_MNG_16 = L_MNG_16 + GT_TAB-MNG01.
*      ENDIF.
*
*    ENDLOOP.

*    CONCATENATE 'DYNAMIC_MENGE' DATE_1 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_1 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_1.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_2 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_2 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_2.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_3 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_3 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_3.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_4 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_4 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_4.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_5 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_5 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_5.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_6 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_6 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_6.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_7 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_7 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_7.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_8 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_8 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_8.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_9 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_9 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_9.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_10 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_10 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_10.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_11 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_11 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_11.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_12 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_12 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_12.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_13 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_13 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_13.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_14 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_14 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_14.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_15 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_15 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_15.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_16 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_16 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_16.
*    ENDIF.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATNR.

    ASSIGN COMPONENT 'MATKL' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATKL.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-WERKS.

    ASSIGN COMPONENT 'PLIFZ' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-PLIFZ.

    ASSIGN COMPONENT 'ZGXLX_MS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = '需求数量'.

    ASSIGN COMPONENT 'MNGKC' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> =  0.

    READ TABLE LT_MAKT WITH KEY MATNR = GT_MARC-MATNR.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      <DYN_FIELD> =  LT_MAKT-MAKTX.
    ENDIF.

    APPEND <DYN_WA> TO <DYN_TABLE>.
    CLEAR <DYN_WA>.

*供给数量
*需求数量
*    CLEAR:
*    L_MNG_1,
*    L_MNG_2,
*    L_MNG_3,
*    L_MNG_4,
*    L_MNG_5,
*    L_MNG_6,
*    L_MNG_7,
*    L_MNG_8,
*    L_MNG_9,
*    L_MNG_10,
*    L_MNG_11,
*    L_MNG_12,
*    L_MNG_13,
*    L_MNG_14,
*    L_MNG_15,
*    L_MNG_16.

    CLEAR L_FIELDNAME.
    LOOP AT GT_TAB WHERE PLUMI = '+' AND MATNR = GT_MARC-MATNR AND WERKS = GT_MARC-WERKS.
      IF GT_TAB-DAT00 BETWEEN DATE1 AND DATE2." 按天
        CONCATENATE 'DYNAMIC_MENGE' GT_TAB-DAT00 INTO L_FIELDNAME.
        ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
        IF GT_TAB-MNG01 IS NOT INITIAL.
          <DYN_FIELD> = GT_TAB-MNG01.
        ENDIF.
      ENDIF.
    ENDLOOP.

*      IF GT_TAB-DAT00 >= DATE2 AND GT_TAB-DAT00 < DATE_1.   " 按1周
*        L_MNG_1 = L_MNG_1 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_1 AND GT_TAB-DAT00 < DATE_2 . " 按2周
*        L_MNG_2 = L_MNG_2 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_2 AND GT_TAB-DAT00 < DATE_3 . " 按3周
*        L_MNG_3 = L_MNG_3 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_3 AND GT_TAB-DAT00 < DATE_4 . " 按4周
*        L_MNG_4 = L_MNG_4 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_4 AND GT_TAB-DAT00 < DATE_5 . " 按5周
*        L_MNG_5 = L_MNG_5 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_5 AND GT_TAB-DAT00 < DATE_6 . " 按6周
*        L_MNG_6 = L_MNG_6 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_6 AND GT_TAB-DAT00 < DATE_7 . " 按7周
*        L_MNG_7 = L_MNG_7 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_7 AND GT_TAB-DAT00 < DATE_8 . " 按8周
*        L_MNG_8 = L_MNG_8 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_8 AND GT_TAB-DAT00 < DATE_9 . " 按9周
*        L_MNG_9 = L_MNG_9 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_9 AND GT_TAB-DAT00 < DATE_10 . " 按10周
*        L_MNG_10 = L_MNG_10 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_10 AND GT_TAB-DAT00 < DATE_11 . " 按11周
*        L_MNG_11 = L_MNG_11 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_11 AND GT_TAB-DAT00 < DATE_12 . " 按12周
*        L_MNG_12 = L_MNG_12 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_12 AND GT_TAB-DAT00 < DATE_13 . " 按13周
*        L_MNG_13 = L_MNG_13 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_13 AND GT_TAB-DAT00 < DATE_14 . " 按14周
*        L_MNG_14 = L_MNG_14 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_14 AND GT_TAB-DAT00 < DATE_15 . " 按15周
*        L_MNG_15 = L_MNG_15 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_15 AND GT_TAB-DAT00 < DATE_16 . " 按16周
*        L_MNG_16 = L_MNG_16 + GT_TAB-MNG01.
*      ENDIF.
*
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_1 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_1 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_1.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_2 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_2 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_2.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_3 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_3 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_3.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_4 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_4 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_4.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_5 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_5 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_5.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_6 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_6 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_6.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_7 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_7 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_7.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_8 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_8 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_8.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_9 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_9 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_9.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_10 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_10 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_10.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_11 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_11 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_11.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_12 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_12 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_12.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_13 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_13 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_13.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_14 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_14 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_14.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_15 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_15 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_15.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_16 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_16 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_16.
*    ENDIF.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATNR.

    ASSIGN COMPONENT 'MATKL' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATKL.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-WERKS.

    ASSIGN COMPONENT 'PLIFZ' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-PLIFZ.

    ASSIGN COMPONENT 'ZGXLX_MS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = '供给数量'.

    READ TABLE GT_TAB_B WITH KEY MATNR = GT_MARC-MATNR
                                 WERKS = GT_MARC-WERKS.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'MNGKC' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      <DYN_FIELD> =  GT_TAB_B-MNGKC.
    ENDIF.

    READ TABLE LT_MAKT WITH KEY MATNR = GT_MARC-MATNR.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      <DYN_FIELD> =  LT_MAKT-MAKTX.
    ENDIF.

    APPEND <DYN_WA> TO <DYN_TABLE>.
    CLEAR <DYN_WA>.


    DATA:OLD_MNG01 TYPE MDEZ-MNG01.

*可用数量
*    CLEAR:
*    L_MNG_1,
*    L_MNG_2,
*    L_MNG_3,
*    L_MNG_4,
*    L_MNG_5,
*    L_MNG_6,
*    L_MNG_7,
*    L_MNG_8,
*    L_MNG_9,
*    L_MNG_10,
*    L_MNG_11,
*    L_MNG_12,
*    L_MNG_13,
*    L_MNG_14,
*    L_MNG_15,
*    L_MNG_16.
    CLEAR:
    L_DAY_1,
    L_DAY_2,
    L_DAY_3,
    L_DAY_4,
    L_DAY_5,
    L_DAY_6,
    L_DAY_7,
    L_DAY_8,
    L_DAY_9,
    L_DAY_10,
    L_DAY_11,
    L_DAY_12,
    L_DAY_13,
    L_DAY_14,
    L_DAY_15,
    L_DAY_16,
    L_DAY_17,
    L_DAY_18,
    L_DAY_19,
    L_DAY_20,
    L_DAY_21,
    L_DAY_22,
    L_DAY_23,
    L_DAY_24,
    L_DAY_25,
    L_DAY_26,
    L_DAY_27,
    L_DAY_28,
    L_DAY_29,
    L_DAY_30,
    L_DAY_31.

*可用天数，当可用数量为负数的，记录日期
    DATA L_DATE  TYPE CHAR10. "检查是否是第一次日期
    DATA L_CHECK TYPE C. "检查是否是第一次标识

    LOOP AT GT_TAB WHERE  MATNR = GT_MARC-MATNR AND WERKS = GT_MARC-WERKS.

      IF GT_TAB-DAT00 BETWEEN DATE1 AND DATE2." 按天
        CONCATENATE 'DYNAMIC_MENGE' GT_TAB-DAT00 INTO L_FIELDNAME.
        ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.

        IF  GT_TAB-DAT00 - DATE1  = 0.                      " day1
          L_DAY_1 = L_DAY_1 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_1.

        ELSEIF GT_TAB-DAT00 - DATE1 = 1.                    " day2
          L_DAY_2 = L_DAY_2 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_2.

        ELSEIF GT_TAB-DAT00 - DATE1 = 2.                    " day3
          L_DAY_3 = L_DAY_3 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_3.

        ELSEIF GT_TAB-DAT00 - DATE1 = 3.                    " day4
          L_DAY_4 = L_DAY_4 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_4.

        ELSEIF GT_TAB-DAT00 - DATE1 = 4.                    " day5
          L_DAY_5 = L_DAY_5 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_5.

        ELSEIF GT_TAB-DAT00 - DATE1 = 5.                    " day6
          L_DAY_6 = L_DAY_6 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_6.

        ELSEIF GT_TAB-DAT00 - DATE1 = 6.                    " day7
          L_DAY_7 = L_DAY_7 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_7.

        ELSEIF GT_TAB-DAT00 - DATE1 = 7.                    " day8
          L_DAY_8 = L_DAY_8 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_8.

        ELSEIF GT_TAB-DAT00 - DATE1 = 8.                    " day9
          L_DAY_9 = L_DAY_9 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_9.

        ELSEIF GT_TAB-DAT00 - DATE1 = 9.                    " day10
          L_DAY_10 = L_DAY_10 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_10.

        ELSEIF GT_TAB-DAT00 - DATE1 = 10.                   " day11
          L_DAY_11 = L_DAY_11 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_11.

        ELSEIF GT_TAB-DAT00 - DATE1 = 11.                   " day12
          L_DAY_12 = L_DAY_12 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_12.

        ELSEIF GT_TAB-DAT00 - DATE1 = 12.                   " day13
          L_DAY_13 = L_DAY_13 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_13.

        ELSEIF GT_TAB-DAT00 - DATE1 = 13.                   " day14
          L_DAY_14 = L_DAY_14 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_14.

        ELSEIF GT_TAB-DAT00 - DATE1 = 14.                   " day15
          L_DAY_15 = L_DAY_15 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_15.
        ENDIF.

        IF  GT_TAB-DAT00 - DATE1    = 15.                      " day16
          L_DAY_16 = L_DAY_16 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_16.

        ELSEIF GT_TAB-DAT00 - DATE1 = 17.                    " day17
          L_DAY_17 = L_DAY_17 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_17.

        ELSEIF GT_TAB-DAT00 - DATE1 = 18.                    " day18
          L_DAY_19 = L_DAY_19 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_19.

        ELSEIF GT_TAB-DAT00 - DATE1 = 19.                    " day19
          L_DAY_20 = L_DAY_20 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_20.

        ELSEIF GT_TAB-DAT00 - DATE1 = 20.                    " day20
          L_DAY_21 = L_DAY_21 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_21.

        ELSEIF GT_TAB-DAT00 - DATE1 = 21.                    " day21
          L_DAY_22 = L_DAY_22 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_22.

        ELSEIF GT_TAB-DAT00 - DATE1 = 22.                    " day22
          L_DAY_23 = L_DAY_23 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_23.

        ELSEIF GT_TAB-DAT00 - DATE1 = 23.                    " day23
          L_DAY_24 = L_DAY_24 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_24.

        ELSEIF GT_TAB-DAT00 - DATE1 = 24.                    " day24
          L_DAY_25 = L_DAY_25 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_25.

        ELSEIF GT_TAB-DAT00 - DATE1 = 25.                    " day25
          L_DAY_26 = L_DAY_26 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_26.

        ELSEIF GT_TAB-DAT00 - DATE1 = 26.                   " day26
          L_DAY_27 = L_DAY_27 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_27.

        ELSEIF GT_TAB-DAT00 - DATE1 = 27.                   " day27
          L_DAY_28 = L_DAY_28 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_28.

        ELSEIF GT_TAB-DAT00 - DATE1 = 28.                   " day28
          L_DAY_29 = L_DAY_29 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_29.

        ELSEIF GT_TAB-DAT00 - DATE1 = 29.                   " day29
          L_DAY_30 = L_DAY_30 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_30.

        ELSEIF GT_TAB-DAT00 - DATE1 = 30.                   " day30
          L_DAY_31 = L_DAY_31 + GT_TAB-MNG01.
          <DYN_FIELD> = L_DAY_31.
        ENDIF.
      ENDIF.
    ENDLOOP.

*      IF GT_TAB-DAT00 >= DATE2 AND GT_TAB-DAT00 < DATE_1.   " 按1周
*        L_MNG_1 = L_MNG_1 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_1 AND GT_TAB-DAT00 < DATE_2 . " 按2周
*        L_MNG_2 = L_MNG_2 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_2 AND GT_TAB-DAT00 < DATE_3 . " 按3周
*        L_MNG_3 = L_MNG_3 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_3 AND GT_TAB-DAT00 < DATE_4 . " 按4周
*        L_MNG_4 = L_MNG_4 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_4 AND GT_TAB-DAT00 < DATE_5 . " 按5周
*        L_MNG_5 = L_MNG_5 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_5 AND GT_TAB-DAT00 < DATE_6 . " 按6周
*        L_MNG_6 = L_MNG_6 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_6 AND GT_TAB-DAT00 < DATE_7 . " 按7周
*        L_MNG_7 = L_MNG_7 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_7 AND GT_TAB-DAT00 < DATE_8 . " 按8周
*        L_MNG_8 = L_MNG_8 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_8 AND GT_TAB-DAT00 < DATE_9 . " 按9周
*        L_MNG_9 = L_MNG_9 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_9 AND GT_TAB-DAT00 < DATE_10 . " 按10周
*        L_MNG_10 = L_MNG_10 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_10 AND GT_TAB-DAT00 < DATE_11 . " 按11周
*        L_MNG_11 = L_MNG_11 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_11 AND GT_TAB-DAT00 < DATE_12 . " 按12周
*        L_MNG_12 = L_MNG_12 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_12 AND GT_TAB-DAT00 < DATE_13 . " 按13周
*        L_MNG_13 = L_MNG_13 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_13 AND GT_TAB-DAT00 < DATE_14 . " 按14周
*        L_MNG_14 = L_MNG_14 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_14 AND GT_TAB-DAT00 < DATE_15 . " 按15周
*        L_MNG_15 = L_MNG_15 + GT_TAB-MNG01.
*      ENDIF.
*
*      IF GT_TAB-DAT00 >= DATE_15 AND GT_TAB-DAT00 < DATE_16 . " 按16周
*        L_MNG_16 = L_MNG_16 + GT_TAB-MNG01.
*      ENDIF.

*    CONCATENATE 'DYNAMIC_MENGE' DATE_1 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_1 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_1.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_2 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_2 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_2.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_3 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_3 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_3.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_4 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_4 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_4.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_5 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_5 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_5.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_6 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_6 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_6.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_7 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_7 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_7.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_8 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_8 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_8.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_9 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_9 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_9.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_10 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_10 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_10.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_11 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_11 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_11.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_12 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_12 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_12.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_13 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_13 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_13.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_14 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_14 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_14.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_15 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_15 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_15.
*    ENDIF.
*
*    CONCATENATE 'DYNAMIC_MENGE' DATE_16 INTO L_FIELDNAME.
*    ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*    IF L_MNG_16 IS NOT INITIAL.
*      <DYN_FIELD> = L_MNG_16.
*    ENDIF.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATNR.

    ASSIGN COMPONENT 'MATKL' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-MATKL.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-WERKS.

    ASSIGN COMPONENT 'PLIFZ' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = GT_MARC-PLIFZ.

    ASSIGN COMPONENT 'ZGXLX_MS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
    <DYN_FIELD> = '可用数量'.

    READ TABLE GT_TAB_B WITH KEY MATNR = GT_MARC-MATNR
                                 WERKS = GT_MARC-WERKS.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'MNGKC' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      <DYN_FIELD> =  GT_TAB_B-MNGKC.
    ENDIF.

    READ TABLE LT_MAKT WITH KEY MATNR = GT_MARC-MATNR.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      <DYN_FIELD> =  LT_MAKT-MAKTX.
    ENDIF.

    APPEND <DYN_WA> TO <DYN_TABLE>.
    CLEAR <DYN_WA>.
  ENDLOOP.

* 可用数量 = 前一列可用数量 + 本列可用数量
  DATA:BEFORE_MNG01 TYPE MDEZ-MNG01.
  DATA:MNGKC TYPE MDEZ-MNG01.
  DATA:DATE_COUNT TYPE SYST-DATUM.

  FIELD-SYMBOLS:<FS> TYPE TY_TAB.
  DATA: L_COUNT TYPE I.

  L_COUNT = DATE2 - DATE1 + 1.

  LOOP AT <DYN_TABLE> ASSIGNING <DYN_WA> .
    ASSIGN COMPONENT 'ZGXLX_MS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.

    IF <DYN_FIELD> = '可用数量'.
      BEFORE_MNG01 = 0.
      DATE_COUNT = DATE1. " 列开始日期

      CLEAR L_CHECK.

      DO L_COUNT TIMES. " 循环按天计算，加上前一列数量
        IF DATE_COUNT = DATE1. " 如果是开始列日期
          CLEAR MNGKC.
          ASSIGN COMPONENT  'MNGKC' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
          BEFORE_MNG01 = <DYN_FIELD>.

          CONCATENATE 'DYNAMIC_MENGE' DATE1 INTO L_FIELDNAME.
          ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
          BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
          <DYN_FIELD>  = BEFORE_MNG01.
        ELSE.

          CONCATENATE 'DYNAMIC_MENGE' DATE_COUNT INTO L_FIELDNAME.
          ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
          BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
          <DYN_FIELD> =  BEFORE_MNG01.
        ENDIF.

*可用天数
        IF L_CHECK <> 'X' .
          IF BEFORE_MNG01 < 0.
            ASSIGN COMPONENT 'KYTS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*第一次为负的日期-当前日期
            L_DATE      =  ABS( SY-DATUM - DATE_COUNT ) .
            <DYN_FIELD> =  ABS( SY-DATUM - DATE_COUNT ) .
            L_CHECK = 'X'.
          ELSE.
            ASSIGN COMPONENT 'KYTS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
            <DYN_FIELD> = '999'.
          ENDIF.
        ELSE.
          ASSIGN COMPONENT 'KYTS' OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*第一次为负的日期-当前日期
          <DYN_FIELD> = L_DATE.
        ENDIF.

        DATE_COUNT = DATE_COUNT + 1." 日期累计
      ENDDO.

    ENDIF.
  ENDLOOP.
*      " 按周计算，加上前一列数量
*      CONCATENATE 'DYNAMIC_MENGE' DATE_1 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_2 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_3 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_4 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_5 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_6 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_7 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_8 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_9 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_10 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_11 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_12 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_13 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_14 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_15 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*      CONCATENATE 'DYNAMIC_MENGE' DATE_16 INTO L_FIELDNAME.
*      ASSIGN COMPONENT L_FIELDNAME OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
*      BEFORE_MNG01 =  <DYN_FIELD> + BEFORE_MNG01.
*      <DYN_FIELD> = BEFORE_MNG01  .
*
*    ENDIF.
*


ENDFORM.                    " WRITE_DATA_TO_DYNTABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_INIT_LAYOUT .
  WA_LAYOUT-CWIDTH_OPT = 'X'.
ENDFORM.                    " FRM_INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .
* ALV显示函数
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      IS_LAYOUT_LVC            = WA_LAYOUT
      IT_FIELDCAT_LVC          = IT_STRUCTURE
*     i_grid_title             = g_title
*     IT_EXCLUDING             =       "系统自带STATUS图标控制内表
*     I_SAVE                   = CNS_X
      I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
    TABLES
      T_OUTTAB                 = <DYN_TABLE>
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FRM_SHOW_ALV

FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_FCAT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM FRM_ALV_FCAT_SET  USING    PU_POS    TYPE I
                                PU_FNAME  TYPE C
                                PU_FTEXT  TYPE C
                                PU_RFIELD TYPE C
                                PU_RTABLE TYPE C
                                PU_QNAME  TYPE C
                                PU_OUTPUTLEN   TYPE C
                                PU_DATATYPE TYPE C
                                PU_NO_ZERO TYPE C.
  CLEAR WA_STRUCTURE.
  WA_STRUCTURE-COL_POS    = PU_POS.
  WA_STRUCTURE-FIELDNAME  = PU_FNAME.
  WA_STRUCTURE-SCRTEXT_L  = PU_FTEXT.
  WA_STRUCTURE-SCRTEXT_M  = PU_FTEXT.
  WA_STRUCTURE-SCRTEXT_S  = PU_FTEXT.
  WA_STRUCTURE-REF_FIELD  = PU_RFIELD.
  WA_STRUCTURE-REF_TABLE  = PU_RTABLE.
  WA_STRUCTURE-QFIELDNAME = PU_QNAME.
  WA_STRUCTURE-OUTPUTLEN  = PU_OUTPUTLEN.
  WA_STRUCTURE-KEY = 'X'.
  WA_STRUCTURE-DATATYPE = PU_DATATYPE.
  WA_STRUCTURE-NO_ZERO = PU_NO_ZERO.
  APPEND WA_STRUCTURE TO IT_STRUCTURE.


ENDFORM.                    " FRM_ALV_FCAT_SET

*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
*           ID 'ACTVT' FIELD '03'
           ID 'WERKS' FIELD  S_WERKS-LOW.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH S_WERKS-LOW.
  ENDIF.
ENDFORM.
