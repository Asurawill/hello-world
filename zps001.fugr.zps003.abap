FUNCTION ZPS003.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_DYXTM) TYPE  ZDYXTM
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  ZPS003_01
*"  TABLES
*"      I_CNECP STRUCTURE  ZPS003
*"      E_MSG STRUCTURE  ZPS003_02 OPTIONAL
*"----------------------------------------------------------------------

* 字段校验
  DATA L_TABIX(10) TYPE C .
  LOOP AT I_CNECP .
    L_TABIX = SY-TABIX .
    IF I_CNECP-CBYS IS NOT INITIAL AND I_CNECP-CBYSMC IS INITIAL .
      E_MSG-TYPE = 'E'.
      CONCATENATE '第' L_TABIX '行数据的成本预算描述字段不能为空' INTO E_MSG-MESSAGE .
      APPEND E_MSG .
      CLEAR E_MSG .
      E_RETURN-TYPE = 'E'.
      E_RETURN-MESSAGE = '项目预算导入失败！'.
      RETURN .
    ENDIF.
  ENDLOOP.

  TYPES:BEGIN OF TY_CHECK,
          POSID   TYPE PRPS-POSID,              "WBS
          XMLB(1) TYPE C,                       "项目类别
          CBYS    TYPE CKIS-KSTAR,              "成本要素
          WERKS   TYPE PRPS-WERKS,              "工厂
        END OF TY_CHECK.
  DATA:GT_C_OUTPUT_CHECK TYPE TABLE OF TY_CHECK WITH HEADER LINE.
  DATA:DR_LEN TYPE I VALUE IS INITIAL .

  DATA LT_CNECP TYPE TABLE OF ZPS003 WITH HEADER LINE .
  DATA LT_CNECP1 TYPE TABLE OF ZPS003 WITH HEADER LINE .

  IF I_CNECP[] IS NOT INITIAL .

    LT_CNECP[] = I_CNECP[] .

* 导入的数据没有对应的项目定义！   一次只可以导入一个项目的预算！
    MOVE-CORRESPONDING LT_CNECP[] TO GT_C_OUTPUT_CHECK[] .
    SORT GT_C_OUTPUT_CHECK BY POSID .
    DELETE ADJACENT DUPLICATES FROM GT_C_OUTPUT_CHECK COMPARING POSID .
    DESCRIBE TABLE GT_C_OUTPUT_CHECK LINES DR_LEN .
    DATA:BEGIN OF LS_PRJ,
           PSPNR1 TYPE PRPS-PSPNR,
           POSID  TYPE PRPS-POSID,
           PSPHI  TYPE PRPS-PSPHI,
           PSPNR2 TYPE PROJ-PSPNR,
           PSPID  TYPE PROJ-PSPID,
         END OF LS_PRJ.
    DATA LT_PRJ LIKE TABLE OF LS_PRJ .

    IF DR_LEN <> 1 .
      SELECT R~PSPNR AS PSPNR1
             R~POSID
             R~PSPHI
             J~PSPNR AS PSPNR2
             J~PSPID
        INTO CORRESPONDING FIELDS OF TABLE LT_PRJ
        FROM PRPS AS R
       INNER JOIN PROJ AS J
          ON R~PSPHI = J~PSPNR
         FOR ALL ENTRIES IN GT_C_OUTPUT_CHECK
       WHERE R~POSID = GT_C_OUTPUT_CHECK-POSID .
      IF LT_PRJ IS INITIAL .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = '导入的数据没有对应的项目定义！'.
        APPEND E_MSG .
        CLEAR E_MSG .
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '项目预算导入失败！'.
        RETURN .
      ENDIF.
      SORT LT_PRJ BY PSPNR2 .
      DELETE ADJACENT DUPLICATES FROM LT_PRJ COMPARING PSPNR2 .
      CLEAR DR_LEN .
      DESCRIBE TABLE LT_PRJ LINES DR_LEN .
      IF DR_LEN > 1 .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = '一次只可以导入一个项目的预算！'.
        APPEND E_MSG .
        CLEAR E_MSG .
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '项目预算导入失败！'.
        RETURN .
      ENDIF.
    ENDIF.

* 物料和项目不为同一公司，请检查！
    REFRESH GT_C_OUTPUT_CHECK[] .
    CLEAR GT_C_OUTPUT_CHECK .
    MOVE-CORRESPONDING LT_CNECP[] TO GT_C_OUTPUT_CHECK[] .
    DELETE GT_C_OUTPUT_CHECK WHERE XMLB = 'V'.
    DATA:BEGIN OF LS_PRPS,
           PSPNR TYPE PRPS-PSPNR,
           POSID TYPE PRPS-POSID,
           WERKS TYPE PRPS-WERKS,
         END OF LS_PRPS.
    DATA LT_PRPS LIKE TABLE OF LS_PRPS .

    SELECT PSPNR
           POSID
           WERKS
      INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
      FROM PRPS
       FOR ALL ENTRIES IN GT_C_OUTPUT_CHECK[]
     WHERE POSID = GT_C_OUTPUT_CHECK-POSID .
    SORT LT_PRPS BY POSID .
    LOOP AT GT_C_OUTPUT_CHECK .
      READ TABLE LT_PRPS INTO LS_PRPS WITH KEY POSID = GT_C_OUTPUT_CHECK-WERKS BINARY SEARCH .
      IF SY-SUBRC = 0 .
        IF LS_PRPS-WERKS <> GT_C_OUTPUT_CHECK-WERKS .
          E_MSG-TYPE = 'E'.
          E_MSG-MESSAGE = '物料和项目不为同一公司，请检查！'.
          APPEND E_MSG .
          CLEAR E_MSG .
          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '项目预算导入失败！'.
          RETURN .
        ENDIF.
        CLEAR LS_PRPS .
      ENDIF.
    ENDLOOP.
    REFRESH GT_C_OUTPUT_CHECK[] .
    CLEAR GT_C_OUTPUT_CHECK .
    REFRESH LT_PRPS .

* 同一个WBS元素下只能出现一个成本要素！
    MOVE-CORRESPONDING LT_CNECP[] TO GT_C_OUTPUT_CHECK[] .
    DELETE GT_C_OUTPUT_CHECK WHERE XMLB = 'M'.
    SORT GT_C_OUTPUT_CHECK BY CBYS .
    DATA LS_C_OUTPUT_CHECK TYPE TY_CHECK .

    LOOP AT GT_C_OUTPUT_CHECK INTO LS_C_OUTPUT_CHECK WHERE XMLB = 'V'.
      CLEAR:DR_LEN .
      LOOP AT GT_C_OUTPUT_CHECK WHERE CBYS = GT_C_OUTPUT_CHECK-CBYS .
        IF DR_LEN = 0.
          DR_LEN = DR_LEN + 1 .
        ELSE.
          IF GT_C_OUTPUT_CHECK-POSID = LS_C_OUTPUT_CHECK-POSID .
            E_MSG-TYPE = 'E'.
            E_MSG-MESSAGE = '同一个WBS元素下只能出现一个成本要素！'.
            APPEND E_MSG .
            CLEAR E_MSG .
            E_RETURN-TYPE = 'E'.
            E_RETURN-MESSAGE = '项目预算导入失败！'.
            RETURN .
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR LS_C_OUTPUT_CHECK .
    ENDLOOP.

**********************************************************************

    TYPES:BEGIN OF TY_PSPHI,
            PSPHI TYPE PRPS-PSPHI,
          END OF TY_PSPHI.
    TYPES:BEGIN OF TY_OBJID,
            TOPNR TYPE PRECP1-TOPNR,
            VERSN TYPE PRECP1-VERSN,
          END OF TY_OBJID.
    TYPES:BEGIN OF TY_VERSN,
            VERSN TYPE PRECP1-VERSN,
          END OF TY_VERSN.
    DATA:GT_PSPHI          TYPE TABLE OF TY_PSPHI WITH HEADER LINE.
    DATA:GT_OBJID1         TYPE TABLE OF TY_OBJID WITH HEADER LINE.
    DATA:GT_OBJID2         TYPE TABLE OF TY_OBJID WITH HEADER LINE.
    DATA:GT_VERSN1         TYPE TABLE OF TY_VERSN WITH HEADER LINE.           "定义放置版本号的内表和工作区
    DATA:GT_VERSN2         TYPE TABLE OF TY_VERSN WITH HEADER LINE.           "定义放置版本号的内表和工作区

    SELECT PSPHI
      FROM PRPS
      INTO CORRESPONDING FIELDS OF TABLE GT_PSPHI
       FOR ALL ENTRIES IN LT_CNECP[]
     WHERE POSID = LT_CNECP-POSID.
    IF GT_PSPHI[] IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM GT_PSPHI COMPARING PSPHI .
      READ TABLE GT_PSPHI INDEX 1 .
      SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
        INTO CORRESPONDING FIELDS OF TABLE GT_OBJID1
        FROM PRECP1
        JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
        WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

      IF GT_OBJID1[] IS NOT INITIAL.
        SELECT VERSN "max( versn ) as versn
          INTO CORRESPONDING FIELDS OF TABLE GT_VERSN1                        "取一个对象编号下对应的当前最大的版本号
          FROM PRECP1
           FOR ALL ENTRIES IN GT_OBJID1
         WHERE TOPNR = GT_OBJID1-TOPNR
            AND VERSN BETWEEN '000' AND '099'.
      ENDIF.

      SELECT TOPNR VERSN                                            "取出了项目定义与用户输入的WBS相等的情况下的对象编号、项目定义、版本号
        INTO CORRESPONDING FIELDS OF TABLE GT_OBJID2
        FROM PRECP1
        JOIN PROJ ON PRECP1~TOPNR = PROJ~OBJNR
        WHERE PROJ~PSPNR = GT_PSPHI-PSPHI.

      IF GT_OBJID2[] IS NOT INITIAL.
        SELECT VERSN "max( versn ) as versn
          INTO CORRESPONDING FIELDS OF TABLE GT_VERSN2                        "取一个对象编号下对应的当前最大的版本号
          FROM PRECP1
          FOR ALL ENTRIES IN GT_OBJID2
          WHERE TOPNR = GT_OBJID2-TOPNR
            AND VERSN BETWEEN '100' AND '199'.
      ENDIF.
    ENDIF.

**********************************************************************
    REFRESH:GT_OBJID1,GT_OBJID2.

    DATA:P_BB1 TYPE PRECP1-VERSN.

    SORT GT_VERSN1 BY VERSN.
    SORT GT_VERSN2 BY VERSN.

    IF GT_VERSN1[] IS NOT INITIAL.
      LOOP AT GT_VERSN1 WHERE VERSN >= '000' AND VERSN <= '099'.
        AT END OF VERSN.                                                  "取出了最大的版本号
          GT_VERSN1-VERSN = GT_VERSN1-VERSN + 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GT_VERSN1-VERSN
            IMPORTING
              OUTPUT = GT_VERSN1-VERSN.

          P_BB1 = GT_VERSN1-VERSN.
        ENDAT.
      ENDLOOP.
    ELSEIF GT_VERSN1[] IS INITIAL.
      P_BB1 = '001'.
    ENDIF.

    DATA:P_BB2 TYPE PRECP1-VERSN.

    IF GT_VERSN2[] IS NOT INITIAL.
      LOOP AT GT_VERSN2 WHERE VERSN >= '100' AND VERSN <= '199'.
        AT END OF VERSN.                                                  "取出了最大的版本号
          GT_VERSN2-VERSN = GT_VERSN2-VERSN + 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GT_VERSN2-VERSN
            IMPORTING
              OUTPUT = GT_VERSN2-VERSN.

          P_BB2 = GT_VERSN2-VERSN.
        ENDAT.
      ENDLOOP.
    ELSE.
      P_BB2 = 101.
    ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:34:01  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:34:01  END
    SORT LT_CNECP[] BY MATNR.

    TYPES:BEGIN OF TY_JG,
            MATNR TYPE CKIS-MATNR,
            HTDJ  TYPE CKIS-GPREIS,
            CLDJ  TYPE P LENGTH 16 DECIMALS 4,
            RGDJ  TYPE CKIS-FPREIS,
          END OF TY_JG.
    DATA:IT_JG TYPE TABLE OF TY_JG,
         WA_JG TYPE TY_JG.
    TYPES:BEGIN OF TY_JGHJ,
            MATNR   TYPE CKIS-MATNR,
            HGPREIS TYPE CKIS-GPREIS,
            CGPREIS TYPE P LENGTH 16 DECIMALS 4,
            FPREIS  TYPE CKIS-FPREIS,
          END OF TY_JGHJ.
    DATA:IT_JGHJ TYPE TABLE OF TY_JGHJ,
         WA_JGHJ TYPE TY_JGHJ.
    DATA:MESSAGE1(200) TYPE C,
         MESSAGE2(200) TYPE C,
         MESSAGE3(200) TYPE C.
    DATA:g(50)  TYPE C,
         w(50)  TYPE C,
         LL(50) TYPE C,
         JJ(50) TYPE C.

    REFRESH IT_JG.
    LOOP AT LT_CNECP.
      AT NEW MATNR.
        READ TABLE LT_CNECP INDEX SY-TABIX.
        IF SY-SUBRC = 0.
          WA_JG-MATNR = LT_CNECP-MATNR.
          WA_JG-HTDJ = LT_CNECP-HGPREIS.
          WA_JG-CLDJ = LT_CNECP-CGPREIS.
          WA_JG-RGDJ = LT_CNECP-FPREIS.
          APPEND WA_JG TO IT_JG.
        ENDIF.
      ENDAT.
      CLEAR LT_CNECP.
    ENDLOOP.

    REFRESH IT_JGHJ.
    LOOP AT LT_CNECP WHERE MATNR <> ''.
      CLEAR WA_JGHJ.
      WA_JGHJ-MATNR = LT_CNECP-MATNR.
      WA_JGHJ-HGPREIS = LT_CNECP-HGPREIS.
      WA_JGHJ-CGPREIS = LT_CNECP-CGPREIS.
      WA_JGHJ-FPREIS = LT_CNECP-FPREIS.
      COLLECT WA_JGHJ INTO IT_JGHJ.
    ENDLOOP.

    LOOP AT IT_JG INTO WA_JG.
      READ TABLE IT_JGHJ INTO WA_JGHJ WITH KEY MATNR = WA_JG-MATNR.
      IF SY-SUBRC = 0.
        CLEAR G.
        LOOP AT LT_CNECP WHERE MATNR = WA_JGHJ-MATNR.
          G = G + 1.
        ENDLOOP.
        W = WA_JGHJ-HGPREIS / G.
        LL = WA_JGHJ-CGPREIS / G.
        JJ = WA_JGHJ-FPREIS / G.


        IF W <> WA_JG-HTDJ.
          CONCATENATE '物料' WA_JGHJ-MATNR '的合同单价不一致！' INTO MESSAGE1.
          E_MSG-TYPE = 'W'.
          E_MSG-MESSAGE = MESSAGE1 .
          APPEND E_MSG .
        ENDIF.

        IF LL <> WA_JG-CLDJ.
          CONCATENATE '物料' WA_JGHJ-MATNR '的材料单价不一致！' INTO MESSAGE2.
          E_MSG-TYPE = 'W'.
          E_MSG-MESSAGE = MESSAGE2 .
          APPEND E_MSG .
        ENDIF.

        IF JJ <> WA_JG-RGDJ.
          CONCATENATE '物料' WA_JGHJ-MATNR '的人工单价不一致！' INTO MESSAGE3.
          E_MSG-TYPE = 'W'.
          E_MSG-MESSAGE = MESSAGE3 .
          APPEND E_MSG .
        ENDIF.
      ENDIF.
      CLEAR:WA_JG,W,LL,JJ,MESSAGE1,MESSAGE2,MESSAGE3.
    ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:35:33  BEGIN
********************************************************************** 开始另一个逻辑判断
*&--代码添加 BY HANDYBY 23.05.2017 15:35:33  END
    DELETE LT_CNECP[] WHERE POSID = ''.

    DATA:l(50) TYPE C.
    DATA:j(50) TYPE C.

    CLEAR L.
    LOOP AT LT_CNECP.
      L = L + 1.
    ENDLOOP.

*&--代码添加 BY HANDYBY 23.05.2017 15:36:12  BEGIN
* 这里GT_HB取值逻辑的作用在于最后调用函数那里会用到，跟这里的校验逻辑无关
*&--代码添加 BY HANDYBY 23.05.2017 15:36:12  END
    TYPES:BEGIN OF TY_HB,
            MATNR  TYPE CKIS-MATNR,              "物料编码
            HPEINH TYPE MENGE_POS,              "合同工程量
            JPEINH TYPE MENGE_POS,              "计划工程量
          END OF TY_HB.
    DATA:GT_HB TYPE TABLE OF TY_HB,
         GS_HB TYPE TY_HB.

    REFRESH GT_HB.
    LOOP AT LT_CNECP WHERE MATNR <> ''.
      CLEAR GS_HB.
      GS_HB-MATNR = LT_CNECP-MATNR.
      GS_HB-HPEINH = LT_CNECP-HPEINH.
      GS_HB-JPEINH = LT_CNECP-JPEINH.
      COLLECT GS_HB INTO GT_HB.
    ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 13:25:10  BEGIN
    SORT GT_HB BY MATNR .
*&--代码添加 BY HANDYBY 28.04.2017 13:25:10  END

    LOOP AT LT_CNECP WHERE MATNR = ''.
      APPEND LT_CNECP TO LT_CNECP1.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM LT_CNECP[] COMPARING MATNR.

    DELETE LT_CNECP[] WHERE MATNR = ''.

    LOOP AT LT_CNECP1.
      APPEND LT_CNECP1 TO LT_CNECP.
    ENDLOOP.

    CLEAR J.
    LOOP AT LT_CNECP.
      J = J + 1.
    ENDLOOP.

    IF J < L.
      E_MSG-TYPE = 'W'.
      E_MSG-MESSAGE = '物料号相同的只被导入一次，且相同物料的计划工程量和合同工程量将被合计！' .
      APPEND E_MSG .
    ENDIF.


*&--代码添加 BY HANDYBY 28.04.2017 12:41:00  BEGIN
    "先检查一下可控的行错误信息.

    TYPES:BEGIN OF TY_CHECK_MATNR  ,
            MATNR TYPE MATNR,
            MEINS TYPE MEINS,
          END OF TY_CHECK_MATNR .
    DATA:GT_CHECK_MATNR TYPE TABLE OF TY_CHECK_MATNR,
         GS_CHECK_MATNR TYPE TY_CHECK_MATNR.

    SELECT A~MATNR B~MEINS
      INTO CORRESPONDING FIELDS OF TABLE GT_CHECK_MATNR
      FROM MARC AS A
     INNER JOIN MARA AS B
        ON A~MATNR = B~MATNR .
    SORT GT_CHECK_MATNR BY MATNR .

    LOOP AT LT_CNECP .

      IF LT_CNECP-POSID IS INITIAL.
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = 'WBS元素不能为空' .
        APPEND E_MSG .
        RETURN .
      ENDIF.

      IF LT_CNECP-XMLB = 'M'.
        IF LT_CNECP-MATNR IS  INITIAL. .
          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '导入失败！'.
          E_MSG-TYPE = 'E'..                             "状态
          E_MSG-MESSAGE = '项目类别为M时，物料编号不能为空'.              "消息
          APPEND E_MSG .
          RETURN  .
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:36:47  BEGIN
* 物料加前导零，否则查不出
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT        = LT_CNECP-MATNR
          IMPORTING
            OUTPUT       = LT_CNECP-MATNR
          EXCEPTIONS
            LENGTH_ERROR = 1
            OTHERS       = 2.
        IF SY-SUBRC <> 0.
          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '导入失败！'.
          E_MSG-TYPE = 'E'..                             "状态
          E_MSG-MESSAGE = 'V'.              "消息
          APPEND E_MSG .
          RETURN .
        ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:36:47  END

*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  BEGIN
* 物料号变大写
        TRANSLATE LT_CNECP-MATNR TO UPPER CASE .
*&--代码添加 BY HANDYBY 25.05.2017 09:49:29  END

        READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
               MATNR = LT_CNECP-MATNR BINARY SEARCH .
        IF SY-SUBRC NE 0 .
          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '导入失败！'.
          E_MSG-TYPE = 'E'.     "状态
          CONCATENATE LT_CNECP-WERKS '工厂' '不存在:' LT_CNECP-MATNR '物料号' INTO    E_MSG-MESSAGE ."消息
          APPEND E_MSG .
          RETURN .
        ENDIF.
      ENDIF.

      IF LT_CNECP-XMLB = 'V' AND LT_CNECP-CBYS IS  INITIAL..
        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        E_MSG-TYPE = 'E'..                             "状态
        E_MSG-MESSAGE = '项目类别为V时，成本要素不能为空'.              "消息
        APPEND E_MSG .
        RETURN .
      ENDIF.
    ENDLOOP.
*&--代码添加 BY HANDYBY 28.04.2017 12:41:00  END

    DATA:PROJ_OBJNR TYPE J_OBJNR , "项目对象号
         PRPS_OBJNR TYPE J_OBJNR . "WBS对象号

    CLEAR:PROJ_OBJNR,PRPS_OBJNR.

    DATA: BEGIN OF LS_PRPS3,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS3.
    DATA: BEGIN OF LS_PRPS4,
            PSPNR TYPE PRPS-PSPNR,
            POSID TYPE PRPS-POSID,
            PSPHI TYPE PRPS-PSPHI,
            OBJNR TYPE PRPS-OBJNR,
          END OF LS_PRPS4.
*  DATA LT_PRPS3 LIKE TABLE OF LS_PRPS3 .
    DATA: BEGIN OF LS_PROJ2 ,
            PSPNR TYPE PROJ-PSPNR,
            PSPID TYPE PROJ-PSPID,
            OBJNR TYPE PROJ-OBJNR,
          END OF LS_PROJ2 .
    DATA: BEGIN OF LS_KALNR,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR.
    DATA: BEGIN OF LS_KALNR1,
            SUBNR TYPE J_OBJNR,
            VERSN TYPE VERSN,
            KALNR TYPE PRECP2-KALNR,
            TOPNR TYPE J_OBJNR,
          END OF LS_KALNR1.
*  DATA LT_PROJ2 LIKE TABLE OF LS_PROJ2 .

    DATA:IT_CKIS TYPE TABLE OF CKIS.
    DATA:IT_CKIS1 TYPE TABLE OF CKIS.
    DATA:WA_CKIS TYPE CKIS.
    DATA:WA_CKIS1 TYPE CKIS.

    IF LT_CNECP[] IS NOT INITIAL .
      READ TABLE LT_CNECP INDEX 1 .
      SELECT  SINGLE PSPNR
                      POSID
                      PSPHI
                      OBJNR
        INTO CORRESPONDING FIELDS OF LS_PRPS3
        FROM  PRPS
        WHERE POSID = LT_CNECP-POSID.
      IF LS_PRPS3 IS NOT INITIAL .
        SELECT  SINGLE PSPNR
                        PSPID
                        OBJNR
          INTO CORRESPONDING FIELDS OF  LS_PROJ2
          FROM PROJ
         WHERE PSPNR = LS_PRPS3-PSPHI .
        IF LS_PROJ2 IS NOT INITIAL .

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '100'.
          IF LS_KALNR IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
              WHERE KALNR = LS_KALNR-KALNR.

            CLEAR LS_PRPS3 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS3
              FROM PRPS
             WHERE OBJNR = LS_KALNR-SUBNR .
          ENDIF.

          SELECT SINGLE
                SUBNR VERSN  KALNR TOPNR
             INTO CORRESPONDING FIELDS OF  LS_KALNR1
              FROM PRECP2
             WHERE TOPNR = LS_PROJ2-OBJNR
             AND SUBNR   = LS_PRPS3-OBJNR
             AND VERSN = '000'.
          IF LS_KALNR1 IS NOT INITIAL .
            SELECT *
              FROM CKIS
              INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
              WHERE KALNR = LS_KALNR1-KALNR.

            CLEAR LS_PRPS4 .
            SELECT SINGLE PSPNR
                           POSID
                           PSPHI
                           OBJNR
              INTO CORRESPONDING FIELDS OF LS_PRPS4
              FROM PRPS
             WHERE OBJNR = LS_KALNR1-SUBNR .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA:LT_WBS_COSTLINES3 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
    DATA:LT_WBS_COSTLINES4 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
    DATA:LS_WBS_COSTLINES3 TYPE PROJ_ELEMENT_CK_ITEMS.
    DATA:LS_WBS_COSTLINES4 TYPE PROJ_ELEMENT_CK_ITEMS.
    DATA:GT_RETURN5        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
    DATA:GS_RETURN5        TYPE BAPIRET2.
    DATA:GT_RETURN4        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
    DATA:GS_RETURN4        TYPE BAPIRET2.
    DATA:LS_ITEM3          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
    DATA:LS_ITEM4          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表
    DATA ERMSG      TYPE STRING.  "错误消息

    REFRESH: LT_WBS_COSTLINES3,GT_RETURN5,LT_WBS_COSTLINES4,GT_RETURN4,LS_WBS_COSTLINES3-COST_LINES,LS_WBS_COSTLINES4-COST_LINES.
    CLEAR:LS_WBS_COSTLINES3,GS_RETURN5,LS_WBS_COSTLINES4,GS_RETURN4.
    IF IT_CKIS IS NOT INITIAL.
      LOOP AT IT_CKIS INTO WA_CKIS.
        CLEAR:LS_ITEM3.
        AT FIRST .
          LS_WBS_COSTLINES3-WBS_NAME = LS_PRPS3-POSID.
        ENDAT.
        LS_ITEM3-ITEM_NUMBER = WA_CKIS-POSNR.
        LS_ITEM3-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM3 TO LS_WBS_COSTLINES3-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES3 TO LT_WBS_COSTLINES3.
        ENDAT.
      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除100版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '100'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES3                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN5.

      READ TABLE GT_RETURN5 INTO GS_RETURN5 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除100版本失败：' GS_RETURN5-MESSAGE  INTO ERMSG .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = ERMSG .
        APPEND E_MSG .
        RETURN .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取100版本的预算项目信息
    IF LS_KALNR IS NOT INITIAL.
      SELECT *
           FROM CKIS
           INTO CORRESPONDING FIELDS OF TABLE IT_CKIS
           WHERE KALNR = LS_KALNR-KALNR.
    ENDIF.


    IF IT_CKIS1 IS NOT INITIAL.
      LOOP AT IT_CKIS1 INTO WA_CKIS1.
        CLEAR:LS_ITEM4.
        AT FIRST .
          LS_WBS_COSTLINES4-WBS_NAME = LS_PRPS4-POSID.
        ENDAT.
        LS_ITEM4-ITEM_NUMBER = WA_CKIS1-POSNR.
        LS_ITEM4-FLAG_DELETE_ITEM = 'X'.
        APPEND LS_ITEM4 TO LS_WBS_COSTLINES4-COST_LINES.
        AT LAST .
          APPEND LS_WBS_COSTLINES4 TO LT_WBS_COSTLINES4.
        ENDAT.

      ENDLOOP.

      CALL FUNCTION 'CNECP_MAINTAIN'          "删除0版本
        EXPORTING
          I_PROJ_ID       = LS_PROJ2-PSPID                           "项目定义
          I_VERSION       = '000'                                         "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES4                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN4.

      READ TABLE GT_RETURN4 INTO GS_RETURN4 WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        CLEAR:ERMSG .
        CONCATENATE '该项目' LS_PROJ2-PSPID  '删除000版本失败：' GS_RETURN4-MESSAGE  INTO ERMSG .
        E_MSG-TYPE = 'E'.
        E_MSG-MESSAGE = ERMSG .
        APPEND E_MSG .
        RETURN .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
    "再重新读取000版本的预算项目信息
    IF LS_KALNR1 IS NOT INITIAL.
      SELECT *
           FROM CKIS
           INTO CORRESPONDING FIELDS OF TABLE IT_CKIS1
           WHERE KALNR = LS_KALNR1-KALNR.
    ENDIF.

**********************************************************************

*&--代码添加 BY HANDYBY 23.05.2017 15:40:38  BEGIN
    DATA:BEGIN OF LS_PRPS2,
           PSPNR TYPE PRPS-PSPNR,
           POSID TYPE PRPS-POSID,
           WERKS TYPE PRPS-WERKS,
         END OF LS_PRPS2.
    DATA LT_PRPS2 LIKE TABLE OF LS_PRPS2 .

    SELECT PSPNR
           POSID
           WERKS
      INTO CORRESPONDING FIELDS OF TABLE LT_PRPS2
      FROM PRPS
       FOR ALL ENTRIES IN I_CNECP[]
     WHERE POSID = I_CNECP-POSID .
    SORT LT_PRPS2 BY POSID .
*&--代码添加 BY HANDYBY 23.05.2017 15:40:38  END

    DATA:L1 TYPE I,       "0版本导入数量
         L2 TYPE I,       "100版本导入数量
         L3 TYPE I,       "0版本导入最后数量
         L4 TYPE I.       "100版本导入最后数量

    DATA:LT_WBS_COSTLINES  TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
    DATA:LT_WBS_COSTLINES1 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
    DATA:LT_WBS_COSTLINES5 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.
    DATA:LT_WBS_COSTLINES6 TYPE TTY_PROJ_ELEMENT_CK_ITEMS.

    DATA:LS_ITEM           TYPE BAPICKECP_ITEM.                               "定义BAPI中用到的结构和内表
    DATA:LS_ITEM1          TYPE BAPICKECP_ITEM.                              "定义BAPI中用到的结构和内表

    DATA:LS_WBS_COSTLINES  TYPE PROJ_ELEMENT_CK_ITEMS.
    DATA:LS_WBS_COSTLINES1 TYPE PROJ_ELEMENT_CK_ITEMS.

*&--代码添加 BY HANDYBY 28.04.2017 12:44:05  BEGIN
    IF IT_CKIS1 IS INITIAL AND IT_CKIS IS INITIAL.
      CLEAR:L1,L2,L3,L4.   "初始导入版本数量

      DATA:L_INDEX TYPE SY-TABIX.
      REFRESH LT_WBS_COSTLINES.
      REFRESH LT_WBS_COSTLINES1.
      REFRESH LT_WBS_COSTLINES5.
      REFRESH LT_WBS_COSTLINES6.

      TYPES:BEGIN OF TY_CH_OUTPUT2,
              POSID      TYPE PRPS-POSID,              "WBS
*              XH           TYPE INT4,                    "序号
              XMLB(1)    TYPE C,                       "项目类别
              WERKS      TYPE T001W-WERKS,             "工厂
              MATNR      TYPE CKIS-MATNR,              "物料编码
              MAKTX      TYPE MAKT-MAKTX,              "物料描述
              HPEINH     TYPE MENGE_POS,              "合同工程量
              HGPREIS    TYPE CKIS-GPREIS,             "合同单价
              JPEINH     TYPE MENGE_POS,              "计划工程量
              CGPREIS    TYPE P LENGTH 16 DECIMALS 4, "ckis-gpreis,             "材料单价
              FPREIS     TYPE CKIS-FPREIS,             "人工单价
              CBYS       TYPE CKIS-KSTAR,              "成本要素
              CBYSMC(40) TYPE C,                       "成本要素名称
*              POST1        TYPE PROJ-POST1,
*              VERNR        TYPE PROJ-VERNR,
*              VBUKR        TYPE PROJ-VBUKR,
*              KALID        TYPE PROJ-KALID,
*              ZTEHT        TYPE PROJ-ZTEHT,
*              TYPE(10)     TYPE C,                       "状态
*              MESSAGE(400) TYPE C,                       "消息
            END OF TY_CH_OUTPUT2.
      DATA:LT_CH_OUTPUT TYPE TABLE OF TY_CH_OUTPUT2,
           LS_CH_OUTPUT TYPE TY_CH_OUTPUT2.
      DATA LS_CH_OUTPUT2 TYPE TY_CH_OUTPUT2 .

      MOVE-CORRESPONDING I_CNECP[] TO LT_CH_OUTPUT .
      SORT LT_CH_OUTPUT BY POSID  .
      LOOP AT LT_CH_OUTPUT INTO LS_CH_OUTPUT2 .
        CLEAR:LS_ITEM,LS_ITEM1.
        L_INDEX = SY-TABIX.

        MOVE-CORRESPONDING LS_CH_OUTPUT2 TO LS_CH_OUTPUT .

*      AT FIRST .
*        READ TABLE GT_CH_OUTPUT INDEX 1 .
*        IF SY-SUBRC EQ 0 .
*      LS_WBS_COSTLINES-WBS_NAME = GT_CH_OUTPUT-POSID.                   "WBS
*      LS_WBS_COSTLINES1-WBS_NAME = GT_CH_OUTPUT-POSID.                  "WBS
*        ENDIF.
*      ENDAT.

        AT NEW POSID .
          LS_WBS_COSTLINES-WBS_NAME = LS_CH_OUTPUT-POSID.                   "WBS
          LS_WBS_COSTLINES1-WBS_NAME = LS_CH_OUTPUT-POSID.                  "WBS
        ENDAT .

        "序号
        LS_ITEM-ITEM_NUMBER = L_INDEX .                                      "序号
        LS_ITEM1-ITEM_NUMBER = L_INDEX .                                      "序号

        LS_ITEM-RESOURCE-MATNR = LS_CH_OUTPUT-MATNR.                       "物料号
        LS_ITEM1-RESOURCE-MATNR = LS_CH_OUTPUT-MATNR.                       "物料号

*        LS_CH_OUTPUT-TYPE = ICON_YELLOW_LIGHT.                             "状态

        IF  LS_ITEM1-RESOURCE-MATNR IS NOT INITIAL.
          READ TABLE GT_CHECK_MATNR INTO GS_CHECK_MATNR WITH KEY
                    MATNR = LS_CH_OUTPUT-MATNR BINARY SEARCH .
          IF SY-SUBRC EQ 0 .
            LS_ITEM-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                 "单位
            LS_ITEM1-QUANTITY-UNIT_OF_MEASURE = GS_CHECK_MATNR-MEINS.                "单位
            CLEAR GS_CHECK_MATNR .
          ENDIF.
        ENDIF.

        IF LS_CH_OUTPUT-CBYS IS NOT INITIAL.
          LS_ITEM-COST_ELEM = LS_CH_OUTPUT-CBYS.                          "成本要素
          LS_ITEM1-COST_ELEM = LS_CH_OUTPUT-CBYS.                          "成本要素
        ENDIF.

        LS_ITEM-RESOURCE-TYPPS = LS_CH_OUTPUT-XMLB.                        "项目类别
        LS_ITEM1-RESOURCE-TYPPS = LS_CH_OUTPUT-XMLB.                       "项目类别

        IF LS_CH_OUTPUT-WERKS IS INITIAL.
*&--代码添加 BY HANDYBY 23.05.2017 15:42:09  BEGIN
          READ TABLE LT_PRPS2 INTO LS_PRPS2 WITH KEY POSID = LS_CH_OUTPUT-POSID BINARY SEARCH .
          IF SY-SUBRC = 0 .
            LS_ITEM-RESOURCE-WERKS = LS_PRPS2-WERKS .
            LS_ITEM1-RESOURCE-WERKS = LS_PRPS2-WERKS.
            CLEAR LS_PRPS2 .
          ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:42:09  END
        ELSE.
          LS_ITEM-RESOURCE-WERKS = LS_CH_OUTPUT-WERKS.
          LS_ITEM1-RESOURCE-WERKS = LS_CH_OUTPUT-WERKS.
        ENDIF.

        LS_ITEM-DESCRIPT       = LS_CH_OUTPUT-MAKTX.                       "物料描述
        LS_ITEM1-DESCRIPT       = LS_CH_OUTPUT-MAKTX.                       "物料描述

        LS_ITEM1-PRICE-FIXED    = LS_CH_OUTPUT-FPREIS.                      "人工单价

        READ TABLE GT_HB INTO GS_HB WITH KEY MATNR = LS_CH_OUTPUT-MATNR BINARY SEARCH .
        IF SY-SUBRC = 0.
          LS_CH_OUTPUT-HPEINH = GS_HB-HPEINH.
          LS_CH_OUTPUT-JPEINH = GS_HB-JPEINH.
          CLEAR GS_HB .
        ENDIF.

        LS_ITEM-QUANTITY-QUANTITY = LS_CH_OUTPUT-HPEINH.                   "合同工程量
        LS_ITEM-PRICE-TOTAL = LS_CH_OUTPUT-HGPREIS.                        "合同单价
        LS_ITEM1-QUANTITY-QUANTITY = LS_CH_OUTPUT-JPEINH.                   "计划工程量
        LS_ITEM1-PRICE-TOTAL = LS_CH_OUTPUT-CGPREIS.                     "材料单价
        LS_ITEM-CURRENCY = 'CNY'.
        LS_ITEM1-CURRENCY = 'CNY'.
        APPEND LS_ITEM TO LS_WBS_COSTLINES-COST_LINES.
        APPEND LS_ITEM1 TO LS_WBS_COSTLINES1-COST_LINES.

*      AT LAST.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
*        APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
*        APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
*      ENDAT .

        AT END OF POSID .
          APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES.
          APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES1.
          APPEND LS_WBS_COSTLINES TO LT_WBS_COSTLINES5.
          APPEND LS_WBS_COSTLINES1 TO LT_WBS_COSTLINES6.                    "成本
          CLEAR LS_WBS_COSTLINES .
          CLEAR LS_WBS_COSTLINES1 .
        ENDAT .

        CLEAR LS_ITEM .
        CLEAR LS_ITEM1 .
        CLEAR LS_CH_OUTPUT .
        CLEAR LS_CH_OUTPUT2 .


      ENDLOOP.

**********************************************************************
      DATA:GT_RETURN         TYPE TABLE OF BAPIRET2.                            "声明返回消息的内表和工作区
      DATA:GT_RETURN1        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
      DATA:GT_RETURN2        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
      DATA:GT_RETURN3        TYPE TABLE OF BAPIRET2.                           "声明返回消息的内表和工作区
      DATA:GS_RETURN         TYPE BAPIRET2.
      DATA:GS_RETURN1        TYPE BAPIRET2.
      DATA:GS_RETURN2        TYPE BAPIRET2.
      DATA:GS_RETURN3        TYPE BAPIRET2.

      REFRESH GT_RETURN.
      REFRESH GT_RETURN1.
      REFRESH GT_RETURN2.
      REFRESH GT_RETURN3.
      CLEAR GS_RETURN .
      CLEAR GS_RETURN1 .
      CLEAR GS_RETURN2 .
      CLEAR GS_RETURN3 .
      READ TABLE I_CNECP INDEX 1 .

      DATA: BEGIN OF LS_PRPS5,
              PSPNR TYPE PRPS-PSPNR,
              PSPHI TYPE PRPS-PSPHI,
            END OF LS_PRPS5 .
      DATA: BEGIN OF LS_PROJ3 ,
              PSPNR TYPE PROJ-PSPNR,
              PSPID TYPE PROJ-PSPID,
            END OF LS_PROJ3 .
      SELECT SINGLE PSPNR
             PSPHI
        INTO CORRESPONDING FIELDS OF LS_PRPS5
        FROM PRPS
       WHERE POSID = I_CNECP-POSID .
      IF LS_PRPS5 IS NOT INITIAL .
        SELECT SINGLE PSPNR
               PSPID
          INTO CORRESPONDING FIELDS OF LS_PROJ3
          FROM PROJ
         WHERE PSPNR = LS_PRPS5-PSPHI .
      ENDIF.
*&--代码添加 BY HANDYBY 23.05.2017 15:43:44  BEGIN
      DATA L_FLAG1 TYPE I VALUE IS INITIAL  .
      DATA L_FLAG2 TYPE I VALUE IS INITIAL  .
      DATA L_MSG TYPE STRING .
*&--代码添加 BY HANDYBY 23.05.2017 15:43:44  END

*&--代码添加 BY HANDYBY 23.05.2017 15:56:01  BEGIN
********************************************************************** 000-099
*&--代码添加 BY HANDYBY 23.05.2017 15:56:01  END

      CALL FUNCTION 'CNECP_MAINTAIN'                                    "合同
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
          I_VERSION       = P_BB2                                       "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES                            "内表
        IMPORTING
          MESSAGES        = GT_RETURN.

      READ TABLE GT_RETURN INTO GS_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        E_MSG-TYPE = 'S'.
        CONCATENATE P_BB2 '版本导入成功！' INTO L_MSG .
        E_MSG-MESSAGE = L_MSG .
        APPEND E_MSG .
        CLEAR E_MSG .
        CLEAR L_MSG .

        CALL FUNCTION 'CNECP_MAINTAIN'          "创建100版本     合同
          EXPORTING
            I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
            I_VERSION       = '100'                                         "版本
            I_COST_VARIANT  = 'PS06'                                      "核算变式
            I_WBS_COSTLINES = LT_WBS_COSTLINES5                            "内表
          IMPORTING
            MESSAGES        = GT_RETURN2.

        READ TABLE GT_RETURN2 INTO GS_RETURN2 WITH KEY TYPE = 'E'.
        IF SY-SUBRC <> 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.

          E_MSG-TYPE = 'S'.
          CONCATENATE  '100' '版本导入成功！' INTO L_MSG .
          E_MSG-MESSAGE = L_MSG .
          APPEND E_MSG .
          CLEAR E_MSG .
          CLEAR L_MSG
           .
          L_FLAG1 = 0 .

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '导入失败！'.
          E_MSG-TYPE = 'E'.
          CONCATENATE  '100版本' GS_RETURN2-MESSAGE INTO L_MSG .
          E_MSG-MESSAGE = L_MSG .
          APPEND E_MSG .
          RETURN  .

          L_FLAG1 = 1 .

        ENDIF.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        E_MSG-TYPE = 'E'.
        CONCATENATE P_BB2  '版本' GS_RETURN-MESSAGE INTO L_MSG .
        E_MSG-MESSAGE = L_MSG .
        APPEND E_MSG .
        RETURN  .

        L_FLAG1 = 2 .

      ENDIF.

*&--代码添加 BY HANDYBY 23.05.2017 15:55:45  BEGIN
******************************************************************** 100-199
*&--代码添加 BY HANDYBY 23.05.2017 15:55:45  END
      CALL FUNCTION 'CNECP_MAINTAIN'                                    "成本
        EXPORTING
          I_PROJ_ID       = LS_PROJ3-PSPID                          "项目编号
          I_VERSION       = P_BB1                                       "版本
          I_COST_VARIANT  = 'PS06'                                      "核算变式
          I_WBS_COSTLINES = LT_WBS_COSTLINES1                           "内表
        IMPORTING
          MESSAGES        = GT_RETURN1.

      READ TABLE GT_RETURN1 INTO GS_RETURN1 WITH KEY TYPE = 'E'.
      IF SY-SUBRC <> 0 .

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        E_MSG-TYPE = 'S'.
        CONCATENATE P_BB1 '版本导入成功！' INTO L_MSG .
        E_MSG-MESSAGE = L_MSG .
        APPEND E_MSG .
        CLEAR E_MSG .
        CLEAR L_MSG .

        CALL FUNCTION 'CNECP_MAINTAIN'          "创建0版本
          EXPORTING
            I_PROJ_ID       = LS_PROJ3-PSPID                           "项目定义
            I_VERSION       = '000'                                         "版本
            I_COST_VARIANT  = 'PS06'                                      "核算变式
            I_WBS_COSTLINES = LT_WBS_COSTLINES6                            "内表
          IMPORTING
            MESSAGES        = GT_RETURN3.

        READ TABLE GT_RETURN3 INTO GS_RETURN3 WITH KEY TYPE = 'E'.
        IF SY-SUBRC <> 0 .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.

          E_MSG-TYPE = 'S'.
          CONCATENATE '000' '版本导入成功！' INTO L_MSG .
          E_MSG-MESSAGE = L_MSG .
          APPEND E_MSG .
          CLEAR E_MSG .
          CLEAR L_MSG .

          L_FLAG2 = 0 .

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          E_RETURN-TYPE = 'E'.
          E_RETURN-MESSAGE = '导入失败！'.
          E_MSG-TYPE = 'E'.
          CONCATENATE  '000版本' GS_RETURN3-MESSAGE INTO L_MSG .
          E_MSG-MESSAGE = L_MSG .
          APPEND E_MSG .
          RETURN  .

          L_FLAG2 = 1 .

        ENDIF.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        E_RETURN-TYPE = 'E'.
        E_RETURN-MESSAGE = '导入失败！'.
        E_MSG-TYPE = 'E'.
        CONCATENATE  P_BB1 '版本' GS_RETURN1-MESSAGE INTO L_MSG .
        E_MSG-MESSAGE = L_MSG .
        APPEND E_MSG .
        RETURN  .

        L_FLAG2 = 2 .

      ENDIF.

    ELSEIF IT_CKIS IS NOT INITIAL.
      E_RETURN-TYPE = 'E'.
      E_RETURN-MESSAGE = '导入失败！'.
      E_MSG-TYPE = 'E'.
      E_MSG-MESSAGE = '该项目原100版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' .
      APPEND E_MSG .
      RETURN  .
    ELSEIF IT_CKIS1 IS NOT INITIAL.
      E_RETURN-TYPE = 'E'.
      E_RETURN-MESSAGE = '导入失败！'.
      E_MSG-TYPE = 'E'.
      E_MSG-MESSAGE = '该项目原000版本未删除，请往CJ9ECP核对删除后再重现导入新版本数据' .
      APPEND E_MSG .
    ENDIF.


**********************************************************************
    DATA:MSG000(50) TYPE C ,   "000版本消息通知
         MSG001(50) TYPE  C,    "000后最新版本消息通知
         MSG100(50) TYPE  C,    "100版本消息通知
         MSG101(50) TYPE  C.   "100后最新版本消息通知

    "增加消息提示
    MSG000     = L_INDEX.
    CONDENSE MSG000 NO-GAPS.
    MSG001 = L_INDEX .
    CONDENSE MSG001 NO-GAPS.

    MSG100 = L_INDEX .
    CONDENSE MSG100 NO-GAPS.
    MSG101 = L_INDEX .
    CONDENSE MSG101 NO-GAPS.

    IF L_FLAG1 = 0.
      "100版本
      E_RETURN-TYPE = 'S'.
      E_RETURN-MESSAGE = '导入成功！'.
      E_MSG-TYPE = 'S'.
      CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
      E_MSG-MESSAGE = MSG100.
      APPEND E_MSG .
      CLEAR MSG100 .
*      CONCATENATE '100版本已成功导入:'   MSG100 '条数量' INTO MSG100 .
      "100后版本
      E_MSG-TYPE = 'S'.
      CONCATENATE P_BB2 '版本已成功导入:'   MSG101 '条数量' INTO MSG101 .
      E_MSG-MESSAGE = MSG101.
      APPEND E_MSG .
      CLEAR MSG101 .
*      CONCATENATE P_BB2 '版本已成功导入' MSG101 '条数量' INTO  MSG101.
    ELSEIF L_FLAG1 = 1 .
*      CONCATENATE  P_BB2 '版本已成功导入:'   MSG101 '条数量' INTO MSG101 .
*      CONCATENATE '100版本导入失败:'  MSG100 '条数量' INTO MSG100 .
    ELSEIF L_FLAG1 = 2 .
*      CONCATENATE  P_BB2 '版本导入失败:'   MSG101 '条数量' INTO MSG101 .
*      CONCATENATE '100版本没有执行导入动作！' '' INTO MSG101 .
    ENDIF.

    IF L_FLAG2 = 0.
      "000版本
      E_RETURN-TYPE = 'S'.
      E_RETURN-MESSAGE = '导入成功！'.
      E_MSG-TYPE = 'S'.
      CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
      E_MSG-MESSAGE = MSG000.
      APPEND E_MSG .
      CLEAR MSG000 .
*      CONCATENATE '000版本已成功导入:'   MSG000 '条数量' INTO MSG000 .
      "000后版本
      E_MSG-TYPE = 'S'.
      CONCATENATE P_BB1 '版本已成功导入:'   MSG001 '条数量' INTO MSG001 .
      E_MSG-MESSAGE = MSG001.
      APPEND E_MSG .
      CLEAR MSG001 .
*      CONCATENATE P_BB1 '版本已成功导入' MSG001 '条数量' INTO  MSG001.
    ELSEIF L_FLAG2 = 1 .
*      CONCATENATE  P_BB1 '版本已成功导入:'   MSG001 '条数量' INTO MSG001 .
*      CONCATENATE '000版本导入失败:'  MSG000 '条数量' INTO MSG000 .
    ELSEIF L_FLAG2 = 2 .
*      CONCATENATE  P_BB1 '版本导入失败:'   MSG001 '条数量' INTO MSG001 .
*      CONCATENATE '000版本没有执行导入动作！' '' INTO MSG000 .
    ENDIF.

  ELSE .
    E_MSG-TYPE = 'E'.
    E_MSG-MESSAGE = '没数据！'.
    APPEND E_MSG .
    CLEAR E_MSG .
    E_RETURN-TYPE = 'E'.
    E_RETURN-MESSAGE = '项目预算导入失败！'.
    RETURN .
  ENDIF.

* 写入调用日志
  DATA: LT_ZZACKNOW TYPE TABLE OF ZZACKNOW,
        LS_ZZACKNOW TYPE ZZACKNOW.
  DATA L_BUKRS TYPE BUKRS .

  READ TABLE I_CNECP INDEX 1 .
  SELECT SINGLE
         BUKRS
    INTO L_BUKRS
    FROM T001W AS A
   INNER JOIN T001K AS B
      ON A~BWKEY = B~BWKEY
   WHERE A~WERKS = I_CNECP-WERKS .

  LS_ZZACKNOW-ZZDYXTM  = I_DYXTM.     "调用系统名
  LS_ZZACKNOW-ZZINTTY  = 'PS'.     "接口类型
  LS_ZZACKNOW-ZZCDATE  = SY-DATUM.   "日期
  LS_ZZACKNOW-ZZCTIME  = SY-UZEIT.   "时间
  LS_ZZACKNOW-ZBUKRS   = L_BUKRS.    "公司代码
  LS_ZZACKNOW-USNAM    = SY-UNAME .   "用户名
  LS_ZZACKNOW-ZZKEYF1  = 'ZPS003' .   "函数模块名
  IF E_RETURN-TYPE = 'E'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用失败！' .
    LS_ZZACKNOW-ZZPROFG = 'E'."S"表成功 “E"表失败
  ELSEIF E_RETURN-TYPE = 'S'.
    LS_ZZACKNOW-ZZCOMEN = '接口调用成功！' .
    LS_ZZACKNOW-ZZPROFG = 'S'."S"表成功 “E"表失败
  ENDIF.

  APPEND LS_ZZACKNOW TO LT_ZZACKNOW.
  MODIFY ZZACKNOW FROM TABLE LT_ZZACKNOW.
  CLEAR LS_ZZACKNOW .
  REFRESH LT_ZZACKNOW .

ENDFUNCTION.
