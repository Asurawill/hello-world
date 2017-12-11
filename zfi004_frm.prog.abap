*&---------------------------------------------------------------------*
*&  包含                ZFI004_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AND_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_AND_PROCESS_DATA .

  " 项目定义
  DATA: BEGIN OF LS_PROJ ,
          PSPNR   TYPE PROJ-PSPNR,
          PSPID   TYPE PROJ-PSPID,
          POST1   TYPE PROJ-POST1,
          ZJGJSJE TYPE PROJ-ZJGJSJE,
          ZKHBM   TYPE PROJ-ZKHBM,
          ZHTJE   TYPE PROJ-ZHTJE,
        END OF LS_PROJ .
  DATA LT_PROJ LIKE TABLE OF LS_PROJ .
  " WBS
  DATA: BEGIN OF LS_PRPS ,
          PSPNR  TYPE PRPS-PSPNR,
          POSID  TYPE PRPS-POSID,
          POST1  TYPE PRPS-POST1,
          PSPHI  TYPE PRPS-PSPHI,
          OBJNR  TYPE PRPS-OBJNR,
          ZKHBM  TYPE PRPS-ZKHBM,
          STUFE  TYPE PRPS-STUFE,
          PRART  TYPE PRPS-PRART,
          ZHTLX  TYPE PRPS-ZHTLX,
          ZHTJR  TYPE PRPS-ZHTJR,
          ZYSRQ1 TYPE PRPS-ZYSRQ1,
          ZYSRQ2 TYPE PRPS-ZYSRQ2,
          ZYSRQ3 TYPE PRPS-ZYSRQ3,
          ZYSRQ4 TYPE PRPS-ZYSRQ4,
          ZYSRQ5 TYPE PRPS-ZYSRQ5,
          ZYSRQ6 TYPE PRPS-ZYSRQ6,
          ZYSRQ7 TYPE PRPS-ZYSRQ7,
          ZYSRQ8 TYPE PRPS-ZYSRQ8,
          ZYSJE1 TYPE PRPS-ZYSJE1,
          ZYSJE2 TYPE PRPS-ZYSJE2,
          ZYSJE3 TYPE PRPS-ZYSJE3,
          ZYSJE4 TYPE PRPS-ZYSJE4,
          ZYSJE5 TYPE PRPS-ZYSJE5,
          ZYSJE6 TYPE PRPS-ZYSJE6,
          ZYSJE7 TYPE PRPS-ZYSJE7,
          ZYSJE8 TYPE PRPS-ZYSJE8,
        END OF LS_PRPS .
  DATA LT_PRPS LIKE TABLE OF LS_PRPS .
  " PRPS-POSID 跟 自建表里的WBS字段长度不匹配，自定义内表进行匹配
  DATA: BEGIN OF LS_PRPS_ZFI005 ,
          XMBH TYPE ZFI005-XMBH,
        END OF LS_PRPS_ZFI005 .
  DATA LT_PRPS_ZFI005 LIKE TABLE OF LS_PRPS_ZFI005 .
  " 结果分析
  DATA: BEGIN OF LS_COEPB ,
          KOKRS  TYPE COEPB-KOKRS,
          BELNR  TYPE COEPB-BELNR,
          BUZEI  TYPE COEPB-BUZEI,
          OBJNR  TYPE COEPB-OBJNR,
          ABKAT  TYPE COEPB-ABKAT,
          AWKUS  TYPE COEPB-AWKUS,
          WOGBTR TYPE COEPB-WOGBTR,
        END OF LS_COEPB .
  DATA LT_COEPB LIKE TABLE OF LS_COEPB .
  " 版本
  DATA: BEGIN OF LS_PRECP2 ,
          SUBNR TYPE PRECP2-SUBNR,
          KALNR TYPE PRECP2-KALNR,
        END OF LS_PRECP2 .
  DATA LT_PRECP2 LIKE TABLE OF LS_PRECP2 .
  " 成本核算
  DATA: BEGIN OF LS_CKIS ,
          KALNR     TYPE CKIS-KALNR,
          KSTAR     TYPE CKIS-KSTAR,
          WRTFW_KPF TYPE CKIS-WRTFW_KPF,
        END OF LS_CKIS .
  DATA LT_CKIS LIKE TABLE OF LS_CKIS .
  " 会计凭证
  DATA: BEGIN OF LS_BKSG ,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          BUZEI TYPE BSEG-BUZEI,
          HKONT TYPE BSEG-HKONT,
          SHKZG TYPE BSEG-SHKZG,
          DMBTR TYPE BSEG-DMBTR,
          MWSKZ TYPE BSEG-MWSKZ,
          ZUONR TYPE BSEG-ZUONR,
          PROJK TYPE BSEG-PROJK,
        END OF LS_BKSG .
  DATA LT_BKSG LIKE TABLE OF LS_BKSG .
  DATA: LT_BKSG2 LIKE LT_BKSG,
        LS_BKSG2 LIKE LINE OF LT_BKSG2.
  DATA: LT_BKSG3 LIKE LT_BKSG,
        LS_BKSG3 LIKE LINE OF LT_BKSG3.
  " 销售回款认领平台
  DATA: BEGIN OF LS_ZFI005 ,
          XSHKNO TYPE ZFI005-XSHKNO,
          BUKRS  TYPE ZFI005-BUKRS,
          XMBH   TYPE ZFI005-XMBH,
          HSL    TYPE ZFI005-HSL,
        END OF LS_ZFI005 .
  DATA LT_ZFI005 LIKE TABLE OF LS_ZFI005 .
  " 销售回款认领平台
  DATA: BEGIN OF LS_ZFI005_SPLIT ,
          XSHKNO TYPE ZFI005_SPLIT-XSHKNO,
          BUKRS  TYPE ZFI005_SPLIT-BUKRS,
          TABIX  TYPE ZFI005_SPLIT-TABIX,
          XMBH   TYPE ZFI005_SPLIT-XMBH,
          HSL    TYPE ZFI005_SPLIT-HSL,
        END OF LS_ZFI005_SPLIT .
  DATA LT_ZFI005_SPLIT LIKE TABLE OF LS_ZFI005_SPLIT .
  " 采购
  DATA: BEGIN OF LS_EKNP ,
          EBELN      TYPE EKKN-EBELN,
          EBELP      TYPE EKKN-EBELP,
          PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
          RETPO      TYPE EKPO-RETPO,
          BRTWR      TYPE EKPO-BRTWR,
          KNTTP      TYPE EKPO-KNTTP,
        END OF LS_EKNP .
  DATA LT_EKNP LIKE TABLE OF LS_EKNP .
  " 采购付款
  DATA: BEGIN OF LS_ZFI017 ,
          ZSQD    TYPE ZFI017-ZSQD,
          BUKRS   TYPE ZFI017-BUKRS,
          GJAHR   TYPE ZFI017-GJAHR,
          EBELN   TYPE ZFI017-EBELN,
          EBELP   TYPE ZFI017-EBELP,
          BELNR   TYPE ZFI017-BELNR,
          POSID   TYPE ZFI017-POSID,
          ZSQFKJE TYPE ZFI017-ZSQFKJE,
        END OF LS_ZFI017 .
  DATA LT_ZFI017 LIKE TABLE OF LS_ZFI017 .
  " 客户
  DATA: BEGIN OF LS_KNA1 ,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
        END OF LS_KNA1 .
  DATA LT_KNA1 LIKE TABLE OF LS_KNA1 .

  " ALV小计用
  DATA: L_PRART     TYPE PRPS-PRART,
        L_ZHTLX     TYPE PRPS-ZHTLX,
        L_ZKHBM     TYPE PRPS-ZKHBM,
        L_ZHTJR     TYPE PRPS-ZHTJR,
        L_YSSR      TYPE COEPB-WOGBTR,
        L_YSCB      TYPE COEPB-WOGBTR,
        L_YSMLL     TYPE COEPB-WOGBTR,
        L_YSFY      TYPE COEPB-WOGBTR,
        L_SJSR      TYPE BSEG-DMBTR,
        L_SJCB      TYPE BSEG-DMBTR,
        L_WGBFB     TYPE BSEG-DMBTR,
        L_GCJS      TYPE BSEG-DMBTR,
        L_GCML      TYPE BSEG-DMBTR,
        L_SJFY      TYPE BSEG-DMBTR,
        L_GCSG      TYPE BSEG-DMBTR,
        L_GCCB_FCSP TYPE BSEG-DMBTR,
        L_YSZK      TYPE BSEG-DMBTR,
        L_YFZK      TYPE BSEG-DMBTR,
        L_YFZG      TYPE BSEG-DMBTR,
        L_Z8008     TYPE BSEG-DMBTR,
        L_YKJSFPJE  TYPE BSEG-DMBTR,
        L_LJSK      TYPE ZFI005-HSL,
        L_DQJDYS    TYPE PRPS-ZYSJE1,
        L_CGE_HS    TYPE EKPO-BRTWR,
        L_YFJE      TYPE ZFI017-ZSQFKJE,
        L_WSKJE     TYPE PRPS-ZHTJR,
        L_LWHTJE    TYPE EKPO-BRTWR,
        L_CLCG      TYPE EKPO-BRTWR,
        L_FBCG      TYPE EKPO-BRTWR.

  DATA L_NUM TYPE I VALUE IS INITIAL .
  DATA L_MWSKZ TYPE BSEG-MWSKZ .

  " 项目预算信息
  RANGES: R_KSTAR FOR CKIS-KSTAR.
  RANGES: R_KSTAR2 FOR CKIS-KSTAR .
  RANGES: R_KSTAR3 FOR CKIS-KSTAR .
  " 项目实际财务信息
  RANGES: R_HKONT FOR BSEG-HKONT .
  RANGES: R_HKONT2 FOR BSEG-HKONT .
  RANGES: R_HKONT3 FOR BSEG-HKONT .
  RANGES: R_HKONT4 FOR BSEG-HKONT .
  RANGES: R_HKONT5 FOR BSEG-HKONT .
  RANGES: R_HKONT6 FOR BSEG-HKONT .
  RANGES: R_HKONT7 FOR BSEG-HKONT .
  RANGES: R_HKONT8 FOR BSEG-HKONT .
  RANGES: R_HKONT9 FOR BSEG-HKONT .

  " 项目预算信息
  R_KSTAR-SIGN = 'I'.
  R_KSTAR-OPTION = 'CP'.
  R_KSTAR-LOW = '6001*'.
  APPEND R_KSTAR .
  R_KSTAR-SIGN = 'I'.
  R_KSTAR-OPTION = 'CP'.
  R_KSTAR-LOW = '6051*'.
  APPEND R_KSTAR .
  R_KSTAR-SIGN = 'I'.
  R_KSTAR-OPTION = 'CP'.
  R_KSTAR-LOW = '6301*'.
  APPEND R_KSTAR .

  R_KSTAR2-SIGN = 'I'.
  R_KSTAR2-OPTION = 'CP'.
  R_KSTAR2-LOW = '5401*'.
  APPEND R_KSTAR2 .
  R_KSTAR2-SIGN = 'I'.
  R_KSTAR2-OPTION = 'CP'.
  R_KSTAR2-LOW = '8008*'.
  APPEND R_KSTAR2 .

  R_KSTAR3-SIGN = 'I'.
  R_KSTAR3-OPTION = 'CP'.
  R_KSTAR3-LOW = '8*'.
  APPEND R_KSTAR3 .
  R_KSTAR3-SIGN = 'I'.
  R_KSTAR3-OPTION = 'NP'.
  R_KSTAR3-LOW = '8008*'.
  APPEND R_KSTAR3 .

  " 项目实际财务信息
  R_HKONT-SIGN = 'I'.
  R_HKONT-OPTION = 'CP'.
  R_HKONT-LOW = '6001*'.
  APPEND R_HKONT .
  R_HKONT-SIGN = 'I'.
  R_HKONT-OPTION = 'CP'.
  R_HKONT-LOW = '6051*'.
  APPEND R_HKONT .
  R_HKONT-SIGN = 'I'.
  R_HKONT-OPTION = 'CP'.
  R_HKONT-LOW = '6301*'.
  APPEND R_HKONT .

  R_HKONT2-SIGN = 'I'.
  R_HKONT2-OPTION = 'CP'.
  R_HKONT2-LOW = '6401*'.
  APPEND R_HKONT2 .

  R_HKONT3-SIGN = 'I'.
  R_HKONT3-OPTION = 'CP'.
  R_HKONT3-LOW = '8001*'.
  APPEND R_HKONT3 .
  R_HKONT3-SIGN = 'I'.
  R_HKONT3-OPTION = 'CP'.
  R_HKONT3-LOW = '8002*'.
  APPEND R_HKONT3 .
  R_HKONT3-SIGN = 'I'.
  R_HKONT3-OPTION = 'CP'.
  R_HKONT3-LOW = '8003*'.
  APPEND R_HKONT3 .
  R_HKONT3-SIGN = 'I'.
  R_HKONT3-OPTION = 'CP'.
  R_HKONT3-LOW = '8004*'.
  APPEND R_HKONT3 .
  R_HKONT3-SIGN = 'I'.
  R_HKONT3-OPTION = 'CP'.
  R_HKONT3-LOW = '8005*'.
  APPEND R_HKONT3 .

  R_HKONT4-SIGN = 'I'.
  R_HKONT4-OPTION = 'CP'.
  R_HKONT4-LOW = '540101*'.
  APPEND R_HKONT4 .

  R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'CP'.
  R_HKONT5-LOW = '1406*'.
  APPEND R_HKONT5 .

  R_HKONT6-SIGN = 'I'.
  R_HKONT6-OPTION = 'CP'.
  R_HKONT6-LOW = '1122*'.
  APPEND R_HKONT6 .

  R_HKONT7-SIGN = 'I'.
  R_HKONT7-OPTION = 'CP'.
  R_HKONT7-LOW = '220201*'.
  APPEND R_HKONT7 .
  R_HKONT7-SIGN = 'I'.
  R_HKONT7-OPTION = 'CP'.
  R_HKONT7-LOW = '220202*'.
  APPEND R_HKONT7 .

  R_HKONT8-SIGN = 'I'.
  R_HKONT8-OPTION = 'CP'.
  R_HKONT8-LOW = '220203*'.
  APPEND R_HKONT8 .

  R_HKONT9-SIGN = 'I'.
  R_HKONT9-OPTION = 'CP'.
  R_HKONT9-LOW = '8008*'.
  APPEND R_HKONT9 .
  R_HKONT9-SIGN = 'I'.
  R_HKONT9-OPTION = 'BT'.
  R_HKONT9-LOW = '8999999996'.
  R_HKONT9-HIGH = '8999999998'.
  APPEND R_HKONT9 .

**&& 取数
* 项目基本信息
  IF P3 = '' .

    SELECT A~PSPNR,
           A~PSPID,
           A~POST1,
           A~ZJGJSJE,
           A~ZKHBM,
           A~ZHTJE
      INTO CORRESPONDING FIELDS OF TABLE @LT_PROJ
      FROM PROJ AS A
     INNER JOIN JEST AS B
        ON A~OBJNR EQ B~OBJNR
       AND ( B~STAT NOT IN ('I0045','I0046')
       AND   B~INACT NE '' )
     WHERE A~VBUKR EQ @P_BUKRS
       AND A~PSPID IN @S_PSPID .
    IF LT_PROJ IS NOT INITIAL .
      SELECT PSPNR POSID POST1 PSPHI
             OBJNR ZKHBM STUFE PRART
             ZHTLX ZHTJR ZYSRQ1 ZYSRQ2
             ZYSRQ3 ZYSRQ4 ZYSRQ5 ZYSRQ6
             ZYSRQ7 ZYSRQ8 ZYSJE1 ZYSJE2
             ZYSJE3 ZYSJE4 ZYSJE5 ZYSJE6
             ZYSJE7 ZYSJE8
        INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
        FROM PRPS
         FOR ALL ENTRIES IN LT_PROJ
       WHERE PSPHI EQ LT_PROJ-PSPNR .
    ENDIF.

  ELSE .

    SELECT A~PSPNR
           A~PSPID
           A~POST1
           A~ZJGJSJE
           A~ZKHBM
           A~ZHTJE
      INTO CORRESPONDING FIELDS OF TABLE LT_PROJ
      FROM PROJ AS A
     WHERE A~VBUKR EQ P_BUKRS
       AND A~PSPID IN S_PSPID .
    IF LT_PROJ IS NOT INITIAL .
      SELECT PSPNR POSID POST1 PSPHI
             OBJNR ZKHBM STUFE PRART
             ZHTLX ZHTJR ZYSRQ1 ZYSRQ2
             ZYSRQ3 ZYSRQ4 ZYSRQ5 ZYSRQ6
             ZYSRQ7 ZYSRQ8 ZYSJE1 ZYSJE2
             ZYSJE3 ZYSJE4 ZYSJE5 ZYSJE6
             ZYSJE7 ZYSJE8
        INTO CORRESPONDING FIELDS OF TABLE LT_PRPS
        FROM PRPS
         FOR ALL ENTRIES IN LT_PROJ
       WHERE PSPHI EQ LT_PROJ-PSPNR .
    ENDIF.

  ENDIF.

  IF LT_PROJ IS INITIAL .
    MESSAGE '没数据' TYPE 'E' .
  ELSE .

    SORT LT_PROJ BY PSPNR .
    SORT LT_PRPS BY PSPHI .

* 项目预算信息
    SELECT KOKRS
           BELNR
           BUZEI
           OBJNR
           ABKAT
           AWKUS
           WOGBTR
      INTO CORRESPONDING FIELDS OF TABLE LT_COEPB
      FROM COEPB
     WHERE GJAHR EQ P_DATUM+0(4)
       AND PERIO EQ P_DATUM+4(2) .
    IF LT_COEPB IS NOT INITIAL .
      REFRESH LT_COEPB .
      SELECT KOKRS
             BELNR
             BUZEI
             OBJNR
             ABKAT
             AWKUS
             WOGBTR
        INTO CORRESPONDING FIELDS OF TABLE LT_COEPB
        FROM COEPB
         FOR ALL ENTRIES IN LT_PRPS
       WHERE OBJNR EQ LT_PRPS-OBJNR
         AND ( ( GJAHR EQ P_DATUM+0(4) AND PERIO LT P_DATUM+4(2) )
          OR     GJAHR LT P_DATUM+0(4) ) .
      SORT LT_COEPB BY OBJNR .
    ELSE.
      SELECT SUBNR
             KALNR
        INTO CORRESPONDING FIELDS OF TABLE LT_PRECP2
        FROM PRECP2
         FOR ALL ENTRIES IN LT_PRPS
       WHERE SUBNR EQ LT_PRPS-OBJNR
         AND VERSN EQ '000' .
      IF LT_PRECP2 IS NOT INITIAL .
        SORT LT_PRECP2 BY SUBNR .
        SELECT KALNR
               KSTAR
               WRTFW_KPF
          INTO CORRESPONDING FIELDS OF TABLE LT_CKIS
          FROM CKIS
           FOR ALL ENTRIES IN LT_PRECP2
         WHERE KALNR EQ LT_PRECP2-KALNR .
        SORT LT_CKIS BY KALNR .
      ENDIF.
    ENDIF.

**& xx
    IF LT_PRPS IS NOT INITIAL .
* 项目实际财务信息
      SELECT A~BUKRS
             A~BELNR
             A~GJAHR
             B~BUZEI
             B~HKONT
             B~SHKZG
             B~DMBTR
             B~MWSKZ
             B~ZUONR
             B~PROJK
        INTO CORRESPONDING FIELDS OF TABLE LT_BKSG
        FROM BKPF AS A
       INNER JOIN BSEG AS B
          ON A~BUKRS EQ B~BUKRS
         AND A~BELNR EQ B~BELNR
         AND A~GJAHR EQ B~GJAHR
         FOR ALL ENTRIES IN LT_PRPS
       WHERE A~BUDAT LE P_DATUM
         AND ( B~ZUONR EQ LT_PRPS-POSID+0(18)
          OR   B~PROJK EQ LT_PRPS-PSPNR ) .
      SORT LT_BKSG BY ZUONR .
      MOVE-CORRESPONDING LT_BKSG TO LT_BKSG2 .
      SORT LT_BKSG2 BY PROJK .

* 累计收款
      LOOP AT LT_PRPS INTO LS_PRPS .
        LS_PRPS_ZFI005-XMBH = LS_PRPS-POSID .
        APPEND LS_PRPS_ZFI005 TO LT_PRPS_ZFI005 .
        CLEAR LS_PRPS_ZFI005 .
        CLEAR LS_PRPS .
      ENDLOOP.
      SORT LT_PRPS_ZFI005 BY XMBH .
      DELETE ADJACENT DUPLICATES FROM LT_PRPS_ZFI005 COMPARING XMBH .
      IF LT_PRPS_ZFI005 IS NOT INITIAL .
        SELECT XSHKNO
               BUKRS
               XMBH
               HSL
          INTO CORRESPONDING FIELDS OF TABLE LT_ZFI005
          FROM ZFI005
           FOR ALL ENTRIES IN LT_PRPS_ZFI005
         WHERE BUKRS EQ P_BUKRS
           AND XMBH EQ LT_PRPS_ZFI005-XMBH
           AND STEP EQ '@08@'
           AND SUB_COUNT EQ 1
           AND YWZL IN ('项目回款','项目预收')
           AND POSTDAT LE P_DATUM .
        SORT LT_ZFI005 BY XMBH .

        SELECT A~XSHKNO
               A~BUKRS
               A~TABIX
               A~XMBH
               A~HSL
          INTO CORRESPONDING FIELDS OF TABLE LT_ZFI005_SPLIT
          FROM ZFI005_SPLIT AS A
         INNER JOIN ZFI005 AS B
            ON A~XSHKNO EQ B~XSHKNO
           AND B~STEP EQ '@08@'
           AND B~POSTDAT LE P_DATUM
           FOR ALL ENTRIES IN LT_PRPS_ZFI005
         WHERE A~BUKRS EQ P_BUKRS
           AND A~XMBH EQ LT_PRPS_ZFI005-XMBH
           AND A~YWZL IN ('项目回款','项目预收') .
        SORT LT_ZFI005_SPLIT BY XMBH .
      ENDIF.

* 采购额（含税）、劳动合同金额、材料采购、分包采购
      SELECT A~EBELN
             A~EBELP
             A~PS_PSP_PNR
             B~RETPO
             B~BRTWR
             B~KNTTP
        INTO CORRESPONDING FIELDS OF TABLE LT_EKNP
        FROM EKKN AS A
       INNER JOIN EKPO AS B
          ON A~EBELN EQ B~EBELN
         AND A~EBELP EQ B~EBELP
         FOR ALL ENTRIES IN LT_PRPS
       WHERE A~PS_PSP_PNR EQ LT_PRPS-PSPNR
         AND B~LOEKZ EQ '' .
      SORT LT_EKNP BY PS_PSP_PNR .

* 已付金额
      SELECT ZSQD
             BUKRS
             GJAHR
             EBELN
             EBELP
             BELNR
             POSID
             ZSQFKJE
        INTO CORRESPONDING FIELDS OF TABLE LT_ZFI017
        FROM ZFI017
         FOR ALL ENTRIES IN LT_PRPS
       WHERE BUKRS EQ P_BUKRS
         AND POSID EQ LT_PRPS-POSID
         AND ZCLJD EQ '3' .
      SORT LT_ZFI017 BY POSID .

* 客户描述
      SELECT KUNNR
             NAME1
        INTO CORRESPONDING FIELDS OF TABLE LT_KNA1
        FROM KNA1
         FOR ALL ENTRIES IN LT_PRPS
       WHERE KUNNR = LT_PRPS-ZKHBM .
      SORT LT_KNA1 BY KUNNR .

    ENDIF.

  ENDIF.

* 将值放进ALV
  LOOP AT LT_PROJ INTO LS_PROJ .

    READ TABLE LT_PRPS WITH KEY PSPHI = LS_PROJ-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
    IF SY-SUBRC EQ 0 .
      LOOP AT LT_PRPS INTO LS_PRPS FROM SY-TABIX .
        IF LS_PRPS-PSPHI EQ LS_PROJ-PSPNR .

          L_NUM = L_NUM + 1 .
          GS_ALV-NUM = L_NUM .

          " && 项目基本信息
          GS_ALV-PSPID = LS_PROJ-PSPID .
          GS_ALV-POST = LS_PROJ-POST1 .
          GS_ALV-POSID = LS_PRPS-POSID .
          GS_ALV-POST1 = LS_PRPS-POST1 .
          GS_ALV-STUFE = LS_PRPS-STUFE .
          GS_ALV-PRART = LS_PRPS-PRART .
          " 后面汇总用
          IF LS_PRPS-STUFE = 1 .
            L_PRART = LS_PRPS-PRART .
          ENDIF.
          GS_ALV-ZHTLX = LS_PRPS-ZHTLX .
          " 后面汇总用
          IF LS_PRPS-STUFE = 1 .
            L_ZHTLX = LS_PRPS-ZHTLX .
          ENDIF.
          GS_ALV-ZKHBM = LS_PRPS-ZKHBM .
          " 后面汇总用
          IF LS_PRPS-STUFE = 1 .
            L_ZKHBM = LS_PRPS-ZKHBM .
          ENDIF.
          READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY KUNNR = LS_PRPS-ZKHBM BINARY SEARCH .
          GS_ALV-NAME1 = LS_KNA1-NAME1 .
          GS_ALV-ZHTJR = LS_PRPS-ZHTJR .
          " 后面汇总用
          IF LS_PRPS-STUFE = 1 .
            L_ZHTJR = LS_PRPS-ZHTJR .
          ENDIF.
          GS_ALV-ZJGJSJE = LS_PROJ-ZJGJSJE .

          " && 项目预算信息
          IF LT_COEPB IS NOT INITIAL .

            READ TABLE LT_COEPB WITH KEY OBJNR = LS_PRPS-OBJNR BINARY SEARCH TRANSPORTING NO FIELDS .
            IF SY-SUBRC EQ 0 .
              LOOP AT LT_COEPB INTO LS_COEPB FROM SY-TABIX .
                IF  LS_COEPB-OBJNR EQ LS_PRPS-OBJNR .
                  IF LS_COEPB-ABKAT EQ 81 .
                    IF LS_COEPB-AWKUS IS INITIAL  .
                      GS_ALV-YSSR = GS_ALV-YSSR + LS_COEPB-WOGBTR .
                    ELSEIF LS_COEPB-AWKUS = 'D' .
                      GS_ALV-YSFY = GS_ALV-YSFY + LS_COEPB-WOGBTR .
                    ENDIF.
                  ELSEIF LS_COEPB-ABKAT EQ 82 .
                    GS_ALV-YSCB = GS_ALV-YSCB + LS_COEPB-WOGBTR .
                  ENDIF.
                ELSE .
                  EXIT .
                ENDIF.
                CLEAR LS_COEPB .
              ENDLOOP.
            ENDIF.

          ELSE .

            READ TABLE LT_PRECP2 INTO LS_PRECP2 WITH KEY SUBNR = LS_PRPS-OBJNR BINARY SEARCH .
            READ TABLE LT_CKIS WITH KEY KALNR = LS_PRECP2-KALNR BINARY SEARCH TRANSPORTING NO FIELDS .
            IF SY-SUBRC EQ 0 .
              LOOP AT LT_CKIS INTO LS_CKIS FROM SY-TABIX .
                IF LS_CKIS-KALNR EQ LS_PRECP2-KALNR .
                  IF LS_CKIS-KSTAR IN R_KSTAR .
                    GS_ALV-YSSR = GS_ALV-YSSR + LS_CKIS-WRTFW_KPF .
                  ELSEIF LS_CKIS-KSTAR IN R_KSTAR2 .
                    GS_ALV-YSCB = GS_ALV-YSCB + LS_CKIS-WRTFW_KPF .
                  ELSEIF LS_CKIS-KSTAR IN R_KSTAR3 .
                    GS_ALV-YSFY = GS_ALV-YSFY + LS_CKIS-WRTFW_KPF .
                  ENDIF.
                ELSE .
                  EXIT .
                ENDIF.
                CLEAR LS_CKIS .
              ENDLOOP.
            ENDIF.

          ENDIF.

          GS_ALV-YSSR = - GS_ALV-YSSR .
          IF GS_ALV-YSSR IS NOT INITIAL AND GS_ALV-YSSR NE 0 .
            GS_ALV-YSMLL = ( GS_ALV-YSSR - GS_ALV-YSCB ) / GS_ALV-YSSR .
          ELSE .
            GS_ALV-YSMLL = 0.
          ENDIF.

          " 后面汇总用
          IF LT_COEPB IS NOT INITIAL .
            IF LS_PRPS-STUFE = 1 .
              L_YSSR = GS_ALV-YSSR .
              L_YSCB = GS_ALV-YSCB .
              IF L_YSSR IS NOT INITIAL AND L_YSSR NE 0 .
                L_YSMLL = ( L_YSSR - L_YSCB ) / L_YSSR .
              ELSE .
                L_YSMLL = 0 .
              ENDIF.
              L_YSFY = GS_ALV-YSFY .
            ENDIF.
          ELSE .
            L_YSSR = L_YSSR + GS_ALV-YSSR .
            L_YSCB = L_YSCB + GS_ALV-YSCB .
            IF L_YSSR IS NOT INITIAL AND L_YSSR NE 0 .
              L_YSMLL = ( L_YSSR - L_YSCB ) / L_YSSR .
            ELSE .
              L_YSMLL = 0 .
            ENDIF.
            L_YSFY = L_YSFY + GS_ALV-YSFY .
          ENDIF.

          " && 项目实际财务信息
          READ TABLE LT_BKSG WITH KEY ZUONR = LS_PRPS-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT LT_BKSG INTO LS_BKSG FROM SY-TABIX .
              IF LS_BKSG-ZUONR = LS_PRPS-POSID .
                MOVE-CORRESPONDING LS_BKSG TO LS_BKSG3 .
                APPEND LS_BKSG3 TO LT_BKSG3 .
                CLEAR LS_BKSG3 .
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_BKSG .
            ENDLOOP.
          ENDIF.

          READ TABLE LT_BKSG2 WITH KEY PROJK = LS_PRPS-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC = 0 .
            LOOP AT LT_BKSG2 INTO LS_BKSG2 FROM SY-TABIX .
              IF LS_BKSG2-PROJK = LS_PRPS-POSID .
                MOVE-CORRESPONDING LS_BKSG2 TO LS_BKSG3 .
                APPEND LS_BKSG3 TO LT_BKSG3 .
                CLEAR LS_BKSG3 .
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_BKSG2 .
            ENDLOOP.
          ENDIF.

          LOOP AT LT_BKSG3 INTO LS_BKSG3 .
            " 实际收入
            IF LS_BKSG3-HKONT IN R_HKONT .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-SJSR = GS_ALV-SJSR - LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-SJSR = GS_ALV-SJSR + LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 实际成本
            IF LS_BKSG3-HKONT IN R_HKONT2 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-SJCB = GS_ALV-SJCB + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-SJCB = GS_ALV-SJCB - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 完工百分比
            IF GS_ALV-YSCB EQ 0 .
              GS_ALV-WGBFB = 100 .
            ELSE .
              GS_ALV-WGBFB = GS_ALV-SJCB / GS_ALV-YSCB * 100 .
              IF GS_ALV-WGBFB GT 100 .
                GS_ALV-WGBFB = 100 .
              ELSEIF GS_ALV-WGBFB LT 0 .
                GS_ALV-WGBFB = 0 .
              ENDIF.
            ENDIF.
            " 工程结算
            IF LS_BKSG3-HKONT EQ '5402010101' .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-GCJS = GS_ALV-GCJS + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-GCJS = GS_ALV-GCJS - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 工程毛利
            IF LS_BKSG3-HKONT EQ '5401030101' .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-GCML = GS_ALV-GCML + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-GCML = GS_ALV-GCML - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 实际费用
            IF LS_BKSG3-HKONT IN R_HKONT3 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-SJFY = GS_ALV-SJFY + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-SJFY = GS_ALV-SJFY - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 工程施工
            IF LS_BKSG3-HKONT IN R_HKONT4 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-GCSG = GS_ALV-GCSG + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-GCSG = GS_ALV-GCSG - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 工程成本/发出商品
            IF LS_BKSG3-HKONT IN R_HKONT5 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-GCCB_FCSP = GS_ALV-GCCB_FCSP + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-GCCB_FCSP = GS_ALV-GCCB_FCSP - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 应收账款
            IF LS_BKSG3-HKONT IN R_HKONT6 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-YSZK = GS_ALV-YSZK + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-YSZK = GS_ALV-YSZK - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 应付账款
            IF LS_BKSG3-HKONT IN R_HKONT7 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-YFZK = GS_ALV-YFZK + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-YFZK = GS_ALV-YFZK - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 应付暂估
            IF LS_BKSG3-HKONT IN R_HKONT8 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-YFZG = GS_ALV-YFZG + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-YFZG = GS_ALV-YFZG - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 8008
            IF LS_BKSG3-HKONT IN R_HKONT9 .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-Z8008 = GS_ALV-Z8008 + LS_BKSG3-DMBTR .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-Z8008 = GS_ALV-Z8008 - LS_BKSG3-DMBTR .
              ENDIF.
            ENDIF.
            " 已开金税发票金额
            IF LS_BKSG3-HKONT EQ '2221030501' .
              PERFORM FRM_CHG_MWSKZ USING LS_BKSG3-MWSKZ L_MWSKZ .
              IF LS_BKSG3-SHKZG = 'S'.
                GS_ALV-YKJSFPJE = GS_ALV-YKJSFPJE - LS_BKSG3-DMBTR / ( L_MWSKZ * ( 1 + L_MWSKZ ) ) .
              ELSEIF LS_BKSG3-SHKZG = 'H'.
                GS_ALV-YKJSFPJE = GS_ALV-YKJSFPJE + LS_BKSG3-DMBTR / ( L_MWSKZ * ( 1 + L_MWSKZ ) ) .
              ENDIF.
            ENDIF.
            " 未开金税发票金额
            IF GS_ALV-ZJGJSJE IS INITIAL .
              GS_ALV-WKJSFPJE =  GS_ALV-ZHTJR - GS_ALV-YKJSFPJE .
            ENDIF.

            CLEAR L_MWSKZ .
            CLEAR LS_BKSG3 .
          ENDLOOP.

          IF GS_ALV-SJSR IS NOT INITIAL AND GS_ALV-SJSR NE 0 .
            GS_ALV-SJMLL = ( GS_ALV-SJSR - GS_ALV-SJCB ) / GS_ALV-SJSR .
          ELSE .
            GS_ALV-SJMLL = 0.
          ENDIF.

          " 后面汇总用
          L_SJSR = L_SJSR + GS_ALV-SJSR  .
          L_SJCB = L_SJCB + GS_ALV-SJCB  .
          L_WGBFB = L_WGBFB + GS_ALV-WGBFB  .
          L_GCJS = L_GCJS + GS_ALV-GCJS  .
          L_GCML = L_GCML + GS_ALV-GCML  .
          L_SJFY = L_SJFY + GS_ALV-SJFY  .
          L_GCSG = L_GCSG + GS_ALV-GCSG  .
          L_GCCB_FCSP = L_GCCB_FCSP + GS_ALV-GCCB_FCSP  .
          L_YSZK = L_YSZK + GS_ALV-YSZK  .
          L_YFZK = L_YFZK + GS_ALV-YFZK  .
          L_YFZG = L_YFZG + GS_ALV-YFZG  .
          L_Z8008 = L_Z8008 + GS_ALV-Z8008  .
          L_YKJSFPJE = L_YKJSFPJE + GS_ALV-YKJSFPJE  .

          " && 其它信息
          " 累计收款
          READ TABLE LT_ZFI005 WITH KEY XMBH = LS_PRPS-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC EQ 0 .
            LOOP AT LT_ZFI005 INTO LS_ZFI005 FROM SY-TABIX .
              IF LS_ZFI005-XMBH EQ LS_PRPS-POSID .
                GS_ALV-LJSK = GS_ALV-LJSK + LS_ZFI005-HSL .
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_ZFI005 .
            ENDLOOP.
          ENDIF.

          READ TABLE LT_ZFI005_SPLIT WITH KEY XMBH = LS_PRPS-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC EQ 0 .
            LOOP AT LT_ZFI005_SPLIT INTO LS_ZFI005_SPLIT FROM SY-TABIX .
              IF LS_ZFI005_SPLIT-XMBH EQ LS_PRPS-POSID .
                GS_ALV-LJSK = GS_ALV-LJSK + LS_ZFI005_SPLIT-HSL .
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_ZFI005_SPLIT .
            ENDLOOP.
          ENDIF.

          " 到期节点应收
          IF P_DATUM IS INITIAL .
            IF LS_PRPS-ZYSRQ1 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE1 .
            ENDIF.
            IF LS_PRPS-ZYSRQ2 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE2 .
            ENDIF.
            IF LS_PRPS-ZYSRQ3 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE3 .
            ENDIF.
            IF LS_PRPS-ZYSRQ4 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE4 .
            ENDIF.
            IF LS_PRPS-ZYSRQ5 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE5 .
            ENDIF.
            IF LS_PRPS-ZYSRQ6 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE6 .
            ENDIF.
            IF LS_PRPS-ZYSRQ7 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE7 .
            ENDIF.
            IF LS_PRPS-ZYSRQ8 LE SY-DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE8 .
            ENDIF.
          ELSE .
            IF LS_PRPS-ZYSRQ1 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE1 .
            ENDIF.
            IF LS_PRPS-ZYSRQ2 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE2 .
            ENDIF.
            IF LS_PRPS-ZYSRQ3 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE3 .
            ENDIF.
            IF LS_PRPS-ZYSRQ4 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE4 .
            ENDIF.
            IF LS_PRPS-ZYSRQ5 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE5 .
            ENDIF.
            IF LS_PRPS-ZYSRQ6 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE6 .
            ENDIF.
            IF LS_PRPS-ZYSRQ7 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE7 .
            ENDIF.
            IF LS_PRPS-ZYSRQ8 LE P_DATUM .
              GS_ALV-DQJDYS = GS_ALV-DQJDYS + LS_PRPS-ZYSJE8 .
            ENDIF.
          ENDIF.

          " 采购额（含税）、劳务合同金额、材料采购、分包采购
          READ TABLE LT_EKNP WITH KEY PS_PSP_PNR = LS_PRPS-PSPNR BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC EQ 0 .
            LOOP AT LT_EKNP INTO LS_EKNP FROM SY-TABIX .
              IF LS_EKNP-PS_PSP_PNR EQ LS_PRPS-PSPNR .
                " 采购额（含税）
                IF LS_EKNP-RETPO EQ 'X'.
                  GS_ALV-CGE_HS = GS_ALV-CGE_HS - LS_EKNP-BRTWR .
                ELSE .
                  GS_ALV-CGE_HS = GS_ALV-CGE_HS + LS_EKNP-BRTWR .
                ENDIF.
                " 劳务合同金额
                IF LS_EKNP-KNTTP EQ 'Y'.
                  IF LS_EKNP-RETPO EQ 'X'.
                    GS_ALV-LWHTJE = GS_ALV-LWHTJE - LS_EKNP-BRTWR .
                  ELSE .
                    GS_ALV-LWHTJE = GS_ALV-LWHTJE + LS_EKNP-BRTWR .
                  ENDIF.
                ENDIF.
                " 材料采购
                IF LS_EKNP-KNTTP EQ 'Q'.
                  IF LS_EKNP-RETPO EQ 'X'.
                    GS_ALV-CLCG = GS_ALV-CLCG - LS_EKNP-BRTWR .
                  ELSE .
                    GS_ALV-CLCG = GS_ALV-CLCG + LS_EKNP-BRTWR .
                  ENDIF.
                ENDIF.
                " 分包采购
                IF LS_EKNP-KNTTP EQ 'P'.
                  IF LS_EKNP-RETPO EQ 'X'.
                    GS_ALV-FBCG = GS_ALV-FBCG - LS_EKNP-BRTWR .
                  ELSE .
                    GS_ALV-FBCG = GS_ALV-FBCG + LS_EKNP-BRTWR .
                  ENDIF.
                ENDIF.
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_EKNP .
            ENDLOOP.
          ENDIF.

          " 已付金额
          READ TABLE LT_ZFI017 WITH KEY POSID = LS_PRPS-POSID BINARY SEARCH TRANSPORTING NO FIELDS .
          IF SY-SUBRC EQ 0 .
            LOOP AT LT_ZFI017 INTO LS_ZFI017 FROM SY-TABIX .
              IF LS_ZFI017-POSID EQ LS_PRPS-POSID .
                GS_ALV-YFJE = GS_ALV-YFJE + LS_ZFI017-ZSQFKJE .
              ELSE .
                EXIT .
              ENDIF.
              CLEAR LS_ZFI017 .
            ENDLOOP.
          ENDIF.

          " 未收款金额
          GS_ALV-WSKJE = GS_ALV-ZHTJR - GS_ALV-LJSK .

          " 后面汇总用
          L_LJSK = L_LJSK + GS_ALV-LJSK .
          L_DQJDYS = L_DQJDYS + GS_ALV-DQJDYS .
          L_CGE_HS = L_CGE_HS + GS_ALV-CGE_HS .
          L_YFJE = L_YFJE + GS_ALV-YFJE .
          L_WSKJE = L_WSKJE + GS_ALV-WSKJE .
          L_LWHTJE = L_LWHTJE + GS_ALV-LWHTJE .
          L_CLCG = L_CLCG + GS_ALV-CLCG .
          L_FBCG = L_FBCG + GS_ALV-FBCG .

          APPEND GS_ALV TO GT_ALV .
          CLEAR GS_ALV .

          REFRESH LT_BKSG3 .
          CLEAR LS_PRECP2 .
          CLEAR LS_KNA1 .
        ELSE .
          EXIT .
        ENDIF.
        CLEAR LS_PRPS .
      ENDLOOP.
    ENDIF.

* 小计
    GS_ALV-TXT = '小计' .
    " 项目基本信息
    GS_ALV-PSPID = LS_PROJ-PSPID .
    GS_ALV-POST = LS_PROJ-POST1 .
    GS_ALV-STUFE = 0 .
    GS_ALV-PRART = L_PRART .
    GS_ALV-ZHTLX = L_ZHTLX .
    GS_ALV-ZKHBM = LS_PROJ-ZKHBM .
    IF GS_ALV-ZKHBM IS INITIAL .
      GS_ALV-ZKHBM = L_ZKHBM .
    ENDIF.
    GS_ALV-ZHTJR = LS_PROJ-ZHTJE .
    IF GS_ALV-ZHTJR IS INITIAL .
      GS_ALV-ZHTJR = L_ZHTJR .
    ENDIF.
    GS_ALV-ZJGJSJE = LS_PROJ-ZJGJSJE .
    " 项目预算信息
    GS_ALV-YSSR = L_YSSR .
    GS_ALV-YSCB = L_YSCB .
    GS_ALV-YSMLL = L_YSMLL .
    GS_ALV-YSFY = L_YSFY .
    " 项目实际财务信息
    GS_ALV-SJSR = L_SJSR .
    GS_ALV-SJCB = L_SJCB .
    IF GS_ALV-SJSR IS NOT INITIAL AND GS_ALV-SJSR NE 0 .
      GS_ALV-SJMLL = ( GS_ALV-SJSR - GS_ALV-SJCB ) / GS_ALV-SJSR .
    ELSE .
      GS_ALV-SJMLL = 0 .
    ENDIF.
    GS_ALV-WGBFB = L_WGBFB .
    GS_ALV-GCJS = L_GCJS .
    GS_ALV-GCML = L_GCML .
    GS_ALV-SJFY = L_SJFY .
    GS_ALV-GCSG = L_GCSG .
    GS_ALV-GCCB_FCSP = L_GCCB_FCSP .
    GS_ALV-YSZK = L_YSZK .
    GS_ALV-YFZK = L_YFZK .
    GS_ALV-YFZG = L_YFZG .
    GS_ALV-Z8008 = L_Z8008 .
    GS_ALV-YKJSFPJE = L_YKJSFPJE .
    IF GS_ALV-ZJGJSJE IS NOT INITIAL .
      GS_ALV-WKJSFPJE = GS_ALV-ZJGJSJE - GS_ALV-YKJSFPJE .
    ELSE .
      GS_ALV-WKJSFPJE = GS_ALV-ZHTJR - GS_ALV-YKJSFPJE .
    ENDIF.
    " 其它信息
    GS_ALV-LJSK = L_LJSK .
    GS_ALV-DQJDYS = L_DQJDYS.
    GS_ALV-CGE_HS = L_CGE_HS .
    GS_ALV-YFJE = L_YFJE .
    GS_ALV-WSKJE = L_WSKJE .
    GS_ALV-LWHTJE = L_LWHTJE .
    GS_ALV-CLCG = L_CLCG .
    GS_ALV-FBCG = L_FBCG .
    " 设置行颜色
    GS_ALV-CLR = 'C310' .

    APPEND GS_ALV TO GT_ALV .
    CLEAR GS_ALV .

    CLEAR: L_PRART ,L_ZHTLX ,L_ZKHBM ,L_ZHTJR ,
           L_YSSR ,L_YSCB ,L_YSMLL ,L_YSFY ,
           L_SJSR ,L_SJCB ,L_WGBFB ,L_GCJS ,L_GCML ,L_SJFY ,L_GCSG ,L_GCCB_FCSP ,L_YSZK ,L_YFZK ,L_YFZG ,L_Z8008 ,L_YKJSFPJE ,
           L_LJSK ,L_DQJDYS ,L_CGE_HS ,L_YFJE ,L_WSKJE ,L_LWHTJE ,L_CLCG ,L_FBCG .
    CLEAR LS_PROJ .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHG_MWSKZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BKSG3_MWSKZ  text
*      -->P_L_MWSKZ  text
*----------------------------------------------------------------------*
FORM FRM_CHG_MWSKZ  USING    P_MWSKZ
                             P_L_MWSKZ.
  CASE P_MWSKZ .
    WHEN '' .
      P_L_MWSKZ = '0.17' .
    WHEN 'X0' .
      P_L_MWSKZ = '0.00' .
    WHEN 'X1' .
      P_L_MWSKZ = '0.17' .
    WHEN 'X2' .
      P_L_MWSKZ = '0.06' .
    WHEN 'X3' .
      P_L_MWSKZ = '0.00' .
    WHEN 'X4' .
      P_L_MWSKZ = '0.17' .
    WHEN 'X5' .
      P_L_MWSKZ = '0.03' .
    WHEN 'X6' .
      P_L_MWSKZ = '0.11' .
    WHEN OTHERS.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SHOW_ALV .
  PERFORM INIT_LAYOUT.             "设置输出格式
*  PERFORM INIT_SORT.               "设置排序、合计
*  PERFORM INIT_VARIANT.            "设置变式控制
  PERFORM FRM_INIT_LVC.            "字段列定义
*  PERFORM FRM_EXCLUDE.
*  PERFORM FRM_BUILD_EVENT.
*  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
  PERFORM FRM_OUTPUT  .
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
  GS_LAYOUT-ZEBRA        = 'X'.
  GS_LAYOUT-CWIDTH_OPT   = 'X'.
  GS_LAYOUT-BOX_FNAME = 'SEL'.
  GS_LAYOUT-INFO_FNAME = 'CLR'.
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
  INIT_FIELDCAT 'NUM'          '序号'         '' 'X' '' '' '' '' ''.
  " 项目基本信息
  INIT_FIELDCAT 'PSPID'        '项目定义'         '' 'X' '' '' '' '' ''.
  INIT_FIELDCAT 'POST'        '项目名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POSID'        'WBS元素'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'POST1'        'WBS描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'STUFE'        '层级'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'PRART'        '项目类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHTLX'        '合同类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZKHBM'        '客户编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'NAME1'        '客户描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHTJR'        '合同金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZJGJSJE'        '竣工决算金额'         '' '' '' '' '' '' ''.
  " 项目预算信息
  INIT_FIELDCAT 'YSSR'        '预算收入'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSCB'        '预算成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSMLL'        '预算毛利率'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSFY'        '预算费用'         '' '' '' '' '' '' ''.
  " 项目实际财务信息
  INIT_FIELDCAT 'SJSR'        '实际收入'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJCB'        '实际成本'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJMLL'        '实际毛利率'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WGBFB'        '完工百分比'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCJS'        '工程结算'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCML'        '工程毛利'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJFY'        '实际费用'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCSG'        '工程施工'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GCCB_FCSP'        '工程成本/发出商品'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YSZK'        '应收账款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFZK'        '应付账款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFZG'        '应付暂估'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'Z8008'        '8008'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YKJSFPJE'        '已开金税发票金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WKJSFPJE'        '未开金税发票金额'         '' '' '' '' '' '' ''.
  " 其他信息
  INIT_FIELDCAT 'LJSK'        '累计收款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'DQJDYS'        '到期节点的应收'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CGE_HS'        '采购额（含税）'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LWHTJE'        '劳务合同金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CLCG'        '材料采购'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'FBCG'        '分包采购'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'YFJE'        '已付金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WSKJE'        '未收款金额'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'TXT'        '摘要'         '' '' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      I_CALLBACK_PROGRAM = SY-REPID
*     I_CALLBACK_PF_STATUS_SET = PU_STATUS
*     I_CALLBACK_USER_COMMAND  = PU_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   = ''
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    = PW_GRID_SETTINGS
      IS_LAYOUT_LVC      = GS_LAYOUT
      IT_FIELDCAT_LVC    = GT_LVC
*     IT_EXCLUDING       = GT_EXCLUDE
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        = PT_SORT[]
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      I_SAVE             = 'A'
*     IS_VARIANT         = PW_VARIANT
*     IT_EVENTS          = GT_EVENTS
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB           = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
