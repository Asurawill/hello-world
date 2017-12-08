REPORT ZPS011.
"ADD  IT02 2015 2217
"项目产值明细表

TABLES:PROJ,PRPS,MARA,MAKT,ZMM024,MSEG.
DATA:BEGIN OF GS_DATA,
        PSPID      TYPE PROJ-PSPID,      "项目编号
        POST1      TYPE PROJ-POST1,      "项目名称
        PS_PSP_PNR TYPE MSEG-PS_PSP_PNR, "WBS
        BUDAT_MKPF  TYPE MSEG-BUDAT_MKPF,"过账日期
        BWART       TYPE MSEG-BWART,     "移动类型
        WEMPF      TYPE MSEG-WEMPF,      "劳务方
        LIFNR      TYPE MSEG-LIFNR,      "劳务供应商编号
        LIFNR_NAME TYPE LFA1-NAME1 ,     "劳务供应商名称
        SJDM       TYPE ZMM024-SJDM,     "设计代码
        MATNR      TYPE MSEG-MATNR,      "物料号
        MAKTX      TYPE MAKT-MAKTX,      "物料描述
        GPREIS     TYPE CKIS-GPREIS,     "物料综合单价
       " FPREIS     TYPE CKIS-FPREIS,     "人工预算单价
        KBETR      TYPE KONV-KBETR,      "人工单价
        CKL        TYPE MSEG-MENGE,      "出库量
        CZ         TYPE CKIS-GPREIS,     "产值
        LWJDK       TYPE CKIS-GPREIS,     "劳务进度款
       " LWYDJDK    TYPE CKIS-GPREIS,     "劳务月度进度款
       " LWYDJDK1   TYPE CKIS-GPREIS,     "劳务月度进度款(预算)
        ZHTJE      TYPE PROJ-ZHTJE,      "合同金额
        SEL(1),
      END OF GS_DATA.
DATA:BEGIN OF GS_MSEG,
        MAT_PSPNR TYPE PS_PSP_PNR ,  "WBS号
        MJAHR TYPE MSEG-MJAHR,
        MBLNR TYPE MSEG-MBLNR,
        ZEILE TYPE MSEG-ZEILE,
        MATNR TYPE MSEG-MATNR,
        MENGE TYPE MENGE_D,
        WERKS TYPE WERKS,
        LIFNR TYPE LIFNR,
        BUDAT_MKPF   TYPE MSEG-BUDAT_MKPF,           "过账日期
        BWART TYPE MSEG-BWART,
        SHKZG TYPE SHKZG,
        WEMPF TYPE MSEG-WEMPF,
    END OF GS_MSEG.

DATA:BEGIN OF GS_CG,
     EBELN TYPE EKKN-EBELN,
     EBELP TYPE EKKN-EBELP,
     PS_PSP_PNR TYPE EKKN-PS_PSP_PNR,
     MATNR TYPE EKPO-MATNR,
     NETPR TYPE EKPO-NETPR,
    END OF GS_CG .

DATA:GT_CG LIKE TABLE OF GS_CG WITH HEADER LINE .
DATA:GT_MSEG1_SUM  LIKE TABLE OF GS_MSEG WITH HEADER LINE.
DATA:GT_MSEG_FL LIKE TABLE OF MSEG WITH HEADER LINE.
DATA: GT_DATA LIKE TABLE OF GS_DATA  WITH HEADER LINE.

DATA: GT_PROJ LIKE TABLE OF PROJ WITH HEADER LINE.

DATA: GT_PRPS LIKE TABLE OF PRPS WITH HEADER LINE.

DATA:GT_MSEG LIKE TABLE OF GS_MSEG WITH HEADER LINE .

DATA: GT_MAKT  LIKE  TABLE OF MAKT WITH HEADER  LINE.

DATA: GT_EKKN LIKE TABLE OF EKKN WITH HEADER LINE.

DATA: GT_EKPO  LIKE TABLE OF EKPO WITH HEADER LINE.

DATA: GT_KONV LIKE TABLE OF KONV  WITH HEADER LINE.

DATA: GT_ZPSPROD LIKE TABLE OF ZPSPROD WITH HEADER LINE.

DATA: GT_ZMM024 LIKE TABLE OF ZMM024 WITH HEADER LINE.

DATA: GT_EKKO LIKE TABLE OF EKKO WITH HEADER LINE.

DATA: GT_T156T LIKE TABLE OF T156T WITH HEADER LINE.

DATA:GT_LFA1 LIKE TABLE OF LFA1 WITH HEADER LINE.

RANGES:R_BWART FOR MSEG-BWART.
  DATA E_WBS_ECP        TYPE TTY_PROJ_ELEMENT_CK_ITEMS_RDEX.
  DATA LT_E_WBS_ECP     TYPE TABLE OF PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LS_E_WBS_ECP     TYPE PROJ_ELEMENT_CK_ITEMS_RDEXP.
  DATA LT_COST_LINES    TYPE TABLE OF KIS1.
  DATA LS_COST_LINES    TYPE KIS1.
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

IF gw_lvc-fieldname = 'PROJK'.
   gw_lvc-NO_ZERO = 'X'.
ENDIF.

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
SELECT-OPTIONS:S_PSPID FOR PROJ-PSPID,      "项目定义
               S_VERNR FOR PROJ-VERNR,      "项目经理
               S_BUDAT FOR MSEG-BUDAT_MKPF, "过账日期
               S_LIFNR FOR  MSEG-LIFNR.     "供应商编码
*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*CLEAR :R_BWART.
*"('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .
*R_BWART-SIGN = 'I'.
*R_BWART-OPTION = 'EQ'.
*R_BWART-LOW = '281'.
*APPEND R_BWART.
*R_BWART-LOW = '282'.
*APPEND R_BWART.
*R_BWART-LOW = '221'.
*APPEND R_BWART.
*R_BWART-LOW = '222'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z19'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z20'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z21'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z22'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z23'.
*APPEND R_BWART.
*R_BWART-LOW = 'Z24'.
*APPEND R_BWART.

*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM xxxxxxx.
*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
**权限检查检查公司代码
*  " PERFORM FRM_AUTH_CHECK USING '03'.
*  PERFORM FRM_AUTH_CHECK.
*  IF SY-SUBRC NE 0.
*    MESSAGE I011(ZFICO01) WITH S_BUKRS-LOW DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  PERFORM FRM_GET_DATA.
  PERFORM FRM_DEAL_DATA.
  PERFORM FRM_ALV_SHOW. "ALV显示

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
 SELECT * FROM PROJ
   INTO CORRESPONDING FIELDS OF TABLE GT_PROJ
   WHERE PSPID IN S_PSPID
   AND   VERNR IN S_VERNR.
SORT  GT_PROJ BY PSPNR PSPID .
CHECK GT_PROJ[] IS NOT INITIAL.

 SELECT * FROM PRPS
    INTO CORRESPONDING FIELDS OF TABLE GT_PRPS
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPHI = GT_PROJ-PSPNR.
SORT GT_PRPS BY PSPNR POSID.

SELECT * FROM ZMM024
     INTO CORRESPONDING FIELDS OF TABLE GT_ZMM024
     FOR ALL ENTRIES IN GT_PROJ
     WHERE POSID = GT_PROJ-PSPID.
SORT GT_ZMM024 BY MATNR POSID.

   SELECT * FROM ZPSPROD
    INTO CORRESPONDING FIELDS OF TABLE GT_ZPSPROD
    FOR ALL ENTRIES IN GT_PROJ
    WHERE PSPID = GT_PROJ-PSPID.
SORT GT_ZPSPROD BY PSPID.
CHECK GT_PRPS[] IS NOT INITIAL.
    SELECT EKKN~EBELN EKKN~EBELP
           EKKN~PS_PSP_PNR  EKPO~MATNR EKPO~NETPR
        FROM  EKKN
        INNER JOIN EKPO
        ON EKKN~EBELN = EKPO~EBELN
        AND  EKKN~EBELP = EKPO~EBELP
        INTO CORRESPONDING FIELDS OF TABLE GT_CG
        FOR ALL ENTRIES IN GT_PRPS
        WHERE EKKN~PS_PSP_PNR  = GT_PRPS-PSPNR
        AND  EKKN~LOEKZ NE 'L'
        AND  EKPO~KNTTP = 'Y'
        AND  EKPO~LOEKZ NE 'L'
        AND  EKPO~MATNR NE ''.
 SORT GT_CG  BY PS_PSP_PNR ascending MATNR ascending EBELN descending EBELP ascending .
 DELETE ADJACENT  DUPLICATES FROM GT_CG COMPARING PS_PSP_PNR MATNR  EBELN.

*    SELECT * FROM EKKN
*    INTO CORRESPONDING FIELDS OF TABLE GT_EKKN
*     FOR ALL ENTRIES IN GT_PRPS
*     WHERE PS_PSP_PNR  = GT_PRPS-PSPNR
*     AND  LOEKZ <> 'L'.
*SORT GT_EKKN BY PS_PSP_PNR .
*CHECK GT_EKKN[] IS NOT INITIAL.
*
*SELECT * FROM EKPO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKPO
*      FOR ALL ENTRIES IN GT_EKKN
*      WHERE EBELN = GT_EKKN-EBELN
*      AND   EBELP = GT_EKKN-EBELP
*      AND   LOEKZ <> 'L'
*      AND   KNTTP = 'Y'.
*SORT GT_EKPO BY EBELN EBELP.
*      SELECT * FROM EKKO
*      INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
*      FOR ALL ENTRIES IN GT_EKPO
*      WHERE EBELN = GT_EKPO-EBELN.
*SORT GT_EKKO BY EBELN .
*      SELECT * FROM KONV
*      INTO CORRESPONDING FIELDS OF TABLE GT_KONV
*      FOR ALL ENTRIES IN GT_EKKO
*      WHERE KNUMV = GT_EKKO-KNUMV
*        AND STUNR = '001'.
*SORT GT_KONV BY KNUMV KPOSN.
CHECK GT_PRPS[] IS NOT INITIAL.

*    SELECT MJAHR MBLNR ZEILE MATNR MENGE PS_PSP_PNR  BUDAT_MKPF BWART SHKZG  WEMPF FROM MSEG
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
*    FOR ALL ENTRIES IN GT_PRPS
*    WHERE PS_PSP_PNR = GT_PRPS-PSPNR
*    AND   BUDAT_MKPF IN S_BUDAT
*    AND   BWART IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .
 IF GT_PRPS[] IS NOT INITIAL.
   SELECT MJAHR MBLNR ZEILE MATNR
          MENGE WERKS LIFNR BUDAT_MKPF
          BWART SHKZG  WEMPF MAT_PSPNR
     INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
     FROM MSEG
     FOR ALL ENTRIES IN GT_PRPS
     WHERE MAT_PSPNR = GT_PRPS-PSPNR
     AND BUDAT_MKPF IN S_BUDAT
     AND BWART IN ('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' )
     AND  MSEG~LIFNR IN S_LIFNR .

  SORT GT_MSEG  BY MAT_PSPNR .

 ENDIF.



 CHECK GT_MSEG[] IS NOT INITIAL.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
   FROM LFA1
   FOR ALL ENTRIES IN GT_MSEG
   WHERE LIFNR = GT_MSEG-LIFNR .
   SORT GT_LFA1 BY  LIFNR .
 "DELETE ADJACENT DUPLICATES  FROM GT_MSEG COMPARING MJAHR MBLNR ZEILE .
* MOVE-CORRESPONDING GT_MSEG[] TO GT_MSEG1[].
* MOVE-CORRESPONDING GT_MSEG[] TO GT_MSEG_FL[].
* SORT GT_MSEG_FL BY WERKS MATNR PS_PSP_PNR BUDAT_MKPF .
* DELETE ADJACENT DUPLICATES FROM GT_MSEG_FL COMPARING WERKS MATNR PS_PSP_PNR BUDAT_MKPF .


*DELETE GT_MSEG1 WHERE BWART NOT IN  R_BWART."('281','282','221','222','Z19','Z20','Z21','Z22','Z23','Z24' ) .

 "SORT GT_MSEG1 BY WERKS MATNR PS_PSP_PNR BUDAT_MKPF .
  SELECT * FROM MAKT
    INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
    FOR ALL ENTRIES IN GT_MSEG
    WHERE MATNR = GT_MSEG-MATNR
    AND SPRAS = '1'.
  SORT GT_MAKT BY MATNR .
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

DATA:PSPID TYPE PROJ-PSPID,
     POST1 TYPE PROJ-POST1,
     ZHTJE TYPE PROJ-ZHTJE,
     PSPNR TYPE PRPS-PSPNR .
LOOP AT  GT_MSEG.
  AT NEW MAT_PSPNR.
    CLEAR:PSPID,POST1,ZHTJE,PSPNR.
    READ TABLE GT_PRPS WITH KEY PSPNR = GT_MSEG-MAT_PSPNR BINARY SEARCH .
      IF SY-SUBRC EQ 0 .
           PSPNR = GT_PRPS-PSPNR.
          READ TABLE  GT_PROJ WITH KEY PSPNR = GT_PRPS-PSPHI.
             IF SY-SUBRC EQ 0.
                PSPID = GT_PROJ-PSPID.
                POST1 = GT_PROJ-POST1. "项目名称
                ZHTJE = GT_PROJ-ZHTJE.  "合同金额
            ENDIF.
      ENDIF.

    CLEAR:E_WBS_ECP,
          LS_E_WBS_ECP,
          LS_COST_LINES.
         .

    REFRESH:LT_E_WBS_ECP,
            LT_COST_LINES.

    CALL FUNCTION 'CNECP_READ'
      EXPORTING
        I_PROJ_DEF    = PSPID
        I_VERSION     = '100'
      IMPORTING
        E_WBS_ECP     = E_WBS_ECP
      EXCEPTIONS
        ERROR_MESSAGE = 1.

    LT_E_WBS_ECP = E_WBS_ECP.

     IF  LT_E_WBS_ECP IS NOT INITIAL.
      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
      INDEX 1.
      IF SY-SUBRC = 0.
        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.
        DELETE LT_COST_LINES WHERE  TYPPS NE 'M' .
      ENDIF.
     ENDIF.
ENDAT.
 CLEAR:GS_DATA.
  GS_DATA-PSPID = PSPID. "项目号
 GS_DATA-POST1 = POST1. "项目名称
 GS_DATA-ZHTJE = ZHTJE.  "合同金额
 "移动类型
 GS_DATA-BWART = GT_MSEG-BWART .

 GS_DATA-PS_PSP_PNR = PSPNR.
  "物料号
  GS_DATA-MATNR = GT_MSEG-MATNR.

  "物料描述
   READ TABLE GT_MAKT WITH KEY MATNR = GS_DATA-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_DATA-MAKTX = GT_MAKT-MAKTX.
    ENDIF.
  "过账日期
  GS_DATA-BUDAT_MKPF = GT_MSEG-BUDAT_MKPF .
 "移动类型
 GS_DATA-BWART = GT_MSEG-BWART .
  IF  GT_MSEG-SHKZG = 'S'.
    GS_DATA-CKL = GT_MSEG-MENGE * -1.
  ELSE.
     GS_DATA-CKL = GT_MSEG-MENGE.
  ENDIF.
    "设计代码
   READ TABLE GT_ZMM024 WITH KEY  MATNR = GS_DATA-MATNR POSID = GS_DATA-PSPID BINARY SEARCH .
    IF SY-SUBRC = 0.
     GS_DATA-SJDM = GT_ZMM024-SJDM.
    ENDIF.
  "劳务方
  GS_DATA-WEMPF = GT_MSEG-WEMPF.
  "劳务供应商编号
  GS_DATA-LIFNR = GT_MSEG-LIFNR .
  READ TABLE GT_LFA1 WITH KEY  LIFNR = GT_MSEG-LIFNR .
  IF SY-SUBRC = 0.
    GS_DATA-LIFNR_NAME = GT_LFA1-NAME1.
  ENDIF.
  "物料综合单价
  PERFORM WLZHDJ_JS  USING  GS_DATA-PSPID GS_DATA-MATNR CHANGING  GS_DATA-GPREIS .
     IF   GS_DATA-GPREIS = 0 .
     LOOP AT GT_ZMM024 WHERE SJDM = GS_DATA-SJDM  AND POSID = GS_DATA-PSPID AND  MATNR NE  GS_DATA-MATNR.
         PERFORM WLZHDJ_JS  USING  GS_DATA-PSPID GT_ZMM024-MATNR CHANGING  GS_DATA-GPREIS .
         IF GS_DATA-GPREIS NE 0 .
            EXIT.
          ENDIF.

     ENDLOOP.

  ENDIF.

 " *人工单价
 READ TABLE GT_CG WITH KEY  PS_PSP_PNR = GS_DATA-PS_PSP_PNR
                            MATNR = GS_DATA-MATNR
                            BINARY SEARCH .
 IF SY-SUBRC EQ 0 .

      GS_DATA-KBETR = GT_CG-NETPR.
 ENDIF.
* READ TABLE GT_EKKN WITH KEY PS_PSP_PNR = GS_DATA-PS_PSP_PNR MATNR = GS_DATA-MATNR.
* IF SY-SUBRC EQ 0 .
*     READ TABLE GT_EKPO WITH KEY EBELN = GT_EKKN-EBELN
*                EBELP = GT_EKKN-EBELP  KNTTP = 'Y'.
*     IF SY-SUBRC EQ 0 .
*          GS_DATA-KBETR = GT_EKPO-NETPR.
*
*    ENDIF.
*
*
* ENDIF.
*
*    LOOP AT  GT_EKKN
*    WHERE  PS_PSP_PNR = GS_DATA-PS_PSP_PNR.
*      READ TABLE GT_EKPO
*      WITH KEY EBELN = GT_EKKN-EBELN
*               EBELP = GT_EKKN-EBELP
*               MATNR = GS_DATA-MATNR
*               KNTTP = 'Y'.
*      IF SY-SUBRC = 0.
*        READ TABLE GT_EKKO
*        WITH KEY EBELN = GT_EKPO-EBELN.
*        IF SY-SUBRC = 0.
*          READ TABLE GT_KONV
*          WITH KEY KNUMV = GT_EKKO-KNUMV
*                   KPOSN = GT_EKPO-EBELP
*                   STUNR = 1.
*          IF SY-SUBRC = 0.
*            GS_DATA-KBETR = GT_KONV-KBETR.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

 "产值
  GS_DATA-CZ = GS_DATA-CKL * GS_DATA-GPREIS.
" 劳务进度款
  GS_DATA-LWJDK = GS_DATA-CKL * GS_DATA-KBETR.
  APPEND GS_DATA TO GT_DATA.
 CLEAR GS_DATA.


ENDLOOP.

LOOP AT GT_ZPSPROD  .
    GS_DATA-PSPID = GT_ZPSPROD-PSPID.

    SELECT SINGLE POST1
    INTO GS_DATA-POST1 FROM PROJ
    WHERE PSPID = GS_DATA-PSPID.

    GS_DATA-MAKTX = '期初项目产值、劳务进度款'.
    GS_DATA-CZ    = GT_ZPSPROD-ZCZ.
    GS_DATA-LWJDK = GT_ZPSPROD-ZLWK.

    APPEND GS_DATA TO GT_DATA.
    CLEAR:GS_DATA,
         GT_DATA.
  ENDLOOP.

  SORT GT_DATA BY PSPID BUDAT_MKPF MATNR.





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
  GW_GRID_SETTINGS-EDT_CLL_CB = 'X'.
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
 GW_LAYOUT-ZEBRA         = 'X'.
  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-BOX_FNAME     = 'SEL'.
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
 INIT_FIELDCAT 'PSPID'        '项目编号'         '' '' '' '' 'X' '' ''.
  INIT_FIELDCAT 'POST1'        '项目名称'         '' '' '' '' '' '' ''.
 " INIT_FIELDCAT 'PS_PSP_PNR'   'WBS'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BUDAT_MKPF'   '过账日期'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'BWART'   '移动类型'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'WEMPF'   '劳务方'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR'   '劳务供应商编号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LIFNR_NAME'   '劳务供应商名称'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'SJDM'         '设计代码'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MATNR'        '物料号'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'MAKTX'        '物料描述'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'GPREIS'       '物料综合单价'         '' '' '' '' '' '' ''.
  "INIT_FIELDCAT 'FPREIS'       '人工预算单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'KBETR'        '人工单价'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CKL'       '出库量'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'CZ'           '产值'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'LWJDK'      '劳务进度款'         '' '' '' '' '' '' ''.
  INIT_FIELDCAT 'ZHTJE'       '合同金额'         '' '' '' '' '' '' ''.

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
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_EVENT .
 GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_1090   text
*      -->P_1091   text
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
  SET PF-STATUS 'STANDARD_SCREEN' EXCLUDING RT_EXTAB.
ENDFORM.                    "

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

  DATA G_REF_GRID TYPE REF TO CL_GUI_ALV_GRID. "刷新行到内表


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_REF_GRID.
*
*  CASE R_UCOMM.
*** 双击
**    WHEN '&IC1'.
**      READ TABLE GT_DATA INTO GS_DATA INDEX RS_SELFIELD-TABINDEX.
**      CHECK SY-SUBRC = 0.
**      IF RS_SELFIELD-FIELDNAME = 'PSPID'
**        AND GS_DATA-PSPID IS NOT INITIAL.
**        SET PARAMETER ID 'PSP' FIELD GS_DATA-PSPID.
**        CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
**      ENDIF.
*
*  ENDCASE.



ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  WLZHDJ_JS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DATA_PSPID  text
*      <--P_GS_DATA_GPREIS  text
*----------------------------------------------------------------------*
FORM WLZHDJ_JS  USING    P_PSPID  LIKE PROJ-PSPID
                         P_MATNR  LIKE MSEG-MATNR
                CHANGING P_GPREIS LIKE CKIS-GPREIS.
*CLEAR:E_WBS_ECP,
*          LS_E_WBS_ECP,
*          LS_COST_LINES,
*          P_GPREIS.
*
*    REFRESH:LT_E_WBS_ECP,
*            LT_COST_LINES.
*
*    CALL FUNCTION 'CNECP_READ'
*      EXPORTING
*        I_PROJ_DEF    = P_PSPID
*        I_VERSION     = '100'
*      IMPORTING
*        E_WBS_ECP     = E_WBS_ECP
*      EXCEPTIONS
*        ERROR_MESSAGE = 1.
*
*    LT_E_WBS_ECP = E_WBS_ECP.
*
*        IF  LT_E_WBS_ECP IS NOT INITIAL.
*      READ TABLE LT_E_WBS_ECP INTO LS_E_WBS_ECP
*      INDEX 1.
*      IF SY-SUBRC = 0.
*        LT_COST_LINES = LS_E_WBS_ECP-COST_LINES.

        READ TABLE   LT_COST_LINES INTO LS_COST_LINES
        WITH KEY  MATNR = P_MATNR.
        IF SY-SUBRC = 0.
          P_GPREIS = LS_COST_LINES-GPREIS.
        ENDIF.
*      ENDIF.
*    ENDIF.
ENDFORM.
