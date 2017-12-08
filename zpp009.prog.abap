*&---------------------------------------------------------------------*
*& Report  ZPP009
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/02/27
*& Request       :
*& Descriptions  : 拆解订单创建
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZPP009.

************************************************************************
* Tables
************************************************************************
TABLES: MAST,STKO,AUFK,AFKO.

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS:P_WERKS TYPE MAST-WERKS OBLIGATORY DEFAULT '1100',
           P_MANTR TYPE MAST-MATNR OBLIGATORY,
           P_STLAN TYPE MAST-STLAN OBLIGATORY DEFAULT '1',
           P_STLST TYPE STKO-STLST OBLIGATORY DEFAULT '2',
           P_STLAL TYPE MAST-STLAL.
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS:P_AUFNR TYPE AUFK-AUFNR OBLIGATORY,
           P_KTEXT TYPE AUFK-KTEXT OBLIGATORY,
           P_AUART TYPE AUFK-AUART OBLIGATORY DEFAULT 'ZP03',
           P_DISPO TYPE AFKO-DISPO OBLIGATORY,
           P_FEVOR TYPE AFKO-FEVOR OBLIGATORY,
           P_GAMNG TYPE AFKO-GAMNG OBLIGATORY,
           P_GMEIN TYPE AFKO-GMEIN OBLIGATORY,
           P_GSTRP TYPE AFKO-GSTRP OBLIGATORY,
           P_GLTRP TYPE AFKO-GLTRP OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLK2.

PERFORM FRM_AUTH_CHECK.

************************************************************************
* Internal Table * WorkArea
************************************************************************
DATA: GT_STB TYPE STANDARD TABLE OF STPOX,
      GS_STB LIKE LINE OF GT_STB.

DATA:GS_TOPMAT TYPE CSTMAT.

DATA:BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

*展开BOM
CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
  EXPORTING
    CAPID                 = 'PP01'
    DATUV                 = SY-DATUM
    MMORY                 = '1'
    STLAL                 = P_STLAL
    MDMPS                 = ''
    MEHRS                 = ''    "多层
    MTNRV                 = P_MANTR
    WERKS                 = P_WERKS
*   stlan                 = '1'
  IMPORTING
    TOPMAT                = GS_TOPMAT
  TABLES
    STB                   = GT_STB
  EXCEPTIONS
    ALT_NOT_FOUND         = 1
    CALL_INVALID          = 2
    MATERIAL_NOT_FOUND    = 3
    MISSING_AUTHORIZATION = 4
    NO_BOM_FOUND          = 5
    NO_PLANT_DATA         = 6
    NO_SUITABLE_BOM_FOUND = 7
    CONVERSION_ERROR      = 8
    OTHERS                = 9.

PERFORM FRM_PREPARE_BDC_2 TABLES GT_STB.


FORM FRM_PREPARE_BDC_2 TABLES GT_STB .
  " Global variable
  DATA: G_MSGTXT(200).
  DATA: L_TABIX     TYPE N LENGTH 2.
  DATA: L_TABIX_1   TYPE N LENGTH 4.

  DATA: L_STR TYPE STRING.

*其他类型转化STRING
  DATA: L_GAMNG   TYPE STRING.
  DATA: L_GSTRP   TYPE STRING.
  DATA: L_GLTRP   TYPE STRING.
  DATA: L_MENGE   TYPE STRING.
  DATA: L_MENGE1  TYPE STPO-MENGE.

  CLEAR G_MSGTXT.
  CLEAR L_TABIX.
  CLEAR L_TABIX_1.

  L_GAMNG = P_GAMNG.
  L_GSTRP = P_GSTRP.
  L_GLTRP = P_GLTRP.

  PERFORM BDC_DYNPRO      USING 'SAPLCOKO1' '0210'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-AUFNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'CAUFVD-WERKS'
                                 P_WERKS.
  PERFORM BDC_FIELD       USING 'AUFPAR-PP_AUFART'
                                 P_AUART.
  PERFORM BDC_FIELD       USING 'CAUFVD-AUFNR'
                                 P_AUFNR.
  PERFORM BDC_FIELD       USING 'CAUFVD-DISPO'
                                 P_DISPO.
  PERFORM BDC_FIELD       USING 'CAUFVD-FEVOR'
                                 P_FEVOR.

  PERFORM BDC_DYNPRO      USING 'SAPLCOKO1' '0115'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'CAUFVD-MATXT'
                                P_KTEXT.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-GSTRP'.
  PERFORM BDC_FIELD       USING 'CAUFVD-GAMNG'
                                L_GAMNG.
  PERFORM BDC_FIELD       USING 'CAUFVD-GMEIN'
                                P_GMEIN.
  PERFORM BDC_FIELD       USING 'CAUFVD-GSTRP'
                                L_GSTRP.
  PERFORM BDC_FIELD       USING 'CAUFVD-GLTRP'
                                L_GLTRP.


  PERFORM BDC_DYNPRO      USING 'SAPLCOSD' '0310'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=CANC'.

  PERFORM BDC_DYNPRO      USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'DKOBR-EMPGE(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'COBRB-KONTY(01)'
                                'G/L'.
  PERFORM BDC_FIELD       USING 'DKOBR-EMPGE(01)'
                                '8003000057'.


  PERFORM BDC_DYNPRO      USING 'SAPLKOBS' '0130'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'COBRB-KONTY(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BACK'.

  PERFORM BDC_DYNPRO      USING 'SAPLCOKO1' '0115'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-MATXT'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=KPU2'.


  LOOP AT GT_STB INTO GS_STB.
*转换成字符型
    CLEAR L_MENGE.
*订单组件数量 = BOM中的数量 /BOM表头数量 * 订单数量 * (-1)
    L_MENGE1 = ( GS_STB-MENGE / GS_TOPMAT-BMENG ) * P_GAMNG * -1.
    L_MENGE = L_MENGE1.

*根据定位，除了第一行录入第一行，其余行都是从第二行开始录入
    IF SY-TABIX = 1.
      L_TABIX = 1.
    ELSE.
      L_TABIX = 2.
    ENDIF.

    PERFORM BDC_DYNPRO      USING 'SAPLCOMK' '0120'.

    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                   'RESBD-POSTP(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'FILTER_BOX'
                                  'NO_FIL'.
    PERFORM BDC_FIELD       USING 'SORT_BOX'
                                  'ST_STA'.

    CLEAR L_STR.
    CONCATENATE 'RESBD-MATNR' '('  L_TABIX   ')' INTO L_STR.
    PERFORM BDC_FIELD       USING L_STR
                                  GS_STB-IDNRK.

    CLEAR L_STR.
    CONCATENATE 'RESBD-MENGE' '('  L_TABIX   ')' INTO L_STR.
    PERFORM BDC_FIELD       USING L_STR
                                  L_MENGE.

    CLEAR L_STR.
    CONCATENATE 'RESBD-POSTP' '('  L_TABIX   ')' INTO L_STR.
    PERFORM BDC_FIELD       USING L_STR
                                  'L'.

    CLEAR L_STR.
    CONCATENATE 'RESBD-VORNR' '('  L_TABIX   ')' INTO L_STR.
    PERFORM BDC_FIELD       USING  L_STR
                                  '0010'.

    PERFORM BDC_DYNPRO      USING 'SAPLCOMD' '0110'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.

    L_TABIX_1 = L_TABIX_1 + 10.

    PERFORM BDC_DYNPRO      USING 'SAPLCOMK' '0120'.

    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                   'RESBD-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=AUFS'.
    PERFORM BDC_FIELD       USING 'FILTER_BOX'
                                  'NO_FIL'.
    PERFORM BDC_FIELD       USING 'SORT_BOX'
                                   'ST_STA'.

*定位数据
    PERFORM BDC_DYNPRO      USING 'SAPLCO05' '0110'.

    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RCOSU-POSNR'.

    PERFORM BDC_FIELD       USING  'BDC_OKCODE'
                                  '=MORE'.

    PERFORM BDC_FIELD       USING 'RCOSU-POSNR'
                                   L_TABIX_1.

  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPLCOMK' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RESBD-POSTP(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
  IF BDCDATA[] IS NOT INITIAL.
    CALL TRANSACTION 'CO07' USING BDCDATA
                             MODE 'N'
                             UPDATE 'S'
                             MESSAGES INTO MESSTAB.

    READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC <> 0.
      MESSAGE S005(Z001).
      WRITE:/ P_AUFNR.
    ENDIF.

    LOOP AT MESSTAB .
      MESSAGE ID MESSTAB-MSGID
              TYPE MESSTAB-MSGTYP
              NUMBER MESSTAB-MSGNR
              INTO G_MSGTXT
              WITH MESSTAB-MSGV1
                   MESSTAB-MSGV2
                   MESSTAB-MSGV3
                   MESSTAB-MSGV4.
      WRITE: / G_MSGTXT.
    ENDLOOP.

    REFRESH BDCDATA[].
    REFRESH MESSTAB[].
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

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
           ID 'WERKS' FIELD  P_WERKS.
  IF SY-SUBRC <> 0.
    MESSAGE E603(FCO) WITH   P_WERKS.
  ENDIF.
ENDFORM.
