*&---------------------------------------------------------------------*
*&程序名称/Program Name         :     ZMM042
*&程序描述/Program Des.         :     调拨单打印
*&开发日期/Date of App          :     2017.6.21
*&作者/Author                   :     韩金栋
*&---------------------------------------------------------------------*
REPORT ZMM042.
TYPE-POOLS:SLIS,ICON.
TABLES:MSEG,MKPF.

DATA:G_PAGE TYPE I VALUE 36.
*&---定义ALV显示的字段列及其描述等属性
DATA: GT_FCAT TYPE LVC_T_FCAT,
      GS_FCAT TYPE LVC_S_FCAT.
DATA: GS_LAYO TYPE LVC_S_LAYO.

*定义打印属性
DATA:   G_FM_NAME  TYPE  RS38L_FNAM,
      G_OUTPUT             TYPE SSFCOMPOP,
      G_CONTROL_PARAMETERS TYPE SSFCTRLOP,
      G_LW_SSFCRESCL       TYPE SSFCRESCL,
      G_OPTION             TYPE SSFCRESCL.
*------------ALV结构定义----------
TYPES:
    BEGIN OF ITAB_ALV,
      BOX TYPE C,
      MBLNR TYPE MSEG-MBLNR,
      XBLNR_MKPF TYPE MSEG-XBLNR_MKPF,
      BLDAT TYPE MKPF-BLDAT,
      BUDAT TYPE MKPF-BUDAT,
      BKTXT TYPE MKPF-BKTXT,
      ZEILE TYPE MSEG-ZEILE,
      BWART TYPE MSEG-BWART,
      SOBKZ TYPE MSEG-SOBKZ,
      MATNR TYPE MSEG-MATNR,
      MAKTX TYPE MAKT-MAKTX,
      MENGE TYPE MSEG-MENGE,
      MEINS TYPE MSEG-MEINS,
      LGORT TYPE MSEG-LGORT,
      MAT_KDAUF TYPE MSEG-MAT_KDAUF,
      UMLGO TYPE MSEG-UMLGO,
      KDAUF TYPE MSEG-KDAUF,
      XAUTO TYPE MSEG-XAUTO,
      SGTXT TYPE MSEG-SGTXT,
      END OF ITAB_ALV.
 DATA : GT_ALV TYPE TABLE OF ITAB_ALV,
        GS_ALV LIKE LINE OF GT_ALV.
 DATA : GT_ALV_B TYPE TABLE OF ITAB_ALV,
        GS_ALV_B LIKE LINE OF GT_ALV_B.
 DATA : GT_ALV_C TYPE TABLE OF ITAB_ALV,
        GS_ALV_C LIKE LINE OF GT_ALV_C.


DATA:
      Z_BTEXT TYPE T156HT-BTEXT,
      Z_SOTXT TYPE T148T-SOTXT,
      Z_LGOBE TYPE T001L-LGOBE,
      YDLXMS TYPE C LENGTH 200.    "（移动类型+特殊库存标识）对应的移动类型描述
*------------主表结构-------------
 TYPES:
    BEGIN OF ITAB_MSEG,
      MBLNR TYPE MSEG-MBLNR,
      XBLNR_MKPF TYPE MSEG-XBLNR_MKPF,
      ZEILE TYPE MSEG-ZEILE,
      BWART TYPE MSEG-BWART,
      SOBKZ TYPE MSEG-SOBKZ,
      MATNR TYPE MSEG-MATNR,
      MENGE TYPE MSEG-MENGE,
      MEINS TYPE MSEG-MEINS,
      LGORT TYPE MSEG-LGORT,
      MAT_KDAUF TYPE MSEG-MAT_KDAUF,
      UMLGO TYPE MSEG-UMLGO,
      KDAUF TYPE MSEG-KDAUF,
      XAUTO TYPE MSEG-XAUTO,
      SGTXT TYPE MSEG-SGTXT,
      BLDAT TYPE MKPF-BLDAT,
      BUDAT TYPE MKPF-BUDAT,
      BKTXT TYPE MKPF-BKTXT,


    END OF ITAB_MSEG.
 DATA:
       GT_MSEG TYPE TABLE OF ITAB_MSEG.
*------------MAKT结构-----------
 TYPES:
    BEGIN OF ITAB_MAKT,
      MATNR TYPE MAKT-MATNR,
      MAKTX TYPE MAKT-MAKTX,
    END OF ITAB_MAKT.
 DATA:
       GT_MAKT TYPE TABLE OF ITAB_MAKT,
       GS_MAKT LIKE LINE OF GT_MAKT.
*---------T156HT结构------------
 TYPES:
    BEGIN OF ITAB_T156HT,
      BWART TYPE T156HT-BWART,
      BTEXT TYPE T156HT-BTEXT,
      SPRAS TYPE T156HT-SPRAS,
    END OF ITAB_T156HT.
 DATA:
       GT_T156HT TYPE TABLE OF ITAB_T156HT .
*---------T148T结构-----------
 TYPES:
    BEGIN OF ITAB_T148T,
      SOBKZ TYPE T148T-SOBKZ,
      SOTXT TYPE T148T-SOTXT,
      SPRAS TYPE T148T-SPRAS,
    END OF ITAB_T148T.
  DATA:
        GT_T148T TYPE TABLE OF ITAB_T148T .
*----------T001L结构-----------
  TYPES:
      BEGIN OF ITAB_T001L,
        LGORT TYPE T001L-LGORT,
        LGOBE TYPE T001L-LGOBE,
      END OF ITAB_T001L.
  DATA:
        GT_T001L TYPE TABLE OF ITAB_T001L,
        GS_T001L LIKE LINE OF GT_T001L.
 DATA:
        GT_T001L_B TYPE TABLE OF ITAB_T001L,
        GS_T001L_B LIKE LINE OF GT_T001L_B.


*-------------自定义结构定义------------
DATA:
      GT_HEAD TYPE TABLE OF ZMM042_HEAD,
      GS_HEAD LIKE LINE OF GT_HEAD.
DATA:
      GT_ITEM TYPE TABLE OF ZMM042_ITEM,
      GS_ITEM LIKE LINE OF GT_ITEM.
 FIELD-SYMBOLS:
                <GS_ALV> TYPE ITAB_ALV,
                <GS_MSEG> TYPE ITAB_MSEG,
                <GS_MAKT> TYPE ITAB_MAKT,
                <GS_T156HT> TYPE ITAB_T156HT,
                <GS_T148T> TYPE ITAB_T148T,
                <GS_T001L> TYPE ITAB_T001L,
                <GS_T001L_B> TYPE ITAB_T001L,
                <GS_ALV_B> TYPE ITAB_ALV,
                <GS_ALV_C> TYPE ITAB_ALV.
*----------选择屏幕--------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS:S_MBLNR FOR MSEG-MBLNR  OBLIGATORY NO-EXTENSION NO INTERVALS,
                   S_ZEILE FOR MSEG-ZEILE  NO-EXTENSION NO INTERVALS,
                   S_XBLNR FOR MSEG-XBLNR_MKPF .
SELECTION-SCREEN END OF BLOCK BLK1.

INITIALIZATION.
START-OF-SELECTION.
     PERFORM FRM_IMPORT.
     IF GT_MSEG IS INITIAL.
        MESSAGE '查询数据为空' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
     ENDIF.
END-OF-SELECTION.
     PERFORM FRM_EXPORT.
*&---------------------------------------------------------------------*
*&      Form  FRM_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------查询SQL子例程----------------------------------*
FORM FRM_IMPORT .
*-------主表SQL查询---------
  SELECT
       MSEG~MBLNR
       MSEG~XBLNR_MKPF
       MSEG~ZEILE
       MSEG~BWART
       MSEG~SOBKZ
       MSEG~MATNR
       MSEG~MENGE
       MSEG~MEINS
       MSEG~LGORT
       MSEG~MAT_KDAUF
       MSEG~UMLGO
       MSEG~KDAUF
       MSEG~XAUTO
       MSEG~SGTXT
       MKPF~BLDAT
       MKPF~BUDAT
       MKPF~BKTXT
    FROM MSEG  INNER JOIN MKPF ON MSEG~MBLNR = MKPF~MBLNR
     INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
    WHERE
        MSEG~MBLNR IN S_MBLNR AND
        MSEG~ZEILE IN S_ZEILE AND
        MSEG~XBLNR_MKPF IN S_XBLNR AND
        MSEG~XAUTO EQ ''.
  IF GT_MSEG IS NOT INITIAL.
*---------------MAKT查询-----------

    SELECT
      MATNR
      MAKTX
      SPRAS
    FROM MAKT INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
      FOR ALL ENTRIES IN GT_MSEG
    WHERE
      MATNR = GT_MSEG-MATNR AND
      SPRAS = '1'.
*----------------T156HT查询-----------
      SELECT
        BWART
        BTEXT
        SPRAS
      FROM T156HT INTO CORRESPONDING FIELDS OF TABLE GT_T156HT
        FOR ALL ENTRIES IN GT_MSEG
       WHERE
        BWART = GT_MSEG-BWART AND
        SPRAS = '1'.
*-------------T148T查询----------
        SELECT
          SOBKZ
          SOTXT
          SPRAS
         FROM T148T INTO CORRESPONDING FIELDS OF TABLE GT_T148T
          FOR ALL ENTRIES IN GT_MSEG
         WHERE
          SOBKZ = GT_MSEG-SOBKZ AND
          SPRAS = '1'.
*------------T001L查询-----------
          SELECT
            LGORT
            LGOBE
          FROM T001L INTO CORRESPONDING FIELDS OF TABLE GT_T001L
            FOR ALL ENTRIES IN GT_MSEG
          WHERE
            LGORT = GT_MSEG-LGORT.
*------------T001L查询-----------
          SELECT
            LGORT
            LGOBE
          FROM T001L INTO CORRESPONDING FIELDS OF TABLE GT_T001L_B
            FOR ALL ENTRIES IN GT_MSEG
          WHERE
            LGORT = GT_MSEG-UMLGO.
  ENDIF.
*--------------取值---------------
      LOOP AT GT_MSEG ASSIGNING <GS_MSEG>.
        MOVE-CORRESPONDING <GS_MSEG> TO GS_ALV.
        READ TABLE GT_MAKT ASSIGNING <GS_MAKT> WITH KEY MATNR = <GS_MSEG>-MATNR.
          IF SY-SUBRC EQ 0.
              GS_ALV-MAKTX = <GS_MAKT>-MAKTX.
          ENDIF.
        APPEND GS_ALV TO GT_ALV.
        CLEAR GS_ALV.
      ENDLOOP.
      SORT GT_ALV BY MBLNR.
      DELETE ADJACENT DUPLICATES FROM GT_ALV COMPARING ALL FIELDS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_export .
  PERFORM LAYOUT.
  PERFORM INIT_FIELDCAT  .
  PERFORM LIST_VIEW.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout .
  GS_LAYO-BOX_FNAME = 'BOX'.       "选择框
  GS_LAYO-ZEBRA = 'X'. "颜色间隔
  GS_LAYO-CWIDTH_OPT = 'X'.  "  自适应宽度
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_FIELDCAT .
  REFRESH: gt_fcat.

  DEFINE set_fcat.

    GS_FCAT-FIELDNAME     = &1.

    GS_FCAT-COLTEXT       = &2.

    GS_FCAT-NO_ZERO       = &3.

    GS_FCAT-OUTPUTLEN     = &4.

    GS_FCAT-EDIT          = &5.

    APPEND GS_FCAT TO GT_FCAT.

  END-OF-DEFINITION.
  SET_FCAT 'MBLNR'  '凭证编码'      '' '' ''.
  SET_FCAT 'XBLNR_MKPF'  '参考字段'          ' ' '' ''.
  SET_FCAT 'BLDAT'  '凭证日期'      ' ' '' ''.
  SET_FCAT 'BUDAT'  '过账日期'      '' '' ''.
  SET_FCAT 'BKTXT'  '抬头文本'       '' '' ''.
  SET_FCAT 'ZEILE'  '行项目号' '' '' ''.
  SET_FCAT 'BWART' '移动类型'          ''  '' ''.
  SET_FCAT 'SOBKZ' '特殊库存标识'          ''  '' ''.
  SET_FCAT 'MATNR'  '物料编号'          ''  '' ''.
  SET_FCAT 'MAKTX'  '物料名称'    ''  '' ''.
  SET_FCAT 'MENGE'  '调拨数量'     '' '' ''.
  SET_FCAT 'MEINS'  '单位'     '' '' ''.
  SET_FCAT 'LGORT'   '源库存地'       '' '' ''.
  SET_FCAT 'MAT_KDAUF'  '源项目号'    '' '' ''.
  SET_FCAT 'UMLGO'  '调入库存地'     '' '' ''.
  SET_FCAT 'KDAUF'   '调入项目号'         '' '' ''.
  SET_FCAT 'XAUTO'  '自动创建标识'     '' '' ''.
  SET_FCAT 'SGTXT'  '项目文本'     '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_VIEW .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      IS_LAYOUT_LVC            = GS_LAYO
*      I_GRID_SETTINGS          = LV_GRID_SETTINGS
      IT_FIELDCAT_LVC          = GT_FCAT
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_SAVE                   = 'A'
*      IT_EVENTS                = LTD_EVENT    "添加事件。
    TABLES
      T_OUTTAB                 = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS '9000' .
ENDFORM. "F_SET_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
FORM  USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: GV_GRID TYPE REF TO CL_GUI_ALV_GRID.
  "刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GV_GRID.
  CALL METHOD GV_GRID->CHECK_CHANGED_DATA.
  CASE R_UCOMM.
      WHEN 'DAYIN'.
        READ TABLE GT_ALV INTO GS_ALV WITH  KEY BOX = 'X'.
         IF SY-SUBRC EQ 0.
           PERFORM FRM_DAYIN.
         ELSE.
           MESSAGE '请至少选择一条数据' TYPE 'S' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

  ENDCASE.
  RS_SELFIELD-REFRESH = 'X'.  "刷新内表
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_DAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DAYIN .
*------------打印机相关设置----------------
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
     EXPORTING
       FORMNAME                 = 'ZMM042_SF'
*           VARIANT                  = ' '
*           DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME                  = G_FM_NAME
    EXCEPTIONS
      NO_FORM                  = 1
      NO_FUNCTION_MODULE       = 2
      OTHERS                   = 3
             .
  IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



  "打印设置
  G_CONTROL_PARAMETERS-NO_OPEN   = 'X'.
  G_CONTROL_PARAMETERS-NO_CLOSE  = 'X'.
*        CONTROL_PARAMETERS-NO_DIALOG = 'X'.


  G_OUTPUT-TDDEST = 'LP01'.
*        OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.
  G_OUTPUT-RQPOSNAME = ''.
  G_OUTPUT-TDDATASET = ''.
  G_OUTPUT-TDSUFFIX1 = ''.
  G_OUTPUT-TDSUFFIX2 = ''.
  G_OUTPUT-TDIMMED   = 'X'.
  G_OUTPUT-TDDELETE  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      CONTROL_PARAMETERS = G_CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = G_OUTPUT
*          IMPORTING
*           JOB_OUTPUT_OPTIONS = OPTION
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.


DATA:
      ZMBLNR TYPE MSEG-MBLNR,
      ZXBLNR_MKPF TYPE MSEG-XBLNR_MKPF,
      ZBLDAT TYPE MKPF-BLDAT,
      ZMAT_KDAUF TYPE MSEG-MAT_KDAUF,
      ZKDAUF TYPE MSEG-KDAUF,
      ZSGTXT TYPE MSEG-SGTXT,
      ZLGOBE TYPE T001L-LGOBE,
      ZDRKCDMS  TYPE T001L-LGOBE.
CLEAR: ZMBLNR,ZXBLNR_MKPF,ZBLDAT,ZMAT_KDAUF,ZKDAUF,ZSGTXT.
      LOOP AT GT_ALV INTO GS_ALV WHERE BOX = 'X'.
        GT_ALV_B = GT_ALV.
        DELETE GT_ALV_B WHERE BOX NE 'X'.
        GT_ALV_C = GT_ALV_B.


          ZMBLNR = GS_ALV-MBLNR.
          ZXBLNR_MKPF = GS_ALV-XBLNR_MKPF.
          ZBLDAT = GS_ALV-BLDAT.
          ZMAT_KDAUF = GS_ALV-MAT_KDAUF.   "源项目号
          ZKDAUF = GS_ALV-KDAUF.
          ZSGTXT = GS_ALV-SGTXT.
         READ TABLE GT_T001L INTO  GS_T001L WITH  KEY LGORT = GS_ALV-LGORT.
            IF SY-SUBRC EQ 0.

                  ZLGOBE = GS_T001L-LGOBE.   "源库存地描述
            ENDIF.

         READ TABLE GT_T001L_B INTO GS_T001L_B WITH  KEY LGORT = GS_ALV-UMLGO.
            IF SY-SUBRC EQ 0.

                  ZDRKCDMS = GS_T001L_B-LGOBE.   "调入库存地描述
            ENDIF.
         AT NEW MBLNR.
          GS_HEAD-MBLNR = ZMBLNR.
          GS_HEAD-XBLNR_MKPF = ZXBLNR_MKPF.
          GS_HEAD-BLDAT = ZBLDAT.
          GS_HEAD-MAT_KDAUF = ZMAT_KDAUF.   "源项目号
          GS_HEAD-KDAUF = ZKDAUF.
          GS_HEAD-SGTXT = ZSGTXT.
          GS_HEAD-LGOBE = ZLGOBE.
          GS_HEAD-DRKCDMS = ZDRKCDMS.

       ENDAT.

       AT END OF MBLNR.

         LOOP AT GT_ALV_C into GS_ALV_C WHERE MBLNR = GS_ALV-MBLNR.


              GS_ITEM-MATNR = GS_ALV_C-MATNR.
              GS_ITEM-MENGE = GS_ALV_C-MENGE.


              GS_ITEM-MEINS = GS_ALV_C-MEINS.
            READ TABLE GT_MAKT INTO  GS_MAKT WITH KEY MATNR = GS_ALV_C-MATNR.
             IF SY-SUBRC EQ 0.
                GS_ITEM-MAKTX = GS_MAKT-MAKTX.
             ENDIF.






              APPEND GS_ITEM TO GT_ITEM.
              CLEAR GS_ITEM.
          ENDLOOP.
*----------------判断空行---------
*      G_COUNT = G_COUNT MOD G_PAGE.
*      IF G_COUNT NE 0.
*        G_COUNT = G_PAGE - G_COUNT.
*
*      ENDIF.

      "  调用SMARTFORMS的FUNCTION MODULE打印
      CALL FUNCTION G_fm_name
        EXPORTING
         CONTROL_PARAMETERS = G_CONTROL_PARAMETERS
          OUTPUT_OPTIONS     = G_OUTPUT
          GS_HEAD            = GS_HEAD
          I_NUM              = G_PAGE
        TABLES
          GT_ITAB            = GT_ITEM
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4.

      ENDAT.
       CLEAR GT_ITEM.
       CLEAR GT_ALV_B.
      ENDLOOP.
        "  关闭打印机设置
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = G_LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
