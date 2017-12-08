REPORT ZSD020.

TABLES:LIKP,LIPS,EKKN.

TYPE-POOLS:SLIS.  "类型池

*---ALV变量定义---
DATA:WA_FIELDCAT TYPE LVC_S_FCAT, "ALV显示字段结构
     GT_FIELDCAT TYPE LVC_T_FCAT, "ALV显示字段内表
     WA_LAYOUT   TYPE LVC_S_LAYO. "ALV显示风格结构

*------------------------------------
*-------定义结构、内表和工作区---------
*------------------------------------

*-------ALV输出结构--------
DATA:BEGIN OF TY_DATA,
       BOX TYPE C,
     VGBEL TYPE LIPS-VGBEL, "项目编号
     ZTEXT(100) TYPE C, "项目名称
     VBELN TYPE LIKP-VBELN, "交货单编号
     WADAT TYPE LIKP-WADAT, "交货日期
     POSNR TYPE LIPS-POSNR, "行项目编号
     MATNR TYPE LIPS-MATNR, "物料编码
     MAKTX TYPE MAKT-MAKTX, "货物名称
     MAKTX1 TYPE MAKT-MAKTX,  "货物英文名称
     LFIMG TYPE LIPS-LFIMG, "数量
     ZSUM(100) TYPE N,
     MEINS TYPE LIPS-MEINS, "单位
     BSTKD TYPE VBKD-BSTKD, "客户订单号
     KDMAT TYPE VBAP-KDMAT, "客户货品编码
      ZZJS TYPE ZZS_ZSD020-ZZJS, "总件数
      ZSHR TYPE ZZS_ZSD020-ZSHR,  "收货人
       ZDH TYPE ZZS_ZSD020-ZDH, "电话
       ZDZ TYPE ZZS_ZSD020-ZDZ, "地址
       ZCS TYPE ZZS_ZSD020-ZCS, "城市
       ZGJ TYPE ZZS_ZSD020-ZGJ, "国家
     ZFHFS TYPE ZZS_ZSD020-ZFHFS, "发货方式
       ZJS TYPE ZZS_ZSD020-ZJS, "货品件数
       ZXH TYPE ZZS_ZSD020-ZXH, "货品箱号
       ZBZ TYPE ZZS_ZSD020-ZBZ, "备注
     END OF TY_DATA.
*----ALV内表和工作区----
DATA:GT_DATA LIKE TABLE OF TY_DATA,
     GS_DATA LIKE LINE OF GT_DATA.
DATA:GT_DATA2 LIKE TABLE OF TY_DATA,
     GS_DATA2 LIKE LINE OF GT_DATA2.

DATA:ZVGBEL TYPE LIPS-VGBEL,
     ZBSTKD TYPE VBKD-BSTKD.

DATA:ZS_ZJB TYPE ZZS_ZSD020.

DATA:SUM(100) TYPE N.
     SUM = 0.

*----LIPS和LIKP作为主表的结构----
DATA:BEGIN OF TY_LIPS,
     VGBEL TYPE LIPS-VGBEL, "项目编号
     VBELN TYPE LIKP-VBELN, "交货单编号
     WADAT TYPE LIKP-WADAT, "交货日期
     POSNR TYPE LIPS-POSNR, "行项目编号
     MATNR TYPE LIPS-MATNR, "物料编码
     LFIMG TYPE LIPS-LFIMG, "数量
     MEINS TYPE LIPS-MEINS, "单位

     UECHA TYPE LIPS-UECHA,
     VGPOS TYPE LIPS-VGPOS,
     KDMAT TYPE LIPS-KDMAT,
     END OF TY_LIPS.
*----LIPS和LIKP作为主表的内表和工作区----
DATA:GT_LIPS LIKE TABLE OF TY_LIPS,
     GS_LIPS LIKE LINE OF GT_LIPS.

DATA:ZEBELP(6) TYPE N.

*----MAKT表结构定义----
DATA:BEGIN OF TY_MAKT,
     MAKTX TYPE MAKT-MAKTX,
     MAKTG TYPE MAKT-MAKTG,
     MATNR TYPE MAKT-MATNR,
     SPRAS TYPE MAKT-SPRAS,
     END OF TY_MAKT.
*----MAKT表内表和工作区定义----
DATA:GT_MAKT LIKE TABLE OF TY_MAKT,
     GS_MAKT LIKE LINE OF GT_MAKT.

*----VBAK表结构定义----
DATA:BEGIN OF TY_VBAK,
     VBELN TYPE VBAK-VBELN,
     END OF TY_VBAK.
*----VBAK表内表和工作区----
DATA:GT_VBAK LIKE TABLE OF TY_VBAK,
     GS_VBAK LIKE LINE OF GT_VBAK.

*----EKKN表结构----
DATA:BEGIN OF TY_EKKN,
     VBELN TYPE EKKN-VBELN,
     EBELN TYPE EKKN-VBELN,
     EBELP TYPE EKKN-EBELP,

     VBELP TYPE EKKN-VBELP,
     END OF TY_EKKN.
*----EKKN表内表和工作区----
DATA:GT_EKKN LIKE TABLE OF TY_EKKN,
     GS_EKKN LIKE LINE OF GT_EKKN.

*----VBKD表结构----
DATA:BEGIN OF TY_VBKD,
     BSTKD TYPE VBKD-BSTKD,
     VBELN TYPE VBKD-VBELN,
     POSNR TYPE VBKD-POSNR,
     END OF TY_VBKD.
*----VBKD表内表和工作区---
DATA:GT_VBKD LIKE TABLE OF TY_VBKD,
     GS_VBKD LIKE LINE OF GT_VBKD.

*----VBAP表结构----
DATA:BEGIN OF TY_VBAP,
     KDMAT TYPE VBAP-KDMAT,
     VBELN TYPE VBAP-VBELN,
     POSNR TYPE VBAP-POSNR,
     END OF TY_VBAP.
*----VBAP表内表和工作区定义----
DATA:GT_VBAP LIKE TABLE OF TY_VBAP,
     GS_VBAP LIKE LINE OF GT_VBAP.

*-----自建表-----
DATA:GT_ZJB LIKE TABLE OF ZZS_ZSD020,
     GS_ZJB LIKE LINE OF GT_ZJB.

*------自建结构定义------
DATA:WA_05 TYPE ZZS_SD020,
     GT_05A LIKE TABLE OF ZZS_SD020_A,
     WA_05A LIKE LINE OF GT_05A.
*-----取项目名称-----
DATA:GV_NAME TYPE THEAD-TDNAME,
     GT_LINES LIKE TABLE OF TLINE,
     WA_LINES LIKE LINE OF GT_LINES.

*----定义宏----
DEFINE SET_OUTTAB.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-FIELDNAME = &1. "字段名
  WA_FIELDCAT-REPTEXT   = &2. "列标题
  WA_FIELDCAT-EDIT       = &3. "可编辑
  WA_FIELDCAT-EMPHASIZE       = &4. "列颜色
  WA_FIELDCAT-OUTPUTLEN       = &5. "列颜色
*  WA_FIELDCAT-REF_TABLE = &3. "参考表
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR WA_FIELDCAT.
END-OF-DEFINITION.

DATA:S_COUNT TYPE I.
DATA:S_PAGE TYPE I VALUE 10.

DATA: GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.


*定义打印类型
DATA: L_FM_NAME TYPE RS38L_FNAM,
       OUTPUT             TYPE SSFCOMPOP,
       CONTROL_PARAMETERS TYPE SSFCTRLOP,
       LW_SSFCRESCL       TYPE SSFCRESCL,
       OPTION             TYPE SSFCRESCL.

*----------定义选择屏幕-----------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:S_VSTEL FOR LIKP-VSTEL,
                 S_VKORG FOR LIKP-VKORG,
                 S_VBELN FOR LIKP-VBELN OBLIGATORY,
                 S_WADAT FOR LIKP-WADAT,
                 S_LFART FOR LIKP-LFART,
                 S_VGBEL FOR LIPS-VGBEL.
SELECTION-SCREEN END OF BLOCK BLK1.

*--------初始化选择屏幕--------
INITIALIZATION.

START-OF-SELECTION.

PERFORM GET_DATA. "取数及赋值操作
 IF GT_LIPS IS INITIAL.
   MESSAGE '无数据，请重新输入！' TYPE 'S' DISPLAY LIKE 'E'.
   EXIT.
 ENDIF.
END-OF-SELECTION.

PERFORM FRM_WRITE.  "格式化输出
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  "----取出主表的数据
  SELECT LIPS~VGBEL
         LIKP~VBELN
         LIKP~WADAT
         LIPS~POSNR
         LIPS~MATNR
         LIPS~LFIMG
         LIPS~MEINS

         LIPS~UECHA
         LIPS~VGPOS
         LIPS~KDMAT
    FROM LIPS
    INNER JOIN LIKP
    ON LIPS~VBELN = LIKP~VBELN
    INTO CORRESPONDING FIELDS OF TABLE GT_LIPS
    WHERE LIKP~VSTEL IN S_VSTEL
      AND LIKP~VKORG IN S_VKORG
      AND LIKP~VBELN IN S_VBELN
      AND LIKP~WADAT IN S_WADAT
      AND LIKP~LFART IN S_LFART
      AND LIPS~VGBEL IN S_VGBEL.

     IF GT_LIPS IS NOT INITIAL.

     "----取MAKT表的数据
     SELECT MAKTX
            MATNR
            SPRAS
       FROM MAKT
       INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
       FOR ALL ENTRIES IN GT_LIPS
       WHERE MATNR = GT_LIPS-MATNR.
       SORT GT_MAKT BY MATNR.


     "----取出VBAK表的数据
     SELECT VBELN
       FROM VBAK
       INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
       FOR ALL ENTRIES IN GT_LIPS
       WHERE VBELN = GT_LIPS-VGBEL.
       SORT GT_VBAK BY VBELN.

     "----取出EKKN表的数据
     SELECT VBELN
            EBELN
            VBELP
       FROM EKKN
       INTO CORRESPONDING FIELDS OF TABLE GT_EKKN.
*       FOR ALL ENTRIES IN GT_LIPS
*       WHERE EBELN = GT_LIPS-VGBEL
*         AND EBELP = GT_LIPS-VGPOS.
       SORT GT_EKKN BY EBELN EBELP.
       LOOP AT GT_LIPS INTO GS_LIPS.
         READ TABLE GT_EKKN INTO GS_EKKN WITH KEY EBELN = GS_LIPS-VGBEL
                                                  EBELP = GS_LIPS-VGPOS.
         IF SY-SUBRC NE 0.
           DELETE GT_EKKN   FROM  GS_EKKN WHERE EBELN = GS_LIPS-VGBEL AND
                                                EBELP = GS_LIPS-VGPOS.
           ENDIF.
          ENDLOOP.
      IF GT_EKKN IS NOT INITIAL.
          "---取出VBAP表的数据
          SELECT  VBAP~KDMAT

            FROM VBAP
            INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
            FOR ALL ENTRIES IN GT_EKKN
            WHERE VBELN = GT_EKKN-VBELN
              AND POSNR = GT_EKKN-VBELP.
            SORT GT_VBAP BY VBELN POSNR.
          ENDIF.


     "----取出VBKD表的数据
     SELECT BSTKD
            VBELN
            POSNR
       FROM VBKD
       INTO CORRESPONDING FIELDS OF TABLE GT_VBKD
       FOR ALL ENTRIES IN GT_LIPS
       WHERE POSNR = GT_LIPS-POSNR.
       SORT GT_VBKD BY VBELN POSNR.

     SELECT *
       FROM ZZS_ZSD020
       INTO CORRESPONDING FIELDS OF TABLE GT_ZJB
       FOR ALL ENTRIES IN GT_LIPS
       WHERE MATNR = GT_LIPS-MATNR.
       SORT GT_ZJB BY MATNR.

     ENDIF.


LOOP AT GT_LIPS INTO GS_LIPS.

  GS_DATA-VBELN = GS_LIPS-VBELN.
  GS_DATA-WADAT = GS_LIPS-WADAT.
  GS_DATA-POSNR = GS_LIPS-POSNR.
  GS_DATA-MATNR = GS_LIPS-MATNR.

  GV_NAME = GS_LIPS-VGBEL.
  CALL FUNCTION 'READ_TEXT'
       EXPORTING
*         CLIENT                        = SY-MANDT
         ID                            = 'Z001'
         LANGUAGE                      = '1'
         NAME                          = GV_NAME
         OBJECT                        = 'VBBK'
       TABLES
         LINES                         = GT_LINES
       EXCEPTIONS
             ID                      = 1
             LANGUAGE                = 2
             NAME                    = 3
             NOT_FOUND               = 4
             OBJECT                  = 5
             REFERENCE_CHECK         = 6
             WRONG_ACCESS_TO_ARCHIVE = 7
             OTHERS                  = 8.
    IF GT_LINES IS NOT INITIAL.
    LOOP AT GT_LINES INTO WA_LINES.
      CONCATENATE GS_DATA-ZTEXT WA_LINES-TDLINE  INTO GS_DATA-ZTEXT.
      ENDLOOP.
    ENDIF.


*    READ TABLE GT_LINES INTO WA_LINES INDEX 1.
*    IF SY-SUBRC = 0.
*      GS_DATA-ZTEXT = WA_LINES-TDLINE.
*      CLEAR WA_LINES.
*      ENDIF.


  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            INPUT         = GS_DATA-VBELN
         IMPORTING
            OUTPUT        = GS_DATA-VBELN.

  LOOP AT GT_MAKT INTO GS_MAKT WHERE MATNR = GS_LIPS-MATNR .
    IF GS_MAKT-SPRAS = '1'.
      GS_DATA-MAKTX = GS_MAKT-MAKTX.
    ENDIF.
    IF GS_MAKT-SPRAS = 'E'.
      GS_DATA-MAKTX1 = GS_MAKT-MAKTX.
    ENDIF.
  ENDLOOP.

  READ TABLE GT_VBAK INTO GS_VBAK WITH KEY VBELN = GS_LIPS-VGBEL BINARY SEARCH.
  IF SY-SUBRC = 0.
    IF GS_VBAK-VBELN IS NOT INITIAL.
      GS_DATA-VGBEL = GS_LIPS-VGBEL.
    ELSE.
      READ TABLE GT_EKKN INTO GS_EKKN WITH KEY EBELN = GS_LIPS-VGBEL
                                           EBELP = GS_LIPS-VGPOS BINARY SEARCH.
      IF SY-SUBRC = 0.
           GS_DATA-VGBEL = GS_EKKN-VBELN.
        ENDIF.
      ENDIF.
    ENDIF.



*  IF GS_LIPS-UECHA IS INITIAL AND GS_LIPS-LFIMG IS INITIAL.
    GS_DATA-LFIMG = GS_DATA-LFIMG + GS_LIPS-LFIMG.
*  ENDIF.

  GS_DATA-MEINS = GS_LIPS-MEINS.

  READ TABLE GT_VBKD INTO GS_VBKD WITH KEY POSNR = GS_LIPS-POSNR BINARY SEARCH.
  IF SY-SUBRC = 0.
    GS_DATA-BSTKD = GS_VBKD-BSTKD.
    ENDIF.

  IF GS_LIPS-KDMAT = ''.
  READ TABLE GT_VBAP INTO GS_VBAP WITH KEY VBELN = GS_EKKN-VBELN
                                           POSNR = GS_EKKN-VBELP BINARY SEARCH.
  IF SY-SUBRC = 0.
    GS_DATA-KDMAT = GS_VBAP-KDMAT.
    ENDIF.
  ELSE.
    GS_DATA-KDMAT = GS_LIPS-KDMAT.
  ENDIF.

  READ TABLE GT_ZJB INTO GS_ZJB WITH KEY MATNR = GS_LIPS-MATNR BINARY SEARCH.
  IF SY-SUBRC = 0.
    GS_DATA-ZJS = GS_ZJB-ZJS.
    GS_DATA-ZSHR = GS_ZJB-ZSHR.
    GS_DATA-ZDZ = GS_ZJB-ZDZ.
    GS_DATA-ZCS = GS_ZJB-ZCS.
    GS_DATA-ZGJ = GS_ZJB-ZGJ.
    GS_DATA-ZFHFS = GS_ZJB-ZFHFS.
    GS_DATA-ZXH = GS_ZJB-ZXH.
    GS_DATA-ZBZ = GS_ZJB-ZBZ.
    GS_DATA-ZZJS = GS_ZJB-ZZJS.
    ENDIF.

    APPEND GS_DATA TO GT_DATA.
    CLEAR GS_DATA.

ENDLOOP.

SORT GT_DATA BY VBELN POSNR.
DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING ALL FIELDS.
*DELETE GT_DATA WHERE VGBEL = ''.
ENDFORM.
*&---------------------------------------------------------------------
*&      Form  FRM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_write .
  PERFORM LAYOUT_BUILD.   "输出样式
  PERFORM FIELDCAT_BUILD. "输出属性
  PERFORM ALV_PRINT.      "ALV展示
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout_build .
  WA_LAYOUT-ZEBRA = 'X'.     "输出网格为斑马线
  WA_LAYOUT-CWIDTH_OPT = 'X'."自适应宽度
  WA_LAYOUT-BOX_FNAME = 'BOX'.       "选择框
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_build .
  SET_OUTTAB 'VGBEL' '项目编号' '' '' ''.
  SET_OUTTAB 'ZTEXT' '项目名称' '' '' ''.
  SET_OUTTAB 'VBELN' '交货单号' '' '' ''.
  SET_OUTTAB 'WADAT' '交货日期' '' '' ''.
  SET_OUTTAB 'POSNR' '行项目编号' '' '' ''.
  SET_OUTTAB 'MATNR' '物料编码' '' '' ''.
  SET_OUTTAB 'MAKTX' '货物名称' '' '' ''.
  SET_OUTTAB 'MAKTX1' '货物英文名称' '' '' ''.
  SET_OUTTAB 'LFIMG' '数量' '' '' ''.
  SET_OUTTAB 'MEINS' '单位' '' '' ''.
  SET_OUTTAB 'BSTKD' '客户订单号' 'X' '' '80'.
  SET_OUTTAB 'KDMAT' '客户货品编码' 'X' '' '80'.
  SET_OUTTAB 'ZZJS' '总件数' 'X' '' '30'.
  SET_OUTTAB 'ZFHFS' '发货方式' 'X' '' '80'.
  SET_OUTTAB 'ZSHR' '收货人名称' 'X' '' '80'.
  SET_OUTTAB 'ZDZ' '地址' 'X' '' '100'.
  SET_OUTTAB 'ZCS' '城市' 'X' '' '80'.
  SET_OUTTAB 'ZGJ' '国家' 'X' '' '80'.
  SET_OUTTAB 'ZJS' '货品件数' 'X' 'C710' '30'.
  SET_OUTTAB 'ZXH' '货品箱号' 'X' 'C710' '30'.
  SET_OUTTAB 'ZBZ' '备注' 'X' 'C710' '200'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_print .
  DATA L_GRID_SETTINGS TYPE LVC_S_GLAY.

  L_GRID_SETTINGS-EDT_CLL_CB ='X'.
  PERFORM FRM_CREATE_EVENTS.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
  EXPORTING
    I_CALLBACK_PROGRAM                = SY-REPID
    I_CALLBACK_PF_STATUS_SET          = 'SET_PF_STATUS'
    I_CALLBACK_USER_COMMAND          = 'USER_COMMAND ' "自定义按钮响应事件
    IS_LAYOUT_LVC                     = WA_LAYOUT
    IT_FIELDCAT_LVC                   = GT_FIELDCAT
    I_SAVE                            = 'A'
    IT_EVENTS                         = GT_EVENTS
    I_GRID_SETTINGS                    = L_GRID_SETTINGS
   TABLES
     T_OUTTAB                          = GT_DATA
  EXCEPTIONS
    PROGRAM_ERROR                     = 1
    OTHERS                            = 2
          .
 IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.
ENDFORM.
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STATUS'.
ENDFORM.
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
   DATA: GV_GRID TYPE REF TO CL_GUI_ALV_GRID.
   DATA: L_NUM TYPE I.
   "刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = GV_GRID.
  CALL METHOD GV_GRID->CHECK_CHANGED_DATA.



  CASE R_UCOMM.
    WHEN 'ZL'.

      PERFORM DAYIN.

   ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DAYIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dayin .
*指定SMARTFORM
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
     EXPORTING
       FORMNAME                 = 'ZSF_ZSD020'

*     VARIANT                  = ' '
*     DIRECT_CALL              = ' '

   IMPORTING
      FM_NAME                  = L_FM_NAME
    EXCEPTIONS
      NO_FORM                  = 1
      NO_FUNCTION_MODULE       = 2
      OTHERS                   = 3
             .
   IF SY-SUBRC <> 0.


* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.


  ENDIF.
 "打印设置
  CONTROL_PARAMETERS-NO_OPEN   = 'X'.
   CONTROL_PARAMETERS-NO_CLOSE  = 'X'.

*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.

  OUTPUT-TDDEST = 'LP01'.

*  OUTPUT-TDPRINTER = 'MICROSOFT OFFICE DOCUMENT IMAGE WRITER'.

  OUTPUT-RQPOSNAME = ''.
   OUTPUT-TDDATASET = ''.
   OUTPUT-TDSUFFIX1 = ''.
   OUTPUT-TDSUFFIX2 = ''.
   OUTPUT-TDIMMED   = 'X'.
   OUTPUT-TDDELETE  = 'X'.

*  MODIFY ZZS_ZSD020 FROM  GS_DATA.
*  MESSAGE '成功保存到自建表！' TYPE 'S'.


*打印控制
  CALL FUNCTION 'SSF_OPEN'
    EXPORTING


*     ARCHIVE_PARAMETERS       =
*     USER_SETTINGS            = 'X'
*     MAIL_SENDER              =
*     MAIL_RECIPIENT           =
*     MAIL_APPL_OBJ            =
     OUTPUT_OPTIONS           = OUTPUT
     CONTROL_PARAMETERS       = CONTROL_PARAMETERS


*   IMPORTING
*     JOB_OUTPUT_OPTIONS       =
   EXCEPTIONS
      FORMATTING_ERROR         = 1
      INTERNAL_ERROR           = 2
      SEND_ERROR               = 3
      USER_CANCELED            = 4
      OTHERS                   = 5
             .
   IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

S_COUNT = 0.
LOOP AT GT_DATA INTO GS_DATA WHERE BOX = 'X'.
   CLEAR ZS_ZJB.
*  ZS_ZJB-MATNR = GS_DATA-MATNR.
*  ZS_ZJB-ZSHR = GS_DATA-ZSHR.
*  ZS_ZJB-ZDH = GS_DATA-ZDH.
*  ZS_ZJB-ZDZ = GS_DATA-ZDZ.
*  ZS_ZJB-ZCS = GS_DATA-ZCS.
*  ZS_ZJB-ZGJ = GS_DATA-ZGJ.
*  ZS_ZJB-ZFHFS = GS_DATA-ZFHFS.
*  ZS_ZJB-ZJS = GS_DATA-ZJS.
*  ZS_ZJB-ZXH = GS_DATA-ZXH.
*  ZS_ZJB-ZBZ = GS_DATA-ZBZ.
  MOVE-CORRESPONDING GS_DATA TO ZS_ZJB.
  MODIFY ZZS_ZSD020 FROM  ZS_ZJB.
  MESSAGE '成功保存到自建表！' TYPE 'S'.
  GT_DATA2 = GT_DATA.
  "--------表头和表尾----------
  ZVGBEL = GS_DATA-VGBEL.
  ZBSTKD = GS_DATA-BSTKD.
  AT NEW VGBEL.
    WA_05-VGBEL = ZVGBEL.
    WA_05-BSTKD = ZBSTKD.
    WA_05-ZFHFS = ZS_ZJB-ZFHFS.
    WA_05-ZSHR = ZS_ZJB-ZSHR.
    WA_05-ZDZ = ZS_ZJB-ZDZ.
    WA_05-ZCS = ZS_ZJB-ZCS.
    WA_05-ZGJ = ZS_ZJB-ZGJ.
    WA_05-ZTEXT = ZS_ZJB-ZZJS.


  ENDAT.
  AT END OF VGBEL.

    DELETE GT_DATA2 WHERE BOX NE 'X'.
  "-------表体------
  LOOP AT GT_DATA2 INTO GS_DATA2 WHERE VGBEL = GS_DATA-VGBEL AND BOX = 'X'.

    S_COUNT = S_COUNT + 1.
    WA_05A-MATNR = GS_DATA2-MATNR.
    WA_05A-KDMAT = GS_DATA2-KDMAT.
    WA_05A-MEINS = GS_DATA2-MEINS.
    WA_05A-LFIMG = GS_DATA2-LFIMG.
    WA_05A-MAKTX = GS_DATA2-MAKTX.
    WA_05A-MAKTX1 = GS_DATA2-MAKTX1.
    WA_05A-ZJS = GS_DATA2-ZJS.
    WA_05A-ZXH = GS_DATA2-ZXH.
    WA_05A-ZBZ = GS_DATA2-ZBZ.

    APPEND WA_05A TO GT_05A.
     CLEAR WA_05A.
  ENDLOOP.


*----------------判断空行--------------------------------------------
  S_COUNT = S_COUNT MOD S_PAGE.
    IF S_COUNT NE 0.
      S_COUNT = S_PAGE - S_COUNT.
      CLEAR WA_05A.
      DO S_COUNT TIMES.
        APPEND WA_05A TO GT_05A.
      ENDDO.
    ENDIF.

  "调用SMARTFORMS的FUNCTION MODULE打印
    CALL FUNCTION L_FM_NAME
     EXPORTING
       CONTROL_PARAMETERS = CONTROL_PARAMETERS
       OUTPUT_OPTIONS     = OUTPUT
       WA_TAB_1 = WA_05
       I_NUM              = S_PAGE
     TABLES
       GT_ZL = GT_05A
     EXCEPTIONS
       FORMATTING_ERROR   = 1
       INTERNAL_ERROR     = 2
       SEND_ERROR         = 3
       USER_CANCELED      = 4.

     IF SY-SUBRC = 0.

     ENDIF.
       CLEAR GT_05A.
       CLEAR S_COUNT.
       ENDAT.

ENDLOOP.

    "关闭打印机
     CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      JOB_OUTPUT_INFO  = LW_SSFCRESCL
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_events .
  GW_EVENTS-NAME =  SLIS_EV_DATA_CHANGED.
  GW_EVENTS-FORM = 'FRM_DATA_CHANGED'.
  APPEND GW_EVENTS TO GT_EVENTS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_data_changed USING ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA: L_GRID TYPE REF TO CL_GUI_ALV_GRID,
        STBL   TYPE LVC_S_STBL.

  DATA L_OUT LIKE LINE OF GT_DATA.
  DATA LT_OUT LIKE TABLE OF L_OUT WITH HEADER LINE.

  FIELD-SYMBOLS: <L_CHANG> TYPE ANY,
                 <L_DATA> LIKE GS_DATA.
  ASSIGN ER_DATA_CHANGED->MP_MOD_ROWS->* TO <L_CHANG>.
  LT_OUT[] = <L_CHANG>.

  FIELD-SYMBOLS <L_OUT> LIKE LINE OF GT_DATA.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.

*  SORT GT_DATA BY POSNR.
  READ TABLE GT_DATA  INTO GS_DATA INDEX 1 .
  READ TABLE LT_OUT INDEX 1 .

  IF  GS_DATA-POSNR = LT_OUT-POSNR .

  LOOP AT GT_DATA ASSIGNING <L_DATA> .
    AT NEW VBELN.
*      L_OUT = <L_DATA>.
      CLEAR LT_OUT.
      READ TABLE LT_OUT WITH KEY VBELN = <L_DATA>-VBELN POSNR = <L_DATA>-POSNR.
*      IF SY-SUBRC = 0.
*        L_OUT-ZSHR  = LT_OUT-ZSHR .
*        L_OUT-ZDZ   = LT_OUT-ZDZ  .
*        L_OUT-ZCS   = LT_OUT-ZCS.
*        L_OUT-ZGJ   = LT_OUT-ZGJ .
*        L_OUT-ZFHFS = LT_OUT-ZFHFS .
*        L_OUT-ZJS   = LT_OUT-ZJS .
*        L_OUT-ZXH   = LT_OUT-ZXH .
*        L_OUT-ZBZ   = LT_OUT-ZBZ .
*        L_OUT-ZZJS  = LT_OUT-ZZJS .
*      ENDIF.
    ENDAT.
    <L_DATA>-ZSHR   = LT_OUT-ZSHR .
    <L_DATA>-ZDZ    = LT_OUT-ZDZ  .
    <L_DATA>-ZCS  = LT_OUT-ZCS.
    <L_DATA>-ZGJ   = LT_OUT-ZGJ .
    <L_DATA>-ZFHFS   = LT_OUT-ZFHFS .
    <L_DATA>-ZJS    = LT_OUT-ZJS  .
    <L_DATA>-ZXH    = LT_OUT-ZXH  .
    <L_DATA>-ZBZ    = LT_OUT-ZBZ  .
    <L_DATA>-ZZJS    = LT_OUT-ZZJS  .
  ENDLOOP.
  ENDIF.


  STBL-COL = 'X'.
  STBL-ROW = 'X'.
  CALL METHOD L_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL.

ENDFORM.
