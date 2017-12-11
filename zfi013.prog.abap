REPORT ZFI013.
*&---------------------------------------------------------------------*
* 作者: 徐兴旭
* 开发日期:2013-10-15
* 补充修改日期：
* 请求号: DEVK905870
* 申请者:
* 功能/技术文档:
* 描述:  DEV FICO DEVL: 合并报表
*&---------------------------------------------------------------------*
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
TYPES: BEGIN OF TY_DTL,
         REPOT   TYPE  ZCS101-REPOT,
         ZNUMBER TYPE  ZCS101-ZNUMBER,
         LINEN   TYPE  ZCS101-LINEN,
         FORMULA TYPE  ZCS101-FORMULA,
         THISPER TYPE  ZCS005-AMOAW, " 期末数
         PREPER  TYPE  ZCS005-AMOAW, " 期初数
       END OF TY_DTL.


TYPES: BEGIN OF TY_SUM,
         REPOT   TYPE  ZCS102-REPOT,
         ZNUMBER TYPE  ZCS102-ZNUMBER,
         ZTEXT   TYPE  ZCS102-ZTEXT,
         THISPER TYPE  ZCS005-AMOAW, " 期末数
         PREPER  TYPE  ZCS005-AMOAW, " 期初数
       END OF TY_SUM.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

CONSTANTS: G_REPORT TYPE ZCS001-REPOT VALUE '2'.

DATA: G_THIS_YEAR TYPE ECMCA-RYEAR,
      G_THIS_PER  TYPE ECMCA-POPER,
      G_PRE_YEAR  TYPE ECMCA-RYEAR,
      G_PRE_PER   TYPE ECMCA-POPER.

*配置表
DATA: GT_ZCS101 TYPE STANDARD TABLE OF ZCS101, " 报表行项目取值
      GT_ZCS102 TYPE STANDARD TABLE OF ZCS102. " 报表行项目文本描述
FIELD-SYMBOLS: <GS_ZCS101> TYPE ZCS101,
               <GS_ZCS102> TYPE ZCS102.


*选择条件对应的 合并资产负债表工作底稿
DATA: GT_THIS_ZCS005 TYPE STANDARD TABLE OF ZCS005.
FIELD-SYMBOLS: <GS_THIS_ZCS005> TYPE ZCS005.

*上年的 合并资产负债表工作底稿
DATA: GT_PRE_ZCS005 TYPE STANDARD TABLE OF ZCS005.
FIELD-SYMBOLS: <GS_PRE_ZCS005> TYPE ZCS005.

*明细
DATA: GT_DTL TYPE STANDARD TABLE OF TY_DTL.
FIELD-SYMBOLS: <GS_DTL> TYPE TY_DTL.

*汇总
DATA: GT_SUM TYPE STANDARD TABLE OF TY_SUM.
FIELD-SYMBOLS: <GS_SUM> TYPE TY_SUM.

DATA: G_TITLE TYPE ZCS003-TEXT,
      G_DATE  TYPE STRING.
*ALV
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA: GS_FIELDCAT TYPE LINE OF SLIS_T_FIELDCAT_ALV.
DATA: GS_LAYOUT TYPE SLIS_LAYOUT_ALV.


*--------------------------------------------------------------------*
* 选择条件
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS: P_DIMEN TYPE TF181-DIMEN OBLIGATORY,                    " 维
            P_CONGR TYPE TF181-CONGR OBLIGATORY,                    " 合并组
            P_RYEAR TYPE ECMCT-RYEAR OBLIGATORY,                    " 会计年度
            P_PABRP TYPE T569V-PABRP OBLIGATORY.                    " 会计期间

SELECTION-SCREEN END OF BLOCK B1.




*--------------------------------------------------------------------*
* 处理快
*--------------------------------------------------------------------*

INITIALIZATION..

* 设置选择界面的值
  PERFORM G_SET_VALUE.


AT SELECTION-SCREEN.

* 检查选择屏幕输入的值
  PERFORM F_CK_SEL_SCR.

START-OF-SELECTION.

* 获取配置表
  PERFORM F_READ_CON_TAB.

* 设置选择条件对应的本期和上期
  PERFORM F_SET_PERIOD.

* 合并资产负债表工作底稿
  PERFORM F_READ_ZCS005.

* 把行次赋值给明细表
  PERFORM F_ZCS101_TO_DTL.

* 对明细行次填值
  PERFORM F_SET_DEL.

* 把ZCS102赋给汇总表
  PERFORM F_ZCS102_TO_SUM.

* 合计显示
  PERFORM F_SUM_ALL.



END-OF-SELECTION.

  IF GT_SUM[] IS INITIAL.
    MESSAGE S004(ZFICO) DISPLAY LIKE 'E'.
  ELSE.
    PERFORM F_ALV_FIELDCAT.
    PERFORM F_LAYOUT.
    PERFORM F_DISPLAY.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  G_SET_VALUE
*&---------------------------------------------------------------------*
*       设置选择屏幕上的会计年度和会计期间的默认值
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM G_SET_VALUE .

  P_RYEAR = SY-DATUM+0(4).
  P_PABRP = SY-DATUM+4(2).

ENDFORM.                    " G_SET_VALUE



*&---------------------------------------------------------------------*
*&      Form  F_CK_SEL_SCR
*&---------------------------------------------------------------------*
*       检查选择屏幕上输入的值的有效性
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CK_SEL_SCR .

  DATA: L_CONGR TYPE TF181-CONGR.
  SELECT SINGLE CONGR INTO L_CONGR
    FROM TF181
    WHERE CONGR = P_CONGR.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZFICO) WITH P_CONGR."合并组 & 不存在!
  ENDIF.

  IF P_PABRP > 16.
    MESSAGE E002(ZFICO) WITH P_PABRP.
  ENDIF.

ENDFORM.                    " F_CK_SEL_SCR




*&---------------------------------------------------------------------*
*&      Form  F_READ_CON_TAB
*&---------------------------------------------------------------------*
*       获取配置表信息，如果ZCS001/ZCS002/ZCS003中有一个表没维护则报错
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_CON_TAB .

* 报表行项目取值
  SELECT  *
    INTO TABLE GT_ZCS101
    FROM ZCS101
    WHERE REPOT = G_REPORT.
  IF SY-SUBRC <> 0.
    MESSAGE E001(ZFICO) WITH 'ZCS101' '报表类型为3的值'."表 & & 没维护,请先维护好这张表!
  ENDIF.

* 报表行项目文本描述
  SELECT  *
    INTO TABLE GT_ZCS102
    FROM ZCS102
    WHERE REPOT = G_REPORT.
  IF SY-SUBRC <> 0.
    MESSAGE E001(ZFICO) WITH 'ZCS102' '报表类型为3的值'."表 & & 没维护,请先维护好这张表!
  ENDIF.

ENDFORM.                    " F_READ_CON_TAB




*&---------------------------------------------------------------------*
*&      Form  F_SET_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SET_PERIOD .

  G_THIS_YEAR = P_RYEAR.
  G_THIS_PER = P_PABRP.

  G_PRE_YEAR = G_THIS_YEAR - 1.
  G_PRE_PER = P_PABRP.

ENDFORM.                    " F_SET_PERIOD




*&---------------------------------------------------------------------*
*&      Form  F_READ_ZCS005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_READ_ZCS005 .

  SELECT  *
    INTO TABLE GT_THIS_ZCS005
    FROM ZCS005
    WHERE RYEAR = G_THIS_YEAR
    AND   POPER = G_THIS_PER
    AND   RDIMEN = P_DIMEN
    AND   RCONGR = P_CONGR.


  SELECT  *
    INTO TABLE GT_PRE_ZCS005
    FROM ZCS005
    WHERE RYEAR = G_PRE_YEAR
    AND   POPER = G_PRE_PER
    AND   RDIMEN = P_DIMEN
    AND   RCONGR = P_CONGR.


ENDFORM.                    " F_READ_ZCS005



*&---------------------------------------------------------------------*
*&      Form  F_ZCS101_TO_DTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ZCS101_TO_DTL .

  FREE: GT_DTL.
  LOOP AT GT_ZCS101 ASSIGNING <GS_ZCS101>.
    APPEND INITIAL LINE TO GT_DTL ASSIGNING <GS_DTL>.
    MOVE-CORRESPONDING <GS_ZCS101> TO <GS_DTL>.
  ENDLOOP.

ENDFORM.                    " F_ZCS101_TO_DTL





*&---------------------------------------------------------------------*
*&      Form  F_SET_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SET_DEL .


  DATA: LT_LINES TYPE STANDARD TABLE OF ZFORMULA_LINE.
  FIELD-SYMBOLS: <LS_LINES> TYPE ZFORMULA_LINE.

  FIELD-SYMBOLS: <LS_DTL> TYPE TY_DTL.
  DATA: L_FORMULA TYPE ZZFI_CONTRAST-FORMULA.


  LOOP AT GT_DTL ASSIGNING <GS_DTL> WHERE FORMULA IS INITIAL.

    READ TABLE GT_THIS_ZCS005 ASSIGNING <GS_THIS_ZCS005> WITH KEY LINEN = <GS_DTL>-LINEN.
    IF SY-SUBRC = 0.
      <GS_DTL>-THISPER = <GS_DTL>-THISPER + <GS_THIS_ZCS005>-AMOAW.
    ENDIF.


    READ TABLE GT_PRE_ZCS005 ASSIGNING <GS_PRE_ZCS005> WITH KEY LINEN = <GS_DTL>-LINEN.
    IF SY-SUBRC = 0.
      <GS_DTL>-PREPER = <GS_DTL>-PREPER + <GS_PRE_ZCS005>-AMOAW.
    ENDIF.

  ENDLOOP.



* 有公式的行
  LOOP AT GT_DTL ASSIGNING <GS_DTL> WHERE FORMULA IS NOT INITIAL.

    FREE: LT_LINES.
    CLEAR: L_FORMULA.
    L_FORMULA = <GS_DTL>-FORMULA.
    CALL FUNCTION 'Z_SPLIT_FORMULA'
      EXPORTING
        I_FORMULA   = L_FORMULA
      TABLES
        OT_LINES    = LT_LINES
      EXCEPTIONS
        LOW_GT_HIGH = 1
        OTHERS      = 2.
    IF SY-SUBRC <> 0.
      MESSAGE E003(ZFICO) WITH <GS_DTL>-FORMULA.
    ENDIF.

    LOOP AT LT_LINES ASSIGNING <LS_LINES>.
      READ TABLE GT_DTL ASSIGNING <LS_DTL> WITH KEY ZNUMBER = <LS_LINES>-LOOPNR.
      IF SY-SUBRC = 0.
        IF <LS_LINES>-SIGN = '+'.
          <GS_DTL>-THISPER = <GS_DTL>-THISPER + <LS_DTL>-THISPER.
          <GS_DTL>-PREPER = <GS_DTL>-PREPER + <LS_DTL>-PREPER.
        ELSE.
          <GS_DTL>-THISPER = <GS_DTL>-THISPER - <LS_DTL>-THISPER.
          <GS_DTL>-PREPER = <GS_DTL>-PREPER - <LS_DTL>-PREPER.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SET_DEL




*&---------------------------------------------------------------------*
*&      Form  F_ZCS102_TO_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ZCS102_TO_SUM .

  FREE: GT_SUM.
  LOOP AT GT_ZCS102 ASSIGNING <GS_ZCS102>.
    APPEND INITIAL LINE TO GT_SUM ASSIGNING <GS_SUM>.
    MOVE-CORRESPONDING <GS_ZCS102> TO <GS_SUM>.
  ENDLOOP.

ENDFORM.                    " F_ZCS102_TO_SUM



*&---------------------------------------------------------------------*
*&      Form  F_SUM_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SUM_ALL .

  LOOP AT GT_SUM ASSIGNING <GS_SUM>.

    LOOP AT GT_DTL ASSIGNING <GS_DTL> WHERE REPOT = <GS_SUM>-REPOT AND ZNUMBER = <GS_SUM>-ZNUMBER.
      <GS_SUM>-THISPER = <GS_SUM>-THISPER + <GS_DTL>-THISPER.
      <GS_SUM>-PREPER = <GS_SUM>-PREPER + <GS_DTL>-PREPER.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SUM_ALL





*&---------------------------------------------------------------------*
*&      Form  f_alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  DATA: POS TYPE I.


  DEFINE FIELDCAT.
    clear gs_fieldcat.
    pos = pos + 1.
    gs_fieldcat-col_pos = pos.
    gs_fieldcat-fieldname = &1.
    gs_fieldcat-ref_tabname = &2.
    gs_fieldcat-ref_fieldname = &3.
    gs_fieldcat-seltext_l = &4.
    gs_fieldcat-no_zero = &5.
    gs_fieldcat-key = &6.
    append gs_fieldcat to gt_fieldcat.
  END-OF-DEFINITION.


  FIELDCAT 'ZTEXT' '' '' '项目' '' ''.
  FIELDCAT 'ZNUMBER' '' '' '序号' '' ''.
  FIELDCAT 'THISPER' '' '' '期末数' '' ''.
  FIELDCAT 'PREPER' '' '' '期初数' '' ''.
ENDFORM.                    "f_alv_fieldcat



*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_LAYOUT .
  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  gs_layout-box_fieldname = 'BOX'.
ENDFORM.                    " F_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  f_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_DISPLAY .

*  可以保存格式
  DATA:GS_VARIANT   TYPE DISVARIANT.
  GS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM          = SY-REPID
      I_SAVE                      = 'A'         "可以保存格式
      IS_VARIANT                  = GS_VARIANT  "LAYOUT参数 可以保存格式
      IS_LAYOUT                   = GS_LAYOUT
      IT_FIELDCAT                 = GT_FIELDCAT
      I_CALLBACK_HTML_TOP_OF_PAGE = 'HTML_TOP_OF_PAGE'
      I_CALLBACK_PF_STATUS_SET    = 'F_PF_STATUS'
      I_CALLBACK_USER_COMMAND     = 'F_USER_COMMAND'
      I_HTML_HEIGHT_TOP           = 18
    TABLES
      T_OUTTAB                    = GT_SUM
    EXCEPTIONS
      PROGRAM_ERROR               = 1
      OTHERS                      = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_DISPLAY


*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DOCUMENT   text
*----------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE USING DOCUMENT TYPE REF TO CL_DD_DOCUMENT.
  DATA: L_YEAR(4) TYPE C,
        L_MON(2)  TYPE C.


  DATA: M_P       TYPE I,
        M_BUFFER  TYPE STRING,
        M_BUFFER1 TYPE STRING,
        M_BUFFER2 TYPE STRING.

  L_YEAR = P_RYEAR.
  L_MON  = P_PABRP.

  CONCATENATE '<HTML ><CENTER><S1><STRONG>' '<FONT SIZE=5>合并利润表</FONT>'
              '</STRONG></S1></CENTER></HTML>'
         INTO M_BUFFER. " 居中 <H1>设置字体格式

  SELECT SINGLE TEXT INTO M_BUFFER1 FROM ZCS003 WHERE RCONGR = P_CONGR.
  CONCATENATE '编制单位:' M_BUFFER1 INTO G_TITLE.

  CONCATENATE P_RYEAR '年' P_PABRP '月' INTO G_DATE.


  CONCATENATE '<P ALIGN = LEFT > <STRONG>编制单位:</STRONG> '
              M_BUFFER1
              '<STRONG><span style="width:500px;"></span>期间:</STRONG> '
              L_YEAR '年' L_MON '期'
         INTO M_BUFFER1 .





  CALL METHOD DOCUMENT->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFFER
    CHANGING
      POSITION = M_P.

  CALL METHOD DOCUMENT->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFFER1
    CHANGING
      POSITION = M_P.


  CLEAR: L_YEAR,L_MON.

  CALL METHOD DOCUMENT->ADD_GAP
    EXPORTING
      WIDTH = 2.

ENDFORM. "HTML_TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM F_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STD'.
ENDFORM.                    "set_pf_status


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND  USING R_UCOMM LIKE SY-UCOMM
                          RS_SELFIELD TYPE SLIS_SELFIELD.

  IF R_UCOMM = 'PRINT'.

    PERFORM F_PRINT.

  ENDIF.

ENDFORM.                    "user_command



*&---------------------------------------------------------------------*
*&      Form  F_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PRINT .

  DATA: T_CONTROL_PARAMETERS TYPE SSFCTRLOP,
        T_OUTPUT_OPTIONS     TYPE SSFCOMPOP.

  T_OUTPUT_OPTIONS-TDNEWID = 'X'.    "New Spool
  T_OUTPUT_OPTIONS-TDIMMED = 'X'.
  T_OUTPUT_OPTIONS-TDDELETE = 'X'.    "Delete Spool After Print
  T_OUTPUT_OPTIONS-TDFINAL = 'X'.
  T_OUTPUT_OPTIONS-TDIEXIT = 'X'.       "Exit after printing in print preview

  DATA: L_SMARTFORM(30) TYPE C,
        L_FM_NAME       TYPE RS38L_FNAM.

  L_SMARTFORM = 'ZFI031_1'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = L_SMARTFORM
    IMPORTING
      FM_NAME            = L_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  CHECK L_FM_NAME IS NOT INITIAL.



  CALL FUNCTION L_FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = T_CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = T_OUTPUT_OPTIONS "默认打印选项设置传输
      USER_SETTINGS      = 'X'
      G_DATE             = G_DATE
      G_TEXT             = G_TITLE
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " F_PRINT
