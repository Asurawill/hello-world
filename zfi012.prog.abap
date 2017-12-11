*&---------------------------------------------------------------------*
* 作者: 徐兴旭
* 开发日期:2013-10-11
* 补充修改日期：
* 请求号: DEVK905870
* 申请者:
* 功能/技术文档:
* 描述:  DEV FICO DEVL: 合并报表
*&---------------------------------------------------------------------*


REPORT  zfi030.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_dtl,
          repot   TYPE  zcs101-repot,
          znumber TYPE  zcs101-znumber,
          linen   TYPE  zcs101-linen,
          formula TYPE  zcs101-formula,
          thisper TYPE  zcs004-amoaw, " 期末数
          preper  TYPE  zcs004-amoaw, " 期初数
       END OF ty_dtl.


TYPES: BEGIN OF ty_sum,
          repot   TYPE  zcs102-repot,
          znumber TYPE  zcs102-znumber,
          ztext   TYPE  zcs102-ztext,
          thisper TYPE  zcs004-amoaw, " 期末数
          preper  TYPE  zcs004-amoaw, " 期初数
       END OF ty_sum.


TYPES: BEGIN OF ty_show,
          repot1   TYPE  zcs102-repot,
          znumber1 TYPE  zcs102-znumber,
          ztext1   TYPE  zcs102-ztext,
          thisper1 TYPE  zcs004-amoaw, " 期末数
          preper1  TYPE  zcs004-amoaw, " 期初数
          repot2   TYPE  zcs102-repot,
          znumber2 TYPE  zcs102-znumber,
          ztext2   TYPE  zcs102-ztext,
          thisper2 TYPE  zcs004-amoaw, " 期末数
          preper2  TYPE  zcs004-amoaw, " 期初数
          box      TYPE  c,
       END OF ty_show.



*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

CONSTANTS: g_report TYPE zcs001-repot VALUE '1'.

DATA: g_this_year TYPE ecmca-ryear,
      g_this_per TYPE ecmca-poper,
      g_pre_year TYPE ecmca-ryear,
      g_pre_per TYPE ecmca-poper.

*配置表
DATA: gt_zcs101 TYPE STANDARD TABLE OF zcs101, " 报表行项目取值
      gt_zcs102 TYPE STANDARD TABLE OF zcs102. " 报表行项目文本描述
FIELD-SYMBOLS: <gs_zcs101> TYPE zcs101,
               <gs_zcs102> TYPE zcs102.

*选择条件对应的 合并资产负债表工作底稿
DATA: gt_this_zcs004 TYPE STANDARD TABLE OF zcs004.
FIELD-SYMBOLS: <gs_this_zcs004> TYPE zcs004.

*上年的 合并资产负债表工作底稿
DATA: gt_pre_zcs004 TYPE STANDARD TABLE OF zcs004.
FIELD-SYMBOLS: <gs_pre_zcs004> TYPE zcs004.

*明细
DATA: gt_dtl TYPE STANDARD TABLE OF ty_dtl.
FIELD-SYMBOLS: <gs_dtl> TYPE ty_dtl.

*汇总
DATA: gt_sum TYPE STANDARD TABLE OF ty_sum.
FIELD-SYMBOLS: <gs_sum> TYPE ty_sum.

DATA: gt_sum1 TYPE STANDARD TABLE OF ty_sum.
FIELD-SYMBOLS: <gs_sum1> TYPE ty_sum.

DATA: gt_sum2 TYPE STANDARD TABLE OF ty_sum.
FIELD-SYMBOLS: <gs_sum2> TYPE ty_sum.


DATA: gt_alv_show TYPE STANDARD TABLE OF ty_show,
      gs_alv_show TYPE ty_show.

DATA: g_title TYPE zcs003-text,
      g_date TYPE string.
*ALV
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gs_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
DATA: gs_layout TYPE slis_layout_alv.

*--------------------------------------------------------------------*
* 选择条件
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_dimen TYPE tf181-dimen OBLIGATORY,                    " 维
            p_congr TYPE tf181-congr OBLIGATORY,                    " 合并组
            p_ryear TYPE ecmct-ryear OBLIGATORY,                    " 会计年度
            p_pabrp TYPE t569v-pabrp OBLIGATORY.                    " 会计期间

SELECTION-SCREEN END OF BLOCK b1.



*--------------------------------------------------------------------*
* 处理快
*--------------------------------------------------------------------*

INITIALIZATION..

* 设置选择界面的值
  PERFORM g_set_value.


AT SELECTION-SCREEN.

* 检查选择屏幕输入的值
  PERFORM f_ck_sel_scr.


START-OF-SELECTION.

* 获取配置表
  PERFORM f_read_con_tab.

* 设置选择条件对应的本期和上期
  PERFORM f_set_period.

* 合并资产负债表工作底稿
  PERFORM f_read_zcs004.


* 把行次赋值给明细表
  PERFORM f_zcs101_to_dtl.

* 对明细行次填值
  PERFORM f_set_del.

* 把ZCS102赋给汇总表
  PERFORM f_zcs102_to_sum.

* 合计显示
  PERFORM f_sum_all.

* 处理显示
  PERFORM f_pre_show.



END-OF-SELECTION.

  IF gt_alv_show[] IS INITIAL.
    MESSAGE s004(zfico) DISPLAY LIKE 'E'.
  ELSE.
    PERFORM f_alv_fieldcat.
    PERFORM f_layout.
    PERFORM f_display.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  G_SET_VALUE
*&---------------------------------------------------------------------*
*       设置选择屏幕上的会计年度和会计期间的默认值
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM g_set_value .

  p_ryear = sy-datum+0(4).
  p_pabrp = sy-datum+4(2).

ENDFORM.                    " G_SET_VALUE



*&---------------------------------------------------------------------*
*&      Form  F_CK_SEL_SCR
*&---------------------------------------------------------------------*
*       检查选择屏幕上输入的值的有效性
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ck_sel_scr .

  DATA: l_congr TYPE tf181-congr.
  SELECT SINGLE congr INTO l_congr
    FROM tf181
    WHERE congr = p_congr.
  IF sy-subrc <> 0.
    MESSAGE e000(zfico) WITH p_congr."合并组 & 不存在!
  ENDIF.

  IF p_pabrp > 16.
    MESSAGE e002(zfico) WITH p_pabrp.
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
FORM f_read_con_tab .

* 报表行项目取值
  SELECT  *
    INTO TABLE gt_zcs101
    FROM zcs101
    WHERE repot = g_report.
  IF sy-subrc <> 0.
    MESSAGE e001(zfico) WITH 'ZCS101' '报表类型为1的值'."表 & & 没维护,请先维护好这张表!
  ENDIF.

* 报表行项目文本描述
  SELECT  *
    INTO TABLE gt_zcs102
    FROM zcs102
    WHERE repot = g_report.
  IF sy-subrc <> 0.
    MESSAGE e001(zfico) WITH 'ZCS102' '报表类型为1的值'."表 & & 没维护,请先维护好这张表!
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
FORM f_set_period .

  g_this_year = p_ryear.
  g_this_per = p_pabrp.

  g_pre_year = g_this_year - 1.
  g_pre_per = '012'.

ENDFORM.                    " F_SET_PERIOD



*&---------------------------------------------------------------------*
*&      Form  F_READ_ZCS004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_read_zcs004 .

  SELECT  *
    INTO TABLE gt_this_zcs004
    FROM zcs004
    WHERE ryear = g_this_year
    AND   poper = g_this_per
    AND   rdimen = p_dimen
    AND   rcongr = p_congr.


  SELECT  *
    INTO TABLE gt_pre_zcs004
    FROM zcs004
    WHERE ryear = g_pre_year
    AND   poper = g_pre_per
    AND   rdimen = p_dimen
    AND   rcongr = p_congr.


ENDFORM.                    " F_READ_ZCS004



*&---------------------------------------------------------------------*
*&      Form  F_ZCS101_TO_DTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_zcs101_to_dtl .

  FREE: gt_dtl.
  LOOP AT gt_zcs101 ASSIGNING <gs_zcs101>.
    APPEND INITIAL LINE TO gt_dtl ASSIGNING <gs_dtl>.
    MOVE-CORRESPONDING <gs_zcs101> TO <gs_dtl>.
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
FORM f_set_del .


  DATA: lt_lines TYPE STANDARD TABLE OF zformula_line.
  FIELD-SYMBOLS: <ls_lines> TYPE zformula_line.

  FIELD-SYMBOLS: <ls_dtl> TYPE ty_dtl.
  DATA: l_formula TYPE zzfi_contrast-formula.


  LOOP AT gt_dtl ASSIGNING <gs_dtl> WHERE formula IS INITIAL.

    READ TABLE gt_this_zcs004 ASSIGNING <gs_this_zcs004> WITH KEY linen = <gs_dtl>-linen.
    IF sy-subrc = 0.
      <gs_dtl>-thisper = <gs_dtl>-thisper + <gs_this_zcs004>-amoaw.
    ENDIF.


    READ TABLE gt_pre_zcs004 ASSIGNING <gs_pre_zcs004> WITH KEY linen = <gs_dtl>-linen.
    IF sy-subrc = 0.
      <gs_dtl>-preper = <gs_dtl>-preper + <gs_pre_zcs004>-amoaw.
    ENDIF.

  ENDLOOP.



* 有公式的行
  LOOP AT gt_dtl ASSIGNING <gs_dtl> WHERE formula IS NOT INITIAL.

    FREE: lt_lines.
    CLEAR: l_formula.
    l_formula = <gs_dtl>-formula.
    CALL FUNCTION 'Z_SPLIT_FORMULA'
      EXPORTING
        i_formula   = l_formula
      TABLES
        ot_lines    = lt_lines
      EXCEPTIONS
        low_gt_high = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zfico) WITH <gs_dtl>-formula.
    ENDIF.

    LOOP AT lt_lines ASSIGNING <ls_lines>.
      READ TABLE gt_dtl ASSIGNING <ls_dtl> WITH KEY znumber = <ls_lines>-loopnr.
      IF sy-subrc = 0.
        IF <ls_lines>-sign = '+'.
          <gs_dtl>-thisper = <gs_dtl>-thisper + <ls_dtl>-thisper.
          <gs_dtl>-preper = <gs_dtl>-preper + <ls_dtl>-preper.
        ELSE.
          <gs_dtl>-thisper = <gs_dtl>-thisper - <ls_dtl>-thisper.
          <gs_dtl>-preper = <gs_dtl>-preper - <ls_dtl>-preper.
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
FORM f_zcs102_to_sum .

  FREE: gt_sum.
  LOOP AT gt_zcs102 ASSIGNING <gs_zcs102>.
    APPEND INITIAL LINE TO gt_sum ASSIGNING <gs_sum>.
    MOVE-CORRESPONDING <gs_zcs102> TO <gs_sum>.
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
FORM f_sum_all .

  LOOP AT gt_sum ASSIGNING <gs_sum>.

    LOOP AT gt_dtl ASSIGNING <gs_dtl> WHERE repot = <gs_sum>-repot AND znumber = <gs_sum>-znumber.
      <gs_sum>-thisper = <gs_sum>-thisper + <gs_dtl>-thisper.
      <gs_sum>-preper = <gs_sum>-preper + <gs_dtl>-preper.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SUM_ALL



*&---------------------------------------------------------------------*
*&      Form  F_PRE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_pre_show .

  DATA: l_lines TYPE i.
  DATA: l_haf1 TYPE p DECIMALS 2.
  DATA: l_haf2 TYPE i.

  DATA: l_pre TYPE i,
        l_after TYPE i.

  DESCRIBE TABLE gt_sum LINES l_lines.

  IF l_lines MOD 2 = 0.
    l_haf2 = l_lines / 2.
  ELSE.
    l_haf1 = l_lines / 2.

    CALL FUNCTION 'ROUND'
      EXPORTING
*       DECIMALS      = 0
        input         = l_haf1
        sign          = '+'
      IMPORTING
        output        = l_haf1
      EXCEPTIONS
        input_invalid = 1
        overflow      = 2
        type_invalid  = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    l_haf2 = l_haf1.

  ENDIF.


  l_pre = l_haf2.
  l_after = l_pre + 1.


  LOOP AT gt_sum ASSIGNING <gs_sum> TO l_pre.
    APPEND <gs_sum> TO gt_sum1.
  ENDLOOP.

  LOOP AT gt_sum ASSIGNING <gs_sum> FROM l_after.
    APPEND <gs_sum> TO gt_sum2.
  ENDLOOP.

  FREE: gt_alv_show.
  LOOP AT gt_sum1 ASSIGNING <gs_sum1>.

    CLEAR: gs_alv_show.
    gs_alv_show-repot1 = <gs_sum1>-repot.
    gs_alv_show-znumber1 = <gs_sum1>-znumber.
    gs_alv_show-ztext1 = <gs_sum1>-ztext.
    gs_alv_show-thisper1 = <gs_sum1>-thisper.
    gs_alv_show-preper1 = <gs_sum1>-preper.

    READ TABLE gt_sum2 ASSIGNING <gs_sum2> INDEX sy-tabix.
    IF sy-subrc = 0.
      gs_alv_show-repot2 = <gs_sum2>-repot.
      gs_alv_show-znumber2 = <gs_sum2>-znumber.
      gs_alv_show-ztext2 = <gs_sum2>-ztext.
      gs_alv_show-thisper2 = <gs_sum2>-thisper.
      gs_alv_show-preper2 = <gs_sum2>-preper.
    ENDIF.

    APPEND gs_alv_show TO gt_alv_show.

  ENDLOOP.

ENDFORM.                    " F_PRE_SHOW




*&---------------------------------------------------------------------*
*&      Form  f_alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  DATA: pos TYPE i.


  DEFINE fieldcat.
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


  fieldcat 'ZTEXT1' '' '' '资产' '' ''.
  fieldcat 'ZNUMBER1' '' '' '序号' '' ''.
  fieldcat 'THISPER1' '' '' '期末数' '' ''.
  fieldcat 'PREPER1' '' '' '期初数' '' ''.

  fieldcat 'ZTEXT2' '' '' '负债和所有者权益' '' ''.
  fieldcat 'ZNUMBER2' '' '' '序号' '' ''.
  fieldcat 'THISPER2' '' '' '期末数' '' ''.
  fieldcat 'PREPER2' '' '' '期初数' '' ''.

ENDFORM.                    "f_alv_fieldcat




*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_layout .
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-box_fieldname = 'BOX'.
ENDFORM.                    " F_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  f_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_display .

*  可以保存格式
  DATA:gs_variant   TYPE disvariant.
  gs_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program          = sy-repid
      i_save                      = 'A'         "可以保存格式
      is_variant                  = gs_variant  "LAYOUT参数 可以保存格式
      is_layout                   = gs_layout
      it_fieldcat                 = gt_fieldcat
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_callback_pf_status_set    = 'F_PF_STATUS'
      i_callback_user_command     = 'F_USER_COMMAND'
      i_html_height_top           = 18
    TABLES
      t_outtab                    = gt_alv_show
    EXCEPTIONS
      program_error               = 1
      OTHERS                      = 2.
  IF sy-subrc <> 0.
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
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.
  DATA: l_year(4) TYPE c,
        l_mon(2) TYPE c.


  DATA: m_p TYPE i ,
        m_buffer TYPE string,
        m_buffer1 TYPE string ,
        m_buffer2 TYPE string .

  DATA: l_date TYPE d.

  l_year = p_ryear.
  l_mon  = p_pabrp.

  CONCATENATE '<HTML ><CENTER><S1><STRONG>' '<FONT SIZE=5>合并资产负债表</FONT>'
              '</STRONG></S1></CENTER></HTML>'
         INTO m_buffer. " 居中 <H1>设置字体格式

  SELECT SINGLE text INTO m_buffer1 FROM zcs003 WHERE rcongr = p_congr.
  CONCATENATE '编制单位:' m_buffer1 INTO g_title.

  CONCATENATE p_ryear p_pabrp '01' INTO l_date.
  CALL FUNCTION 'FKK_GET_LAST_DAY_OF_MONTH'
    EXPORTING
*     I_YEAR     =
*     I_MONTH    =
      i_date_in  = l_date
    IMPORTING
      e_date_out = l_date.

  CONCATENATE p_ryear '年' p_pabrp '月' l_date+6(2) INTO g_date.


  CONCATENATE '<P ALIGN = LEFT > <STRONG>编制单位:</STRONG> '
              m_buffer1
              '<STRONG><span style="width:500px;"></span>期间:</STRONG> '
              l_year '年' l_mon '期'
         INTO m_buffer1 .


  CALL METHOD document->html_insert
    EXPORTING
      contents = m_buffer
    CHANGING
      position = m_p.

  CALL METHOD document->html_insert
    EXPORTING
      contents = m_buffer1
    CHANGING
      position = m_p.


  CLEAR: l_year,l_mon.

  CALL METHOD document->add_gap
    EXPORTING
      width = 2.

ENDFORM. "HTML_TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM f_pf_status USING rt_extab TYPE slis_t_extab.
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
FORM f_user_command  USING r_ucomm LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.

  IF r_ucomm = 'PRINT'.

    PERFORM f_print.

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
FORM f_print .


  DATA: t_control_parameters TYPE ssfctrlop,
        t_output_options TYPE ssfcompop.

  t_output_options-tdnewid = 'X'.    "New Spool
  t_output_options-tdimmed = 'X'.
  t_output_options-tddelete = 'X'.    "Delete Spool After Print
  t_output_options-tdfinal = 'X'.
  t_output_options-tdiexit = 'X'.       "Exit after printing in print preview

  DATA: l_smartform(30) TYPE c,
        l_fm_name TYPE rs38l_fnam.

  l_smartform = 'ZFI030_1'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = l_smartform
    IMPORTING
      fm_name            = l_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CHECK l_fm_name IS NOT INITIAL.

  CALL FUNCTION l_fm_name
    EXPORTING
      control_parameters = t_control_parameters
      output_options     = t_output_options "默认打印选项设置传输
      user_settings      = 'X'
      g_date             = g_date
      g_text             = g_title
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.                    " F_PRINT
