*&---------------------------------------------------------------------*
* 作者: 徐兴旭
* 开发日期:2013-10-13
* 补充修改日期：
* 请求号: DEVK905870
* 申请者:
* 功能/技术文档:
* 描述:  DEV FICO DEVL: 合并利润表-底稿
*&---------------------------------------------------------------------*

REPORT  ZFI013A.



*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_ecmct,
          rldnr  TYPE  ecmct-rldnr, " 主键
          rrcty  TYPE  ecmct-rrcty,
          rvers  TYPE  ecmct-rvers,
          robjnr TYPE  ecmct-robjnr,
          cobjnr TYPE  ecmct-cobjnr,
          sobjnr TYPE  ecmct-sobjnr,



          ryear  TYPE  ecmct-ryear,   " 会计年度
          rbunit TYPE  ecmct-rbunit,  " 合并单元
          ritem  TYPE  ecmct-ritem,   " 财务报表项目
          plevl  TYPE  ecmct-plevl,   " 记帐等级
*          ZZFBKER TYPE ecmct-ZZFBKER, " 功能范围
          kslvt  TYPE  ecmct-kslvt,   " 期初金额
          ksl01  TYPE  ecmct-ksl01,   "
          ksl02  TYPE  ecmct-ksl02,   "
          ksl03  TYPE  ecmct-ksl03,   "
          ksl04  TYPE  ecmct-ksl04,   "
          ksl05  TYPE  ecmct-ksl05,   "
          ksl06  TYPE  ecmct-ksl06,   "
          ksl07  TYPE  ecmct-ksl07,   "
          ksl08  TYPE  ecmct-ksl08,   "
          ksl09  TYPE  ecmct-ksl09,   "
          ksl10  TYPE  ecmct-ksl10,   "
          ksl11  TYPE  ecmct-ksl11,   "
          ksl12  TYPE  ecmct-ksl12,   "
          ksl13  TYPE  ecmct-ksl13,   "
          ksl14  TYPE  ecmct-ksl14,   "
          ksl15  TYPE  ecmct-ksl15,   "
          ksl16  TYPE  ecmct-ksl16,   "
       END OF ty_ecmct.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*


DATA: g_report TYPE zcs001-repot.



*配置表
DATA: gt_zcs001 TYPE STANDARD TABLE OF zcs001, " 报表行项目取值
      gt_zcs002 TYPE STANDARD TABLE OF zcs002, " 报表行项目文本描述
      gt_zcs003 TYPE STANDARD TABLE OF zcs003. " 合并组
FIELD-SYMBOLS: <gs_zcs001> TYPE zcs001,
               <gs_zcs002> TYPE zcs002,
               <gs_zcs003> TYPE zcs003.

*动态内表的字段结构
DATA: gt_structure TYPE lvc_t_fcat,
      gs_structure TYPE lvc_s_fcat.

*动态内表和工作区
DATA: dy_table TYPE REF TO data,
      wa_table TYPE REF TO data.

*动态内表，工作区，字段的指针
FIELD-SYMBOLS: <gt_data> TYPE STANDARD TABLE,
               <gs_data>,
               <g_field>.

*字段的值
DATA: g_field TYPE lvc_fname.

*ecmct
DATA: gt_ecmct TYPE STANDARD TABLE OF ty_ecmct.
FIELD-SYMBOLS: <gs_ecmct> TYPE ty_ecmct.

*PLEVL<=10
DATA: gt_ecmct_le10 TYPE STANDARD TABLE OF ty_ecmct.
FIELD-SYMBOLS: <gs_ecmct_le10> TYPE ty_ecmct.

*PLEVL>10
DATA: gt_ecmct_gt10 TYPE STANDARD TABLE OF ty_ecmct.
FIELD-SYMBOLS: <gs_ecmct_gt10> TYPE ty_ecmct.

*显示表
DATA: dy_tab_show TYPE REF TO data,
      wa_tab_show TYPE REF TO data.

FIELD-SYMBOLS: <gt_show> TYPE STANDARD TABLE,
               <gs_show>.

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

SELECTION-SCREEN SKIP 1.

*PARAMETERS: p_sum RADIOBUTTON GROUP g1 DEFAULT 'X',
*            p_dtl RADIOBUTTON GROUP g1.

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

* 生成动态内表
  PERFORM f_dy_tab.

* 把行次赋值给动态内表
  PERFORM f_zcs001_to_dytab.

* 取ECMCT的值
  PERFORM f_get_ecmct.

* 对明细行次填值
  PERFORM f_set_del.

* 生成显示的动态内表
  PERFORM f_dytab_show.

* 把ZCS002赋给显示表
  PERFORM f_zcs002_to_dytab.

* 合计显示
  PERFORM f_sum_all.

  PERFORM f_save_to_zcs005.


END-OF-SELECTION.

  IF <gt_show>[] IS INITIAL.
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

*  IF p_sum = 'X'.
    g_report = '2'.
*  ELSEIF p_dtl = 'X'.
*    g_report = '5'.
*  ENDIF.

* 报表行项目取值
  SELECT  *
    INTO TABLE gt_zcs001
    FROM zcs001
    WHERE repot = g_report.
  IF sy-subrc <> 0.
    MESSAGE S001(zfico) WITH 'ZCS001' '报表类型为2的值' DISPLAY LIKE 'E'."表 & & 没维护,请先维护好这张表!
    LEAVE LIST-PROCESSING.
  ENDIF.

* 报表行项目文本描述
  SELECT  *
    INTO TABLE gt_zcs002
    FROM zcs002
    WHERE repot = g_report.
  IF sy-subrc <> 0.
    MESSAGE S001(zfico) WITH 'ZCS002' '报表类型为2的值' DISPLAY LIKE 'E'."表 & & 没维护,请先维护好这张表!
    LEAVE LIST-PROCESSING.
  ENDIF.

* 合并组
  SELECT  *
    INTO TABLE gt_zcs003
    FROM zcs003
    WHERE rdimen = p_dimen
    AND   rcongr = p_congr.
  IF sy-subrc <> 0.
    MESSAGE S001(zfico) WITH 'ZCS003' '' DISPLAY LIKE 'E'."表 & & 没维护,请先维护好这张表!
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " F_READ_CON_TAB



*&---------------------------------------------------------------------*
*&      Form  F_DY_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_dy_tab .

* 创建结构
  PERFORM f_create_structure.

* 创建内表
  PERFORM f_create_dynamic_table .


ENDFORM.                    " F_DY_TAB




*&---------------------------------------------------------------------*
*&      Form  F_CREATE_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_structure .

  DATA: l_field TYPE lvc_fname,  " 字段名
        l_pos   TYPE lvc_colpos VALUE 0. " 列标


  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'REPOT' l_pos space 0  0 'ZCS001' 'REPOT'.   " 报表类型

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'LINEN' l_pos space 0  0 'ZCS001' 'LINEN'.   " 行次

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'ZNUMBER' l_pos space 0  0 'ZCS001' 'ZNUMBER'.   " 序号

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'FITEM' l_pos space 0  0 'ZCS001' 'FITEM'.   " 从

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'TITEM' l_pos space 0  0 'ZCS001' 'TITEM'.   " 到

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'ZMINUS' l_pos space 0  0 'ZCS001' 'ZMINUS'.   " 负

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'FORMULA' l_pos space 0  0 'ZCS001' 'FORMULA'.   " 负

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'ZZFKBER' l_pos space 0  0 'ZCS001' 'ZZFKBER'.   " 功能范围

  LOOP AT gt_zcs003 ASSIGNING <gs_zcs003>.

    CLEAR: l_field.
    l_field = <gs_zcs003>-rbunit.
    ADD 1 TO l_pos.
    PERFORM f_init_structure USING: l_field l_pos space 0  0 'EINE' 'AMOAW'.   " 合并单元

  ENDLOOP.

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'SUM' l_pos space 0  0 'EINE' 'AMOAW'.   " 合计

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'GTZER' l_pos space 0  0 'EINE' 'AMOAW'.   " 借方抵消数

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'LTZER' l_pos space 0  0 'EINE' 'AMOAW'.   " 贷方抵消数

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'ALLSUM' l_pos space 0  0 'EINE' 'AMOAW'.   " 合并数据


ENDFORM.                    " F_CREATE_STRUCTURE



*&---------------------------------------------------------------------*
*&      Form  f_init_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDNAME  text
*      -->P_COL_POS    text
*      -->P_INTTYPE    text
*      -->P_INTLEN     text
*      -->P_DECIMALS   text
*----------------------------------------------------------------------*
FORM f_init_structure USING p_fieldname TYPE lvc_fname
                          p_col_pos   TYPE lvc_colpos
                          p_inttype   TYPE inttype
                          p_intlen    TYPE intlen
                          p_decimals  TYPE decimals
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field.
  gs_structure-fieldname = p_fieldname.
  gs_structure-col_pos = p_col_pos.
  gs_structure-inttype = p_inttype.
  gs_structure-intlen  = p_intlen.
  gs_structure-decimals = p_decimals.
  gs_structure-ref_table = p_ref_table.
  gs_structure-ref_field = p_ref_field.
  APPEND gs_structure TO gt_structure.
  CLEAR: gs_structure .

ENDFORM.                    " INIT_STRUCTURE


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_dynamic_table .

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_structure
    IMPORTING
      ep_table        = dy_table.

* 指向内表的指针
  ASSIGN dy_table->* TO <gt_data>.

* 创建内表的行
  CREATE DATA wa_table LIKE LINE OF <gt_data>.

* 指向行的指针
  ASSIGN wa_table->* TO <gs_data>.

  FREE: gt_structure.

ENDFORM.                    " F_CREATE_DYNAMIC_TABLE



*&---------------------------------------------------------------------*
*&      Form  F_ZCS002_TO_DYTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_zcs001_to_dytab .

  LOOP AT gt_zcs001 ASSIGNING <gs_zcs001>.

    CLEAR: <gs_data>.
    IF <gs_zcs001>-titem IS INITIAL .
      <gs_zcs001>-titem = <gs_zcs001>-fitem.
    ENDIF.
    MOVE-CORRESPONDING <gs_zcs001> TO <gs_data>.
    APPEND <gs_data> TO <gt_data>.

  ENDLOOP.

ENDFORM.                    " F_ZCS002_TO_DYTAB



*&---------------------------------------------------------------------*
*&      Form  F_GET_ECMCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_ecmct .

  IF gt_zcs003 IS NOT INITIAL.

    SELECT  rldnr
            rrcty
            rvers
            robjnr
            cobjnr
            sobjnr

            ryear
            rbunit
            ritem
            plevl
*            zzfbker
            kslvt
            ksl01
            ksl02
            ksl03
            ksl04
            ksl05
            ksl06
            ksl07
            ksl08
            ksl09
            ksl10
            ksl11
            ksl12
            ksl13
            ksl14
            ksl15
            ksl16
      INTO CORRESPONDING FIELDS OF TABLE gt_ecmct
      FROM ecmct
      FOR ALL ENTRIES IN gt_zcs003
      WHERE rbunit = gt_zcs003-rbunit
      AND   ryear = p_ryear.

    SORT gt_ecmct BY ritem rbunit.

*   PLEVL<=10
    APPEND LINES OF gt_ecmct TO gt_ecmct_le10.
    DELETE gt_ecmct_le10 WHERE plevl > 10.

*   PLEVL>10
    APPEND LINES OF gt_ecmct TO gt_ecmct_gt10.
    DELETE gt_ecmct_gt10 WHERE plevl <= 10.

    FREE: gt_ecmct.

  ENDIF.

ENDFORM.                    " F_GET_ECMCT



*&---------------------------------------------------------------------*
*&      Form  F_SET_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_del .

  FIELD-SYMBOLS: <l_fitem>,
                 <l_titem>.
  DATA: l_field TYPE lvc_fname.  " 字段名

  DATA: l_pabrp TYPE t569v-pabrp,
        l_kslxx TYPE lvc_fname.
  FIELD-SYMBOLS: <l_kslxx>,
                 <l_sum>,
                 <l_zminus>.

  FIELD-SYMBOLS: <l_gtzer>,
                 <l_ltzer>.
  FIELD-SYMBOLS: <l_allsum>.
  FIELD-SYMBOLS: <l_field>.

  FIELD-SYMBOLS: <l_zzfkber>.

  DATA: l_formula TYPE zzfi_contrast-formula.

* 有FITEM的行
  LOOP AT <gt_data> ASSIGNING <gs_data>.

    PERFORM f_assign_filed USING 'FITEM'.
    CHECK <g_field> IS NOT INITIAL."

    ASSIGN COMPONENT 'FITEM' OF STRUCTURE <gs_data> TO <l_fitem>." 从
    ASSIGN COMPONENT 'TITEM' OF STRUCTURE <gs_data> TO <l_titem>." 到
    ASSIGN COMPONENT 'ZMINUS' OF STRUCTURE <gs_data> TO <l_zminus>.
    ASSIGN COMPONENT 'SUM' OF STRUCTURE <gs_data> TO <l_sum>.
    ASSIGN COMPONENT 'ZZFKBER' OF STRUCTURE <gs_data> TO <l_zzfkber>.


*   把范围内的值依照不同的合并单元填入不同的列，计算总计字段
    LOOP AT  gt_ecmct_le10 ASSIGNING <gs_ecmct_le10> WHERE ritem GE <l_fitem> AND ritem LE <l_titem>.

*     有功能范围的功能范围也要做一个选项
      IF <l_zzfkber> IS NOT INITIAL .
        " change by handwlb 20141215
*        CHECK <gs_ecmct_le10>-ZZFBKER = <l_zzfkber>.
       ELSE.
*         CONTINUE.
                " change by handwlb 20141215 end
      ENDIF.

*     相应的列
      CLEAR: l_field.
      l_field = <gs_ecmct_le10>-rbunit.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.

*     期初
      IF <l_zminus> IS INITIAL.
        <g_field> = <g_field> + <gs_ecmct_le10>-kslvt.
        <l_sum> = <l_sum> + <gs_ecmct_le10>-kslvt."SUM
      ELSE.
        <g_field> = <g_field> - <gs_ecmct_le10>-kslvt.
        <l_sum> = <l_sum> - <gs_ecmct_le10>-kslvt."SUM
      ENDIF.


*     加到当前月
      l_pabrp = '01'.
      WHILE  l_pabrp LE p_pabrp.

        CONCATENATE 'KSL' l_pabrp INTO l_kslxx.
        ASSIGN COMPONENT l_kslxx OF STRUCTURE <gs_ecmct_le10> TO <l_kslxx>.
        IF <l_zminus> IS INITIAL.
          <g_field> = <g_field> +  <l_kslxx>.
          <l_sum> = <l_sum> +  <l_kslxx>."SUM
        ELSE.
          <g_field> = <g_field> -  <l_kslxx>.
          <l_sum> = <l_sum> -  <l_kslxx>."SUM
        ENDIF.

        ADD 1 TO l_pabrp.
      ENDWHILE.

    ENDLOOP.


    UNASSIGN: <g_field>,
              <l_kslxx>.


*   借方抵消数/贷方抵消数
    ASSIGN COMPONENT 'GTZER' OF STRUCTURE <gs_data> TO <l_gtzer>.
    ASSIGN COMPONENT 'LTZER' OF STRUCTURE <gs_data> TO <l_ltzer>.
    LOOP AT gt_ecmct_gt10 ASSIGNING <gs_ecmct_gt10> WHERE ritem GE <l_fitem> AND ritem LE <l_titem>.

      IF <l_zzfkber> IS NOT INITIAL.
                " change by handwlb 20141215
*        CHECK <gs_ecmct_gt10>-ZZFBKER = <l_zzfkber>.
      ELSE.
*        CONTINUE.
                " change by handwlb 20141215 end
      ENDIF.

      IF <gs_ecmct_gt10>-kslvt > 0.
        <l_gtzer> = <l_gtzer> + <gs_ecmct_gt10>-kslvt.
      ELSE.
        <l_ltzer> = <l_ltzer> - <gs_ecmct_gt10>-kslvt.
      ENDIF.

*     加到当前月
      l_pabrp = '01'.
      WHILE  l_pabrp LE p_pabrp.

        CONCATENATE 'KSL' l_pabrp INTO l_kslxx.
        ASSIGN COMPONENT l_kslxx OF STRUCTURE <gs_ecmct_gt10> TO <l_kslxx>.

        IF <l_kslxx>  > 0.
          <l_gtzer> = <l_gtzer> + <l_kslxx>.
        ELSE.
          <l_ltzer> = <l_ltzer> - <l_kslxx>.
        ENDIF.

        ADD 1 TO l_pabrp.
      ENDWHILE.

    ENDLOOP.

*   合并数据
    ASSIGN COMPONENT 'ALLSUM' OF STRUCTURE <gs_data> TO <l_allsum>.
    IF <l_zminus> IS INITIAL.
      <l_allsum> = <l_sum> + ( <l_gtzer> - <l_ltzer> ).
    ELSE.
      <l_allsum> = <l_sum> - ( <l_gtzer> - <l_ltzer> ).
    ENDIF.

  ENDLOOP.





* 有公式的行
  DATA: lt_lines TYPE STANDARD TABLE OF zformula_line.
  FIELD-SYMBOLS: <ls_lines> TYPE zformula_line.
  FIELD-SYMBOLS: <ls_data>.

  FIELD-SYMBOLS: <l_linen>,
                 <l_sign>.

  LOOP AT <gt_data> ASSIGNING <gs_data>.

    PERFORM f_assign_filed USING 'FORMULA'.
    CHECK <g_field> IS NOT INITIAL."

    FREE: lt_lines.
    CLEAR: l_formula.
    l_formula = <g_field>.
    CALL FUNCTION 'Z_SPLIT_FORMULA'
      EXPORTING
        i_formula   = l_formula
      TABLES
        ot_lines    = lt_lines
      EXCEPTIONS
        low_gt_high = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zfico) WITH <g_field>.
    ENDIF.

    LOOP AT lt_lines ASSIGNING <ls_lines>.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_lines> TO <l_sign>.


*      loop at <gt_data> ASSIGNING <ls_data> WHERE LINEN = <ls_lines>-linen.
      LOOP AT <gt_data> ASSIGNING <ls_data>.
        ASSIGN COMPONENT 'LINEN' OF STRUCTURE <ls_data> TO <l_linen>.
        CHECK <l_linen> = <ls_lines>-loopnr.

*       各合并单元为一列
        LOOP AT gt_zcs003 ASSIGNING <gs_zcs003>.
          CLEAR: l_field.
          l_field = <gs_zcs003>-rbunit.

          UNASSIGN: <g_field>,<l_field>.
          ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.
          ASSIGN COMPONENT l_field OF STRUCTURE <ls_data> TO <l_field>.

          IF <l_sign> = '+'.
            <g_field> = <g_field> + <l_field>.
          ELSE.
            <g_field> = <g_field> - <l_field>.
          ENDIF.
        ENDLOOP.

*       合计
        UNASSIGN: <g_field>,<l_field>.
        l_field = 'SUM'.
        ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.
        ASSIGN COMPONENT l_field OF STRUCTURE <ls_data> TO <l_field>.
        IF <l_sign> = '+'.
          <g_field> = <g_field> + <l_field>.
        ELSE.
          <g_field> = <g_field> - <l_field>.
        ENDIF.
*       借方抵消数
        UNASSIGN: <g_field>,<l_field>.
        l_field = 'GTZER'.
        ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.
        ASSIGN COMPONENT l_field OF STRUCTURE <ls_data> TO <l_field>.
        IF <l_sign> = '+'.
          <g_field> = <g_field> + <l_field>.
        ELSE.
          <g_field> = <g_field> - <l_field>.
        ENDIF.

*       贷方抵消数
        UNASSIGN: <g_field>,<l_field>.
        l_field = 'LTZER'.
        ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.
        ASSIGN COMPONENT l_field OF STRUCTURE <ls_data> TO <l_field>.
        IF <l_sign> = '+'.
          <g_field> = <g_field> + <l_field>.
        ELSE.
          <g_field> = <g_field> - <l_field>.
        ENDIF.

*       合并数据
        UNASSIGN: <g_field>,<l_field>.
        l_field = 'ALLSUM'.
        ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <g_field>.
        ASSIGN COMPONENT l_field OF STRUCTURE <ls_data> TO <l_field>.
        IF <l_sign> = '+'.
          <g_field> = <g_field> + <l_field>.
        ELSE.
          <g_field> = <g_field> - <l_field>.
        ENDIF.


      ENDLOOP.

    ENDLOOP.


  ENDLOOP.

ENDFORM.                    " F_SET_DEL



*&---------------------------------------------------------------------*
*&      Form  assign_filed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILED_NAME  text
*----------------------------------------------------------------------*
FORM f_assign_filed  USING p_filed_name.

  ASSIGN COMPONENT p_filed_name OF STRUCTURE <gs_data> TO <g_field>.

ENDFORM.                    "assign_filed



*&---------------------------------------------------------------------*
*&      Form  F_DYTAB_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_dytab_show .

* 创建结构
  PERFORM f_create_structure_show.

* 创建内表
  PERFORM f_create_dynamic_table_show .


ENDFORM.                    " F_DYTAB_SHOW


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_STRUCTURE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_structure_show .

  DATA: l_field TYPE lvc_fname,  " 字段名
        l_pos   TYPE lvc_colpos VALUE 0. " 列标


  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'REPOT' l_pos space 0  0 'ZCS002' 'REPOT'.   " 报表类型

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'LINEN' l_pos space 0  0 'ZCS002' 'LINEN'.   " 行次

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'LTEXT' l_pos space 0  0 'ZCS002' 'LTEXT'.   " 会计项目


  LOOP AT gt_zcs003 ASSIGNING <gs_zcs003>.

    CLEAR: l_field.
    l_field = <gs_zcs003>-rbunit.
    ADD 1 TO l_pos.
    PERFORM f_init_structure USING: l_field l_pos space 0  0 'EINE' 'AMOAW'.   " 合并单元

  ENDLOOP.

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'SUM' l_pos space 0  0 'EINE' 'AMOAW'.   " 合计

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'GTZER' l_pos space 0  0 'EINE' 'AMOAW'.   " 借方抵消数

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'LTZER' l_pos space 0  0 'EINE' 'AMOAW'.   " 贷方抵消数

  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'ALLSUM' l_pos space 0  0 'EINE' 'AMOAW'.   " 合并数据


  ADD 1 TO l_pos.
  PERFORM f_init_structure USING: 'BOX' l_pos 'C' 1  0 '' ''.   "


ENDFORM.                    " F_CREATE_STRUCTURE_SHOW



*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_dynamic_table_show .

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_structure
    IMPORTING
      ep_table        = dy_tab_show.

* 指向内表的指针
  ASSIGN dy_tab_show->* TO <gt_show>.

* 创建内表的行
  CREATE DATA wa_tab_show LIKE LINE OF <gt_show>.

* 指向行的指针
  ASSIGN wa_tab_show->* TO <gs_show>.

  FREE: gt_structure.
ENDFORM.                    " F_CREATE_DYNAMIC_TABLE_SHOW



*&---------------------------------------------------------------------*
*&      Form  F_ZCS002_TO_DYTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_zcs002_to_dytab .

  LOOP AT gt_zcs002 ASSIGNING <gs_zcs002> .

    CLEAR: <gs_show>.
    MOVE-CORRESPONDING <gs_zcs002> TO <gs_show>.
    APPEND <gs_show> TO <gt_show>.

  ENDLOOP.

ENDFORM.                    " F_ZCS002_TO_DYTAB




*&---------------------------------------------------------------------*
*&      Form  F_SUM_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sum_all .

  FIELD-SYMBOLS: <l_show_report>,
                 <l_show_linen>,
                 <l_show_kslxx>.


  FIELD-SYMBOLS: <l_data_report>,
                 <l_data_linen>,
                 <l_data_kslxx>.

  DATA: l_field TYPE lvc_fname.  " 字段名

  LOOP AT <gt_show> ASSIGNING <gs_show>.

    ASSIGN COMPONENT 'REPOT' OF STRUCTURE <gs_show> TO <l_show_report>.
    ASSIGN COMPONENT 'LINEN' OF STRUCTURE <gs_show> TO <l_show_linen>.

    LOOP AT <gt_data> ASSIGNING <gs_data>.
      ASSIGN COMPONENT 'REPOT' OF STRUCTURE <gs_data> TO <l_data_report>.
      ASSIGN COMPONENT 'LINEN' OF STRUCTURE <gs_data> TO <l_data_linen>.
      CHECK <l_data_report> = <l_show_report> AND <l_data_linen> = <l_show_linen>.

      LOOP AT gt_zcs003 ASSIGNING <gs_zcs003>.
        CLEAR: l_field.
        l_field = <gs_zcs003>-rbunit.

        ASSIGN COMPONENT l_field OF STRUCTURE <gs_show> TO <l_show_kslxx>.
        ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <l_data_kslxx>.
        <l_show_kslxx> = <l_show_kslxx> + <l_data_kslxx>.
      ENDLOOP.






*       合计
      UNASSIGN: <l_show_kslxx>,<l_data_kslxx>.
      l_field = 'SUM'.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_show> TO <l_show_kslxx>.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <l_data_kslxx>.
      <l_show_kslxx> = <l_show_kslxx> + <l_data_kslxx>.

*       借方抵消数
      UNASSIGN: <l_show_kslxx>,<l_data_kslxx>.
      l_field = 'GTZER'.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_show> TO <l_show_kslxx>.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <l_data_kslxx>.
      <l_show_kslxx> = <l_show_kslxx> + <l_data_kslxx>.

*       贷方抵消数
      UNASSIGN: <l_show_kslxx>,<l_data_kslxx>.
      l_field = 'LTZER'.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_show> TO <l_show_kslxx>.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <l_data_kslxx>.
      <l_show_kslxx> = <l_show_kslxx> + <l_data_kslxx>.

*       合并数据
      UNASSIGN: <l_show_kslxx>,<l_data_kslxx>.
      l_field = 'ALLSUM'.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_show> TO <l_show_kslxx>.
      ASSIGN COMPONENT l_field OF STRUCTURE <gs_data> TO <l_data_kslxx>.
      <l_show_kslxx> = <l_show_kslxx> + <l_data_kslxx>.


    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SUM_ALL



*&---------------------------------------------------------------------*
*&      Form  F_SAVE_TO_ZCS005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save_to_zcs005 .

*  CHECK p_sum = 'X'.


  DATA: lt_zcs005 TYPE STANDARD TABLE OF zcs005,
        ls_zcs005 TYPE zcs005.

  FIELD-SYMBOLS: <l_linen>.
  FIELD-SYMBOLS: <l_amoaw>.

  FREE: lt_zcs005.

  LOOP AT <gt_show> ASSIGNING <gs_show>.

    CLEAR: ls_zcs005.
    ls_zcs005-rdimen = p_dimen.
    ls_zcs005-rcongr = p_congr.
    ls_zcs005-ryear = p_ryear.
    ls_zcs005-poper = p_pabrp.

    ASSIGN COMPONENT 'LINEN' OF STRUCTURE <gs_show> TO <l_linen>.
    ls_zcs005-linen = <l_linen>.

    ASSIGN COMPONENT 'ALLSUM' OF STRUCTURE <gs_show> TO <l_amoaw>.
    ls_zcs005-amoaw = <l_amoaw>.

    APPEND ls_zcs005 TO lt_zcs005.

  ENDLOOP.

  MODIFY zcs005 FROM TABLE lt_zcs005.



ENDFORM.                    " F_SAVE_TO_ZCS005


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


  fieldcat 'LTEXT' '' '' '会计项目' '' ''.
  fieldcat 'LINEN' '' '' '行次' '' ''.
  LOOP AT gt_zcs003 ASSIGNING <gs_zcs003>.
    fieldcat <gs_zcs003>-rbunit '' '' <gs_zcs003>-text2 '' ''.
  ENDLOOP.
  fieldcat 'SUM' '' '' '合计' '' ''.
  fieldcat 'GTZER' '' '' '借方抵消数' '' ''.
  fieldcat 'LTZER' '' '' '贷方抵消数' '' ''.
  fieldcat 'ALLSUM' '' '' '合并数据' '' ''.

ENDFORM.                    "f_alv_fieldcat




*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_layout .
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname = 'BOX'.
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
      i_callback_user_command     = 'F_USER_COMMAND'  "用户交互
      i_html_height_top           = 18
    TABLES
      t_outtab                    = <gt_show>
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
  l_year = p_ryear.
  l_mon  = p_pabrp.

  DATA: m_p TYPE i ,
        m_buffer TYPE string,
        m_buffer1 TYPE string ,
        m_buffer2 TYPE string .


  CONCATENATE '<HTML ><CENTER><S1><STRONG>' '<FONT SIZE=5>合并工作底稿-利润表</FONT>'
              '</STRONG></S1></CENTER></HTML>'
         INTO m_buffer. " 居中 <H1>设置字体格式

  SELECT SINGLE text INTO m_buffer1 FROM zcs003 WHERE rcongr = p_congr.
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
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  DATA: lc_grid TYPE REF TO cl_gui_alv_grid.

  FIELD-SYMBOLS: <ls_click>.

* 双击的字段
  DATA: l_click_field TYPE lvc_fname.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lc_grid.

  CALL METHOD lc_grid->check_changed_data.

  CASE   r_ucomm.
    WHEN '&IC1'.
      READ TABLE <gt_show> ASSIGNING <ls_click> INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        CASE rs_selfield-fieldname.
          WHEN 'GTZER' OR 'LTZER'.
*            PERFORM f_click USING <ls_click> rs_selfield-fieldname.
        ENDCASE.
      ENDIF.
  ENDCASE.


ENDFORM.                    "F_USER_COMMAND





*&---------------------------------------------------------------------*
*&      Form  F_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_click USING ls_click l_fieldname.


  TYPES: BEGIN OF ty_ecmca_1,
            docnr  TYPE  ecmca-docnr,
            ryear  TYPE  ecmca-ryear,
            poper  TYPE  ecmca-poper,
            ritem  TYPE  ecmca-ritem,
            plevl  TYPE  ecmca-plevl,
         END OF ty_ecmca_1.

  TYPES: BEGIN OF ty_ecmca_2,
            docnr    TYPE  ecmca-docnr,
            docln    TYPE  ecmca-docln,
            gtolt    TYPE  zgtolt,
            rbunit   TYPE  ecmca-rbunit,
            ritem    TYPE  ecmca-ritem,
            subit    TYPE  ecmca-subit,
            rbuptr   TYPE  ecmca-rbuptr,
*            zzfkber  TYPE  ecmca-zzfkber,
            zzrstg1  TYPE  ecmca-zzrstg1,
            ksl      TYPE  ecmca-ksl,
            sgtxt    TYPE  ecmca-sgtxt,
         END OF ty_ecmca_2.
*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  DATA: lt_click_linen TYPE STANDARD TABLE OF zcs001.
  FIELD-SYMBOLS: <ls_click_linen> TYPE zcs001.
  FIELD-SYMBOLS: <ls_zcs001> TYPE zcs001.

  DATA: lt_click_linen_zzfkber TYPE STANDARD TABLE OF zcs001.
  DATA: lt_click_linen_no_zzfkber TYPE STANDARD TABLE OF zcs001.



  DATA: lt_lines TYPE STANDARD TABLE OF zformula_line.
  FIELD-SYMBOLS: <ls_lines> TYPE zformula_line.

* 双击的行次
  DATA: l_report TYPE zcs001-repot,
        l_linen  TYPE zcs001-linen.

  FIELD-SYMBOLS: <l_report>,
                 <l_linen>.
* 选择期间
  DATA: p_poper TYPE ecmca-poper.

  DATA: lt_docno TYPE STANDARD TABLE OF ty_ecmca_1.

  DATA: lt_ecmea TYPE STANDARD TABLE OF ty_ecmca_2.
  FIELD-SYMBOLS: <ls_ecmea> TYPE ty_ecmca_2.

*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*


  p_poper = p_pabrp.

* 找出点击到的行次
  ASSIGN COMPONENT 'REPOT' OF STRUCTURE ls_click TO <l_report>.
  ASSIGN COMPONENT 'LINEN' OF STRUCTURE ls_click TO <l_linen>.
  l_report = <l_report>.
  l_linen = <l_linen>.

* 根据点到的行从ZCS001里面找出明细范围
  FREE: lt_click_linen.
  LOOP AT gt_zcs001 ASSIGNING <gs_zcs001> WHERE repot = l_report AND linen = l_linen.
    IF <gs_zcs001>-fitem IS NOT INITIAL .
      APPEND <gs_zcs001> TO lt_click_linen.
    ELSE.

      FREE: lt_lines.
      CALL FUNCTION 'Z_SPLIT_FORMULA'
        EXPORTING
          i_formula   = <gs_zcs001>-formula
        TABLES
          ot_lines    = lt_lines
        EXCEPTIONS
          low_gt_high = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        MESSAGE e003(zfico) WITH <g_field>.
      ENDIF.

      LOOP AT lt_lines ASSIGNING <ls_lines>.

        LOOP AT gt_zcs001 ASSIGNING <ls_zcs001> WHERE linen = <ls_lines>-loopnr.
          CHECK <ls_zcs001>-fitem IS NOT INITIAL.
          APPEND <ls_zcs001> TO lt_click_linen.
        ENDLOOP.

      ENDLOOP.

    ENDIF.
  ENDLOOP.



  CHECK lt_click_linen[] IS NOT INITIAL .
  FREE: lt_click_linen_zzfkber,
        lt_click_linen_no_zzfkber.
  LOOP AT lt_click_linen ASSIGNING <ls_click_linen>.
    IF <ls_click_linen>-zzfkber IS INITIAL .
      APPEND <ls_click_linen> TO lt_click_linen_no_zzfkber.
    ELSE.
      APPEND <ls_click_linen> TO lt_click_linen_zzfkber.
    ENDIF.
  ENDLOOP.

  IF l_fieldname = 'GTZER'.
*   有功能范围的和没有的区别查询
    IF lt_click_linen_no_zzfkber IS NOT INITIAL.
      SELECT  docnr
              ryear
              poper
              ritem
              plevl
        INTO CORRESPONDING FIELDS OF TABLE lt_docno
        FROM ecmca
        FOR ALL ENTRIES IN lt_click_linen_no_zzfkber
        WHERE ritem GE lt_click_linen_no_zzfkber-fitem
        AND   ritem LE lt_click_linen_no_zzfkber-titem
        AND   ryear = p_ryear
        AND   poper LE p_poper
        AND   ksl GT 0
        AND   plevl GT 10.
    ENDIF.
    IF lt_click_linen_zzfkber IS NOT INITIAL.
      SELECT  docnr
              ryear
              poper
              ritem
              plevl
        APPENDING CORRESPONDING FIELDS OF TABLE lt_docno
        FROM ecmca
        FOR ALL ENTRIES IN lt_click_linen_zzfkber
        WHERE ritem GE lt_click_linen_zzfkber-fitem
        AND   ritem LE lt_click_linen_zzfkber-titem
*        AND   zzfkber = lt_click_linen_zzfkber-zzfkber
        AND   ryear = p_ryear
        AND   poper LE p_poper
        AND   ksl GT 0
        AND   plevl GT 10.
    ENDIF.
  ELSEIF l_fieldname = 'LTZER'.
*   有功能范围的和没有的区别查询
    IF lt_click_linen_no_zzfkber IS NOT INITIAL.
      SELECT  docnr
              ryear
              poper
              ritem
              plevl
        INTO CORRESPONDING FIELDS OF TABLE lt_docno
        FROM ecmca
        FOR ALL ENTRIES IN lt_click_linen_no_zzfkber
        WHERE ritem GE lt_click_linen_no_zzfkber-fitem
        AND   ritem LE lt_click_linen_no_zzfkber-titem
        AND   ryear = p_ryear
        AND   poper LE p_poper
        AND   ksl LT 0
        AND   plevl GT 10.
    ENDIF.
    IF lt_click_linen_zzfkber IS NOT INITIAL.
      SELECT  docnr
              ryear
              poper
              ritem
              plevl
        APPENDING CORRESPONDING FIELDS OF TABLE lt_docno
        FROM ecmca
        FOR ALL ENTRIES IN lt_click_linen_zzfkber
        WHERE ritem GE lt_click_linen_zzfkber-fitem
        AND   ritem LE lt_click_linen_zzfkber-titem
*        AND   zzfkber = lt_click_linen_zzfkber-zzfkber
        AND   ryear = p_ryear
        AND   poper LE p_poper
        AND   ksl LT 0
        AND   plevl GT 10.
    ENDIF.
  ENDIF.

  SORT lt_docno BY docnr.
  DELETE ADJACENT DUPLICATES FROM lt_docno COMPARING docnr.


  IF lt_docno IS NOT INITIAL.
    SELECT  docnr
            docln
            rbunit
            ritem
            subit
            rbuptr
*            zzfkber
            zzrstg1
            ksl
            sgtxt
      INTO CORRESPONDING FIELDS OF TABLE lt_ecmea
      FROM ecmca
      FOR ALL ENTRIES IN lt_docno
      WHERE docnr = lt_docno-docnr.
    LOOP AT lt_ecmea ASSIGNING <ls_ecmea>.
      IF <ls_ecmea>-ksl > 0 .
        <ls_ecmea>-gtolt = '借'.
      ELSE.
        <ls_ecmea>-gtolt = '贷'.
      ENDIF.
    ENDLOOP.
  ENDIF.


* 显示
  DATA: go_table TYPE REF TO cl_salv_table.
  DATA: alv_functions TYPE REF TO cl_salv_functions.
  TRY.

      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_table
        CHANGING
          t_table      = lt_ecmea.
    CATCH cx_salv_msg .
  ENDTRY.

  go_table->set_screen_status(
            pfstatus      =  'STD'
            report        =  sy-repid
            set_functions = go_table->c_functions_all ).

  go_table->display( ).

ENDFORM.                    " F_CLICK
