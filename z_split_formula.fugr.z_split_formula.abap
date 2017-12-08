FUNCTION Z_SPLIT_FORMULA.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_FORMULA) TYPE  ZZFI_CONTRAST-FORMULA
*"  TABLES
*"      OT_LINES STRUCTURE  ZFORMULA_LINE
*"----------------------------------------------------------------------


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  types: begin of ty_data,
              str type char100,
           end of ty_data.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*
  data: gt_data type standard table of ty_data.
  field-symbols: <gs_data> type ty_data.

  data: temp_lg type standard table of ty_data.
  field-symbols: <gs_temp> type ty_data.

  data: gs_linen type zformula_line.

  data: g_formula like i_formula. "公式

  data: g_line_pos type zformula_line-loopnr.

  data: l_num_1 type zcs001-znumber,
        l_num_2 type zcs001-znumber.


*--------------------------------------------------------------------*
*
*--------------------------------------------------------------------*

  clear: g_formula.
  g_formula = i_formula.

*   去掉=
  replace '=' with space into g_formula.

  condense g_formula.  " 去掉空格   张伯兴添加  2014.8.21  DEVK902840

*   先通过+来拆分
  split g_formula at '+' into table gt_data.


*   再通过-来拆分
  loop at gt_data assigning <gs_data>.


    split <gs_data>-str at '-' into table temp_lg.
    "=10-300-670 =>  10 300 670
    loop at temp_lg assigning <gs_temp>.
      if sy-tabix = 1.
        gs_linen-sign = '+'.
      else.
        gs_linen-sign = '-'.
      endif.
**  汉得张伯兴 添加开始 20140828
      " =0010+0020:0080-0090
      " =0010-0020:0080-0090
      search <gs_temp>-str for ':'.
      if sy-subrc = 0.
        clear: l_num_1, l_num_2.
        split <gs_temp>-str at ':' into l_num_1 l_num_2.
        l_num_2 = l_num_2 + '0010'.
        while l_num_1 < l_num_2.
          gs_linen-loopnr = l_num_1.

          l_num_1 = l_num_1 + '0010'.
          append gs_linen to ot_lines.
        endwhile.
        continue.
      endif.
**  汉得张伯兴 添加结束 20140828
      gs_linen-loopnr = <gs_temp>-str.
      if gs_linen-sign is not initial and gs_linen-loopnr is not initial.
        append gs_linen to ot_lines.
      endif.
    endloop.
  endloop.




ENDFUNCTION.
