*----------------------------------------------------------------------*
***INCLUDE ZFI008_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9001 input.
  data: l_subrc type sy-subrc.

  save_ok = ok_code.
  clear ok_code.

  case save_ok.
    when 'BACK'.
      perform frm_check_changed changing l_subrc.
      if l_subrc = 0.
        leave to screen 0.
      endif.
    when 'EXIT'.
      perform frm_check_changed changing l_subrc.
      if l_subrc = 0.
        leave program.
      endif.
    when 'POST'.
      perform frm_post_accdoc.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  VBELN_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module vbeln_input input.
  select single *
    into wa_vbak
    from vbak
    where vbeln = wa_head-vbeln.
  if sy-subrc ne 0.
    message '此销售订单不存在！' type 'E'.
  endif.

  select single *
    into wa_vbkd
    from vbkd
    where vbeln = wa_head-vbeln.

  select *
    into table it_vbap
    from vbap
    where vbeln = wa_head-vbeln.
  sort it_vbap by vbeln posnr.

  select *
    into table it_zfi008
    from zfi008
    where vbeln = wa_head-vbeln.
  sort it_zfi008 by vbeln posnr.

  select *
    into table it_vbpa
    from vbpa
    where vbeln = wa_head-vbeln
     and  parvw = 'RE'.
  sort it_vbpa by vbeln posnr parvw.


  " 货币默认为销售订单货币
  wa_head-waers = wa_vbak-waerk.

  " 取项目名称 - 销售订单抬头文本
  perform frm_get_pjnam using wa_head-vbeln
                        changing wa_head-pjnam.

  if it_item is initial.
    " 初始化三行行项目

    perform frm_item_init.
  else.
    loop at it_item assigning <fs_item>.
      if <fs_item>-vbeln is not initial.
        <fs_item>-vbeln = wa_head-vbeln.
      endif.
    endloop.
  endif.

  g_changed = abap_true.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  LEAVE_DYNPRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module leave_dynpro input.
  save_ok = ok_code.
  clear ok_code.

  case save_ok.
    when 'CANL'.
      leave program.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  WAERS_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module waers_input input.
  if wa_head-waers is not initial.
    loop at it_item assigning <fs_item>.
      <fs_item>-waers = wa_head-waers.
    endloop.
  endif.

  g_changed = abap_true.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  BUKRS_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module bukrs_input input.
  check wa_head-bukrs is not initial.

  perform frm_auth_check using '10' wa_head-bukrs.
  if sy-subrc ne 0.
    message e010(zfico01) with wa_head-bukrs.
  endif.

  select single land1
    into g_land1
    from t001
    where bukrs = wa_head-bukrs.
  if g_land1 is not initial.
    select single kalsm
      into g_kalsm
      from t005
      where land1 = g_land1.
  endif.

  g_changed = abap_true.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9002 input.
  save_ok = ok_code.
  clear ok_code.

  case save_ok.
    when 'OK'.
      if g_stgrd is initial.
        message '冲销原因必输！' type 'I' display like 'E'.
      else.
        wa_reversal-reason_rev = g_stgrd.
        leave to screen 0.
      endif.
    when 'CANL'.
      clear: wa_reversal-reason_rev,
             wa_reversal-pstng_date.
      leave to screen 0.
    when others.
  endcase.
endmodule.
