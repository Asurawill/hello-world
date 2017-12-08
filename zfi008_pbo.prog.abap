*----------------------------------------------------------------------*
***INCLUDE ZFI008_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9001 output.
  set pf-status 'STA9001'.
  set titlebar 'TIT9001'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module alv_init output.
  if gr_container is initial.
    perform frm_create_container.
    perform frm_alv_display.
  else.
    perform frm_refresh_alv.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_screen output.
  loop at screen.
    if screen-group1 = 'MOD'.
      if g_edit_mod = gc_editable.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.
      modify screen.
    endif.
  endloop.

  data l_alv_edit type int4.
  l_alv_edit = gr_alvgrid->is_ready_for_input( ).
  if ( l_alv_edit = 1 and g_edit_mod ne gc_editable )
    or ( l_alv_edit = 0 and g_edit_mod ne gc_readonly ).
    l_alv_edit = 1 - l_alv_edit.
    perform frm_change_edit_mode using l_alv_edit.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9002 output.
  set pf-status 'STA9002'.
*  SET TITLEBAR 'xxx'.
endmodule.
