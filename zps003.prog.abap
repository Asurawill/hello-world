report zps003.

type-pools:slis.

tables:proj.

types:begin of ty_tab,
        zpspid type zpscost-zpspid, "项目定义
        zcsxm  type zpscost-zcsxm, "措施项目
        zxfc   type zpscost-zxfc, "小辅材
        zgf    type zpscost-zgf, "规费
        zsj    type zpscost-zsj, "税金
        zzlj   type zpscost-zzlj, "暂列金
        zqtfy  type zpscost-zqtfy, "其他费用
        zbz    type zpscost-zbz, "备注
      end of ty_tab.
data:it_tab type table of ty_tab,
     wa_tab type ty_tab.
data:it_fieldcat type lvc_t_fcat,
     wa_fieldcat like line of it_fieldcat,

     it_layout   type table of lvc_s_layo,
     wa_layout   type lvc_s_layo.

define init_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = &1.
  wa_fieldcat-coltext = &2.
  wa_fieldcat-ref_table = &3.
  wa_fieldcat-ref_field = &4.
  APPEND wa_fieldcat TO it_fieldcat.
end-of-definition.
selection-screen begin of block text with frame title text-001.
select-options:s_xmdy for proj-pspid.
selection-screen end of block text.

*----------------------------------------------------------------------*
*                  初 始 化 块                                         *
*----------------------------------------------------------------------*
initialization.
*----------------------------------------------------------------------*
*                  选 择 屏 幕 字 段 处 理 块
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  逻 辑 处 理 块                                      *
*----------------------------------------------------------------------*
start-of-selection.
  perform frm_getdata.
*  perform frm_dealdata.
  perform frm_layout.
  perform frm_fieldcat.
  perform frm_output.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_getdata .
  select *
    from zpscost
    into corresponding fields of table it_tab.

*  if it_tab is initial.
*    message:'没有数据！' type 'I' display like 'E'.
*    stop.
*  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form frm_dealdata .
*  loop at it_tab into wa_tab.
*
*  endloop.
*endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_layout .
  wa_layout-cwidth_opt = 'X'.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_fieldcat .
  init_fieldcat 'zpspid' '项目定义' 'zpscost' 'zpspid'.
  init_fieldcat 'zcsxm' '措施项目' 'zpscost' 'zcsxm'.
  init_fieldcat 'zxfc' '小辅材' 'zpscost' 'zxfc'.
  init_fieldcat 'zgf' '规费' 'zpscost' 'zgf'.
  init_fieldcat 'zsj' '税金' 'zpscost' 'zsj'.
  init_fieldcat 'zzlj' '暂列金' 'zpscost' 'zzlj'.
  init_fieldcat 'zqtfy' '其他费用' 'zpscost' 'zqtfy'.
  init_fieldcat 'zbz' '备注' 'zpscost' 'zbz'.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_output .
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'ALV_PF_STATUS'
*     i_callback_user_command  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = it_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC        =
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    tables
      t_outtab           = it_tab
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.
