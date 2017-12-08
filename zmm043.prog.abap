REPORT ZMM043.
"Created by :IT02
"Request:    项目物料管理报表
"Modify by:
"Modify date:

*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2017-06-22  it02&魏云           ED1K905809  “项目物料管理报表


"
*DATA:gt_zzbksj TYPE TABLE OF zzbksj,
*     gs_zzbksj TYPE zzbksj.

DATA:i_msg TYPE string. "消息

"START-OF-SELECTION.

PARAMETERS:
  g_g1 TYPE char1 RADIOBUTTON GROUP g1,
  g_g2 TYPE char1 RADIOBUTTON GROUP g1.
"  G_G5 TYPE CHAR1 RADIOBUTTON GROUP G1.

*"更新凭证及项目匹配关系
IF g_g1 = 'X'.
  CALL TRANSACTION 'ZMM043_1'.
ENDIF.

IF g_g2 = 'X'.
  CALL TRANSACTION 'ZMM043_2'.
ENDIF.
