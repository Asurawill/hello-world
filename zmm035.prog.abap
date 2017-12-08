REPORT zmm035.

"Created by :IT02
"Request:    整备库报表维护及查询
"Modify by:
"Modify date:

*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2017-02-10  it02&魏云           ED1K905553  “隐藏刷新台账事务
*& 2017-06-26  it02&魏云           ED1K905872  “取消-隐藏刷新台账事务


"
DATA:gt_zzbksj TYPE TABLE OF zzbksj,
     gs_zzbksj TYPE zzbksj.

DATA:i_msg TYPE string. "消息

"START-OF-SELECTION.

PARAMETERS:
  g_g1 TYPE char1 RADIOBUTTON GROUP g1,
  g_g2 TYPE char1 RADIOBUTTON GROUP g1,
  g_g3 TYPE char1 RADIOBUTTON GROUP g1,
  g_g4 TYPE char1 RADIOBUTTON GROUP g1.
"  G_G5 TYPE CHAR1 RADIOBUTTON GROUP G1.

*"更新凭证及项目匹配关系
IF g_g1 = 'X'.
 CALL TRANSACTION 'ZMM035_1'.
ENDIF.

IF g_g2 = 'X'.
  CALL TRANSACTION 'ZMM035_2'.
ENDIF.

IF g_g3 = 'X'.

  CALL TRANSACTION 'ZMM035_3'.
ENDIF.


IF g_g4 = 'X'.
  CALL TRANSACTION 'ZMM035_4'.

ENDIF.

INITIALIZATION.
  SELECT * INTO TABLE gt_zzbksj
    FROM zzbksj .
  SORT  gt_zzbksj BY whdat DESCENDING whtim DESCENDING .
  READ TABLE gt_zzbksj INTO gs_zzbksj INDEX 1.
  IF sy-subrc EQ 0.
    CLEAR:i_msg.
    CONCATENATE '上次更新时间为:' gs_zzbksj-whdat+0(4) '年' INTO i_msg.
    CONCATENATE i_msg gs_zzbksj-whdat+4(2) '月' INTO i_msg.
    CONCATENATE i_msg gs_zzbksj-whdat+6(2) '日' ',' INTO i_msg.
    CONCATENATE i_msg gs_zzbksj-whtim+0(2) ':'  INTO i_msg.
    CONCATENATE i_msg gs_zzbksj-whtim+2(2) ':'  INTO i_msg.
    CONCATENATE i_msg gs_zzbksj-whtim+4(2)  INTO i_msg.
    MESSAGE i_msg TYPE 'I'.
  ENDIF.
