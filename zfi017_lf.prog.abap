REPORT ZFI017_LF.
*&---------------------------------------------------------------------*
*& Report  ZFI006
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/03/2
*& Request       :
*& Descriptions  : 采购付款计划，申请及执行跟踪报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*


PARAMETERS:G_G1 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G2 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G3 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G4 TYPE CHAR1 RADIOBUTTON GROUP G1.

*采购录入
IF G_G1 = 'X'.
  CALL TRANSACTION 'ZFI017_LF1'.
ENDIF.

*财务审批
IF G_G2 = 'X'.
  CALL TRANSACTION 'ZFI017_LF2'.
ENDIF.

*报表查询
IF G_G3 = 'X'.
  CALL TRANSACTION 'ZFI017_LF3'.
ENDIF.

*申请修改
IF G_G4 = 'X'.
  CALL TRANSACTION 'ZFI017_LF4'.
ENDIF.
