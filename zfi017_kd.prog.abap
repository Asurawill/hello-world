REPORT ZFI017_KD.
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& Create by     : 康迪（hand）
*& Create date   : 2017/08/21
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
  CALL TRANSACTION 'ZFI017_1_KD'.
ENDIF.

*财务审批
IF G_G2 = 'X'.
  CALL TRANSACTION 'ZFI017_2_KD'.
ENDIF.

*报表查询
IF G_G3 = 'X'.
  CALL TRANSACTION 'ZFI017_3_KD'.
ENDIF.

*申请修改
IF G_G4 = 'X'.
  CALL TRANSACTION 'ZFI017_4_KD'.
ENDIF.
