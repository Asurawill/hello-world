*&---------------------------------------------------------------------*
*& Report  ZCO007
*&
*&---------------------------------------------------------------------*
*& Create by     : 汪昱 （hand)
*& Create date   : 2015/8/3
*& Request       :
*& Descriptions  : 公司间采购成本&收入报表
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
REPORT ZCO007.


PARAMETERS:G_G1 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G2 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G3 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G4 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G5 TYPE CHAR1 RADIOBUTTON GROUP G1.


IF G_G1 = 'X'.
  CALL TRANSACTION 'ZCO007_5'.
ENDIF.

*差异分摊明细表
IF G_G2 = 'X'.
  CALL TRANSACTION 'ZCO007_4'.
ENDIF.

*开票明细表
IF G_G3 = 'X'.
  CALL TRANSACTION 'ZCO007_3'.
ENDIF.

*WL交货明细表
IF G_G4 = 'X'.
  CALL TRANSACTION 'ZCO007_2'.
ENDIF.

*ML差异分析报表
IF G_G5 = 'X'.
  CALL TRANSACTION 'ZCO007_1'.
ENDIF.
