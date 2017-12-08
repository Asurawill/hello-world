*&---------------------------------------------------------------------*
*&  包含                ZXQAAU01
*&  利亚德SAP实施项目开发申请-CO-无标准成本不允许下达生产订单 V10
*&  利亚德SAP实施项目开发申请-CO-无标准成本不允许发生货物移动 V10
*&---------------------------------------------------------------------*
DATA lw_marc TYPE marc.
SELECT SINGLE matnr
              werks
              mmsta
  FROM marc
  INTO CORRESPONDING FIELDS OF lw_marc
  WHERE matnr = i_matnr
  AND   werks = i_werks
  .
IF lw_marc-mmsta <> ''.
  IF lw_marc-mmsta <> 'Z2' .
  MESSAGE e002(zfico01) WITH i_matnr i_werks.
  ENDIF.
ENDIF.
E_TQ32 = I_TQ32.
"I_MATNR
"I_WERKS
