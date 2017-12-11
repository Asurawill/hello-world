*&---------------------------------------------------------------------*
*&  包含                ZXM02U05
*&---------------------------------------------------------------------*

DATA lw_marc TYPE marc.
DATA l_str TYPE string.
DATA l_message TYPE bapiret2.

IF im_data_new-matnr <> '' AND im_data_new-werks <> ''.
  SELECT SINGLE matnr
                werks
                bstrf
    FROM marc
    INTO CORRESPONDING FIELDS OF lw_marc
    WHERE matnr = im_data_new-matnr
    AND   werks = im_data_new-werks.
  IF sy-subrc = 0 AND lw_marc-bstrf > 0.
    l_str = lw_marc-bstrf.
    CONCATENATE '该工厂物料存在舍入值为：' l_str ',请留意。' INTO l_str.
    l_message-type = 'W'.
    l_message-id = 'SV'.
    l_message-number = '000'.
*          l_message-MESSAGE = l_str.
    l_message-message_v1 = l_str.
    APPEND l_message TO ex_messages.
  ENDIF.
ENDIF.
