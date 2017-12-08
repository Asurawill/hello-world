class ZME_BSART_DEL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_BSART_DETERMINE .
protected section.
private section.

  aliases MC_BSART_NB
    for IF_EX_ME_BSART_DETERMINE~MC_BSART_NB .
  aliases MC_FALSE
    for IF_EX_ME_BSART_DETERMINE~MC_FALSE .
  aliases MC_TRUE
    for IF_EX_ME_BSART_DETERMINE~MC_TRUE .
ENDCLASS.



CLASS ZME_BSART_DEL IMPLEMENTATION.


METHOD IF_EX_ME_BSART_DETERMINE~BSART_DETERMINE.

* define local data objects
  DATA: lo_badi  TYPE REF TO me_bsart_determine,
        lv_bsart TYPE bsart,
        lv_group TYPE mmpur_bool.
*
* call BAdI to determine 'group item' flag
  GET BADI lo_badi.
  CALL BADI lo_badi->grouped_by_item
    EXPORTING
      is_eban  = is_eban
    CHANGING
      cv_group = lv_group.
* call old functionality to determine Document type
  CALL FUNCTION 'MM_PROPOSE_DOC_TYPE'
    EXPORTING
      im_eban   = is_eban
      im_gpstyp = lv_group
    IMPORTING
      ex_bsart  = cv_bsart.
*
  IF cv_bsart IS INITIAL.
    cv_bsart = me->mc_bsart_nb.
  ELSE.
    cv_bsart = lv_bsart.
  ENDIF.
ENDMETHOD.


METHOD IF_EX_ME_BSART_DETERMINE~GROUPED_BY_ITEM.
* define local data objects
  DATA lo_const TYPE REF TO cl_mmpur_constants.
*
  lo_const = cl_mmpur_constants=>get_instance( ).
  IF is_eban-bsart EQ 'NB' AND is_eban-pstyp EQ lo_const->pstyp_0.
    cv_group = lo_const->no.
  ELSE.
    cv_group = lo_const->yes.
  ENDIF.
ENDMETHOD.
ENDCLASS.
