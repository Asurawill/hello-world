class ZBADI_NW_CO_VERS_CK definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_BADI_NW_CO_VER_CK .
protected section.
private section.
ENDCLASS.



CLASS ZBADI_NW_CO_VERS_CK IMPLEMENTATION.


  METHOD IF_BADI_NW_CO_VER_CK~ACTIVITY_VERSION_CHANGE.
** ADD BY HANDWY 2015-7-16 版本固定999
*    CO_OBJECT_HEADER-VERSN = '999'.
**ENDADD
  ENDMETHOD.
ENDCLASS.
