class ZCL_IM_BADI_MATERIAL_REF definition
  public
  final
  create public .

public section.

  interfaces IF_EX_MATERIAL_REFERENCE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BADI_MATERIAL_REF IMPLEMENTATION.


  METHOD if_ex_material_reference~create_material.
    "如果创建物料的评估范围为：2100 ，价格控制指示符默认为'V ' ADD BY it02 20160728
    IF c_mbew-bwkey = '2100' AND  c_mbew-vprsv ne  'V'..
      c_mbew-vprsv = 'V'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
