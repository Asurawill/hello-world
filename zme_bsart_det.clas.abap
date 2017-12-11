class ZME_BSART_DET definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_BSART_DET .
protected section.
private section.
ENDCLASS.



CLASS ZME_BSART_DET IMPLEMENTATION.


  METHOD IF_EX_ME_BSART_DET~BSART_DETERMINE.

* ADD BY HANDWY 2015-8-12  ME57 默认给订单类型Z01,公司间的默认Z06
    DATA L_LEN   TYPE I.
    DATA L_FLIEF TYPE EBAN-FLIEF.

    CLEAR L_LEN.
    CLEAR L_FLIEF.

    L_FLIEF = IS_EBAN-FLIEF.

    SHIFT L_FLIEF LEFT DELETING LEADING '0'.

    IF EF_BSART IS INITIAL.

      L_LEN = STRLEN( L_FLIEF ).

      IF L_LEN = '4'.
        EF_BSART = 'Z06'.
        EF_PSTYP = '0'.
      ELSE.
        EF_BSART = 'Z01'.
        EF_PSTYP = '0'.
      ENDIF.
    ENDIF.
* ENDADD.

  ENDMETHOD.
ENDCLASS.
