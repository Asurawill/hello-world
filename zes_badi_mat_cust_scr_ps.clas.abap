class ZES_BADI_MAT_CUST_SCR_PS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_MAT_CUSTOMER_SCR .
protected section.
private section.
ENDCLASS.



CLASS ZES_BADI_MAT_CUST_SCR_PS IMPLEMENTATION.


  METHOD IF_MAT_CUSTOMER_SCR~GET_CUST_FEILDS.


    "    IM_CI_RSADD-ZTEST = CI_RSADD-ZTEST.
*    IF SY-UNAME = 'HANDABAP'.
*      BREAK-POINT.
*    ENDIF.
*    BREAK HAND.
*
*    IM_CI_RSADD-ZTEST = IM_CI_RSADD-ZTEST.

*    DATA GS_ZTEST99 TYPE ZTEST99  .
*    CLEAR GS_ZTEST99.
*
*    SELECT SINGLE * FROM ZTEST99
*      INTO CORRESPONDING FIELDS OF GS_ZTEST99
*      WHERE RSNUM = IM_CI_RSADD-RSNUM
*      AND   RSPOS = IM_CI_RSADD-RSPOS.
*
*    IM_CI_RSADD-ZTEST = GS_ZTEST99-ZTEST.

    DATA RESBD TYPE RESBD.
    DATA RESB  TYPE RESB.

    "BREAK HAND.
    "BREAK handabap.

*    DATA  GT_ZTEST99 TYPE TABLE OF ZTEST99.
*    DATA  GS_ZTEST99 TYPE ZTEST99.
*
*    REFRESH GT_ZTEST99.
*    MOVE-CORRESPONDING IM_CI_RSADD TO GS_ZTEST99.
*    APPEND GS_ZTEST99 TO GT_ZTEST99.
*
*    MODIFY ZTEST99 FROM TABLE GT_ZTEST99.


  ENDMETHOD.


  METHOD IF_MAT_CUSTOMER_SCR~GET_SCREEN_DETAILS.
    "BREAK HAND.
    PRG_NAME = 'ZTEST'.
    SCR_NUM = '9000'.
    BREAK handabap.
  ENDMETHOD.
ENDCLASS.