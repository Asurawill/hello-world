*----------------------------------------------------------------------*
***INCLUDE ZXCN1I04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZBMMC_VALUELIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZBMMC_VALUELIST INPUT.
  DATA L_ID TYPE VRM_ID .
  DATA LT_VALUES TYPE VRM_VALUES WITH HEADER LINE .

  L_ID = 'CNCI_PRPS-ZBMMC' .

  REFRESH LT_VALUES .

  LT_VALUES-KEY = '1'.
  LT_VALUES-TEXT = '数字文化体验BU'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '2'.
  LT_VALUES-TEXT = '技术服务部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '3'.
  LT_VALUES-TEXT = '文化旅游展演BU'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '4'.
  LT_VALUES-TEXT = '公共文化设施BU'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
*&--代码添加 BY HANDYBY 26.07.2017 16:24:07  BEGIN
  LT_VALUES-KEY = '5'.
  LT_VALUES-TEXT = 'LEMUSE事业部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '6'.
  LT_VALUES-TEXT = '采购部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '7'.
  LT_VALUES-TEXT = '经营管理部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '8'.
  LT_VALUES-TEXT = '科技项目部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '9'.
  LT_VALUES-TEXT = '西北办'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '10'.
  LT_VALUES-TEXT = '市场与客户体验部'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
  LT_VALUES-KEY = '11'.
  LT_VALUES-TEXT = '总裁办'.
  APPEND LT_VALUES .
  CLEAR LT_VALUES .
*&--代码添加 BY HANDYBY 26.07.2017 16:24:07  END

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = L_ID
      VALUES          = LT_VALUES[]
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
  ENDIF.

ENDMODULE.
