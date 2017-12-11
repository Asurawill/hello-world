*----------------------------------------------------------------------*
***INCLUDE LZPS001F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_MSG1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSG1[]  text
*----------------------------------------------------------------------*
FORM FRM_MSG1  TABLES P1 STRUCTURE BAPI_METH_MESSAGE
                       P2 STRUCTURE BAPI_METH_MESSAGE .
  MOVE-CORRESPONDING P2[] TO P1[] .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MSG2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MSG[]  text
*      -->P_LT_RETURN2[]  text
*----------------------------------------------------------------------*
FORM FRM_MSG2  TABLES P1 STRUCTURE BAPI_METH_MESSAGE
                      P2 STRUCTURE BAPIRET2.
  LOOP AT P2.
    P1-MESSAGE_TYPE = P2-TYPE.
    P1-MESSAGE_ID = P2-ID.
    P1-MESSAGE_NUMBER = P2-NUMBER.
    P1-MESSAGE_TEXT = P2-MESSAGE.
    APPEND P1 .
    CLEAR P1 .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MSG3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MSG[]  text
*      -->P_LT_MSG2[]  text
*----------------------------------------------------------------------*
FORM FRM_MSG3  TABLES   P1 STRUCTURE BAPI_METH_MESSAGE
                      P2 STRUCTURE BAPI_STATUS_RESULT.
  LOOP AT P2.
    P1-MESSAGE_TYPE = P2-MESSAGE_TYPE.
    P1-MESSAGE_ID = P2-MESSAGE_ID.
    P1-MESSAGE_NUMBER = P2-MESSAGE_NUMBER.
    P1-MESSAGE_TEXT = P2-MESSAGE_TEXT.
    APPEND P1 .
    CLEAR P1 .
  ENDLOOP.
ENDFORM.
