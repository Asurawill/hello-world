FUNCTION ZFISEL01.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_VKORG STRUCTURE  ZVKORG
*"      T_AUART STRUCTURE  ZAUART
*"      T_ZXSXX STRUCTURE  ZSSXX
*"----------------------------------------------------------------------
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_VKORG STRUCTURE  ZVKORG
*"      T_AUART STRUCTURE  ZAUART
*"      T_ZXSXX STRUCTURE  ZSSXX
*"----------------------------------------------------------------------
  RANGES:P_VKORG FOR VBAK-VKORG.
  RANGES:P_AUART FOR VBAK-AUART.
  IF T_VKORG[] IS NOT INITIAL.
   LOOP AT T_VKORG.
       TRANSLATE T_VKORG-VKORG  TO UPPER CASE.
     P_VKORG-SIGN = 'I'.
     P_VKORG-OPTION = 'EQ'.
     P_VKORG-LOW = T_VKORG-VKORG.
     APPEND P_VKORG.
     ENDLOOP.
ENDIF.
IF T_AUART[] IS NOT INITIAL.
  LOOP AT T_AUART.
     TRANSLATE T_AUART-AUART TO UPPER CASE.
     P_AUART-SIGN = 'I'.
     P_AUART-OPTION = 'EQ'.
     P_AUART-LOW = T_AUART-AUART.
     APPEND P_AUART.
    ENDLOOP.
  ENDIF.
 SELECT * INTO CORRESPONDING FIELDS OF TABLE T_ZXSXX
    FROM ZFIVBELN
   WHERE VKORG IN P_VKORG  AND AUART IN P_AUART.
   IF T_ZXSXX[] IS NOT INITIAL.
  DATA L_TDNAME TYPE THEAD-TDNAME.
  DATA T_TLINE TYPE TABLE OF TLINE WITH HEADER LINE.
  LOOP AT T_ZXSXX.
  L_TDNAME = T_ZXSXX-VBELN.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      ID                            = 'Z001'
      LANGUAGE                      = SY-LANGU
      NAME                          = L_TDNAME
      OBJECT                        = 'VBBK'
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
*     OLD_LINE_COUNTER              =
    TABLES
      LINES                         = T_TLINE[]
    EXCEPTIONS
      ID                            = 1
      LANGUAGE                      = 2
      NAME                          = 3
      NOT_FOUND                     = 4
      OBJECT                        = 5
      REFERENCE_CHECK               = 6
      WRONG_ACCESS_TO_ARCHIVE       = 7
      OTHERS                        = 8
            .
     IF T_TLINE[] IS NOT INITIAL.
       LOOP AT T_TLINE.
        CONCATENATE  T_ZXSXX-XMMC T_TLINE-TDLINE INTO T_ZXSXX-XMMC.
        MODIFY T_ZXSXX.
         ENDLOOP..
       ENDIF.
   ENDLOOP.
    ENDIF.

IF T_ZXSXX[] IS NOT  INITIAL.
 R_MSG = '据读取成功,请查看T_XSXX表'.
 ELSE.
 R_MSG =  '数据读取失败,请重新更新检索条件' .
  ENDIF.




ENDFUNCTION.
