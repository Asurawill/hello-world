FUNCTION ZFI_SAKAN.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     VALUE(R_MSG) TYPE  CHAR0256
*"  TABLES
*"      T_SAKNR STRUCTURE  ZSAKAN_INFO OPTIONAL
*"----------------------------------------------------------------------
DATA: T_SKAT LIKE TABLE OF SKAT WITH  HEADER LINE.

DATA:T_T077Z LIKE TABLE OF T077Z WITH HEADER LINE.

SELECT A~BUKRS A~SAKNR  A~WAERS B~KTOKS
  INTO CORRESPONDING FIELDS OF TABLE T_SAKNR
  FROM SKB1 AS A
  INNER JOIN SKA1 AS B
  ON A~SAKNR = B~SAKNR
  WHERE A~BUKRS = P_BUKRS AND B~KTOPL = '1000' .
  IF T_SAKNR[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE T_SKAT
      FROM SKAT
      FOR ALL ENTRIES IN T_SAKNR
      WHERE SAKNR = T_SAKNR-SAKNR.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE T_T077Z
        FROM T077Z
        FOR ALL ENTRIES IN T_SAKNR
        WHERE   KTOKS = T_SAKNR-KTOKS.
     LOOP AT T_SAKNR.
        READ TABLE T_SKAT WITH KEY SAKNR = T_SAKNR-SAKNR.
          IF SY-SUBRC = 0.
             T_SAKNR-TXT50 = T_SKAT-TXT50.
            ENDIF.
         READ TABLE T_T077Z WITH KEY KTOKS = T_SAKNR-KTOKS.
          IF SY-SUBRC = 0.
             T_SAKNR-TXT30 = T_T077Z-TXT30.
            ENDIF.
          MODIFY T_SAKNR.
       ENDLOOP.
     R_MSG = '据读取成功,请查看T_SAKAN表'.
 ELSE.
 R_MSG =  '数据读取失败,请重新更新检索条件' .
    ENDIF.
ENDFUNCTION.