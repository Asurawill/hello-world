FUNCTION ZFI_QM.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MANDT) TYPE  MANDT OPTIONAL
*"     VALUE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(I_GJAHR) TYPE  GJAHR OPTIONAL
*"     VALUE(I_DATE) TYPE  ZDATE OPTIONAL
*"  EXPORTING
*"     VALUE(O_FLAG) TYPE  CHAR1
*"     VALUE(O_MSG) TYPE  CHAR0256
*"  TABLES
*"      I_KOSTL STRUCTURE  ZKOSTL
*"      I_KUNNR STRUCTURE  ZKUNNR
*"      I_LIFNR STRUCTURE  ZLIFNR
*"      I_AUFNR STRUCTURE  ZAUFNR
*"      I_VBEL STRUCTURE  ZVBEL
*"      I_HKONT STRUCTURE  ZHKONT
*"      O_DATA STRUCTURE  ZFS001
*"----------------------------------------------------------------------
clear O_DATA.

clear O_DATA[].
DATA: T_ITEM LIKE TABLE OF ZFI010 WITH HEADER LINE .
RANGES:P_BUKRS FOR BSEG-BUKRS.
RANGES:P_GJAHR FOR BSEG-GJAHR.
RANGES:P_MONAT FOR BKPF-MONAT.
RANGES:P_KOSTL FOR BSEG-KOSTL.
RANGES:P_KUNNR FOR BSEG-KUNNR.
RANGES:P_LIFNR FOR BSEG-LIFNR.
RANGES:P_AUFNR FOR BSEG-AUFNR.
RANGES:P_VBEL FOR BSEG-VBEL2.
RANGES:P1_HKONT FOR BSEG-HKONT.
RANGES:P2_HKONT FOR BSEG-HKONT.
"借贷标识   本位币金额借方  本位币金额贷方   外币金额借方  外币金额贷方
DATA:BEGIN OF W_DATA,
     SHKZG LIKE BSEG-SHKZG,
     DMBTRS(16) TYPE P DECIMALS 2 ,
     DMBTRH(16) TYPE P DECIMALS 2 ,
     WRBTRS(16) TYPE P DECIMALS 2 ,
     WRBTRH(16) TYPE P DECIMALS 2 ,
  END OF W_DATA.
DATA: T_DATA LIKE TABLE OF W_DATA WITH HEADER LINE.
   AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID 'BUKRS' FIELD I_BUKRS.
   IF SY-SUBRC <> 0.
    O_FLAG = 'E'.
    CONCATENATE '无权查询' I_BUKRS '公司代码财务数据!' INTO O_MSG.
     EXIT.
   ENDIF.
CHECK O_FLAG NE 'E'.
IF I_KOSTL[] IS NOT INITIAL.
 LOOP AT I_KOSTL.
   P_KOSTL-SIGN = 'I'.
   P_KOSTL-OPTION = 'EQ'.
   P_KOSTL-LOW = I_KOSTL-KOSTL.
   APPEND P_KOSTL.
   ENDLOOP.
ENDIF.
IF I_KUNNR[] IS NOT INITIAL.
  LOOP AT  I_KUNNR.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_KUNNR-KUNNR
    IMPORTING
      OUTPUT = I_KUNNR-KUNNR.
    TRANSLATE I_KUNNR-KUNNR  TO UPPER CASE.
    P_KUNNR-SIGN = 'I'.
    P_KUNNR-OPTION = 'EQ'.
    P_KUNNR-LOW = I_KUNNR-KUNNR.
    APPEND P_KUNNR.
    ENDLOOP.
  ENDIF.

IF I_LIFNR[] IS NOT INITIAL.
   LOOP AT I_LIFNR.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_LIFNR-LIFNR
    IMPORTING
      OUTPUT = I_LIFNR-LIFNR.
     TRANSLATE I_LIFNR-LIFNR  TO UPPER CASE.
     P_LIFNR-SIGN = 'I'.
     P_LIFNR-OPTION = 'EQ'.
     P_LIFNR-LOW = I_LIFNR-LIFNR.
     APPEND P_LIFNR.
     ENDLOOP.
  ENDIF.
IF I_AUFNR[] IS NOT INITIAL.
  LOOP AT I_AUFNR.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_AUFNR-AUFNR
    IMPORTING
      OUTPUT = I_AUFNR-AUFNR.
       TRANSLATE I_AUFNR-AUFNR  TO UPPER CASE.
     P_AUFNR-SIGN = 'I'.
     P_AUFNR-OPTION = 'EQ'.
     P_AUFNR-LOW = I_AUFNR-AUFNR.
     APPEND P_AUFNR.
    ENDLOOP.
  ENDIF.
IF I_VBEL[] IS NOT INITIAL.
   LOOP AT I_VBEL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_VBEL-VBEL
    IMPORTING
      OUTPUT = I_VBEL-VBEL.
       TRANSLATE I_VBEL-VBEL  TO UPPER CASE.
     P_VBEL-SIGN = 'I'.
     P_VBEL-OPTION = 'EQ'.
     P_VBEL-LOW = I_VBEL-VBEL.
     APPEND P_VBEL.
     ENDLOOP.
ENDIF.
DATA:P_CHECK(1)."P判断是否包含通配符
IF I_HKONT[] IS NOT INITIAL.
  LOOP AT I_HKONT.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_HKONT-HKONT
    IMPORTING
      OUTPUT = I_HKONT-HKONT.
   SEARCH I_HKONT FOR '_'.
    IF SY-SUBRC = 0.
       P_CHECK = 1.
       P2_HKONT-SIGN = 'I'.
     P2_HKONT-OPTION = 'EQ'.
      P2_HKONT-LOW = I_HKONT-HKONT.
      APPEND P2_HKONT.
   ENDIF.
   SEARCH I_HKONT FOR '%'.
    IF SY-SUBRC = 0.
       P_CHECK = 1.
       P2_HKONT-SIGN = 'I'.
     P2_HKONT-OPTION = 'EQ'.
      P2_HKONT-LOW = I_HKONT-HKONT.
      APPEND P2_HKONT.
      ENDIF.
    SORT P2_HKONT.
    DELETE ADJACENT DUPLICATES FROM P2_HKONT.
  IF  P_CHECK NE 1.
     P1_HKONT-SIGN = 'I'.
     P1_HKONT-OPTION = 'EQ'.
      P1_HKONT-LOW = I_HKONT-HKONT.
      APPEND P1_HKONT.
   ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR O_DATA .
  CLEAR O_DATA[].
  IF P1_HKONT[] IS NOT INITIAL.
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE T_ITEM
  FROM ZFI012
  WHERE BUKRS = I_BUKRS AND GJAHR = I_GJAHR AND BUDAT <=  I_DATE AND KOSTL IN P_KOSTL AND KUNNR IN P_KUNNR AND LIFNR IN P_LIFNR AND AUFNR IN P_AUFNR AND VBEL2 IN P_VBEL AND
   HKONT IN P1_HKONT.
 ENDIF.
 DATA:P2LEN TYPE I .
IF P2_HKONT[] IS NOT INITIAL.
  P2LEN = LINES( P2_HKONT ).
   LOOP AT P2_HKONT.
      SELECT *  APPENDING  CORRESPONDING FIELDS OF TABLE T_ITEM
     FROM ZFI012
     WHERE BUKRS = I_BUKRS AND GJAHR = I_GJAHR AND BUDAT <=  I_DATE AND KOSTL IN P_KOSTL AND KUNNR IN P_KUNNR AND LIFNR IN P_LIFNR AND AUFNR IN P_AUFNR AND VBEL2 IN P_VBEL AND
     HKONT LIKE P2_HKONT-LOW.
     ENDLOOP.
  ENDIF.
IF P1_HKONT[] IS INITIAL AND P2_HKONT[] IS INITIAL.
      SELECT *  INTO CORRESPONDING FIELDS OF TABLE T_ITEM
  FROM ZFI012
  WHERE BUKRS = I_BUKRS AND GJAHR = I_GJAHR AND BUDAT <=  I_DATE AND KOSTL IN P_KOSTL AND KUNNR IN P_KUNNR AND LIFNR IN P_LIFNR AND AUFNR IN P_AUFNR AND VBEL2 IN P_VBEL .
ENDIF.
*  WHERE BUKRS IN I_BUKRS AND GJAHR IN I_GJAHR
*  AND HKONT IN I_HKONT AND KUNNR IN I_KUNNR AND KOSTL IN I_KOSTL AND AUFNR IN I_AUFNR
*  AND VBEL2 IN I_VBEL.
SORT T_ITEM BY BUKRS GJAHR BUDAT BELNR BUZEI KOSTL KUNNR LIFNR AUFNR VBEL2 HKONT SHKZG.
DELETE ADJACENT DUPLICATES FROM T_ITEM COMPARING BUKRS GJAHR BUDAT BELNR BUZEI KOSTL KUNNR LIFNR AUFNR VBEL2 HKONT SHKZG.
LOOP AT T_ITEM.
   if t_item-waers = t_item-waers_1.
      t_item-WRBTR = 0.
    modify t_item.
   endif.
  CLEAR T_DATA.
  T_DATA-SHKZG = T_ITEM-SHKZG.
  IF T_DATA-SHKZG = 'S'.
     T_DATA-DMBTRS = T_ITEM-DMBTR.
     T_DATA-WRBTRS = T_ITEM-WRBTR.
    COLLECT T_DATA.
    ELSE.
     T_DATA-DMBTRH = T_ITEM-DMBTR.
     T_DATA-WRBTRH = T_ITEM-WRBTR.
     COLLECT T_DATA .
    ENDIF.
  ENDLOOP.
LOOP AT T_DATA.
   O_DATA-DMBTRS = T_DATA-DMBTRS.
   O_DATA-WRBTRS = T_DATA-WRBTRS.
   O_DATA-DMBTRH = T_DATA-DMBTRH.
   O_DATA-WRBTRH = T_DATA-WRBTRH.
  COLLECT O_DATA.
   CLEAR O_DATA.
  ENDLOOP.
ENDFUNCTION.
