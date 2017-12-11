*&---------------------------------------------------------------------*
*&  包含                ZXM06U43
*&---------------------------------------------------------------------*


*&--代码添加 BY HANDYBY 18.08.2017 11:59:13  BEGIN
SELECT SINGLE *
  FROM ZZT_ENTH_CONFIG
  INTO @DATA(LW_ZZT_ENTH_CONFIG)
  WHERE ZZENTH_ID = 'SD01'
    AND ZZENTH_KEY1 = @I_EKKO-BUKRS
    AND ZZENTH_ACTIVE = 'X'.

IF SY-SUBRC = 0 .

  IF SY-TCODE <> 'ME22N' AND SY-TCODE  <> 'ME22' AND  SY-TCODE <> 'ME23N' AND SY-TCODE  <> 'ME23'.
    EXIT.
  ENDIF.

  DATA T_NETPR TYPE EKPO-NETPR.
  DATA TT_SUM   TYPE EKBE-BPMNG.
  DATA T_SUM    TYPE EKBE-BPMNG.
  DATA T_SHKZG  TYPE EKBE-SHKZG.

  LOOP AT TEKPO.

    SELECT SINGLE NETPR INTO T_NETPR FROM EKPO WHERE EBELN = TEKPO-EBELN
                                               AND EBELP = TEKPO-EBELP.

    IF TEKPO-NETPR <> T_NETPR.
      SELECT BPMNG SHKZG INTO (T_SUM,T_SHKZG) FROM EKBE WHERE EBELN = TEKPO-EBELN
                                        AND   EBELP = TEKPO-EBELP
                                        AND   BEWTP = 'E'.
        IF T_SHKZG = 'S'.
          TT_SUM = TT_SUM + T_SUM.
        ELSE.
          TT_SUM = TT_SUM - T_SUM.
        ENDIF.

      ENDSELECT.
      IF TT_SUM <> 0.
        MESSAGE E899(MM) WITH TEKPO-EBELN TEKPO-EBELP '由于已收货，不允许修改价格'.
      ENDIF.
    ENDIF.

    CLEAR T_NETPR.
    CLEAR TT_SUM.
    CLEAR T_SUM.
  ENDLOOP.

ENDIF.
*&--代码添加 BY HANDYBY 18.08.2017 11:59:13  END
