  DATA: L_FELD(40) TYPE C VALUE '(SAPMQEVA)G_CHARGE_FIXIERT'.
  FIELD-SYMBOLS: <FS>.
  ASSIGN (L_FELD) TO <FS>.
  IF I_TCODE <> 'QA13'.
    IF     (   I_QALS-HERKUNFT EQ '03'
       OR      I_QALS-HERKUNFT EQ '13'  )
       AND NOT I_QALS-XCHPF    IS INITIAL
       AND     I_QALS-CHARG    IS INITIAL
       AND (   I_QALS-STAT02   IS INITIAL
             OR (     NOT I_QALS-STAT02   IS INITIAL
                  AND     I_QALS-PPKZTLZU CA '13'    ) ).
      <FS> = 'X'.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&  包含                ZXQEVU07
*&---------------------------------------------------------------------*
