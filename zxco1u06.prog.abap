*&---------------------------------------------------------------------*
*&  包含                ZXCO1U06
*&---------------------------------------------------------------------*

IF SY-TCODE = 'CO01' OR SY-TCODE = 'CO41' OR SY-TCODE = 'CO40' OR SY-TCODE = 'MD04' OR SY-TCODE = 'MD07' OR SY-TCODE = 'CO08' .
  IF ( HEADER_IMP-AUART = 'ZP03'
    OR HEADER_IMP-AUART ='ZP07'
    OR HEADER_IMP-AUART ='ZP08'
    OR HEADER_IMP-AUART ='ZP09')
    AND  HEADER_IMP-WERKS = '1500' .
    MESSAGE E008(ZPP01).
  ENDIF.
ENDIF.

IF SY-TCODE = 'CO07' .
  IF ( HEADER_IMP-AUART = 'ZP01'
    OR HEADER_IMP-AUART ='ZP02'
    OR HEADER_IMP-AUART ='ZP04'
    OR HEADER_IMP-AUART ='ZP05'
    OR HEADER_IMP-AUART ='ZP06')
    AND  HEADER_IMP-WERKS = '1500' .
    MESSAGE E009(ZPP01).
  ENDIF.
ENDIF.
