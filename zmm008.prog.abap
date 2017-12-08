*&---------------------------------------------------------------------*
*& Report  ZMM008
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmm008.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .

PARAMETER :rad1 RADIOBUTTON GROUP rad DEFAULT 'X',
           rad2 RADIOBUTTON GROUP rad.

SELECTION-SCREEN END OF BLOCK b1.
**GETTING DEFAULT VARIANT

INITIALIZATION.

AT SELECTION-SCREEN.

**PERFORM DECLARATIONS
START-OF-SELECTION.
  IF rad1 = 'X'.
*    PERFORM fm_call_sm30 USING 'ZMM002_1'.
    SUBMIT zmm008_1 VIA SELECTION-SCREEN AND RETURN.
  ENDIF.

  IF rad2 = 'X'.
*    PERFORM fm_call_sm30 USING 'ZMM002_2'.
    SUBMIT zmm008_2 VIA SELECTION-SCREEN AND RETURN.
  ENDIF.

END-OF-SELECTION.
