*----------------------------------------------------------------------*
***INCLUDE ZMM008_2_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATUM_TO_TIMESTAMPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<WA_ZMM002_2>_TIMESTAMP  text
*----------------------------------------------------------------------*
FORM convert_datum_to_timestamps  CHANGING p_timestamp.
   DATA: p_date      TYPE d,
         p_time      TYPE t,
         l_timezone  LIKE ttzz-tzone,
         l_timestamp TYPE timestamp.
   p_date = sy-datum.
   p_time = sy-uzeit.
   p_time = p_time.
   CONVERT DATE p_date
           TIME p_time
           INTO
           TIME STAMP l_timestamp  TIME ZONE l_timezone.
   p_timestamp = l_timestamp.
ENDFORM.
