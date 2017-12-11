PROGRAM zrggbr000 .
*---------------------------------------------------------------------*
*                                                                     *
*   Regeln: EXIT-Formpool for Uxxx-Exits                              *
*                                                                     *
*   This formpool is used by SAP for demonstration purposes only.     *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE fgbbgd00.               "Data types


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
*TYPE-POOLS: GB002. " TO BE INCLUDED IN
*TABLES: BKPF,      " ANY SYSTEM THAT
*        BSEG,      " HAS 'FI' INSTALLED
*        COBL,
*        GLU1.
*ENHANCEMENT-POINT RGGBR000_01 SPOTS ES_RGGBR000 STATIC.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*


*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM get_exit_titles TABLES etab.

  DATA: BEGIN OF exits OCCURS 50,
          name(5)   TYPE c,
          param     LIKE c_exit_param_none,
          title(60) TYPE c,
        END OF exits.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  EXITS-NAME  = 'U101'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-100.                 "Posting date check
*  APPEND EXITS.

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.        "Complete data used in exit.
  exits-title = text-101.                 "Posting date check
  APPEND exits.

* forms for SAP_EIS
  exits-name  = 'US001'.                  "single validation: only one
  exits-param = c_exit_param_none.        "data record used
  exits-title = text-102.                 "Example EIS
  APPEND exits.

  exits-name  = 'UM001'.                  "matrix validation:
  exits-param = c_exit_param_class.       "complete data used in exit.
  exits-title = text-103.                 "Example EIS
  APPEND exits.

  exits-name  = 'UZ001'.                  "matrix validation:
  exits-param = c_exit_param_class.       "complete data used in exit.
  exits-title = '关联交易检查'.                 "Example EIS
  APPEND exits.

***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
  INCLUDE rggbr_ps_titles.

***********************************************************************
** EXIT EXAMPLES FROM Argentina Legal Change - Law Res 177
***********************************************************************
INCLUDE RGGBS_AR_TITLES.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.                    "GET_EXIT_TITLES

*eject
*----------------------------------------------------------------------*
*       FORM U100                                                      *
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule                          *
*       This exit can be used in FI for callup points 1,2 or 3.        *
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM u100  USING b_result.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*
*   IF SY-DATUM = BKPF-BUDAT.
*     B_RESULT  = B_TRUE.
*  ELSE.
*    B_RESULT  = B_FALSE.
*  ENDIF.

*ENHANCEMENT-POINT RGGBR000_02 SPOTS ES_RGGBR000 STATIC.

*ENHANCEMENT-POINT RGGBR000_03 SPOTS ES_RGGBR000.


ENDFORM.                                                    "U100

*eject
*----------------------------------------------------------------------*
*       FORM U101                                                      *
*----------------------------------------------------------------------*
*       Example of an exit using the complete data from one            *
*       multi-line rule.                                               *
*       This exit is intended for use from callup point 3, in FI.      *
*                                                                      *
*       If account 400000 is used, then account 399999 must be posted  *
*       to in another posting line.                                    *
*----------------------------------------------------------------------*
*  -->  BOOL_DATA   The complete posting data.                         *
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u101 USING    bool_data TYPE gb002_015
*          CHANGING B_RESULT.
*  DATA: B_ACC_400000_USED LIKE D_BOOL VALUE 'F'.
*
*  B_RESULT = B_TRUE.
** Has account 400000 has been used?
*  LOOP AT BOOL_DATA-BSEG INTO BSEG
*                 WHERE HKONT  = '0000400000'.
*     B_ACC_400000_USED = B_TRUE.
*     EXIT.
*  ENDLOOP.
*
** Check that account 400000 has been used.
*  CHECK B_ACC_400000_USED = B_TRUE.
*
*  B_RESULT = B_FALSE.
*  LOOP AT BOOL_DATA-BSEG INTO BSEG
*                 WHERE HKONT  = '0000399999'.
*     B_RESULT = B_TRUE.
*     EXIT.
* ENDLOOP.
*
*ENDFORM.

*eject
*----------------------------------------------------------------------*
*       FORM US001
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule in SAP-EIS
*       for aspect 001 (single validation).
*       one data record is transfered in structure CF<asspect>
*----------------------------------------------------------------------
*       Attention: for any FORM one has to make an entry in the
*       form GET_EXIT_TITLES at the beginning of this include
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM us001 USING b_result.

*TABLES CF001.                                 "table name aspect 001
*
*  IF ( CF001-SPART = '00000001' OR
*       CF001-GEBIE = '00000001' ) AND
*       CF001-ERLOS >= '1000000'.
*
**   further checks ...
*
*    B_RESULT  = B_TRUE.
*  ELSE.
*
**   further checks ...
*
*    B_RESULT  = B_FALSE.
*  ENDIF.

ENDFORM.                                                    "US001

*eject
*----------------------------------------------------------------------*
*       FORM UM001
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule in SAP-EIS
*       for aspect 001 (matrix validation).
*       Data is transfered in BOOL_DATA:
*       BOOL_DATA-CF<aspect> is intern table of structure CF<asspect>
*----------------------------------------------------------------------
*       Attention: for any FORM one has to make an entry in the
*       form GET_EXIT_TITLES at the beginning of this include
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM um001 USING bool_data    "TYPE GB002_<boolean class of aspect 001>
           CHANGING b_result.

*DATA: LC_CF001 LIKE CF001.
*DATA: LC_COUNT TYPE I.

*  B_RESULT = B_TRUE.
*  CLEAR LC_COUNT.
*  process data records in BOOL_DATA
*  LOOP AT BOOL_DATA-CF001 INTO LC_CF001.
*    IF LC_CF001-SPART = '00000001'.
*      ADD 1 TO LC_COUNT.
*      IF LC_COUNT >= 2.
**       division '00000001' may only occur once !
*        B_RESULT = B_FALSE.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
**   further checks ....
*
*  ENDLOOP.

ENDFORM.                                                    "UM001
*----------------UZ001---------------------begin add  it02 150922
FORM UZ001 USING    bool_data TYPE gb002_015
          CHANGING B_RESULT.
  DATA: B_ACC LIKE D_BOOL VALUE 'F'."默认值设为F （假）
  DATA: B_VBUND  LIKE D_BOOL VALUE 'F'."默认值设为F （假）
  DATA: W_BSEG  TYPE BSEG .
  RANGES R_HKONT5 FOR BSEG-HKONT .
  R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'BT'.
  R_HKONT5-LOW = '1001000000'."现金科目
  R_HKONT5-HIGH = '1001999999'.
  APPEND R_HKONT5 .
  R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'BT'.
  R_HKONT5-LOW = '1002000000'. "银行科目科目
  R_HKONT5-HIGH = '1002999999'.
  APPEND R_HKONT5 .
    R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'BT'.
  R_HKONT5-LOW = '1012000000'. "其他货币资金科目
  R_HKONT5-HIGH = '1012999999'.
  APPEND R_HKONT5 .
  R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'BT'.
  R_HKONT5-LOW = '6001000000'. "主营业务收入科目
  R_HKONT5-HIGH = '6001999999'.
  APPEND R_HKONT5 .
  R_HKONT5-SIGN = 'I'.
  R_HKONT5-OPTION = 'BT'.
  R_HKONT5-LOW = '6051000000'. "其他业务收入科目
  R_HKONT5-HIGH = '6051999999'.
  APPEND R_HKONT5 .
  B_RESULT = B_TRUE.
* Has account 400000 has been used?
  CLEAR: B_ACC ,B_VBUND .
  "(1)先检查凭证行项目中关联方往来是否有 涉及银行 、其他货币、主营业务收入、业务收入往来科目
  LOOP AT BOOL_DATA-BSEG  INTO W_BSEG
                 WHERE HKONT IN R_HKONT5.
    IF W_BSEG-RSTGR EQ ''.
        B_ACC = B_TRUE.
        EXIT.
     ELSE.
       IF W_BSEG-RSTGR =  'A02' OR W_BSEG-RSTGR =  'B02' OR W_BSEG-RSTGR ='B03' .
         CONTINUE.
        ELSE.
          B_ACC = B_TRUE.
        EXIT.
       ENDIF.
     ENDIF.
  ENDLOOP.
   CHECK B_ACC =  B_TRUE .
  "(2):检查1通过 再检查贸易伙伴是否不为空且不为9999
  LOOP AT BOOL_DATA-BSEG  INTO W_BSEG
                 WHERE VBUND NE '' AND  VBUND NE '999999'.
      B_VBUND = B_TRUE.
     EXIT.

  ENDLOOP.
"(3):检查（2）是否贸易伙伴
 CHECK B_VBUND = B_TRUE .
* Check that account 400000 has been used.
 " CHECK B_ACC_400000_USED = B_TRUE.
"(4):(3)OK ,返回B_RESULT值为 F.
  B_RESULT = B_FALSE.

*
ENDFORM.
*----------------UZ001---------------------end add  it02 150922
***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
*INCLUDE rggbr_ps_forms.

***********************************************************************
** EXIT EXAMPLES FROM Argentina Legal Change - Law Res 177
***********************************************************************
INCLUDE RGGBS_AR_FORMS.
