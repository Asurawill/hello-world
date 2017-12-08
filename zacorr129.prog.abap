*&---------------------------------------------------------------- -----*
*& Report RACORR129
*&
*&---------------------------------------------------------------- -----*
*&
*& Sets XBZDAT in T090NP for given METPER
*&
*&---------------------------------------------------------------- -----*
REPORT RACORR129.
TABLES: T090NP.
*92925 - 'Calculate acc. to asset val. date' IND CANNOT BE MAINTAINED
*VERSION 21 VALIDITY: 2013.01.03 - ACTIVE LANGUAGE ENGLISH
*RELEASED ON 2013.01.03 09:50:35
*RELEASE STATUS RELEASED FOR CUSTOMER
*COMPONENT FI-AA ASSET ACCOUNTING
*PRIORITY RECOMMENDATIONS / ADDITIONAL INFO
*CATEGORY CONSULTING
PARAMETERS: PA_AFAPL LIKE T090NP-AFAPL OBLIGATORY. "Chart of dep.
PARAMETERS: PA_PER LIKE T090NP-METPER OBLIGATORY. "Period control met.

PARAMETERS:G1 TYPE CHAR1.
IF G1 =  ''.
  UPDATE T090NP SET XBZDAT = '' WHERE METPER = PA_PER
                                 AND AFAPL = PA_AFAPL.
ELSE.
  UPDATE T090NP SET XBZDAT = 'X' WHERE METPER = PA_PER
                               AND AFAPL = PA_AFAPL.
ENDIF.
