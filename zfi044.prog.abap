*&---------------------------------------------------------------------*
*& 程序名称:ZFI044
*& 作者    ： 余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    : 项目财务信息表
*& 开发申请：
*& 变更记录
*&
** 修改日期      开发人员     请求号           描述
*&---------------------------------------------------------------------*

REPORT ZFI044.

************************************************************************
* Includes
************************************************************************

*INCLUDE ZFI004_TOP .
INCLUDE ZFI044_TOP .

*INCLUDE ZFI004_SEL .
INCLUDE ZFI044_SEL .

*INCLUDE ZFI004_FRM .
INCLUDE ZFI044_FRM .

AT SELECTION-SCREEN OUTPUT .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1 .
      WHEN 'M21' .
        IF P1 = 'X' .
          SCREEN-ACTIVE = '1' .
        ELSE .
          SCREEN-ACTIVE = '0' .
        ENDIF.
      WHEN 'M31' .
        IF P2 = 'X' .
          SCREEN-ACTIVE = '1' .
        ELSE .
          SCREEN-ACTIVE = '0' .
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN .
  ENDLOOP.

START-OF-SELECTION .
  IF P1 = 'X'.
    PERFORM FRM_CHECK_EMPTY .
    PERFORM FRM_GET_AND_PROCESS_DATA .
    PERFORM FRM_SHOW_ALV .
  ELSEIF P2 = 'X'.

  ENDIF.
