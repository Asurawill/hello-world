*&---------------------------------------------------------------------*
*& 程序名称: ZFI045
*& 作者    ： 余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    : 241物料差异分摊
*& 开发申请：
*& 变更记录
*&
** 修改日期      开发人员     请求号           描述
*&---------------------------------------------------------------------*

REPORT ZFI045.

************************************************************************
* Includes
************************************************************************
INCLUDE ZFI045_TOP .

INCLUDE ZFI045_SEL .

INCLUDE ZFI045_FRM .

START-OF-SELECTION .
  PERFORM FRM_GET_AND_PROCESS_DATA .
  PERFORM FRM_SHOW_ALV .
