*&---------------------------------------------------------------------*
*& 程序名称: ZPS014
*& 作者    ： 余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    : 项目物资预算实际对比表
*& 开发申请：
*& 变更记录
*&
** 修改日期      开发人员     请求号           描述
*&---------------------------------------------------------------------*

REPORT ZPS014.

************************************************************************
* Includes
************************************************************************
INCLUDE ZPS014_TOP .

INCLUDE ZPS014_SEL .

INCLUDE ZPS014_FRM .

START-OF-SELECTION .
*  PERFORM FRM_CHECK_EMPTY .
  PERFORM FRM_GET_AND_PROCESS_DATA .
  PERFORM FRM_SHOW_ALV .
