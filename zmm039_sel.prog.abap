*&---------------------------------------------------------------------*
*&  包含                ZMM039_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_file LIKE rlgrap-filename. "物料导入摸版
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN COMMENT 01(20) TEXT-h01.
SELECTION-SCREEN: FUNCTION KEY 1.

*SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-003.
*PARAMETERS: p_new  RADIOBUTTON GROUP g2."新增
*PARAMETERS: p_modify  RADIOBUTTON GROUP g2."修改
*
*SELECTION-SCREEN END OF BLOCK blk2.
