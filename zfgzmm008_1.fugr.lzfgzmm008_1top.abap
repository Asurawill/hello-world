* regenerated at 28.02.2015 13:33:04
FUNCTION-POOL ZFGZMM008_1                MESSAGE-ID SV.

* INCLUDE LZFGZMM008_1D...                   " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZFGZMM008_1T00                         . "view rel. data dcl.

  data:g_dbdh TYPE zdbdh  .
  data:g_maxdbdh TYPE zdbdh  .
  ranges gr_timestamp for ZMM002_1-TIMESTAMP.
  data:gt_zmm002_1 TYPE ZMM002_1 OCCURS 0 WITH HEADER LINE.
