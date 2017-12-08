* regenerated at 02.03.2015 14:38:16
FUNCTION-POOL ZFGZMM008_2                MESSAGE-ID SV.

* INCLUDE LZFGZMM008_2D...                   " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZFGZMM008_2T00                         . "view rel. data dcl.
    data:g_dbdh TYPE zdbdh  .
    data:g_maxdbdh TYPE zdbdh  .
  ranges gr_timestamp for ZMM002_2-TIMESTAMP.
  data:gt_zmm002_2 TYPE ZMM002_2 OCCURS 0 WITH HEADER LINE.
