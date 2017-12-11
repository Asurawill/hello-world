REPORT ZQQMAT.

  data : l_marc_tab like marc occurs 0 with header line,
         l_found.

* QMATV is not set. At least one inspection type is active
  select * into corresponding fields of table l_marc_tab
    from (            marc as m
           inner join qmat as q
           on m~matnr = q~matnr
           and m~werks = q~werks )
    where m~qmatv = space
    and   q~aktiv = 'X'.

  if sy-subrc eq 0.
    l_found = 'X'.
  endif.

  delete adjacent duplicates from l_marc_tab comparing werks matnr.
  loop at l_marc_tab.
    write : / l_marc_tab-werks, l_marc_tab-matnr.
  endloop.
  skip 1.

  refresh l_marc_tab.

* QMATV is set. No inspection type is active
  select *
     from marc into corresponding fields of table l_marc_tab
     where qmatv ne space
       and not exists ( select * from qmat
                        where matnr eq marc~matnr
                          and werks eq marc~werks
                          and aktiv ne space ).

  if sy-subrc eq 0.
    l_found = 'X'.
  endif.

  loop at l_marc_tab.
    write : / l_marc_tab-werks, l_marc_tab-matnr.
  endloop.

  if l_found eq space.
    write : / 'no inconsistencies found'.
  endif.
