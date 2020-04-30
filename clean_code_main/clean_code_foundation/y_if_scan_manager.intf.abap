interface Y_IF_SCAN_MANAGER
  public .


  methods GET_LEVELS
    returning
      value(RESULT) type SLEVEL_TAB .
  methods GET_STATEMENTS
    returning
      value(RESULT) type SSTMNT_TAB .
  methods GET_STRUCTURES
    returning
      value(RESULT) type SSTRUC_TAB .
  methods GET_TOKENS
    returning
      value(RESULT) type STOKESX_TAB .
  methods IS_SCAN_OK
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_REF_SCAN
    importing
      !IO_REF_SCAN type ref to CL_CI_SCAN .
endinterface.
