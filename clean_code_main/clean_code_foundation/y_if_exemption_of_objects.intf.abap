interface Y_IF_EXEMPTION_OF_OBJECTS
  public .


  methods IS_EXEMPTED
    importing
      !NAME type SOBJ_NAME
    returning
      value(RESULT) type ABAP_BOOL .
endinterface.
