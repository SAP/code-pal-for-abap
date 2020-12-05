interface Y_IF_CLEAN_CODE_MANAGER
  public .


  types:
    BEGIN OF check_configuration,
      object_creation_date     TYPE datum,
      threshold                TYPE ycicc_threshold,
      prio                     TYPE ycicc_message_kind,
      apply_on_productive_code TYPE ycicc_productive_code,
      apply_on_testcode        TYPE ycicc_testcode,
    END OF check_configuration .
  types:
    check_configurations TYPE STANDARD TABLE OF check_configuration WITH DEFAULT KEY .

  methods READ_CHECK_CUSTOMIZING
    importing
      CHECKID type SEOCLSNAME
    returning
      value(RESULT) type CHECK_CONFIGURATIONS
    raising
      YCX_NO_CHECK_CUSTOMIZING .
  methods CALCULATE_OBJ_CREATION_DATE
    importing
      OBJECT_NAME type SOBJ_NAME
      OBJECT_TYPE type TROBJTYPE
    returning
      value(RESULT) type CREATIONDT .
endinterface.
