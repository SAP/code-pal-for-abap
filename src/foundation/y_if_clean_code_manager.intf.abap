INTERFACE y_if_clean_code_manager
  PUBLIC .


  TYPES:
    BEGIN OF check_configuration,
      object_creation_date     TYPE datum,
      threshold                TYPE ycicc_threshold,
      prio                     TYPE ycicc_message_kind,
      apply_on_productive_code TYPE ycicc_productive_code,
      apply_on_testcode        TYPE ycicc_testcode,
      ignore_pseudo_comments   TYPE y_code_pal_pseudo_comments,
    END OF check_configuration .
  TYPES:
    check_configurations TYPE STANDARD TABLE OF check_configuration WITH DEFAULT KEY .

  METHODS read_check_customizing
    IMPORTING
      checkid       TYPE seoclsname
    RETURNING
      VALUE(result) TYPE check_configurations
    RAISING
      ycx_no_check_customizing .
  METHODS calculate_obj_creation_date
    IMPORTING
      object_name   TYPE sobj_name
      object_type   TYPE trobjtype
    RETURNING
      VALUE(result) TYPE creationdt .
ENDINTERFACE.
