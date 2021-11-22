INTERFACE y_if_code_pal_manager PUBLIC.

  TYPES: BEGIN OF check_configuration,
           object_creation_date     TYPE datum,
           threshold                TYPE ycicc_threshold,
           prio                     TYPE ycicc_message_kind,
           apply_on_productive_code TYPE ycicc_productive_code,
           apply_on_testcode        TYPE ycicc_testcode,
           ignore_pseudo_comments   TYPE y_code_pal_pseudo_comments,
         END OF check_configuration.

  TYPES check_configurations TYPE STANDARD TABLE OF check_configuration WITH DEFAULT KEY.

  METHODS read_check_customizing IMPORTING checkid       TYPE seoclsname
                                 RETURNING VALUE(result) TYPE check_configurations
                                 RAISING ycx_code_pal_no_customizing.

  DATA creation_date TYPE REF TO y_if_code_pal_creation_date READ-ONLY.
  DATA database_access TYPE REF TO y_code_pal_database_access READ-ONLY.
  DATA exemption TYPE REF TO y_if_code_pal_exemption READ-ONLY.
  DATA statistics TYPE REF TO y_if_scan_statistics READ-ONLY.

ENDINTERFACE.
