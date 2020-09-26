INTERFACE lif_db_reader.
  METHODS is_fm_rfc_enabled
    IMPORTING function      TYPE char30
    RETURNING VALUE(result) TYPE abap_bool.
ENDINTERFACE.

CLASS lcl_db_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_db_reader.
ENDCLASS.
