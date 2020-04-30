INTERFACE lif_db_reader.
  METHODS is_fm_rfc_enabled
    IMPORTING function      TYPE string
    RETURNING VALUE(result) TYPE fmode.
ENDINTERFACE.

CLASS lcl_db_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_db_reader.
ENDCLASS.
