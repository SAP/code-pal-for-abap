INTERFACE y_if_exemption_of_object PUBLIC.

  CLASS-METHODS is_exempted IMPORTING object_name   TYPE sobj_name
                            RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.
