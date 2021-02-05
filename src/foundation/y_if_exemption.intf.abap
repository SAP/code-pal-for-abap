INTERFACE y_if_exemption PUBLIC .

  CLASS-METHODS is_object_exempted IMPORTING object_type   TYPE trobjtype
                                             object_name   TYPE sobj_name
                                   RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.
