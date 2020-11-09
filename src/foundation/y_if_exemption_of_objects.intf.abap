INTERFACE y_if_exemption_of_objects PUBLIC .

  class-METHODS create RETURNING VALUE(result) TYPE REF TO y_if_exemption_of_objects.

  METHODS is_exempted IMPORTING name          TYPE sobj_name
                      RETURNING VALUE(result) TYPE abap_bool .

ENDINTERFACE.
