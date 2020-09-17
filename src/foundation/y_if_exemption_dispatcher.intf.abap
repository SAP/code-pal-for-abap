INTERFACE y_if_exemption_dispatcher
  PUBLIC .

  METHODS is_function_group_exempted
    IMPORTING
      VALUE(name)        TYPE sobj_name
    RETURNING
      VALUE(is_exempted) TYPE abap_bool.

  METHODS is_class_exempted
    IMPORTING
      VALUE(name)        TYPE sobj_name
    RETURNING
      VALUE(is_exempted) TYPE abap_bool.

  METHODS is_program_exempted
    IMPORTING
      VALUE(name)        TYPE sobj_name
    RETURNING
      VALUE(is_exempted) TYPE abap_bool.
ENDINTERFACE.
