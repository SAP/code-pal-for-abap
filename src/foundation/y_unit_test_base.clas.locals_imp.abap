*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltd_clean_code_exemption DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_exemption.

ENDCLASS.

CLASS ltd_clean_code_exemption IMPLEMENTATION.

  METHOD y_if_exemption~is_object_exempted.
    RETURN.
  ENDMETHOD.

ENDCLASS.
