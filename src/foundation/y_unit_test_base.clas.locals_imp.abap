*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltd_exemption DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_exemption.

ENDCLASS.

CLASS ltd_exemption IMPLEMENTATION.

  METHOD y_if_code_pal_exemption~is_exempt.
    result = abap_false.
  ENDMETHOD.

ENDCLASS.
