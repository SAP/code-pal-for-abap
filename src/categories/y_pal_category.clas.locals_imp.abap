*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_code_pal_base DEFINITION INHERITING FROM y_code_pal_base.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.

CLASS lcl_code_pal_base IMPLEMENTATION.

  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.

ENDCLASS.
