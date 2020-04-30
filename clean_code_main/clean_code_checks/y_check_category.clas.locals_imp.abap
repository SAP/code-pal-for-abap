*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_check_base_mock DEFINITION INHERITING FROM y_check_base.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
ENDCLASS.

CLASS lcl_check_base_mock IMPLEMENTATION.
  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.
