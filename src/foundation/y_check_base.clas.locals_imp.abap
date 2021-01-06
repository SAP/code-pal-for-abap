*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_ref_scan_manager_double.
  PUBLIC SECTION.
    METHODS constructor.

ENDCLASS.


CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    inject_code( VALUE #( ( 'REPORT y_example. ' ) ) ).
  ENDMETHOD.

ENDCLASS.



CLASS ltd_check_base DEFINITION INHERITING FROM y_check_base.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.


CLASS ltd_check_base IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ref_scan_manager = NEW ltd_ref_scan_manager( ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


ENDCLASS.
