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
    ref_scan_manager->set_ref_scan( VALUE #( ) ).
    clean_code_manager = NEW y_clean_code_manager_double( me ).
    clean_code_exemption_handler = NEW ltd_clean_code_exemption(  ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


ENDCLASS.
