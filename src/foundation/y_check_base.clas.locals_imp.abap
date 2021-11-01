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


CLASS ltd_check_base DEFINITION INHERITING FROM y_check_base.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_ref_scan RETURNING VALUE(result) TYPE REF TO cl_ci_scan.
    METHODS set_ref_scan IMPORTING ref_scan TYPE REF TO cl_ci_scan.

    METHODS get_is_test_code IMPORTING statement TYPE sstmnt
                             RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.


ENDCLASS.


CLASS ltd_check_base IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    ref_scan = y_code_pal_ref_scan_double=>get( VALUE #( ( 'REPORT y_example. ' ) ) ).

    clean_code_manager = NEW y_clean_code_manager_double( me ).
    clean_code_exemption_handler = NEW ltd_clean_code_exemption(  ).
  ENDMETHOD.

  METHOD get_ref_scan.
    result = ref_scan.
  ENDMETHOD.

  METHOD set_ref_scan.
    me->ref_scan = ref_scan.
  ENDMETHOD.

  METHOD get_is_test_code.
    result = is_test_code( statement ).
  ENDMETHOD.

  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.

ENDCLASS.
