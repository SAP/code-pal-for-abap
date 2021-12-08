*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



CLASS ltd_check_base DEFINITION INHERITING FROM y_code_pal_base.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_ref_scan RETURNING VALUE(result) TYPE REF TO cl_ci_scan.
    METHODS set_ref_scan IMPORTING ref_scan TYPE REF TO cl_ci_scan.

    METHODS get_is_test_code IMPORTING statement TYPE sstmnt
                             RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.


CLASS ltd_check_base IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    ref_scan = y_code_pal_ref_scan_double=>get( VALUE #( ( 'REPORT y_example. ' ) ) ).
    manager = NEW y_code_pal_manager_double( me ).
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

  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
