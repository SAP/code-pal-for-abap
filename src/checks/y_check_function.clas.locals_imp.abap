CLASS ltd_check_function DEFINITION INHERITING FROM y_check_function.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_ref_scan RETURNING VALUE(result) TYPE REF TO cl_ci_scan.
    METHODS set_ref_scan IMPORTING ref_scan TYPE REF TO cl_ci_scan.

    METHODS get_statistics RETURNING VALUE(result) TYPE REF TO y_if_scan_statistics.
    METHODS set_statistics IMPORTING statistics TYPE REF TO y_if_scan_statistics.
ENDCLASS.


CLASS ltd_check_function IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    object_name = 'LTD_CHECK_FUNCTION'.
    object_type = 'CLAS'.
    attributes_ok = abap_true.
    clean_code_manager = NEW y_clean_code_manager_double( me ).
    statistics = NEW y_scan_statistics( ).
  ENDMETHOD.

  METHOD get_ref_scan.
    result = ref_scan.
  ENDMETHOD.

  METHOD set_ref_scan.
    me->ref_scan = ref_scan.
  ENDMETHOD.

  METHOD get_statistics.
    result = statistics.
  ENDMETHOD.

  METHOD set_statistics.
    me->statistics = statistics.
  ENDMETHOD.

ENDCLASS.
