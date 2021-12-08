CLASS ltd_check_function DEFINITION INHERITING FROM y_pal_function.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_ref_scan RETURNING VALUE(result) TYPE REF TO cl_ci_scan.
    METHODS set_ref_scan IMPORTING ref_scan TYPE REF TO cl_ci_scan.
ENDCLASS.


CLASS ltd_check_function IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    object_name = 'LTD_CHECK_FUNCTION'.
    object_type = 'CLAS'.
    has_attributes = abap_false.
    attributes_ok = abap_true.
    manager = NEW y_code_pal_manager_double( me ).
  ENDMETHOD.

  METHOD get_ref_scan.
    result = ref_scan.
  ENDMETHOD.

  METHOD set_ref_scan.
    me->ref_scan = ref_scan.
  ENDMETHOD.

ENDCLASS.
