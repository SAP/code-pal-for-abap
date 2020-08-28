CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 0 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '20190101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_scan_manager_double FOR TESTING.
  PUBLIC SECTION.
    METHODS set_data_for_ok.
    METHODS set_data_for_error.
    METHODS set_pseudo_comment_ok.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD set_data_for_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS meth1. ' )
      ( '     CONSTANTS cons1 VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth2. ' )
      ( '     DATA data1. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS meth3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD meth1. ' )
      ( '     meth2( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth2. ' )
      ( '     meth3( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth3. ' )
      ( '     data1 = cons1. ' )
      ( '     DATA(data1) = data1. ' )
      ( '     DATA(data2) = cons1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS meth1. ' )
      ( '     CONSTANTS cons1 VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth2. ' )
      ( '     DATA data1. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS meth3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD meth1. ' )
      ( '     me->meth2( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth2. ' )
      ( '     me->meth3( ). ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth3. ' )
      ( '     me->data1 = me->cons1. ' )
      ( '     DATA(data1) = me->data1. ' )
      ( '     DATA(data2) = me->cons1. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS meth1. ' )
      ( '     CONSTANTS cons1 VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth2. ' )
      ( '     DATA data1. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS meth3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD meth1. ' )
      ( '     me->meth2( ). "#EC SELF_REF ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth2. ' )
      ( '     me->meth3( ). "#EC SELF_REF ' )
      ( '   ENDMETHOD. ' )

      ( '   METHOD meth3. ' )
      ( '     me->data1 = me->cons1. "#EC SELF_REF ' )
      ( '     DATA(data1) = me->data1. "#EC SELF_REF ' )
      ( '     DATA(data2) = me->cons1. "#EC SELF_REF ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.


ENDCLASS.

CLASS ltd_clean_code_exemption_no DEFINITION FOR TESTING
  INHERITING FROM y_exemption_handler.

  PUBLIC SECTION.
    METHODS: is_object_exempted REDEFINITION.
ENDCLASS.

CLASS ltd_clean_code_exemption_no IMPLEMENTATION.
  METHOD is_object_exempted.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS local_test_class DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  PROTECTED SECTION.
    METHODS is_bound FOR TESTING.
    METHODS cut_error FOR TESTING.
    METHODS cut_ok FOR TESTING.
    METHODS pseudo_comment_ok FOR TESTING.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_check_self_reference.
    DATA ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.
    METHODS setup.
    METHODS assert_errors IMPORTING err_cnt TYPE i.
    METHODS assert_pseudo_comments IMPORTING pc_cnt TYPE i.
ENDCLASS.

CLASS y_check_self_reference DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_self_reference( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW ltd_clean_code_manager( ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( act = cut ).
  ENDMETHOD.

  METHOD cut_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cut_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 5 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 5 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_errors( )
                                        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_pseudo_comments( )
                                        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
