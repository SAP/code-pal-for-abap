CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'N' threshold = 0 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 0 ) ).
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
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS cx_demo DEFINITION INHERITING FROM cx_static_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     EVENTS event. ' )
      ( '     METHODS one RAISING cx_demo. ' )
      ( '     METHODS two RAISING cx_demo. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD one. ' )
      ( '     RAISE EXCEPTION TYPE cx_demo. ' )
      ( '     RAISE RESUMABLE EXCEPTION TYPE cx_demo. ' )
      ( '     RAISE SHORTDUMP TYPE cx_demo. ' )
      ( '     RAISE EVENT event. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD two. ' )
      ( '     DATA cflag TYPE abap_bool. ' )
      ( '     DATA(iflag) = COND i( WHEN cflag = abap_true  THEN 1 ' )
      ( '                           WHEN cflag = abap_false THEN 0 ' )
      ( |                           ELSE THROW cx_demo_dyn_t100( MESSAGE e888(sabapdemos) WITH 'Illegal value!' ) ). | )
      ( '   ENDMETHOD.')
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS one EXCEPTIONS exception. ' )
      ( '     METHODS two EXCEPTIONS exception. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD one. ' )
      ( '     RAISE exception. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD two. ' )
      ( |     MESSAGE 'test' TYPE 'I' RAISING exception.| )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-METHODS one EXCEPTIONS exception. ' )
      ( '     METHODS two EXCEPTIONS exception. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD one. ' )
      ( '     RAISE exception. "#EC NON_CL_EXCEPT' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD two. ' )
      ( |     MESSAGE 'test' TYPE 'I' RAISING exception. "#EC NON_CL_EXCEPT| )
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

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_non_class_exception,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      check_ok FOR TESTING,
      check_error FOR TESTING,
      check_pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_non_class_exception DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_non_class_exception( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW ltd_clean_code_manager( ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD check_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 2 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 2 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_errors( )
        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_pseudo_comments( )
        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
