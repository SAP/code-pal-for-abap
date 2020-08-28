CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_ref_scan_manager_double FOR TESTING.
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
      ( '     METHODS set_commit IMPORTING do_commit TYPE abap_bool. ' )
      ( '     METHODS update_without_commit. ' )
      ( '     METHODS update_and_commit. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS delete_without_commit. ' )
      ( '     METHODS datele_and_commit. ' )
      ( '     METHODS datele_child_without_commit. ' )
      ( '     METHODS datele_child_and_commit. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD set_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD update_without_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD update_and_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD delete_without_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD datele_and_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD datele_child_without_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD datele_child_and_commit. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS set_commit IMPORTING do_commit TYPE abap_bool. ' )
      ( '     METHODS update IMPORTING do_commit TYPE abap_bool. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS delete IMPORTING do_commit TYPE abap_bool ' )
      ( '                              do_child TYPE abap_bool. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD set_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD update. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD delete. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS set_commit IMPORTING do_commit TYPE abap_bool. ' )
      ( '     METHODS update IMPORTING do_commit TYPE abap_bool. "#EC BOOL_PARAM ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS delete IMPORTING do_commit TYPE abap_bool ' )
      ( '                              do_child TYPE abap_bool. "#EC BOOL_PARAM ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD set_commit. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD update. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD delete. ' )
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
    DATA cut TYPE REF TO y_check_boolean_input_param.
    DATA ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.
    METHODS setup.
    METHODS assert_errors IMPORTING err_cnt TYPE i.
    METHODS assert_pseudo_comments IMPORTING pc_cnt TYPE i.
ENDCLASS.

CLASS y_check_boolean_input_param DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_boolean_input_param( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW y_clean_code_manager_double( cut ).
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
    assert_errors( 2 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 2 ).
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
