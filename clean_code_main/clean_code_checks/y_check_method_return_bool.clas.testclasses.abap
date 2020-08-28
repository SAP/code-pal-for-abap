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
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS is_active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS has_entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS are_oficial RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS try_read RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS can_select RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS have_file RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS must_validate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS starts_vowel RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS ends_dot RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS should_concatenate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS exist_entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS contain_value RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS increment_one CHANGING integer TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD is_active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD has_entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD are_oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD try_read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD can_select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD have_file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD must_validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD starts_vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD ends_dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD should_concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD exist_entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD contain_value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
     inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS active RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS oficial RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS read RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS select RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS file RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS validate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS vowel RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS dot RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS concatenate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS value RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS increment_one CHANGING integer TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
     inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS active RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS entry RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS oficial RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS read RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS select RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS file RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS validate RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS vowel RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS dot RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS concatenate RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL' )
      ( '     METHODS value RETURNING VALUE(result) TYPE abap_bool. ' )
      ( '     METHODS sum IMPORTING a TYPE i b TYPE i RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS increment_one CHANGING integer TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD active. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD entry. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD oficial. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD read. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD select. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD file. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD validate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD vowel. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD dot. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD concatenate. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD value. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD sum. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD increment_one. ' )
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
    DATA: cut                     TYPE REF TO y_check_method_return_bool,
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

CLASS y_check_method_return_bool DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_method_return_bool( ).
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
    assert_errors( 11 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 7 ).
    assert_pseudo_comments( 4 ).
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
