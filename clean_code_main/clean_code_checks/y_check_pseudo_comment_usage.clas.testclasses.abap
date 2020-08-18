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

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_error.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.
  METHOD set_data_for_ok.
    convert_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA value TYPE c.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    convert_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA value TYPE c. "#EC MOCK_PCOM' )
    ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_exemption_no DEFINITION FOR TESTING INHERITING FROM y_exemption_handler.
  PUBLIC SECTION.
    METHODS: is_object_exempted REDEFINITION.
ENDCLASS.

CLASS ltd_clean_code_exemption_no IMPLEMENTATION.
  METHOD is_object_exempted.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mock_check_pcom DEFINITION FOR TESTING INHERITING FROM y_check_base.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_mock_check_pcom IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    settings-pseudo_comment = '"#EC MOCK_PCOM' ##NO_TEXT.
  ENDMETHOD.
  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mock_check_empty DEFINITION FOR TESTING INHERITING FROM y_check_base.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_mock_check_empty IMPLEMENTATION.
  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mock_pseudo_comment_usage DEFINITION FOR TESTING INHERITING FROM y_check_pseudo_comment_usage.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS select_object_list REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_mock_pseudo_comment_usage IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD select_object_list.
    APPEND 'LCL_MOCK_CHECK_PCOM' TO result.
    APPEND 'LCL_MOCK_CHECK_EMPTY' TO result.
    APPEND INITIAL LINE TO result.
  ENDMETHOD.
ENDCLASS.


CLASS local_test_class DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_pseudo_comment_usage,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      is_bound FOR TESTING,
      cut_ok FOR TESTING,
      cut_error FOR TESTING.
ENDCLASS.

CLASS y_check_pseudo_comment_usage DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW lcl_mock_pseudo_comment_usage( ).
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

  METHOD cut_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
  ENDMETHOD.

  METHOD cut_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 1 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_errors( )
        exp = err_cnt ).
  ENDMETHOD.
ENDCLASS.
