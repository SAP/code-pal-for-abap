CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 0 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '20190101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_test_data_ok_case DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_if_ok,
      set_data_for_if_0_ok,
      set_data_for_do_ok,
      set_data_for_check_ok,
      set_data_for_case_subrc_ok,
      set_data_for_check_lines_ok.
ENDCLASS.

CLASS ltd_test_data_ok_case IMPLEMENTATION.

  METHOD set_data_for_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 0.' )
    ( 'val = 0.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_if_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'IF sy-subrc NE 0.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_if_0_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 0.' )
    ( 'IF val > 0.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_do_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 10.' )
    ( 'DO val TIMES.' )
    ( 'ENDDO.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_check_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 0.' )
    ( 'CHECK sy-subrc = val.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_case_subrc_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'CASE sy-subrc.' )
    ( ' WHEN 4.' )
    ( ' WHEN OTHERS.' )
    ( 'ENDCASE.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_check_lines_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 1.' )
    ( 'CHECK val > 1.' )
    ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_test_data_error_case DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_if_error,
      set_data_for_do_error,
      set_data_for_check_error,
      set_pseudo_comment_ok.
ENDCLASS.

CLASS ltd_test_data_error_case IMPLEMENTATION.

  METHOD set_data_for_if_error.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 1.' )
    ( 'IF val EQ 8.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_do_error.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DO 5 TIMES.' )
    ( 'ENDDO.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_check_error.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 0.' )
    ( 'CHECK val = 23.' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )

    ( 'DATA val TYPE i VALUE 1.' )
    ( 'IF val EQ 8. "#EC CI_MAGIC' )
    ( 'ENDIF.' )
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

CLASS lth_assertion DEFINITION FOR TESTING.
  PUBLIC SECTION.
    METHODS:
      assert_errors
        IMPORTING
          act TYPE any
          exp TYPE any,
      assert_pseudo_comments
        IMPORTING
          act TYPE any
          exp TYPE any.
ENDCLASS.

CLASS lth_assertion IMPLEMENTATION.
  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act
        exp = exp ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act
        exp = exp ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_magic_number_ok DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_magic_number,
          ref_scan_manager_double TYPE REF TO ltd_test_data_ok_case,
          assertion               TYPE REF TO lth_assertion.

    METHODS:
      setup,
      is_bound FOR TESTING,
      magic_number_ok FOR TESTING,
      magic_number_in_if_ok FOR TESTING,
      magic_number_in_if_0_ok FOR TESTING,
      magic_number_in_do_ok FOR TESTING,
      magic_number_in_check_ok FOR TESTING,
      magic_number_in_case_subrc_ok FOR TESTING,
      magic_number_in_check_lines_ok FOR TESTING.
ENDCLASS.

CLASS y_check_magic_number DEFINITION LOCAL FRIENDS ltc_magic_number_ok.

CLASS ltc_magic_number_ok IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_magic_number( ).
    ref_scan_manager_double = NEW ltd_test_data_ok_case( ).
    assertion = NEW lth_assertion( ).
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

  METHOD magic_number_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_if_ok.
    ref_scan_manager_double->set_data_for_if_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_if_0_ok.
    ref_scan_manager_double->set_data_for_if_0_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_do_ok.
    ref_scan_manager_double->set_data_for_do_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_check_ok.
    ref_scan_manager_double->set_data_for_check_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_case_subrc_ok.
    ref_scan_manager_double->set_data_for_case_subrc_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_check_lines_ok.
    ref_scan_manager_double->set_data_for_check_lines_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_magic_number_error DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_magic_number,
          ref_scan_manager_double TYPE REF TO ltd_test_data_error_case,
          assertion               TYPE REF TO lth_assertion.
    METHODS:
      setup,
      is_bound FOR TESTING,
      magic_number_in_if_error FOR TESTING,
      magic_number_in_do_error FOR TESTING,
      magic_number_in_check_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_magic_number DEFINITION LOCAL FRIENDS ltc_magic_number_error.

CLASS ltc_magic_number_error IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_magic_number( ).
    ref_scan_manager_double = NEW ltd_test_data_error_case( ).
    assertion = NEW lth_assertion( ).
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

  METHOD magic_number_in_if_error.
    ref_scan_manager_double->set_data_for_if_error( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 1 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_do_error.
    ref_scan_manager_double->set_data_for_do_error( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 1 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD magic_number_in_check_error.
    ref_scan_manager_double->set_data_for_check_error( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 1 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assertion->assert_errors( act = cut->statistics->get_number_errors( ) exp = 0 ).
    assertion->assert_pseudo_comments( act = cut->statistics->get_number_pseudo_comments( ) exp = 1 ).
  ENDMETHOD.
ENDCLASS.
