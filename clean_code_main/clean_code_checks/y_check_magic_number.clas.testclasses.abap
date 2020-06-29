CLASS ltc_magic_number_ok DEFINITION DEFERRED.
CLASS y_check_magic_number DEFINITION LOCAL FRIENDS ltc_magic_number_ok.
CLASS ltc_magic_number_error DEFINITION DEFERRED.
CLASS y_check_magic_number DEFINITION LOCAL FRIENDS ltc_magic_number_error.

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

CLASS ltd_test_data_ok_case DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager PARTIALLY IMPLEMENTED.

    METHODS:
      set_data_for_ok,
      set_data_for_if_ok,
      set_data_for_if_0_ok,
      set_data_for_do_ok,
      set_data_for_when_ok,
      set_data_for_check_ok,
      set_data_for_case_subrc_ok,
      set_data_for_check_lines_ok.

  PRIVATE SECTION.
    DATA:
      levels     TYPE slevel_tab,
      structures TYPE sstruc_tab,
      statements TYPE sstmnt_tab,
      tokens     TYPE stokesx_tab.
ENDCLASS.

CLASS ltd_test_data_ok_case IMPLEMENTATION.
  METHOD y_if_scan_manager~get_structures.
    result = structures.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_statements.
    result = statements.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_tokens.
    result = tokens.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_levels.
    result = levels.
  ENDMETHOD.

  METHOD y_if_scan_manager~set_ref_scan.
    RETURN.
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '3' type = 'K' ) ).
    tokens = VALUE #( ( str = 'VALUE_A' type = 'I' row = '1' )
                      ( str = '='       type = 'I' row = '1' )
                      ( str = '0'       type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_if_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'IF'       type = 'I' row = '1' )
                      ( str = 'SY-SUBRC' type = 'I' row = '1' )
                      ( str = 'NE'       type = 'I' row = '1' )
                      ( str = '0'        type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_if_0_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'IF'    type = 'I' row = '1' )
                      ( str = 'VALUE' type = 'I' row = '1' )
                      ( str = '>'     type = 'I' row = '1' )
                      ( str = '0'     type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_do_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '3' type = 'K' ) ).
    tokens = VALUE #( ( str = 'DO'       type = 'I' row = '1' )
                      ( str = 'NO_CASES' type = 'I' row = '1' )
                      ( str = 'TIMES'    type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_when_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '2' type = 'K' ) ).
    tokens = VALUE #( ( str = 'WHEN'  type = 'I' row = '1' )
                      ( str = 'ALPHA' type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_check_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'CHECK' type = 'I' row = '1' )
                      ( str = 'X'     type = 'I' row = '1' )
                      ( str = '='     type = 'I' row = '1' )
                      ( str = 'ALPHA' type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_case_subrc_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 4 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 4 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '2' type = 'K' )
                          ( level = '1' from = '3' to = '4' type = 'K' )
                          ( level = '1' from = '5' to = '6' type = 'K' )
                          ( level = '1' from = '7' to = '7' type = 'K' ) ).
    tokens = VALUE #( ( str = 'CASE'     type = 'I' row = '1' )
                      ( str = 'SY-SUBRC' type = 'I' row = '1' )
                      ( str = 'WHEN'     type = 'I' row = '2' )
                      ( str = '4'        type = 'I' row = '2' )
                      ( str = 'WHEN'     type = 'I' row = '3' )
                      ( str = 'OTHERS'   type = 'I' row = '3' )
                      ( str = 'ENDCASE'  type = 'I' row = '4' ) ).
  ENDMETHOD.

  METHOD set_data_for_check_lines_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'CHECK' type = 'I' row = '1' )
                      ( str = 'LINES' type = 'I' row = '1' )
                      ( str = '>'     type = 'I' row = '1' )
                      ( str = '1'     type = 'I' row = '1' ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_test_data_error_case DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager PARTIALLY IMPLEMENTED.

    METHODS:
      set_data_for_if_error,
      set_data_for_do_error,
      set_data_for_when_error,
      set_data_for_check_error,
      set_pseudo_comment_ok.

  PRIVATE SECTION.
    DATA:
      levels     TYPE slevel_tab,
      structures TYPE sstruc_tab,
      statements TYPE sstmnt_tab,
      tokens     TYPE stokesx_tab.
ENDCLASS.

CLASS ltd_test_data_error_case IMPLEMENTATION.
  METHOD y_if_scan_manager~get_structures.
    result = structures.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_statements.
    result = statements.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_tokens.
    result = tokens.
  ENDMETHOD.

  METHOD y_if_scan_manager~get_levels.
    result = levels.
  ENDMETHOD.

  METHOD y_if_scan_manager~set_ref_scan.
    RETURN.
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_if_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'IF'       type = 'I' row = '1' )
                      ( str = 'TEST_ERR' type = 'I' row = '1' )
                      ( str = 'EQ'       type = 'I' row = '1' )
                      ( str = '8'        type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_do_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '3' type = 'K' ) ).
    tokens = VALUE #( ( str = 'DO'    type = 'I' row = '1' )
                      ( str = '5'     type = 'I' row = '1' )
                      ( str = 'TIMES' type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_when_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '2' type = 'K' ) ).
    tokens = VALUE #( ( str = 'WHEN' type = 'I' row = '1' )
                      ( str = '10000'   type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_data_for_check_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).
    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 stmnt_type = scan_struc_stmnt_type-method ) ).
    statements = VALUE #( ( level = '1' from = '1' to = '4' type = 'K' ) ).
    tokens = VALUE #( ( str = 'CHECK' type = 'I' row = '1' )
                      ( str = 'X'     type = 'I' row = '1' )
                      ( str = '='     type = 'I' row = '1' )
                      ( str = '23'    type = 'I' row = '1' ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    set_data_for_if_error( ).
    statements = VALUE #( BASE statements
                        ( level = 1 from = '5' to = '5' type = 'P' ) ).
    tokens = VALUE #( BASE tokens
                    ( str = '"#EC CI_MAGIC' type = 'C' row = 1 ) ).
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
      magic_number_in_when_ok FOR TESTING,
      magic_number_in_check_ok FOR TESTING,
      magic_number_in_case_subrc_ok FOR TESTING,
      magic_number_in_check_lines_ok FOR TESTING.
ENDCLASS.

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

  METHOD magic_number_in_when_ok.
    ref_scan_manager_double->set_data_for_when_ok( ).
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
      magic_number_in_when_error FOR TESTING,
      magic_number_in_check_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

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

  METHOD magic_number_in_when_error.
    ref_scan_manager_double->set_data_for_when_error( ).
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
