CLASS lcl_test_code_detector DEFINITION.
  PUBLIC SECTION.
    INTERFACES y_if_testcode_detector.
ENDCLASS.

CLASS lcl_test_code_detector IMPLEMENTATION.
  METHOD y_if_testcode_detector~set_ref_scan_manager.
    RETURN.
  ENDMETHOD.

  METHOD y_if_testcode_detector~clear.
    RETURN.
  ENDMETHOD.

  METHOD y_if_testcode_detector~is_testcode.
    result = abap_true.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true prio = 'N' threshold = 0 )
                      ( apply_on_testcode = abap_true prio = 'E' threshold = 0 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '19000101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager PARTIALLY IMPLEMENTED.

    METHODS:
      set_data_for_ok,
      set_data_for_error,
      set_check_pseudo_comment_ok.

  PRIVATE SECTION.
    DATA:
      levels     TYPE slevel_tab,
      structures TYPE sstruc_tab,
      statements TYPE sstmnt_tab,
      tokens     TYPE stokesx_tab.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.
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
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 4 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 4 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '4'  type = 'K' )
                          ( level = 1 from = '5'  to = '8'  type = 'K' )
                          ( level = 1 from = '9'  to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '14' type = 'K' ) ).

    tokens = VALUE #( ( str = 'INSERT' type = 'I' row = 1 )
                      ( str = 'itab'   type = 'I' row = 1 )
                      ( str = 'INTO'   type = 'I' row = 1 )
                      ( str = 'TABLE'  type = 'I' row = 1 )
                      ( str = 'MODIFY' type = 'I' row = 2 )
                      ( str = 'TABLE'  type = 'I' row = 2 )
                      ( str = 'itab'   type = 'I' row = 2 )
                      ( str = 'FROM'   type = 'I' row = 2 )
                      ( str = 'DELETE' type = 'I' row = 3 )
                      ( str = 'itab'   type = 'I' row = 3 )
                      ( str = 'WHERE'  type = 'I' row = 3 )
                      ( str = 'DELETE' type = 'I' row = 4 )
                      ( str = 'itab'   type = 'I' row = 4 )
                      ( str = 'FROM'   type = 'I' row = 4 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 11 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 11 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '1'  type = 'K' )
                          ( level = 1 from = '2'  to = '2'  type = 'K' )
                          ( level = 1 from = '3'  to = '3'  type = 'K' )
                          ( level = 1 from = '4'  to = '4'  type = 'K' )
                          ( level = 1 from = '5'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '8'  type = 'K' )
                          ( level = 1 from = '9'  to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '14' type = 'K' )
                          ( level = 1 from = '15' to = '17' type = 'K' )
                          ( level = 1 from = '18' to = '20' type = 'K' )
                          ( level = 1 from = '21' to = '23' type = 'K' ) ).

    tokens = VALUE #( ( str = 'COMMIT'   type = 'I' row = 1 )
                      ( str = 'ROLLBACK' type = 'I' row = 2 )
                      ( str = 'SELECT'   type = 'I' row = 3 )
                      ( str = 'EXEC'     type = 'I' row = 4 )
                      ( str = 'ALTER'    type = 'I' row = 5 )
                      ( str = 'INSERT'   type = 'I' row = 6 )
                      ( str = 'INTO'     type = 'I' row = 6 )
                      ( str = 'tadir'    type = 'I' row = 6 )
                      ( str = 'DELETE'   type = 'I' row = 7 )
                      ( str = 'FROM'     type = 'I' row = 7 )
                      ( str = 'tadir'    type = 'I' row = 7 )
                      ( str = 'INSERT'   type = 'I' row = 8 )
                      ( str = 'tadir'    type = 'I' row = 8 )
                      ( str = 'FROM'     type = 'I' row = 8 )
                      ( str = 'UPDATE'   type = 'I' row = 9 )
                      ( str = 'tadir'    type = 'I' row = 9 )
                      ( str = 'FROM'     type = 'I' row = 9 )
                      ( str = 'MODIFY'   type = 'I' row = 10 )
                      ( str = 'tadir'    type = 'I' row = 10 )
                      ( str = 'FROM'     type = 'I' row = 10 )
                      ( str = 'DELETE'   type = 'I' row = 11 )
                      ( str = 'tadir'    type = 'I' row = 11 )
                      ( str = 'FROM'     type = 'I' row = 11 ) ).
  ENDMETHOD.

  METHOD set_check_pseudo_comment_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 11 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 11 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '1'  type = 'K' )
                          ( level = 1 from = '2'  to = '2'  type = 'P' )
                          ( level = 1 from = '3'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '6'  type = 'P' )
                          ( level = 1 from = '7'  to = '9'  type = 'K' )
                          ( level = 1 from = '10' to = '10' type = 'P' )
                          ( level = 1 from = '11' to = '13' type = 'K' )
                          ( level = 1 from = '14' to = '14' type = 'P' ) ).

    tokens = VALUE #( ( str = 'COMMIT'   type = 'I' row = 1 )
                      ( str = '"#EC DB_ACCESS_UT'   type = 'C' row = 1 )
                      ( str = 'DELETE'   type = 'I' row = 2 )
                      ( str = 'FROM'     type = 'I' row = 2 )
                      ( str = 'tadir'    type = 'I' row = 2 )
                      ( str = '"#EC DB_ACCESS_UT'   type = 'C' row = 2 )
                      ( str = 'INSERT'   type = 'I' row = 3 )
                      ( str = 'tadir'    type = 'I' row = 3 )
                      ( str = 'FROM'     type = 'I' row = 3 )
                      ( str = '"#EC DB_ACCESS_UT'   type = 'C' row = 3 )
                      ( str = 'DELETE'   type = 'I' row = 4 )
                      ( str = 'tadir'    type = 'I' row = 4 )
                      ( str = 'FROM'     type = 'I' row = 4 )
                      ( str = '"#EC DB_ACCESS_UT'   type = 'C' row = 4 ) ).
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
    DATA: cut                     TYPE REF TO y_check_db_access_in_ut,
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

CLASS y_check_db_access_in_ut DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_db_access_in_ut( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW ltd_clean_code_manager( ).
    cut->test_code_detector = NEW lcl_test_code_detector( ).
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
    ref_scan_manager_double->set_check_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
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
