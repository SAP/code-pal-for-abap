CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'N' threshold = 10 ) ).
    result = VALUE #( BASE result
                    ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 10 ) ).
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
      set_data_for_error.

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
    RETURN.                                       "empty for test case
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 14 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 14 stmnt_type = scan_struc_stmnt_type-function ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1' type = 'P' )
                          ( level = 1 from = '2' to = '2' type = 'K' )
                          ( level = 1 from = '3' to = '3' type = 'K' )
                          ( level = 1 from = '4' to = '4' type = 'K' )
                          ( level = 1 from = '5' to = '5' type = 'K' )
                          ( level = 1 from = '6' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'K' )
                          ( level = 1 from = '8' to = '8' type = 'K' )
                          ( level = 1 from = '9' to = '10' type = 'K' )
                          ( level = 1 from = '11' to = '11' type = 'P' )
                          ( level = 1 from = '12' to = '12' type = 'P' )
                          ( level = 1 from = '13' to = '13' type = 'P' )
                          ( level = 1 from = '14' to = '14' type = 'K' ) ).

    tokens = VALUE #( ( str = '"COMMENT'    type = 'C' row = 1 )
                      ( str = 'FUNCTION'    type = 'I' row = 2 )
                      ( str = 'SOME_CODE'   type = 'I' row = 3 )
                      ( str = 'SOME_CODE'   type = 'I' row = 4 )
                      ( str = 'SOME_CODE'   type = 'I' row = 5 )
                      ( str = 'SOME_CODE'   type = 'I' row = 6 )
                      ( str = 'SOME_CODE'   type = 'I' row = 7 )
                      ( str = 'SOME_CODE'   type = 'I' row = 8 )
                      ( str = 'SOME_CODE'   type = 'I' row = 9 )
                      ( str = 'SOME_CODE'   type = 'I' row = 9 )
                      ( str = '"COMMENT'    type = 'C' row = 10 )
                      ( str = '*"*COMMENT'  type = 'C' row = 11 )
                      ( str = '*"'          type = 'C' row = 12 )
                      ( str = 'ENDFUNCTION' type = 'I' row = 13 )
                      ( str = '"COMMENT'    type = 'C' row = 14 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 11 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 11 stmnt_type = scan_struc_stmnt_type-class_implementation ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1' type = 'K' )
                          ( level = 1 from = '2' to = '2' type = 'K' )
                          ( level = 1 from = '3' to = '3' type = 'K' )
                          ( level = 1 from = '4' to = '4' type = 'K' )
                          ( level = 1 from = '5' to = '5' type = 'K' )
                          ( level = 1 from = '6' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'K' )
                          ( level = 1 from = '8' to = '8' type = 'P' )
                          ( level = 1 from = '9' to = '9' type = 'P' )
                          ( level = 1 from = '10' to = '10' type = 'P' ) ).

    tokens = VALUE #( ( str = 'SOME_CODE'  type = 'I' row = 1 )
                      ( str = 'SOME_CODE'  type = 'I' row = 2 )
                      ( str = 'SOME_CODE'  type = 'I' row = 3 )
                      ( str = 'SOME_CODE'  type = 'I' row = 4 )
                      ( str = 'SOME_CODE'  type = 'I' row = 5 )
                      ( str = 'SOME_CODE'  type = 'I' row = 6 )
                      ( str = 'SOME_CODE'  type = 'I' row = 7 )
                      ( str = '"COMMENT'   type = 'C' row = 8 )
                      ( str = '"COMMENT'   type = 'C' row = 9 )
                      ( str = '"COMMENT'   type = 'C' row = 10 ) ).
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

CLASS ltc_comment_usage DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_comment_usage,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      is_bound FOR TESTING,
      cut_ok FOR TESTING,
      cut_error FOR TESTING.
ENDCLASS.

CLASS y_check_comment_usage DEFINITION LOCAL FRIENDS ltc_comment_usage.

CLASS ltc_comment_usage IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_comment_usage( ).
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
