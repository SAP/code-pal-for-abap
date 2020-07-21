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
    RETURN.                                       "empty for test case
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 11 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 11 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition )
                          ( stmnt_from = 1 stmnt_to = 11 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-interface ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '18' type = 'K' )
                          ( level = 1 from = '19' to = '24' type = 'K' )
                          ( level = 1 from = '25' to = '30' type = 'K' )
                          ( level = 1 from = '31' to = '36' type = 'K' )
                          ( level = 1 from = '37' to = '42' type = 'K' )
                          ( level = 1 from = '43' to = '48' type = 'K' )
                          ( level = 1 from = '49' to = '54' type = 'K' )
                          ( level = 1 from = '55' to = '60' type = 'K' )
                          ( level = 1 from = '61' to = '66' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHODS'       type = 'I' row = 1 )
                      ( str = 'IS_NAME'       type = 'I' row = 1 )
                      ( str = 'RETURNING'     type = 'I' row = 1 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 1 )
                      ( str = 'TYPE'          type = 'I' row = 1 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 1 )

                      ( str = 'METHODS'       type = 'I' row = 2 )
                      ( str = 'HAS_NAME'      type = 'I' row = 2 )
                      ( str = 'RETURNING'     type = 'I' row = 2 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 2 )
                      ( str = 'TYPE'          type = 'I' row = 2 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 2 )

                      ( str = 'METHODS'       type = 'I' row = 3 )
                      ( str = 'NAME'          type = 'I' row = 3 )
                      ( str = 'RETURNING'     type = 'I' row = 3 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 3 )
                      ( str = 'TYPE'          type = 'I' row = 3 )
                      ( str = 'STRING'        type = 'I' row = 3 )

                      ( str = 'METHODS'       type = 'I' row = 4 )
                      ( str = 'ARE_NAME'      type = 'I' row = 4 )
                      ( str = 'RETURNING'     type = 'I' row = 4 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 4 )
                      ( str = 'TYPE'          type = 'I' row = 4 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 4 )

                      ( str = 'METHODS'       type = 'I' row = 5 )
                      ( str = 'HAVE_NAME'     type = 'I' row = 5 )
                      ( str = 'RETURNING'     type = 'I' row = 5 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 5 )
                      ( str = 'TYPE'          type = 'I' row = 5 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 5 )

                      ( str = 'METHODS'       type = 'I' row = 6 )
                      ( str = 'CONTAINS_NAME' type = 'I' row = 6 )
                      ( str = 'RETURNING'     type = 'I' row = 6 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 6 )
                      ( str = 'TYPE'          type = 'I' row = 6 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 6 )

                      ( str = 'METHODS'       type = 'I' row = 7 )
                      ( str = 'VALUE_EXISTS'  type = 'I' row = 7 )
                      ( str = 'RETURNING'     type = 'I' row = 7 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 7 )
                      ( str = 'TYPE'          type = 'I' row = 7 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 7 )

                      ( str = 'METHODS'       type = 'I' row = 8 )
                      ( str = 'CAN_NAME'      type = 'I' row = 8 )
                      ( str = 'RETURNING'     type = 'I' row = 8 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 8 )
                      ( str = 'TYPE'          type = 'I' row = 8 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 8 )

                      ( str = 'METHODS'       type = 'I' row = 9 )
                      ( str = 'MUST_NAME'     type = 'I' row = 9 )
                      ( str = 'RETURNING'     type = 'I' row = 9 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 9 )
                      ( str = 'TYPE'          type = 'I' row = 9 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 9 )

                      ( str = 'METHODS'       type = 'I' row = 10 )
                      ( str = 'SHOULD_NAME'   type = 'I' row = 10 )
                      ( str = 'RETURNING'     type = 'I' row = 10 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 10 )
                      ( str = 'TYPE'          type = 'I' row = 10 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 10 )

                      ( str = 'METHODS'       type = 'I' row = 11 )
                      ( str = 'TRY_THIS'      type = 'I' row = 11 )
                      ( str = 'RETURNING'     type = 'I' row = 11 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 11 )
                      ( str = 'TYPE'          type = 'I' row = 11 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 11 )
                      ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 4 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 4 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition )
                          ( stmnt_from = 1 stmnt_to = 4 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-interface ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '18' type = 'K' )
                          ( level = 1 from = '19' to = '24' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHODS'       type = 'I' row = 1 )
                      ( str = 'I_NAME'        type = 'I' row = 1 )
                      ( str = 'RETURNING'     type = 'I' row = 1 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 1 )
                      ( str = 'TYPE'          type = 'I' row = 1 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 1 )
                      ( str = 'METHODS'       type = 'I' row = 2 )
                      ( str = 'HA_NAME'       type = 'I' row = 2 )
                      ( str = 'RETURNING'     type = 'I' row = 2 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 2 )
                      ( str = 'TYPE'          type = 'I' row = 2 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 2 )
                      ( str = 'METHODS'       type = 'I' row = 3 )
                      ( str = 'AR_NAME'       type = 'I' row = 3 )
                      ( str = 'RETURNING'     type = 'I' row = 3 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 3 )
                      ( str = 'TYPE'          type = 'I' row = 3 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 3 )
                      ( str = 'METHODS'       type = 'I' row = 4 )
                      ( str = 'HAV_NAME'      type = 'I' row = 4 )
                      ( str = 'RETURNING'     type = 'I' row = 4 )
                      ( str = 'VALUE(RESULT)' type = 'I' row = 4 )
                      ( str = 'TYPE'          type = 'I' row = 4 )
                      ( str = 'ABAP_BOOL'     type = 'I' row = 4 )
                      ).
  ENDMETHOD.

  METHOD set_check_pseudo_comment_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 2 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 2 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition )
                          ( stmnt_from = 1 stmnt_to = 2 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-interface ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'P' ) ).

    tokens = VALUE #( ( str = 'METHODS'            type = 'I' row = 1 )
                      ( str = 'NAME'               type = 'I' row = 1 )
                      ( str = 'RETURNING'          type = 'I' row = 1 )
                      ( str = 'VALUE(RESULT)'      type = 'I' row = 1 )
                      ( str = 'TYPE'               type = 'I' row = 1 )
                      ( str = 'ABAP_BOOL'          type = 'I' row = 1 )
                      ( str = '"#EC METH_RET_BOOL' type = 'C' row = 1 ) ).
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
    assert_errors( 8 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_check_pseudo_comment_ok( ).
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
