CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 0 ) ).
    result = VALUE #( BASE result
                    ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 0 ) ).
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
    RETURN.                                       "empty for test case
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 23 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 14 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 2 stmnt_to = 8 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 15 stmnt_to = 19 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 16 stmnt_to = 18 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 20 stmnt_to = 23 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1' type = 'K' )
                          ( level = 1 from = '2' to = '2' type = 'K' )
                          ( level = 1 from = '3' to = '3' type = 'K' )
                          ( level = 1 from = '4' to = '4' type = 'K' )
                          ( level = 1 from = '5' to = '5' type = 'K' )
                          ( level = 1 from = '6' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'K' )
                          ( level = 1 from = '8' to = '8' type = 'K' )
                          ( level = 1 from = '9' to = '9' type = 'K' )
                          ( level = 1 from = '10' to = '10' type = 'K' )
                          ( level = 1 from = '11' to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '13' type = 'K' )
                          ( level = 1 from = '14' to = '14' type = 'K' )
                          ( level = 1 from = '15' to = '15' type = 'K' )
                          ( level = 1 from = '16' to = '16' type = 'K' )
                          ( level = 1 from = '17' to = '17' type = 'K' )
                          ( level = 1 from = '18' to = '18' type = 'K' )
                          ( level = 1 from = '19' to = '19' type = 'K' )
                          ( level = 1 from = '20' to = '20' type = 'K' )
                          ( level = 1 from = '21' to = '21' type = 'P' )
                          ( level = 1 from = '22' to = '22' type = 'K' )
                          ( level = 1 from = '23' to = '23' type = 'K' ) ).

    tokens = VALUE #( ( str = 'IF'        type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'STATEMENT' type = 'I' row = 3 )
                      ( str = 'ELSEIF'    type = 'I' row = 4 )
                      ( str = 'STATEMENT' type = 'I' row = 5 )
                      ( str = 'ELSE'      type = 'I' row = 6 )
                      ( str = 'STATEMENT' type = 'I' row = 7 )
                      ( str = 'ENDIF'     type = 'I' row = 8 )
                      ( str = 'STATEMENT' type = 'I' row = 9 )
                      ( str = 'ELSEIF'    type = 'I' row = 10 )
                      ( str = 'STATEMENT' type = 'I' row = 11 )
                      ( str = 'ELSE'      type = 'I' row = 12 )
                      ( str = 'STATEMENT' type = 'I' row = 13 )
                      ( str = 'ENDIF'     type = 'I' row = 14 )
                      ( str = 'IF'        type = 'I' row = 15 )
                      ( str = 'IF'        type = 'I' row = 16 )
                      ( str = 'STATEMENT' type = 'I' row = 17 )
                      ( str = 'ENDIF'     type = 'I' row = 18 )
                      ( str = 'ENDIF'     type = 'I' row = 19 )
                      ( str = 'IF'        type = 'I' row = 20 )
                      ( str = '* COMMENT' type = 'C' row = 21 )
                      ( str = 'STATEMENT' type = 'I' row = 22 )
                      ( str = 'ENDIF'     type = 'I' row = 23 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 12 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 8 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 2 stmnt_to = 5 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 9 stmnt_to = 12 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 10 stmnt_to = 11 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1' type = 'K' )
                          ( level = 1 from = '2' to = '2' type = 'K' )
                          ( level = 1 from = '3' to = '3' type = 'K' )
                          ( level = 1 from = '4' to = '4' type = 'K' )
                          ( level = 1 from = '5' to = '5' type = 'K' )
                          ( level = 1 from = '6' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'K' )
                          ( level = 1 from = '8' to = '8' type = 'K' )
                          ( level = 1 from = '9' to = '9' type = 'K' )
                          ( level = 1 from = '10' to = '10' type = 'K' )
                          ( level = 1 from = '11' to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'K' ) ).

    tokens = VALUE #( ( str = 'IF'        type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'ELSEIF'    type = 'I' row = 3 )
                      ( str = 'ELSE'      type = 'I' row = 4 )
                      ( str = 'ENDIF'     type = 'I' row = 5 )
                      ( str = 'ELSEIF'    type = 'I' row = 6 )
                      ( str = 'ELSE'      type = 'I' row = 7 )
                      ( str = 'ENDIF'     type = 'I' row = 8 )
                      ( str = 'IF'        type = 'I' row = 9 )
                      ( str = 'IF'        type = 'I' row = 10 )
                      ( str = 'ENDIF'     type = 'I' row = 11 )
                      ( str = 'ENDIF'     type = 'I' row = 12 ) ).
  ENDMETHOD.

  METHOD set_check_pseudo_comment_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 12 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 13 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 2 stmnt_to = 8 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 14 stmnt_to = 18 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if )
                          ( stmnt_from = 15 stmnt_to = 17 type = scan_struc_type-alternation stmnt_type = scan_struc_stmnt_type-if ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1' type = 'K' )
                          ( level = 1 from = '2' to = '2' type = 'K' )
                          ( level = 1 from = '3' to = '3' type = 'P' )
                          ( level = 1 from = '4' to = '4' type = 'K' )
                          ( level = 1 from = '5' to = '5' type = 'P' )
                          ( level = 1 from = '6' to = '6' type = 'K' )
                          ( level = 1 from = '7' to = '7' type = 'P' )
                          ( level = 1 from = '8' to = '8' type = 'K' )
                          ( level = 1 from = '9' to = '9' type = 'K' )
                          ( level = 1 from = '10' to = '10' type = 'P' )
                          ( level = 1 from = '11' to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'P' )
                          ( level = 1 from = '13' to = '13' type = 'K' )
                          ( level = 1 from = '14' to = '14' type = 'K' )
                          ( level = 1 from = '15' to = '15' type = 'K' )
                          ( level = 1 from = '16' to = '16' type = 'P' )
                          ( level = 1 from = '17' to = '17' type = 'K' )
                          ( level = 1 from = '18' to = '18' type = 'K' ) ).

    tokens = VALUE #( ( str = 'IF'                   type = 'I' row = 1 )
                      ( str = 'IF'                   type = 'I' row = 2 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 2 )
                      ( str = 'ELSEIF'               type = 'I' row = 3 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 3 )
                      ( str = 'ELSE'                 type = 'I' row = 4 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 4 )
                      ( str = 'ENDIF'                type = 'I' row = 5 )
                      ( str = 'ELSEIF'               type = 'I' row = 6 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 6 )
                      ( str = 'ELSE'                 type = 'I' row = 7 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 7 )
                      ( str = 'ENDIF'                type = 'I' row = 8 )
                      ( str = 'IF'                   type = 'I' row = 9 )
                      ( str = 'IF'                   type = 'I' row = 10 )
                      ( str = '"#EC EMPTY_IF_BRANCH' type = 'C' row = 10 )
                      ( str = 'ENDIF'                type = 'I' row = 11 )
                      ( str = 'ENDIF'                type = 'I' row = 12 ) ).

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

CLASS ltc_empty_if_branches DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_empty_if_branches,
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

CLASS y_check_empty_if_branches DEFINITION LOCAL FRIENDS ltc_empty_if_branches.

CLASS ltc_empty_if_branches IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_empty_if_branches( ).
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
    assert_errors( 6 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_check_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 6 ).
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
