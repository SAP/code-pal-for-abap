CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 2 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
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
      set_pseudo_comment_ok.

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
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 6 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 6 stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '2'   type = 'K' )
                          ( level = 1 from = '3' to = '6'   type = 'K' )
                          ( level = 1 from = '7' to = '9'   type = 'K' )
                          ( level = 1 from = '10' to = '10' type = 'K' )
                          ( level = 1 from = '11' to = '11' type = 'N' )
                          ( level = 1 from = '12' to = '12' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHOD'    type = 'I' row = 1 )
                      ( str = 'CALC'      type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'VALUE_A'   type = 'I' row = 2 )
                      ( str = '='         type = 'I' row = 2 )
                      ( str = '1'         type = 'I' row = 2 )
                      ( str = 'VALUE_B'   type = 'I' row = 3 )
                      ( str = '='         type = 'I' row = 3 )
                      ( str = '2'         type = 'I' row = 3 )
                      ( str = 'ENDIF'     type = 'I' row = 4 )
                      ( str = ''          type = 'I' row = 5 )
                      ( str = 'ENDMETHOD' type = 'I' row = 6 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 6 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 6 stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '2'  type = 'K' )
                          ( level = 1 from = '3'  to = '6'  type = 'K' )
                          ( level = 1 from = '7'  to = '9'  type = 'K' )
                          ( level = 1 from = '10' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '13' type = 'K' )
                          ( level = 1 from = '14' to = '14' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHOD'    type = 'I' row = 1 )
                      ( str = 'CALC'      type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'VALUE_A'   type = 'I' row = 2 )
                      ( str = '='         type = 'I' row = 2 )
                      ( str = '1'         type = 'I' row = 2 )
                      ( str = 'VALUE_B'   type = 'I' row = 3 )
                      ( str = '='         type = 'I' row = 3 )
                      ( str = '2'         type = 'I' row = 3 )
                      ( str = 'VALUE_C'   type = 'I' row = 4 )
                      ( str = '='         type = 'I' row = 4 )
                      ( str = '3'         type = 'I' row = 4 )
                      ( str = 'ENDIF'     type = 'I' row = 5 )
                      ( str = 'ENDMETHOD' type = 'I' row = 6 ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    set_data_for_error( ).

    APPEND VALUE sstmnt( level = 1 from = '15' to = '15' type = 'P' )
           TO statements.
    APPEND VALUE stokesx( str = '"#EC CI_NOES' type = 'C' row = 6 )
           TO tokens.
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
    DATA: cut                     TYPE REF TO y_check_num_exec_statements,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      num_exec_statements_ok FOR TESTING,
      num_exec_statements_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_num_exec_statements DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_num_exec_statements( ).
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

  METHOD num_exec_statements_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD num_exec_statements_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 1 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 1 ).
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

CLASS ltd_ref_scan_manager_event DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager PARTIALLY IMPLEMENTED.

    METHODS:
      set_data_for_ok,
      set_data_for_error,
      set_pseudo_comment_ok.

  PRIVATE SECTION.
    DATA:
      levels     TYPE slevel_tab,
      structures TYPE sstruc_tab,
      statements TYPE sstmnt_tab,
      tokens     TYPE stokesx_tab.
ENDCLASS.

CLASS ltd_ref_scan_manager_event IMPLEMENTATION.
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

    structures = VALUE #( ( type = scan_struc_type-event stmnt_from = 1 stmnt_to = 4 stmnt_type = scan_struc_stmnt_type-start_of_selection ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '1'   type = 'K' )
                          ( level = 1 from = '2' to = '5'   type = 'K' )
                          ( level = 1 from = '6' to = '8'   type = 'K' )
                          ( level = 1 from = '9' to = '9'   type = 'K' ) ).

    tokens = VALUE #( ( str = 'START-OF-SELECTION' type = 'I' row = 1 )
                      ( str = 'IF'                 type = 'I' row = 2 )
                      ( str = 'VALUE_A'            type = 'I' row = 2 )
                      ( str = '='                  type = 'I' row = 2 )
                      ( str = '1'                  type = 'I' row = 2 )
                      ( str = 'VALUE_B'            type = 'I' row = 3 )
                      ( str = '='                  type = 'I' row = 3 )
                      ( str = '2'                  type = 'I' row = 3 )
                      ( str = 'ENDIF'              type = 'I' row = 4 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 5 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( type = scan_struc_type-event stmnt_from = 1 stmnt_to = 5 stmnt_type = scan_struc_stmnt_type-start_of_selection ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '1'  type = 'K' )
                          ( level = 1 from = '2'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '8'  type = 'K' )
                          ( level = 1 from = '9'  to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'K' ) ).

    tokens = VALUE #( ( str = 'START-OF-SELECTION' type = 'I' row = 1 )
                      ( str = 'IF'                 type = 'I' row = 2 )
                      ( str = 'VALUE_A'            type = 'I' row = 2 )
                      ( str = '='                  type = 'I' row = 2 )
                      ( str = '1'                  type = 'I' row = 2 )
                      ( str = 'VALUE_B'            type = 'I' row = 3 )
                      ( str = '='                  type = 'I' row = 3 )
                      ( str = '2'                  type = 'I' row = 3 )
                      ( str = 'VALUE_C'            type = 'I' row = 4 )
                      ( str = '='                  type = 'I' row = 4 )
                      ( str = '3'                  type = 'I' row = 4 )
                      ( str = 'ENDIF'              type = 'I' row = 5 ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    set_data_for_error( ).

    structures = VALUE #( ( type = scan_struc_type-event stmnt_from = 1 stmnt_to = 6 stmnt_type = scan_struc_stmnt_type-start_of_selection ) ).

    APPEND VALUE sstmnt( level = 1 from = '13' to = '13' type = 'P' )
           TO statements.
    APPEND VALUE stokesx( str = '"#EC CI_NOES' type = 'C' row = 6 )
           TO tokens.
  ENDMETHOD.
ENDCLASS.

CLASS local_test_class2 DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_num_exec_statements,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager_event.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      num_exec_statements_ok FOR TESTING,
      num_exec_statements_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_num_exec_statements DEFINITION LOCAL FRIENDS local_test_class2.

CLASS local_test_class2 IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_num_exec_statements( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager_event( ).
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

  METHOD num_exec_statements_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD num_exec_statements_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 1 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 1 ).
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
