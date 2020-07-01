CLASS ltc_cyclo_complexity DEFINITION DEFERRED.
CLASS y_check_cyclomatic_complexity DEFINITION LOCAL FRIENDS ltc_cyclo_complexity.

CLASS ltd_clean_code_manager_error DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_error IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 2 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '19000101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_manager_warning DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_warning IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 )
                      ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '19000101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_manager_no_cust DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_no_cust IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    RAISE EXCEPTION TYPE ycx_no_check_customizing.
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
    RETURN.                                       "empty for test case
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 5 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 5 stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '2'   type = 'K' )
                          ( level = 1 from = '3' to = '7'   type = 'K' )
                          ( level = 1 from = '8' to = '10'  type = 'K' )
                          ( level = 1 from = '11' to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHOD'    type = 'I' row = 1 )
                      ( str = 'CALC'      type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'VALUE_A' type = 'I' row = 2 )
                      ( str = '='         type = 'I' row = 2 )
                      ( str = '1'         type = 'I' row = 2 )
                      ( str = 'VALUE_B' type = 'I' row = 3 )
                      ( str = '='         type = 'I' row = 3 )
                      ( str = '2'         type = 'I' row = 3 )
                      ( str = 'ENDIF'     type = 'I' row = 4 )
                      ( str = 'ENDMETHOD' type = 'I' row = 5 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 11 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 11 stmnt_type = scan_struc_stmnt_type-method ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '2'   type = 'K' )
                          ( level = 1 from = '3' to = '6'   type = 'K' )
                          ( level = 1 from = '7' to = '9'   type = 'K' )
                          ( level = 1 from = '10' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '17' type = 'K' )
                          ( level = 1 from = '18' to = '19' type = 'K' )
                          ( level = 1 from = '20' to = '21' type = 'K' )
                          ( level = 1 from = '22' to = '22' type = 'K' )
                          ( level = 1 from = '23' to = '23' type = 'K' )
                          ( level = 1 from = '24' to = '24' type = 'K' )
                          ( level = 1 from = '25' to = '25' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHOD'    type = 'I' row = 1 )
                      ( str = 'CALC'      type = 'I' row = 1 )
                      ( str = 'IF'        type = 'I' row = 2 )
                      ( str = 'VALUE_A'   type = 'I' row = 2 )
                      ( str = '='         type = 'I' row = 2 )
                      ( str = '1'         type = 'I' row = 2 )
                      ( str = 'VALUE_B'   type = 'I' row = 3 )
                      ( str = '='         type = 'I' row = 3 )
                      ( str = '2'         type = 'I' row = 3 )
                      ( str = 'DO'        type = 'I' row = 6 )
                      ( str = 'COUNTER'   type = 'I' row = 6 )
                      ( str = 'TIMES'     type = 'I' row = 6 )
                      ( str = 'VALUE_C'   type = 'I' row = 7 )
                      ( str = '='         type = 'I' row = 7 )
                      ( str = 'VALUE_C'   type = 'I' row = 7 )
                      ( str = '+'         type = 'I' row = 7 )
                      ( str = '1'         type = 'I' row = 7 )
                      ( str = 'CASE'      type = 'I' row = 8 )
                      ( str = 'VALUE_D'   type = 'I' row = 8 )
                      ( str = 'WHEN'      type = 'I' row = 9 )
                      ( str = 'ACTIVE'    type = 'I' row = 9 )
                      ( str = 'ENDCASE'   type = 'I' row = 10 )
                      ( str = 'ENDDO'     type = 'I' row = 11 )
                      ( str = 'ENDIF'     type = 'I' row = 12 )
                      ( str = 'ENDMETHOD' type = 'I' row = 13 ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    set_data_for_error( ).
    statements = VALUE #( BASE statements
                        ( level = 1 from = '26' to = '26' type = 'P' ) ).
    tokens = VALUE #( BASE tokens
                    ( str = '"#EC CI_CYCLO' type = 'C' row = 14 ) ).
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

CLASS ltc_cyclo_complexity DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_cyclomatic_complexity,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_warnings IMPORTING warn_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      max_nesting_depth_ok FOR TESTING,
      max_nesting_depth_error FOR TESTING,
      max_nesting_depth_warning FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS ltc_cyclo_complexity IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_cyclomatic_complexity( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD max_nesting_depth_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD max_nesting_depth_error.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 1 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD max_nesting_depth_warning.
    cut->clean_code_manager = NEW ltd_clean_code_manager_warning( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 1 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 1 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_errors( )
        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_warnings.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_warnings( )
        exp = warn_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_pseudo_comments( )
        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cyclo_complexity_event DEFINITION DEFERRED.
CLASS y_check_cyclomatic_complexity DEFINITION LOCAL FRIENDS ltc_cyclo_complexity_event.

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
    RETURN.                                       "empty for test case
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
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 8 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( type = scan_struc_type-event stmnt_from = 1 stmnt_to = 8 stmnt_type = scan_struc_stmnt_type-start_of_selection ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '1'  type = 'K' )
                          ( level = 1 from = '2'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '9'  type = 'K' )
                          ( level = 1 from = '10' to = '13' type = 'K' )
                          ( level = 1 from = '14' to = '14' type = 'K' )
                          ( level = 1 from = '15' to = '15' type = 'K' )
                          ( level = 1 from = '16' to = '16' type = 'K' )
                          ( level = 1 from = '17' to = '17' type = 'K' ) ).

    tokens = VALUE #( ( str = 'START-OF-SELECTION' type = 'I' row = 1 )
                      ( str = 'IF'                 type = 'I' row = 2 )
                      ( str = 'VALUE_A'            type = 'I' row = 2 )
                      ( str = '='                  type = 'I' row = 2 )
                      ( str = '1'                  type = 'I' row = 2 )
                      ( str = 'IF'                 type = 'I' row = 3 )
                      ( str = 'VALUE_B'            type = 'I' row = 3 )
                      ( str = '='                  type = 'I' row = 3 )
                      ( str = '2'                  type = 'I' row = 3 )
                      ( str = 'IF'                 type = 'I' row = 4 )
                      ( str = 'VALUE_C'            type = 'I' row = 4 )
                      ( str = '='                  type = 'I' row = 4 )
                      ( str = '3'                  type = 'I' row = 4 )
                      ( str = 'RETURN'             type = 'I' row = 5 )
                      ( str = 'ENDIF'              type = 'I' row = 6 )
                      ( str = 'ENDIF'              type = 'I' row = 7 )
                      ( str = 'ENDIF'              type = 'I' row = 8 ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    set_data_for_error( ).

    structures = VALUE #( ( type = scan_struc_type-event stmnt_from = 1 stmnt_to = 9 stmnt_type = scan_struc_stmnt_type-start_of_selection ) ).

    statements = VALUE #( BASE statements
                        ( level = 1 from = '18' to = '18' type = 'P' ) ).
    tokens = VALUE #( BASE tokens
                    ( str = '"#EC CI_CYCLO' type = 'C' row = 9 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cyclo_complexity_event DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_cyclomatic_complexity,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager_event.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_warnings IMPORTING warn_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      complexity_ok FOR TESTING,
      complexity_error FOR TESTING,
      complexity_warning FOR TESTING,
      pseudo_comment_ok FOR TESTING,
      raise_exception_no_customizing FOR TESTING.
ENDCLASS.

CLASS ltc_cyclo_complexity_event IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_cyclomatic_complexity( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager_event( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD complexity_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD complexity_error.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 1 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD complexity_warning.
    cut->clean_code_manager = NEW ltd_clean_code_manager_warning( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 1 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 1 ).
  ENDMETHOD.

  METHOD raise_exception_no_customizing.
    cut->clean_code_manager = NEW ltd_clean_code_manager_no_cust( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_errors( )
        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_warnings.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_warnings( )
        exp = warn_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->statistics->get_number_pseudo_comments( )
        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
