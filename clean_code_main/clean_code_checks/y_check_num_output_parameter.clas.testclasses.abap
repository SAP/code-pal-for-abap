CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'N' threshold = 1 ) ).
    result = VALUE #( BASE result
                    ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 1 ) ).
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
    RETURN.                                       "empty for test case
  ENDMETHOD.

  METHOD y_if_scan_manager~is_scan_ok.
    result = abap_true.
  ENDMETHOD.

  METHOD set_data_for_ok.
    levels = VALUE #( ( stmnt = 1 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '6' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHODS'      type = 'I' row = 1 )
                      ( str = 'METHOD_NAME1' type = 'I' row = 1 )
                      ( str = 'EXPORTING'    type = 'I' row = 1 )
                      ( str = 'LIKE'         type = 'I' row = 1 )
                      ( str = 'CHANGING'     type = 'I' row = 1 )
                      ( str = 'TYPE'         type = 'I' row = 1 )
                      ( str = 'RETURNING'    type = 'I' row = 1 )
                      ( str = 'TYPE'         type = 'I' row = 1 ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    levels = VALUE #( ( stmnt = 0 from = 1 to = 1 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 1 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '7' type = 'K' ) ).

    tokens = VALUE #( ( str = 'METHODS'      type = 'I' row = 1 )
                      ( str = 'METHOD_NAME1' type = 'I' row = 1 )
                      ( str = 'EXPORTING'    type = 'I' row = 1 )
                      ( str = 'LIKE'         type = 'I' row = 1 )
                      ( str = 'TYPE'         type = 'I' row = 1 )
                      ( str = 'RETURNING'    type = 'I' row = 1 )
                      ( str = 'TYPE'         type = 'I' row = 1 ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    levels = VALUE #( ( stmnt = 0 from = 1 to = 2 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 2 type = scan_struc_type-class stmnt_type = scan_struc_stmnt_type-class_definition ) ).

    statements = VALUE #( ( level = 1 from = '1' to = '7' type = 'K' )
                          ( level = 1 from = '8' to = '8' type = 'P' ) ).

    tokens = VALUE #( ( str = 'METHODS'              type = 'I' row = 1 )
                      ( str = 'METHOD_NAME1'         type = 'I' row = 1 )
                      ( str = 'EXPORTING'            type = 'I' row = 1 )
                      ( str = 'LIKE'                 type = 'I' row = 1 )
                      ( str = 'TYPE'                 type = 'I' row = 1 )
                      ( str = 'RETURNING'            type = 'I' row = 1 )
                      ( str = 'TYPE'                 type = 'I' row = 1 )
                      ( str = '"#EC NUM_OUTPUT_PARA' type = 'C' row = 1 ) ).
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

CLASS ltc_num_output_param DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_num_output_parameter,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      cut_error FOR TESTING,
      cut_ok FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_num_output_parameter DEFINITION LOCAL FRIENDS ltc_num_output_param.

CLASS ltc_num_output_param IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_num_output_parameter( ).
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
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cut_error.
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
