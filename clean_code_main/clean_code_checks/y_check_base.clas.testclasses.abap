CLASS ltc_base DEFINITION DEFERRED.
CLASS y_check_base DEFINITION LOCAL FRIENDS ltc_base.

CLASS ltd_clean_code_manager_error DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_error IMPLEMENTATION.
  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    RETURN.
  ENDMETHOD.

  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 2 ) ).
    result = VALUE #( BASE result
                    ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_check_base_mock DEFINITION FOR TESTING
INHERITING FROM y_check_base.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
ENDCLASS.

CLASS ltc_check_base_mock IMPLEMENTATION.
  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_manager_warning DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_warning IMPLEMENTATION.
  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    RETURN.
  ENDMETHOD.

  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_false prio = 'E' threshold = 5 ) ).
    result = VALUE #( BASE result
                    ( apply_on_testcode = abap_false prio = 'W' threshold = 1 ) ).
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
      ref_scan   TYPE REF TO cl_ci_scan,
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

CLASS ltc_base DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_base,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_warnings IMPORTING warn_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      raise_error FOR TESTING,
      raise_pseudo_comment FOR TESTING.
ENDCLASS.

CLASS ltc_base IMPLEMENTATION.
  METHOD setup.
    cut = NEW ltc_check_base_mock( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->statistics = NEW lcl_statistics( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD raise_error.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_error( ).

    cut->scimessages = VALUE #( ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 100 kind = 'E' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 101 kind = 'W' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 102 kind = 'N' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| ) ).
    cut->raise_error(
      EXPORTING
        p_sub_obj_type    = ''
        p_level           = 0
        p_from            = 0
        p_position        = 11
        p_test            = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY'
        p_code            = '100'
        p_suppress        = '#EC CI_CYCLO'
        p_kind            = 'E' ).

    assert_errors( 1 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD raise_pseudo_comment.
    cut->clean_code_manager = NEW ltd_clean_code_manager_warning( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).

    cut->scimessages = VALUE #( ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 100 kind = 'E' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 101 kind = 'W' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY' code = 102 kind = 'N' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| ) ).
    cut->raise_error(
      EXPORTING
        p_sub_obj_type    = ''
        p_level           = 0
        p_from            = 0
        p_position        = 11
        p_test            = 'CL_CI_CC_CYCLOMATIC_COMPLEXITY'
        p_code            = '101'
        p_suppress        = '#EC CI_CYCLO'
        p_kind            = 'W' ).

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

CLASS ltc_statistics DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO lcl_statistics.

    METHODS:
      setup,
      is_bound FOR TESTING,
      check_errors FOR TESTING,
      check_warnings FOR TESTING,
      check_pseudo_comments FOR TESTING,
      increment_pseudo_comments FOR TESTING.
ENDCLASS.

CLASS ltc_statistics IMPLEMENTATION.
  METHOD setup.
    cut = NEW lcl_statistics( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD check_errors.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 0 ).
  ENDMETHOD.

  METHOD check_warnings.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'W'
        pc   = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comments.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = 'P' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 1 ).
  ENDMETHOD.

  METHOD increment_pseudo_comments.
    cut->y_if_scan_statistics~collect(
      EXPORTING
        kind = 'E'
        pc   = 'P' ).
    cut->y_if_scan_statistics~increment_pseudo_comment_cnt( ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_errors( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_warnings( )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_scan_statistics~get_number_pseudo_comments( )
      exp = 2 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager_test DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_scan_manager PARTIALLY IMPLEMENTED.

    METHODS:
      set_data_without_test,
      set_data_with_test.

  PRIVATE SECTION.
    DATA:
      ref_scan   TYPE REF TO cl_ci_scan,
      levels     TYPE slevel_tab,
      structures TYPE sstruc_tab,
      statements TYPE sstmnt_tab,
      tokens     TYPE stokesx_tab.
ENDCLASS.

CLASS ltd_ref_scan_manager_test IMPLEMENTATION.
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

  METHOD set_data_without_test.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 5 name = 'ZTEST' type = 'P' )
                      ( depth = 1 level = 0 stmnt = 0 from = 6 to = 9 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 5 stmnt_type = scan_struc_stmnt_type-class_definition     back = 0 )
                          ( stmnt_from = 6 stmnt_to = 9 stmnt_type = scan_struc_stmnt_type-class_implementation back = 0 )
                          ( stmnt_from = 2 stmnt_to = 4 stmnt_type = scan_struc_stmnt_type-public_section       back = 2 )
                          ( stmnt_from = 7 stmnt_to = 8 stmnt_type = scan_struc_stmnt_type-method               back = 2 ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '3'  type = 'K' )
                          ( level = 1 from = '4'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '7'  type = 'K' )
                          ( level = 1 from = '8'  to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '12' type = 'K' )
                          ( level = 1 from = '13' to = '15' type = 'K' )
                          ( level = 1 from = '16' to = '17' type = 'K' )
                          ( level = 1 from = '18' to = '18' type = 'K' )
                          ( level = 1 from = '19' to = '19' type = 'K' ) ).

    tokens = VALUE #( ( str = 'CLASS'          type = 'I' row = 1 )
                      ( str = 'CLASS_1'        type = 'I' row = 1 )
                      ( str = 'DEFINITION'     type = 'I' row = 1 )
                      ( str = 'PUBLIC'         type = 'I' row = 2 )
                      ( str = 'SECTION'        type = 'I' row = 2 )
                      ( str = 'METHODS'        type = 'I' row = 3 )
                      ( str = 'METHOD_1'       type = 'I' row = 3 )
                      ( str = 'DATA'           type = 'I' row = 4 )
                      ( str = 'D1'             type = 'I' row = 4 )
                      ( str = 'TYPE'           type = 'I' row = 4 )
                      ( str = 'INT4'           type = 'I' row = 4 )
                      ( str = 'ENDCLASS'       type = 'I' row = 5 )
                      ( str = 'CLASS'          type = 'I' row = 6 )
                      ( str = 'CLASS_1'        type = 'I' row = 6 )
                      ( str = 'IMPLEMENTATION' type = 'I' row = 6 )
                      ( str = 'METHOD'         type = 'I' row = 7 )
                      ( str = 'METHOD_1'       type = 'I' row = 7 )
                      ( str = 'ENDMETHOD'      type = 'I' row = 8 )
                      ( str = 'ENDCLASS'       type = 'I' row = 9 ) ).
  ENDMETHOD.

  METHOD set_data_with_test.
    levels = VALUE #( ( depth = 1 level = 0 stmnt = 0 from = 1 to = 5 name = 'ZTEST' type = 'P' )
                      ( depth = 1 level = 0 stmnt = 0 from = 12 to = 12 name = 'ZTEST' type = 'P' ) ).

    structures = VALUE #( ( stmnt_from = 1 stmnt_to = 5 stmnt_type = scan_struc_stmnt_type-class_definition      back = 0 )
                          ( stmnt_from = 6 stmnt_to = 12 stmnt_type = scan_struc_stmnt_type-class_implementation back = 0 )
                          ( stmnt_from = 2 stmnt_to = 4 stmnt_type = scan_struc_stmnt_type-public_section        back = 2 )
                          ( stmnt_from = 7 stmnt_to = 11 stmnt_type = scan_struc_stmnt_type-method               back = 2 )
                          ( stmnt_from = 8 stmnt_to = 10 stmnt_type = scan_struc_stmnt_type-if                   back = 4 ) ).

    statements = VALUE #( ( level = 1 from = '1'  to = '5'  type = 'K' )
                          ( level = 1 from = '6'  to = '7'  type = 'K' )
                          ( level = 1 from = '8'  to = '11' type = 'K' )
                          ( level = 1 from = '12' to = '15' type = 'K' )
                          ( level = 1 from = '16' to = '16' type = 'K' )
                          ( level = 1 from = '17' to = '19' type = 'K' )
                          ( level = 1 from = '20' to = '21' type = 'K' )
                          ( level = 1 from = '22' to = '25' type = 'K' )
                          ( level = 1 from = '26' to = '28' type = 'K' )
                          ( level = 1 from = '29' to = '29' type = 'K' )
                          ( level = 1 from = '30' to = '30' type = 'K' )
                          ( level = 1 from = '31' to = '31' type = 'K' ) ).

    tokens = VALUE #( ( str = 'CLASS'          type = 'I' row = 1 )
                      ( str = 'TEST_CLASS_1'   type = 'I' row = 1 )
                      ( str = 'DEFINITION'     type = 'I' row = 1 )
                      ( str = 'FOR'            type = 'I' row = 1 )
                      ( str = 'TESTING'        type = 'I' row = 1 )
                      ( str = 'PUBLIC'         type = 'I' row = 2 )
                      ( str = 'SECTION'        type = 'I' row = 2 )
                      ( str = 'METHODS'        type = 'I' row = 3 )
                      ( str = 'TEST_METHOD_1'  type = 'I' row = 3 )
                      ( str = 'FOR'            type = 'I' row = 3 )
                      ( str = 'TESTING'        type = 'I' row = 3 )
                      ( str = 'DATA'           type = 'I' row = 4 )
                      ( str = 'TEST_INT'       type = 'I' row = 4 )
                      ( str = 'TYPE'           type = 'I' row = 4 )
                      ( str = 'INT4'           type = 'I' row = 4 )
                      ( str = 'ENDCLASS'       type = 'I' row = 5 )
                      ( str = 'CLASS'          type = 'I' row = 6 )
                      ( str = 'TEST_CLASS_1'   type = 'I' row = 6 )
                      ( str = 'IMPLEMENTATION' type = 'I' row = 6 )
                      ( str = 'METHOD'         type = 'I' row = 7 )
                      ( str = 'TEST_METHOD_1'  type = 'I' row = 7 )
                      ( str = 'IF'             type = 'I' row = 8 )
                      ( str = 'COUNT'          type = 'I' row = 8 )
                      ( str = '='              type = 'I' row = 8 )
                      ( str = 'MAX'            type = 'I' row = 8 )
                      ( str = 'MIN'            type = 'I' row = 9 )
                      ( str = '='              type = 'I' row = 9 )
                      ( str = '2'              type = 'I' row = 9 )
                      ( str = 'ENDIF'          type = 'I' row = 10 )
                      ( str = 'ENDMETHOD'      type = 'I' row = 11 )
                      ( str = 'ENDCLASS'       type = 'I' row = 12 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_test_code_detector DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO lcl_test_code_detector,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager_test.

    METHODS:
      setup,
      is_bound FOR TESTING,
      is_test_class_no FOR TESTING,
      is_test_class_yes FOR TESTING,
      is_test_method_no FOR TESTING,
      is_test_method_yes FOR TESTING,
      is_if_in_test_code_yes FOR TESTING,
      is_test_attribute_no FOR TESTING,
      is_test_attribute_yes FOR TESTING.
ENDCLASS.

CLASS ltc_test_code_detector IMPLEMENTATION.
  METHOD setup.
    ref_scan_manager_double = NEW ltd_ref_scan_manager_test( ).
    cut = NEW lcl_test_code_detector(  ).
    cut->y_if_testcode_detector~set_ref_scan_manager( ref_scan_manager_double ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD is_test_class_no.
    ref_scan_manager_double->set_data_without_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 1.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_false ).
  ENDMETHOD.

  METHOD is_test_class_yes.
    ref_scan_manager_double->set_data_with_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 1.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_test_method_no.
    ref_scan_manager_double->set_data_without_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 4.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_false ).
  ENDMETHOD.

  METHOD is_test_method_yes.
    ref_scan_manager_double->set_data_with_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 4.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_if_in_test_code_yes.
    ref_scan_manager_double->set_data_with_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 5.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_test_attribute_no.
    ref_scan_manager_double->set_data_without_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_false ).
  ENDMETHOD.

  METHOD is_test_attribute_yes.
    ref_scan_manager_double->set_data_with_test( ).

    DATA(structures) = ref_scan_manager_double->y_if_scan_manager~get_structures( ).
    READ TABLE structures INTO DATA(structure) INDEX 3.

    cl_abap_unit_assert=>assert_equals(
      act = cut->y_if_testcode_detector~is_testcode( structure )
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_check_configuration DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO y_check_base,
          exp TYPE y_if_clean_code_manager=>check_configuration.

    METHODS:
      setup,
      is_bound FOR TESTING,
      no_config_ok FOR TESTING,
      one_config_ok FOR TESTING,
      one_config_error FOR TESTING,
      two_configs_same_prio_err FOR TESTING,
      two_configs_diff_prio_warn FOR TESTING,
      two_configs_diff_prio_err FOR TESTING,
      two_configs_same_limit_err FOR TESTING,
      two_configs_diff_test_note FOR TESTING.
ENDCLASS.

CLASS y_check_base DEFINITION LOCAL FRIENDS ltc_check_configuration.

CLASS ltc_check_configuration IMPLEMENTATION.
  METHOD setup.
    cut = NEW ltc_check_base_mock( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( act = cut ).
  ENDMETHOD.

  METHOD no_config_ok.
    exp = VALUE #( apply_on_testcode = abap_false prio = '' threshold = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 3 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD one_config_ok.
    exp = VALUE #( apply_on_testcode = abap_false prio = '' threshold = 0 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false prio = 'E' threshold = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 3 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD one_config_error.
    exp = VALUE #( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 15 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD two_configs_same_prio_err.
    exp = VALUE #( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 10 )
                                         ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 15 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD two_configs_diff_prio_warn.
    exp = VALUE #( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 5 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 10 )
                                         ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 8 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD two_configs_diff_prio_err.
    exp = VALUE #( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 10 )
                                         ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 8 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD two_configs_same_limit_err.
    exp = VALUE #( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 10 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'N' threshold = 10 )
                                         ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 10 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 12 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.

  METHOD two_configs_diff_test_note.
    exp = VALUE #( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'N' threshold = 10 ).
    cut->check_configurations = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'N' threshold = 10 )
                                         ( apply_on_testcode = abap_true  apply_on_productive_code = abap_true prio = 'N' threshold = 10 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->detect_check_configuration( threshold = 12 include = 'Y____TEST' )
      exp = exp ).
  ENDMETHOD.
ENDCLASS.
