CLASS ltd_clean_code_manager_error DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_error IMPLEMENTATION.
  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    RETURN.
  ENDMETHOD.

  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 2 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 1 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_check_base_mock DEFINITION FOR TESTING INHERITING FROM y_check_base.
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
    result = VALUE #( ( apply_on_testcode = abap_false prio = 'E' threshold = 5 )
                      ( apply_on_testcode = abap_false prio = 'W' threshold = 1 ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_error,
      set_pseudo_comment_ok.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD set_data_for_ok.
    convert_code( VALUE #(
     ( 'REPORT y_example. ' )

     ( 'CLASS y_check_base DEFINITION. ' )
     ( '  PUBLIC SECTION. ' )
     ( '  PROTECTED SECTION. ' )
     ( '  PRIVATE SECTION. ' )
     ( '    METHODS name. ' )
     ( 'ENDCLASS. ' )

     ( 'CLASS y_check_base IMPLEMENTATION. ' )
     ( '  METHOD name. ' )
     ( '    DATA(value_a) = 1. ' )
     ( '    DATA(value_b) = 2. ' )
     ( '    IF value_a = 1. ' )
     ( '      value_b = 2. ' )
     ( '    ENDIF. ' )
     ( '  ENDMETHOD. ' )
     ( 'ENDCLASS.' )
      ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    convert_code( VALUE #(
    ( 'REPORT y_example. ' )

    ( 'CLASS y_check_base DEFINITION. ' )
    ( '  PUBLIC SECTION. ' )
    ( '  PROTECTED SECTION. ' )
    ( '  PRIVATE SECTION. ' )
    ( '    METHODS name. ' )
    ( 'ENDCLASS. ' )

    ( 'CLASS y_check_base IMPLEMENTATION. ' )
    ( '  METHOD name. ' )
    ( '    DATA(value_a) = 1. ' )
    ( '    DATA(value_b) = 2. ' )
    ( '    IF value_a = 1. ' )
    ( '      value_b = 2. ' )
    ( '      DO value_b TIMES. ' )
    ( '        DATA(value_c) = 3. ' )
    ( '        CASE value_c. ' )
    ( '          WHEN 3. ' )
    ( '        ENDCASE. ' )
    ( '      ENDDO. ' )
    ( '    ENDIF. ' )
    ( '  ENDMETHOD. ' )
    ( 'ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    convert_code( VALUE #(
    ( 'REPORT y_example. ' )

    ( 'CLASS y_check_base DEFINITION. ' )
    ( '  PUBLIC SECTION. ' )
    ( '  PROTECTED SECTION. ' )
    ( '  PRIVATE SECTION. ' )
    ( '    METHODS name. ' )
    ( 'ENDCLASS. ' )

    ( 'CLASS y_check_base IMPLEMENTATION. ' )
    ( '  METHOD name. ' )
    ( '    DATA(value_a) = 1. ' )
    ( '    DATA(value_b) = 2. ' )
    ( '    IF value_a = 1. ' )
    ( '      value_b = 2. ' )
    ( '      DO value_b TIMES. ' )
    ( '        DATA(value_c) = 3. ' )
    ( '        CASE value_c. ' )
    ( '          WHEN 3. ' )
    ( '        ENDCASE. ' )
    ( '      ENDDO. ' )
    ( '    ENDIF. ' )
    ( '  ENDMETHOD. "#EC CI_CYCLO ' )
    ( 'ENDCLASS. ' )
    ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_clean_code_exemption_no DEFINITION FOR TESTING INHERITING FROM y_exemption_handler.
  PUBLIC SECTION.
    METHODS: is_object_exempted REDEFINITION.
ENDCLASS.

CLASS ltd_clean_code_exemption_no IMPLEMENTATION.
  METHOD is_object_exempted.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_base DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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

CLASS y_check_base DEFINITION LOCAL FRIENDS ltc_base.

CLASS ltc_base IMPLEMENTATION.

  METHOD setup.
    cut = NEW ltc_check_base_mock( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->statistics = NEW y_scan_statistics( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = cut ).
  ENDMETHOD.

  METHOD raise_error.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_error( ).

    cut->scimessages = VALUE #( ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 100 kind = 'E' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 101 kind = 'W' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 102 kind = 'N' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| ) ).
    cut->settings-pseudo_comment = '#EC CI_CYCLO'.
    cut->myname = 'Y_CHECK_CYCLOMATIC_COMPLEXITY'.
    cut->raise_error(
      EXPORTING
        object_type     = ''
        statement_level = 0
        statement_from  = 0
        statement_index = 21
        error_priority  = 'E' ).

    assert_errors( 1 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD raise_pseudo_comment.
    cut->clean_code_manager = NEW ltd_clean_code_manager_warning( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).

    cut->scimessages = VALUE #( ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 100 kind = 'E' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 101 kind = 'W' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| )
                                ( test = 'Y_CHECK_CYCLOMATIC_COMPLEXITY' code = 102 kind = 'N' text = |Cyclotomic complexity is &1, exceeding threshold of &2| pcom = |CI_CYCLO| ) ).
    cut->settings-pseudo_comment = '#EC CI_CYCLO'.
    cut->myname = 'Y_CHECK_CYCLOMATIC_COMPLEXITY'.
    cut->raise_error(
      EXPORTING
        object_type     = ''
        statement_level = 0
        statement_from  = 0
        statement_index = 21
        error_priority  = 'W' ).

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

CLASS ltc_check_configuration DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS given_error_threshold_one.
    METHODS given_error_threshold_five.
    METHODS given_warning_threshold_one.
    METHODS given_warning_threshold_five.
    METHODS given_note_threshold_one.
    METHODS given_note_threshold_five.
    METHODS when_four_errors.
    METHODS when_eight_error.
  PROTECTED SECTION.
    DATA cut TYPE REF TO y_check_base.
  PRIVATE SECTION.
    METHODS setup.
    METHODS is_bound FOR TESTING.
ENDCLASS.

CLASS y_check_base DEFINITION LOCAL FRIENDS ltc_check_configuration.

CLASS ltc_check_configuration IMPLEMENTATION.
  METHOD setup.
    cut = NEW ltc_check_base_mock( ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( act = cut ).
  ENDMETHOD.

  METHOD given_error_threshold_one.
    APPEND VALUE #( prio = 'E' threshold = 1 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD given_error_threshold_five.
    APPEND VALUE #( prio = 'E' threshold = 5 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD given_note_threshold_one.
    APPEND VALUE #( prio = 'N' threshold = 1 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD given_note_threshold_five.
    APPEND VALUE #( prio = 'N' threshold = 5 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD given_warning_threshold_five.
    APPEND VALUE #( prio = 'W' threshold = 1 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD given_warning_threshold_one.
    APPEND VALUE #( prio = 'W' threshold = 5 ) TO cut->check_configurations.
  ENDMETHOD.

  METHOD when_eight_error.
*    cut->detect_check_configuration(
*        threshold =
*        include   =
*    ).
  ENDMETHOD.

  METHOD when_four_errors.

  ENDMETHOD.

ENDCLASS.
