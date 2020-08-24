CLASS ltd_clean_code_manager_error DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager_error IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 3 )
                      ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'W' threshold = 2 ) ).
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
    result = VALUE #( ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'E' threshold = 15 )
                      ( apply_on_testcode = abap_false apply_on_productive_code = abap_true prio = 'W' threshold = 3 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '19000101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_scan_manager_double.
  PUBLIC SECTION.
    METHODS set_data_for_ok.
    METHODS set_data_for_error.
    METHODS set_pseudo_comment_ok.
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

     ( 'START-OF-SELECTION.' )
     ( '  DATA(value_a) = 1. ' )
     ( '  DATA(value_b) = 2. ' )
     ( '  IF value_a = 1. ' )
     ( '    value_b = 2. ' )
     ( '  ENDIF. ' )
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

      ( 'START-OF-SELECTION.' )
      ( '  DATA(value_a) = 1. ' )
      ( '  DATA(value_b) = 2. ' )
      ( '  IF value_a = 1. ' )
      ( '    value_b = 2. ' )
      ( '    DO value_b TIMES. ' )
      ( '      DATA(value_c) = 3. ' )
      ( '      CASE value_c. ' )
      ( '        WHEN 3. ' )
      ( '      ENDCASE. ' )
      ( '    ENDDO. ' )
      ( '  ENDIF. ' )
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

      ( 'START-OF-SELECTION.' )
      ( '  DATA(value_a) = 1. ' )
      ( '  DATA(value_b) = 2. ' )
      ( '  IF value_a = 1. ' )
      ( '    value_b = 2. ' )
      ( '    DO value_b TIMES. ' )
      ( '      DATA(value_c) = 3. ' )
      ( '      CASE value_c. ' )
      ( '        WHEN 3. ' )
      ( '      ENDCASE. ' )
      ( '    ENDDO. ' )
      ( '  ENDIF. "#EC CI_CYCLO ' )
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

CLASS ltc_cyclo_complexity DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_check_cyclomatic_complexity.
    DATA ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.
    METHODS setup.
    METHODS assert_errors IMPORTING err_cnt TYPE i.
    METHODS assert_warnings IMPORTING warn_cnt TYPE i.
    METHODS assert_pseudo_comments IMPORTING pc_cnt TYPE i.
    METHODS is_bound FOR TESTING.
    METHODS cyclomatic_complexity_ok FOR TESTING.
    METHODS cyclomatic_complexity_error FOR TESTING.
    METHODS cyclomatic_complexity_warning FOR TESTING.
    METHODS pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_cyclomatic_complexity DEFINITION LOCAL FRIENDS ltc_cyclo_complexity.

CLASS ltc_cyclo_complexity IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_cyclomatic_complexity( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

  METHOD cyclomatic_complexity_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cyclomatic_complexity_error.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 2 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cyclomatic_complexity_warning.
    cut->clean_code_manager = NEW ltd_clean_code_manager_warning( ).
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 2 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    cut->clean_code_manager = NEW ltd_clean_code_manager_error( ).
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 2 ).
  ENDMETHOD.

  METHOD assert_errors.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_errors( )
                                        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_warnings.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_warnings( )
                                        exp = warn_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_pseudo_comments( )
                                        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
