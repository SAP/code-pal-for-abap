CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'N' threshold = 1 )
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
    METHODS:
      convert_code IMPORTING source TYPE string255_tab.

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

    DATA(source) = VALUE string255_tab(
      ( ' CLASS z_check_num_output_parameter DEFINITION PUBLIC FINAL CREATE PUBLIC. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS: ' )
      ( '       clean_method RETURNING VALUE(ro_excel) TYPE REF TO cl_abap_xsd ' )
      ( '                    RAISING cx_dynamic_check, ' )
      ( '       exporting_example EXPORTING exporting_01 TYPE REF TO cl_abap_xsd ' )
      ( '                         RAISING   cx_dynamic_check, ' )
      ( '       changing_example CHANGING changing_01 TYPE REF TO cl_abap_xsd ' )
      ( '                        RAISING  cx_dynamic_check, ' )
      ( '       inline_example EXPORTING inline_01 TYPE REF TO cl_abap_xsd RAISING cx_dynamic_check. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-METHODS static_example IMPORTING import_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                  EXPORTING exporting_01 TYPE REF TO cl_abap_xsd. ' )
      ( ' ENDCLASS. ' )
    ).

    convert_code( source ).

  ENDMETHOD.

  METHOD set_data_for_error.

    DATA(source) = VALUE string255_tab(
      ( ' CLASS z_check_num_output_parameter DEFINITION PUBLIC FINAL CREATE PUBLIC. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS: ' )
      ( '       clean_method RETURNING VALUE(ro_excel) TYPE REF TO cl_abap_xsd ' )
      ( '                    RAISING cx_dynamic_check, ' )
      ( '       exporting_example EXPORTING exporting_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_02 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_03 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_04 TYPE REF TO cl_abap_xsd ' )
      ( '                         RAISING   cx_dynamic_check,' )
      ( '       changing_example CHANGING changing_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                 changing_02 TYPE REF TO cl_abap_xsd ' )
      ( '                                 changing_03 TYPE REF TO cl_abap_xsd ' )
      ( '                        RAISING  cx_dynamic_check, ' )
      ( '       inline_example EXPORTING inline_01 TYPE REF TO cl_abap_xsd inline_02 TYPE REF TO cl_abap_xsd RAISING cx_dynamic_check. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-METHODS static_example IMPORTING import_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                  EXPORTING exporting_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                            exporting_02 TYPE REF TO cl_abap_xsd. ' )
      ( ' ENDCLASS. ' )
    ).

    convert_code( source ).

  ENDMETHOD.

  METHOD set_pseudo_comment_ok.

    DATA(source) = VALUE string255_tab(
      ( ' CLASS z_check_num_output_parameter DEFINITION PUBLIC FINAL CREATE PUBLIC. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS: ' )
      ( '       clean_method RETURNING VALUE(ro_excel) TYPE REF TO cl_abap_xsd ' )
      ( '                    RAISING cx_dynamic_check, ' )
      ( '       exporting_example EXPORTING exporting_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_02 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_03 TYPE REF TO cl_abap_xsd ' )
      ( '                                   exporting_04 TYPE REF TO cl_abap_xsd ' )
      ( '                         RAISING   cx_dynamic_check, "#EC NUM_OUTPUT_PARA ' )
      ( '       changing_example CHANGING changing_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                 changing_02 TYPE REF TO cl_abap_xsd ' )
      ( '                                 changing_03 TYPE REF TO cl_abap_xsd ' )
      ( '                        RAISING  cx_dynamic_check, "#EC NUM_OUTPUT_PARA ' )
      ( '       inline_example EXPORTING inline_01 TYPE REF TO cl_abap_xsd inline_02 TYPE REF TO cl_abap_xsd RAISING cx_dynamic_check. "#EC NUM_OUTPUT_PARA ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-METHODS static_example IMPORTING import_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                  EXPORTING exporting_01 TYPE REF TO cl_abap_xsd ' )
      ( '                                            exporting_02 TYPE REF TO cl_abap_xsd. "#EC NUM_OUTPUT_PARA ' )
      ( ' ENDCLASS. ' )
    ).

    convert_code( source ).

  ENDMETHOD.


  METHOD convert_code.
    SCAN ABAP-SOURCE source
    LEVELS INTO levels
    STRUCTURES INTO structures
    STATEMENTS INTO statements
    TOKENS INTO tokens
    WITH COMMENTS.
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
  PROTECTED SECTION.
    METHODS:
      is_bound FOR TESTING,
      cut_error FOR TESTING,
      cut_ok FOR TESTING,
      pseudo_comment_ok FOR TESTING.
  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_check_num_output_parameter,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.
    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i.
ENDCLASS.

CLASS y_check_num_output_parameter DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_num_output_parameter( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW ltd_clean_code_manager( ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( act = cut ).
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
    assert_errors( 4 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
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
