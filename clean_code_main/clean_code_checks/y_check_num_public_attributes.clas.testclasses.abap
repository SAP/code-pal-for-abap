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


CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_scan_manager_double FOR TESTING.
  PUBLIC SECTION.
    METHODS set_data_for_ok.
    METHODS set_data_for_error.
    METHODS set_pseudo_comment_ok.
  PRIVATE SECTION.
ENDCLASS.


CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD set_data_for_ok.
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_one DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CONSTANTS one TYPE i VALUE 1. ' )
      ( '     CONSTANTS two TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA: BEGIN OF three, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF three. ' )
      ( '     DATA five TYPE i. ' )
      ( '     CONSTANTS six TYPE i VALUE 6. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA seven TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_one DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: BEGIN OF one, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    convert_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_one DEFINITION. "#EC NUM_PUBLIC_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: BEGIN OF one, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. "#EC NUM_PUBLIC_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three DEFINITION. "#EC NUM_PUBLIC_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ) ).
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
    DATA: cut                     TYPE REF TO y_check_num_public_attributes,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      number_public_attributes_ok FOR TESTING,
      number_public_attributes_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_num_public_attributes DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_num_public_attributes( ).
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

  METHOD number_public_attributes_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD number_public_attributes_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 3 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 3 ).
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
