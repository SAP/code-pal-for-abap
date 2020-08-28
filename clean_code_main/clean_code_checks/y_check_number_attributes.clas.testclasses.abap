CLASS ltd_ref_scan_manager DEFINITION INHERITING FROM y_ref_scan_manager_double FOR TESTING.
  PUBLIC SECTION.
    METHODS set_data_for_ok.
    METHODS set_data_for_error.
    METHODS set_pseudo_comment_ok.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD set_data_for_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_one DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: BEGIN OF one, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF one. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_one DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     CLASS-DATA six TYPE i VALUE 6. ' )
      ( '     CLASS-DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     CLASS-DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_three IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_one DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_one IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_two DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )
      ( ' CLASS y_example_two IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_three DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     CLASS-DATA six TYPE i VALUE 6. ' )
      ( '     CLASS-DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     CLASS-DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
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
    DATA: cut                     TYPE REF TO y_check_number_attributes,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      cut_ok FOR TESTING,
      cut_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_number_attributes DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_number_attributes( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW y_clean_code_manager_double( cut ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( cut ).
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
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_errors( )
                                        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_pseudo_comments( )
                                        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
