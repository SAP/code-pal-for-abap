CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_note.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.

  METHOD set_data_for_ok.
    inject_code( VALUE #(
    ( 'REPORT name1. ' )
    ( 'AT SELECTION-SCREEN.' )
    ( '  DATA name3 TYPE string. ' )
    ( '  "?<html> ' )
    ( '*"*COMMENT ' )
    ( '*" ' )
    ( '  "! docu ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_note.
    inject_code( VALUE #(
    ( 'REPORT name1. ' )
    ( 'AT SELECTION-SCREEN.' )
    ( '  DATA name3 TYPE string. ' )
    ( '*COMMENT ' )
    ( '  "do something ' )
    ( '  "do more ' )
    ( '  "do much more ' )
    ( '  "but not any time soon ' )
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
    DATA: cut                     TYPE REF TO y_check_comment_usage,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_notes IMPORTING err_cnt TYPE i,
      is_bound FOR TESTING,
      cut_ok FOR TESTING,
      cut_note FOR TESTING.
ENDCLASS.

CLASS y_check_comment_usage DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_comment_usage( ).
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
    assert_notes( 0 ).
  ENDMETHOD.

  METHOD cut_note.
    ref_scan_manager_double->set_data_for_note( ).
    cut->run( ).
    assert_notes( 1 ).
  ENDMETHOD.

  METHOD assert_notes.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_notes( )
                                        exp = err_cnt ).
  ENDMETHOD.
ENDCLASS.
