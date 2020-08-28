CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_warning,
      set_pseudo_comment_ok.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.
  METHOD set_data_for_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'CLASS classname DEFINITION.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS class_based.' )
    ( '  METHODS system_based.' )
    ( 'ENDCLASS.' )

    ( 'CLASS classname IMPLEMENTATION.' )
    ( ' METHOD class_based.' )
    ( '  TRY.' )
    ( '   CATCH cx_sy_no_reference.' )
    ( '    DATA cx TYPE c.' )
    ( '  ENDTRY.' )
    ( ' ENDMETHOD.' )

    ( ' METHOD system_based.' )
    ( '  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.' )
    ( '   DATA cy TYPE c.' )
    ( '  ENDCATCH.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_warning.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'CLASS classname DEFINITION.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS class_based.' )
    ( '  METHODS system_based.' )
    ( 'ENDCLASS.' )

    ( 'CLASS classname IMPLEMENTATION.' )
    ( ' METHOD class_based.' )
    ( '  TRY.' )
    ( '   CATCH cx_sy_no_reference.' )
    ( '* comment' )
    ( '  ENDTRY.' )
    ( ' ENDMETHOD.' )

    ( ' METHOD system_based.' )
    ( '  CATCH SYSTEM-EXCEPTIONS OTHERS = 1.' )
    ( '  ENDCATCH.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'CLASS classname DEFINITION.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS class_based.' )
    ( '  METHODS system_based.' )
    ( 'ENDCLASS.' )

    ( 'CLASS classname IMPLEMENTATION.' )
    ( ' METHOD class_based.' )
    ( '  TRY.' )
    ( '   CATCH cx_sy_no_reference. "#EC EMPTY_CATCH' )
    ( '* comment' )
    ( '  ENDTRY.' )
    ( ' ENDMETHOD.' )

    ( ' METHOD system_based.' )
    ( '  CATCH SYSTEM-EXCEPTIONS OTHERS = 1. "#EC EMPTY_CATCH' )
    ( '  ENDCATCH.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
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
    DATA: cut                     TYPE REF TO y_check_empty_catches,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_warnings IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      cut_ok FOR TESTING,
      cut_warning FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_empty_catches DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_empty_catches( ).
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
    assert_warnings( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cut_warning.
    ref_scan_manager_double->set_data_for_warning( ).
    cut->run( ).
    assert_warnings( 2 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD pseudo_comment_ok.
    ref_scan_manager_double->set_pseudo_comment_ok( ).
    cut->run( ).
    assert_warnings( 0 ).
    assert_pseudo_comments( 2 ).
  ENDMETHOD.

  METHOD assert_warnings.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_warnings( )
                                        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_pseudo_comments( )
                                        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
