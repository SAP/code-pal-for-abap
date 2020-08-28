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
      ( ' CLASS y_example_class DEFINITION. "#EC NUMBER_METHODS ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '     METHODS two. ' )
      ( '     METHODS three. ' )
      ( '     METHODS four. ' )
      ( '     METHODS five. ' )
      ( '     METHODS six. ' )
      ( '     METHODS seven. ' )
      ( '     METHODS eight. ' )
      ( '     METHODS nine. ' )
      ( '     METHODS ten. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS eleven. ' )
      ( '     CLASS-METHODS twelve. ' )
      ( '     CLASS-METHODS thirteen. ' )
      ( '     CLASS-METHODS fourteen. ' )
      ( '     CLASS-METHODS fifteen. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS: ' )
      ( '       sixteen, ' )
      ( '       seventeen, ' )
      ( '       eighteen, ' )
      ( '       nineteen, ' )
      ( '       twenty. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ENDMETHOD. ' )
      ( '   METHOD two. ENDMETHOD. ' )
      ( '   METHOD three. ENDMETHOD. ' )
      ( '   METHOD four. ENDMETHOD. ' )
      ( '   METHOD five. ENDMETHOD. ' )
      ( '   METHOD six. ENDMETHOD. ' )
      ( '   METHOD seven. ENDMETHOD. ' )
      ( '   METHOD eight. ENDMETHOD. ' )
      ( '   METHOD nine. ENDMETHOD. ' )
      ( '   METHOD ten. ENDMETHOD. ' )
      ( '   METHOD eleven. ENDMETHOD. ' )
      ( '   METHOD twelve. ENDMETHOD. ' )
      ( '   METHOD thirteen. ENDMETHOD. ' )
      ( '   METHOD fourteen. ENDMETHOD. ' )
      ( '   METHOD fifteen. ENDMETHOD. ' )
      ( '   METHOD sixteen. ENDMETHOD. ' )
      ( '   METHOD seventeen. ENDMETHOD. ' )
      ( '   METHOD eighteen. ENDMETHOD. ' )
      ( '   METHOD nineteen. ENDMETHOD. ' )
      ( '   METHOD twenty. ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. "#EC NUMBER_METHODS ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '     METHODS two. ' )
      ( '     METHODS three. ' )
      ( '     METHODS four. ' )
      ( '     METHODS five. ' )
      ( '     METHODS six. ' )
      ( '     METHODS seven. ' )
      ( '     METHODS eight. ' )
      ( '     METHODS nine. ' )
      ( '     METHODS ten. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS eleven. ' )
      ( '     CLASS-METHODS twelve. ' )
      ( '     CLASS-METHODS thirteen. ' )
      ( '     CLASS-METHODS fourteen. ' )
      ( '     CLASS-METHODS fifteen. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS: ' )
      ( '       sixteen, ' )
      ( '       seventeen, ' )
      ( '       eighteen, ' )
      ( '       nineteen, ' )
      ( '       twenty. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ENDMETHOD. ' )
      ( '   METHOD two. ENDMETHOD. ' )
      ( '   METHOD three. ENDMETHOD. ' )
      ( '   METHOD four. ENDMETHOD. ' )
      ( '   METHOD five. ENDMETHOD. ' )
      ( '   METHOD six. ENDMETHOD. ' )
      ( '   METHOD seven. ENDMETHOD. ' )
      ( '   METHOD eight. ENDMETHOD. ' )
      ( '   METHOD nine. ENDMETHOD. ' )
      ( '   METHOD ten. ENDMETHOD. ' )
      ( '   METHOD eleven. ENDMETHOD. ' )
      ( '   METHOD twelve. ENDMETHOD. ' )
      ( '   METHOD thirteen. ENDMETHOD. ' )
      ( '   METHOD fourteen. ENDMETHOD. ' )
      ( '   METHOD fifteen. ENDMETHOD. ' )
      ( '   METHOD sixteen. ENDMETHOD. ' )
      ( '   METHOD seventeen. ENDMETHOD. ' )
      ( '   METHOD eighteen. ENDMETHOD. ' )
      ( '   METHOD nineteen. ENDMETHOD. ' )
      ( '   METHOD twenty. ENDMETHOD. ' )
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
    DATA: cut                     TYPE REF TO y_check_number_methods,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      number_methods_ok FOR TESTING,
      number_methods_error FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_number_methods DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_number_methods( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW y_clean_code_manager_double( cut ).
    cut->clean_code_exemption_handler = NEW ltd_clean_code_exemption_no( ).
    cut->attributes_maintained = abap_true.
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

  METHOD number_methods_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD number_methods_error.
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
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_errors( )
                                        exp = err_cnt ).
  ENDMETHOD.

  METHOD assert_pseudo_comments.
    cl_abap_unit_assert=>assert_equals( act = cut->statistics->get_number_pseudo_comments( )
                                        exp = pc_cnt ).
  ENDMETHOD.
ENDCLASS.
