CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 0 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '20190101'.
  ENDMETHOD.
ENDCLASS.

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
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth1 IMPORTING param1 TYPE string ' )
      ( '                             param2 TYPE i. ' )
      ( '     METHODS meth2 IMPORTING param1 TYPE string ' )
      ( '                   EXPORTING param2 TYPE i ' )
      ( '                             param3 TYPE i. ' )
      ( '     METHODS meth3 IMPORTING param1 TYPE string ' )
      ( '                             param2 TYPE i ' )
      ( '                   RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS meth4 IMPORTING param1 TYPE string. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )

      ( |     meth1( param1 = 'example'   | )
      ( |            param2 = 10 ).       | )

      ( |     meth2( EXPORTING                  | )
      ( |              param1 = 'example'       | )
      ( |            IMPORTING                  | )
      ( |             param2 = DATA(param2)    | )
      ( |              param3 = DATA(param3) ). | )

      ( |     DATA(meth3) = meth3( param1 = 'example'   | )
      ( |                          param2 = 10 ).       | )

      ( |     meth4( 'example' ). | )

      ( '   ENDMETHOD. ' )

      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth2. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth3. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth4. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth1 IMPORTING param1 TYPE string. ' )
      ( '     METHODS meth2 IMPORTING param1 TYPE string ' )
      ( '                   RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS meth3 IMPORTING param1 TYPE string ' )
      ( '                             param2 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )

      ( |     meth1( EXPORTING param1 = 'example' ). | )

      ( |     DATA(meth2) = meth2( EXPORTING param1 = 'example' ). | )

      ( |     meth3( EXPORTING param1 = 'example' | )
      ( |                      param2 = 10 ). | )

      ( '   ENDMETHOD. ' )

      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth2. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth3. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS execute. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS meth1 IMPORTING param1 TYPE string. ' )
      ( '     METHODS meth2 IMPORTING param1 TYPE string ' )
      ( '                   RETURNING VALUE(result) TYPE i. ' )
      ( '     METHODS meth3 IMPORTING param1 TYPE string ' )
      ( '                             param2 TYPE i. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD execute. ' )

      ( |     meth1( EXPORTING param1 = 'example' ). "#EC OPTL_EXP | )

      ( |     DATA(meth2) = meth2( EXPORTING param1 = 'example' ). "#EC OPTL_EXP | )

      ( |     meth3( EXPORTING param1 = 'example' | )
      ( |                      param2 = 10 ). "#EC OPTL_EXP | )

      ( '   ENDMETHOD. ' )

      ( '   METHOD meth1. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth2. ' )
      ( '   ENDMETHOD. ' )
      ( '   METHOD meth3. ' )
      ( '   ENDMETHOD. ' )
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
  PROTECTED SECTION.
    METHODS is_bound FOR TESTING.
    METHODS cut_error FOR TESTING.
    METHODS cut_ok FOR TESTING.
    METHODS pseudo_comment_ok FOR TESTING.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_check_omit_optional_exp.
    DATA ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.
    METHODS setup.
    METHODS assert_errors IMPORTING err_cnt TYPE i.
    METHODS assert_pseudo_comments IMPORTING pc_cnt TYPE i.
ENDCLASS.

CLASS y_check_omit_optional_exp DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_omit_optional_exp( ).
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
