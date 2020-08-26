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
    result = '19000101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_error,
      set_check_pseudo_comment_ok.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.
  METHOD set_data_for_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA val TYPE abap_bool.' )

    ( 'IF val = val.' )

    ( ' IF val = val.' )
    ( '  val = abap_true.' )
    ( ' ELSEIF val = val.' )
    ( '  val = abap_true.' )
    ( ' ELSE.' )
    ( '  val = abap_true.' )
    ( ' ENDIF.' )

    ( ' val = abap_true.' )
    ( 'ELSEIF val = val.' )
    ( ' val = abap_true.' )
    ( 'ELSE.' )
    ( ' val = abap_true.' )
    ( 'ENDIF.' )

    ( 'IF val = val.' )

    ( ' IF val = val.' )
    ( '  val = abap_true.' )
    ( ' ENDIF.' )

    ( 'val = abap_true.' )
    ( 'ENDIF.' )

    ( 'IF val = val.' )
    ( '* comment' )
    ( ' val = abap_true.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA val TYPE abap_bool.' )

    ( 'IF val = val.' )
    ( ' DATA val2 TYPE c.' )

    ( ' IF val = val.' )
    ( '  DATA(a) = 1.' )
    ( ' ELSEIF val = val.' )
    ( '  DATA b TYPE c.' )
    ( ' ELSE.' )
    ( '  FIELD-SYMBOLS: <fs> TYPE c.' )
    ( ' ENDIF.' )

    ( | DATA(name) = 'name'.| )
    ( 'ELSEIF val = val.' )
    ( ' TYPES: lcl_str TYPE c.' )
    ( 'ELSE.' )
    ( ' DATA conver TYPE c.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_check_pseudo_comment_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA val TYPE abap_bool.' )

    ( 'IF val = val.' )
    ( ' DATA val2 TYPE c. "#EC DECL_IN_IF' )

    ( ' IF val = val.' )
    ( '  DATA(a) = 1. "#EC DECL_IN_IF' )
    ( ' ELSEIF val = val.' )
    ( '  DATA b TYPE c. "#EC DECL_IN_IF' )
    ( ' ELSE.' )
    ( '  FIELD-SYMBOLS: <fs> TYPE c. "#EC DECL_IN_IF' )
    ( ' ENDIF.' )

    ( | DATA(name) = 'name'. "#EC DECL_IN_IF| )
    ( 'ELSEIF val = val.' )
    ( ' TYPES: lcl_str TYPE c. "#EC DECL_IN_IF' )
    ( 'ELSE.' )
    ( ' DATA conver TYPE c. "#EC DECL_IN_IF' )
    ( 'ENDIF.' )
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
    DATA: cut                     TYPE REF TO y_check_declaration_in_if,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      check_ok FOR TESTING,
      check_error FOR TESTING,
      check_pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_declaration_in_if DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_declaration_in_if( ).
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

  METHOD check_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_error.
    ref_scan_manager_double->set_data_for_error( ).
    cut->run( ).
    assert_errors( 7 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_check_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 7 ).
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
