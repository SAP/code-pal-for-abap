CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double.
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

    ( ' IF val = val. " comment' )
    ( ' ELSEIF val = val.' )
    ( ' ELSE.' )
    ( ' ENDIF.' )

    ( 'ELSEIF val = val.' )
    ( 'ELSE.' )
    ( '* comment' )
    ( 'ENDIF.' )

    ( 'IF val = val.' )
    ( ' IF val = val.' )
    ( ' ENDIF.' )
    ( 'ENDIF.' )
    ) ).
  ENDMETHOD.

  METHOD set_check_pseudo_comment_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_test.' )
    ( 'START-OF-SELECTION.' )
    ( 'DATA val TYPE abap_bool.' )

    ( 'IF val = val.' )

    ( ' IF val = val.     "#EC EMPTY_IF_BRANCH' )
    ( ' ELSEIF val = val. "#EC EMPTY_IF_BRANCH' )
    ( ' ELSE.             "#EC EMPTY_IF_BRANCH' )
    ( ' ENDIF.' )

    ( 'ELSEIF val = val.  "#EC EMPTY_IF_BRANCH' )
    ( 'ELSE.              "#EC EMPTY_IF_BRANCH' )
    ( '* comment' )
    ( 'ENDIF.' )

    ( 'IF val = val.' )
    ( ' IF val = val.     "#EC EMPTY_IF_BRANCH' )
    ( ' ENDIF.' )
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
    DATA: cut                     TYPE REF TO y_check_empty_if_branches,
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

CLASS y_check_empty_if_branches DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_empty_if_branches( ).
    ref_scan_manager_double = NEW ltd_ref_scan_manager( ).
    cut->ref_scan_manager ?= ref_scan_manager_double.
    cut->clean_code_manager = NEW y_clean_code_manager_double( cut ).
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
    assert_errors( 6 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD check_pseudo_comment_ok.
    ref_scan_manager_double->set_check_pseudo_comment_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 6 ).
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
