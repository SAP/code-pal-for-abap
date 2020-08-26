CLASS ltd_clean_code_manager DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: y_if_clean_code_manager.
ENDCLASS.

CLASS ltd_clean_code_manager IMPLEMENTATION.
  METHOD y_if_clean_code_manager~read_check_customizing.
    result = VALUE #( ( apply_on_testcode = abap_true apply_on_productive_code = abap_true prio = 'E' threshold = 1 ) ).
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = '20190101'.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_ref_scan_manager DEFINITION FOR TESTING INHERITING FROM y_scan_manager_double.
  PUBLIC SECTION.
    METHODS:
      set_data_for_ok,
      set_data_for_error,
      set_pseudo_comment_ok.
ENDCLASS.

CLASS ltd_ref_scan_manager IMPLEMENTATION.
  METHOD set_data_for_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_repo.' )

    ( 'CLASS lcl_abstr DEFINITION ABSTRACT.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS abstr_method ABSTRACT.' )
    ( '  METHODS constructor.' )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_abstr IMPLEMENTATION.' )
    ( ' METHOD constructor.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_inh_abstr DEFINITION INHERITING FROM lcl_abstr.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS abstr_method REDEFINITION.' )
    ( '  METHODS constructor.' )
    ( ' PROTECTED SECTION.' )
    ( '  METHODS prot_method.' )
    ( ' PRIVATE SECTION.' )
    ( '  METHODS priv_method.' )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_inh_abstr IMPLEMENTATION.' )
    ( ' METHOD constructor.' )
    ( '  super->constructor( ).' )
    ( ' ENDMETHOD.' )

    ( ' METHOD abstr_method.' )
    ( ' ENDMETHOD.' )

    ( ' METHOD prot_method.' )
    ( ' ENDMETHOD.' )

    ( ' METHOD priv_method.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_for_error.
    inject_code( VALUE #(
    ( 'REPORT ut_repo.' )

    ( 'CLASS lcl_classname DEFINITION.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS publ_method.' )
    ( ' PROTECTED SECTION.' )
    ( ' PRIVATE SECTION.' )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_classname IMPLEMENTATION.' )
    ( ' METHOD publ_method.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.

  METHOD set_pseudo_comment_ok.
    inject_code( VALUE #(
    ( 'REPORT ut_repo.' )

    ( 'CLASS lcl_classname DEFINITION. "#EC INTF_IN_CLASS' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS publ_method.'  )
    ( ' PROTECTED SECTION.' )
    ( ' PRIVATE SECTION.' )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_classname IMPLEMENTATION.' )
    ( ' METHOD publ_method.' )
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
    DATA: cut                     TYPE REF TO y_check_is_interface_in_class,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager.

    METHODS:
      setup,
      assert_errors IMPORTING err_cnt TYPE i,
      assert_pseudo_comments IMPORTING pc_cnt TYPE i,
      is_bound FOR TESTING,
      cut_error FOR TESTING,
      cut_ok FOR TESTING,
      pseudo_comment_ok FOR TESTING.
ENDCLASS.

CLASS y_check_is_interface_in_class DEFINITION LOCAL FRIENDS local_test_class.

CLASS local_test_class IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_check_is_interface_in_class( ).
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

  METHOD cut_ok.
    ref_scan_manager_double->set_data_for_ok( ).
    cut->run( ).
    assert_errors( 0 ).
    assert_pseudo_comments( 0 ).
  ENDMETHOD.

  METHOD cut_error.
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
