CLASS ltd_ref_scan_manager_test DEFINITION FOR TESTING INHERITING FROM y_ref_scan_manager_double. "#EC INTF_IN_CLASS
  PUBLIC SECTION.
    METHODS:
      set_data_without_test,
      set_data_with_test.
ENDCLASS.

CLASS ltd_ref_scan_manager_test IMPLEMENTATION.
  METHOD set_data_without_test.
    inject_code( VALUE #(
    ( 'REPORT lcl_test.' )

    ( 'CLASS lcl_classname DEFINITION.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS methodname.' )
    ( '  DATA itest TYPE i.'  )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_classname IMPLEMENTATION.' )
    ( ' METHOD methodname.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.

  METHOD set_data_with_test.
    inject_code( VALUE #(
    ( 'REPORT lcl_test.' )

    ( 'CLASS lcl_classname DEFINITION FOR TESTING.' )
    ( ' PUBLIC SECTION.' )
    ( '  METHODS methodname.' )
    ( '  DATA itest TYPE i.'  )
    ( 'ENDCLASS.' )

    ( 'CLASS lcl_classname IMPLEMENTATION.' )
    ( ' METHOD methodname.' )
    ( ' ENDMETHOD.' )
    ( 'ENDCLASS.' )
    ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_test_code_detector DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: cut                     TYPE REF TO y_test_code_detector,
          ref_scan_manager_double TYPE REF TO ltd_ref_scan_manager_test.

    METHODS:
      setup,
      is_bound FOR TESTING,
      is_test_class_no FOR TESTING,
      is_test_class_yes FOR TESTING,
      is_test_method_no FOR TESTING,
      is_test_method_yes FOR TESTING,
      is_if_in_test_code_yes FOR TESTING,
      is_test_attribute_no FOR TESTING,
      is_test_attribute_yes FOR TESTING.
ENDCLASS.

CLASS ltc_test_code_detector IMPLEMENTATION.
  METHOD setup.
    ref_scan_manager_double = NEW ltd_ref_scan_manager_test( ).
    cut = NEW y_test_code_detector(  ).
    cut->y_if_testcode_detector~set_ref_scan_manager( ref_scan_manager_double ).
  ENDMETHOD.

  METHOD is_bound.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.

  METHOD is_test_class_no.
    ref_scan_manager_double->set_data_without_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
          FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_false ).
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_test_class_yes.
    ref_scan_manager_double->set_data_with_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
          FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_true ).
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_test_method_no.
    ref_scan_manager_double->set_data_without_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
          FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-method.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_false ).

    ENDLOOP.
  ENDMETHOD.

  METHOD is_test_method_yes.
    ref_scan_manager_double->set_data_with_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
          FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-method.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_true ).

    ENDLOOP.
  ENDMETHOD.

  METHOD is_if_in_test_code_yes.
    ref_scan_manager_double->set_data_with_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
          FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_true ).

    ENDLOOP.
  ENDMETHOD.

  METHOD is_test_attribute_no.
    ref_scan_manager_double->set_data_without_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
              FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_false ).

    ENDLOOP.
  ENDMETHOD.

  METHOD is_test_attribute_yes.
    ref_scan_manager_double->set_data_with_test( ).

    LOOP AT ref_scan_manager_double->y_if_scan_manager~structures ASSIGNING FIELD-SYMBOL(<structure>)
              FROM 2 WHERE stmnt_type = scan_struc_stmnt_type-class_definition.

      cl_abap_unit_assert=>assert_equals(
        act = cut->y_if_testcode_detector~is_testcode( <structure> )
        exp = abap_true ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
