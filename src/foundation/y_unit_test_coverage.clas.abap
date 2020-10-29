CLASS y_unit_test_coverage DEFINITION PUBLIC CREATE PUBLIC . "#EC INTF_IN_CLASS
  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO y_unit_test_coverage.
    METHODS execute IMPORTING check TYPE REF TO y_check_base.
    METHODS get_statement_coverage RETURNING VALUE(result) TYPE decfloat16.
    METHODS get_branch_coverage RETURNING VALUE(result) TYPE decfloat16.
    METHODS get_procedure_coverage RETURNING VALUE(result) TYPE decfloat16.
  PROTECTED SECTION.
    METHODS execute_abap_unit_test IMPORTING check         TYPE REF TO y_check_base
                                   RETURNING VALUE(result) TYPE REF TO if_scv_result
                                   RAISING   cx_scv_execution_error
                                             cx_scv_call_error
                                             cx_dynamic_check.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO y_unit_test_coverage.
    DATA object TYPE cl_aucv_task=>ty_object_directory_element.
    DATA measurement TYPE REF TO if_scv_result.
    METHODS convert_check_to_object IMPORTING check         TYPE REF TO y_check_base
                                    RETURNING VALUE(result) TYPE cl_aucv_task=>ty_object_directory_element.
ENDCLASS.



CLASS Y_UNIT_TEST_COVERAGE IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW y_unit_test_coverage( ).
    ENDIF.
    result = instance.
  ENDMETHOD.


  METHOD execute.
    DATA(object) = convert_check_to_object( check ).

    IF me->object = object.
      RETURN.
    ENDIF.

    TRY.
        measurement = execute_abap_unit_test( check ).
        me->object = object.
      CATCH cx_scv_execution_error cx_scv_call_error cx_dynamic_check.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD execute_abap_unit_test.
    DATA(abap_unit_test) = cl_aucv_task=>create( i_measure_coverage = abap_true
                                                 i_max_risk_level = if_aunit_task=>c_risk_level-harmless
                                                 i_max_duration_category = if_aunit_task=>c_duration_category-short ).

    DATA(object) = convert_check_to_object( check ).

    abap_unit_test->add_associated_unit_tests( VALUE #( ( object ) ) ).

    abap_unit_test->run( if_aunit_task=>c_run_mode-catch_short_dump ).

    result = abap_unit_test->get_coverage_measurement( )->build_program_result( check->program_name ).
  ENDMETHOD.


  METHOD get_branch_coverage.
    result = measurement->get_coverage( ce_scv_coverage_type=>branch )->get_percentage( ).
    result = round( val = result dec = 2 ).
  ENDMETHOD.


  METHOD get_procedure_coverage.
    result = measurement->get_coverage( ce_scv_coverage_type=>procedure )->get_percentage( ).
    result = round( val = result dec = 2 ).
  ENDMETHOD.


  METHOD get_statement_coverage.
    result = measurement->get_coverage( ce_scv_coverage_type=>statement )->get_percentage( ).
    result = round( val = result dec = 2 ).
  ENDMETHOD.


  METHOD convert_check_to_object.
    result = VALUE #( object = check->object_type
                      obj_name = check->object_name ).
  ENDMETHOD.
ENDCLASS.
