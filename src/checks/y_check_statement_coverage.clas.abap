CLASS y_check_statement_coverage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA ut_task TYPE REF TO cl_aucv_task.

    METHODS init_ut_task
      RAISING cx_dynamic_check.

    METHODS has_unit_tests
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_coverage
      RETURNING VALUE(result) TYPE REF TO if_scv_coverage
      RAISING   cx_scv_execution_error
                cx_scv_call_error.

    METHODS get_measurement
      RETURNING VALUE(result) TYPE REF TO if_scv_result
      RAISING   cx_scv_execution_error
                cx_scv_call_error.

ENDCLASS.



CLASS Y_CHECK_STATEMENT_COVERAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-prio = c_note.
    settings-threshold = 100.
    settings-is_threshold_reversed = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-documentation = |{ c_docs_path-checks }statement-coverage.md|.

    set_check_message( 'Statements Total &1 | Executed &2 | Not Executed &3 | Covered &4%' ).
  ENDMETHOD.


  METHOD execute_check.
    TRY.
        init_ut_task( ).

        DATA(check_configuration) = detect_check_configuration( error_count = get_coverage( )->get_percentage( )
                                                                statement = VALUE #( level = 1 ) ).
        IF check_configuration IS INITIAL.
          RETURN.
        ENDIF.

        raise_error( statement_level     = 1
                     statement_index     = 1
                     statement_from      = 1
                     error_priority      = check_configuration-prio
                     parameter_01        = |{ get_coverage( )->get_total( ) }|
                     parameter_02        = |{ get_coverage( )->get_executed( ) }|
                     parameter_03        = |{ get_coverage( )->get_not_executed( ) }|
                     parameter_04        = |{ round( val = get_coverage( )->get_percentage( ) dec = 2 ) }| ).

      CATCH cx_dynamic_check
            cx_scv_execution_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_coverage.
    result = get_measurement( )->get_coverage( ce_scv_coverage_type=>statement ).
  ENDMETHOD.


  METHOD get_measurement.
    result = ut_task->get_coverage_measurement( )->build_program_result( program_name ).
  ENDMETHOD.


  METHOD has_unit_tests.
    TRY .
        cl_aunit_prog_byte_code_svc=>analyse_program( EXPORTING program_name = program_name
                                                      IMPORTING source_code_has_tests = result ).
      CATCH cx_aunit_prog_exists_not
            cx_aunit_prog_compilation.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD init_ut_task.
    ut_task = cl_aucv_task=>create( i_measure_coverage      = abap_true
                                    i_max_risk_level        = if_aunit_task=>c_risk_level-harmless
                                    i_max_duration_category = if_aunit_task=>c_duration_category-short ).

    ut_task->add_associated_unit_tests( VALUE #( ( object = object_type
                                                   obj_name = object_name ) ) ).

    ut_task->run( if_aunit_task=>c_run_mode-catch_short_dump ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.
