CLASS y_check_procedure_coverage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
ENDCLASS.



CLASS y_check_procedure_coverage IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    version = version + 1.

    settings-threshold = 30.
    settings-is_threshold_reversed = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-apply_on_test_code = abap_false.
    settings-ignore_pseudo_comments = abap_true.
    settings-documentation = |{ c_docs_path-checks }unit-test-coverages.md|.

    set_check_message( 'Procedure Coverage must be higher than &2%! (&1%<=&2%)' ).
  ENDMETHOD.


  METHOD execute_check.

    TRY.
        DATA(coverage) = y_unit_test_coverage=>get( program_name = program_name
                                                    object = VALUE #( object = object_type obj_name = object_name )
                                                    coverage_type = ce_scv_coverage_type=>procedure ).

        DATA(procedure) = round( val = coverage->get_percentage( )
                                 dec = 2 ).
      CATCH cx_scv_execution_error.
        RETURN.
    ENDTRY.

    DATA(check_configuration) = detect_check_configuration( error_count = CONV #( procedure )
                                                            statement = VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = 1
                 statement_index = 1
                 statement_from = 1
                 error_priority = check_configuration-prio
                 parameter_01 = |{ procedure }|
                 parameter_02 = |{ check_configuration-threshold }|
                 parameter_03 = |{ coverage->get_total( ) }|
                 parameter_04 = |{ coverage->get_executed( ) }| ).

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.
ENDCLASS.
