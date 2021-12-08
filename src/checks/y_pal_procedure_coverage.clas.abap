CLASS y_pal_procedure_coverage DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.



CLASS y_pal_procedure_coverage IMPLEMENTATION.

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

    remote_enabled = abap_false.
    remote_rfc_enabled = abap_false.

    set_check_message( 'Procedure Coverage must be higher than &2%! (&1%<=&2%)' ).
  ENDMETHOD.


  METHOD execute_check.

    TRY.
        DATA(coverage) = y_code_pal_coverage=>get( program_name = program_name
                                                    object = VALUE #( object = object_type obj_name = object_name )
                                                    coverage_type = ce_scv_coverage_type=>procedure ).

        DATA(procedure) = round( val = coverage->get_percentage( )
                                 dec = 2 ).
      CATCH cx_scv_execution_error.
        RETURN.
    ENDTRY.

    TRY.
        DATA(first_statement) = ref_scan->statements[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    DATA(check_configuration) = detect_check_configuration( error_count = CONV #( procedure )
                                                            statement = first_statement ).

    raise_error( statement_level = first_statement-level
                 statement_index = first_statement-from
                 statement_from = first_statement-from
                 check_configuration = check_configuration
                 parameter_01 = |{ procedure }|
                 parameter_02 = |{ check_configuration-threshold }|
                 parameter_03 = |{ coverage->get_total( ) }|
                 parameter_04 = |{ coverage->get_executed( ) }| ).
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
