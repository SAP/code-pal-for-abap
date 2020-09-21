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

    settings-prio = c_warning.
    settings-threshold = 100.
    settings-is_threshold_reversed = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_on_testcode_selection = abap_true.
    settings-documentation = |{ c_docs_path-checks }procedure-coverage.md|.

    set_check_message( 'Procedure Coverage of &1% is under the threshold of &2%.' ).
  ENDMETHOD.


  METHOD execute_check.

    DATA(unit_test_coverage) = y_unit_test_coverage=>get_instance( ).

    unit_test_coverage->execute( me ).

    DATA(coverage) = unit_test_coverage->get_procedure_coverage( ).

    DATA(check_configuration) = detect_check_configuration( error_count = CONV #( coverage )
                                                            statement = VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = 1
                 statement_index     = 1
                 statement_from      = 1
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ coverage }|
                 parameter_02        = |{ check_configuration-threshold }| ).

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


ENDCLASS.
