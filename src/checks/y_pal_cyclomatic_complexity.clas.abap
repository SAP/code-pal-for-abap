CLASS y_pal_cyclomatic_complexity DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS second_token TYPE i VALUE 2.

    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA cyclo_comp TYPE i.
    DATA statement_for_message TYPE sstmnt.

    METHODS compute_cyclomatic_complexity CHANGING c_cyclo_comp TYPE i.

    METHODS determine_position IMPORTING type          TYPE flag
                                         index         TYPE i
                               RETURNING VALUE(result) TYPE int4.
ENDCLASS.



CLASS Y_PAL_CYCLOMATIC_COMPLEXITY IMPLEMENTATION.


  METHOD compute_cyclomatic_complexity.
    CASE keyword( ).
      WHEN 'IF'
      OR 'ELSEIF'
      OR 'WHILE'
      OR 'CHECK'
      OR 'CATCH'
      OR 'CLEANUP'
      OR 'ASSERT'
      OR 'ENDAT'
      OR 'ENDSELECT'
      OR 'LOOP'
      OR 'ON'
      OR 'PROVIDE'.
        c_cyclo_comp = c_cyclo_comp + 1.
      WHEN 'WHEN'.
        IF get_token_rel( second_token ) <> 'OTHERS'.
          c_cyclo_comp = c_cyclo_comp + 1.
        ENDIF.
      WHEN 'DO'.
        READ TABLE ref_scan->tokens INDEX statement_wa-to INTO DATA(token).
        IF token-str = 'TIMES'.
          c_cyclo_comp = c_cyclo_comp + 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_CYCLO' ##NO_TEXT.
    settings-threshold = 10.
    settings-documentation = |{ c_docs_path-checks }cyclomatic-complexity.md|.

    set_check_message( 'Cyclomatic complexity must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD determine_position.
    result = index.
    IF type = scan_struc_type-event.
      result = result - 1.
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    statement_wa = statement.

    IF index = structure-stmnt_from.
      statement_for_message = statement.
      cyclo_comp = 0.
    ENDIF.

    compute_cyclomatic_complexity( CHANGING c_cyclo_comp = cyclo_comp ).

    IF index = structure-stmnt_to.
      DATA(check_configuration) = detect_check_configuration( error_count = cyclo_comp
                                                              statement = statement_for_message ).

      DATA(position) = determine_position( type = structure-type
                                           index = index ).

      raise_error( statement_level = statement_for_message-level
                   statement_index = position
                   statement_from = statement_for_message-from
                   check_configuration = check_configuration
                   parameter_01 = |{ cyclo_comp }|
                   parameter_02 = |{ check_configuration-threshold }| ).
    ENDIF.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
