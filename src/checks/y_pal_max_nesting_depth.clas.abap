CLASS y_pal_max_nesting_depth DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA statement_for_message TYPE sstmnt.
    DATA curr_nesting TYPE i.
    DATA max_nesting TYPE i.

    METHODS compute_nesting_level IMPORTING token_str TYPE string.

    METHODS determine_position IMPORTING type          TYPE flag
                                         index         TYPE i
                               RETURNING VALUE(result) TYPE int4.

    METHODS is_macro IMPORTING token TYPE stokesx
                     RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_max_nesting_depth IMPLEMENTATION.


  METHOD compute_nesting_level.
    CASE token_str.
      WHEN 'IF'
      OR 'WHILE'
      OR 'LOOP'
      OR 'DO'
      OR 'PROVIDE'
      OR 'TRY'
      OR 'CASE'.
        curr_nesting = curr_nesting + 1.
      WHEN 'ENDIF'
      OR 'ENDWHILE'
      OR 'ENDLOOP'
      OR 'ENDDO'
      OR 'ENDPROVIDE'
      OR 'ENDTRY'
      OR 'ENDCASE'.
        max_nesting = nmax( val1 = max_nesting
                            val2 = curr_nesting ).
        curr_nesting = curr_nesting - 1.
      WHEN 'ENDAT'
      OR 'ENDSELECT'.
        IF curr_nesting >= max_nesting.
          max_nesting = curr_nesting + 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_NESTING' ##NO_TEXT.
    settings-documentation = |{ c_docs_path-checks }maximum-nesting-depth.md|.

    set_check_message( 'Nesting depth must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD determine_position.
    result = index.
    IF type = scan_struc_type-event.
      result = result - 1.
    ENDIF.
  ENDMETHOD.


  METHOD inspect_tokens.
    IF index = structure-stmnt_from.
      statement_for_message = statement.
      curr_nesting = 0.
      max_nesting = 0.
    ENDIF.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
      FROM statement-from TO statement-to.

      IF is_macro( <token> ).
        CONTINUE.
      ENDIF.

      compute_nesting_level( <token>-str ).

      IF index = structure-stmnt_to.
        DATA(check_configuration) = detect_check_configuration( error_count = max_nesting
                                                                statement = statement_for_message ).

        raise_error( statement_level = statement_for_message-level
                     statement_index = determine_position( type = structure-type index = index )
                     statement_from = statement_for_message-from
                     check_configuration = check_configuration
                     parameter_01 = |{ max_nesting }|
                     parameter_02 = |{ check_configuration-threshold }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_macro.
    result = xsdbool( token-row = 0 ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
