CLASS y_pal_num_output_parameter DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS count_outputs_of_statement IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE i.

ENDCLASS.



CLASS y_pal_num_output_parameter IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NUM_OUTPUT_PARA' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 2.
    settings-documentation = |{ c_docs_path-checks }number-output-parameter.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition )
                                        ( scan_struc_stmnt_type-interface ) ).

    relevant_structure_types = VALUE #( ).

    set_check_message( 'Number of output parameters must be lower than &2! (&1>=&2)' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'METHODS'
    OR get_token_abs( statement-from ) = 'CLASS-METHODS'.

    DATA(outputs_of_statement) = count_outputs_of_statement( statement ).

    DATA(check_configuration) = detect_check_configuration( error_count = outputs_of_statement
                                                            statement = statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from + 1
                 check_configuration = check_configuration
                 parameter_01 = |{ outputs_of_statement }|
                 parameter_02 = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD count_outputs_of_statement.
    DATA(skip) = abap_false.

    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from
    TO statement-to.

      IF <token>-str = 'IMPORTING'.
        skip = abap_true.
      ELSEIF <token>-str = 'EXPORTING'
      OR <token>-str = 'CHANGING'
      OR <token>-str = 'RETURNING'.
        skip = abap_false.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      IF <token>-str = 'TYPE'
      OR <token>-str = 'LIKE'.
        result = result + 1.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
