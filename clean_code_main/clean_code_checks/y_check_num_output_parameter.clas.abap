CLASS y_check_num_output_parameter DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    METHODS count_outputs_of_statement IMPORTING statement TYPE sstmnt RETURNING VALUE(result) TYPE i.
ENDCLASS.



CLASS Y_CHECK_NUM_OUTPUT_PARAMETER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Number of Output Parameters'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '690'.
    has_documentation = abap_true.

    settings-pseudo_comment = '"#EC NUM_OUTPUT_PARA' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 2.
    settings-documentation = |{ c_docs_path-checks }number-output-parameter.md|.

    y_message_registration=>add_message(
      EXPORTING
        check_name     = me->myname
        text           = '[Clean Code]: Too many output parameters!'(102)
        pseudo_comment = settings-pseudo_comment
      CHANGING
        messages       = me->scimessages ).
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE stmnt_type EQ scan_struc_stmnt_type-class_definition
    OR stmnt_type EQ scan_struc_stmnt_type-interface.

      TRY.
          DATA(apply_on_testcode) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF test_code_detector->is_testcode( <structure> ) EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
      FROM <structure>-stmnt_from TO <structure>-stmnt_to.
        DATA(index) = sy-tabix.
        inspect_tokens( index = index
                        structure = <structure>
                        statement = <statement> ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from ) = 'METHODS'
    OR get_token_abs( statement-from ) = 'CLASS-METHODS'.

    DATA(outputs_of_statement) = count_outputs_of_statement( statement ).

    DATA(configuration) = detect_check_configuration( error_count = outputs_of_statement
                                                      statement = statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from + 1
                 error_priority      = configuration-prio ).

  ENDMETHOD.


  METHOD count_outputs_of_statement.
    DATA(skip) = abap_false.

    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
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
ENDCLASS.
