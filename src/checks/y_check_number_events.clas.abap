CLASS y_check_number_events DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    DATA event_counter TYPE i VALUE 0.

    METHODS checkif_error
      IMPORTING index     TYPE i
                statement TYPE sstmnt.
ENDCLASS.



CLASS Y_CHECK_NUMBER_EVENTS IMPLEMENTATION.


  METHOD checkif_error.
    DATA(check_configuration) = detect_check_configuration( error_count = event_counter
                                                            statement = statement ).
    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio
                 parameter_01        = |{ event_counter }|
                 parameter_02        = |{ check_configuration-threshold }| ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC NUMBER_EVENTS' ##NO_TEXT.
    settings-documentation = |{ c_docs_path-checks }number-events.md|.

    set_check_message( '&1 events reach threshold of &2' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
      WHERE stmnt_type = scan_struc_stmnt_type-class_definition
         OR stmnt_type = scan_struc_stmnt_type-interface.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      READ TABLE ref_scan_manager->get_statements( ) INTO DATA(statement_for_message)
        INDEX <structure>-stmnt_from.
      event_counter = 0.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( statement = <statement> ).
      ENDLOOP.

      checkif_error( index = <structure>-stmnt_from
                     statement = statement_for_message ).
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CASE get_token_abs( statement-from ).
      WHEN 'EVENTS' OR 'CLASS-EVENTS'.
        ADD 1 TO event_counter.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
