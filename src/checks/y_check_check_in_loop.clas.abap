CLASS y_check_check_in_loop DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS get_back_statement IMPORTING structure     TYPE sstruc
                               RETURNING VALUE(result) TYPE sstmnt.

ENDCLASS.



CLASS y_check_check_in_loop IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CHECK_IN_LOOP' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }check-in-loop.md|.

    set_check_message( 'Use an IF statement in combination with CONTINUE instead CHECK!' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE stmnt_type EQ scan_struc_stmnt_type-check.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
      FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index = index
                        structure = <structure>
                        statement = <statement> ).
        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'CHECK'.
    CHECK get_token_abs( get_back_statement( structure )-from ) = 'LOOP'.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio ).
  ENDMETHOD.


  METHOD get_back_statement.
    DATA(structures) = ref_scan_manager->get_structures( ).
    DATA(statements) = ref_scan_manager->get_statements( ).

    TRY.
        DATA(back_structure) = structures[ structure-back ].
        result = statements[ back_structure-stmnt_from ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
