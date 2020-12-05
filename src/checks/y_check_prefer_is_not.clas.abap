CLASS y_check_prefer_is_not DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS y_check_prefer_is_not IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREFER_IS_NOT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-is-not-to-not-is.md|.

    set_check_message( 'Prefer IS NOT to NOT IS!' ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
    WHERE stmnt_type = scan_struc_stmnt_type-if.

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

    DATA(tokens) = ref_scan_manager->get_tokens( ).

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = 'IF'
    OR str = 'ELSEIF'
    OR str = 'AND'
    OR str = 'OR'.

      DATA(position) = sy-tabix.

      TRY.
          IF tokens[ position + 1 ]-str <> 'NOT'.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      TRY.
          IF tokens[ position + 2 ]-str = 'LINE_EXISTS('.
            CONTINUE.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(configuration) = detect_check_configuration( statement ).

      IF configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = configuration-prio ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
