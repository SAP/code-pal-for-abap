CLASS y_check_prefer_is_not DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
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


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from ) = 'IF'.

    DATA(tokens) = ref_scan_manager->get_tokens( ).

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str = 'IF'
    OR str = 'AND'
    OR str = 'OR'.

      TRY.
          DATA(next_token) = tokens[ sy-tabix + 1 ].
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      IF next_token-str <> 'NOT'.
        CONTINUE.
      ENDIF.

      DATA(configuration) = detect_check_configuration( statement ).

      IF configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = configuration-prio ).

      " Report the issue only once
      RETURN.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
