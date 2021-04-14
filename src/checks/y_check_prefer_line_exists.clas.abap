CLASS y_check_prefer_line_exists DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

    METHODS get_statement_inline IMPORTING statement TYPE sstmnt
                                 RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS y_check_prefer_line_exists IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREF_LINE_EX' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-line-exists.md|.

    set_check_message( 'Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'READ'
    OR get_token_abs( statement-from ) = 'LOOP'.

    DATA(inline) = get_statement_inline( statement ).

    IF inline NP '* TRANSPORTING NO FIELDS *'.
      RETURN.
    ENDIF.

    IF inline CP '* FROM *'
    OR inline CP '* TO *'.
      RETURN.
    ENDIF.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = configuration-prio ).
  ENDMETHOD.


  METHOD get_statement_inline.
    LOOP AT ref_scan_manager->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.
      result = |{ result } { <token>-str }|.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
